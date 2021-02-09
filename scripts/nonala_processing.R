## NonALA data processing


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "rje", "stringr", "lubridate", "sp", "raster")
lapply(x, require, character.only = TRUE)
rm(x)

## Server paths
bugs_data = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
nonala_data = file.path(bugs_data, "nonALA")
output_dir = file.path(bugs_data, "outputs")


## Specify column order for input data
# ala_names <- read.csv2(file.path(output_dir, "ALAdata_colnames.csv"))
dat_cols <- c("data_source", "id", "class", "family", "scientificName", "latitude", "longitude", "year", "sensitive", "habitat")


## NSW - State data ####
## Load combined csv
nsw <- fread(file.path(nonala_data, "nsw_state.csv"))
names(nsw)

## Remove invalid records
unique(nsw$Exotic)
nsw <- nsw[nsw$Exotic == "",]

## Extract year and subset data to > = 1990
nsw$year <- year(dmy(sub(" .*", "", nsw$DateLast)))
print(setorder(nsw[, .N, by = year], year))
# nsw <- nsw[year >= 1990]

## Indicate sensitive records
unique(nsw$NSWStatus)
unique(nsw$CommStatus)
unique(nsw$SensitivityClass)

nsw$sensitive <- rep(NA, nrow(nsw))
nsw[(NSWStatus !=""|CommStatus !=""|!is.na(SensitivityClass))]$sensitive = 1
nsw[is.na(sensitive)]$sensitive = 0

# Lat/long to WGS84 - lat/long appear to be in WGS already, no need to convert
mask_file <- file.path(output_dir, "masks", "ausmask_noaa_1kmWGS_NA.tif")
ausmask <- raster::raster(mask_file)

gda_crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
pointss_raw <- sp::SpatialPointsDataFrame(as.matrix(nsw[,.(Longitude_GDA94, Latitude_GDA94)]), proj4string = CRS(gda_crs), data = as.data.frame(nsw[,.(ScientificName)]))

## Quick plotting
quickPlot::clearPlot()
quickPlot::Plot(ausmask,
                title = "",
                axes = FALSE,
                legend = FALSE,
                col = "khaki",
                addTo = "fire",
                new = TRUE)
quickPlot::Plot(points_raw,
                cols = "tomato3",
                title = "",
                addTo = "ausmask")

wgs_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
points_reproj <- rgdal::project(as.matrix(nsw[,.(Longitude_GDA94, Latitude_GDA94)]), wgs_crs)
points_reproj <- sp::SpatialPoints(points_reproj, proj4string = CRS(wgs_crs))
quickPlot::Plot(points_reproj,
                cols = "slategray3",
                title = "",
                addTo = "ausmask")

## Precise plotting
plot(ausmask)
plot(points_reproj, add = TRUE, pch = 17, col = "slategray3", cex = 0.5)
plot(points_raw, add = TRUE, pch = 17, col = "thistle4", cex = 0.5)

## Habitat
unique(nsw$Datasource)
nsw$habitat <- rep(character(), nrow(nsw))
nsw[Datasource =="NSW_bionet_aq"]$habitat = "aquatic"
nsw[Datasource =="NSW_bionet_terr"]$habitat = "terrestrial"

## Rename data columns
nsw <- nsw[,-c(2,9:16)]
names(nsw) <- dat_cols
setorder(nsw, scientificName)
saveRDS(nsw, file = file.path(nonala_data, "nsw_state.rds"))



## VIC - State data ####
## Load combined csv
vic <- fread(file.path(nonala_data, "vic_state.csv"))
names(vic)

## Remove invalid records
unique(vic$Reliability)
vic[,.N,by=Reliability]
vic <- vic[Reliability != "",]

vic[,.N,by=`Taxon Origin`]
vic <- vic[`Taxon Origin` !="Introduced"]

## Extract year and subset data to > = 1990
  # ## Check
  # x <- cbind(vic$`Survey Start Date`, year(dmy(sub(" .*", "", vic$`Survey Start Date`))))
  # dplyr::sample_n(as.data.frame(x), 50, replace = FALSE)
vic$year <- year(dmy(sub(" .*", "", vic$`Survey Start Date`)))
print(setorder(vic[, .N, by = year], year))
# vic <- vic[year >= 1990]

## Indicate sensitive records
vic$sensitive <- rep(NA, nrow(vic))

unique(vic$`Conservation Status`)
vic[, .N, by = `Conservation Status`]
vic[`Conservation Status` != ""]$sensitive = 1

unique(vic$`Site Name`)
nrow(vic[`Site Name`=="Sensitive record"])
vic[`Site Name`=="Sensitive record"]$sensitive = 1

unique(vic$EPBC)
vic[, .N, by = EPBC]
vic[EPBC != ""]$sensitive = 1

unique(vic$FFG)
vic[, .N, by = FFG]
vic[FFG != ""]$sensitive = 1

unique(vic$`Victorian Advisory List`)
vic[, .N, by = `Victorian Advisory List`]
vic[`Victorian Advisory List` != ""]$sensitive = 1

vic[is.na(sensitive)]$sensitive = 0

## Habitat
unique(vic$Datasource)
vic$habitat <- rep(character(), nrow(vic))
vic[Datasource =="VIC_vba_aq"]$habitat = "aquatic"
vic[Datasource =="VIC_vba_terr"]$habitat = "terrestrial"

# Lat/long to WGS84 - lat/long appear to be in WGS already, no need to convert
mask_file <- file.path(output_dir, "masks", "ausmask_noaa_1kmWGS_NA.tif")
ausmask <- raster::raster(mask_file)

gda_crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
points_raw <- sp::SpatialPoints(as.matrix(vic[,.(`Longitude GDA94`, `Latitude GDA94`)]), proj4string = CRS(gda_crs))

## Quick plotting
quickPlot::clearPlot()
quickPlot::Plot(ausmask,
                title = "",
                axes = FALSE,
                legend = FALSE,
                col = "khaki",
                addTo = "fire",
                new = TRUE)
quickPlot::Plot(points_raw,
                cols = "tomato3",
                title = "",
                addTo = "ausmask")

wgs_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
points_reproj <- rgdal::project(as.matrix(vic[,.(`Longitude GDA94`, `Latitude GDA94`)]), wgs_crs)
points_reproj <- sp::SpatialPoints(points_reproj, proj4string = CRS(wgs_crs))
quickPlot::Plot(points_reproj,
                cols = "slategray3",
                title = "",
                addTo = "ausmask")

## Precise plotting
plot(ausmask)
plot(points_reproj, add = TRUE, pch = 17, col = "slategray3", cex = 0.5)
plot(points_raw, add = TRUE, pch = 17, col = "thistle4", cex = 0.5)

## Rename data columns
vic <- vic[,-c(4,5,6,9:18)]
vic$class <- rep(character(), nrow(vic))
vic$family <- rep(character(), nrow(vic))
vic <- vic[,c(1,2,9,10,3:8)]
names(vic) <- dat_cols
setorder(vic, scientificName)
saveRDS(vic, file = file.path(nonala_data, "vic_state.rds"))



## QLD - State data ####
## Load combined csv
qld <- fread(file.path(nonala_data, "qld_state.csv"))
names(qld)

## Remove invalid records
unique(qld$VETTING)
qld[,.N,by=VETTING]
  # unique(qld[Datasource == "QLD_wn_indet"]$VETTING)
qld <- qld[VETTING != "U",]

## Extract year and subset data to > = 1990
  # ## Check
  # x <- year(dmy(sub(" .*", "", qld$END_DATE)))
  # which(is.na(x))
  # qld$END_DATE[which(is.na(x))]
qld$year <- year(dmy(sub(" .*", "", qld$END_DATE)))
print(setorder(qld[, .N, by = year], year))
# qld <- qld[year >= 1990]

## Indicate sensitive records
qld$sensitive <- rep(NA, nrow(qld))
unique(qld$NCA)
unique(qld$EPBC)

  # # ## Checks
  # qld[, .N, by = NCA]
  # qld[, .N, by = EPBC]
  # dim(qld[NCA != ""|EPBC != ""])
  # qld[NCA != ""|EPBC != ""]$NCA
  # qld[NCA != ""|EPBC != ""]$EPBC

qld[NCA != ""|EPBC != ""]$sensitive = 1
qld[is.na(sensitive)]$sensitive = 0

## Habitat
unique(qld$Datasource)
qld[, .N, by = Datasource]
qld$habitat <- rep(character(), nrow(qld))

# Lat/long to WGS84 - lat/long appear to be in WGS already, no need to convert
mask_file <- file.path(output_dir, "masks", "ausmask_noaa_1kmWGS_NA.tif")
ausmask <- raster::raster(mask_file)

gda_crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
points_raw <- sp::SpatialPoints(as.matrix(qld[,.(LONGITUDE, LATITUDE)]), proj4string = CRS(gda_crs))

## Quick plotting
quickPlot::clearPlot()
quickPlot::Plot(ausmask,
                title = "",
                axes = FALSE,
                legend = FALSE,
                col = "khaki",
                addTo = "fire",
                new = TRUE)
quickPlot::Plot(points_raw,
                cols = "tomato3",
                title = "",
                addTo = "ausmask")

wgs_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
points_reproj <- rgdal::project(as.matrix(qld[,.(LONGITUDE, LATITUDE)]), wgs_crs)
points_reproj <- sp::SpatialPoints(points_reproj, proj4string = CRS(wgs_crs))
quickPlot::Plot(points_reproj,
                cols = "slategray3",
                title = "",
                addTo = "ausmask")

## Precise plotting
plot(ausmask)
plot(points_reproj, add = TRUE, pch = 17, col = "slategray3", cex = 0.5)
plot(points_raw, add = TRUE, pch = 17, col = "thistle4", cex = 0.5)

## Rename data columns
qld <- qld[,-c(3,7:11)]
qld$class <- rep(character(), nrow(qld))
qld$family <- rep(character(), nrow(qld))
qld <- qld[,c(1,2,9,10,3:8)]
names(qld) <- dat_cols
setorder(qld, scientificName)
saveRDS(qld, file = file.path(nonala_data, "qld_state.rds"))



## SA - State data ####
## Load combined csv
sa <- fread(file.path(nonala_data, "sa_state.csv"))
names(sa)

## Remove invalid records
unique(sa$SPECIESCONSTAT)
grep("\\*", sa$SPECIESCONSTAT)
dim(sa[grep("\\*", sa$SPECIESCONSTAT),])
sa <- sa[-grep("\\*", sa$SPECIESCONSTAT),]

## Extract year and subset data to > = 1990
  ## Checks
  # sum(is.na(sa$SIGHTINGDATE))
  # x <- year(dmy(sub(" .*", "", sa$SIGHTINGDATE)))
  # which(is.na(x))

sa$year <- year(dmy(sub(" .*", "", sa$SIGHTINGDATE)))
print(setorder(sa[, .N, by = year], year))
# sa <- sa[year >= 1990]

## Indicate sensitive records
sa$sensitive <- rep(NA, nrow(sa))
unique(sa$ESACTSTATUSCODE)
unique(sa$NPWACTSTATUSCODE)
sa[is.na(sensitive)]$sensitive = 0

## Lat/long in WGS84
mask_file <- file.path(output_dir, "masks", "ausmask_noaa_1kmWGS_NA.tif")
ausmask <- raster::raster(mask_file)

gda_crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
points_raw <- sp::SpatialPoints(as.matrix(sa[,.(LONGITUDE, LATITUDE)]), proj4string = CRS(gda_crs))

## Quick plotting
quickPlot::clearPlot()
quickPlot::Plot(ausmask,
                title = "",
                axes = FALSE,
                legend = FALSE,
                col = "khaki",
                addTo = "fire",
                new = TRUE)
quickPlot::Plot(points_raw,
                cols = "tomato3",
                title = "",
                addTo = "ausmask")

wgs_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
points_reproj <- rgdal::project(as.matrix(sa[,.(LONGITUDE, LATITUDE)]), wgs_crs)
points_reproj <- sp::SpatialPoints(points_reproj, proj4string = CRS(wgs_crs))
quickPlot::Plot(points_reproj,
                cols = "slategray3",
                title = "",
                addTo = "ausmask")

## Precise plotting
plot(ausmask)
plot(points_reproj, add = TRUE, pch = 17, col = "slategray3", cex = 0.5)
plot(points_raw, add = TRUE, pch = 17, col = "thistle4", cex = 0.5)

## Habitat
unique(sa$BIOREGION) ## see Data_Request_Marsh_BDBSA_Fauna_Invertebrates.xlsx
unique(sa$BIOSUBREGIONNAME)
unique(sa$LANDSCAPEREGION)
sa$habitat <- rep(character(), nrow(sa))

## Rename data columns
sa <- sa[,-c(3,4,8:18)]
sa$class <- rep(character(), nrow(sa))
sa$family <- rep(character(), nrow(sa))
sa <- sa[,c(1,2,9,10,3:8)]
names(sa) <- dat_cols
setorder(sa, scientificName)
saveRDS(sa, file = file.path(nonala_data, "sa_state.rds"))


## WA - State data ####
## Load combined csv
wa <- fread(file.path(nonala_data, "wa_state.csv"))
names(wa)

## Indicate sensitive records
wa$sensitive <- rep(NA, nrow(wa))
unique(wa$WA_LISTING)
wa[, .N, by = "WA_LISTING"]
wa[is.na(sensitive)]$sensitive = 1

## Year
print(setorder(wa[, .N, by = YEAR], YEAR))

## Lat/long in WGS84
mask_file <- file.path(output_dir, "masks", "ausmask_noaa_1kmWGS_NA.tif")
ausmask <- raster::raster(mask_file)

gda_crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
points_raw <- sp::SpatialPoints(as.matrix(wa[,.(LONG_GDA, LAT_GDA)]), proj4string = CRS(gda_crs))

## Quick plotting
quickPlot::clearPlot()
quickPlot::Plot(ausmask,
                title = "",
                axes = FALSE,
                legend = FALSE,
                col = "khaki",
                addTo = "fire",
                new = TRUE)
quickPlot::Plot(points_raw,
                cols = "tomato3",
                title = "",
                addTo = "ausmask")

wgs_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
points_reproj <- rgdal::project(as.matrix(wa[,.(LONG_GDA, LAT_GDA)]), wgs_crs)
points_reproj <- sp::SpatialPoints(points_reproj, proj4string = CRS(wgs_crs))
quickPlot::Plot(points_reproj,
                cols = "slategray3",
                title = "",
                addTo = "ausmask")

## Precise plotting
plot(ausmask)
plot(points_reproj, add = TRUE, pch = 17, col = "slategray3", cex = 0.5)
plot(points_raw, add = TRUE, pch = 17, col = "thistle4", cex = 0.5)

## Habitat
unique(wa$LOCALITY)
unique(wa$SITE)
wa$habitat <- rep(character(), nrow(wa))
 
## Rename data columns
wa <- wa[,-c(4,5,6,11,12,13)]
wa$class <- rep(character(), nrow(wa))
wa <- wa[,c(1,2,10,3,4, 6,5,7,8,9)]
names(wa) <- dat_cols
setorder(wa, scientificName)
saveRDS(wa, file = file.path(nonala_data, "wa_state.rds"))



## WA - Museum data ####
wamu <- fread(file.path(nonala_data, "wa_arachnida_museum.csv"))
names(wamu)

## Valid records
wamu[, .N, by = GENUS]
dim(wamu[GENUS == ""])
wamu <- wamu[GENUS != ""]

## Add scientificName column
wamu$scientificName <- paste0(wamu$GENUS, " ", wamu$SPECIES)
wamu$scientificName <- gsub(" $", "", wamu$scientificName)
wamu$scientificName <- gsub("\\s+", " ", stringr::str_trim(wamu$scientificName)) ## replace multiple white spaces with single
# unique(grep("`", wamu$scientificName, value = TRUE))
# wamu$scientificName <- gsub("`", " ", wamu$scientificName)

## Habitat
wamu$habitat <- rep(character(), nrow(wamu))

## Data source
wamu$data_source <- rep("WA_arachnida_museum", nrow(wamu))

## Sensitive records
wamu$sensitive <- rep(NA, nrow(wamu))

# wa <- readRDS(file = file.path(nonala_data, "wa_state.rds"))
wastate_sensitive <- unique(wa[sensitive == 1]$scientificName)
  # wamu$scientificName[wamu$scientificName %in% wastate_sensitive]
  # length(unique(wamu$scientificName[wamu$scientificName %in% wastate_sensitive]))
  # length(unique(wamu$scientificName))
wamu[wamu$scientificName %in% wastate_sensitive]$sensitive = 1

wamu[is.na(sensitive)]$sensitive = 0

## Rename data columns
wamu <- wamu[,-c(2,3,5,7:13)]
wamu <- wamu[,c(9,1,2,3,7,4,5,6,10,8)]
names(wamu) <- dat_cols
setorder(wamu, scientificName)
saveRDS(wamu, file = file.path(nonala_data, "wa_museum.rds"))


## SA - private data ####s
sa <- fread(file.path(nonala_data, "sa_private.csv"))
names(sa)

## Valid records
sa[, .N, by = Genus]
sa <- sa[Genus != ""]

## Add scientificName column
sa$scientificName <- paste0(sa$Genus, " ", sa$Species)
sa$scientificName <- gsub(" $", "", sa$scientificName)
sa$scientificName <- gsub("\\s+", " ", stringr::str_trim(sa$scientificName)) ## replace multiple white spaces

## Extract year and subset data to > = 1990
  # ## Checks
  # sum(is.na(sa$Date))
  # x <- year(dmy(sub(" .*", "", sa$Date)))
  # sa$Date[which(is.na(x))]

sa$year <- year(dmy(sub(" .*", "", sa$Date)))
print(setorder(sa[, .N, by = year], year))

## Indicate sensitive records
sa$sensitive <- rep(NA, nrow(sa))
sa$sensitive = 0

## Lat/long
unique(sa$Datum)

## Habitat
sa$habitat <- rep(character(), nrow(sa))

## Rename data columns
sa <- sa[,-c(5:12, 14)]
sa <- sa[,c(1,2,5:7,3,4,8:10)]
names(sa) <- dat_cols
setorder(sa, scientificName)
saveRDS(sa, file = file.path(nonala_data, "sa_private.rds"))


## Combine datasets ####
## State + Museum data
dat <- do.call("rbind", lapply(list.files(nonala_data, 
                                           pattern = ".rds$", 
                                           full.names = TRUE), readRDS))