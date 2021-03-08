## NonALA data processing - WGS 84


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

## Load mask and CRS text strings - WGS 84
mask_file <- file.path(output_dir, "masks", "ausmask_noaa_1kmWGS_NA.tif")
ausmask <- raster::raster(mask_file)
gda_crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
wgs_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## Load AFD data
afd <- fread(file.path(output_dir, "afd_species_clean.csv"))
afd <- afd[,.(CLASS, ORDER, FAMILY, GENUS, VALID_NAME)]
names(afd) <- c("afd_Class", "afd_Order", "afd_Family", "afd_Genus", "afd_ValidName")
setkey(afd, afd_ValidName)


## NSW - State data ####
rm(dat)
dat <- fread(file.path(nonala_data, "nsw_state.csv"))
names(dat)

## Remove invalid records
unique(dat$Exotic)
dat <- dat[dat$Exotic == "",]

## Extract year
dat$year <- year(dmy(dat$DateLast))
print(setorder(dat[, .N, by = year], year))
sum(is.na(dat$year))
sum(dat$year == "")

## Indicate sensitive records
unique(dat$NSWStatus)
unique(dat$CommStatus)
unique(dat$SensitivityClass)

dat$sensitive <- rep(NA, nrow(dat))
dat[(NSWStatus !=""|CommStatus !=""|!is.na(SensitivityClass))]$sensitive = 1
dat[is.na(sensitive)]$sensitive = 0

## Habitat
unique(dat$Datasource)
dat$habitat <- rep(character(), nrow(dat))
dat[Datasource =="NSW_bionet_aq"]$habitat = "aquatic"
dat[Datasource =="NSW_bionet_terr"]$habitat = "terrestrial"

## Lat/long (covert GDA94 to WGS84)
points_raw <- sp::SpatialPoints(dat[,.(Longitude_GDA94, Latitude_GDA94)], proj4string = CRS(gda_crs))
points_reproj <- sp::spTransform(points_raw, CRS(wgs_crs))

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
  quickPlot::Plot(points_reproj,
                  cols = "slategray3",
                  title = "",
                  addTo = "ausmask")
  
    # ## Precise plotting
    # plot(ausmask, box = FALSE, axes = FALSE, legend = FALSE)
    # plot(points_reproj, add = TRUE, pch = 17, col = "tomato3", cex = 0.5)
    # plot(points_raw, add = TRUE, pch = 17, col = "slategray3", cex = 0.5)

str(points_reproj@coords)
dat$latitude <- points_reproj@coords[,"Latitude_GDA94"]
dat$longitude <- points_reproj@coords[,"Longitude_GDA94"]

## Checks for out-of-extent lat/longs
dim(dat[which(longitude < ausmask@extent@xmin)])
dim(dat[which(longitude > ausmask@extent@xmax)])
dim(dat[which(latitude < ausmask@extent@ymin)])
dim(dat[which(latitude > ausmask@extent@ymax)])

## Rename data columns
dat <- dat[,-c(2,7:16)]
dat <- dat[,c(1:5,9,10,6:8)]
names(dat) <- dat_cols
setorder(dat, scientificName)
head(dat)
str(dat)
saveRDS(dat, file = file.path(nonala_data, "nsw_state.rds"))



## VIC - State data ####
rm(dat)
dat <- fread(file.path(nonala_data, "vic_state.csv"))
names(dat)

## Remove invalid records
unique(dat$Reliability)
dat[,.N,by=Reliability]
dat <- dat[Reliability != "",]

dat[,.N,by=`Taxon Origin`]
dat <- dat[`Taxon Origin` !="Introduced"]

## Extract year
dat$year <- year(dmy(dat$`Survey Start Date`))
print(setorder(dat[, .N, by = year], year))
sum(is.na(dat$year))
sum(dat$year == "")

## Indicate sensitive records
dat$sensitive <- rep(NA, nrow(dat))

unique(dat$`Conservation Status`)
dat[, .N, by = `Conservation Status`]
dat[`Conservation Status` != ""]$sensitive = 1

unique(dat$`Site Name`)
nrow(dat[`Site Name`=="Sensitive record"])
dat[`Site Name`=="Sensitive record"]$sensitive = 1

unique(dat$EPBC)
dat[, .N, by = EPBC]
dat[EPBC != ""]$sensitive = 1

unique(dat$FFG)
dat[, .N, by = FFG]
dat[FFG == "Listed"]$sensitive = 1

unique(dat$`Victorian Advisory List`)
dat[, .N, by = `Victorian Advisory List`]
dat[`Victorian Advisory List` != ""]$sensitive = 1

## Label sensitive for species labelled as 'breed' or 'rest' under restricted flag in VBA species-checklist
checklist <- fread(file.path(nonala_data, "state/Vic/Species-Checklist.csv"))
unique(checklist$TAXON_TYPE)
checklist <- checklist[TAXON_TYPE == "Invertebrates" ]
checklist <- checklist[,.(SCIENTIFIC_NAME, SCIENTIFIC_NME_SYNONYM, RESTRICTED_FLAG)]
checklist[, .N, by = RESTRICTED_FLAG]
checklist <- checklist[RESTRICTED_FLAG != ""]

sort(unique(dat[dat$`Scientific Name` %in% checklist$SCIENTIFIC_NAME]$`Scientific Name`))
sort(checklist$SCIENTIFIC_NAME)
dim(dat[dat$`Scientific Name` %in% checklist$SCIENTIFIC_NAME])
dat[dat$`Scientific Name` %in% checklist$SCIENTIFIC_NAME]$sensitive = 1

dat[is.na(sensitive)]$sensitive = 0
dat[, .N, by = sensitive]

## Habitat
unique(dat$Datasource)
dat$habitat <- rep(character(), nrow(dat))
dat[Datasource =="VIC_vba_aq"]$habitat = "aquatic"
dat[Datasource =="VIC_vba_terr"]$habitat = "terrestrial"

## Lat/long to WGS84 - lat/long appear to be in WGS already, no need to convert
points_raw <- sp::SpatialPoints(dat[,.(`Longitude GDA94`, `Latitude GDA94`)], proj4string = CRS(gda_crs))
points_reproj <- sp::spTransform(points_raw, CRS(wgs_crs))

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
  quickPlot::Plot(points_reproj,
                  cols = "slategray3",
                  title = "",
                  addTo = "ausmask")
  
    # ## Precise plotting
    # plot(ausmask, box = FALSE, axes = FALSE, legend = FALSE)
    # plot(points_reproj, add = TRUE, pch = 17, col = "tomato3", cex = 0.5)
    # plot(points_raw, add = TRUE, pch = 17, col = "slategray3", cex = 0.5)

str(points_reproj@coords)
dat$latitude <- points_reproj@coords[,'Latitude GDA94']
dat$longitude <- points_reproj@coords[,'Longitude GDA94']

  
## Checks for out-of-extent lat/longs
dim(dat[which(longitude < ausmask@extent@xmin)])
dim(dat[which(longitude > ausmask@extent@xmax)])
dim(dat[which(latitude < ausmask@extent@ymin)])
dim(dat[which(latitude > ausmask@extent@ymax)])

## Rename data columns
dat$class <- rep(character(), nrow(dat))
dat$family <- rep(character(), nrow(dat))
dat <- dat[,-c(4:18)]
dat <- dat[,c(1,2,9,10,3,7,8,4:6)]
names(dat) <- dat_cols
setorder(dat, scientificName)
head(dat)
str(dat)

## Get tax info from AFD
rm(afd_info)
afd_info <- data.table()
for (sp in unique(dat$scientificName)){
  if (length(unique(afd[which(afd$afd_ValidName %in% sp)]$afd_ValidName)) > 1){
    warning(paste0("More than 1 unique taxon info found for ", sp))
    
  } else {
    if (length(unique(afd[which(afd$afd_ValidName %in% sp)]$afd_ValidName)) == 0) {
      warning(paste0("No taxon info found for ", sp))
    } else {
      x <- unique((afd[.(sp)]))
      afd_info <- rbind(afd_info, cbind(scientificName = sp, x))
    }
  }
}
warnings()
sum(duplicated(afd_info))
afd_info <- setDT(afd_info, key = "scientificName")

## Add tax info to data
for (sp in afd_info$scientificName){
  dat[scientificName == sp]$class = afd_info[scientificName == sp]$afd_Class
  dat[scientificName == sp]$family = afd_info[scientificName == sp]$afd_Family
}

sum(is.na(dat$class))
sum(is.na(dat$family))

## Save data table
saveRDS(dat, file = file.path(nonala_data, "vic_state.rds"))



## QLD - State data ####
rm(dat)
dat <- fread(file.path(nonala_data, "qld_state.csv"))
names(dat)

## Remove invalid records
unique(dat$VETTING)
dat[,.N,by=VETTING]
  # unique(dat[Datasource == "QLD_wn_indet"]$VETTING)
dat <- dat[VETTING != "U",]

## Extract year
dat$year <- year(dmy(dat$END_DATE))
print(setorder(dat[, .N, by = year], year))
sum(is.na(dat$year))
sum(dat$year == "")

## Indicate sensitive records
dat$sensitive <- rep(NA, nrow(dat))
unique(dat$NCA)
unique(dat$EPBC)

  # # ## Checks
  # dat[, .N, by = NCA]
  # dat[, .N, by = EPBC]
  # dim(dat[NCA != ""|EPBC != ""])
  # dat[NCA != ""|EPBC != ""]$NCA
  # dat[NCA != ""|EPBC != ""]$EPBC

dat[NCA != ""|EPBC != ""]$sensitive = 1
dat[is.na(sensitive)]$sensitive = 0

## Habitat
unique(dat$Datasource)
dat[, .N, by = Datasource]
dat$habitat <- rep(character(), nrow(dat))

## Lat/long (convert GDA94 to WGS84)
points_raw <- sp::SpatialPoints(dat[,.(LONGITUDE, LATITUDE)], proj4string = CRS(gda_crs))
points_reproj <- sp::spTransform(points_raw, CRS(wgs_crs))

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
  quickPlot::Plot(points_reproj,
                  cols = "slategray3",
                  title = "",
                  addTo = "ausmask")
  
    # ## Precise plotting
    # plot(ausmask, box = FALSE, axes = FALSE, legend = FALSE)
    # plot(points_reproj, add = TRUE, pch = 17, col = "tomato3", cex = 0.5)
    # plot(points_raw, add = TRUE, pch = 17, col = "slategray3", cex = 0.5)

str(points_reproj@coords)
dat$latitude <- points_reproj@coords[,"LATITUDE"]
dat$longitude <- points_reproj@coords[,"LONGITUDE"]


## Checks for out-of-extent lat/longs
dim(dat[which(longitude < ausmask@extent@xmin)])
dim(dat[which(longitude > ausmask@extent@xmax)])
dim(dat[which(latitude < ausmask@extent@ymin)])
dim(dat[which(latitude > ausmask@extent@ymax)])

## Rename data columns
dat$class <- rep(character(), nrow(dat))
dat$family <- rep(character(), nrow(dat))
dat <- dat[,-c(3,5:11)]
dat <- dat[,c(1,2,9,10,3,7,8,4:6)]
names(dat) <- dat_cols
setorder(dat, scientificName)
str(dat)
head(dat)

## Get tax info from AFD
rm(afd_info)
afd_info <- data.table()
for (sp in unique(dat$scientificName)){
  if (length(unique(afd[which(afd$afd_ValidName %in% sp)]$afd_ValidName)) > 1){
    warning(paste0("More than 1 unique taxon info found for ", sp))
    
  } else {
    if (length(unique(afd[which(afd$afd_ValidName %in% sp)]$afd_ValidName)) == 0) {
      warning(paste0("No taxon info found for ", sp))
    } else {
      x <- unique((afd[.(sp)]))
      afd_info <- rbind(afd_info, cbind(scientificName = sp, x))
    }
  }
}
warnings()
sum(duplicated(afd_info))
afd_info <- setDT(afd_info, key = "scientificName")

## Add tax info to data
for (sp in afd_info$scientificName){
  dat[scientificName == sp]$class = afd_info[scientificName == sp]$afd_Class
  dat[scientificName == sp]$family = afd_info[scientificName == sp]$afd_Family
}

sum(is.na(dat$class))
sum(is.na(dat$family))

## Save data table
saveRDS(dat, file = file.path(nonala_data, "qld_state.rds"))



## SA - State data ####
rm(dat)
dat <- fread(file.path(nonala_data, "sa_state.csv"))
names(dat)

## Remove invalid records (records with an asterix)
unique(dat$SPECIESCONSTAT)
grep("\\*", dat$SPECIESCONSTAT)
dim(dat[grep("\\*", dat$SPECIESCONSTAT),])
dat <- dat[-grep("\\*", dat$SPECIESCONSTAT),]

## Extract year
dat$year <- year(dmy(dat$SIGHTINGDATE))
print(setorder(dat[, .N, by = year], year))
sum(is.na(dat$year))
sum(dat$year == "")

## Indicate sensitive records (none in SA)
dat$sensitive <- rep(NA, nrow(dat))
unique(dat$ESACTSTATUSCODE)
unique(dat$NPWACTSTATUSCODE)
dat[is.na(sensitive)]$sensitive = 0

## Habitat
unique(dat$BIOREGION) ## see Data_Request_Marsh_BDBSA_Fauna_Invertebrates.xlsx
unique(dat$BIOSUBREGIONNAME)
unique(dat$LANDSCAPEREGION)
dat$habitat <- rep(character(), nrow(dat))

## Lat/long in WGS84
points_raw <- sp::SpatialPoints(dat[,.(LONGITUDE, LATITUDE)], proj4string = CRS(wgs_crs))

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
  
    # ## Precise plotting
    # plot(ausmask, box = FALSE, axes = FALSE, legend = FALSE)
    # plot(points_raw, add = TRUE, pch = 17, col = "slategray3", cex = 0.5)

## Checks for out-of-extent lat/longs
dim(dat[which(LONGITUDE < ausmask@extent@xmin)])
dim(dat[which(LONGITUDE > ausmask@extent@xmax)])
dim(dat[which(LATITUDE < ausmask@extent@ymin)])
dim(dat[which(LATITUDE > ausmask@extent@ymax)])

## Rename data columns
dat$class <- rep(character(), nrow(dat))
dat$family <- rep(character(), nrow(dat))
dat <- dat[,-c(3,4,8:18)]
dat <- dat[,c(1,2,9,10,3:8)]
names(dat) <- dat_cols
setorder(dat, scientificName)
str(dat)
head(dat)

## Get tax info from AFD
rm(afd_info)
afd_info <- data.table()
for (sp in unique(dat$scientificName)){
  if (length(unique(afd[which(afd$afd_ValidName %in% sp)]$afd_ValidName)) > 1){
    warning(paste0("More than 1 unique taxon info found for ", sp))
    
  } else {
    if (length(unique(afd[which(afd$afd_ValidName %in% sp)]$afd_ValidName)) == 0) {
      warning(paste0("No taxon info found for ", sp))
    } else {
      x <- unique((afd[.(sp)]))
      afd_info <- rbind(afd_info, cbind(scientificName = sp, x))
    }
  }
}
warnings()
sum(duplicated(afd_info))
afd_info <- setDT(afd_info, key = "scientificName")

## Add tax info to data
for (sp in afd_info$scientificName){
  dat[scientificName == sp]$class = afd_info[scientificName == sp]$afd_Class
  dat[scientificName == sp]$family = afd_info[scientificName == sp]$afd_Family
}

sum(is.na(dat$class))
sum(is.na(dat$family))

## Save data table
saveRDS(dat, file = file.path(nonala_data, "sa_state.rds"))



## SA - private data ####
rm(dat)
dat <- fread(file.path(nonala_data, "sa_private.csv"))
names(dat)

## Valid records
dat[, .N, by = Genus]
dat <- dat[Genus != ""]

## Add scientificName column
dat$scientificName <- paste0(dat$Genus, " ", dat$Species)
dat$scientificName <- gsub(" $", "", dat$scientificName)
dat$scientificName <- gsub("\\s+", " ", stringr::str_trim(dat$scientificName)) ## replace multiple white spaces

## Extract year
dat$year <- year(dmy(dat$Date))
print(setorder(dat[, .N, by = year], year))
sum(is.na(dat$year))
sum(dat$year == "")

## Indicate sensitive records
dat$sensitive <- rep(NA, nrow(dat))
dat$sensitive = 0

## Habitat
dat$habitat <- rep(character(), nrow(dat))

## Lat/long in mutiple CRS
## Notes: GDA converted to WGS, AGD not converted
# See reasoning for not converting AGD66 in data processing doc
unique(dat$Datum)
dat[, .N, by = Datum]

points_GDA <- dat[Datum == "GDA94", .(Longitude, Latitude)]
points_GDA <- sp::SpatialPoints(points_GDA, proj4string = CRS(gda_crs))
points_GDAreproj <- sp::spTransform(points_GDA, CRS(wgs_crs))

points_WGS <- dat[(Datum == "WGS 84" | Datum == "WGS84"), .(Longitude, Latitude)]
points_WGS <- sp::SpatialPoints(points_WGS, proj4string = CRS(wgs_crs))

points_AGD <- dat[Datum == "AGD66", .(Longitude, Latitude)]
agd_crs = "+proj=utm +zone=53 +south +ellps=aust_SA +units=m +no_defs"
points_AGD <- sp::SpatialPoints(points_WGS, proj4string = CRS(agd_crs))
# points_AGDreproj <- sp::spTransform(points_AGD, CRS(wgs_crs))

  ## Quick plotting
  quickPlot::clearPlot()
  quickPlot::Plot(ausmask,
                  title = "",
                  axes = FALSE,
                  legend = FALSE,
                  col = "khaki",
                  addTo = "fire",
                  new = TRUE)
  
  quickPlot::Plot(points_GDA,
                  cols = "tomato3",
                  title = "",
                  addTo = "ausmask")
  quickPlot::Plot(points_GDAreproj,
                  cols = "slategray3",
                  title = "",
                  addTo = "ausmask")
  
  quickPlot::Plot(points_WGS,
                  cols = "steelblue1",
                  title = "",
                  addTo = "ausmask")
  
  quickPlot::Plot(points_AGD,
                  cols = "plum1",
                  title = "",
                  addTo = "ausmask")
  
    # ## Precise plotting
    # plot(ausmask, box = FALSE, axes = FALSE, legend = FALSE)
    # plot(points_raw, add = TRUE, pch = 17, col = "slategray3", cex = 0.5)

str(points_GDAreproj@coords)
dat$latitude <- dat$Latitude
dat[Datum == "GDA94"]$latitude <- points_GDAreproj@coords[,"Latitude"]
dat$longitude <- dat$Longitude
dat[Datum == "GDA94"]$longitude <- points_GDAreproj@coords[,"Longitude"]

## Checks for out-of-extent lat/longs
dim(dat[which(longitude < ausmask@extent@xmin)])
dim(dat[which(longitude > ausmask@extent@xmax)])
dim(dat[which(latitude < ausmask@extent@ymin)])
dim(dat[which(latitude > ausmask@extent@ymax)])

## Rename data columns
dat <- dat[,-c(3:12, 14)]
dat <- dat[,c(1:5,9,10,6:8)]
names(dat) <- dat_cols
setorder(dat, scientificName)
str(dat)
head(dat)
saveRDS(dat, file = file.path(nonala_data, "sa_private.rds"))



## WA - State data ####
## Load combined csv
rm(dat)
dat <- fread(file.path(nonala_data, "wa_state.csv"))
names(dat)

## Year
print(setorder(dat[, .N, by = YEAR], YEAR))

## Indicate sensitive records
dat$sensitive <- rep(NA, nrow(dat))
unique(dat$WA_LISTING)
dat[, .N, by = "WA_LISTING"]
dat[is.na(sensitive)]$sensitive = 1

## Year
print(setorder(dat[, .N, by = YEAR], YEAR))
sum(is.na(dat$YEAR))
sum(dat$YEAR == "")

## Habitat
unique(dat$LOCALITY)
unique(dat$SITE)
dat$habitat <- rep(character(), nrow(dat))

## Lat/long (covert GDA94 to WGS84)
points_raw <- sp::SpatialPoints(dat[,.(LONG_GDA, LAT_GDA)], proj4string = CRS(gda_crs))
points_reproj <- sp::spTransform(points_raw, CRS(wgs_crs))

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
  quickPlot::Plot(points_reproj,
                  cols = "slategray3",
                  title = "",
                  addTo = "ausmask")
  
    # ## Precise plotting
    # plot(ausmask, box = FALSE, axes = FALSE, legend = FALSE)
    # plot(points_reproj, add = TRUE, pch = 17, col = "tomato3", cex = 0.5)
    # plot(points_raw, add = TRUE, pch = 17, col = "slategray3", cex = 0.5)

str(points_reproj@coords)
dat$latitude <- points_reproj@coords[,"LAT_GDA"]
dat$longitude <- points_reproj@coords[,"LONG_GDA"]

## Checks for out-of-extent lat/longs
dim(dat[which(longitude < ausmask@extent@xmin)])
dim(dat[which(longitude > ausmask@extent@xmax)])
dim(dat[which(latitude < ausmask@extent@ymin)])
dim(dat[which(latitude > ausmask@extent@ymax)])

## Rename data columns
dat$class <- rep(character(), nrow(dat))
dat <- dat[,-c(4:6,8,9,11,12,13)]
dat <- dat[,c(1,2,10,3,4,8,9,5,6,7)]
names(dat) <- dat_cols
setorder(dat, scientificName)
str(dat)
head(dat)

## Get tax info from AFD
rm(afd_info)
afd_info <- data.table()
for (sp in unique(dat$scientificName)){
  if (length(unique(afd[which(afd$afd_ValidName %in% sp)]$afd_ValidName)) > 1){
    warning(paste0("More than 1 unique taxon info found for ", sp))
    
  } else {
    if (length(unique(afd[which(afd$afd_ValidName %in% sp)]$afd_ValidName)) == 0) {
      warning(paste0("No taxon info found for ", sp))
    } else {
      x <- unique((afd[.(sp)]))
      afd_info <- rbind(afd_info, cbind(scientificName = sp, x))
    }
  }
}
warnings()
sum(duplicated(afd_info))
afd_info <- setDT(afd_info, key = "scientificName")

## Add tax info to data
for (sp in afd_info$scientificName){
  dat[scientificName == sp]$class = afd_info[scientificName == sp]$afd_Class
}

sum(is.na(dat$class))
sum(is.na(dat$family))

## Save data table
saveRDS(dat, file = file.path(nonala_data, "wa_state.rds"))



## WA - Museum data ####
rm(dat)
dat <- fread(file.path(nonala_data, "wa_arachnida_museum.csv"))
names(dat)

## Data source
dat$data_source <- rep("WA_arachnida_museum", nrow(dat))

## Valid records
dat[, .N, by = GENUS]
dim(dat[GENUS == ""])
dat <- dat[GENUS != ""]

## Add scientificName column
dat$scientificName <- paste0(dat$GENUS, " ", dat$SPECIES)
dat$scientificName <- gsub(" $", "", dat$scientificName)
dat$scientificName <- gsub("\\s+", " ", stringr::str_trim(dat$scientificName)) ## replace multiple white spaces with single
  # unique(grep("`", dat$scientificName, value = TRUE))
  # dat$scientificName <- gsub("`", " ", dat$scientificName)

## Remove na lat/longs
dim(dat)
dim(dat[!is.na(LATDEC)])
dat <- dat[!is.na(LATDEC)]
dim(dat)

dim(dat[LATDEC == ""])
dat <- dat[LATDEC != ""]
dim(dat)

dim(dat[!is.na(LONGDEC)])
dat <- dat[!is.na(LONGDEC)]
dim(dat)

dim(dat[LONGDEC == ""])
dat <- dat[LONGDEC != ""]
dim(dat)

## Year
unique(dat$SYEAR)
print(setorder(dat[, .N, by = SYEAR], SYEAR))

dat[SYEAR == "200"]$SYEAR = "2000"
dat[SYEAR == "202"]$SYEAR = "2020"
dat[SYEAR == "@014"]$SYEAR = "2014"
unique(dat$SYEAR)

sum(is.na(dat$SYEAR))
sum(dat$SYEAR == "")

## Sensitive records
dat$sensitive <- rep(NA, nrow(dat))
wa <- readRDS(file = file.path(nonala_data, "wa_state.rds"))
wastate_sensitive <- unique(wa[sensitive == 1]$scientificName)
  # dat$scientificName[dat$scientificName %in% wastate_sensitive]
  # length(unique(dat$scientificName[dat$scientificName %in% wastate_sensitive]))
  # length(unique(dat$scientificName))
dat[dat$scientificName %in% wastate_sensitive]$sensitive = 1

dat[is.na(sensitive)]$sensitive = 1 ## For now mark all as sensitive

## Habitat
dat$habitat <- rep(character(), nrow(dat))

## Lat/long in WGS84
points_raw <- sp::SpatialPoints(dat[,.(LONGDEC, LATDEC)], proj4string = CRS(wgs_crs))

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
  
    # ## Precise plotting
    # plot(ausmask, axes = FALSE, legend = FALSE, box = FALSE)
    # plot(points_raw, add = TRUE, pch = 18, col = "tomato3", cex = 0.5)

## Checks for out-of-extent lat/longs
dim(dat[which(LONGDEC < ausmask@extent@xmin)])
dim(dat[which(LONGDEC > ausmask@extent@xmax)])
dim(dat[which(LATDEC < ausmask@extent@ymin)])
dim(dat[which(LATDEC > ausmask@extent@ymax)])

## Remove points falling off extent
dim(dat)
dat <- dat[which(LONGDEC >= ausmask@extent@xmin)]
dat <- dat[which(LONGDEC <= ausmask@extent@xmax)]
dat <- dat[which(LATDEC >= ausmask@extent@ymin)]
dat <- dat[which(LATDEC <= ausmask@extent@ymax)]
dim(dat)

## Rename data columns
dat <- dat[,-c(2,3,5:11,13:20)]
dat <- dat[,c(7, 1:3,8,4:6,9,10)]
names(dat) <- dat_cols
setorder(dat, scientificName)
str(dat)
head(dat)
saveRDS(dat, file = file.path(nonala_data, "wa_museum.rds"))


## Combine datasets& clean ####
rm(dat)
dat <- do.call("rbind", lapply(list.files(nonala_data, 
                                          pattern = ".rds$", 
                                          full.names = TRUE), readRDS))
setDT(dat)
str(dat)
dim(dat)
  # ## Checks
  # x <- as.numeric(dat$longitude)
  # which(is.na(x))

## >> Clean species names ####
## JM: Remove records with genus name ending in 'dae' as this indicates family level information
x <- unique(grep("dae ", dat$scientificName, value = TRUE))
  # write.csv(x, file = file.path(output_dir, "dae_names.csv"), row.names = FALSE)
dat <- dat[!(scientificName %in% x)]

## Remove records with single word names ending in 'dae'
x <- unique(grep("dae$", dat$scientificName, value = TRUE))
x <- x[which(sapply(strsplit(as.character(x), " "), length) == 1)]
dat <- dat[!(scientificName %in% x)]

## Remove genus indet. (genus indeterminnate)
x <- unique(grep("Genus indet.", dat$scientificName, value = TRUE))
dat <- dat[!(scientificName %in% x)]
unique(grep(toupper("Genus indet."), dat$scientificName, value = TRUE))

x <- unique(grep("gen. indet", dat$scientificName, value = TRUE))
dat <- dat[!(scientificName %in% x)]
unique(grep(toupper("gen. indet"), dat$scientificName, value = TRUE))

x <- unique(grep("gen. indent", dat$scientificName, value = TRUE))
dat <- dat[!(scientificName %in% x)]
unique(grep(toupper("gen. indent"), dat$scientificName, value = TRUE))

## Remove 'formes'
x <- unique(grep("FORMES", dat$scientificName, value = TRUE))
dat <- dat[!(scientificName %in% x)]

## More name text searching
dat[grep("\\?", dat$scientificName),]$scientificName
dat[grep("\\=", dat$scientificName),]$scientificName
grep("gen.", dat$scientificName, value = TRUE)

  # ## Find improper names without class and family information
  # x <- unique(dat$scientificName)
  # unique(sapply(strsplit(as.character(x), " "), length))
  # 
  # x[which(sapply(strsplit(as.character(x), " "), length) == 7)]
  # 
  # n = 9
  # dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)]]
  # 
  # n = 8
  # dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)]]
  # sum(is.na(dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)], family]))
  # sum(is.na(dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)], class]))
  # 
  # n = 7
  # dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)]]
  # sum(is.na(dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)], family]))
  # sum(is.na(dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)], class]))
  # 
  # n = 6
  # dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)]]
  # sum(is.na(dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)], family]))
  # sum(is.na(dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)], class]))
  # 
  # n = 5
  # dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)]]
  # sum(is.na(dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)], family]))
  # sum(is.na(dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)], class]))
  # 
  # n = 4
  # dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)]]
  # sum(is.na(dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)], family]))
  # sum(is.na(dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)], class]))
  # 
  # temp <- dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)]]
  # t4 <- temp[is.na(class) | is.na(family)]
  # 
  # n = 3
  # dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)]]
  # sum(is.na(dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)], family]))
  # sum(is.na(dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)], class]))
  # 
  # temp <- dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)]]
  # t3 <- temp[is.na(class) | is.na(family)]
  # 
  # n = 2
  # dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)]]
  # sum(is.na(dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)], family]))
  # sum(is.na(dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)], class]))
  # 
  # temp <- dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)]]
  # t2 <- temp[is.na(class) | is.na(family)]
  # 
  # n = 1
  # dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)]]
  # sum(is.na(dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)], family]))
  # sum(is.na(dat[scientificName %in% x[which(sapply(strsplit(as.character(x), " "), length) == n)], class]))
  # 
  # t <- rbind(t1, t2, t3)
  # dim(t)
  # t <- setDT(t)[order(scientificName), .SD[1L] ,.(data_source, class, family, scientificName)]
  # dim(t)
  # t <- t[,.(data_source, class, family, scientificName)]
  # write.csv(t, file = file.path(output_dir, "incomplete_names_nonALA.csv"), row.names = FALSE)

## JM: Supunna picta to be replaced by synonym Nyssus coloripes
## See incomplete_names_nonALA_JRM_highlighted.xlsx
grep("Supunna picta", afd$afd_ValidName)
grep("Nyssus coloripes", afd$afd_ValidName)
afd[grep("Nyssus coloripes", afd$afd_ValidName),]

x <- unique(dat$scientificName)
length(grep("Supunna picta", dat$scientificName, value = TRUE))
length(grep("Nyssus coloripes", dat$scientificName, value = TRUE))
dat[scientificName == "Supunna picta"]$scientificName = "Nyssus coloripes"
length(grep("Supunna picta", dat$scientificName, value = TRUE))
length(grep("Nyssus coloripes", dat$scientificName, value = TRUE))

## JM: Omoedus orbiculatus to be replaced by synonym Zenodorus orbiculatus
## See incomplete_names_nonALA_JRM.xlsx
grep("Omoedus orbiculatus", afd$afd_ValidName)
grep("Zenodorus orbiculatus", afd$afd_ValidName)
afd[grep("Zenodorus orbiculatus", afd$afd_ValidName),]

x <- unique(dat$scientificName)
length(grep("Omoedus orbiculatus", dat$scientificName, value = TRUE))
length(grep("Zenodorus orbiculatus", dat$scientificName, value = TRUE))
dat[scientificName == "Omoedus orbiculatus"]$scientificName = "Zenodorus orbiculatus"
length(grep("Omoedus orbiculatus", dat$scientificName, value = TRUE))
length(grep("Zenodorus orbiculatus", dat$scientificName, value = TRUE))

## JM: Remove species marked as exclude in incomplete_names_nonALA_JRM_highlighted.csv and incomplete_names_nonALA_JRM.csv
exclude1 <- fread(file.path(nonala_data, "incomplete_names_nonALA_JRM_highlighted.csv"))
exclude1 <- exclude1[exclude != ""]
exclude2 <- fread(file.path(nonala_data, "incomplete_names_nonALA_JRM.csv"))
exclude2 <- exclude2[exclude != ""]
exclude <- rbind(exclude1, exclude2)

dat <- dat[!(scientificName %in% exclude$scientificName)]
rm(exclude, exclude1, exclude2)


## >> Add tax info for species with class/family = NA ####
## >> Get tax info from AFD
## List of names with NAs in class/family
na_tofill <- c(unique(dat[is.na(class)]$scientificName), unique(dat[is.na(family)]$scientificName))
na_tofill <- na_tofill[!duplicated(na_tofill)]

afd_info <- data.table()
for (sp in na_tofill){
  if (length(unique(afd[which(afd$afd_ValidName %in% sp)]$afd_ValidName)) > 1){
    warning(paste0("More than 1 unique taxon info found for ", sp))
    
  } else {
    if (length(unique(afd[which(afd$afd_ValidName %in% sp)]$afd_ValidName)) == 0) {
      warning(paste0("No taxon info found for ", sp))
    } else {
      x <- unique((afd[.(sp)]))
      afd_info <- rbind(afd_info, cbind(scientificName = sp, x))
    }
  }
}
warnings()
sum(duplicated(afd_info))
afd_info <- setDT(afd_info, key = "scientificName")

## Add tax info to data
for (sp in afd_info$scientificName){
  dat[scientificName == sp]$class = afd_info[scientificName == sp]$afd_Class
  dat[scientificName == sp]$family = afd_info[scientificName == sp]$afd_Family
}

sum(is.na(dat$class))
sum(is.na(dat$family))

## >> Get class and family information from incomplete_names_nonALA_JRM.csv
## List of names with NAs in class/family
na_tofill <- c(unique(dat[is.na(class)]$scientificName), unique(dat[is.na(family)]$scientificName))
na_tofill <- na_tofill[!duplicated(na_tofill)]

## Load incomplete_names_nonALA_JRM.csv
taxinfo <- fread(file.path(nonala_data, "incomplete_names_nonALA_JRM.csv"))
taxinfo <- taxinfo[exclude == ""]
taxinfo <- taxinfo[,-"exclude"]
sum(duplicated(taxinfo))
sum(duplicated(taxinfo$scientificName))
taxinfo[which(duplicated(taxinfo$scientificName)),]$scientificName
taxinfo[scientificName == "Austroaeschna parvistigma"]
taxinfo[scientificName == "Cheumatopsyche sp."]
taxinfo[scientificName == "Triplectides sp."]
taxinfo <- taxinfo[!duplicated(taxinfo[,.(class, family, scientificName)])]

message(cat("All names in taxinfo contained in ALA data: "),
        all(taxinfo$scientificName %in% na_tofill))

message(cat("All names in ALA data with NA for class/family in taxinfo: "),
        all(na_tofill %in% taxinfo$scientificName))
na_tofill[!(na_tofill %in% taxinfo$scientificName)]
unique(dat[scientificName %in% na_tofill[!(na_tofill %in% taxinfo$scientificName)]]$data_source)
  ## To be dealt with below...

## Add tax info to data
for (sp in taxinfo$scientificName){
  dat[scientificName %in% sp]$class = taxinfo[scientificName == sp]$class
  dat[scientificName %in% sp]$family = taxinfo[scientificName == sp]$family
}

sum(is.na(dat$class))
sum(is.na(dat$family))

## >> Class/info info remining speces
na_tofill <- c(unique(dat[is.na(class)]$scientificName), unique(dat[is.na(family)]$scientificName))
na_tofill <- na_tofill[!duplicated(na_tofill)]

## JM: delete "Phasmatodea sp."
na_tofill <- na_tofill[-2]
dat <- dat[!(scientificName %in% "Phasmatodea sp.")]

## Info provided by JM
taxinfo <- data.table()
taxinfo$scientificName <- na_tofill
taxinfo$family <- c("Formicidae", "Hydropsychidae", 
                    "Formicidae", "Formicidae", "Leptoceridae")
taxinfo$class <- rep("Insecta", nrow(taxinfo))

  ## Check where the species data is from 
  unique(dat[scientificName %in% na_tofill]$data_source)

## Add tax info to data
for (sp in taxinfo$scientificName){
  dat[scientificName %in% sp]$class = taxinfo[scientificName == sp]$class
  dat[scientificName %in% sp]$family = taxinfo[scientificName == sp]$family
}

sum(is.na(dat$class))
sum(is.na(dat$family))

## >> Remove duplictaes ####
sum(duplicated(dat))
  # ## Check
  # ids <- which(duplicated(dat))
  # all(dat[ids,]$id == dat[ids,]$id)
dat <- dat[!duplicated(dat)]
dim(dat)

## >> Mask data ####
points_raw <- sp::SpatialPoints(dat[,.(longitude, latitude)], proj4string = CRS(wgs_crs))

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

  # ## Precise plotting
  # plot(ausmask, axes = FALSE, legend = FALSE, box = FALSE)
  # plot(points_raw, add = TRUE, pch = 18, col = "tomato3", cex = 0.5)

## Checks for out-of-extent lat/longs
dim(dat[which(longitude < ausmask@extent@xmin)])
dim(dat[which(longitude > ausmask@extent@xmax)])
dim(dat[which(latitude < ausmask@extent@ymin)])
dim(dat[which(latitude > ausmask@extent@ymax)])

# write.csv(dat, file = file.path(output_dir, paste0("premask_nonala_", Sys.Date(),".csv")), 
#           row.names = FALSE)

  # ## Clip data by mask extent - if points falling off mask
  # dat <- dat[which(longitude >= ausmask@extent@xmin)]
  # dat <- dat[which(longitude <= ausmask@extent@xmax)]
  # dat <- dat[which(latitude >= ausmask@extent@ymin)]
  # dat <- dat[which(latitude <= ausmask@extent@ymax)]

## Clip data/occurrence points if they fall outside mask polygon(s)
sp <- SpatialPoints(dat[,c("longitude", "latitude")], 
                    proj4string = crs(ausmask))
grd.pts <-extract(ausmask, sp)

## Subset data - HERE THERE MIGHT BE PROBLEM WITH ALIGNMENT OF DATA AND MASK
dat0 <- dat[is.na(grd.pts),]
dat1 <- dat[!is.na(grd.pts),]
message(cat("Records falling on NAs on mask and therefore removed: "),
        dim(dat0)[1])

## Precise plotting
points1 <- sp::SpatialPoints(dat1[,.(longitude, latitude)], proj4string = CRS(wgs_crs))
points0 <- sp::SpatialPoints(dat0[,.(longitude, latitude)], proj4string = CRS(wgs_crs))
plot(ausmask, axes = FALSE, legend = FALSE, box = FALSE)
plot(points1, add = TRUE, pch = 18, col = "tomato3", cex = 0.5)
plot(points0, add = TRUE, pch = 18, col = "green", cex = 0.5)


## >> Create spfile column ####
## THIS IS NOT QUITE RIGHT BECAUSE WE GET DUPLICATES
## BUT MAYBE DOESN'T MATTER BECAUSE THE IMPROPER NAMES WON'T BE FOUND IN ALA DATA ANYWAY
dat1$spfile <- str_replace_all(dat1$scientificName, " ", "00xx00")
dat1$spfile <- str_replace_all(dat1$spfile, "[^[:alnum:]]", "")
dat1$spfile <- tolower(gsub("00xx00", "_", dat1$spfile))
length(unique(dat1$scientificName))
length(unique(dat1$spfile))
# ## The following steps are done in a subsequent script to allow unique spfiles for each scientificName
# ## They cannot be run yet because it causes problem when matching names with ala data 
  # ## Create new_id column to give unique ID by scientific name
  # setDT(dat1)[, new_id := .GRP, by = scientificName]
  # ## Merge spfile and new_ID
  # dat1$spfile <- paste0(dat1$spfile, "_", dat1$new_id)
  # length(unique(dat1$scientificName))
  # length(unique(dat1$spfile))


## >> Save species data file ####
write.csv(dat1, file = file.path(output_dir, paste0("clean1_nonala_", Sys.Date(),".csv")), 
          row.names = FALSE)


# ## EXTRAS
# my_string <- "startABC~!@#$%^&*()_+=-{}[]|\"':;<,>.?/XYXend"
# str_replace_all(my_string, "[^[:alnum:]]", "")
# str_replace_all(my_string, "[[:punct:]]", "")