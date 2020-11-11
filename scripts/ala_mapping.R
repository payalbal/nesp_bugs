## ALA data mapping
## For indivial species with > = 5 records
## Mask including islands and offshore territories + buffer
## Species habitat polygons for EOO and AOO
## Species distribution (wide/restricted)
## Prelim analysis area (in/out; prop of habitat within) - to clip
## Fire extent (yes/no; prop of habitat impacted) - 

## For all species
## Think or resolution of analyses. 
## Stacked polygons or points per grid?


##
## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "sp", "raster", "rgdal")
lapply(x, require, character.only = TRUE)
rm(x)

## Server paths
## Server paths
bugs_data = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
mask_data = file.path(bugs_data, "masks")
output_dir = file.path(bugs_data, "outputs")
spdata_dir = file.path(output_dir, "ala_data" ,"spdata")

## Species files
spfiles <- list.files(spdata_dir, recursive = FALSE, full.names = TRUE)
spfiles <- spfiles[!file.info(spfiles)$isdir]


## Subset to species with > = 5 records
countMTE5 <- as.data.table(read.csv(file.path(output_dir, "countMTE5.csv")))
y <- countMTE5$scientificName
y <- stringr::str_replace_all(y, " ", "00xx00")
y <- stringr::str_replace_all(y, "[^[:alnum:]]", "")
y <- tolower(gsub("00xx00", "_", y))
y <- sort(unique(y))

spfiles <- spfiles[tools::file_path_sans_ext(basename(spfiles)) %in% y]
message(cat("Number of files retained same as number of species with > = 5 records: "),
        length(spfiles) == dim(countMTE5)[1])


## Load mask
ausmask <- file.path(output_dir, "masks", "ausmask_noaa_1km.tif")
ausmask <- raster(ausmask)
plot(ausmask, axes = FALSE, box = FALSE, legend = FALSE)


## Preliminary analysis area
paa <- file.path(bugs_data, "Preliminary_Analysis_Areas/prelim_analysis_areas_dissolve.shp")
paa <- rgdal::readOGR(paa)
plot(paa, add=TRUE)

## Fire layers
fireseverity <- raster(file.path(bugs_data, "fire/AUS_GEEBAM_Fire_Severity_NIAFED20200224/AUS_GEEBAM_Fire_Severity_NIAFED20200224.tif"))
fireextent <- rgdal::readOGR(file.path(bugs_data, "fire/NIAFED_v20200623/NIAFED_20190701_20200622_v20200623.shp"))


## Fire overlap analyses

## Species maps
qa[which(qa$exclude == 1),]$name
