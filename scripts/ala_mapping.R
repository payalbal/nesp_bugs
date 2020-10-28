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

x <- c("data.table", "sp", "raster")
lapply(x, require, character.only = TRUE)
rm(x)

## Server paths
bugs_data = file.path(getwd(), "nesp_bugs", "nesp_bugs_data")
output_dir = file.path(getwd(), "nesp_bugs", "outputs")
spdata_dir = file.path(output_dir, "ala_data" ,"spdata")


# ## Local paths
# bugs_data = bugs_data = "/Volumes/uom_data/nesp_bugs_data"
# output_dir = "/Volumes/uom_data/nesp_bugs_data/outputs"
# data_dir = file.path(output_dir,  "ala_data" ,"spdata")
# dir.create(data_dir)
# map_dir = file.path(data_dir, "spmaps_unmasked")
# dir.create(map_dir)


## Species files


## List of species with > = 5 records
countMTE5 <- as.data.table(read.csv(file.path(output_dir, "countMTE5.csv")))
qa[which(qa$exclude == 1),]$name


## Mask...
ausmask <- file.path(output_dir, "aus_mainland_WGS.tif")
ausmask <- raster(ausmask)
plot(ausmask)



## Preliminary analysis area

## Fire severity
severity <- raster(file.path(bugs_data, "fire/AUS_GEEBAM_Fire_Severity_NIAFED20200224/AUS_GEEBAM_Fire_Severity_NIAFED20200224.tif"))

extent <- rgdal::readOGR(file.path(bugs_data, "fire/NIAFED_v20200623/NIAFED_20190701_20200622_v20200623.shp"))

