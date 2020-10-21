## ALA data mapping
## For indiovial species
## Mask including islands and offshore territories + buffer
## Species habitat polygons
## Species distribution (wide/restricted)
## Prelim analysis area (in/out; prop of habitat within)
## Fire extent (yes/no; prop of habitat impacted)

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
output_dir = file.path(getwd(), "nesp_bugs", "outputs")
data_dir = file.path(output_dir, "ala_data" ,"spdata")
dir.create(data_dir)
map_dir = file.path(data_dir, "spmaps_unmasked")
dir.create(map_dir)

# ## Local paths
# output_dir = "/Volumes/uom_data/nesp_bugs_data/outputs"
# data_dir = file.path(output_dir,  "ala_data" ,"spdata")
# dir.create(data_dir)
# map_dir = file.path(data_dir, "spmaps_unmasked")
# dir.create(map_dir)

## Mask
ausmask <- file.path(output_dir, "aus_mainland_WGS.tif")
ausmask <- raster(ausmask)
plot(ausmask)
