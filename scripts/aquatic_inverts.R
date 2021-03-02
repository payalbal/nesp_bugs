## Aquatic innvertebrates


## Set working environment ####
rm(list = ls())
gc()

x <- c("data.table", "sp", "raster")
lapply(x, require, character.only = TRUE)
rm(x)

bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
aqua_dir = file.path(bugs_dir, "aquatics")
output_dir = file.path(bugs_dir, "outputs")
spdata_dir = file.path(output_dir, "ala_nonala_data" ,"spdata")


## Subset data ####
aqualist <- fread(file.path(aqua_dir, "aquatic_subset_JM.csv"))
aqualist <- tolower(gsub(" ", "_", aqualist$Species))


spfiles <- list.files(spdata_dir, pattern= ".rds$", full.names = TRUE)
all_sp <- basename(tools::file_path_sans_ext(spfiles))
all_sp <- gsub("\\d+$", "", all_sp) ## removed digits at the end of a string only
all_sp <- gsub("_$", "", all_sp)
aqualist %in% all_sp
aquafiles <- spfiles[all_sp %in% aqualist]

dat <- do.call("rbind", lapply(aquafiles, readRDS))


## >> Precise plotting
mask_file <- file.path(output_dir, "masks", "ausmask_noaa_1kmWGS_NA.tif")
ausmask <- raster::raster(mask_file)
wgs_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

points <- sp::SpatialPoints(dat[,.(longitude, latitude)], proj4string = CRS(wgs_crs))
plot(ausmask, axes = FALSE, legend = FALSE, box = FALSE)
plot(points, add = TRUE, pch = 18, col = "tomato3", cex = 0.5)



## preliminary analysis area
paa <- file.path(bugs_data, "Preliminary_Analysis_Areas/prelim_analysis_areas_dissolve.shp")
paa <- rgdal::readOGR(paa)


## Slug risk layer 
slug <- file.path(aqua_dir, "ward_slugrisk", "stream_Rusle_Studysite_3.shp")
slug <- rgdal::readOGR(slug)



## Overlap 
