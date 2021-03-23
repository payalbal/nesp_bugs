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

all.data <- fread(file.path(output_dir, "data_ALAnonALA.csv"))
all_sp <- gsub("\\d+$", "", all.data$spfile) ## removed digits at the end of a string only
all_sp <- gsub("_$", "", all_sp)

dat <- all.data[all_sp %in% aqualist]

  # ## Same as...
  # spfiles <- list.files(spdata_dir, pattern= ".rds$", full.names = TRUE)
  # all_sp <- basename(tools::file_path_sans_ext(spfiles))
  # all_sp <- gsub("\\d+$", "", all_sp) ## removed digits at the end of a string only
  # all_sp <- gsub("_$", "", all_sp)
  # aqualist %in% all_sp
  # aquafiles <- spfiles[all_sp %in% aqualist]
  # dat <- do.call("rbind", lapply(aquafiles, readRDS))


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

slug
slug@proj4string
names(slug@data)
slug <-  slug["RUSLERF_LH"]

writeOGR(slug, dsn = file.path(output_dir, "slugrisk"), layer = "slugrisk_RUSLERF_LH", driver = "ESRI Shapefile", overwrite_layer = TRUE)

## Rasterise
## Convert from GDA94 to Albers equal area
t_crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs" ## EPSG:4283
s_crs = "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" ## albers equal area

infile <- file.path(output_dir, "slugrisk" ,"slugrisk_RUSLERF_LH.shp")
outfile <- gsub("_RUSLERF_LH.shp$", ".tif", infile)

system(paste0("gdal_rasterize -a RUSLERF_LH -at -ot Byte -tr .0025 .0025 -l RUSLERF_LH ",
              infile, " ",
              outfile))

## Reproject to AEA
infile <- outfile 
outfile <- file.path(aqua_dir, "slugrisk_reproj.tif")

system(paste0("gdalwarp -overwrite -ot Byte -te -2214250 -4876750 2187750 -1110750 -tr 250 250 -s_srs 'EPSG:4283' -t_srs '+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' ",
              infile,
              outfile))


## Overlap 
