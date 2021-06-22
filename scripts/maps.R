
## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "rgdal", "sp")
lapply(x, require, character.only = TRUE)
rm(x)


## Files and folder
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")
spdata_dir = file.path(output_dir, "ala_nonala_data" ,"spdata")


## Map species shapefiles > raster > fire ####
## EOO shapefiles were not created
## >> Species raster clipped to PAA
shapefile_dir = file.path(output_dir, "species_shapefiles")

fire_severity <- raster::raster(file.path(output_dir, "fire", "severity5_eqar250_native_paa.tif"))
fire_res <- raster::res(fire_severity)
fire_crs <- as.character(raster::crs(fire_severity))
fire_extent <- raster::extent(fire_severity)

species_maps <- readRDS(file.path(output_dir, "species_ahullEOOspdf.rds"))
out <- fread(file.path(output_dir, "invert_overlap_2021-06-19.csv"))

polygon_list <- out[PAA_Points == 0 & Total_fire_polygon != 0]$spfile
species_name <- polygon_list[1]
species_poly = species_maps[[species_name]]

rgdal::writeOGR(species_poly, dsn = shapefile_dir, layer = species_name, driver = "ESRI Shapefile", overwrite_layer = TRUE)

system(paste0("gdal_rasterize -at -burn 1 -ot Byte -tr .0025 .0025 -l ",
              species_name, " ",
              file.path(shapefile_dir, species_name), ".shp ",
              file.path(shapefile_dir, species_name), ".tif"))

system(paste0("gdalwarp -overwrite -ot Byte -tr ",
              paste(fire_res, collapse = " "), " -te ",
              paste(fire_extent[1], fire_extent[3],
                    fire_extent[2], fire_extent[4]),
              " -s_srs 'EPSG:4326' -t_srs '", fire_crs, "' ",
              file.path(shapefile_dir, species_name), ".tif ",
              file.path(shapefile_dir, species_name), "_p.tif"))


## Map species points ####
spfiles <- list.files(spdata_dir, pattern= ".rds$", full.names = TRUE)
dat <- spfiles[grep(species_name, spfiles)]
write.csv(readRDS(dat), file = file.path(shapefile_dir, paste0(species_name, ".csv")), row.names = FALSE)

# basemap_file <- file.path(output_dir, "masks", "auslands_1poly_wgs84.shp")
basemap_file <- file.path(output_dir, "masks", "auslands_wgs84.shp")
basemap <- readOGR(basemap_file)
wgs_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

dat <- as.data.table(readRDS(dat))
sp <- SpatialPoints(dat[, .(longitude, latitude)],
                    proj4string = CRS(wgs_crs))
plot(basemap, col = "wheat", main = basename(tools::file_path_sans_ext(spname)))
plot(sp, add = TRUE, pch = 8, cex = 3, col = "tomato3")

## Fire overlap outputs
out <- fread(file.path(output_dir, "invert_overlap_2021-06-11.csv"))
out.paa <- fread(file.path(output_dir, "invert_overlap_PAAonly_2021-06-18.csv"))

out[grep(spname, out$spfile)]
out.paa[grep(spname, out.paa$spfile)]