## Processing for regional layers - to run once only


## Set working environment
x <- c("data.table", "sp", "raster", "rgdal", 
       "gdalUtils", "rgeos")
lapply(x, require, character.only = TRUE)
rm(x)

bugs_dir = "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")

if(!dir.exists(file.path(bugs_dir, "outputs", "regions"))){
  dir.create(file.path(bugs_dir, "outputs", "regions"))
}


## State polygons
## Source: ...
state_poly <- readOGR(file.path(output_dir, "masks", "auslands_wgs84.shp"), layer = "STATE") 



## Bushfire recovery region polygons 
## Source: ...
## NOTE: Needs to be updated to include WA? (see notes in doc)
recovery_poly <- readOGR(file.path(bugs_dir, "Bushfire_recovery_regions", "regions.shp")
                        , layer = "regions")

## Reproject bushfire recovery regions to match fire severity raster
infile <- file.path(bugs_dir, "Bushfire_recovery_regions", "regions.shp")
outfile <- file.path(output_dir, "regions", "bushfire_recovery.tif")
system(paste0("gdal_rasterize -at -a id -ot Byte -te -2214250 -4876750 2187750 -1110750 -tr 250 250 -a_srs '+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' -l regions ",
              infile, " ", outfile))
## rdal_rasterise page says: Note that on the fly reprojection of vector data to the coordinate system of the raster data is only supported since GDAL 2.1.0.
raster(outfile)
recovery_vals <- raster(outfile)[]
recovery_classes <- sort(unique(na.omit(recovery_vals)))


## >> Load in fire severity raster (re-classed) and get unique classes ####
fire_severity <- raster(file.path(output_dir, "fire", "severity3_eqar250.tif"))
fire_vals <- fire_severity[]
fire_classes <- sort(unique(na.omit(fire_vals)))

polygon_overlap <- function(species_name, species_poly, boundary_poly_filename, shapefile_dir, fire_vals, fire_classes, outdir){
  
  ## Write spatial data to disk if coming from RDS file
  writeOGR(species_poly, dsn = shapefile_dir, layer = species_name, driver = "ESRI Shapefile", overwrite_layer = TRUE)
  
  ## Reproject species boundaries to match fire severity raster
  system(paste0("gdal_rasterize -at -burn 1 -ot Byte -tr .0025 .0025 -l ",
                species_name, " ",
                file.path(shapefile_dir, species_name), ".shp ",
                file.path(shapefile_dir, species_name), ".tif"))
  
  system(paste0("gdalwarp -overwrite -ot Byte -te -2214250 -4876750 2187750 -1110750 -tr 250 250 -s_srs 'EPSG:4326' -t_srs '+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' ",
                file.path(shapefile_dir, species_name), ".tif ",
                file.path(shapefile_dir, species_name), "_p.tif"))
  
  ## Create table of areas within each fire class
  species_map <- raster(paste0(file.path(shapefile_dir, species_name), "_p.tif"))
  boundary_map <- raster(paste0(file.path(shapefile_dir, boundary_poly_filename), "_p.tif"))
  
  dt <- data.table("species_map" = species_map[],
                   "boundary_map" = boundary_map[],
                   "fire_severity" = fire_vals)
  
  df <- data.frame(matrix(ncol = length(fire_classes) + 4))
  df[ , 1] <- species_name
  df[ , -c(1, (ncol(df)-2):ncol(df))] <- sapply(fire_classes, FUN = function(x) dt[species_map == 1 & boundary_map == 1 & fire_severity == x, length(fire_severity) * 250 * 250 / 1000000])
  df[ , ncol(df)-2] <- rowSums(df[, -c(1, (ncol(df)-2):ncol(df))])
  df[ , ncol(df)-1] <- dt[species_map == 1, length(species_map)* 250 * 250 / 1000000]
  df[ , ncol(df)] <- (df[ , ncol(df)-2]/df[ , ncol(df)-1])*100
  colnames(df) <- c("Species", paste0("Fire_Class_", fire_classes), "Total_Overlap", "Species_Polygon", "Percent_Overlap")
  
  ## Remove files
  file.remove(file.path(shapefile_dir, dir(path = shapefile_dir)))
  
  ## Save output as csv
  write.csv(df, file = paste0(file.path(outdir, species_name), ".csv"), row.names = FALSE)
  
}