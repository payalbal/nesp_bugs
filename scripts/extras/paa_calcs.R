## PAA calcs

## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "sp", "raster", "rgdal", "gdalUtils", "rgeos", "usethis")
lapply(x, require, character.only = TRUE)
rm(x)

## File paths
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")
paa_file <- file.path(output_dir, "fire", "prelim_analysis_areas_dissolve_eqar.shp") 
paa_shp <- rgdal::readOGR(paa_file)
gArea(paa_shp)


## Calulate PAA overlaps with GEEBAM clipped to native ####
fire_severity <- raster(file.path(output_dir, "fire", "severity5_eqar250_native_paa.tif"))
fire_vals <- fire_severity[]
fire_classes <- sort(unique(na.omit(fire_vals)))
fire_res <- res(fire_severity)
fire_crs <- as.character(crs(fire_severity))
fire_extent <- extent(fire_severity)

## Rasterise PAA
system(paste0("gdal_rasterize -at -burn 1 -ot Byte -tr ",
              paste(fire_res, collapse = " "), " -te ", 
              paste(fire_extent[1], fire_extent[3], 
                    fire_extent[2], fire_extent[4]), " ",
              " -l prelim_analysis_areas_dissolve_eqar ",
              paa_file, " ",
              gsub(".shp", ".tif", paa_file)))

paa <- raster(gsub(".shp", ".tif", paa_file))

## Create table of areas within each fire class
dt <- data.table("paa" = paa[],
                 "fire_vals" = fire_vals)

df <- data.frame(matrix(ncol = length(fire_classes) + 2))
colnames(df) <- c(paste0("Fire_Class_", fire_classes), "Total_PAA_inFire",
                  "Total_PAA_Raster")
df[, 1:(ncol(df)-2)] <- sapply(fire_classes, FUN = function(x) dt[paa == 1 & fire_vals == x, length(fire_vals) * 250 * 250 / 1000000])
df[, ncol(df)-1] <- sum(df[, 1:(ncol(df)-2)])
df[, ncol(df)] <- dt[paa == 1, length(paa)* 250 * 250 / 1000000]
df$Total_PAA_Shapefile <- gArea(paa_shp)/1000000
  ## difference of ~6 cells between raster and shapefile calcs
paa1 <- df
rm(df, dt, fire_severity, fire_vals, 
   fire_classes, fire_crs, fire_extent, fire_res)


## Calulate PAA overlaps with GEEBAM not clipped to native ####
fire_severity <- raster(file.path(output_dir, "fire", "severity5_eqar250.tif"))
fire_vals <- fire_severity[]
fire_classes <- sort(unique(na.omit(fire_vals)))
fire_res <- res(fire_severity)
fire_crs <- as.character(crs(fire_severity))
fire_extent <- extent(fire_severity)

## Create table of areas within each fire class
dt <- data.table("paa" = paa[],
                 "fire_vals" = fire_vals)

df <- data.frame(matrix(ncol = length(fire_classes) + 2))
colnames(df) <- c(paste0("Fire_Class_", fire_classes), "Total_PAA_inFire",
                  "Total_PAA_Raster")
df[, 1:(ncol(df)-2)] <- sapply(fire_classes, FUN = function(x) dt[paa == 1 & fire_vals == x, length(fire_vals) * 250 * 250 / 1000000])
df[, ncol(df)-1] <- sum(df[, 1:(ncol(df)-2)])
df[, ncol(df)] <- dt[paa == 1, length(paa)* 250 * 250 / 1000000]
df$Total_PAA_Shapefile <- gArea(paa_shp)/1000000
  ## difference of ~6 cells between raster and shapefile calcs

paa2 <- df
rm(df, dt, fire_severity, fire_vals, 
   fire_classes, fire_crs, fire_extent, fire_res)

dt <- rbind(paa1, paa2)
rownames(dt) <- c("GEEBAM_clipped_with_nativeveg", "GEEBAM_not_clipped")
write.csv(t(dt), file = file.path(output_dir, "paa_clacs.csv"), 
          row.names = FALSE )

## Calculate PAA area overlapping with native veg ####
## Clip native veg raster with Preliminary analysis area shapefile
infile <- file.path(output_dir, "native_vegetation", "nvis_v6_reclass.tif")
outfile <- gsub(".tif", "_paa.tif", infile)

system(paste0("gdalwarp -overwrite ", "-te ", 
              paste(fire_extent[1], fire_extent[3], 
                    fire_extent[2], fire_extent[4]), 
              " -of GTiff -cutline ", paa_file, 
              " -cl prelim_analysis_areas_dissolve_eqar ",
              infile, " ", outfile))

paa <- raster(gsub(".shp", ".tif", paa_file))
native <- raster(outfile)
native_vals <- native[]
native_classes <- sort(unique(na.omit(native_vals)))
dt <- data.table("paa" = paa[],
                 "native_vals" = native_vals)
message(cat("Total PAA within native veg: "),
        dt[paa == 1 & native_vals == 1, length(native_vals) * 250 * 250 / 1000000])

