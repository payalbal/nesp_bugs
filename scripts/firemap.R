## Reclassified fire map

## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "sp", "raster", "rgdal", "gdalUtils")
lapply(x, require, character.only = TRUE)
rm(x)

## Server paths
bugs_data = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
mask_data = file.path(bugs_data, "masks")
output_dir = file.path(bugs_data, "outputs", "fire")
dir.create(output_dir)

## NIAFED fire data
fireseverity <- raster(file.path(bugs_data, "fire/AUS_GEEBAM_Fire_Severity_NIAFED20200224_QGIS/AUS_GEEBAM_Fire_Severity_QGIS_NIAFED20200224.tif"))
fireextent <- rgdal::readOGR(file.path(bugs_data, "fire/NIAFED_v20200623/NIAFED_20190701_20200622_v20200623.shp"))

## Fire severity layer processing ####

## Create buffer (20 km as per previous discussions with DELP) - ask JW
...


## Reclassify from 5 to 3 fire classes (if using buffer, leave buffer as a separate class)

# ## Using gdal_calc.py - NOT WORKING
# infile <- file.path(bugs_data, "fire/AUS_GEEBAM_Fire_Severity_NIAFED20200224_QGIS/AUS_GEEBAM_Fire_Severity_QGIS_NIAFED20200224.tif")
# gdalUtils::gdalinfo(infile)
# outfile <- file.path(output_dir, "severity3.tif")
# system(paste0("gdal_calc.py -A ", infile, " --calc=\"(A==1)*1 + ((A==2)+(A==3))*2 + ((A==4)+(A==5))*3\" --NoDataValue=0 --outfile ", outfile))
# raster(outfile)

## Using the raster package
reclassMatrix <- matrix(c(1,1,
                          2,2,
                          3,2,
                          4,3,
                          5,3),5,2,byrow = TRUE)
out <- raster::reclassify(fireseverity, rcl = reclassMatrix)
writeRaster(out, filename = file.path(output_dir, "severity3_raster.tif"))


## Reproject to Equal area 
## See Proj4 in https://spatialreference.org/ref/sr-org/australia-albers-equal-area-conic-134/
infile <- file.path(output_dir, "severity3_raster.tif")
outfile <- file.path(output_dir, "severity3_eqar.tif")
wgs_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
# crs(fireseverity)
new_crs = "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
gdalUtils::gdalwarp(infile, outfile, s_srs = wgs_crs, t_srs = new_crs)

## Resample to 250m resolution (from ~40m resolution)
## https://gis.stackexchange.com/questions/255150/using-resample-vs-aggregate-extend-in-r-to-have-rasters-of-matching-resolutio
infile <- outfile
outfile <- file.path(output_dir, "severity3_eqar_250avg.tif")
gdalUtils::gdalwarp(infile, outfile, tr = c(250,250), r = "average") 

outfile <- file.path(output_dir, "severity3_eqar_250mode.tif")
gdalUtils::gdalwarp(infile, outfile, tr = c(250,250), r = "mode") 


## Reproject PAA to equal area






## Preliminary analysis area
paa <- file.path(bugs_data, "Preliminary_Analysis_Areas/prelim_analysis_areas_dissolve.shp")
paa <- rgdal::readOGR(paa)

shapefile(paa, )