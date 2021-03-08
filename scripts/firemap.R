## Reclassified fire map

## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "sp", "raster", "rgdal", "gdalUtils", "rgeos")
lapply(x, require, character.only = TRUE)
rm(x)

## File paths
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_data, "outputs", "fire")
if (!dir.exists(output_dir)) {dir.create(output_dir)}
source(file.path("/tempdata/workdir/nesp_bugs/", "scripts", "gdal_calc.R"))  # by jgarber

## Pre-processing fire severity raster - to run once only #####
if(!dir.exists(file.path(bugs_dir, "outputs", "fire"))){
  dir.create(file.path(bugs_dir, "outputs", "fire"))
}

## Reproject raster with GDAL system call
infile <- file.path(bugs_dir, "fire/AUS_GEEBAM_Fire_Severity_NIAFED20200224_QGIS/AUS_GEEBAM_Fire_Severity_QGIS_NIAFED20200224.tif")
outfile <- gsub(".tif", "_reproj.tif", infile)
system(paste0("gdalwarp -overwrite -ot Byte -te -2214250 -4876750 2187750 -1110750 -tr 250 250 ",
              "-t_srs '+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' ", infile, " ", outfile))
gdalUtils::gdalinfo(outfile)

# Reclassify values in raster
infile <- outfile
outfile <- file.path(output_dir, "fire", "severity3_eqar250.tif")
system(paste0("gdal_calc.py -A ", infile,
              " --calc='(A==1)*1 + ((A==2)+(A==3))*2 + ((A==4)+(A==5))*3' --NoDataValue=0",
              " --outfile=", outfile))
gdalUtils::gdalinfo(outfile)

# # Mask fire map
# infile <- file.path(output_dir, "fire", "severity3_eqar250.tif")
# outfile <- file.path(output_dir, "fire", "severity3_eqar250_masked.tif")
# 
# ## step one_clip by e
# 
# ## step two_mask
# gdalmask(infile = infile, mask = mask_file, outfile = outfile, output_Raster = FALSE, overwrite=TRUE, verbose=TRUE)


# ## Input data - NIAFED fire data
# ## Source (severity): http://www.environment.gov.au/fed/catalog/search/resource/details.page?uuid=%7B8CE7D6BE-4A82-40D7-80BC-647CB1FE5C08%7D
# ## source (extent): http://www.environment.gov.au/fed/catalog/search/resource/details.page?uuid=%7B9ACDCB09-0364-4FE8-9459-2A56C792C743%7D
# fireseverity <- raster(file.path(bugs_data, "fire/AUS_GEEBAM_Fire_Severity_NIAFED20200224_QGIS/AUS_GEEBAM_Fire_Severity_QGIS_NIAFED20200224.tif"))
# fireextent <- rgdal::readOGR(file.path(bugs_data, "fire/NIAFED_v20200623/NIAFED_20190701_20200622_v20200623.shp"))
# 
# ## >> Reclassify severity ####
# ## From 5 to 3 fire classes (if using buffer, leave buffer as a separate class)
# infile <- file.path(bugs_data, "fire/AUS_GEEBAM_Fire_Severity_NIAFED20200224_QGIS/AUS_GEEBAM_Fire_Severity_QGIS_NIAFED20200224.tif")
# gdalUtils::gdalinfo(infile)
# outfile <- file.path(output_dir, "severity3.tif")
# system(paste0("gdal_calc.py -A ", infile,
#               " --calc='(A==1)*1 + ((A==2)+(A==3))*2 + ((A==4)+(A==5))*3' --NoDataValue=0",
#               " --outfile=", outfile))
# gdalUtils::gdalinfo(outfile)
# raster(outfile) ## values 0, 255 > 255 implies no data value
# 
# ## Using the raster package - VERY SLOW
# reclassMatrix <- matrix(c(1,1,
#                           2,2,
#                           3,2,
#                           4,3,
#                           5,3),5,2,byrow = TRUE)
# out <- raster::reclassify(fireseverity, rcl = reclassMatrix)
# writeRaster(out, filename = file.path(output_dir, "severity3_raster.tif"))
# 
# ## >> Reproject to Equal area ####
# ## See Proj4 in https://spatialreference.org/ref/sr-org/australia-albers-equal-area-conic-134/
# infile <- file.path(output_dir, "severity3_raster.tif")
# outfile <- file.path(output_dir, "severity3_eqar.tif")
# wgs_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
# # crs(fireseverity)
# new_crs = "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# gdalUtils::gdalwarp(infile, outfile, s_srs = wgs_crs, t_srs = new_crs)
# 
# ## >> Resample to 250m resolution (from ~40m resolution) ####
# ## https://gis.stackexchange.com/questions/255150/using-resample-vs-aggregate-extend-in-r-to-have-rasters-of-matching-resolutio
# infile <- outfile
# outfile <- file.path(output_dir, "severity3_eqar250_near.tif")
# gdalUtils::gdalwarp(infile, outfile, tr = c(250,250), r = "near")
# raster(outfile)
# sampleRandom(raster(outfile), size = 50, ext = extent(raster(outfile)) , na.rm = TRUE, sp=FALSE, asRaster=FALSE) 
# 
# ## >> Convert to polygons ####
# ## In QGIS (see steps data processing document)
# ## In R - not trialled yet
# infile <- file.path(output_dir, "severity3_eqar_250avg.tif")
# outfile <- file.path(output_dir, "severity3_eqar_250avg_polygon2.shp")
# system(paste0("gdal_polygonize.py ", infile, " ", outfile, " -b 1 -f \"ESRI Shapefile\" severity3_eqar_250avg_polygon fireclass"))
# system(paste0("gdal_polygonize.py ", infile, " ", outfile, " -b 1 -f 'ESRI Shapefile' severity3_eqar_250avg_polygons fire_class"))
# # gdal_polygonize.py /tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/fire/severity3_eqar_250avg.tif /tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/fire/severity3_eqar_250avg_polygon.shp -b 1 -f "ESRI Shapefile" severity3_eqar_250avg_polygon fireclass
# # 
# # system("gdal_polygonize.py /tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/fire/severity3_eqar_250avg.tif /tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/fire/severity3_eqar_250avg_polygon.shp -b 1 -f \"ESRI Shapefile\" severity3_eqar_250avg_polygon fireclass")
# OR...??
# system("source ~/environments/gsdms_env/bin/activate") ??
# OR...??
# source("/tempdata/workdir/nesp_bugs/scripts/gdal_polygonizeR.R")
# system.time(p <- gdal_polygonizeR(outfile))


# ## Preliminnary analysis area
# ## source: ...??
# paa <- file.path(bugs_data, "Preliminary_Analysis_Areas/prelim_analysis_areas_dissolve.shp")
# paa <- rgdal::readOGR(paa)
# 
# ## Reproject PAA to equal area (?)
# fire <- raster(file.path(output_dir, "severity3_eqar250.tif"))
# paa2 <- sp::spTransform(paa, CRSobj = crs(fire))
# 
# clearPlot()
# quickPlot::Plot(fire,
#                 title = "",
#                 axes = FALSE,
#                 legend = FALSE,
#                 col = "khaki",
#                 addTo = "fire",
#                 new = TRUE)
# quickPlot::Plot(paa2,
#                 cols = "tomato3",
#                 title = "",
#                 addTo = "fire")
