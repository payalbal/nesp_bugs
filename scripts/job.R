
## File paths
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs", "fire")

## Convert GEEBAM fire severity raster to shapefile
infile <- file.path(bugs_dir, "fire/AUS_GEEBAM_Fire_Severity_NIAFED20200224_QGIS/AUS_GEEBAM_Fire_Severity_QGIS_NIAFED20200224.tif")
outfile <- file.path(output_dir, gsub(".tif",".shp", basename(infile)))
system(paste0("gdal_polygonize.py ", infile, " ", outfile, " -f 'ESRI Shapefile'"))