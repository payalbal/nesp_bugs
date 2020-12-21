## Fire overlap analysis: foreach{}
## Collaborator: Casey Visintin

## Notes
## Yet to run for Prelim analysis area (in/out; prop of habitat within) - to clip


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")
x <- c("data.table", "sp", "raster", "rgdal", 
       "gdalUtils", "rgeos", "doMC", "foreach")
lapply(x, require, character.only = TRUE)
rm(x)

## File paths and folders
bugs_data = "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_data, "outputs")
shapefile_dir = file.path(output_dir, "species_shapefiles")
if(!dir.exists(shapefile_dir)){
  dir.create(shapefile_dir)
}

# polygons_dir = file.path(output_dir,"polygons")
# IUCNshpfiles <- list.files(file.path(polygons_dir, "shapesIUCN"), 
#                            pattern = ".shp$", 
#                            full.names = TRUE, all.files = TRUE)


# ## Pre-processing fire severity raster - to run once only #####
# if(!dir.exists(file.path(bugs_data, "outputs", "fire"))){
#   dir.create(file.path(bugs_data, "outputs", "fire"))
# }
# 
# ## Reproject raster with GDAL system call
# infile <- file.path(bugs_data, "fire/AUS_GEEBAM_Fire_Severity_NIAFED20200224_QGIS/AUS_GEEBAM_Fire_Severity_QGIS_NIAFED20200224.tif")
# outfile <- gsub(".tif", "_reproj.tif", infile)
# system(paste0("gdalwarp -overwrite -ot Byte -te -2214250 -4876750 2187750 -1110750 -tr 250 250 ",
#               "-t_srs '+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' ", infile, " ", outfile))
# gdalUtils::gdalinfo(outfile)
# 
# # Reclassify values in raster
# infile <- outfile
# outfile <- file.path(output_dir, "fire", "severity3_eqar250.tif")
# system(paste0("gdal_calc.py -A ", infile,
#               " --calc='(A==1)*1 + ((A==2)+(A==3))*2 + ((A==4)+(A==5))*3' --NoDataValue=0",
#               " --outfile=", outfile))
# gdalUtils::gdalinfo(outfile)



## Fire overlap for species WITH EOO polygons  ####
## Load in fire severity raster (re-classed) and get unique classes
fire_severity <- raster(file.path(output_dir, "fire", "severity3_eqar250.tif"))
fire_vals <- fire_severity[]
fire_classes <- sort(unique(na.omit(fire_vals)))

## Load in species rds
species_maps <- readRDS(file.path(output_dir, "ala_EOO.rds"))
polygon_list <- names(species_maps)

# ## To use IUCNshpfiles from IUCNeval instead
# ## Need to rename the files to remove special characters
# ## e.g. for 'Ochlerotatus'_('Finlaya')_monocellatus_EOO_poly
# ## easiest to use rds
# polygon_list <- tools::file_path_sans_ext(basename(IUCNshpfiles))

## Run overlap analysis in parallel: doMC ####
registerDoMC(future::availableCores()-2)
system.time(areas <- foreach(polys = polygon_list, 
                             .combine = rbind, 
                             .errorhandling = "remove",
                             .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                               
                               ## write spatial data to disk if coming from RDS file
                               writeOGR(species_maps[[polys]], dsn = shapefile_dir, layer = polys, driver = "ESRI Shapefile", overwrite_layer = TRUE)
                               
                               ## reproject species boundaries to match fire severity raster
                               system(paste0("gdal_rasterize -at -burn 1 -ot Byte -tr .0025 .0025 -l ",
                                             polys, " ",
                                             file.path(shapefile_dir, polys), ".shp ",
                                             file.path(shapefile_dir, polys), ".tif"))
                               
                               system(paste0("gdalwarp -overwrite -ot Byte -te -2214250 -4876750 2187750 -1110750 -tr 250 250 -s_srs 'EPSG:4326' -t_srs '+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' ",
                                             file.path(shapefile_dir, polys), ".tif ",
                                             file.path(shapefile_dir, polys), "_p.tif"))
                               
                               ## Create table of areas within each fire class
                               species_map <- raster(paste0(file.path(shapefile_dir, polys), "_p.tif"))
                               
                               dt <- data.table("species_map" = species_map[],
                                                "fire_severity" = fire_vals)
                               
                               df <- data.frame(matrix(ncol = length(fire_classes) + 3))
                               df[ , 1] <- polys
                               df[ , -c(1, ncol(df)-1, ncol(df))] <- sapply(fire_classes, FUN = function(x) dt[species_map == 1 & fire_severity == x, length(fire_severity) * 250 * 250 / 1000000])
                               df[ , ncol(df)-1] <- rowSums(df[, -c(1,ncol(df)-1, ncol(df))])
                               df[ , ncol(df)] <- dt[species_map == 1, length(species_map)* 250 * 250 / 1000000]
                               colnames(df) <- c("Species", paste0("Fire_Class_", fire_classes), "Total_Overlap", "Species_Polygon")
                               
                               file.remove(file.path(shapefile_dir, dir(path = shapefile_dir)))
                               
                               df
                             })


## Save outputs
write.csv(areas, file = file.path(output_dir, "ala_EOO_fireoverlap1.csv"), row.names = FALSE)


## Errors - rerun ####
polygon_list %in% areas$Species...


## Save outputs
write.csv(areas, file = file.path(output_dir, "ala_EOO_fireoverlap1.csv"), row.names = FALSE)






#### Fire overlap for species WITHOUT EOO polygons  ####
## Check number of records for these species against masked data
spnames <- fread(file.path(output_dir, "ala_noEOOspecies.csv"))
nrow(spnames)
counts <- fread(file.path(output_dir, "ala_masked_datacounts.csv"))
nrow(counts)
counts <- counts[species %in% spnames$x]
nrow(counts)

## Check number of records for these species against IUCN.eval data
csvout <- fread(file.path(output_dir, "ala_polygons_areas.csv"))
dim(csvout[is.na(EOO)])
summary(csvout[is.na(EOO)]$Nbe_unique_occ.)

spmasked_dir <- file.path(output_dir, "ala_data" ,"spdata_masked")

maskedfiles <- length(list.files(spmasked_dir, pattern= "_masked.rds$", full.names = TRUE))
length(grep(spnames$x %in% maskedfiles))

## Save outputs
saveRDS(areas, file = file.path(output_dir, "ala_points_fireoverlap.rds"))
write.csv(areas, file = file.path(output_dir, "ala_points_fireoverlap.rds"), row.names = FALSE)






## EXTRAS ------------ ####

# # Download, extract, and remove zipfile
# zipdst <- file.path(bugs_data, "fire/AUS_GEEBAM_Fire_Severity_QGIS.zip")
# system(paste0("curl http://www.environment.gov.au/fed/catalog/search/resource/downloadData.page?uuid=%7B8CE7D6BE-4A82-40D7-80BC-647CB1FE5C08%7D -o ", zipdst))
# 
#     # system(paste0("curl --header 'Host: www.environment.gov.au' --user-agent 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:84.0) Gecko/20100101 Firefox/84.0' --header 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8' --header 'Accept-Language: en-US,en;q=0.5' --header 'Content-Type: application/x-www-form-urlencoded' --header 'Origin: http://www.environment.gov.au' --referer 'http://www.environment.gov.au/fed/catalog/search/resource/downloadData.page?uuid=%7B8CE7D6BE-4A82-40D7-80BC-647CB1FE5C08%7D' --cookie 'JSESSIONID=3B5D12DD42C3FE22CB7EB91764EF65D4; Drupal.session_cache.sid=R0Ib7fkVaAjS' --header 'Upgrade-Insecure-Requests: 1' --request POST --data-urlencode 'downloadFilesTable%3A0%3Aj_id_jsp_1490667900_4pc7=downloadFilesTable:0:j_id_jsp_1490667900_4pc7' --data-urlencode 'javax.faces.ViewState=-7640385847597866074:-7131431088440779044' --data-urlencode 'downloadFilesTable%3A0%3Aj_id_jsp_1490667900_4pc7%3Aj_id_jsp_1490667900_5pc7=downloadFilesTable:0:j_id_jsp_1490667900_4pc7:j_id_jsp_1490667900_5pc7' --data-urlencode 'filename=AUS_GEEBAM_Fire_Severity_QGIS.zip' --data-urlencode 'fileIdentifier=E4170D64-F926-46A9-9856-24FCA107CA1D' --data-urlencode 'uuid=8CE7D6BE-4A82-40D7-80BC-647CB1FE5C08' 'http://www.environment.gov.au/fed/catalog/search/resource/downloadData.page' --output ", zipdst))
# 
# unzip(zipfile = zipdst, files = "AUS_GEEBAM_Fire_Severity_QGIS_NIAFED20200224.tif", exdir = file.path(bugs_data, "fire"), junkpaths = TRUE)
# file.remove(zipdst)