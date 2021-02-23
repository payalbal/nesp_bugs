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
if(!dir.exists(shapefile_dir)){dir.create(shapefile_dir)}

overlap_dir = file.path(output_dir, "polygon_overlap")
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

source("/tempdata/workdir/nesp_bugs/scripts/polygon_overlap.R")

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
## >> Load in spdf data for species with EOO ####
species_maps <- readRDS(file.path(output_dir, "ala_EOO.rds"))
polygon_list <- names(species_maps)

# ## To use IUCNshpfiles from IUCNeval instead
# ## Need to rename the files to remove special characters
# ## e.g. for 'Ochlerotatus'_('Finlaya')_monocellatus_EOO_poly
# ## easiest to use rds
# polygon_list <- tools::file_path_sans_ext(basename(IUCNshpfiles))

## >> Load in fire severity raster (re-classed) and get unique classes ####
fire_severity <- raster(file.path(output_dir, "fire", "severity3_eqar250.tif"))
fire_vals <- fire_severity[]
fire_classes <- sort(unique(na.omit(fire_vals)))

## Batch specification
# batches <- seq(0, length(polygon_list), 1000)
# polygon_list <- polygon_list[15001:20000]


  # ## >> Run overlap analysis in parallel: doMC ####
  # ## 'log' only useful when running small number of species
  # registerDoMC(future::availableCores()-2)
  # system.time(#log <- 
  #               foreach(polys = polygon_list, 
  #                       .combine = rbind,
  #                       .errorhandling = "pass",
  #                       .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
  #                         
  #                         polygon_overlap(species_name = polys,
  #                                         species_poly = species_maps[[polys]],
  #                                         shapefile_dir = shapefile_dir,
  #                                         fire_vals = fire_vals,
  #                                         fire_classes = fire_classes, 
  #                                         outdir = overlap_dir)
  #                       })
  # 

## Error checking ####
## >> Display results summary ####
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
  # message(cat("Number of input species: "),
  #         length(polygon_list))
  # message(cat("Number of output files: "),
  #         length(csvfiles))

## >> Find missing species from outputs ####
csvnames <- tools::file_path_sans_ext(basename(csvfiles))
error_list <- polygon_list[!polygon_list %in% csvnames]
message(cat("Number of species in error list: "),
        length(error_list))

## Reruns ####
## Repeat this till most of the errors are fixed
## Errors seem to be an artefact of the system rather than problem with data/code
if(length(error_list) <= future::availableCores()-2) {
  registerDoMC(length(error_list))
} else{
  registerDoMC(future::availableCores()-2)
}
system.time(log <-
              foreach(polys = error_list,
                      .combine = rbind,
                      .errorhandling = "pass",
                      .packages = c('sp', 'raster',
                                    'rgdal', 'data.table')) %dopar%{

                                      polygon_overlap(species_name = polys,
                                                      species_poly = species_maps[[polys]],
                                                      shapefile_dir = shapefile_dir,
                                                      fire_vals = fire_vals,
                                                      fire_classes = fire_classes,
                                                      outdir = overlap_dir)
})

csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of input species: "),
        length(polygon_list))
message(cat("Number of output files: "),
        length(csvfiles))
csvnames <- basename(tools::file_path_sans_ext(csvfiles))
error_list[!error_list %in% csvnames]

## Output table ####
## Merge csv files
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
out <- do.call("rbind", lapply(csvfiles , fread))
message(cat("Check for NAs: "),
        sum(is.na(out)))
names(out)[1] <- "SpeciesName"
out <- as.data.table(out)
setorder(out, SpeciesName)
setkey(out, "SpeciesName")

## Extract taxonomic information for species
ala <- readRDS(file.path(output_dir, "clean2_ala_2020-10-28.rds"))
setkeyv(ala, "spfile")

message(cat("Are all species in output table found in cleaned ALA data? "),
        length(out$SpeciesName) == length(which(out$SpeciesName %in% ala$spfile)))

taxinfo <- c("phylum", "class", "order", "family", 
             "genus", "species", "subspecies", "id")
temp <- data.table()
for (sp in out$SpeciesName){
  if (length(unique(ala[which(ala$spfile %in% sp)]$spfile)) != 1){
    
    warning(paste0("More than 1 unique taxon info found for ", sp))
    
  } else {
    x <- unique((ala[.(sp), ..taxinfo]))
    temp <- rbind(temp, cbind(SpeciesName = sp, x))
  }
}

## Check and remove duplicates from extracted taxonomic information
sum(duplicated(temp$SpeciesName))
message(cat("# rows in extracted info - # duplicates == # rows in output table: "),
        nrow(temp) - sum(duplicated(temp$SpeciesName)) == nrow(out))

temp <- temp[!which(duplicated(temp$SpeciesName))]
message(cat("Are all species in output table found in extracted taxon info: "),
        sum(out$SpeciesName %in% temp$SpeciesName) == nrow(out))
names(temp)
setkey(temp, "SpeciesName")

out <- merge(out, temp, by = "SpeciesName")

## Save output table
write.csv(out, file = file.path(output_dir, "EOO_fireoverlap.csv"), row.names = FALSE)


## Remove files ####
file.remove(file.path(overlap_dir, dir(path = overlap_dir)))
unlink(shapefile_dir, recursive = TRUE)
unlink(overlap_dir, recursive = TRUE)







## EXTRAS ------------ ####

# # Download, extract, and remove zipfile
# zipdst <- file.path(bugs_data, "fire/AUS_GEEBAM_Fire_Severity_QGIS.zip")
# system(paste0("curl http://www.environment.gov.au/fed/catalog/search/resource/downloadData.page?uuid=%7B8CE7D6BE-4A82-40D7-80BC-647CB1FE5C08%7D -o ", zipdst))
# 
#     # system(paste0("curl --header 'Host: www.environment.gov.au' --user-agent 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:84.0) Gecko/20100101 Firefox/84.0' --header 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8' --header 'Accept-Language: en-US,en;q=0.5' --header 'Content-Type: application/x-www-form-urlencoded' --header 'Origin: http://www.environment.gov.au' --referer 'http://www.environment.gov.au/fed/catalog/search/resource/downloadData.page?uuid=%7B8CE7D6BE-4A82-40D7-80BC-647CB1FE5C08%7D' --cookie 'JSESSIONID=3B5D12DD42C3FE22CB7EB91764EF65D4; Drupal.session_cache.sid=R0Ib7fkVaAjS' --header 'Upgrade-Insecure-Requests: 1' --request POST --data-urlencode 'downloadFilesTable%3A0%3Aj_id_jsp_1490667900_4pc7=downloadFilesTable:0:j_id_jsp_1490667900_4pc7' --data-urlencode 'javax.faces.ViewState=-7640385847597866074:-7131431088440779044' --data-urlencode 'downloadFilesTable%3A0%3Aj_id_jsp_1490667900_4pc7%3Aj_id_jsp_1490667900_5pc7=downloadFilesTable:0:j_id_jsp_1490667900_4pc7:j_id_jsp_1490667900_5pc7' --data-urlencode 'filename=AUS_GEEBAM_Fire_Severity_QGIS.zip' --data-urlencode 'fileIdentifier=E4170D64-F926-46A9-9856-24FCA107CA1D' --data-urlencode 'uuid=8CE7D6BE-4A82-40D7-80BC-647CB1FE5C08' 'http://www.environment.gov.au/fed/catalog/search/resource/downloadData.page' --output ", zipdst))
# 
# unzip(zipfile = zipdst, files = "AUS_GEEBAM_Fire_Severity_QGIS_NIAFED20200224.tif", exdir = file.path(bugs_data, "fire"), junkpaths = TRUE)
# file.remove(zipdst)