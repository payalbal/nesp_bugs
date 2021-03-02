## Fire overlap analysis: foreach{}
## Collaborator: Casey Visintin


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
species_maps <- readRDS(file.path(output_dir, "species_ahullEOOspdf.rds"))
polygon_list <- names(species_maps)

## >> Load in fire severity raster (re-classed) and get unique classes ####
fire_severity <- raster(file.path(output_dir, "fire", "severity3_eqar250.tif"))
fire_vals <- fire_severity[]
fire_classes <- sort(unique(na.omit(fire_vals)))

## >> Run overlap analysis in parallel: doMC ####
## 'log' only useful when running small number of species
registerDoMC(future::availableCores()-2)
system.time(foreach(polys = polygon_list,
                      .combine = rbind,
                      .errorhandling = "pass",
                      .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{

                        polygon_overlap(species_name = polys,
                                        species_poly = species_maps[[polys]],
                                        shapefile_dir = shapefile_dir,
                                        fire_vals = fire_vals,
                                        fire_classes = fire_classes,
                                        outdir = overlap_dir)
                      })


## Error checking ####
## >> Display results summary ####
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of input species: "),
        length(polygon_list))
message(cat("Number of output files: "),
        length(csvfiles))

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
system.time(foreach(polys = error_list,
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
out <- do.call("rbind", lapply(csvfiles, fread))
message(cat("Check for NAs: "),
        sum(is.na(out)))
names(out)[1] <- "spfile"
setDT(out, key = "spfile")

message(cat("Check if area overlapping with fire is always <= Total polygonn area for species: "),
        all(out$Total_Overlap <= out$Species_Polygon))

## Add class/family information to output table
tax <- fread(file = file.path(output_dir, "data_ALAnonALA.csv"))
tax <- setDT(tax, key = "spfile")[, .SD[1L] ,.(scientificName, class, family, spfile)]
tax <- tax[,.(scientificName, class, family, spfile)]
tax <- tax[spfile %in% out$spfile]

dim(out); length(unique(out$spfile)); length(unique(out$scientificName))
dim(tax); length(unique(tax$spfile)); length(unique(tax$scientificName))

out <- merge(out, tax, by = "spfile")
out <- out[,c(10:8, 2:7, 1)]

## Save output table
setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, "species_polygon_fireoverlap.csv"), row.names = FALSE)


# ## Remove files ####
# file.remove(file.path(overlap_dir, dir(path = overlap_dir)))
# unlink(shapefile_dir, recursive = TRUE)
# unlink(overlap_dir, recursive = TRUE)


## Summarize outputs ####
message(cat("NA in scientificName: "),
        length(which(is.na(out$scientificName))))

message(cat("Total number of species: "),
        nrow(out))

message(cat("Number of species showing overlap: "))
out[, .N, by = Total_Overlap]

message(cat("# Species showing 100% fire overlap: "),
        nrow(out[Percent_Overlap == 100]));
round(nrow(out[Percent_Overlap == 100])/nrow(out), 2)
message(cat("# Species showing 100% fire overlap: "))
print(setorder(out[Percent_Overlap == 100][, .(class,family, scientificName)], class, family, scientificName))

message(cat("# Species showing >= 90% fire overlap: "),
        nrow(out[Percent_Overlap >= 90]));
nrow(out[Percent_Overlap >= 90])/nrow(out)

message(cat("# Species showing >= 50% fire overlap: "),
        nrow(out[Percent_Overlap >= 50]));
round(nrow(out[Percent_Overlap >= 50])/nrow(out), 2)

message(cat("# Species showing < 50% and > 30% fire overlap: "),
        nrow(out[Percent_Overlap < 50 & Percent_Overlap > 30]));
round(nrow(out[Percent_Overlap < 50 & Percent_Overlap > 30])/nrow(out), 2)      

message(cat("# Species showing <= 30% fire overlap: "),
        nrow(out[Percent_Overlap <= 30]));
round(nrow(out[Percent_Overlap <= 30])/nrow(out), 2)

message(cat("# Species showing no fire overlap: "),
        nrow(out[Percent_Overlap == 0]))
round(nrow(out[Percent_Overlap == 0])/nrow(out), 2)



