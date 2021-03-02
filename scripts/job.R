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
overlap_dir = file.path(output_dir, "polygon_overlap")
source("~/gsdms_r_vol/tempdata/workdir/nesp_bugs/scripts/polygon_overlap.R")



## Fire overlap for species WITH EOO polygons  ####
## >> Load in spdf data for species with EOO ####
species_maps <- readRDS(file.path(output_dir, "species_ahullEOOspdf.rds"))
polygon_list <- names(species_maps)

## >> Load in fire severity raster (re-classed) and get unique classes ####
fire_severity <- raster(file.path(output_dir, "fire", "severity3_eqar250.tif"))
fire_vals <- fire_severity[]
fire_classes <- sort(unique(na.omit(fire_vals)))


## Error runs.... ####
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
registerDoMC(75)
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