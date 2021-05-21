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
bugs_dir = "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")
new_output_dir = file.path(bugs_dir, "outputs", "outputs_for_updated_species_only")
shapefile_dir = file.path(new_output_dir, "species_shapefiles")
overlap_dir = file.path(new_output_dir, "polygon_overlap")
if(!dir.exists(shapefile_dir)){dir.create(shapefile_dir)}
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}
source("~/gsdms_r_vol/tempdata/workdir/nesp_bugs/scripts/polygon_overlap.R")



## Fire overlap for species WITH EOO polygons  ####
## >> Load in spdf data for species with EOO ####
species_maps <- readRDS(file.path(new_output_dir, "species_ahullEOOspdf_updatedsp.rds"))
polygon_list <- names(species_maps)

## >> Find missing species from outputs (if any) ####
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
csvnames <- tools::file_path_sans_ext(basename(csvfiles))
species_list <- polygon_list[!polygon_list %in% csvnames]

## >> Load in fire severity raster (re-classed) and get unique classes ####
fire_severity <- raster(file.path(output_dir, "fire", "severity5_eqar250_native.tif"))
fire_vals <- fire_severity[]
fire_classes <- sort(unique(na.omit(fire_vals)))

## >> Run overlap analysis in parallel: doMC ####
## 'log' only useful when running small number of species
registerDoMC(76)
system.time(foreach(polys = species_list,
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

## Errors seem to be an artefact of the system rather than problem with data/code
