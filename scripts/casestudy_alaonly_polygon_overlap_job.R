## Polygon overlap for case study using ALA data only for species


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "sp", "raster", 
       "rgdal", "rgeos", "gdalUtils",
       "doMC", "future", "future.apply", "parallel")
lapply(x, require, character.only = TRUE)
rm(x)

## Files and folder
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")
casestudy_dir = file.path(output_dir, "case_study", "datasources")

## >> Polygon overlap ####
shapefile_dir = file.path(casestudy_dir, "species_shapefiles")
overlap_dir = file.path(casestudy_dir, "polygon_overlap")
if(!dir.exists(shapefile_dir)){dir.create(shapefile_dir)}
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

source("~/gsdms_r_vol/tempdata/workdir/nesp_bugs/scripts/polygon_paa_overlap.R")


## Fire overlap for species WITH EOO polygons  ####
## >> Load in spdf data for species with EOO ####
species_maps <- readRDS(file.path(casestudy_dir, "species_ahullEOOspdf.rds"))
polygon_list <- names(species_maps)
message(cat("Number of species with polygons: "),
        length(polygon_list))

## >> Find missing species from outputs (if any) ####
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
csvnames <- tools::file_path_sans_ext(basename(csvfiles))
species_list <- polygon_list[!polygon_list %in% csvnames]

## >> Load in fire severity raster and get unique classes ####
fire_severity <- raster(file.path(output_dir, "fire", "severity5_eqar250_native_paa.tif"))
fire_vals <- fire_severity[]
fire_classes <- sort(unique(na.omit(fire_vals)))
fire_res <- res(fire_severity)
fire_crs <- as.character(crs(fire_severity))
fire_extent <- extent(fire_severity)

## >> Run overlap analysis in parallel: doMC ####
## 'log' only useful when running small number of species
registerDoMC(future::availableCores()-1)
system.time(foreach(polys = species_list,
                    .combine = rbind,
                    .errorhandling = "pass",
                    .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                      
                      polygon_paa_overlap(species_name = polys,
                                          species_poly = species_maps[[polys]],
                                          shapefile_dir = shapefile_dir,
                                          fire_res = fire_res,
                                          fire_crs = fire_crs,
                                          fire_extent = fire_extent,
                                          fire_vals = fire_vals,
                                          fire_classes = fire_classes,
                                          outdir = overlap_dir)
                    })
