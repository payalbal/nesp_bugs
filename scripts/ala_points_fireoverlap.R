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
spmasked_dir <- file.path(output_dir, "ala_data" ,"spdata_masked")

overlap_dir = file.path(output_dir, "points_overlap")
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

source("/tempdata/workdir/nesp_bugs/scripts/points_overlap.R")


## Fire overlap for species WITHOUT EOO polygons  ####

## >> Load species data ####
## Species names without EOOs + 3 error species from ala_polygons.R
points_list <- fread(file.path(output_dir, "ala_noEOOspecies.csv"))
points_list <- points_list$x
length(points_list)

## Check number of records for these species inn UCN.eval outputs
temp <- fread(file.path(output_dir, "ala_polygons_areas.csv"))
dim(temp[is.na(EOO)])
summary(temp[is.na(EOO)]$Nbe_unique_occ.)

## Find cleand/masked species data files
datfiles <- list.files(spmasked_dir, pattern= "_masked.rds$", full.names = TRUE)
x <- basename(tools::file_path_sans_ext(datfiles))
x <- gsub("_masked", "", x)
datfiles <- datfiles[x %in% points_list] ## subset datfiles for all IUCN species

## >> Load fire severity raster (re-classed) and get unique classes ####
fire_severity <- raster(file.path(output_dir, "fire", "severity3_eqar250.tif"))
fire_classes <- sort(unique(na.omit(fire_severity[])))

## Function parameters
wgs_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
eqarea_crs <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

## >> Run overlap analysis in parallel: doMC ####
registerDoMC(future::availableCores()-2)
system.time(log <- foreach(species_dat = datfiles, 
                           .combine = rbind,
                           .errorhandling = "pass",
                           .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                             
                             points_overlap(data_rds = species_dat, 
                                     crs_org = wgs_crs, 
                                     crs_new = eqarea_crs, 
                                     fire_classes = fire_classes,
                                     outdir = overlap_dir)
                           })
                             



## Error checking ####
## >> Display results summary ####
log
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of input species: "),
        length(datfiles))
message(cat("Number of output files: "),
        length(csvfiles))

## >> Find missing species from outputs ####
csvnames <- tools::file_path_sans_ext(basename(csvfiles))
error1_list <- error_list <- polygon_list[!polygon_list %in% csvnames]


## Reruns ####
...get code from ala_EOO_foreobverlap.R




## >> Error checking ####
all(df$Total_Overlap <= df$Occurrence_Points)
