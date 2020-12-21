## Fire overlap analysis: future.apply{}
## Collaborator: Casey Visintin

## Notes
## Yet to run for Prelim analysis area (in/out; prop of habitat within) - to clip


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")
x <- c("data.table", "sp", "raster", "rgdal", "gdalUtils", "rgeos", "foreach",
       "doParallel")
lapply(x, require, character.only = TRUE)
rm(x)

## File paths and folders
bugs_data = "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_data, "outputs")
shapefile_dir <- file.path(output_dir, "species_shapefiles")
if(!dir.exists(shapefile_dir)){dir.create(shapefile_dir)}
overlap_dir = file.path(output_dir, "overlaps")
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

source("/tempdata/workdir/nesp_bugs/")

## Load in fire severity raster (re-classed) and get unique classes
fire_file <- file.path(output_dir, "fire", "severity3_eqar250.tif")
fire_severity <- raster(fire_file)
fire_vals <- fire_severity[]
fire_classes <- sort(unique(na.omit(fire_vals)))

## Load species rds file
species_polys <- readRDS(file.path(output_dir, "ala_EOO.rds"))
species_names <- names(species_polys)



## Run overlap analysis ####
plan(multiprocess, workers = future::availableCores()-2)
options(future.globals.maxSize = +Inf)
errorlog <- paste0(output_dir, "/errorlog_fire_overlapR_", gsub("-", "", Sys.Date()), ".txt")
# if(file.exists(errorlog)){unlink(errorlog)}
writeLines(c(""), errorlog)

system.time(
  suppressWarnings(
    future.apply::future_lapply(
      species_names,
      function(x){
        tmp <- tryCatch(expr = overlap(species_name = x,
                                       rds_file = file.path(output_dir, "ala_EOO.rds"),
                                       shapefile_dir = shapefile_dir,
                                       fire_vals = fire_vals,
                                       fire_classes = fire_classes, 
                                       outdir = overlap_dir),
                        error = function(e) {
                          cat(
                            paste(as.character(x), "\n"),
                            file = errorlog,
                            append = TRUE)
                        }
        )
      }, future.seed = TRUE)))



## Check files ####
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of .csv output files created from IUCN.eval(): "),
        length(csvfiles))

## Combine outputs ####
out <- do.call("rbind", lapply(csvfiles , fread))
dim(out)
setorder(out, Species)
out <- as.data.table(out)
write.csv(out, file = file.path(output_dir, "ala_EOO_fireoverlaps.csv"), 
          row.names = FALSE)

message(cat("#species with EOOs: "),
        nrow(out[!is.na(EOO)]))
message(cat("#species without EOOs: "),
        nrow(out[is.na(EOO)]))
message(cat("max #records for species without EOOs: "),
        max(out[is.na(EOO)]$Nbe_unique_occ.))

