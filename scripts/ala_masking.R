## Mask ALA data ####

## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")
x <- c("data.table", "sp", "raster", "rgdal", "rgeos",
       "quickPlot", "fastshp",
       "alphahull", "ConR", "rnaturalearthdata", 
       "future", "future.apply", "parallel")
lapply(x, require, character.only = TRUE)
# options(rgl.useNULL=TRUE) ## to suppress warnings when using library(red)
# x <- c("red", "rCAT")
# lapply(x, require, character.only = TRUE)
rm(x)

## File paths and folders
bugs_data = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_data, "outputs")
spdata_dir = file.path(output_dir, "ala_data" ,"spdata")

spmasked_dir <- file.path(output_dir, "ala_data" ,"spdata_masked")
if (!dir.exists(spmasked_dir)) {dir.create(spmasked_dir)}

spfiles <- list.files(spdata_dir, pattern = ".rds$", full.names = TRUE)
message(cat("Total number of species in cleaned ALA data: "),
        length(spfiles))

mask_file <- file.path(output_dir, "masks", "ausmask_noaa_1kmWGS_NA.tif")
source("/tempdata/workdir/nesp_bugs/scripts/mask_spdat.R")

mc.cores = future::availableCores()-2
set.seed(1, kind = "L'Ecuyer-CMRG" )

system.time(mclapply(spfiles,
                     mask_spdat,
                     mask_file = mask_file,
                     data_dir = spmasked_dir,
                     mc.cores = mc.cores))
length(list.files(spmasked_dir, pattern= "_masked.rds$", full.names = TRUE))
length(list.files(spmasked_dir, pattern= ".csv$", full.names = TRUE))

## Tabulate records lost in masking from species csv files
out <- list.files(spmasked_dir, pattern = ".csv$", full.names = TRUE)
out <- do.call("rbind", lapply(out, fread))
setorder(out, n_masked, n_clean2)
message(cat("Check if # species with >0 records == # rds files saved from mask_spdat:"),
        dim(out[n_masked > 0]) ==
          length(list.files(spmasked_dir, pattern= "_masked.rds$", full.names = TRUE)))
write.csv(out, file = file.path(output_dir, "ala_masked_datacounts.csv"), row.names = FALSE)

## Note: mclapply() is much faster than future_lapply()
## Cannot use tryCatch with mclappply??

# ## Package: future - for catching errors
# plan(multiprocess, workers = future::availableCores()-2)
# options(future.globals.maxSize = +Inf) ## CAUTION: Set this to a finite value
# errorlog <- paste0(output_dir, "/errorlog_ala_polygonsR_", gsub("-", "", Sys.Date()), ".txt")
# writeLines(c(""), errorlog)
#
# system.time(
#   suppressWarnings(
#     future.apply::future_lapply(
#       spfiles,
#       function(x){
#         tmp <- tryCatch(expr = mask_spdat(species_filename = x,
#                                           mask_file = mask_file,
#                                           data_dir = spmasked_dir),
#                         error = function(e) {
#                           cat(
#                             paste(as.character(x), "\n"),
#                             file = errorlog,
#                             append = TRUE)
#                         })
#       }, future.seed = TRUE)))
# length(list.files(spmasked_dir, pattern= "_masked.rds$", full.names = TRUE))
# length(list.files(spmasked_dir, pattern= ".csv$", full.names = TRUE))

## >> Display #records for species before/after masking ####
counts <- fread(file.path(output_dir, "ala_masked_datacounts.csv"))
message(cat("Number of species with more than 0 records: "),
        nrow(counts[n_masked > 0]))
message(cat("Number of species with 0 records: "),
        nrow(counts[n_masked == 0]))
message(cat("Number of species with at least 3 records: "),
        nrow(counts[n_masked >= 3]))
## Becasue IUCN.eval() removes duplicates by name-lat-long