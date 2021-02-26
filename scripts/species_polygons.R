## EOO and AOO polygons for ALA + nonALA data

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
spdata_dir = file.path(output_dir, "ala_nonala_data" ,"spdata")

polygons_dir = file.path(output_dir,"polygons_ahull")
if (!dir.exists(polygons_dir)) {dir.create(polygons_dir)}

working_dir <- paste0("~/gsdms_r_vol", polygons_dir)
# working_dir <- "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/polygons/"


## Using ConR package - minimum convex polygon or alpha hulls
## https://cran.r-project.org/web/packages/ConR/index.html
## Notes: 
##  WGS84 required for data
##  data format: latitude, longitude (in decimal degrees), and taxon name
##  outputs for EOO and AOO same as from {red} when method.range = "convex.hull"
##  ** country_map + exclude.area can be used for cropping to prelimiunary analysis area
spfiles <- list.files(spdata_dir, pattern= ".rds$", full.names = TRUE)
message(cat("Total number of species in cleaned ALA data: "),
        length(spfiles))

## >> Run IUCN.eval in parallel ####
source("~/gsdms_r_vol/tempdata/workdir/nesp_bugs/scripts/conr_iucn_eval.R")
basemap_file <- file.path(output_dir, "masks", "auslands_1poly_wgs84.shp")
hull.method <- "alpha.hull" # "convex.hull"  

## Package: future - for catching errors
plan(multiprocess, workers = future::availableCores()-2)
options(future.globals.maxSize = +Inf) ## CAUTION: Set this to a value, e.g. availablecores-1?/RAM-10?
errorlog <- paste0(output_dir, "/errorlog_polygons_ahull_", gsub("-", "", Sys.Date()), ".txt")
# if(file.exists(errorlog)){unlink(errorlog)}
writeLines(c(""), errorlog)

...system.time(
  suppressWarnings(
    future.apply::future_lapply(
      spfiles,
      function(x){
        tmp <- tryCatch(expr = conr_iucn_eval(species_filename = x,
                                              hull.method = hull.method,
                                              exclude.by.map = TRUE,
                                              basemap_path = basemap_file,
                                              working_dir = working_dir,
                                              iucn_outpath = polygons_dir),
                        error = function(e) {
                          cat(
                            paste(as.character(x), "\n"),
                            file = errorlog,
                            append = TRUE)
                        }
        )
      }, future.seed = TRUE)))

## >> Check files ####
csvfiles <- list.files(polygons_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of .csv output files created from IUCN.eval(): "),
        length(csvfiles))
  ## Check when files were created
  unique(lubridate::date(file.info(csvfiles, extra_cols = TRUE)$mtime))

rdsfiles <- list.files(polygons_dir, pattern = ".rds$", 
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of .rds output files created from IUCN.eval(): "),
        length(rdsfiles))


## Resolve errors ####
polygons_error_dir <- "/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/polygons_errors"
dir.create(polygons_error_dir)
working_error_dir <- "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/polygons_errors/"
setwd(working_error_dir)

source("~/gsdms_r_vol/tempdata/workdir/nesp_bugs/scripts/conr_iucn_eval.R")
basemap_file <- file.path(output_dir, "masks", "auslands_1poly_wgs84.shp")

errorlog <- file.path(output_dir, "errorlog_polygons_ahull_20210225.txt")
errorfiles <- trimws(readLines(errorlog)[-1])
message(cat("Number of species showinng errors: "),
        length(errorfiles))
all(errorfiles %in% spfiles)

## List species not in output files
output_sp <- basename(tools::file_path_sans_ext(rdsfiles))
input_sp <- basename(tools::file_path_sans_ext(spfiles))
input_sp[!input_sp %in% output_sp]
message(cat("error species == species not found in rds output files: "),
        all(basename(tools::file_path_sans_ext(errorfiles)) 
            %in% input_sp[!input_sp %in% output_sp]))

## >> Batch rerun for species showing errors
mc.cores = future::availableCores()-2
set.seed(1, kind = "L'Ecuyer-CMRG")
system.time(invisible(mclapply(errorfiles,
                               conr_iucn_eval,
                               hull.method = hull.method,
                               basemap_path = basemap_file,
                               working_dir = working_error_dir,
                               iucn_outpath = polygons_error_dir,
                               mc.cores = mc.cores)))
  ## >> Warning message:
  ## In mclapply(errorfiles, conr_iucn_eval, hull.method = hull.method,  :
  ##              scheduled cores ... encountered errors 
  ##              in user code, all values of the jobs will be affected
  ## Rerun for species with errors..

## Check # files created in error directory
csvfiles2 <- list.files(polygons_error_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of .csv output files created from IUCN.eval(): "),
        length(csvfiles2))

rdsfiles2 <- list.files(polygons_error_dir, pattern = ".rds$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of .rds output files created from IUCN.eval(): "),
        length(rdsfiles2))

## List species not in output files
error_sp <- basename(tools::file_path_sans_ext(errorfiles))
sp1 <- basename(tools::file_path_sans_ext(rdsfiles2))
errorfiles <- errorfiles[!error_sp %in% sp1]
  ## rerun mclapply(errorfiles....)


## >> Individual reruns for species with unresolved errors ####
## NOTE: Rerun multiple times for a species
basemap <- readOGR(file.path(output_dir, "masks/auslands_wgs84.shp"))

## Read species data
length(errorfiles)
dat <- as.data.table(readRDS(errorfiles[6]))
dim(dat)
spname <- unique(dat$spfile)
message(cat("Processing species... ",
            spname))
dat <- dat[ , .(latitude, longitude, scientificName, family, year)]
names(dat) <- c("latitude", "longitude", "tax", "family", "coly")

## Run ConR function
out <- IUCN.eval(dat,
                 method.range = hull.method,
                 alpha = 2,
                 Cell_size_AOO = 2,
                 Cell_size_locations = 2,
                 country_map = basemap,
                 exclude.area = TRUE,
                 write_file_option = "csv",
                 file_name = spname,
                 export_shp = TRUE, ## to get SpatialPolygonsDataFrame in output
                 write_results = TRUE,
                 write_shp = FALSE, ## to write shapefile files to folder
                 SubPop = FALSE,
                 DrawMap = FALSE)

saveRDS(out, file = paste0(polygons_error_dir, "/", spname, ".rds"))
rm(dat, spname, out)

## Error in { : task 1 failed - "task 1 failed - "subscript out of bounds""
## Error in { : task 1 failed - "task 1 failed - "object 'case' not found""

## >> Copy files & clean up ####
csvfiles <- list.files(polygons_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
csvfiles2 <- list.files(polygons_error_dir, pattern = ".csv$",
                        full.names = TRUE, all.files = TRUE)
message(cat("#.csv output files form main run: "),
        length(csvfiles))
message(cat("#.csv output files from error run: "),
        length(csvfiles2))

file.copy(csvfiles2, polygons_dir,
          overwrite = FALSE, recursive = TRUE,
          copy.mode = TRUE, copy.date = TRUE)

csvfiles <- list.files(polygons_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("#.csv output files: "),
        length(csvfiles))
file.remove(csvfiles2)

rdsfiles <- list.files(polygons_dir, pattern = ".rds$", 
                       full.names = TRUE, all.files = TRUE)
rdsfiles2 <- list.files(polygons_error_dir, pattern = ".rds$",
                        full.names = TRUE, all.files = TRUE)
message(cat("#.rds output files from main run: "),
        length(rdsfiles))
message(cat("#.rds output files from error run: "),
        length(rdsfiles2))

file.copy(rdsfiles2, polygons_dir,
          overwrite = FALSE, recursive = TRUE,
          copy.mode = TRUE, copy.date = TRUE)
rdsfiles <- list.files(polygons_dir, pattern = ".rds$", 
                       full.names = TRUE, all.files = TRUE)
message(cat("#.rds output files: "),
        length(rdsfiles))
file.remove(rdsfiles2)

pngdirs <- list.dirs(polygons_error_dir)
unlink(polygons_error_dir, recursive = TRUE)
rm(csvfiles2, rdsfiles2, errorfiles)



## Save IUCN.eval() outputs ####
## >> List species not in output files ####
rdsfiles <- list.files(polygons_dir, pattern = ".rds$", 
                       full.names = TRUE, all.files = TRUE)
output_sp <- basename(tools::file_path_sans_ext(rdsfiles))
input_sp <- basename(tools::file_path_sans_ext(spfiles))
input_sp[!input_sp %in% output_sp]
errorfiles <- spfiles[!input_sp %in% output_sp]

## >> AOO & EOO areas ####
## Create data table from .csv output files
csvfiles <- list.files(polygons_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
out <- do.call("rbind", lapply(csvfiles , read.csv))
dim(out)

out$spfile <- basename(tools::file_path_sans_ext(csvfiles))
names(out)[1] <- "scientificName"

setDT(out, key = "spfile")
out <- out[,Nbe_subPop := NULL]

## Add rows for species with errors
dat <- do.call("rbind", lapply(errorfiles, readRDS))

temp <- data.frame(matrix(NA, length(errorfiles), dim(out)[2]))
names(temp) <- names(out)
temp$scientificName <- dat[, .N, scientificName]$scientificName
temp$Nbe_unique_occ. <- dat[, .N, scientificName]$N
temp$spfile <- basename(tools::file_path_sans_ext(errorfiles))
setDT(temp)

out <- rbind(out, temp)

## Add class/family information to output table
tax <- fread(file = file.path(output_dir, "data_ALAnonALA.csv"))
tax <- setDT(tax, key = "spfile")[, .SD[1L] ,.(scientificName, class, family, spfile)]
tax <- tax[,.(scientificName, class, family, spfile)]

dim(out); length(unique(out$spfile)); length(unique(out$scientificName))
dim(tax); length(unique(tax$spfile)); length(unique(tax$scientificName))

out <- merge(out, tax, by = "spfile")

## Clean table
names(out)
all(out$scientificName.x == out$scientificName.y) 
length(which(out$scientificName.x != out$scientificName.y))
id <- which(out$scientificName.x != out$scientificName.y)

  ## Check the two scientificName columns a few times
  ## It appears that IUCN.eval() outputs remove ? from species name perhaps?
out[sample(id, 5), .(scientificName.x, scientificName.y)]
  ## Keep original names with ?, i.e. scientificName.y
out <- out[, c(11: 13, 3:10, 1)]
names(out)[1] <- "scientificName"
head(out)

## Save outputs
setorder(out, EOO, AOO)
write.csv(out, file = file.path(output_dir, "species_ahullpolys_areas.csv"), 
          row.names = FALSE)
# file.remove(csvfiles)

message(cat("#species with EOOs: "),
        nrow(out[!is.na(EOO)]))
message(cat("#species without EOOs: "),
        nrow(out[is.na(EOO)]))
message(cat("max #records for species without EOOs (excluding species with errors): "),
        max(out[!(scientificName %in% temp$scientificName) & is.na(EOO)]$Nbe_unique_occ.))


## >> EOO polygons ####
## Create list of .rds output files
rdsfiles <- list.files(polygons_dir, pattern = ".rds$", 
                       full.names = TRUE, all.files = TRUE)
polynames <- basename(tools::file_path_sans_ext(rdsfiles))
temp <- lapply(rdsfiles, readRDS)
names(temp) <- polynames
length(temp)
saveRDS(temp, file = file.path(output_dir, "species_ahullpolys_rdsfiles.rds"))

## Create list of SPDF from .rds output files
temp2 <- lapply(temp, "[[", 1)
temp2 <- lapply(temp2, "[[", 2)
length(temp2)
saveRDS(temp2, file = file.path(output_dir, "ala_polygons_convhull.rds"))

## Create list of non-NULL SPDF for species with EOOs
na.eooIDX <- sapply(temp2, length)
na.eooIDX <- which(na.eooIDX == 0)
message(cat("# Number of species without EOOS from IUCN.eval(): "),
        length(na.eooIDX))
message(cat("# Proportion of species without EOOS from IUCN.eval(): "),
        length(na.eooIDX)/length(temp2))

message(cat("# Number of species with EOOS from IUCN.eval(): "),
        length(temp2) - length(na.eooIDX))
message(cat("# Proportion of species with EOOS from IUCN.eval(): "),
        (length(temp2) - length(na.eooIDX))/length(temp2))

temp3 <- temp2[-na.eooIDX]
length(temp3)
saveRDS(temp3, file = file.path(output_dir, "ala_EOO_convhull.rds"))
# temp <- do.call(cbind, temp3)
# names(temp) <- names(temp3)

## Create list of species names without EOOs
# eidx <- c(4, 15, 21, 1, 86, 115, 99, 63, 30, 58, 52, 49, 80, 55)
# errorsp <- gsub("_masked.rds$", "", basename(errorfiles[eidx[c(2, 5, 10)]]))
errorsp <- gsub("_masked.rds$", "", basename(errorfiles))
length(sort(c(names(na.eooIDX), errorsp)))
write.csv(sort(c(names(na.eooIDX), errorsp)), 
          file = file.path(output_dir, "ala_noEOOspecies_convhull.csv"), 
          row.names = FALSE)
message(cat("Total number of species: "),
        length(c(names(na.eooIDX), errorsp)) + length(temp3))





# ## DEBUGGING <start> ------------------------------------------------ ####
# polygons_dir <- "/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/polygons2"
# dir.create(polygons_dir)
# working_dir <- "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/polygons2/"
# setwd(working_dir)
# dat <- as.data.table(readRDS(spfiles[4]))
# dim(dat)
# spname <- unique(dat$spfile)
# message(cat("Processing species... ",
#             spname))
# dat <- dat[ , .(latitude, longitude, scientificName, family, year)]
# names(dat) <- c("latitude", "longitude", "tax", "family", "coly")
# 
# ## Run ConR function
# out <- IUCN.eval(dat,
#                  method.range = "alpha.hull",
#                  alpha = 2,
#                  Cell_size_AOO = 2,
#                  country_map = basemap,
#                  exclude.area = TRUE,
#                  SubPop = FALSE,
#                  DrawMap = TRUE,
#                  write_file_option = "csv",
#                  file_name = spname,
#                  export_shp = TRUE,
#                  write_shp = TRUE,
#                  write_results = TRUE)
# 
# browseURL(pngfiles[grep(names(ala_polys)[4], pngfiles)])
# 
# ala_polys <- readRDS(file.path(output_dir, "ala_polygons.rds"))
# mask_file <- file.path(output_dir, "masks", "ausmask_noaa_1kmWGS_NA.tif")
# ausmask <- raster(mask_file)
# ala_polys[4]
# 
# clearPlot()
# quickPlot::Plot(ausmask,
#                 title = "",
#                 axes = FALSE,
#                 legend = FALSE,
#                 col = "khaki",
#                 addTo = "ausmask",
#                 new = TRUE)
# quickPlot::Plot(sp1,
#                 cols = "tomato3",
#                 title = "",
#                 addTo = "ausmask")
# 
# equalarea_proj <- CRS("+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# sp1_ee <- spTransform(sp1, CRSobj = equalarea_proj)
# 
# quickPlot::Plot(sp1_ee,
#                 cols = "tomato3",
#                 title = "",
#                 addTo = "ausmask")
# sp1_ee <- readShapePoly("sp1", verbose=TRUE, proj4string=equalarea_proj)
# 
# plot(raster(mask_file), col = "khaki", axes = FALSE, box = FALSE, legend = FALSE)
# plot(sp1, add = TRUE, pch = 17, col = "navy", cex = 0.5)

# ## DEBUGGING <end> -------------------------------------------------- ####

