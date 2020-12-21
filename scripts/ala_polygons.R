## EOO and AOO polygons for ALA data
## Notes...
## Can estimate species distribution (wide/restricted)


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
map_dir = file.path(output_dir, "spmaps_unmasked")
spmasked_dir <- file.path(output_dir, "ala_data" ,"spdata_masked")
if (!dir.exists(spmasked_dir)) {dir.create(spmasked_dir)}
polygons_dir = file.path(output_dir,"polygons")
# unlink(polygons_dir, recursive = TRUE, force = TRUE)
if (!dir.exists(polygons_dir)) {dir.create(polygons_dir)}
working_dir <- paste0("~/gsdms_r_vol", polygons_dir)
# working_dir <- "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/polygons/"



# ## I. Mask ALA data ####
# spfiles <- list.files(spdata_dir, pattern = ".rds$", full.names = TRUE)
# message(cat("Total number of species in cleaned ALA data: "),
#         length(spfiles))
# 
# mask_file <- file.path(output_dir, "masks", "ausmask_noaa_1kmWGS_NA.tif")
# source("/tempdata/workdir/nesp_bugs/scripts/mask_spdat.R")
# 
# mc.cores = future::availableCores()-2
# set.seed(1, kind = "L'Ecuyer-CMRG" )
# 
# system.time(mclapply(spfiles,
#                      mask_spdat,
#                      mask_file = mask_file, 
#                      data_dir = spmasked_dir,
#                      mc.cores = mc.cores))
# length(list.files(spmasked_dir, pattern= "_masked.rds$", full.names = TRUE))
# length(list.files(spmasked_dir, pattern= ".csv$", full.names = TRUE))
# 
# ## Tabulate records lost in masking from species csv files
# out <- list.files(spmasked_dir, pattern = ".csv$", full.names = TRUE)
# out <- do.call("rbind", lapply(out, fread))
# setorder(out, n_masked, n_clean2)
# message(cat("Check if # species with >0 records == # rds files saved from mask_spdat:"),
#         dim(out[n_masked > 0]) == 
#           length(list.files(spmasked_dir, pattern= "_masked.rds$", full.names = TRUE)))
# write.csv(out, file = file.path(output_dir, "ala_masked_datacounts.csv"), row.names = FALSE)
# 
# ## Note: mclapply() is much faster than future_lapply()
# ## Cannot use tryCatch with mclappply??
# 
# # ## Package: future - for catching errors
# # plan(multiprocess, workers = future::availableCores()-2)
# # options(future.globals.maxSize = +Inf) ## CAUTION: Set this to a finite value
# # errorlog <- paste0(output_dir, "/errorlog_ala_polygonsR_", gsub("-", "", Sys.Date()), ".txt")
# # writeLines(c(""), errorlog)
# # 
# # system.time(
# #   suppressWarnings(
# #     future.apply::future_lapply(
# #       spfiles,
# #       function(x){
# #         tmp <- tryCatch(expr = mask_spdat(species_filename = x,
# #                                           mask_file = mask_file,
# #                                           data_dir = spmasked_dir),
# #                         error = function(e) {
# #                           cat(
# #                             paste(as.character(x), "\n"),
# #                             file = errorlog,
# #                             append = TRUE)
# #                         })
# #       }, future.seed = TRUE)))
# # length(list.files(spmasked_dir, pattern= "_masked.rds$", full.names = TRUE))
# # length(list.files(spmasked_dir, pattern= ".csv$", full.names = TRUE))



## II. Species polygons ####
## Using ConR package - minimum convex polygon or alpha hulls
## https://cran.r-project.org/web/packages/ConR/index.html
## Notes: 
##  WGS84 required for data
##  data format: latitude, longitude (in decimal degrees), and taxon name
##  outputs for EOO and AOO same as from {red} when method.range = "convex.hull"
##  ** country_map + exclude.area can be used for cropping to prelimiunary analysis area
spfiles <- list.files(spmasked_dir, pattern= "_masked.rds$", full.names = TRUE)
message(cat("Total number of species in cleaned ALA data: "),
        length(spfiles))

source("~/gsdms_r_vol/tempdata/workdir/nesp_bugs/scripts/conr_iucn_eval.R")
basemap_file <- file.path(output_dir, "masks", "auslands_1poly_wgs84.shp")

# ## >> Run IUCN.eval in sequence ####
# basemap <- readOGR(basemap_file)
# setwd(working_dir) ## must do for IUCN.eval()
#
# for (spfile in spfiles){
#
#   ## Read species data
#   dat <- as.data.table(readRDS(spfile))
#   spname <- unique(dat$spfile)
#   message(cat("Processing species... ",
#               spname))
#   dat <- dat[ , .(latitude, longitude, scientificName, family, year)]
#   names(dat) <- c("latitude", "longitude", "tax", "family", "coly")
#
#   ## Run ConR function
#   out <- IUCN.eval(dat,
#                    method.range = "alpha.hull",
#                    alpha = 2,
#                    Cell_size_AOO = 2,
#                    country_map = basemap,
#                    exclude.area = TRUE,
#                    SubPop = FALSE,
#                    DrawMap = TRUE,
#                    write_file_option = "csv",
#                    file_name = spname,
#                    export_shp = TRUE,
#                    write_shp = TRUE,
#                    write_results = TRUE)
#
#   saveRDS(out, file = paste0(polygons_dir, "/", spname, ".rds"))
#
#   ## Clear objects
#   rm(dat, spname, out)
# }

## >> Run IUCN.eval in parallel ####

# ## Package: mclappy - does not show/catch errors
# mc.cores = future::availableCores()-2
# set.seed(1, kind = "L'Ecuyer-CMRG" )
# system.time(invisible(mclapply(spfiles,
#                                conr_iucn_eval,
#                                basemap_path = basemap_file,
#                                working_dir = working_dir,
#                                iucn_outpath = polygons_dir,
#                                mc.cores = mc.cores)))
# length(list.files(polygons_dir, pattern = ".rds$", full.names = TRUE))
# length(list.files(polygons_dir, pattern = ".csv$", full.names = TRUE))
#
#   # ## Checks
#   # rds <- list.files(polygons_dir, pattern = ".rds$", full.names = TRUE)
#   # rds <- gsub(".rds$", "", rds)
#   # csv <- list.files(polygons_dir, pattern = ".csv$", full.names = TRUE)
#   # csv <- gsub(".csv$", "", csv)
#   # length(rds) == length(csv)
#   # which(!(rds %in% csv))
#   # which(!(csv %in% rds))
#
# ## Marginally faster than future_lapply

## Package: future - for catching errors
plan(multiprocess, workers = future::availableCores()-2)
options(future.globals.maxSize = +Inf) ## CAUTION: Set this to a value, e.g. availablecores-1?/RAM-10?
errorlog <- paste0(output_dir, "/errorlog_ala_polygonsR_", gsub("-", "", Sys.Date()), ".txt")
# if(file.exists(errorlog)){unlink(errorlog)}
writeLines(c(""), errorlog)

system.time(
  suppressWarnings(
    future.apply::future_lapply(
      spfiles,
      function(x){
        tmp <- tryCatch(expr = conr_iucn_eval(species_filename = x,
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

rdsfiles <- list.files(polygons_dir, pattern = ".rds$", 
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of .rds output files created from IUCN.eval(): "),
        length(rdsfiles))

IUCNshpfiles <- list.files(file.path(polygons_dir, "shapesIUCN"), 
                           pattern = ".shp$", 
                           full.names = TRUE, all.files = TRUE)
message(cat("Number of .shp output files created from IUCN.eval(): "),
        length(IUCNshpfiles))

pngfiles <- list.files(polygons_dir, pattern = "png$", recursive = TRUE, 
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of .png map files created from IUCN.eval(): "),
        length(pngfiles))


## III. Resolve errors ####
errorlog <- file.path(output_dir, "errorlog_ala_polygonsR_20201215.txt")
errorfiles <- trimws(readLines(errorlog)[-1])
message(cat("NUmber of species showinng errors: "),
        length(errorfiles))
errorfiles %in% spfiles

polyfiles_sp <- basename(tools::file_path_sans_ext(rdsfiles))
spfiles_sp <- basename(tools::file_path_sans_ext(spfiles))
spfiles_sp <- gsub("_masked$", "", spfiles_sp)
spfiles_sp[!spfiles_sp %in% polyfiles_sp]
gsub("_masked", "", basename(tools::file_path_sans_ext(errorfiles))) %in% spfiles_sp[!spfiles_sp %in% polyfiles_sp]

# errorsp <- c("Mecyclothorax (Mecyclothorax) howei",
#              "Schizognathus viridiaeneus",
#              "Diemodynerus saucius",
#              "Pnirsus notaticollis",
#              "Pheroliodes monteithi",
#              "Helina micans",
#              "Torresitrachia leichhardti",
#              "Melanozosteria lentiginosa")


## >> Batch rerun for species showing errors ####
polygons_error_dir <- "/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/polygons_errors"
dir.create(polygons_error_dir)
working_error_dir <- "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/polygons_errors/"
setwd(working_error_dir)

source("~/gsdms_r_vol/tempdata/workdir/nesp_bugs/scripts/conr_iucn_eval.R")
basemap_file <- file.path(output_dir, "masks", "auslands_1poly_wgs84.shp")

mc.cores = length(errorfiles)
set.seed(1, kind = "L'Ecuyer-CMRG" )
system.time(invisible(mclapply(errorfiles,
                               conr_iucn_eval,
                               basemap_path = basemap_file,
                               working_dir = working_error_dir,
                               iucn_outpath = polygons_error_dir,
                               mc.cores = mc.cores)))
## >> Warning message:
##    In mclapply(errorfiles, conr_iucn_eval, basemap_path = basemap_file,:
##    scheduled cores 4, 15, 21, 1, 86, 115, 99, 63, 30, 58, 52, 49, 80, 55 
##    encountered errors in user code, all values of the jobs will be affected

## Check # files created in error directory
csvfiles <- list.files(polygons_error_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of .csv output files created from IUCN.eval(): "),
        length(csvfiles))

rdsfiles <- list.files(polygons_error_dir, pattern = ".rds$", 
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of .rds output files created from IUCN.eval(): "),
        length(rdsfiles))

IUCNshpfiles <- list.files(file.path(polygons_error_dir, "shapesIUCN"), 
                           pattern = ".shp$", 
                           full.names = TRUE, all.files = TRUE)
message(cat("Number of .shp output files created from IUCN.eval(): "),
        length(IUCNshpfiles))

pngfiles <- list.files(polygons_error_dir, pattern = "png$", recursive = TRUE, 
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of .png map files created from IUCN.eval(): "),
        length(pngfiles))

## Catch indices of species with unresolved errors from warninng message
eidx <- c(4, 15, 21, 1, 86, 115, 99, 63, 30, 58, 52, 49, 80, 55)
length(eidx)
message(cat("Number of species with unresolved errors: "),
        length(errorfiles) - length(rdsfiles))


## >> Individual reruns for species with unresolved errors ####
## NOTE: Rerun multiple times for a species
basemap <- readOGR(file.path(output_dir, "masks/auslands_wgs84.shp"))

## Read species data
dat <- as.data.table(readRDS(errorfiles[eidx[5]]))
dim(dat)
spname <- unique(dat$spfile)
message(cat("Processing species... ",
            spname))
dat <- dat[ , .(latitude, longitude, scientificName, family, year)]
names(dat) <- c("latitude", "longitude", "tax", "family", "coly")

## Run ConR function
out <- IUCN.eval(dat,
                 method.range = "alpha.hull",
                 alpha = 2,
                 Cell_size_AOO = 2,
                 country_map = basemap,
                 exclude.area = TRUE,
                 SubPop = FALSE,
                 DrawMap = TRUE,
                 write_file_option = "csv",
                 file_name = spname,
                 export_shp = TRUE,
                 write_shp = TRUE,
                 write_results = TRUE)

saveRDS(out, file = paste0(polygons_error_dir, "/", spname, ".rds"))

## Clear objects
rm(dat, spname, out)

## Remaining species with unresolved errors
## NOTE: 5 species remain with unsesolved errors
##  >> Error in { : task 1 failed - "task 1 failed - "object 'case' not found""
basename(errorfiles[eidx[c(2, 5, 10)]])


## >> Copy files ####
csvfiles2 <- list.files(polygons_error_dir, pattern = ".csv$",
                        full.names = TRUE, all.files = TRUE)
message(cat("Number of .csv output files created from IUCN.eval(): "),
        length(csvfiles2))
rdsfiles2 <- list.files(polygons_error_dir, pattern = ".rds$", 
                        full.names = TRUE, all.files = TRUE)
message(cat("Number of .rds output files created from IUCN.eval(): "),
        length(rdsfiles2))

IUCNshpfiles2 <- list.files(file.path(polygons_error_dir, "shapesIUCN"), 
                            pattern = ".dbf$", 
                            full.names = TRUE, all.files = TRUE)
message(cat("Number of .shp output files created from IUCN.eval(): "),
        length(IUCNshpfiles2))

pngfiles2 <- list.files(polygons_error_dir, pattern = "png$", recursive = TRUE, 
                        full.names = TRUE, all.files = TRUE)
message(cat("Number of .png map files created from IUCN.eval(): "),
        length(pngfiles2))

file.copy(rdsfiles2, polygons_dir,
          overwrite = FALSE, recursive = TRUE,
          copy.mode = TRUE, copy.date = TRUE)
file.remove(rdsfiles2)

file.copy(csvfiles2, polygons_dir,
          overwrite = FALSE, recursive = TRUE,
          copy.mode = TRUE, copy.date = TRUE)
file.remove(csvfiles2)

IUCNshpfiles2 <- list.files(file.path(polygons_error_dir, "shapesIUCN"), 
                            full.names = TRUE)
file.copy(IUCNshpfiles2, file.path(polygons_dir, "shapesIUCN"),
          overwrite = FALSE, recursive = TRUE,
          copy.mode = TRUE, copy.date = TRUE)
unlink(file.path(polygons_error_dir, "shapesIUCN"), recursive = TRUE)

pngdirs <- list.dirs(polygons_error_dir)
pngdirs <- pngdirs[-grep("/polygons_errors$", pngdirs)]
for(i in pngdirs){
  system(paste0("cp -R ", i, " ", polygons_dir))
}
unlink(polygons_error_dir, recursive = TRUE)


## >> Check files ####
csvfiles <- list.files(polygons_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of .csv output files created from IUCN.eval(): "),
        length(csvfiles))

rdsfiles <- list.files(polygons_dir, pattern = ".rds$", 
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of .rds output files created from IUCN.eval(): "),
        length(rdsfiles))

IUCNshpfiles <- list.files(file.path(polygons_dir, "shapesIUCN"), 
                           pattern = ".shp$", 
                           full.names = TRUE, all.files = TRUE)
message(cat("Number of .shp output files created from IUCN.eval(): "),
        length(IUCNshpfiles))

pngfiles <- list.files(polygons_dir, pattern = "png$", recursive = TRUE, 
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of .png map files created from IUCN.eval(): "),
        length(pngfiles))



## IV. Save IUCN.eval() outputs ####
## >> AOO & EOO areas ####
## Create data table from .csv output files
csvfiles <- list.files(polygons_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
out <- do.call("rbind", lapply(csvfiles , read.csv))
dim(out)
setorder(out, EOO, AOO)
out <- as.data.table(out)
write.csv(out, file = file.path(output_dir, "ala_polygons_areas.csv"), 
          row.names = FALSE)

message(cat("#species with EOOs: "),
        nrow(out[!is.na(EOO)]))
message(cat("#species without EOOs: "),
        nrow(out[is.na(EOO)]))
message(cat("max #records for species without EOOs: "),
        max(out[is.na(EOO)]$Nbe_unique_occ.))


## >> EOO polygons ####
## Create list of .rds output files
rdsfiles <- list.files(polygons_dir, pattern = ".rds$", 
                       full.names = TRUE, all.files = TRUE)
polynames <- basename(tools::file_path_sans_ext(rdsfiles))
temp <- lapply(rdsfiles, readRDS)
names(temp) <- polynames
length(temp)
saveRDS(temp, file = file.path(output_dir, "ala_IUCNeval_output.rds"))

## Create list of SPDF from .rds output files
temp2 <- lapply(temp, "[[", 1)
temp2 <- lapply(temp2, "[[", 2)
length(temp2)
saveRDS(temp2, file = file.path(output_dir, "ala_polygons.rds"))

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
saveRDS(temp3, file = file.path(output_dir, "ala_EOO.rds"))
# temp <- do.call(cbind, temp3)
# names(temp) <- names(temp3)

## Create list of species names without EOOs
errorlog <- file.path(output_dir, "errorlog_ala_polygonsR_20201215.txt")
errorfiles <- trimws(readLines(errorlog)[-1])
eidx <- c(4, 15, 21, 1, 86, 115, 99, 63, 30, 58, 52, 49, 80, 55)
errorsp <- gsub("_masked.rds$", "", basename(errorfiles[eidx[c(2, 5, 10)]]))
sort(c(names(na.eooIDX), errorsp))
write.csv(sort(c(names(na.eooIDX), errorsp)), 
          file = file.path(output_dir, "ala_noEOOspecies.csv"), 
          row.names = FALSE)
message(cat("Total number of species: "),
        length(c(names(na.eooIDX), errorsp)) + length(temp3))




## Display #records for species before/after masking ####
counts <- fread(file.path(output_dir, "ala_masked_datacounts.csv"))
message(cat("Number of species with more than 0 records: "),
        nrow(counts[n_masked > 0]))
message(cat("This is the same as number of polygons files created: "),
        length(rdsfiles))
## Because 3 files showed errors

message(cat("Number of species with 0 records: "),
        nrow(counts[n_masked == 0]))

message(cat("Number of species with at least 3 records for creating EOO: "),
        nrow(counts[n_masked >= 3]))
## Becasue IUCN.eval() removes duplicates by name-lat-long





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
clearPlot()
quickPlot::Plot(ausmask,
                title = "",
                axes = FALSE,
                legend = FALSE,
                col = "khaki",
                addTo = "ausmask",
                new = TRUE)
quickPlot::Plot(sp1,
                cols = "tomato3",
                title = "",
                addTo = "ausmask")

equalarea_proj <- CRS("+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
sp1_ee <- spTransform(sp1, CRSobj = equalarea_proj)

quickPlot::Plot(sp1_ee,
                cols = "tomato3",
                title = "",
                addTo = "ausmask")
sp1_ee <- readShapePoly("sp1", verbose=TRUE, proj4string=equalarea_proj)

plot(raster(mask_file), col = "khaki", axes = FALSE, box = FALSE, legend = FALSE)
plot(sp1, add = TRUE, pch = 17, col = "navy", cex = 0.5)

# ## DEBUGGING <end> -------------------------------------------------- ####






## EXTRA ------------------------------------------------------------- ####
## Other EOO options ####
n <- 500
dat <- as.data.table(readRDS(spfiles[n]))
dim(dat)
xyall <- xy.coords(dat[ , c("latitude", "longitude")])
length(xyall[[1]])

## Estimate EOO: rCAT package (endoresed by IUCN) - minimum convex polygon
## https://cran.r-project.org/web/packages/rCAT/index.html
## Lat, long data points format
x <- xyall$x
y <- xyall$y
xy <- data.frame(lat = x, long = y)
## find the true centre of the points
cp <- trueCOGll(xy)
## project to an equal area projection
xy_proj <- simProjWiz(xy,cp)
## Calculate the Extent of Occurrence EOO
## Returns: area returned is in x,y units, but negative as polygon is constructed anticlockwise
rcatEOO_m2 <- EOOarea(xy_proj)
rcatEOO_km2 <- rcatEOO_m2/1000000
## Calculate the Area of Occupancy AOO for 2km cells
## Returns: integer number of unique cells as an integer
cellsize_m <- 2000
rcatAOO_ncells <- AOOsimp(xy_proj, cellsize_m)
rcatAOO_km2 <- rcatAOO_ncells * (cellsize_m/1000)^2

## Estimate EOO: red package - minimum convex polygon
## https://cran.r-project.org/web/packages/red/index.html
## long, lat data points format
xy_mat <- as.matrix(cbind(xyall$y, xyall$x))
red_EOO <- red::eoo(xy_mat)
red_AOO <- red::aoo(xy_mat)

## Estimate EOO: alphahull package
## https://cran.r-project.org/web/packages/alphahull/index.html
## Used for mapping polygons only, NOT for EOO area calculations
message("Removing duplicates from lat-long...")
xysub <- xy.coords(dat[!duplicated(dat[ , c("latitude", "longitude")]) , c("latitude", "longitude")])
length(xysub[[1]])
ahull_EOO <- ahull(x = xysub$x, y = xysub$y, alpha = 2)
areaahull(ahull_EOO)
plot(raster(mask.file), col = "khaki", axes = FALSE, box = FALSE, legend = FALSE)
plot(ahull_EOO, add = TRUE, col = "mediumaquamarine")
points(dat[,.(longitude, latitude)], pch = 2, col = "navy", cex = 0.5)
## Plot
plot(raster(mask.file), col = "khaki", axes = FALSE, box = FALSE, legend = FALSE)
plot(ahull_EOO, add = TRUE, col = "mediumaquamarine")
points(dat[,.(longitude, latitude)], pch = 2, col = "navy", cex = 0.5)



# ## future_lapply with warnings and errors: not working
# ## ------------------------------------------
# invisible(
#   future.apply::future_lapply(
#     spfiles,
#     function(x){
#       tmp <- tryCatch(expr = conr_iucn_eval(species_filename = x, 
#                                             basemap_path = basemap_path, 
#                                             working_dir = working_dir, 
#                                             iucn_outpath = polygons_dir),
#                       
#                       error = function(errorcondition) {
#                         cat(
#                           paste(
#                             "Error in: ", as.character(spname), "\n"), 
#                           file = logfile, 
#                           append = TRUE)
#                       },
#                       
#                       warning = function(warningcondition) {
#                         # Add warning message to the log file
#                         cat(
#                           paste(
#                             "\nEOO parameter cannot be estimated for",
#                             as.character(spname),
#                             "because there is less than 3 unique occurrences"
#                           ),
#                           file = logfile,
#                           append = TRUE)
#                       }
#       )
#     }, future.seed = TRUE))
#
# 
# ## Compare system times: Extract points
# ## ------------------------------------------
# wgs_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# basemap <- readOGR(basemap_file, verbose = FALSE)
# crs(basemap) <- wgs_crs
# ausmap <- raster(mask_file)
# sp_xy <- SpatialPoints(dat[,c("longitude", "latitude")], proj4string = CRS(as.character(wgs_crs)))
# system.time(gIntersects(sp_xy, basemap)) ## only gives true/FALSE
# system.time(extract(ausmask, sp_xy)) ## can get indices
# 
# 
# ## Comapre system times: Plotting 
# ## ------------------------------------------
# system.time(ausmask <- raster(mask_file))
# system.time(plot(raster(mask_file), col = "khaki", axes = FALSE, box = FALSE, legend = FALSE))
# plot(sp_xy, add = TRUE, pch = 17, col = "navy", cex = 0.5)
# 
# clearPlot()
# system.time(quickPlot::Plot(ausmask, 
#                             title = "",
#                             axes = FALSE, 
#                             legend = FALSE,
#                             col = "khaki", 
#                             addTo = "ausmask", 
#                             new = TRUE))
# Plot(sp_xy, pch = 17, 
#      col = "darkcyan", 
#      title = "", 
#      addTo = "ausmask")
# 
# wgs_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# system.time(basemap <- shapefile(basemap_file))
# crs(basemap) <- wgs_crs
# 
# system.time(plot(basemap, col = "khaki"))
# plot(sp_xy, add = TRUE, pch = 17, col = "lightgreen", cex = 0.5)
# 
# clearPlot() 
# system.time(quickPlot::Plot(basemap, 
#                             title = "",
#                             axes = FALSE, 
#                             legend = FALSE,
#                             addTo = "basemap", 
#                             new = TRUE))
# Plot(sp_xy, pch = 19, 
#      col = "hotpink", 
#      title = "", 
#      addTo = "basemap")
# 
# 
# ## To order files by date created
# details <-  file.info(rdsfiles)
# details  <-  details[with(details, order(as.POSIXct(mtime), decreasing = TRUE)), ]

