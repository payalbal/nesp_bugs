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

polygons_dir = file.path(output_dir,"polygons_alphahull_masked_10k")
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
spfiles <- list.files(spmasked_dir, pattern= "_masked.rds$", full.names = TRUE)
message(cat("Total number of species in cleaned ALA data: "),
        length(spfiles))


# ## >> Run IUCN.eval in sequence ####
# basemap <- readOGR(basemap_file)
# hull.method <- "convex.hull"  # alpha.hull
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
#                    # method.range = hull.method,
#                    alpha = 2,
#                    Cell_size_AOO = 2,
#                    Cell_size_locations = 2
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
source("~/gsdms_r_vol/tempdata/workdir/nesp_bugs/scripts/conr_iucn_eval.R")
basemap_file <- file.path(output_dir, "masks", "auslands_1poly_wgs84.shp")
hull.method <- "alpha.hull" # "convex.hull"  

# ## Package: mclappy - does not show/catch errors
# mc.cores = future::availableCores()-2
# set.seed(1, kind = "L'Ecuyer-CMRG" )
# system.time(invisible(mclapply(spfiles,
#                                conr_iucn_eval,
#                                hull.method = hull.method,
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
errorlog <- paste0(output_dir, "/errorlog_ala_polygons_convhullR_", gsub("-", "", Sys.Date()), ".txt")
# if(file.exists(errorlog)){unlink(errorlog)}
writeLines(c(""), errorlog)

system.time(
  suppressWarnings(
    future.apply::future_lapply(
      spfiles,
      function(x){
        tmp <- tryCatch(expr = conr_iucn_eval(species_filename = x,
                                              hull.method = hull.method,
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

IUCNshpfiles <- list.files(file.path(polygons_dir, "shapesIUCN"), 
                       pattern = ".shp$", 
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of .shp output files created from IUCN.eval(): "),
        length(IUCNshpfiles))

pngfiles <- list.files(polygons_dir, pattern = "png$", recursive = TRUE, 
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of .png map files created from IUCN.eval(): "),
        length(pngfiles))


## Resolve errors ####
polygons_error_dir <- "/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/polygons_errors"
dir.create(polygons_error_dir)
working_error_dir <- "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/polygons_errors/"
setwd(working_error_dir)

source("~/gsdms_r_vol/tempdata/workdir/nesp_bugs/scripts/conr_iucn_eval.R")
basemap_file <- file.path(output_dir, "masks", "auslands_1poly_wgs84.shp")

## Aplha hull polygon error file
# errorlog <- file.path(output_dir, "errorlog_ala_polygons_alphahullR_20201215.txt")

# Convex hull polygon error file
# errorlog <- file.path(output_dir, "errorlog_ala_polygons_convhullR_20210125.txt")
errorlog <- file.path(output_dir, "errorlog_ala_polygons_convhullR_20210125.txt")
errorfiles <- trimws(readLines(errorlog)[-1])
message(cat("Number of species showinng errors: "),
        length(errorfiles))
all(errorfiles %in% spfiles)

## List species not in output files
polyfiles_sp <- basename(tools::file_path_sans_ext(rdsfiles))
spfiles_sp <- basename(tools::file_path_sans_ext(spfiles))
spfiles_sp <- gsub("_masked$", "", spfiles_sp)
spfiles_sp[!spfiles_sp %in% polyfiles_sp]
message(cat("errorfiles == species nto found in output file: "),
        all(gsub("_masked", "", basename(tools::file_path_sans_ext(errorfiles))) 
        %in% spfiles_sp[!spfiles_sp %in% polyfiles_sp]))

  # ## >> alpha hull run - error species ####
  # errorsp <- c("Mecyclothorax (Mecyclothorax) howei",
  #              "Schizognathus viridiaeneus",
  #              "Diemodynerus saucius",
  #              "Pnirsus notaticollis",
  #              "Pheroliodes monteithi",
  #              "Helina micans",
  #              "Torresitrachia leichhardti",
  #              "Melanozosteria lentiginosa")
  # 
  #
  # ## >> Batch rerun for species showing errors
  # mc.cores = length(errorfiles)
  # set.seed(1, kind = "L'Ecuyer-CMRG" )
  # system.time(invisible(mclapply(errorfiles,
  #                                conr_iucn_eval,
  #                                hull.method = hull.method,
  #                                basemap_path = basemap_file,
  #                                working_dir = working_error_dir,
  #                                iucn_outpath = polygons_error_dir,
  #                                mc.cores = mc.cores)))
  #   ## >> Warning message:
  #   ##    In mclapply(errorfiles, conr_iucn_eval, basemap_path = basemap_file,:
  #   ##    scheduled cores 4, 15, 21, 1, 86, 115, 99, 63, 30, 58, 52, 49, 80, 55 
  #   ##    encountered errors in user code, all values of the jobs will be affected
  # 
  # ## Check # files created in error directory
  # csvfiles <- list.files(polygons_error_dir, pattern = ".csv$",
  #                        full.names = TRUE, all.files = TRUE)
  # message(cat("Number of .csv output files created from IUCN.eval(): "),
  #         length(csvfiles))
  # 
  # rdsfiles <- list.files(polygons_error_dir, pattern = ".rds$", 
  #                        full.names = TRUE, all.files = TRUE)
  # message(cat("Number of .rds output files created from IUCN.eval(): "),
  #         length(rdsfiles))
  # 
  # IUCNshpfiles <- list.files(file.path(polygons_error_dir, "shapesIUCN"), 
  #                            pattern = ".shp$", 
  #                            full.names = TRUE, all.files = TRUE)
  # message(cat("Number of .shp output files created from IUCN.eval(): "),
  #         length(IUCNshpfiles))
  # 
  # pngfiles <- list.files(polygons_error_dir, pattern = "png$", recursive = TRUE, 
  #                        full.names = TRUE, all.files = TRUE)
  # message(cat("Number of .png map files created from IUCN.eval(): "),
  #         length(pngfiles))
  # 
  # ## Catch indices of species with unresolved errors from warninng message
  # eidx <- c(4, 15, 21, 1, 86, 115, 99, 63, 30, 58, 52, 49, 80, 55)
  # length(eidx)
  # message(cat("Number of species with unresolved errors: "),
  #         length(errorfiles) - length(rdsfiles))


## >> Individual reruns for species with unresolved errors ####
## NOTE: Rerun multiple times for a species
basemap <- readOGR(file.path(output_dir, "masks/auslands_wgs84.shp"))

## Read species data

  # dat <- as.data.table(readRDS(errorfiles[eidx[6]]))

dat <- as.data.table(readRDS(errorfiles[2]))
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

  # ## Remaining species with unresolved errors
  # ## NOTE: 5 species remain with unsesolved errors
  # ##  >> Error in { : task 1 failed - "task 1 failed - "object 'case' not found""
  # basename(errorfiles[eidx[c(2, 5, 10)]])
  # 
  # 
  # ## >> Copy files ####
  # csvfiles2 <- list.files(polygons_error_dir, pattern = ".csv$",
  #                        full.names = TRUE, all.files = TRUE)
  # message(cat("Number of .csv output files created from IUCN.eval(): "),
  #         length(csvfiles2))
  # rdsfiles2 <- list.files(polygons_error_dir, pattern = ".rds$", 
  #                        full.names = TRUE, all.files = TRUE)
  # message(cat("Number of .rds output files created from IUCN.eval(): "),
  # 
  # IUCNshpfiles2 <- list.files(file.path(polygons_error_dir, "shapesIUCN"), 
  #                            pattern = ".dbf$", 
  #                            full.names = TRUE, all.files = TRUE)
  # message(cat("Number of .shp output files created from IUCN.eval(): "),
  #         length(IUCNshpfiles2))
  # 
  # pngfiles2 <- list.files(polygons_error_dir, pattern = "png$", recursive = TRUE, 
  #                        full.names = TRUE, all.files = TRUE)
  # message(cat("Number of .png map files created from IUCN.eval(): "),
  #         length(pngfiles2))
  # 
  # file.copy(rdsfiles2, polygons_dir,
  #           overwrite = FALSE, recursive = TRUE,
  #           copy.mode = TRUE, copy.date = TRUE)
  # file.remove(rdsfiles2)
  # 
  # file.copy(csvfiles2, polygons_dir,
  #           overwrite = FALSE, recursive = TRUE,
  #           copy.mode = TRUE, copy.date = TRUE)
  # file.remove(csvfiles2)
  # 
  # IUCNshpfiles2 <- list.files(file.path(polygons_error_dir, "shapesIUCN"), 
  #                             full.names = TRUE)
  # file.copy(IUCNshpfiles2, file.path(polygons_dir, "shapesIUCN"),
  #           overwrite = FALSE, recursive = TRUE,
  #           copy.mode = TRUE, copy.date = TRUE)
  # unlink(file.path(polygons_error_dir, "shapesIUCN"), recursive = TRUE)
  # 
  # pngdirs <- list.dirs(polygons_error_dir)
  # pngdirs <- pngdirs[-grep("/polygons_errors$", pngdirs)]
  # for(i in pngdirs){
  #   system(paste0("cp -R ", i, " ", polygons_dir))
  # }
  # unlink(polygons_error_dir, recursive = TRUE)


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
write.csv(pngfiles, file.path(output_dir, "convexHull_pngfiles.csv"), 
          row.names = FALSE)


## Save IUCN.eval() outputs ####
## >> AOO & EOO areas ####
## Create data table from .csv output files
csvfiles <- list.files(polygons_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
out <- do.call("rbind", lapply(csvfiles , read.csv))
dim(out)
setorder(out, EOO, AOO)
out <- as.data.table(out)

  # ## Add rows for species with errors
  # basename(errorfiles[eidx[c(2, 5, 10)]])
  # dat <- do.call("rbind", lapply(errorfiles[eidx[c(2, 5, 10)]], readRDS))
  # r1 <- c(unique(dat$scientificName)[1], rep(NA, 9))
  # r2 <- c(unique(dat$scientificName)[2], rep(NA, 9))
  # r3 <- c(unique(dat$scientificName)[3], rep(NA, 9))
  # r <- data.table(rbind(r1, r2, r3))
  # names(r) = names(out)
  # out <- rbind(out, r)

## Save outputs
write.csv(out, file = file.path(output_dir, "ala_polygons_areas_convhull.csv"), 
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
saveRDS(temp, file = file.path(output_dir, "ala_IUCNeval_output_convhull.rds"))

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


## Combine alpha hull and convex hull polygons area tables
## Note convex hull areas from polygonns_convhull_10k to keep it consistent with alpha hull
##  so that in both unstances unique occurrences are calculated from a grid of 10k cell size
ch <- fread(file.path(output_dir, "ala_polygons_areas_convhull.csv"))[,1:4]
ah <- fread(file.path(output_dir, "ala_polygons_areas_alphahull.csv"))[,1:4]

names(ch)[4] <- names(ah)[4] <- "uniqueN"
setorder(ah, X)
setorder(ch, X)
  
# ch10k <- fread(file.path(output_dir, "ala_polygons_areas_convhull_10k.csv"))[,1:4]
  # setorder(ch10k, X)
  # all(ch$X == ch10k$X)
  # all.equal(ch, ch10k)
  # all(ch$EOO == ch10k$EOO, na.rm = TRUE)
  # id <- which(ch$AOO != ch10k$AOO) ## AOO column differs; 
  # ## Note: Difference between convhull and convhull_10k
  # ##  looks like ch10k is wrong in AOO calcs because it gives AOO as 4 
  # ##  when there are 2 unique occurrences (2 * (2*2 cell) = 8km2)


setkey(ch, X)
setkey(ah, X)

out <- merge(ch, ah, by ="X", all = TRUE, 
             suffixes=c(".convhull", ".alphahull"))
names(out)[1] <- "Species"
write.csv(out, file = file.path(output_dir, "ala_polygons_areas_alphaconvhull.csv"), 
          row.names = FALSE)



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
#                                             hull.method = "alpha.hull",
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

