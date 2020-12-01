## ALA data mapping
## For indivial species with > = 5 records
## Species habitat polygons for EOO and AOO
## Species distribution (wide/restricted)
## Prelim analysis area (in/out; prop of habitat within) - to clip
## Fire extent (yes/no; prop of habitat impacted) - 

## For all species
## Think or resolution of analyses. 
## Stacked polygons or points per grid?


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

## Server paths
bugs_data = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_data, "outputs")
spdata_dir = file.path(output_dir, "ala_data" ,"spdata")
spmasked_dir <- file.path(output_dir, "ala_data" ,"spdata_masked")
dir.create(spmasked_dir)
map_dir = file.path(spdata_dir, "spmaps_unmasked")
polygons_dir = file.path(bugs_data, "outputs", "polygons")
dir.create(polygons_dir)

## Files
spfiles <- list.files(spdata_dir, pattern= ".rds$", full.names = TRUE)
message(cat("Total number of species in cleaned ALA data: "),
        length(spfiles))

## Mask data ####
source("/tempdata/workdir/nesp_bugs/scripts/mask_spdat.R")
mask_file <- file.path(output_dir, "masks", "ausmask_noaa_1kmWGS_NA.tif")

## Package: mclappy
mc.cores = future::availableCores()-2
set.seed(1, kind = "L'Ecuyer-CMRG" )
system.time(mclapply(spfiles,
                     mask_spdat,
                     mask_file = mask_file, 
                     data_dir = spmasked_dir,
                     mc.cores = mc.cores))
length(list.files(spmasked_dir, pattern= "_masked.rds$", full.names = TRUE))

## Note: mclapply() is much faster than future_lapply()
## Cannot use tryCatch with mclappply??
## Can use this for subsequent runs if no errors show from first run using future_lapply()

## Package: future
plan(multiprocess, workers = future::availableCores()-2)
options(future.globals.maxSize = +Inf) ## CAUTION: Set this to a value, e.g. availablecores-1?/RAM-10?
errorlog <- paste0(output_dir, "/errorlog_ala_polygonsR_", gsub("-", "", Sys.Date()), ".txt")
writeLines(c(""), errorlog)

system.time(
  suppressWarnings(
    future.apply::future_lapply(
      spfiles[1:100],
      function(x){
        tmp <- tryCatch(expr = mask_spdat(species_filename = x,
                                          mask_file = mask_file,
                                          data_dir = spmasked_dir),
                        error = function(e) {
                          cat(
                            paste(as.character(x), "\n"),
                            file = errorlog,
                            append = TRUE)
                        })
      }, future.seed = TRUE)))

length(list.files(spmasked_dir, pattern= "_masked.rds$", full.names = TRUE))

## Species polygons ####
## Using ConR package - minimum convex polygon or alpha hulls
## https://cran.r-project.org/web/packages/ConR/index.html
## Notes: 
##  WGS84 required for data
##  data format: latitude, longitude (in decimal degrees), and taxon name
##  outputs for EOO and AOO same as from {red} when method.range = "convex.hull"
##  ** country_map + exclude.area can be used for cropping to prelimiunary analysis area

spfiles <- list.files(spmasked_dir, pattern= "_masked.rds$", full.names = TRUE)
basemap_file <- file.path(output_dir, "masks", "auslands_1poly_wgs84.shp")

## Subset ####
n = sample(1:length(spfiles), 20)
spfile_sub <- spfiles[n]

# ## Run IUCN.eval in sequence ####
# basemap <- readOGR(file.path(output_dir, "masks/auslands_wgs84.shp"))
# setwd("~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/polygons/")
# 
# for (spfile in spfile_sub){
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

## Run in parallel ####
## Set working conditions for ConR function
source("~/gsdms_r_vol/tempdata/workdir/nesp_bugs/scripts/conr_iucn_eval.R")
working_dir <- "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/polygons/"

## Package: mclappy
system.time(invisible(mclapply(spfile_sub,
                               conr_iucn_eval,
                               basemap_path = basemap_file, 
                               working_dir = working_dir, 
                               iucn_outpath = polygons_dir, 
                               mc.cores = 10)))

## Package: future
plan(multiprocess, workers = 20)
# plan(multiprocess, workers = future::availableCores()-2)
options(future.globals.maxSize = +Inf) ## CAUTION: Set this to a value, e.g. availablecores-1?/RAM-10?

system.time(
  suppressWarnings(
    future.apply::future_lapply(
      spfile_sub,
      function(x){
        tmp <- tryCatch(expr = conr_iucn_eval(species_filename = x,
                                              basemap_path = basemap_path,
                                              working_dir = working_dir,
                                              iucn_outpath = polygons_dir)
        )
      }, future.seed = TRUE)))



## Save outputs
out <- list.files(polygons_dir, pattern = ".csv", full.names = TRUE)
out <- do.call("rbind", lapply(out , read.csv))
out <- out[order(out$EOO, na.last = FALSE),]
write.csv(out, file = file.path(output_dir, "IUCNcalc_alaMTE5sp.csv"))

EOO_NAs <- out[which(is.na(out$EOO)),]
write.csv(EOO_NAs, file = file.path(output_dir, "EOO_NAs_alaMTE5sp.csv"))


## Preliminary analysis area
paa <- file.path(bugs_data, "Preliminary_Analysis_Areas/prelim_analysis_areas_dissolve.shp")
paa <- rgdal::readOGR(paa)
plot(paa, add=TRUE)

## Fire layers
fireseverity <- raster(file.path(bugs_data, "fire/AUS_GEEBAM_Fire_Severity_NIAFED20200224/AUS_GEEBAM_Fire_Severity_NIAFED20200224.tif"))
fireextent <- rgdal::readOGR(file.path(bugs_data, "fire/NIAFED_v20200623/NIAFED_20190701_20200622_v20200623.shp"))






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





## EXTRAS
# ## future_lapply with warnings and errors: not working
# ## ------------------------------------------
# invisible(
#   future.apply::future_lapply(
#     spfile_sub,
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