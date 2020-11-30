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
       "alphahull", "ConR", "rnaturalearthdata", 
       "future", "future.apply")
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

mask_file <- file.path(output_dir, "masks", "ausmask_noaa_1kmWGS_NA.tif")
basemap_file <- file.path(output_dir, "masks", "auslands_wgs84.shp")

## Step 1 - Mask data
all_na <- "/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/ala_data/spdata/cosa_pharetra.rds"
species_filename <- "/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/ala_data/spdata/oithona_similis.rds"
species_filename <- "/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/ala_data/spdata/plakobranchus_ocellatus.rds"
species_filename <- spfiles[10]

dat <- as.data.table(readRDS(species_filename))
system.time(plot(raster(mask_file), col = "khaki", axes = FALSE, box = FALSE, legend = FALSE))
system.time(quickPlot::Plot(ausmask, col = "khaki", axes = FALSE, legend = FALSE, title = ""))
system.time(plot(basemap, col = "khaki"))
# ## Clip points to shapefile
# wgs_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# basemap <- readOGR(basemap_file, verbose = FALSE)
# crs(basemap) <- wgs_crs
# sp_xy <- SpatialPoints(dat[,c("longitude", "latitude")], proj4string = CRS(as.character(wgs_crs)))
# plot(sp_xy, add = TRUE, pch = 2, col = "navy", cex = 0.5)
# int <- gIntersects(sp_xy, basemap) 

## Clip points to raster mask
ausmask <- raster(mask_file)
sp_xy <- SpatialPoints(dat[,c("longitude", "latitude")], proj4string = crs(ausmask))
plot(sp_xy, add = TRUE, pch = 2, col = "navy", cex = 0.5)
vals <- extract(ausmask, sp_xy)
in_mask <- !is.na(vals)
dat_masked <- dat[in_mask, ]
plot(sp_xy[in_mask], add = TRUE, pch = 2, col = "red", cex = 0.5)


system.time(invisible(mclapply(spfiles,
                               func,
                               
                               mc.cores = 10)))


# ## Subset to species with > = 5 records - NOT USEFUL; 
# ## Note: countMTE5.csv is from before cleaning-11 and saving by species (ala_byspecies.R)
# countMTE5 <- as.data.table(read.csv(file.path(output_dir, "countMTE5.csv")))
# y <- countMTE5$scientificName
# y <- stringr::str_replace_all(y, " ", "00xx00")
# y <- stringr::str_replace_all(y, "[^[:alnum:]]", "")
# y <- tolower(gsub("00xx00", "_", y))
# y <- sort(unique(y))
# 
# message(cat("Number of species with > = 5 records: "),
#         dim(countMTE5)[1])
# spfiles <- spfiles[tools::file_path_sans_ext(basename(spfiles)) %in% y]
# mapfiles <- mapfiles[tools::file_path_sans_ext(basename(mapfiles)) %in% y]
# message(cat("Are the number of files (rds; pfd) retained 
#             same as number of species with > = 5 records: "),
#         length(spfiles) == dim(countMTE5)[1], "; ",
#         length(mapfiles) == dim(countMTE5)[1])




## Estimate EOO: ConR package - minimum convex polygon or alpha hulls
## https://cran.r-project.org/web/packages/ConR/index.html
## Notes: 
##  WGS84 required for data
##  data format: latitude, longitude (in decimal degrees), and taxon name
##  outputs for EOO and AOO same as from {red} when method.range = "convex.hull"
##  ** country_map + exclude.area can be used for cropping to prelimiunary analysis area

spfiles <- list.files(spdata_dir, pattern= "_masked.rds$", full.names = TRUE)

## Subset ####
n = sample(1:length(spfiles), 20)
spfile_sub <- spfiles[n]

# ## Run in sequence ####
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





## EXTRAS...
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