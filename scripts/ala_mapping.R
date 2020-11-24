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


options(rgl.useNULL=TRUE) ## for suppress warnings from library(red)
x <- c("data.table", "sp", "raster", "rgdal", "alphahull", "red", "rCAT", "ConR", "rnaturalearthdata")
lapply(x, require, character.only = TRUE)
rm(x)

## Server paths
bugs_data = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
mask_data = file.path(bugs_data, "masks")
output_dir = file.path(bugs_data, "outputs")
spdata_dir = file.path(output_dir, "ala_data" ,"spdata")
map_dir = file.path(spdata_dir, "spmaps_unmasked")
polypath = file.path(bugs_data, "outputs", "polygons")
dir.create(polypath)

## Files
spfiles <- list.files(spdata_dir, pattern= ".rds$", full.names = TRUE)
mapfiles <- list.files(map_dir, pattern = ".pdf$", full.names = TRUE)
mask.file = file.path(output_dir, "masks","ausmask_noaa_1kmWGS_NA.tif")
message(cat("Total number of species in cleaned ALA data: "),
        length(spfiles))

## Subset to species with > = 5 records ###
countMTE5 <- as.data.table(read.csv(file.path(output_dir, "countMTE5.csv")))
y <- countMTE5$scientificName
y <- stringr::str_replace_all(y, " ", "00xx00")
y <- stringr::str_replace_all(y, "[^[:alnum:]]", "")
y <- tolower(gsub("00xx00", "_", y))
y <- sort(unique(y))

message(cat("Number of species with > = 5 records: "),
        dim(countMTE5)[1])
spfiles <- spfiles[tools::file_path_sans_ext(basename(spfiles)) %in% y]
mapfiles <- mapfiles[tools::file_path_sans_ext(basename(mapfiles)) %in% y]
message(cat("Are the number of files (rds; pfd) retained 
            same as number of species with > = 5 records: "),
        length(spfiles) == dim(countMTE5)[1], "; ",
        length(mapfiles) == dim(countMTE5)[1])

## EOO polygons ##
n = 500
temp <- as.data.table(readRDS(spfiles[n]))
dim(temp)
xyall <- xy.coords(temp[ , c("latitude", "longitude")])
length(xyall[[1]])

## Estimate EOO: alphahull package
## https://cran.r-project.org/web/packages/alphahull/index.html
## Used for mapping polygons only, NOT for EOO area calculations
message("Removing duplicates from lat-long...")
xysub <- xy.coords(temp[!duplicated(temp[ , c("latitude", "longitude")]) , c("latitude", "longitude")])
length(xysub[[1]])
ahull_EOO <- ahull(x = xysub$x, y = xysub$y, alpha = 2)
areaahull(ahull_EOO)
plot(raster(mask.file), col = "khaki", axes = FALSE, box = FALSE, legend = FALSE)
plot(ahull_EOO, add = TRUE, col = "mediumaquamarine")
points(temp[,.(longitude, latitude)], pch = 2, col = "navy", cex = 0.5)


## Estimate EOO: ConR package - minimum convex polygon or alpha hulls
## https://cran.r-project.org/web/packages/ConR/index.html
## 
## Notes: 
##  WGS84 required for data
##  data format: latitude, longitude (in decimal degrees), and taxon name
## outputs for EOO and AOO same as from {red} when method.range = "convex.hull"
require(rnaturalearthdata)
setwd("~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/polygons/")
df <- temp[ , .(latitude, longitude, scientificName, family, year)]
spname <- unique(temp$spfile)
basemap <- readOGR(file.path(output_dir, "masks/auslands_wgs84.shp"))
names(df) <- c("latitude", "longitude", "tax", "family", "coly")
out <- IUCN.eval(df, method.range = "alpha.hull",
                 alpha = 2, Cell_size_AOO = 2,
                 country_map = basemap,
                 SubPop = FALSE,
                 DrawMap = TRUE,
                 write_file_option = "csv", 
                 file_name = spname,
                 export_shp = TRUE, 
                 write_shp = TRUE, 
                 write_results = TRUE)


## country_map + exclude.area can be used for cropping to prelimiunary analysis area

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





## Plot
plot(raster(mask.file), col = "khaki", axes = FALSE, box = FALSE, legend = FALSE)
plot(ahull_EOO, add = TRUE, col = "mediumaquamarine")
points(temp[,.(longitude, latitude)], pch = 2, col = "navy", cex = 0.5)

# map_filename <- ...
# pdf(map_filename)
# plot(raster(mask.file), col = "peru", axes = FALSE, box = FALSE, legend = FALSE)
# plot(out, add = TRUE, col = "grey")
# points(temp[,.(longitude, latitude)], pch = 2, col = "blue", cex = 0.5)
# dev.off()

## AOO polygons

## Notes on AOO from IUCN guidelines
## If EOO is less than AOO, EOO should be changed to make it equal to AOO to ensure consistency with the definition of AOO as an area within EOO.

## Load mask
ausmask <- file.path(output_dir, "masks", "ausmask_noaa_250mEE.tif")
ausmask <- raster(ausmask)
plot(ausmask, axes = FALSE, box = FALSE, legend = FALSE)


## Preliminary analysis area
paa <- file.path(bugs_data, "Preliminary_Analysis_Areas/prelim_analysis_areas_dissolve.shp")
paa <- rgdal::readOGR(paa)
plot(paa, add=TRUE)

## Fire layers
fireseverity <- raster(file.path(bugs_data, "fire/AUS_GEEBAM_Fire_Severity_NIAFED20200224/AUS_GEEBAM_Fire_Severity_NIAFED20200224.tif"))
fireextent <- rgdal::readOGR(file.path(bugs_data, "fire/NIAFED_v20200623/NIAFED_20190701_20200622_v20200623.shp"))


## Fire overlap analyses

## Species maps
qa[which(qa$exclude == 1),]$name
