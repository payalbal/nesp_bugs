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
x <- c("data.table", "sp", "raster", "rgdal", "alphahull", "red", "rCAT")
lapply(x, require, character.only = TRUE)
rm(x)

## Server paths
bugs_data = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
mask_data = file.path(bugs_data, "masks")
output_dir = file.path(bugs_data, "outputs")
spdata_dir = file.path(output_dir, "ala_data" ,"spdata")
map_dir = file.path(spdata_dir, "spmaps_unmasked")

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
temp <- as.data.table(readRDS(spfiles[1]))
dim(temp)
xy_all <- xy.coords(temp[ , c("longitude", "latitude")])
# temp <- temp[!duplicated(temp[ , c("longitude", "latitude")]), ]
xy_subset <- xy.coords(temp[!duplicated(temp[ , c("longitude", "latitude")]) , c("longitude", "latitude")])

## Estimate EOO: red
## https://cran.r-project.org/web/packages/red/index.html
red_EOO <- red::eoo(as.matrix(cbind(xy_all$x, xy_all$y)))
red_AOO <- red::aoo(as.matrix(cbind(xy_all$x, xy_all$y)))

## Estimate EOO: rCAT in Cartesian Coorindates - XX
## https://cran.r-project.org/web/packages/rCAT/index.html
## Seems wrong. aoo is 3 irrspective of cell size.
cartcords <- ll2cart(latr = xy_all$x, longr = xy_all$y)
rcat_EOO <- rCAT::EOOarea(cartcords[,1:2])
rcat_AOO <- rCAT::AOOsimp(cartcords[,1:2], cellsize = 100000)


## Estimate EOO: alphahull package
## https://cran.r-project.org/web/packages/alphahull/index.html
ahull_EOO <- ahull(x = xy_subset$x, y = xy_subset$y, alpha = 3)
areaahull(ahull_EOO)
plot(raster(mask.file), col = "khaki", axes = FALSE, box = FALSE, legend = FALSE)
plot(ahull_EOO, add = TRUE, col = "mediumaquamarine")
points(temp[,.(longitude, latitude)], pch = 2, col = "navy", cex = 0.5)


# Random sample in the unit square
x <- runif(20, 0, 180)
y <- runif(20, 0, 90)
p <- as.data.frame(cbind(x, y))
# p <- xy.coords(p)
pcart <- ll2cart(latr = p$x, longr = p$y)

plot(p)  

ahull.obj <- ahull(p, alpha = 2)
alphahull::areaahull(ahull.obj)

red::eoo(pcart[,1:2])
red::aoo(pcart[,1:2])

rCAT::AOOsimp(pcart[,1:2], cellsize = 2)
rCAT::EOOarea(pcart[,1:2])

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
