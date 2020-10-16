## Create australia coastline shapefile

## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

devtools::install_github('smwindecker/gdaltools', force = TRUE)

x <- c("sp", "raster", "gdaltools", "rgdal", "gdalUtils")
lapply(x, require, character.only = TRUE)


## Server paths
output_dir = file.path(getwd(), "nesp_bugs", "outputs")
bugs_data = file.path(getwd(), "nesp_bugs", "nesp_bugs_data")

# ## Local paths
# output_dir = "/Volumes/uom_data/nesp_bugs_data/outputs"
# bugs_data = "/Volumes/uom_data/nesp_bugs_data"

# install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "binary")
# devtools::install_github('skiptoniam/sense')
source("/Users/payalb/Dropbox/Projects/discovery_trade/analyses/regSSP_fraclu/scripts/gdal_raster_functions.R")
x <- c("sp", "rgeos","rnaturalearth", "rnaturalearthhires", "devtools", "usethis")
lapply(x, require, character.only = TRUE)


## Temp mask using rnaturaleath package ####
plot(sf::st_geometry(rnaturalearth::ne_countries(country = "australia",
                                                 returnclass = "sf")))
## coastline more detailed in ne_states compared to ne_country
aus.mask <- rnaturalearth::ne_states("australia",
                                     returnclass = "sf")
# ## Reproject to Australian Albers
# aus.mask <- sf::st_transform(aus.mask,
#                                crs = 3577)
# plot(sf::st_geometry(aus.mask))

aus.mask <- sf::st_union(aus.mask)
aus.mask <- as(aus.mask, "Spatial")
# ext <-  c(113.338953078, 153.569469029, -43.6345972634, -10.6681857235) #xmin, xmax, ymin,ymax
aus.mask <- rasterize(aus.mask, raster(ext=extent(aus.mask), res =  0.008333333), field = 1) ## 1km2: 0.008333333
## Reproject to Australian Albers
newproj <- "+proj=aea +lat_0=0 +lon_0=132 +lat_1=-18 +lat_2=-36 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
aus.mask.wgs <- aus.mask
aus.mask <- projectRaster(aus.mask, crs= newproj)
plot(aus.mask, col = "grey", axes = FALSE, box = FALSE, legend = FALSE)
writeRaster(aus.mask, "./output/ausmask_3577.tif", format = "GTiff")
writeRaster(aus.mask.wgs, "./output/ausmask_WGS.tif", format = "GTiff")


## Mask using GEODATA COAST 100K 2004 ####
## Projection: Projection/datum: Geographical coordinates using the Geocentric Datum of Australia 1994 (GDA94)
# aus_geodata_poly <- sf::st_read(file.path(bugs_data, "env_data/GEODATA COAST 100K 2004/61395_shp/australia/cstauscd_r.shp"))
aus_geodata <- rgdal::readOGR(file.path(bugs_data, "env_data/GEODATA COAST 100K 2004/61395_shp/australia/cstauscd_r.shp"))
unique(aus_geodata$FEAT_CODE)
aus_mainland <- aus_geodata[aus_geodata$FEAT_CODE == "mainland",]
# aus_island <- aus_geodata[aus_geodata$FEAT_CODE == "island",]

## Convert to raster
temp <- rgeos::gUnionCascaded(aus_mainland)
shapefile(x = temp, file = file.path(output_dir, "aus_mainland.shp"))

ausmask <- raster(file.path(output_dir, "ausmask_WGS.tif"))
infile <- file.path(output_dir, "aus_mainland.shp")
outfile <- file.path(output_dir, "aus_mainland.tif")

rasterize_shp(infile, outfile, res = res(ausmask)[1], ext = extent(ausmask)[c(1,2,3,4)])

## Reproject to WGS84
infile <- outfile
outfile <- file.path(output_dir, "aus_mainland_WGS.tif")

source_crs <- crs(raster(infile))
new_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
new_res <- res(ausmask)
gdalUtils::gdalwarp(srcfile = infile, dstfile = outfile, s_srs = source_crs, t_srs = new_crs, tr = new_res, verbose=TRUE)

## OTHER
## Mask using GADM ####
reg_mask <- getData("GADM", country = region, level = 0, path = rdata_path)
reg_mask <- gSimplify(reg_mask, tol = 0.00833)
mask_template <- raster(
  nrow = 4091,
  ncol = 4990,
  crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ",
  ext = extent(c(112.4667, 154.05,-44.04167,-9.95))
)
mask_template[] <- 1
reg_mask <- mask(mask_template, reg_mask)
reg_mask <- aggregate(reg_mask, fact = 10)


## Mask using NOAA data ####
## Layer provided by Skip W
## Includes offshore territoties and Islands
## Needs to be clipped down by extent/boundinng box INCLUDING Australia + territories EXCLUDING Antractica
aus_noaa <- rgdal::readOGR("/Volumes/uom_data/nesp_bugs_data/Topo250kv3AMBIS3islBlnNoaa/Topo250kv3AMBIS3islBlnNoaa.shp")
raster::extent(aus_noaa@bbox)

## EXTRA
# aus.mask  <- sf::st_read("./output/aus_tileedge.shp")
# plot(sf::st_geometry(aus.mask))
# 
# aus.mask <- concaveman(aus.mask) # concave polygon from lines
# aus.mask <- sf::st_zm(aus.mask) # drop M
# plot(sf::st_geometry(aus.mask))
# 
# temp <- sf::st_polygonize(aus.mask)
# temp <- sf::st_cast(aus_tileedge, "POLYGON")
# temp2 <- fasterize::fasterize(temp, raster::raster())