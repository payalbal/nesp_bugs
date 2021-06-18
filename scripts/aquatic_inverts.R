## Aquatic innvertebrates


## Set working environment ####
rm(list = ls())
gc()

x <- c("data.table", "sp", "raster", "rgdal", 
       "gdalUtils", "rgeos", "doMC", "foreach")
lapply(x, require, character.only = TRUE)
rm(x)

bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
aqua_dir = file.path(bugs_dir, "aquatics")
output_dir = file.path(bugs_dir, "outputs")


## Subset data ####
  # aqualist <- fread(file.path(aqua_dir, "aquatic_subset_JM.csv"))
  # aqualist <- tolower(gsub(" ", "_", aqualist$Species))
aqualist <- fread(file.path(aqua_dir, "aquatic.csv"))
aqualist[duplicated(aqualist$spfile)]$spfile

aqualist[grep("triaenodes_theiophora_57141", aqualist$spfile)]
aqualist[grep("triaenodes_virgula_57147", aqualist$spfile)]

aqualist <- aqualist$spfile; length(aqualist)
aqualist <- aqualist[!duplicated(aqualist)]; length(aqualist)

data <- fread(file.path(output_dir, "data_ALAnonALA_wgs84_corrected.csv"))

sum(aqualist %in% data$spfile)
aqualist[!aqualist %in% data$spfile]

deleted_sp <- fread(file.path(bugs_dir, "data_corrections", "delete_species_fromdata.csv"))
aqualist[!aqualist %in% data$spfile] %in% deleted_sp$x
  ## species not found were deleted from data

aqua_data <- data[spfile %in% aqualist]
length(unique(aqua_data$spfile))


## >> Check data
mask_file <- file.path(output_dir, "masks", "ausmask_noaa_1kmWGS_NA.tif")
ausmask <- raster::raster(mask_file)
wgs_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

points <- sp::SpatialPoints(aqua_data[,.(longitude, latitude)], proj4string = CRS(wgs_crs))
plot(ausmask, axes = FALSE, legend = FALSE, box = FALSE)
plot(points, add = TRUE, pch = 18, col = "tomato3", cex = 0.5)

aqua_data[, .N, by = "scientificName"]


# ## Slugrisk layer processing - to be done once ####
# slug <- file.path(aqua_dir, "ward_slugrisk", "stream_Rusle_Studysite_3.shp")
# slug <- rgdal::readOGR(slug)
# 
# slug
# slug@proj4string
# names(slug@data)
# slug <-  slug["RUSLERF_LH"]
# 
# writeOGR(slug, dsn = file.path(output_dir, "slugrisk"), layer = "slugrisk_RUSLERF_LH", driver = "ESRI Shapefile", overwrite_layer = TRUE)
# 
# ## Rasterise slugrisk
# ## >> Rasterise, retain values in layer RUSLERF_LH
# infile <- file.path(output_dir, "slugrisk" ,"slugrisk_RUSLERF_LH.shp")
# outfile <- gsub("_RUSLERF_LH.shp$", ".tif", infile)
# system(paste0("gdal_rasterize -at -a RUSLERF_LH -ot Byte -tr .0025 .0025 -l slugrisk_RUSLERF_LH ",
#               infile, " ", outfile))
# sort(unique(na.omit(raster(outfile)[])))
# 
# ## >> Reproject to Albers Equal Area
# fire_severity <- raster(file.path(output_dir, "fire", "severity5_eqar250_native_paa.tif"))
# fire_res <- res(fire_severity)
# fire_crs <- as.character(crs(fire_severity))
# fire_extent <- extent(fire_severity)
# 
# infile <- outfile
# outfile <- gsub(".tif", "_p.tif", outfile)
# 
# system(paste0("gdalwarp -overwrite -ot Byte -tr ", 
#               paste(fire_res, collapse = " "), " -te ", 
#               paste(fire_extent[1], fire_extent[3], 
#                     fire_extent[2], fire_extent[4]), 
#               " -s_srs 'EPSG:4326' -t_srs '", fire_crs, "' ",
#               infile, " ", outfile))
# 
# gdalUtils::gdalinfo(outfile)
# 
# ## Assign NAs to no data
# infile <- outfile
# outfile <- gsub("_p.tif", "_NA.tif", infile)
# system(paste0("gdal_calc.py -A ", infile,
#               " --calc='(A==1)*1 + (A==2)*2 + (A==0)*0' --NoDataValue=0",
#               " --outfile=", outfile))
# sort(unique(na.omit(raster(outfile)[])))


## Point overlaps ####
## >> Link files and folders ####
overlap_dir = file.path(output_dir, "slurisk", "slug_points_overlap")
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

source("/tempdata/workdir/nesp_bugs/scripts/points_slugfire_overlap.R")

spfiles <- list.files(file.path(output_dir, "ala_nonala_data" ,
                                "spdata"), pattern= ".rds$", full.names = TRUE)
spnames <- tools::file_path_sans_ext(basename(spfiles))
spfiles <- spfiles[spnames %in% aqualist]

## >> Load in fire severity raster
fire_severity <- raster(file.path(output_dir, "fire", "severity5_eqar250_native_paa.tif"))
fire_classes <- sort(unique(na.omit(fire_severity[])))

## >> Slugrisk raster
slug_severity <- raster(file.path(output_dir, "slugrisk", "slugrisk_NA.tif"))
slug_classes <- sort(unique(na.omit(slug_severity[])))

## Function parameters
wgs_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
eqarea_crs <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

## >> Run overlap analysis in parallel: doMC ####
registerDoMC(length(spfiles))
system.time(log <- foreach(species_dat = spfiles, 
                           .combine = rbind,
                           .errorhandling = "pass",
                           .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                             
                             points_slugfire_overlap(data_rds = species_dat, 
                                            crs_org = wgs_crs, 
                                            crs_new = eqarea_crs, 
                                            fire_severity = fire_severity,
                                            fire_classes = fire_classes,
                                            slug_severity = slug_severity,
                                            slug_classes = slug_classes,
                                            outdir = overlap_dir)
                           })

log
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of input species: "),
        length(spfiles))
message(cat("Number of output files: "),
        length(csvfiles))

## >> Output table ####
point <- do.call("rbind", lapply(csvfiles, fread)); dim(point)
names(point)[1] <- "spfile"
setDT(point, key = "spfile")
names(point)[-c(1, ncol(point))] <- paste0(names(point)[-c(1, ncol(point))], "_Points")
write.csv(point, file = file.path(output_dir,  "slugrisk", "slug_point_overlap.csv"), 
          row.names = FALSE)


## Polygon overlaps ####
# job_script <- file.path("/tempdata/workdir/nesp_bugs/", "scripts", "aquatic_inverts_polygon_job.R")
# rstudioapi::jobRunScript(job_script, encoding = "unknown", workingDir = "/tempdata/workdir/nesp_bugs",
#                          importEnv = FALSE, exportEnv = "")

shapefile_dir = file.path(output_dir,  "slugrisk", "species_shapefiles")
if(!dir.exists(shapefile_dir)){dir.create(shapefile_dir)}

overlap_dir = file.path(output_dir, "slugrisk", "slug_polygon_overlap")
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

source("/tempdata/workdir/nesp_bugs/scripts/polygon_slugfire_overlap.R")

## >> Load in spdf data for species with EOO ####
species_maps <- readRDS(file.path(output_dir, "species_ahullEOOspdf.rds"))
polygon_list <- names(species_maps)
polygon_list <- polygon_list[polygon_list %in% aqualist]


## >> Load in fire severity raster
fire_severity <- raster(file.path(output_dir, "fire", "severity5_eqar250_native_paa.tif"))
fire_vals <- fire_severity[]
fire_classes <- sort(unique(na.omit(fire_vals)))
fire_res <- res(fire_severity)
fire_crs <- as.character(crs(fire_severity))
fire_extent <- extent(fire_severity)

## >> Slugrisk raster
slug_severity <- raster(file.path(output_dir, "slugrisk", "slugrisk_NA.tif"))
slug_vals <- slug_severity[]
slug_classes <- sort(unique(na.omit(slug_vals)))

## >> Run overlap analysis in parallel: doMC ####
## 'log' only useful when running small number of species
registerDoMC(length(polygon_list))
system.time(log <- foreach(polys = polygon_list,
                    .combine = rbind,
                    .errorhandling = "pass",
                    .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                      
                      polygon_slugfire_overlap(species_name = polys,
                                      species_poly = species_maps[[polys]],
                                      shapefile_dir = shapefile_dir,
                                      fire_res = fire_res, 
                                      fire_crs = fire_crs, 
                                      fire_extent = fire_extent, 
                                      fire_vals = fire_vals,
                                      fire_classes = fire_classes,
                                      slug_vals = slug_vals,
                                      slug_classes = slug_classes,
                                      outdir = overlap_dir)
                    })


log
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of input species: "),
        length(polygon_list))
message(cat("Number of output files: "),
        length(csvfiles))

## >> Output table ####
poly <- do.call("rbind", lapply(csvfiles, fread)); dim(poly)
names(poly)[1] <- "spfile"
setDT(poly, key = "spfile")
names(poly)[-c(1, ncol(poly))] <- paste0(names(poly)[-c(1, ncol(poly))], "_Area")
write.csv(poly, file = file.path(output_dir,  "slugrisk", "slug_polygon_overlap.csv"), 
          row.names = FALSE)


## Combine polygons and point output tables ####
## Merge rows for species with polygon overlap
out1 <- merge(point, poly, by = "spfile")
dim(out1)

## Get rows for species without polygon informatiom (n = 29948)
sum(!(point$spfile %in% out1$spfile))
out2 <- point[!(point$spfile %in% out1$spfile)]
dim(out2)

## Add empty polygon information columns to out2 
dt <- setNames(data.table(matrix(nrow = nrow(out2), ncol = length(names(poly)[-1]))), names(poly)[-1])
out2 <- cbind(out2, dt)
dim(out2)
rm(dt)

## Check
dim(out1)[1] + dim(out2)[1] == dim(point)[1]
ncol(out1) == ncol(out2)
all(names(out1) == names(out2))

## Combine both tables
out <- rbind(out1, out2); dim(out)
rm(out1, out2, point, poly)

## Checks on merged table
sum(is.na(out$Species_Polygon))
sum(!is.na(out$Species_Polygon))

## Add confidence columns
aqualist <- fread(file.path(aqua_dir, "aquatic.csv"))
aqualist <- aqualist[!duplicated(aqualist)]; dim(aqualist)
out1 <- merge(out, aqualist, by = "spfile"); dim(out1)

## Save table
out1 <- out1[, c(1, 38:43, 2:37)]
setDT(out1, key = "spfile")
write.csv(out, file = file.path(output_dir, "slugrisk", paste0("aquatic_slug_fire_overlap_", 
                                                               Sys.Date(), ".csv")), 
          row.names = FALSE)
