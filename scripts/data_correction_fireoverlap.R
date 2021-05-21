## Fire overlap for corrected ALA-nonALA data


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")
x <- c("data.table", "sp", "raster", "rgdal", "rgeos",
       "quickPlot", "fastshp",
       "alphahull", "ConR", "rnaturalearthdata", 
       "future", "future.apply", "parallel",
       "doMC", "foreach", "rstudioapi")
lapply(x, require, character.only = TRUE)
rm(x)

## File paths and folders
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
corr_dir = file.path(bugs_dir, "data_corrections")
output_dir = file.path(bugs_dir, "outputs")

new_output_dir = file.path(bugs_dir, "outputs", "outputs_for_updated_species_only")
if (!dir.exists(new_output_dir)) {dir.create(new_output_dir)}

spdata_dir = file.path(output_dir, "ala_nonala_data" ,"spdata_updated_species_only")
if (!dir.exists(spdata_dir)) {dir.create(spdata_dir)}

polygons_dir = file.path(output_dir,"species_polygons_updated_species_only")
if (!dir.exists(polygons_dir)) {dir.create(polygons_dir)}
working_dir <- paste0("~/gsdms_r_vol", polygons_dir)



## Species data preparation ####
## ------------------------------------------------------------- ##

## >> Load updated data & subset to updated species only ####
data <- fread(file.path(output_dir, "data_ALAnonALA_wgs84_corrected.csv"))
new_sp <- fread(file.path(corr_dir, "update_species.csv"))

dim(data)
data <- data[spfile %in% unique(new_sp$name_corrections)]
dim(data)
length(unique(data$spfile)) == length(unique(new_sp$name_corrections))

## Update scientificName according to new_sp
for (i in unique(new_sp$name_corrections)){
  data[spfile %in% i]$scientificName = new_sp[spfile == i]$scientificName
}
length(unique(data$spfile))
length(unique(data$scientificName))
rm(i)

## Remove duplicates
message(cat("Number of duplicate records: "),
        sum(duplicated(data[,c("scientificName", 
                              "latitude", 
                              "longitude")])))
data <- setDT(data)[order(-data_source), .SD[1L] ,.(scientificName, latitude, longitude)]

## Year filter
## >> Number of records lost with year filter
t1 <- data[, .N, scientificName]
t2 <- data[!is.na(year) & year >= 1990][, .N, scientificName]
t <- merge(t1, t2, by = "scientificName", all.x = TRUE)
dim(t)
names(t)[2:3] <- c("n.all", "n.sub")
t[which(is.na(n.sub))]$n.sub = 0
sum(is.na(t$n.sub))
setorder(t, n.sub)

## >> Remove records based on filter rule: 
##  if >= 3 records after filter, remove NA and <1990
##  if < 3 records after filter, keep NA and <1990
sp_applyfilter <- t[n.sub >= 3]$scientificName
length(sp_applyfilter)

dim(data)
dat0 <- data[scientificName %in% sp_applyfilter][!is.na(year) & year >= 1990]
dat1 <- data[!(scientificName %in% sp_applyfilter)]

## Checks
sum(dat0[, .N, scientificName]$N < 3)
dim(dat0)[1] + dim(dat1)[1]
length(unique(dat0$scientificName)) + 
  length(unique(dat1$scientificName)) == length(unique(data$scientificName))

data <- rbind(dat0, dat1)
length(unique(data$spfile))
rm(t1, t2, t, dat0, dat1, sp_applyfilter)

## >> Save rds files for updated species ####
## ------------------------------------------------------------- ##

save_spdata2 <- function(species_uid, data, data_dir){
  
  temp <- dat[spfile == species_uid]
  spfile <- unique(temp$spfile)
  
  if (length(spfile) > 1){
    stop("Error: More than 1 unique spfile for naming species file...")
  }
  
  saveRDS(as.data.table(temp),
          file = file.path(data_dir, paste0(spfile, ".rds")))
}


## Run function in parallel
errorlog <- paste0(new_output_dir, "/errorlog_data_ALAnonALA_corrections_", gsub("-", "", Sys.Date()), ".txt")
writeLines(c(""), errorlog)

dat <- data
all_species <- unique(dat$spfile)

plan(multiprocess, workers = future::availableCores()-2)
options(future.globals.maxSize = +Inf)

system.time(invisible(future.apply::future_lapply(all_species,
                                      function(x){
                                        tmp <- tryCatch(expr = save_spdata2(species_uid = x, 
                                                                            data = dat, 
                                                                            data_dir = spdata_dir),
                                                        error = function(e){ 
                                                          print(paste("\nError: More than 1 unique spfile for naming species file for...", x))
                                                          cat(paste(x, "\n"),
                                                              file = errorlog, 
                                                              append = TRUE)
                                                        })
                                      })))

## Check files
length(list.files(spdata_dir, pattern = ".rds$"))
length(all_species)
rm(dat, save_spdata2)


## Create polygons: from species_polygons.R ####
## ------------------------------------------------------------- ##

setwd(working_dir)

spfiles <- list.files(spdata_dir, pattern= ".rds$", full.names = TRUE)
message(cat("Total number of updated species: "),
        length(spfiles))

## >> Run IUCN.eval in parallel ####
source("~/gsdms_r_vol/tempdata/workdir/nesp_bugs/scripts/conr_iucn_eval.R")
basemap_file <- file.path(output_dir, "masks", "auslands_1poly_wgs84.shp")
hull.method <- "alpha.hull" # "convex.hull"  

plan(multiprocess, workers = future::availableCores()-2)
options(future.globals.maxSize = +Inf) 
errorlog <- paste0(new_output_dir, "/errorlog_species_polygons_corrections_", gsub("-", "", Sys.Date()), ".txt")
# if(file.exists(errorlog)){unlink(errorlog)}
writeLines(c(""), errorlog)

system.time(
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

## Check files
csvfiles <- list.files(polygons_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
rdsfiles <- list.files(polygons_dir, pattern = ".rds$", 
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of .csv output files created from IUCN.eval(): "),
        length(csvfiles))
message(cat("Number of .rds output files created from IUCN.eval(): "),
        length(rdsfiles))
message(cat("Total number of updated species: "),
        length(spfiles))

## >> Resolve errors ####
## List species not in output files
errorfiles <- trimws(readLines(errorlog)[-1])
message(cat("Number of species showing errors: "),
        length(errorfiles))

length(csvfiles)+length(errorfiles) == length(spfiles)
all(errorfiles %in% spfiles)

output_sp <- basename(tools::file_path_sans_ext(rdsfiles))
input_sp <- basename(tools::file_path_sans_ext(spfiles))
input_sp[!input_sp %in% output_sp]
message(cat("error species == species not found in rds output files: "),
        all(basename(tools::file_path_sans_ext(errorfiles)) 
            %in% input_sp[!input_sp %in% output_sp]))

mc.cores = length(errorfiles)
set.seed(1, kind = "L'Ecuyer-CMRG")
system.time(invisible(mclapply(errorfiles,
                               conr_iucn_eval,
                               hull.method = hull.method,
                               exclude.by.map = TRUE,
                               basemap_path = basemap_file,
                               working_dir = working_dir,
                               iucn_outpath = polygons_dir,
                               mc.cores = mc.cores)))

## Check files
csvfiles <- list.files(polygons_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
rdsfiles <- list.files(polygons_dir, pattern = ".rds$", 
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of .csv output files created from IUCN.eval(): "),
        length(csvfiles))
message(cat("Number of .rds output files created from IUCN.eval(): "),
        length(rdsfiles))
message(cat("Total number of updated species: "),
        length(spfiles))

output_sp <- basename(tools::file_path_sans_ext(rdsfiles))
input_sp <- basename(tools::file_path_sans_ext(spfiles))
input_sp[!input_sp %in% output_sp]

## Individual runs
setwd(working_dir)
basemap <- readOGR(basemap_file)

dat <- as.data.table(readRDS(grep("idiommata_blackwallii_9041", spfiles, 
                                  value = TRUE)))
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

saveRDS(out, file = paste0(polygons_dir, "/", spname, ".rds"))
rm(dat, spname, out)

## >> Check files ####
csvfiles <- list.files(polygons_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
rdsfiles <- list.files(polygons_dir, pattern = ".rds$", 
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of .csv output files created from IUCN.eval(): "),
        length(csvfiles))
message(cat("Number of .rds output files created from IUCN.eval(): "),
        length(rdsfiles))
message(cat("Total number of updated species: "),
        length(spfiles))
rm(conr_iucn_eval, csvfiles, rdsfiles, spfiles, errorlog, errorfiles, input_sp, output_sp, hull.method, basemap_file, basemap, mc.cores)

## >> AOO & EOO table from csv files ####
csvfiles <- list.files(polygons_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
out <- do.call("rbind", lapply(csvfiles , read.csv))
dim(out)

out$spfile <- basename(tools::file_path_sans_ext(csvfiles))
names(out)[1] <- "scientificName"

setDT(out, key = "spfile")
out <- out[,Nbe_subPop := NULL]

setorder(out, EOO, AOO)
write.csv(out, file = file.path(new_output_dir, "species_EOO_AOO_ahullareas_updatedsp.csv"), 
          row.names = FALSE)

## >> EOO polygons from rds files ####
rdsfiles <- list.files(polygons_dir, pattern = ".rds$", 
                       full.names = TRUE, all.files = TRUE)
polynames <- basename(tools::file_path_sans_ext(rdsfiles))
temp <- lapply(rdsfiles, readRDS)
names(temp) <- polynames
length(temp)
saveRDS(temp, file = file.path(new_output_dir, "species_ahull_outputs_updatedsp.rds"))

## >> List of SPDF from .rds files ####
temp2 <- lapply(temp, "[[", 1)
temp2 <- lapply(temp2, "[[", 2)
length(temp2)
saveRDS(temp2, file = file.path(new_output_dir, "species_ahullspdf_updatedsp.rds"))

## Create list of non-NULL SPDF for species with EOOs
na.eooIDX <- sapply(temp2, length)
na.eooIDX <- which(na.eooIDX == 0)
temp3 <- temp2[-na.eooIDX]
length(temp3)
saveRDS(temp3, file = file.path(new_output_dir, "species_ahullEOOspdf_updatedsp.rds"))

## Create list of species names without EOOs (including error species)
length(sort(names(na.eooIDX)))
write.csv(sort(names(na.eooIDX)), 
          file = file.path(new_output_dir, "species_ahullnoEOO_updatedsp.csv"), 
          row.names = FALSE)

## Check
message(cat("Total number of species: "),
        length(names(na.eooIDX)) + length(temp3))
rm(csvfiles, rdsfiles, out, na.eooIDX, polynames, temp, temp2, temp3)


## State/region overlap ####
## ------------------------------------------------------------- ##

source("/tempdata/workdir/nesp_bugs/scripts/region_overlap.R")
spfiles <- list.files(spdata_dir, pattern= ".rds$", full.names = TRUE)
length(spfiles)

## I. Bushfire recovery regions overlap ####
## >> Run overlap analysis in parallel: doMC ####
region <- raster(file.path(output_dir, "regions","bushfire_recovery_p.tif"))
region_vals <- region[]
region_classes <- sort(unique(na.omit(region_vals)))
region_classes

overlap_dir = file.path(new_output_dir, "regions_overlap")
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

wgs_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
eqarea_crs <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

registerDoMC(future::availableCores()-2)
system.time(log <- foreach(species_dat = spfiles, 
                           .combine = rbind,
                           .errorhandling = "pass",
                           .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                             
                             region_overlap(data_rds = species_dat, 
                                            crs_org = wgs_crs, 
                                            crs_new = eqarea_crs, 
                                            region_raster = region,
                                            region_classes = region_classes,
                                            outdir = overlap_dir)
                           })



## >> Error checking ####
log
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of input species: "),
        length(spfiles))
message(cat("Number of output files: "),
        length(csvfiles))


## >> Output table ####
out <- do.call("rbind", lapply(csvfiles, fread))
message(cat("Check for NAs: "),
        sum(is.na(out)))
reg_names <- fread(file.path(output_dir, "bushfire_recregions_names.csv"))
names(out) <- c("spfile", "NA", gsub(" ", "_", tolower(reg_names$name)))

setDT(out, key = "spfile")
write.csv(out, file = file.path(new_output_dir, "species_by_bushfireregions_updatedsp.csv"), row.names = FALSE)
rm(region, region_vals, region_classes, overlap_dir, wgs_crs, eqarea_crs, log, csvfiles, out, reg_names)

# ## >> Remove files ####
# file.remove(file.path(overlap_dir, dir(path = overlap_dir)))
# unlink(overlap_dir, recursive = TRUE)


## II. States overlap ####
## >> Run overlap analysis in parallel: doMC ####
region <- raster(file.path(output_dir, "regions", "auslands_wgs84_p.tif"))
region_vals <- region[]
region_classes <- sort(unique(na.omit(region_vals)))
region_classes

overlap_dir = file.path(new_output_dir, "states_overlap")
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

wgs_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
eqarea_crs <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

registerDoMC(future::availableCores()-2)
system.time(log <- foreach(species_dat = spfiles, 
                           .combine = rbind,
                           .errorhandling = "pass",
                           .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                             
                             region_overlap(data_rds = species_dat, 
                                            crs_org = wgs_crs, 
                                            crs_new = eqarea_crs, 
                                            region_raster = region,
                                            region_classes = region_classes,
                                            outdir = overlap_dir)
                           })



## >> Error checking ####
log
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of input species: "),
        length(spfiles))
message(cat("Number of output files: "),
        length(csvfiles))


## >> Output table ####
out <- do.call("rbind", lapply(csvfiles, fread))
message(cat("Check for NAs: "),
        sum(is.na(out)))
reg_names <- fread(file.path(output_dir, "state_names.csv"))
reg_names$name[1] <- "NA"
names(out) <- c("spfile", reg_names$name)

setDT(out, key = "spfile")
write.csv(out, file = file.path(new_output_dir, "species_by_states_updatedsp.csv"), row.names = FALSE)
rm(region, region_vals, region_classes, overlap_dir, wgs_crs, eqarea_crs, log, csvfiles, out, reg_names, region_overlap, spfiles)

# ## >> Remove files ####
# file.remove(file.path(overlap_dir, dir(path = overlap_dir)))
# unlink(overlap_dir, recursive = TRUE)


## Fire overlap ####
## ------------------------------------------------------------- ##

spfiles <- list.files(spdata_dir, pattern= ".rds$", full.names = TRUE)
length(spfiles)

fire_severity <- raster(file.path(output_dir, "fire", "severity5_eqar250_native.tif"))
fire_vals <- fire_severity[]
fire_classes <- sort(unique(na.omit(fire_vals)))

## I. Points overlap ####
overlap_dir = file.path(new_output_dir, "points_overlap")
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}
source("/tempdata/workdir/nesp_bugs/scripts/points_overlap.R")

wgs_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
eqarea_crs <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

## >> Run overlap analysis in parallel: doMC ####
registerDoMC(future::availableCores())
system.time(log <- foreach(species_dat = spfiles, 
                           .combine = rbind,
                           .errorhandling = "pass",
                           .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                             
                             points_overlap(data_rds = species_dat, 
                                            crs_org = wgs_crs, 
                                            crs_new = eqarea_crs, 
                                            fire_severity = fire_severity,
                                            fire_classes = fire_classes,
                                            outdir = overlap_dir)
                           })




## >> Error checking ####
log
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of input species: "),
        length(spfiles))
message(cat("Number of output files: "),
        length(csvfiles))


## >> Output table ####
out <- do.call("rbind", lapply(csvfiles, fread))
names(out)[1] <- "spfile"
dim(out)
names(out)[grep("Fire_Class_", names(out))] <- paste0(names(out)[grep("Fire_Class_", names(out))], "_Points")

## >> Add percentage overlap columns
out$Overlap_Points_Fire345_GEEBAM2_as_unburnt <- 
  ((out$Fire_Class_3 + out$Fire_Class_4 + out$Fire_Class_5)/
     (out$Occurrence_Points - out$Fire_Class_1)) * 100

out$Overlap_Points_Fire2345_GEEBAM2_as_burnt <- 
  ((out$Fire_Class_2 + out$Fire_Class_3 + out$Fire_Class_4 + out$Fire_Class_5)/
     (out$Occurrence_Points - out$Fire_Class_1)) * 100

out$Overlap_Points_Severe_Fire45 <- 
  ((out$Fire_Class_4 + out$Fire_Class_5)/
     (out$Occurrence_Points - out$Fire_Class_1)) * 100

sum(is.na(out$Overlap_Points_Fire345_GEEBAM2_as_unburnt))
sum(is.na(out$Overlap_Points_Fire2345_GEEBAM2_as_burnt))
sum(is.na(out$Overlap_Points_Severe_Fire45))

## >> Save table
names(out)[c(1:6, 8:10, 7)]
out <- out[, c(1:6, 8:10, 7)]
names(out)

setDT(out, key = "spfile")
write.csv(out, file = file.path(new_output_dir, "species_points_fireoverlap_updatedsp.csv"), 
          row.names = FALSE)
rm(points_overlap, overlap_dir, log, csvfiles, out, wgs_crs, eqarea_crs)


## II. Polygons overlap ####
## >> Run overlap analysis in parallel: doMC ####
job_script <- file.path("/tempdata/workdir/nesp_bugs/", "scripts", "species_EOO_fireoverlap_updatedsp_job.R")
jobRunScript(job_script, encoding = "unknown", workingDir = "/tempdata/workdir/nesp_bugs",
             importEnv = FALSE, exportEnv = "")

## >> Error checking ####
shapefile_dir = file.path(new_output_dir, "species_shapefiles")
overlap_dir = file.path(new_output_dir, "polygon_overlap")
  # # ## Remove exisitng overlap and shapefiles folder
  # unlink(shapefile_dir, recursive = TRUE, force = TRUE)
  # file.remove(file.path(overlap_dir, dir(path = overlap_dir)))
  # unlink(overlap_dir, recursive = TRUE)

species_maps <- readRDS(file.path(new_output_dir, "species_ahullEOOspdf_updatedsp.rds"))
polygon_list <- names(species_maps)
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of output files: "),
        length(csvfiles))
message(cat("Number of input species: "),
        length(polygon_list))

## >> Error runs ####
## Repeat till most of the errors are fixed
if (length(polygon_list) - length(csvfiles) > 0){
  jobRunScript(job_script, encoding = "unknown", workingDir = "/tempdata/workdir/nesp_bugs",
               importEnv = FALSE, exportEnv = "")
}

## >> Output table ####
out <- do.call("rbind", lapply(csvfiles, fread))
names(out)[1] <- "spfile"
dim(out)
names(out)[grep("Fire_Class_", names(out))] <- paste0(names(out)[grep("Fire_Class_", names(out))], "_Area")

## >> Add percentage overlap columns
out$Overlap_Polygons_Fire345_GEEBAM2_as_unburnt <- 
  ((out$Fire_Class_3 + out$Fire_Class_4 + out$Fire_Class_5)/
     (out$Species_Polygon - out$Fire_Class_1)) * 100

out$Overlap_Polygons_Fire2345_GEEBAM2_as_burnt <- 
  ((out$Fire_Class_2 + out$Fire_Class_3 + out$Fire_Class_4 + out$Fire_Class_5)/
     (out$Species_Polygon - out$Fire_Class_1)) * 100

out$Overlap_Polygons_Severe_Fire45 <- 
  ((out$Fire_Class_4 + out$Fire_Class_5)/
     (out$Species_Polygon - out$Fire_Class_1)) * 100

sum(is.na(out$Overlap_Polygons_Fire345_GEEBAM2_as_unburnt))
sum(is.na(out$Overlap_Polygons_Fire2345_GEEBAM2_as_burnt))
sum(is.na(out$Overlap_Polygons_Severe_Fire45))

## >> Save table
names(out)[c(1:6, 8:10, 7)]
out <- out[, c(1:6, 8:10, 7)]
names(out)

setDT(out, key = "spfile")
write.csv(out, file = file.path(new_output_dir, "species_polygon_fireoverlap_updatedsp.csv"), row.names = FALSE)
rm(overlap_dir, species_maps, polygon_list, job_script, csvfiles, out, fire_classes, fire_vals, fire_severity, spfiles)



## Combine outputs for updated species ####
## ------------------------------------------------------------- ##

poly <- fread(file.path(new_output_dir, "species_polygon_fireoverlap_updatedsp.csv"))
names(poly); dim(poly)
point <- fread(file.path(new_output_dir, "species_points_fireoverlap_updatedsp.csv"))
names(point); dim(point)

## Merge rows for species with polygon overlap info
out1 <- merge(point, poly, by = "spfile")
dim(out1)

## Get rows for species without polygon information
sum(!(point$spfile %in% out1$spfile))
out2 <- point[!(point$spfile %in% out1$spfile)]
dim(out2)

## Add empty polygon information columns to out2 
dt <- setNames(data.table(matrix(nrow = nrow(out2), ncol = length(names(poly)[-1]))), names(poly)[-1])
out2 <- cbind(out2, dt)
dim(out2)
rm(dt)

## Checks
dim(out1)[1] + dim(out2)[1] == dim(point)[1]
ncol(out1) == ncol(out2)

## Combine both tables
out <- rbind(out1, out2)
names(out); dim(out)
rm(out1, out2, point, poly)

## Checks on merged table
message(cat("Species without polygons: "),
        sum(is.na(out$Species_Polygon)))
message(cat("Species with polygons: "),
        sum(!is.na(out$Species_Polygon)))

## Add taxonomic information
x <- data[!duplicated(data[,.(spfile, scientificName, class, order, family)])][, .(spfile, scientificName, class, order, family)]
nrow(x) == nrow(out)
all(sort(x$spfile) == sort(out$spfile))

out <- merge(out, x, by = "spfile")
names(out); dim(out)
rm(x)

## Add EOO/AOO information
x <- fread(file.path(new_output_dir, "species_EOO_AOO_ahullareas_updatedsp.csv"))
nrow(x) == nrow(out)
all(sort(x$spfile) == sort(out$spfile))

out1 <- merge(out, x, by = "spfile")
all(out1$scientificName.x == out1$scientificName.y)
rm(out1)
x[, scientificName := NULL]

out <- merge(out, x, by = "spfile")
names(out); dim(out)

all(out$Nbe_unique_occ. == out$Occurrence_Points)
out[, Nbe_unique_occ. := NULL]
out[, c("Nbe_loc", "Category_CriteriaB", "Category_code", 
        "Category_AOO", "Category_EOO") := NULL]
names(out); dim(out)
rm(x)

## Add region information
region1 <- fread(file.path(new_output_dir, "species_by_states_updatedsp.csv"))
names(region1)[-1] <- paste0("state_", names(region1)[-1])
region2 <- fread(file.path(new_output_dir, "species_by_bushfireregions_updatedsp.csv"))
names(region2)[-1] <- paste0("fire.rec.reg_", names(region2)[-1])

dim(region1); length(unique(region1$spfile))
dim(region2); length(unique(region2$spfile))

setDT(region1, key = "spfile"); setDT(region2, key = "spfile")
region <- merge(region1, region2, by = "spfile")
setDT(region, key = "spfile"); dim(region)

region <- region[spfile %in% out$spfile]
dim(out); length(unique(out$spfile)); length(unique(out$scientificName))
dim(region); length(unique(region$spfile))
out <- merge(out, region, by = "spfile")

setDT(out, key = "spfile"); names(out); dim(out)
rm(region1, region2, region)

## Reorder columns
names(out)[c(1, 20:23, 2:19, 24:44)]
out <- out[, c(1, 20:23, 2:19, 24:44)]
setDT(out, key = "spfile"); dim(out)
write.csv(out, file = file.path(new_output_dir, "invert_fireoverlap_updatedsp.csv"), 
          row.names = FALSE)
