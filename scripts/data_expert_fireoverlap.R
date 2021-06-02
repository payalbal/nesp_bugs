## IUCN priority species subset


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")
x <- c("data.table", "stringr", "rstudioapi",
       "sp", "raster", "rgdal", "gdalUtils", "rgeos", 
       "alphahull", "ConR", "rnaturalearthdata", 
       "doMC", "foreach",
       "future", "future.apply", "parallel")
lapply(x, require, character.only = TRUE)
rm(x)

## File paths and folders
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
iucn_dir = "~/gsdms_r_vol/tempdata/research-cifs/uom_data/iucn_bugs_data"
output_dir = file.path(bugs_dir, "outputs")

new_output_dir = file.path(bugs_dir, "outputs", "outputs_for_expert_data_only")
if (!dir.exists(new_output_dir)) {dir.create(new_output_dir)}

spdata_dir = file.path(output_dir, "ala_nonala_data" ,"spdata_expert_data_only")
if (!dir.exists(spdata_dir)) {dir.create(spdata_dir)}

polygons_dir = file.path(output_dir,"species_polygons_expert_data_only")
if (!dir.exists(polygons_dir)) {dir.create(polygons_dir)}
working_dir <- paste0("~/gsdms_r_vol", polygons_dir)

# ## >> Remove files ####
# file.remove(file.path(new_output_dir, dir(path = new_output_dir)))
# unlink(new_output_dir, recursive = TRUE)


## Prepare data #### 
## >> Load ALAnonALA data ####
data_all <- data <- fread(file.path(output_dir, "data_ALAnonALA_wgs84_corrected.csv"))
data$spfile2 <- gsub("_\\d+$", "", data$spfile)

## >> Add expert elicited data for (some) species ####
data_expert <- fread(file.path(iucn_dir, "nonALA_data", "data_expert.csv"))
data_expert2 <- fread(file.path(iucn_dir, "nonALA_data", "Extra_species_data_28May2021.csv"))
all(names(data_expert) == names(data_expert2))
# ## Find family info for data_expert2
# data_expert[grep("Moggridgea rainbowi", data_expert$tax)]$family

data_expert <- rbind(data_expert, data_expert2); dim(data_expert)
setDT(data_expert, key = "tax")
rm(data_expert2)

## Modify name for species...
data_expert[tax == "Xanthesma argohesma nukarnensis"]
data[scientificName == "Xanthesma (Argohesma) nukarnensis"]
data_expert[tax == "Xanthesma argohesma nukarnensis"]$tax = "Xanthesma (Argohesma) nukarnensis"

## Add spfile column
data_expert$spfile <- str_replace_all(data_expert$tax, " ", "00xx00")
data_expert$spfile <- str_replace_all(data_expert$spfile, "[^[:alnum:]]", "")
data_expert$spfile <- tolower(gsub("00xx00", "_", data_expert$spfile))

## Format data_expert
data_expert$class <- rep(character(), nrow(data_expert))
data_expert$order <- rep(character(), nrow(data_expert))
names(data_expert)[c(3,1,2,8,9,4,5,6,7)]
data_expert <- data_expert[,c(3,1,2,8,9,4,5,6,7)]

## Checks
# x <- as.numeric(data_expert$latitude)
# which(is.na(x))
length(unique(data_expert$tax))
length(unique(data_expert$spfile))
str(data_expert)

## >> Subset ALAnonALA data ####
## Look for Psacadonotus insulanus: not found
grep("Psacadonotus insulanus", data$scientificName)
grep("psacadonotus_insulanus", data$spfile2)

## Remove (some) species with incorrect data from ALA nonALA data based on expert input
remove_data <- c("nunciella_kangarooensis", 
                 "ogyris_halmaturia", 
                 "kosciuscola_cognatus", 
                 "kosciuscola_cuneatus", 
                 "kosciuscola_tristis", 
                 "kosciuscola_usitatus")
dim(data[data$spfile2 %in% remove_data])
dim(data); data <- data[!data$spfile2 %in% remove_data]; dim(data)
rm(remove_data)

## Subset data 
data <- data[ , .(scientificName, latitude, longitude, class, order, family, year, data_source, spfile, spfile2)]
data <- data[spfile2 %in% unique(data_expert$spfile)]; dim(data)
length(unique(data$spfile))
str(data)


## >> Combine ALAnonALAL and expert datasets ####
data[, spfile2 := NULL]
names(data_expert) = names(data)

data <- rbind(data, data_expert)
setDT(data, key = "spfile")

dim(data); unique(data$data_source)
length(unique(data$scientificName))
length(unique(data$scientificName))
rm(data_expert)

## Correct and/or remove duplicates
## Find duplicated spfile
length(unique(data$spfile)); length(unique(data$scientificName))

for (i in unique(data$scientificName)){
  if(length(unique(data[scientificName == i]$spfile)) > 1){
    print (i)
  }
}
# [1] "Metaballus mesopterus"
# [1] "Moggridgea rainbowi"
# [1] "Nanodectes platycercus"
# [1] "Xanthesma (Argohesma) nukarnensis"

x <- "Metaballus mesopterus"
unique(data[scientificName == x]$spfile)
data[scientificName == x]$spfile <-"metaballus_mesopterus_43213"
data[spfile == "metaballus_mesopterus_43213"]
data[spfile == "metaballus_mesopterus_43213"]$class <- "insecta"
data[spfile == "metaballus_mesopterus_43213"]$family <- "Tettigoniidae"

x <- "Moggridgea rainbowi"
unique(data[scientificName == x]$spfile)
data[scientificName == x]$spfile <-"moggridgea_rainbowi_10318"
data[spfile == "moggridgea_rainbowi_10318"]
data[spfile == "moggridgea_rainbowi_10318"]$class <- "arachnida"
data[spfile == "moggridgea_rainbowi_10318"]$family <- "Migidae"

x <- "Nanodectes platycercus"
unique(data[scientificName == x]$spfile)
data[scientificName == x]$spfile <- "nanodectes_platycercus_44624"
data[spfile == "nanodectes_platycercus_44624"]
data[spfile == "nanodectes_platycercus_44624"]$class <- "insecta"
data[spfile == "nanodectes_platycercus_44624"]$family <- "Tettigoniidae"

x <- "Xanthesma (Argohesma) nukarnensis"
unique(data[scientificName == x]$spfile)
data[scientificName == x]$spfile <- "xanthesma_argohesma_nukarnensis_58525"
data[spfile == "xanthesma_argohesma_nukarnensis_58525"]
data[spfile == "xanthesma_argohesma_nukarnensis_58525"]$class <- "insecta"
data[spfile == "xanthesma_argohesma_nukarnensis_58525"]$family <- "Colletidae"

rm(i,x)

## Find duplicates and prioritise ALA records for removal
dim(data)
dim(data[!duplicated(data[, .(spfile, scientificName, class, order, family, latitude, longitude)])])
data <- setDT(data)[order(-data_source), .SD[1L] ,.(spfile, scientificName, class, order, family, latitude, longitude)]; dim(data)

## Checks for species where data is added (by species..)
expert_sp <- unique(data$scientificName)
temp <- matrix(NA, length(expert_sp), 3)
for (i in 1:length(expert_sp)) {
  temp[i,] <- c(expert_sp[i], 
                nrow(data_all[scientificName == expert_sp[i]]), 
                nrow(data[scientificName == expert_sp[i]]))
}
colnames(temp) <- c("species", "ALAnonALA data", "ALAnonALA+expert data")
temp
rm(expert_sp, temp, i)

## Save ALAnonnALA + expert data for IUCN species
write.csv(data, file = file.path(output_dir, "sp_with_expert_data.csv"), 
          row.names = FALSE)

## Summarize data by species and data source
data_sources <- fread(file.path(output_dir, "data_sources.csv"))[,1:2]
unique(data$data_source)
unique(data_sources$type)

out <- data[, .N, by=.(scientificName, data_source)]
length(unique(out$scientificName))

out$data_type <- rep(character(), nrow(out))
out[data_source %in% data_sources[type == "state"]$data_source]$data_type = "state"
out[data_source %in% data_sources[type == "museum"]$data_source]$data_type = "museum"
out[data_source %in% data_sources[type == "ala"]$data_source]$data_type = "ala"
out[data_source %in% data_sources[type == "private"]$data_source]$data_type = "private"

out <- out[, .(N=sum(N)),
           by=.(scientificName, data_type)]
out <- dcast(out, scientificName ~ data_type, value.var = "N")
# out <- out[,c(1,2,5,3,4)]
setDT(out, key = "scientificName")
setorder(out, -private, na.last = TRUE)
out[is.na(out)] <- 0
out
  # ## Function to replace NA in large data tables
  # ## Ref: https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
  # f_dowle2 = function(DT) {
  #   for (i in names(DT))
  #     DT[is.na(get(i)), (i):=0]
  # }

write.csv(out, file = file.path(new_output_dir, "datatype_counts_sp_with_expert_data.csv"), row.names = FALSE)
rm(out, data_sources)


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
errorlog <- paste0(new_output_dir, "/errorlog_expert_data_", gsub("-", "", Sys.Date()), ".txt")
# if(file.exists(errorlog)){unlink(errorlog)}
writeLines(c(""), errorlog)

dat <- data
all_species <- unique(dat$spfile)

plan(multiprocess, workers = length(all_species))
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
rm(data, save_spdata2, all_species)


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

plan(multiprocess, workers = length(spfiles))
errorlog <- paste0(new_output_dir, "/errorlog_species_polygons_expert_data_", gsub("-", "", Sys.Date()), ".txt")
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

dat <- as.data.table(readRDS(grep("aenigmatinea_glatzella", spfiles, 
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

registerDoMC(length(spfiles))
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
out <- do.call("rbind", lapply(csvfiles, fread)); dim(out)
message(cat("Check for NAs: "),
        sum(is.na(out)))
reg_names <- fread(file.path(output_dir, "bushfire_recregions_names.csv"))
names(out) <- c("spfile", "NA", gsub(" ", "_", tolower(reg_names$name)))

setDT(out, key = "spfile")
write.csv(out, file = file.path(new_output_dir, "species_by_bushfireregions_updatedsp.csv"), row.names = FALSE)
rm(region, region_vals, region_classes, overlap_dir, wgs_crs, eqarea_crs, log, csvfiles, out, reg_names)


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

registerDoMC(length(spfiles))
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
out <- do.call("rbind", lapply(csvfiles, fread)); dim(out)
message(cat("Check for NAs: "),
        sum(is.na(out)))
reg_names <- fread(file.path(output_dir, "state_names.csv"))
reg_names$name[1] <- "NA"
names(out) <- c("spfile", reg_names$name)

setDT(out, key = "spfile")
write.csv(out, file = file.path(new_output_dir, "species_by_states_updatedsp.csv"), row.names = FALSE)
rm(region, region_vals, region_classes, overlap_dir, wgs_crs, eqarea_crs, log, csvfiles, out, reg_names, region_overlap, spfiles)




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
registerDoMC(length(spfiles))
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
out <- do.call("rbind", lapply(csvfiles, fread)); dim(out)
names(out)[1] <- "spfile"
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
shapefile_dir = file.path(new_output_dir, "species_shapefiles")
overlap_dir = file.path(new_output_dir, "polygon_overlap")
if(!dir.exists(shapefile_dir)){dir.create(shapefile_dir)}
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}
source("~/gsdms_r_vol/tempdata/workdir/nesp_bugs/scripts/polygon_overlap.R")

## >> Load in spdf data for species with EOO ####
species_maps <- readRDS(file.path(new_output_dir, "species_ahullEOOspdf_updatedsp.rds"))
polygon_list <- names(species_maps)

## >> Find missing species from outputs (if any) ####
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
csvnames <- tools::file_path_sans_ext(basename(csvfiles))
species_list <- polygon_list[!polygon_list %in% csvnames]

## >> Run overlap analysis in parallel: doMC ####
## 'log' only useful when running small number of species
registerDoMC(length(species_list))
system.time(foreach(polys = species_list,
                    .combine = rbind,
                    .errorhandling = "pass",
                    .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                      
                      polygon_overlap(species_name = polys,
                                      species_poly = species_maps[[polys]],
                                      shapefile_dir = shapefile_dir,
                                      fire_vals = fire_vals,
                                      fire_classes = fire_classes,
                                      outdir = overlap_dir)
                    })

## >> Error checking ####
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of output files: "),
        length(csvfiles))
message(cat("Number of input species: "),
        length(polygon_list))

## >> Error runs ####
## Repeat above steps till most of the errors are fixed

## >> Output table ####
out <- do.call("rbind", lapply(csvfiles, fread)); dim(out)
names(out)[1] <- "spfile"
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
rm(polygon_overlap, overlap_dir, species_maps, polygon_list, job_script, csvfiles, csvnames, out, fire_classes, fire_vals, fire_severity, spfiles)



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
setDT(out, key = "spfile")

## Checks on merged table
message(cat("Species without polygons: "),
        sum(is.na(out$Species_Polygon)))
message(cat("Species with polygons: "),
        sum(!is.na(out$Species_Polygon)))

## Add taxonomic information
data <- fread(file.path(output_dir, "sp_with_expert_data.csv"))
x <- data[!duplicated(data[, .(spfile, scientificName, class, order, family)])][, .(spfile, scientificName, class, order, family)]
nrow(x) == nrow(out)
all(sort(x$spfile) == sort(out$spfile))

out <- merge(out, x, by = "spfile")
names(out); dim(out)
rm(x)

## Add EOO/AOO information
x <- fread(file.path(new_output_dir, "species_EOO_AOO_ahullareas_updatedsp.csv"))
nrow(x) == nrow(out) ## 2 error species
x[duplicated(x$spfile)]
## >> Check
out1 <- merge(out, x, by = "spfile")
all(out1$scientificName.x == out1$scientificName.y)
rm(out1)
## >> Merge rows found in both datasets (excludes error species)
x[, scientificName := NULL]
out1 <- merge(out, x, by = "spfile")
names(out1); dim(out1)
## >> Extract rows left out for the error species
sum(!(out$spfile %in% out1$spfile)) 
out2 <- out[!(out$spfile %in% out1$spfile)]
dim(out2)
## >> Checks
dim(out1)[1] + dim(out2)[1] == dim(out)[1]
ncol(out1) == ncol(out2)
names(out1)[!names(out1) %in% names(out2)]
## >> Add empty polygon information columns to out2 
dt <- setNames(data.table(matrix(nrow = nrow(out2), ncol = length(names(out1)[!names(out1) %in% names(out2)]))), names(out1)[!names(out1) %in% names(out2)])
out2 <- cbind(out2, dt)
dim(out2)
rm(dt)
## >> Combine table for error species
dim(out1)[1] + dim(out2)[1] == dim(out)[1]
ncol(out1) == ncol(out2)
out <- rbind(out1, out2)
names(out); dim(out)
setDT(out, key = "spfile")
rm(out1, out2)
## >> Drop columns
all(out$Nbe_unique_occ. == out$Occurrence_Points, na.rm = TRUE)
# out[which(!out$Nbe_unique_occ. == out$Occurrence_Points),][, .(spfile, Nbe_unique_occ., Occurrence_Points)]
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
write.csv(out, file = file.path(new_output_dir, "invert_fireoverlap_expertdata.csv"), 
          row.names = FALSE)

dim(out)
length(unique(out$spfile))
length(unique(out$scientificName))
out$scientificName[duplicated(out$scientificName)]


