
## Set working environment ####

rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "sp", "raster", 
       "rgdal", "rgeos", "gdalUtils",
       "alphahull", "ConR", "rnaturalearthdata",
       "doMC", "future", "future.apply", "parallel")
lapply(x, require, character.only = TRUE)
rm(x)


## Files and folder
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")
casestudy_dir = file.path(output_dir, "case_study")
if (!dir.exists(casestudy_dir)) {dir.create(casestudy_dir)}


## Q1. Adequacy of ALA data ####
## To compare species distributions & fire overlap based on ALA only versus ALA+other (state/museum) data
casestudy_dir = file.path(casestudy_dir, "datasources")
if (!dir.exists(casestudy_dir)) {dir.create(casestudy_dir)}

## I. Prepare datsets for analyses ####
## >> Subset by total number of records ####
data <- fread(file.path(output_dir, "data_ALAnonALA_wgs84_corrected.csv"))
counts <- data[, .N, spfile]
message(cat("Number of species with more than 10 records: "),
        nrow(counts[N > 10]))
message(cat("Number of species with more than 20 records: "),
        nrow(counts[N > 20]))
message(cat("Number of species with more than 50 records: "),
        nrow(counts[N > 50]))

data <- data[spfile %in% counts[N > 50]$spfile]
nrow(counts[N > 50]); length(unique(data$spfile))


## >> Subset by number of records per data source ####
unique(data$data_source)
sources <- fread(file.path(output_dir, "data_sources.csv"))[,1:2] ## file created manually based on unique data_source in data
unique(sources$type)

out <- data[, .N, by=.(spfile, data_source)]
length(unique(out$spfile))

out$data_type <- rep(character(), nrow(out))
out[data_source %in% sources[type == "state"]$data_source]$data_type = "state"
out[data_source %in% sources[type == "museum"]$data_source]$data_type = "museum"
out[data_source %in% sources[type == "ala"]$data_source]$data_type = "ala"
out[data_source %in% sources[type == "private"]$data_source]$data_type = "private"

# out <- out[, .(N=sum(N)),
#            by=.(spfile, data_type)] ## not needed, same as counts from previous step
out <- dcast(out, spfile ~ data_type, value.var = "N")
setDT(out, key = "spfile")
out[is.na(out)] <- 0

# ## Add tax info - only needed if out needs to be presented
# tax <- setDT(data, key = "spfile")[, .SD[1L] ,.(scientificName, class, order, family, spfile)]
# tax <- tax[,.(scientificName, class, order, family, spfile)]
# all(out$spfile %in% tax$spfile)
# 
# out <- merge(out, tax, by = "spfile")
# out <- out[, c(1, 5:8, 2:4)]
setDT(out, key = "spfile")


## >> Subset to species with data accross all sources ####
out[ala != 0]
out[museum == 0 & state == 0]
out <- out[ala != 0 & (museum != 0 | state != 0)]


## >> Create datasets for species with ALA only and ALA+state/museum data ####
## >> >> ALA + state/museum data ####
ala_plus <- data[spfile %in% out$spfile]
length(out$spfile); length(unique(ala_plus$spfile))
dim(ala_plus)

## Get counts for subset data by data_type 
ala_plus[, .N, by = "data_source"]
ala_plus$data_type <- rep(character(), nrow(ala_plus))
ala_plus[data_source %in% sources[type == "state"]$data_source]$data_type = "state"
ala_plus[data_source %in% sources[type == "museum"]$data_source]$data_type = "museum"
ala_plus[data_source %in% sources[type == "ala"]$data_source]$data_type = "ala"
ala_plus[data_source %in% sources[type == "private"]$data_source]$data_type = "private"
ala_plus[, .N, by = "data_type"]

counts <- ala_plus[, .N, by=.(spfile, data_type)]
counts <- dcast(counts, spfile ~ data_type, value.var = "N")
counts[is.na(counts)] <- 0
setDT(counts, key = "spfile")
counts[ala == 0]

write.csv(ala_plus, file = file.path(casestudy_dir, "data_casestudy_Q1.csv"),
          row.names = FALSE)

## >> >> ALA only ####
ala_only <- ala_plus[data_type == "ala"]; dim(ala_only)
length(unique(ala_only$spfile))


## II. Prepare rds files ####
## >> Create rds files for ALA only data ####
save_spdata2 <- function(species_uid, data, data_dir){
  
  temp <- dat[spfile == species_uid]
  spfile <- unique(temp$spfile)
  
  if (length(spfile) > 1){
    stop("Error: More than 1 unique spfile for naming species file...")
  }
  
  saveRDS(as.data.table(temp),
          file = file.path(data_dir, paste0(spfile, ".rds")))
}

spdata_dir = file.path(casestudy_dir,"ALAonly_spdata")
if (!dir.exists(spdata_dir)) {dir.create(spdata_dir)}

errorlog <- paste0(casestudy_dir, "/errorlog_ALAonly_rdsfiles.txt")
writeLines(c(""), errorlog)

dat <- ala_only
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
file.remove(errorlog)


## II. Create polygons ####
## >> ALA only data ####
polygons_dir = file.path(casestudy_dir,"ALAonly_polygons")
if (!dir.exists(polygons_dir)) {dir.create(polygons_dir)}
working_dir <- paste0("~/gsdms_r_vol", polygons_dir)

spfiles <- list.files(spdata_dir, pattern= ".rds$", full.names = TRUE)
length(spfiles)

source("~/gsdms_r_vol/tempdata/workdir/nesp_bugs/scripts/conr_iucn_eval.R")
basemap_file <- file.path(output_dir, "masks", "auslands_1poly_wgs84.shp")
hull.method <- "alpha.hull" # "convex.hull

plan(multiprocess, workers = future::availableCores()-2)
options(future.globals.maxSize = +Inf) ## CAUTION: Set this to a value, e.g. availablecores-1?/RAM-10?
errorlog <- paste0(casestudy_dir, "/errorlog_ALAonly_polygons.txt")
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
message(cat("Total number of input species files from cleaned ALA data: "),
        length(spfiles))
message(cat("Number of .csv output files created from IUCN.eval(): "),
        length(csvfiles))
message(cat("Number of .rds output files created from IUCN.eval(): "),
        length(rdsfiles))

output_sp <- basename(tools::file_path_sans_ext(rdsfiles))
input_sp <- basename(tools::file_path_sans_ext(spfiles))
input_sp[!input_sp %in% output_sp]
spfiles <- spfiles[!input_sp %in% output_sp]
file.remove(errorlog)
rm(conr_iucn_eval, working_dir)

## >> Save combined .csv output files from IUCN.eval() as data.table
csvfiles <- list.files(polygons_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
out <- do.call("rbind", lapply(csvfiles , read.csv)); dim(out)
out$spfile <- basename(tools::file_path_sans_ext(csvfiles))
names(out)[1] <- "scientificName"
setDT(out, key = "spfile")
out <- out[,Nbe_subPop := NULL]
write.csv(out, file = file.path(casestudy_dir, "species_EOO_AOO_ahullareas.csv"),
          row.names = FALSE)

message(cat("Number of species with EOOs: "),
        nrow(out[!is.na(EOO)]))
message(cat("Number of species without EOOs: "),
        nrow(out[is.na(EOO)]))

## Create list of non-NULL SPDF for species with EOOs
rdsfiles <- list.files(polygons_dir, pattern = ".rds$",
                       full.names = TRUE, all.files = TRUE)
polynames <- basename(tools::file_path_sans_ext(rdsfiles))
temp <- lapply(rdsfiles, readRDS)
names(temp) <- polynames
length(temp)

temp2 <- lapply(temp, "[[", 1)
temp2 <- lapply(temp2, "[[", 2)

na.eooIDX <- sapply(temp2, length)
na.eooIDX <- which(na.eooIDX == 0)
temp3 <- temp2[-na.eooIDX]
length(temp3)
saveRDS(temp3, file = file.path(casestudy_dir, "species_ahullEOOspdf.rds"))


## III. Fire overlap ####
## >> Point overlap ####
overlap_dir = file.path(casestudy_dir, "ALAonly_points_overlap")
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}
source("/tempdata/workdir/nesp_bugs/scripts/points_overlap.R")

spfiles <- list.files(spdata_dir, pattern= ".rds$", full.names = TRUE)
length(spfiles)

fire_severity <- raster(file.path(output_dir, "fire", "severity5_eqar250_native_paa.tif"))
fire_classes <- sort(unique(na.omit(fire_severity[])))

wgs_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
eqarea_crs <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

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
log
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of input species: "),
        length(spfiles))
message(cat("Number of output files: "),
        length(csvfiles))

## Output table
out <- do.call("rbind", lapply(csvfiles, fread)); dim(out)
names(out)[1] <- "spfile"

message(cat("Check if #points overlapping with fire is always <= Total # points for species: "),
        all(rowSums(out[,.(Fire_Class_1, Fire_Class_2, Fire_Class_3, Fire_Class_4, Fire_Class_5)], na.rm = TRUE) <= out$Occurrence_Points))
message(cat("Check for NAs: "),
        sum(is.na(out)))
num <- grep("Fire_Class_", names(out))
message(cat("Number of species with 0 overlap: "),
        sum(rowSums(out[, ..num]) == 0))
rm(num)

## Add percentage overlap columns
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

## Save table
names(out)[grep("Fire_Class_", names(out))] <- paste0(names(out)[grep("Fire_Class_", names(out))], "_Points")
setDT(out, key = "spfile")
write.csv(out, file = file.path(casestudy_dir, "species_points_fireoverlap.csv"), 
          row.names = FALSE)


## >> Polygon overlap ####
shapefile_dir = file.path(casestudy_dir, "species_shapefiles")
overlap_dir = file.path(casestudy_dir, "polygon_overlap")
if(!dir.exists(shapefile_dir)){dir.create(shapefile_dir)}
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

## Load spdf data for species with EOO ####
species_maps <- readRDS(file.path(casestudy_dir, "species_ahullEOOspdf.rds"))
polygon_list <- names(species_maps)
message(cat("Number of species with polygons: "),
        length(polygon_list))

## Run overlap analysis in parallel: doMC ####
job_script <- file.path("/tempdata/workdir/nesp_bugs/", 
                        "scripts", "casestudy_alaonly_polygon_overlap_job.R")
rstudioapi::jobRunScript(job_script, encoding = "unknown", 
                         workingDir = "/tempdata/workdir/nesp_bugs",
                         importEnv = FALSE, exportEnv = "")

## Check files
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of input species: "),
        length(polygon_list))
message(cat("Number of output files: "),
        length(csvfiles)) 
length(polygon_list) - length(csvfiles)

## Output table
out <- do.call("rbind", lapply(csvfiles, fread)); dim(out)
names(out)[1] <- "spfile"

message(cat("Check if area overlapping with fire is always <= Total polygon area for species: "),
        all(rowSums(out[,.(Fire_Class_1, Fire_Class_2, Fire_Class_3, Fire_Class_4, Fire_Class_5)]) <= out$Species_Polygon))
message(cat("Check for NAs: "),
        sum(is.na(out)))
message(cat("Number of species with Species_Polygon area = 0: "),
        sum(out$Species_Polygon == 0))
out[which(out$Species_Polygon == 0)]
  ## all 21 species showing zero fire overlap

## Add percentage overlap columns
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

## Save table
names(out)[grep("Fire_Class_", names(out))] <- paste0(names(out)[grep("Fire_Class_", names(out))], "_Area")
setDT(out, key = "spfile")
write.csv(out, file = file.path(casestudy_dir, "species_polygon_fireoverlap.csv"), 
          row.names = FALSE)


## IV. Output table for ALA only data ####
## >> Combine point and polygon outputs
poly <- fread(file.path(casestudy_dir, "species_polygon_fireoverlap.csv")); dim(poly)
point <- fread(file.path(casestudy_dir, "species_points_fireoverlap.csv")); dim(point)
  ## we can lose the 6 species without polygons

## Merge rows for species with polygon overlap innfo (n = 29029)
out <- merge(point, poly, by = "spfile"); dim(out)
setDT(out, key = "spfile")
rm(point, poly)

## Add tax info
tax <- fread(file.path(output_dir, "data_ALAnonALA_wgs84_corrected_taxinfo.csv"))
all(out$spfile %in% tax$spfile)
tax <- tax[spfile %in% out$spfile]
setDT(tax, key = "spfile")

dim(out); dim(tax)
out <- merge(out, tax); dim(out)

## Total number of points overlaping GEEBAM
grep("_Points$", names(out), value = TRUE)[1:5]
num <- grep("_Points$", names(out), value = TRUE)[1:5]
out$Total_fire_points <- rowSums(out[, ..num])
nrow(out[Total_fire_points == 0])

## Total polygon area overlaping GEEBAM
grep("_Area$", names(out), value = TRUE)[1:5]
num <- grep("_Area$", names(out), value = TRUE)[1:5]
out$Total_fire_polygon <- rowSums(out[, ..num])
nrow(out[Total_fire_polygon == 0])

## Add information from species_EOO_AOO_ahullareas.csv
polyareas <- fread(file.path(casestudy_dir, "species_EOO_AOO_ahullareas.csv"))
polyareas <- setDT(polyareas, key = "spfile")[spfile %in% out$spfile][, .(spfile, EOO, AOO, Nbe_unique_occ., scientificName)]
dim(out); length(unique(out$spfile)); length(unique(out$scientificName))
dim(polyareas); length(unique(polyareas$spfile)); length(unique(polyareas$scientificName))

  # ## Check
  # out2 <- merge(out, polyareas, by = "spfile")
  # all(out2$scientificName.x == out2$scientificName.y)
  # which(!(out2$scientificName.x == out2$scientificName.y))
  # sum(!(out2$scientificName.x == out2$scientificName.y))
  # out2[which(!(out2$scientificName.x == out2$scientificName.y))][,.(scientificName.x, scientificName.y)]
  # ## out2$scientificName.x & out2$scientificName.y are the same
  # rm(out2)
polyareas[, scientificName := NULL]
out <- merge(out, polyareas, by = "spfile"); dim(out)

## Save table
all(out$Nbe_unique_occ. == out$Occurrence_Points)
out[, Nbe_unique_occ. := NULL]

names(out[, c(1, 20:23, 2:6, 8:10, 24, 7, 11:15, 17:19, 25, 16, 26:27)])
out <- out[, c(1, 20:23, 2:6, 8:10, 24, 7, 11:15, 17:19, 25, 16, 26:27)]
setDT(out, key = "spfile")
write.csv(out, file = file.path(casestudy_dir, paste0("ALAonly_overlap.csv")), 
          row.names = FALSE)


## V. Output table for ALA + state/museum data ####
sum(duplicated(out$spfile))

all_out <- fread(file.path(output_dir, "invert_overlap_2021-06-24.csv"))
all_out <- all_out[spfile %in% out$spfile]
names(all_out)
all_out <- all_out[, 1:28]
all_out[, PAA_Points := NULL]

write.csv(all_out, file = file.path(casestudy_dir, "ALAplus_overlap.csv"),
          row.names = FALSE)


## VI. Combine outputs for datasets ####
ala_only <- fread(file.path(casestudy_dir, "ALAonly_overlap.csv"))
ala_plus <- fread(file.path(casestudy_dir, "ALAplus_overlap.csv"))
all(names(ala_only) == names(ala_plus))

## Remove columns
num <- grep("Fire_Class_", names(ala_only), invert = TRUE)
ala_only <- ala_only[, ..num]
ala_plus <- ala_plus[, ..num]
all(names(ala_only) == names(ala_plus))

all(ala_only$scientificName == ala_plus$scientificName)
ala_only <- ala_only[, c(1, 6:17)] ## drop scientificName, class, order, family columns

## Rename columns
names(ala_only)[-1] <- paste0(names(ala_only)[-1], "_ALAonly")
names(ala_plus)[-c(1:5)] <- paste0(names(ala_plus)[-c(1:5)], "_ALAplus")

## Merge
setDT(ala_only, key = "spfile")
setDT(ala_plus, key = "spfile")
all <- merge(ala_plus, ala_only, by = "spfile")
names(all)

setDT(all, key = "spfile")
setorder(all, "EOO_ALAplus", "EOO_ALAonly", na.last = TRUE) 
write.csv(all, file = file.path(casestudy_dir, "datasources_compare.csv"),
          row.names = FALSE)

## V. Plot differences ####
## >> Differences in EOO ####
## ALA only data
out1 <- fread(file.path(casestudy_dir, "species_EOO_AOO_ahullareas.csv"))
message(cat("Number of species with EOOs: "),
        nrow(out1[!is.na(EOO)]))
message(cat("Number of species without EOOs: "),
        nrow(out1[is.na(EOO)]))

## ALA + state/museum data
out2 <- fread(file.path(output_dir, "species_EOO_AOO_ahullareas.csv"))
out2 <- out2[spfile %in% all_species]
message(cat("Number of species with EOOs: "),
        nrow(out2[!is.na(EOO)]))
message(cat("Number of species without EOOs: "),
        nrow(out2[is.na(EOO)]))

##
all(out1$spfile == out2$spfile)
all(out1$scientificName == out2$scientificName)

eoo <- out1[,.(spfile, scientificName, EOO, AOO, Nbe_unique_occ.)]
eoo$EOO_alaplus <- out2$EOO
eoo$AOO_alaplus <- out2$AOO
eoo$N_alaplus <- out2$Nbe_unique_occ.
names(eoo) <- c("spfile", "scientificName", "EOO_ala", "AOO_ala", "N_ala", "EOO_alaplus", "AOO_alaplus", "N_alaplus")

all(eoo$EOO_alaplus > eoo$EOO_ala)
sum(eoo$EOO_alaplus < eoo$EOO_ala, na.rm = TRUE)
## note: plausible because more data might lead many small polygons than potentially bigger polygons created from less data.

## Subset to non NA EOO species
eoo <- eoo[!is.na(EOO_ala)]

plot(density(eoo$EOO_alaplus))


## >> Difference in fire overlap
## ALA data only

## ALA + state/museum data
fire_sp <- fread(file.path(output_dir, "invert_overlap_PAAonly_2021-06-11.csv"))

## Histogram differences fire overlap > 0%
fire_sp0 <- fire_sp[Overlap_Polygons_Fire2345_GEEBAM2_as_burnt > 0]$spfile
data1 <- data[spfile %in% fire_sp0]; dim(data1)
length(fire_sp0); length(unique(data1$spfile))
unique(data1$spfile)

## Histogram differences fire overlap > 30%
fire_sp30 <- fire_sp[Overlap_Polygons_Fire2345_GEEBAM2_as_burnt > 30]$spfile
data2 <- data[spfile %in% fire_sp30]; dim(data1)
length(fire_sp30); length(unique(data2$spfile))
unique(data2$spfile)









## Q2. Reliability of observatonal data in ALA?
## Compare outputs from observational versus specimen in ALA
rm(data)
data <- data


## Q1. Change in species distributions (ALA and other)
## >> By number of records ####
counts <- data[, .N, spfile]
message(cat("Number of species with more than 10 records: "),
        nrow(counts[N > 10]))
message(cat("Number of species with more than 20 records: "),
        nrow(counts[N > 20]))
message(cat("Number of species with more than 50 records: "),
        nrow(counts[N > 50]))

data <- data[spfile %in% counts[N > 50]$spfile]
nrow(counts[N > 50]); length(unique(data$spfile))



## Summary stats

## Can identify

## Cannot identify




## >> By fire impact ####
# fire_sp <- fread(file.path(bugs_dir, "JM_traits", "FireInvert_scored_expertANDimpacted14.06.csv"))
# fire_sp <- fire_sp[,c(4:7)]
# names(fire_sp)[1] <- "spfile"
# fire_sp <- fire_sp[mean_severe_use_pt_if_pt12 > 30]$spfile

fire_sp <- fread(file.path(output_dir, "invert_overlap_PAAonly_2021-06-11.csv"))
fire_sp <- fire_sp[Overlap_Polygons_Fire2345_GEEBAM2_as_burnt > 30]$spfile

data <- data[spfile %in% fire_sp]; dim(data)
length(fire_sp); length(unique(data$spfile))

## >> By taxonomic group: order = Lepidoptera - not applied yet
nrow(data[order == "lepidoptera"])
length(grep("lepidop", data$order))

data[grep("kosciuscola_tristis", data$spfile)]

butterflies <- tolower(c("Lycaenidae", "Papilionidae", "Nymphalidae", "Pieridae", "Hesperiidae"))



## Species selection from expert dataset ####
##  >>>>> Not incorporated into data_ALAnonALA_wgs84_corrected.csv <<<<<
rm(data)
data <- data_expert

dim(data)
unique(data$spfile)
unique(data$scientificName)
unique(data$data_source)

## Add to dataset
data[grep("kosciuscola_tristis", data$spfile)]
k.tristis_data <- fread(file.path(bugs_dir, "data_corrections", "kosciuscola_tristis_expert_data.csv"))
  ## Need sub-species identified

## >> By fire impact - NOT RUN (leaves only 1 species) ####
fire_sp <- fread(file.path(output_dir, "invert_overlap_PAAonly_2021-06-11.csv"))
fire_sp <- fire_sp[Overlap_Polygons_Fire2345_GEEBAM2_as_burnt > 30]$spfile

data <- data[spfile %in% fire_sp]; dim(data)
length(fire_sp); length(unique(data$spfile))

## >> By number of records ####
counts <- data[, .N, spfile]
message(cat("Number of species with more than 5 records: "),
        nrow(counts[N > 5]))
message(cat("Number of species with more than 10 records: "),
        nrow(counts[N > 10]))
message(cat("Number of species with more than 20 records: "),
        nrow(counts[N > 20]))
message(cat("Number of species with more than 50 records: "),
        nrow(counts[N > 50]))

data <- data[spfile %in% counts[N > 5]$spfile]
nrow(counts[N > 5]); length(unique(data$spfile))

## >> By number of records by data source ####
unique(data$data_source)
sources <- fread(file.path(output_dir, "data_sources.csv"))[,1:2] ## file created manually based on unique data_source in data
unique(sources$type)

## Summarize data by species and data source 
out <- data[, .N, by=.(spfile, data_source)]
length(unique(out$spfile))

out$data_type <- rep(character(), nrow(out))
out[data_source %in% sources[type == "state"]$data_source]$data_type = "state"
out[data_source %in% sources[type == "museum"]$data_source]$data_type = "museum"
out[data_source %in% sources[type == "ala"]$data_source]$data_type = "ala"
out[data_source %in% sources[type == "private"]$data_source]$data_type = "private"

out <- out[, .(N=sum(N)),
           by=.(spfile, data_type)]
out <- dcast(out, spfile ~ data_type, value.var = "N")
setDT(out, key = "spfile")
out[is.na(out)] <- 0

## Add tax info
tax <- setDT(data, key = "spfile")[, .SD[1L] ,.(scientificName, class, order, family, spfile)]
tax <- tax[,.(scientificName, class, order, family, spfile)]
all(out$spfile %in% tax$spfile)

out <- merge(out, tax, by = "spfile")
out <- out[, c(1, 4:7, 2:3)]
setDT(out, key = "spfile")

## Subset to species with data accross all sources
out
out[ala != 0 & private != 0]

## Check if these species are fire impacted
message(cat("Species with expert data impacted by fire: "),
        out[ala != 0 & private != 0][out[ala != 0 & private != 0]$spfile %in% fire_sp]$spfile)


# ## Function to replace NA in large data tables
# ## Ref: https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
# f_dowle2 = function(DT) {
#   for (i in names(DT))
#     DT[is.na(get(i)), (i):=0]
# }


# data_expert <- fread(file.path(output_dir, "sp_with_expert_data.csv"))
