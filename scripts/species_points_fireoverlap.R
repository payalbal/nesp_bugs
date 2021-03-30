## Fire overlap analysis: foreach{}
## Collaborator: Casey Visintin


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")
x <- c("data.table", "sp", "raster", "rgdal", 
       "gdalUtils", "rgeos", "doMC", "foreach")
lapply(x, require, character.only = TRUE)
rm(x)

## File paths and folders
bugs_data = "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_data, "outputs")
spdata_dir = file.path(output_dir, "ala_nonala_data" ,"spdata")

overlap_dir = file.path(output_dir, "points_overlap")
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

source("/tempdata/workdir/nesp_bugs/scripts/points_overlap.R")


## Fire overlap for species WITHOUT EOO polygons  ####
## >> Load species  names without EOOs
points_list <- fread(file.path(output_dir, paste0("species_ahullnoEOO.csv")))
points_list <- points_list$x
length(points_list)

## >> Find species data rds files
spfiles <- list.files(spdata_dir, pattern= ".rds$", full.names = TRUE)
x <- basename(tools::file_path_sans_ext(spfiles))
spfiles <- spfiles[x %in% points_list]

## >> Load in fire severity raster (re-classed) and get unique classes ####
fire_severity <- raster(file.path(output_dir, "fire", "severity3_eqar250.tif"))
fire_classes <- sort(unique(na.omit(fire_severity[])))

## Function parameters
wgs_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
eqarea_crs <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

## >> Run overlap analysis in parallel: doMC ####
registerDoMC(future::availableCores()-2)
system.time(log <- foreach(species_dat = spfiles, 
                           .combine = rbind,
                           .errorhandling = "pass",
                           .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                             
                             points_overlap(data_rds = species_dat, 
                                            crs_org = wgs_crs, 
                                            crs_new = eqarea_crs, 
                                            fire_classes = fire_classes,
                                            outdir = overlap_dir)
                           })




## Error checking ####
log
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of input species: "),
        length(spfiles))
message(cat("Number of output files: "),
        length(csvfiles))


## Output table ####
## Merge csv files
out <- do.call("rbind", lapply(csvfiles, fread))
names(out)[1] <- "spfile"
setDT(out, key = "spfile")

message(cat("Check if #points overlapping with fire is always <= Total # points for species: "),
        all(rowSums(out[,.(Fire_Class_1, Fire_Class_2, Fire_Class_3)]) <= out$Occurrence_Points))

## Look for NAs in table
message(cat("Check for NAs: "),
        sum(is.na(out)))

## Look for Species_Polygon = 0 in table
message(cat("Number of species with Occurrence_Points = 0: "),
        sum(out$Occurrence_Points == 0))


## Add percentage overlap
out$Percent_Overlap <- ((out$Fire_Class_2 + out$Fire_Class_3)/(out$Occurrence_Points - out$Fire_Class_1)) * 100
sum(is.na(out$Percent_Overlap))
naidx <- which(is.na(out$Percent_Overlap))
out[naidx]


## Add class/family information to output table
tax <- fread(file = file.path(output_dir, "data_ALAnonALA_wgs84.csv"))
tax <- setDT(tax, key = "spfile")[, .SD[1L] ,.(scientificName, class, family, spfile)]
tax <- tax[,.(scientificName, class, family, spfile)]
tax <- tax[spfile %in% out$spfile]

dim(out); length(unique(out$spfile))
dim(tax); length(unique(tax$spfile)); length(unique(tax$scientificName))

out <- merge(out, tax, by = "spfile")

## Save output table
setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, "species_points_fireoverlap.csv"), row.names = FALSE)


## Add EOO and AOO information from pecies_EOO_AOO_ahullareas.csv ####
polyareas <- fread(file.path(output_dir, "species_EOO_AOO_ahullareas.csv"))
polyareas <- setDT(polyareas, key = "spfile")[spfile %in% out$spfile][, .(spfile, AOO, Nbe_unique_occ., scientificName)]

dim(out); length(unique(out$spfile)); length(unique(out$scientificName))
dim(polyareas); length(unique(polyareas$spfile)); length(unique(polyareas$scientificName))

# ## Check
out2 <- merge(out, polyareas, by = "spfile")
all(out2$scientificName.x == out2$scientificName.y)
which(!(out2$scientificName.x == out2$scientificName.y))
out2[which(!(out2$scientificName.x == out2$scientificName.y))]
  ## out2$scientificName.x & out2$scientificName.y are the same

polyareas[, scientificName := NULL]
out <- merge(out, polyareas, by = "spfile")
write.csv(out, file = file.path(output_dir, "species_points_fireoverlap_AOOinfo.csv"), 
          row.names = FALSE)


## Remove files ####
# file.remove(file.path(overlap_dir, dir(path = overlap_dir)))
# unlink(overlap_dir, recursive = TRUE)


## Summarize outputs ####
out <- fread(file.path(output_dir, "species_points_fireoverlap.csv"))
message(cat("NA in scientificName: "),
        length(which(is.na(out$scientificName))))

message(cat("Total number of species: "),
        nrow(out))

message(cat("Number of species showing overlap: "))
out[, .N, by = Occurrence_Points]

## Unique class-family summaries in 100% overlap
out[Percent_Overlap == 100][, .SD[1L] ,.(class, family)][,.(class,family)]
out[Percent_Overlap == 100][, .SD[1L] ,.(class)][,.(class)][, .N, class]$class
out[Percent_Overlap == 100][, .SD[1L] ,.(family)][,.(family)][, .N, family]

## High severity overlaps
fire3_overlap <- out$Fire_Class_3/out$Occurrence_Points
message(cat("Number of species with high fire severity impact on all (n = 2) recorded data points: "),
        length(which(fire3_overlap == 1)))




