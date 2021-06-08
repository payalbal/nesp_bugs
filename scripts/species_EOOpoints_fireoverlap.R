## Fire overlap analysis: foreach{}
## Collaborator: Casey Visintin

## Points overlap for species with 3+ records


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
bugs_dir = "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")
spdata_dir = file.path(output_dir, "ala_nonala_data" ,"spdata")

overlap_dir = file.path(output_dir, "polygon_points_overlap")
## Remove existing overlap folder
# file.remove(file.path(overlap_dir, dir(path = overlap_dir)))
# unlink(overlap_dir, recursive = TRUE)
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

source("/tempdata/workdir/nesp_bugs/scripts/points_overlap.R")


## Fire overlap for species WITH EOO polygons  ####
## >> Load species  names with EOOs
points_list <- readRDS(file.path(output_dir, "species_ahullEOOspdf.rds"))
points_list <- names(points_list)
length(points_list)

## >> Find species data rds files
spfiles <- list.files(spdata_dir, pattern= ".rds$", full.names = TRUE)
x <- basename(tools::file_path_sans_ext(spfiles))
spfiles <- spfiles[x %in% points_list]

## >> Load in fire severity raster (re-classed) and get unique classes ####
fire_severity <- raster(file.path(output_dir, "fire", "severity5_eqar250_native_paa.tif"))
fire_classes <- sort(unique(na.omit(fire_severity[])))

## Function parameters
wgs_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
eqarea_crs <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

## >> Run overlap analysis in parallel: doMC ####
registerDoMC(future::availableCores()-2)
system.time(foreach(species_dat = spfiles, 
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
write.csv(out, file = file.path(output_dir, "species_EOOpoints_fireoverlap.csv"), row.names = FALSE)


## Merge with polygon overlap table
temp <- fread(file.path(output_dir, "species_polygon_fireoverlap_EOOinfo.csv"))
names(temp)[c(2:4,6)] <- paste0(names(temp)[c(2:4,6)], "_Area")
setDT(temp, key = "spfile")

names(out)[c(2:4,6)] <- paste0(names(out)[c(2:4,6)], "_Npoints")
out[, class := NULL]; out[, family := NULL]
setDT(out, key = "spfile")

# ## Check
out2 <- merge(temp, out, by = "spfile")
all(out2$scientificName.x == out2$scientificName.y)
which(!(out2$scientificName.x == out2$scientificName.y))

all(out2$Nbe_unique_occ. == out2$Occurrence_Points)

out[, scientificName := NULL]
out[, Nbe_unique_occ. := NULL]
out <- merge(temp, out, by = "spfile")

out <- out[ , c(1,7,8,9,2:6,12:16,10,11)]
write.csv(out, file = file.path(output_dir, "species_polygon_fireoverlap_allinfo.csv"),
          row.names = FALSE)

