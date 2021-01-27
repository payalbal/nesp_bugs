## Fire overlap analysis: foreach{}
## Collaborator: Casey Visintin

## Notes
## Yet to run for Prelim analysis area (in/out; prop of habitat within) - to clip


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
spmasked_dir <- file.path(output_dir, "ala_data" ,"spdata_masked")

overlap_dir = file.path(output_dir, "points_overlap")
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

source("/tempdata/workdir/nesp_bugs/scripts/points_overlap.R")


## Fire overlap for species WITHOUT EOO polygons  ####

## >> Load species data ####
## Species names without EOOs + 3 error species from ala_polygons.R
points_list <- fread(file.path(output_dir, "ala_noEOOspecies.csv"))
points_list <- points_list$x
length(points_list)

## Check number of records for these species in UCN.eval outputs
temp <- fread(file.path(output_dir, "ala_polygons_areas.csv"))
dim(temp[is.na(EOO)])
summary(temp[is.na(EOO)]$Nbe_unique_occ.)

## Find cleand/masked species data files
datfiles <- list.files(spmasked_dir, pattern= "_masked.rds$", full.names = TRUE)
x <- basename(tools::file_path_sans_ext(datfiles))
x <- gsub("_masked", "", x)
datfiles <- datfiles[x %in% points_list] ## subset datfiles for all IUCN species

## >> Load fire severity raster (re-classed) and get unique classes ####
fire_severity <- raster(file.path(output_dir, "fire", "severity3_eqar250.tif"))
fire_classes <- sort(unique(na.omit(fire_severity[])))

## Function parameters
wgs_crs  <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
eqarea_crs <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

## >> Run overlap analysis in parallel: doMC ####
registerDoMC(future::availableCores()-2)
system.time(log <- foreach(species_dat = datfiles, 
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
        length(datfiles))
message(cat("Number of output files: "),
        length(csvfiles))


## Output table ####
## Merge csv files
out <- do.call("rbind", lapply(csvfiles , fread))
out <- fread(file.path(output_dir, "Points_fireoverlap.csv"))
names(out)[1] <- "SpeciesName"
setorder(out, SpeciesName)
out <- as.data.table(out)
setkey(out, "SpeciesName")

message(cat("Check if #points overlapping with fire is always <= Total # points for species: "),
        all(out$Total_Overlap <= out$Occurrence_Points))

## Extract taxonomic information for species
ala <- readRDS(file.path(output_dir, "clean2_ala_2020-10-28.rds"))
setkey(ala, "spfile")

message(cat("Are all species in output table found in cleaned ALA data? "),
        length(out$Species) == length(which(out$Species %in% ala$spfile)))

taxinfo <- c("phylum", "class", "order", "family", 
             "genus", "species", "subspecies")
temp <- data.table()
redo <- c()
for (sp in out$Species){
  if (nrow(unique((ala[.(sp), ..taxinfo]))) > 1){
    
    warning(paste0("More than 1 unique taxon info found for ", sp))
    redo <- c(redo, sp)
    
  } else {
    x <- unique((ala[.(sp), ..taxinfo]))
    temp <- rbind(temp, cbind(SpeciesName = sp, x))
  }
}

## Rerun for duplicates found
temp2 <- c()
for (sp in redo){
  x <- unique((ala[.(sp), ..taxinfo]))
  temp2 <- rbind(temp2, cbind(SpeciesName = sp, x))
}
temp2 <- temp2[!which(duplicated(temp2$SpeciesName))]

## Combine tables
temp <- rbind(temp, temp2)
temp <- as.data.table(temp)
setorder(temp, SpeciesName)
setkey(temp, "SpeciesName")
out <- merge(out, temp, by = "SpeciesName")
setorder(out, SpeciesName)
setkey(out)

## Find NA species
length(which(is.na(out$SpeciesName)))

## Save output table
write.csv(out, file = file.path(output_dir, "Points_fireoverlap.csv"), 
          row.names = FALSE)


## Remove files ####
file.remove(file.path(overlap_dir, dir(path = overlap_dir)))
unlink(overlap_dir, recursive = TRUE)


## Summarize outputs ####
message("Total number of species: ")
nrow(out)

message("Number of species showing overlap: ")
out[, .N, by = Total_Overlap]

message("Species showing 100% fire overlap: ")
length(out[Total_Overlap == Occurrence_Points]$Species)

message("Species showing 50% fire overlap: ")
length(out[Total_Overlap == (Occurrence_Points/2)]$Species)

message("Species showing no fire overlap: ")
length(out[Total_Overlap == 0]$Species)





