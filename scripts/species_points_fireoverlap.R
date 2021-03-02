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

## >> Find species data rds files
spfiles <- list.files(spdata_dir, pattern= ".rds$", full.names = TRUE)
x <- basename(tools::file_path_sans_ext(spfiles))
spfiles <- spfiles[x %in% points_list] ## subset spfiles for all IUCN species

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
        all(out$Total_Overlap <= out$Occurrence_Points))

## Add class/family information to output table
tax <- fread(file = file.path(output_dir, "data_ALAnonALA.csv"))
tax <- setDT(tax, key = "spfile")[, .SD[1L] ,.(scientificName, class, family, spfile)]
tax <- tax[,.(scientificName, class, family, spfile)]
tax <- tax[spfile %in% out$spfile]

dim(out); length(unique(out$spfile)); length(unique(out$scientificName))
dim(tax); length(unique(tax$spfile)); length(unique(tax$scientificName))

out <- merge(out, tax, by = "spfile")
out <- out[,c(10:8, 2:7, 1)]

## Save output table
setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, "species_points_fireoverlap.csv"), row.names = FALSE)


## Remove files ####
file.remove(file.path(overlap_dir, dir(path = overlap_dir)))
unlink(shapefile_dir, recursive = TRUE)
unlink(overlap_dir, recursive = TRUE)


## Summarize outputs ####
point.sp <- fread(file.path(output_dir, "species_points_fireoverlap.csv"))
message(cat("NA in scientificName: "),
        length(which(is.na(point.sp$scientificName))))

message(cat("Total number of species: "),
        nrow(point.sp))

message(cat("Number of species showing overlap: "))
point.sp[, .N, by = Total_Overlap]

message(cat("# Species showing 100% fire overlap: "),
        nrow(point.sp[Percent_Overlap == 100]));
round(nrow(point.sp[Percent_Overlap == 100])/nrow(point.sp), 2)

## Unique class-family summaries in 100% overlap
point.sp[Percent_Overlap == 100][, .SD[1L] ,.(class, family)][,.(class,family)]
point.sp[Percent_Overlap == 100][, .SD[1L] ,.(class)][,.(class)][, .N, class]
point.sp[Percent_Overlap == 100][, .SD[1L] ,.(family)][,.(family)][, .N, family]

message(cat("# Species showing == 100% fire overlap: "),
        nrow(point.sp[Percent_Overlap == 100]));
round(nrow(point.sp[Percent_Overlap == 100])/nrow(point.sp), 2)

message(cat("# Species showing > 50% fire overlap: "),
        nrow(point.sp[Percent_Overlap == 50]));
round(nrow(point.sp[Percent_Overlap == 50])/nrow(point.sp), 2)

message(cat("# Species showing no fire overlap: "),
        nrow(point.sp[Percent_Overlap == 0]))
round(nrow(point.sp[Percent_Overlap == 0])/nrow(point.sp), 2)




