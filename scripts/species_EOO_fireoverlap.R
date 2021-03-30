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
bugs_dir = "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")

shapefile_dir = file.path(output_dir, "species_shapefiles")
if(!dir.exists(shapefile_dir)){dir.create(shapefile_dir)}

overlap_dir = file.path(output_dir, "polygon_overlap")
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

source("/tempdata/workdir/nesp_bugs/scripts/polygon_overlap.R")


## Overlaps for species WITH EOO polygons  ####
## >> Load in spdf data for species with EOO ####
species_maps <- readRDS(file.path(output_dir, "species_ahullEOOspdf.rds"))
polygon_list <- names(species_maps)

## >> Load in fire severity raster (re-classed) and get unique classes ####
fire_severity <- raster(file.path(output_dir, "fire", "severity3_eqar250.tif"))
fire_vals <- fire_severity[]
fire_classes <- sort(unique(na.omit(fire_vals)))

## >> Run overlap analysis in parallel: doMC ####
## 'log' only useful when running small number of species
registerDoMC(future::availableCores()-2)
system.time(foreach(polys = polygon_list,
                      .combine = rbind,
                      .errorhandling = "pass",
                      .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{

                        polygon_overlap(species_name = polys, # polys = polygon_list[335]
                                        species_poly = species_maps[[polys]],
                                        shapefile_dir = shapefile_dir,
                                        fire_vals = fire_vals,
                                        fire_classes = fire_classes,
                                        outdir = overlap_dir)
                      })


## Error checking ####
## >> Display results summary ####
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of input species: "),
        length(polygon_list))
message(cat("Number of output files: "),
        length(csvfiles))
length(polygon_list) - length(csvfiles)

## >> Find missing species from outputs ####
csvnames <- tools::file_path_sans_ext(basename(csvfiles))
error_list <- polygon_list[!polygon_list %in% csvnames]
message(cat("Number of species in error list: "),
        length(error_list))


## Reruns I - batch runs ####
## Repeat this till most of the errors are fixed
## Errors seem to be an artefact of the system rather than problem with data/code
if(length(error_list) <= future::availableCores()-2) {
  registerDoMC(length(error_list))
} else{
  registerDoMC(future::availableCores()-2)
}
system.time(foreach(polys = error_list,
                    .combine = rbind,
                    .errorhandling = "pass",
                    .packages = c('sp', 'raster',
                                  'rgdal', 'data.table')) %dopar%{
                                    
                                   polygon_overlap(species_name = polys,
                                                    species_poly = species_maps[[polys]],
                                                    shapefile_dir = shapefile_dir,
                                                    fire_vals = fire_vals,
                                                    fire_classes = fire_classes,
                                                    outdir = overlap_dir)
                                  })


csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of input species: "),
        length(polygon_list))
message(cat("Number of output files: "),
        length(csvfiles))
csvnames <- basename(tools::file_path_sans_ext(csvfiles))
error_list[!error_list %in% csvnames]


## Reruns II - individual species runs ####
polys = error_list[2]
species_name = polys
species_poly = species_maps[[polys]]
outdir = overlap_dir
## Run polygon_pverlap stepwise...

csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
message(cat("Number of input species: "),
        length(polygon_list))
message(cat("Number of output files: "),
        length(csvfiles))
csvnames <- basename(tools::file_path_sans_ext(csvfiles))
error_list[!error_list %in% csvnames]


## Output table ####
## Merge csv files
out <- do.call("rbind", lapply(csvfiles, fread))
names(out)[1] <- "spfile"
setDT(out, key = "spfile")


message(cat("Check if area overlapping with fire is always <= Total polygon area for species: "),
        all(rowSums(out[,.(Fire_Class_1, Fire_Class_2, Fire_Class_3)]) <= out$Species_Polygon))

## Look for NAs in table
message(cat("Check for NAs: "),
        sum(is.na(out)))

  # which(is.na(out), arr.ind=TRUE)
  # error_list <- csvnames[!csvnames %in% out$Species]
  # error_files <- csvfiles[!csvnames %in% out$Species]

## Look for Species_Polygon = 0 in table
message(cat("Number of species with Species_Polygon area = 0: "),
        sum(out$Species_Polygon == 0))

naidx <- which(out$Species_Polygon == 0)
out[naidx]

## >> Check individual species
polys = "acontiostoma_marionis_21033"
species_name = polys
species_poly = species_maps[[polys]]
outdir = overlap_dir
## Run polygon_pverlap stepwise...
## Create shapefiles and import in QGIS on top of aus_mask: auslands_aea137
## Then create tif and import inn QGIS
## We see that these species lie on islands that fall off the specified extent of the species rasetr (tif)
## Extent is specified as per the the fire map (see notes in polygon_overlap.R)
## Therefore, the species raster created for these species is all 0 and hence polygon calculatyed for these species = 0
## If we use the alternate function to create species raster (see notes at the end of in polygon_overlap.R),
## we get raster values with 0 and 1, but these cannot be overlaid with the fire map oif lesser extent anyway,
## So we will have to discard these species. 

## Add percentage overlap
out$Percent_Overlap <- ((out$Fire_Class_2 + out$Fire_Class_3)/(out$Species_Polygon - out$Fire_Class_1)) * 100
sum(is.na(out$Percent_Overlap))
all(which(is.na(out$Percent_Overlap)) == naidx)

## Add class/family information to output table
tax <- fread(file = file.path(output_dir, "data_ALAnonALA_wgs84.csv"))
tax <- setDT(tax, key = "spfile")[, .SD[1L] ,.(scientificName, class, family, spfile)]
tax <- tax[,.(scientificName, class, family, spfile)]
tax <- tax[spfile %in% out$spfile]

dim(out); length(unique(out$spfile))
dim(tax); length(unique(tax$spfile)); length(unique(tax$scientificName))

out <- merge(out, tax, by = "spfile")
out <- out[,c(10:8, 2:7, 1)]

## Save output table
setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, "species_polygon_fireoverlap.csv"), row.names = FALSE)


## Add EOO and AOO information from pecies_EOO_AOO_ahullareas.csv ####
polyareas <- fread(file.path(output_dir, "species_EOO_AOO_ahullareas.csv"))
polyareas <- setDT(polyareas, key = "spfile")[spfile %in% out$spfile][, .(spfile, EOO, AOO, Nbe_unique_occ., scientificName)]

dim(out); length(unique(out$spfile)); length(unique(out$scientificName))
dim(polyareas); length(unique(polyareas$spfile)); length(unique(polyareas$scientificName))

  # ## Check
  # out2 <- merge(out, polyareas, by = "spfile")
  # all(out2$scientificName.x == out2$scientificName.y)
polyareas[, scientificName := NULL]
out <- merge(out, polyareas, by = "spfile")
write.csv(out, file = file.path(output_dir, "species_polygon_fireoverlap_EOOinfo.csv"), 
          row.names = FALSE)

# ## Remove directories and files ####
unlink(shapefile_dir, recursive = TRUE, force = TRUE)
# file.remove(file.path(overlap_dir, dir(path = overlap_dir)))
# unlink(overlap_dir, recursive = TRUE)


## Summarize outputs ####
message(cat("NA in scientificName: "),
        length(which(is.na(out$scientificName))))

message(cat("Total number of species: "),
        nrow(out))

message(cat("# Species showing 100% fire overlap: "),
        nrow(out[Percent_Overlap == 100]));
nrow(out[Percent_Overlap == 100])/nrow(out)
message(cat("# Species showing 100% fire overlap: "))
print(setorder(out[Percent_Overlap == 100][, .(class,family, scientificName)], class, family, scientificName))

message(cat("# Species showing >= 90% fire overlap: "),
        nrow(out[Percent_Overlap >= 90]));
nrow(out[Percent_Overlap >= 90])/nrow(out)

message(cat("# Species showing >= 50% fire overlap: "),
        nrow(out[Percent_Overlap >= 50]));
round(nrow(out[Percent_Overlap >= 50])/nrow(out), 2)

message(cat("# Species showing no fire overlap: "),
        nrow(out[Percent_Overlap == 0]))
round(nrow(out[Percent_Overlap == 0])/nrow(out), 2)

## High severity overlaps
fire3_overlap <- out$Fire_Class_3/out$Species_Polygon
message(cat("Number of species with high fire severity impact on > = 50% of their range: "),
        length(which(fire3_overlap >= 0.5)))
message(cat("Species with high fire severity impact on > = 50% of their range: "))
out[which(fire3_overlap >= 0.5)]$scientificName
