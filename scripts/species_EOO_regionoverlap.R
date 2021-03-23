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

overlap_dir = file.path(output_dir, "polygon_region_overlap")
if(!dir.exists(overlap_dir)){dir.create(overlap_dir)}

source("/tempdata/workdir/nesp_bugs/scripts/polygon_region_overlap.R")


## Overlaps for species WITH EOO polygons  ####
## >> Load in spdf data for species with EOO ####
species_maps <- readRDS(file.path(output_dir, "species_ahullEOOspdf.rds"))
polygon_list <- names(species_maps)



## >> Load in region rasters and get unique classes ####
## >>>> State raster (based on GEOCAOST 100K)
states <- raster(file.path(output_dir, ".tif"))
state_vals <- states[]
state_classes <- sort(unique(na.omit(state_vals)))

## >>>> Bushfire recovery regions raster
recovery.regions <- raster(file.path(output_dir, ".tif"))
recovery_vals <- recovery.regions[]
recovery_classes <- sort(unique(na.omit(recovery_vals)))

## >>>> NRM regions raster
nrm.regions <- raster(file.path(output_dir, ".tif"))
nrm_vals <- nrm.regions[]
nrm_classes <- sort(unique(na.omit(nrm_vals)))


## >> Run overlap analysis in parallel: doMC ####
## 'log' only useful when running small number of species
registerDoMC(future::availableCores()-2)
system.time(foreach(polys = polygon_list,
                    .combine = rbind,
                    .errorhandling = "pass",
                    .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                      
                      polygon_region_overlap(species_name = polys, # polys = polygon_list[335]
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


## Reruns ####
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
                                    
                                    polygon_region_overlap(species_name = polys,
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



## Output table ####
## Merge csv files
out <- do.call("rbind", lapply(csvfiles, fread))
message(cat("Check for NAs: "),
        sum(is.na(out)))
which(is.na(out), arr.ind=TRUE)

error_list <- csvnames[!csvnames %in% out$Species]

error_files <- csvfiles[!csvnames %in% out$Species]
names(out)[1] <- "spfile"
setDT(out, key = "spfile")

## Add total and percentage overlap
out$Total_Pverlap <- #rowSums(df[, -c(1:2, (ncol(df)-2):ncol(df))])
  out$Percent_Overlap <- #(df[ , ncol(df)-2]/(df[ , ncol(df)-1] - df[, ncol(df)-5]))*100
  
  message(cat("Check if area overlapping with fire is always <= Total polygonn area for species: "),
          all(out$Total_Overlap <= out$Species_Polygon))

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
write.csv(out, file = file.path(output_dir, "species_polygon_fireoverlap.csv"), row.names = FALSE)


# ## Remove files ####
# file.remove(file.path(overlap_dir, dir(path = overlap_dir)))
# unlink(shapefile_dir, recursive = TRUE, force = TRUE)
# unlink(overlap_dir, recursive = TRUE)


## Summarize outputs ####
message(cat("NA in scientificName: "),
        length(which(is.na(out$scientificName))))

message(cat("Total number of species: "),
        nrow(out))

message(cat("Number of species showing overlap: "))
out[, .N, by = Total_Overlap]

message(cat("# Species showing 100% fire overlap: "),
        nrow(out[Percent_Overlap == 100]));
round(nrow(out[Percent_Overlap == 100])/nrow(out), 2)
message(cat("# Species showing 100% fire overlap: "))
print(setorder(out[Percent_Overlap == 100][, .(class,family, scientificName)], class, family, scientificName))

message(cat("# Species showing >= 90% fire overlap: "),
        nrow(out[Percent_Overlap >= 90]));
nrow(out[Percent_Overlap >= 90])/nrow(out)

message(cat("# Species showing >= 50% fire overlap: "),
        nrow(out[Percent_Overlap >= 50]));
round(nrow(out[Percent_Overlap >= 50])/nrow(out), 2)

message(cat("# Species showing < 50% and > 30% fire overlap: "),
        nrow(out[Percent_Overlap < 50 & Percent_Overlap > 30]));
round(nrow(out[Percent_Overlap < 50 & Percent_Overlap > 30])/nrow(out), 2)      

message(cat("# Species showing <= 30% fire overlap: "),
        nrow(out[Percent_Overlap <= 30]));
round(nrow(out[Percent_Overlap <= 30])/nrow(out), 2)

message(cat("# Species showing no fire overlap: "),
        nrow(out[Percent_Overlap == 0]))
round(nrow(out[Percent_Overlap == 0])/nrow(out), 2)



