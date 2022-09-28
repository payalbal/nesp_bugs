## NRM - fire overlap


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "sp", "raster", "rgdal", 
       "gdalUtils", "rgeos", "usethis",
       "doMC", "foreach", 
       "readxl")
lapply(x, require, character.only = TRUE)
rm(x)


## >> File paths ####
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")

if(!dir.exists(file.path(bugs_dir, "outputs", "nrm_regions"))){
  dir.create(file.path(bugs_dir, "outputs", "nrm_regions"))
}

if(!dir.exists(file.path(bugs_dir, "outputs", "nrm_regions", "fire_overlap"))){
  dir.create(file.path(bugs_dir, "outputs", "nrm_regions", "fire_overlap"))
}

source(file.path("/tempdata/workdir/nesp_bugs/", "scripts", "gdal_calc.R"))  # by jgarber


## >> Load in fire severity raster and get unique classes ####
fire_severity <- raster(file.path(output_dir, "fire", "severity5_eqar250_native_paa.tif"))
fire_vals <- fire_severity[]
fire_classes <- sort(unique(na.omit(fire_vals)))
fire_res <- res(fire_severity)
fire_crs <- as.character(crs(fire_severity))
fire_extent <- extent(fire_severity)


## >> NRM names ####
## Create csv from attribute table in QGIS:
## Right click layer > export > Save FEature as > csv
nrm_names <- fread(file.path(bugs_dir, "NRM_regions_2020", "nrm_regionnames.csv"))

nrm_names <- nrm_names[,c(1,6,8)]
nrm_names$NRM_class <- paste0("nrm_region", nrm_names$id)
nrm_names[,id := NULL]
names(nrm_names)[2] <- c("NRM_AREA")
setDT(nrm_names, key = "NRM_class")

## Prepare NRM layer ####
## >> Add id column in shapefile attribite table ####
# In QGIS: Shapefile > Open attribute table > Toggle editing on > Open field calculator > Create new field; output field name: id; double click on ‘row_number’ (because #rows=#unqiuw values) > OK > Toggle editing off > Save changes

## >> Rasterize layer ####
infile <- file.path(bugs_dir, "NRM_regions_2020", "NRM_regions_2020.shp")
outfile <- file.path(output_dir, "nrm_regions", "nrm_regions.tif")

system(sprintf("gdal_rasterize -at -a id -ot Byte -tr .0025 .0025 -l NRM_regions_2020 %s %s",
              infile, outfile))

raster(outfile)
sort(unique(na.omit(raster(outfile))))

## >> Reproject according to fire layer as mask ####
infile <- outfile
outfile <- gsub(".tif", "_reproj.tif", infile)

system(sprintf("gdalwarp -overwrite -ot Byte -tr %s -t_srs '%s' %s %s", 
               paste(fire_res, collapse = " "),
               fire_crs, infile, outfile))
gdalUtils::gdalinfo(infile)
gdalUtils::gdalinfo(outfile)

raster(outfile)
nrm_classes <- sort(unique(na.omit(raster(outfile))))
nrm_classes


## >> Clip raster with Preliminary analysis area shapefile ####
nrm.extent <- extent(raster(outfile))
infile <- outfile
outfile <- gsub(".tif", "_paa.tif", infile)

paa_file <- file.path(output_dir, "fire", "prelim_analysis_areas_dissolve_eqar.shp") 


system(paste0("gdalwarp -overwrite ", "-te ", 
              paste(nrm.extent[1], nrm.extent[3], 
                    nrm.extent[2], nrm.extent[4]), 
              " -of GTiff -cutline ", paa_file, 
              " -cl prelim_analysis_areas_dissolve_eqar ",
              infile, " ", outfile))

gdalinfo(outfile)
raster(outfile)


## Calculate fire overlap for each NRM class in PAA #### 
## >> Create separate raster for each NRM class ####
infile <- outfile

nrm_classes <- sort(unique(na.omit(raster(infile))))
nrm_classes
nrm_classes <- nrm_classes[-grep("0", nrm_classes)]

for (x in nrm_classes){
  outfile <- gsub("_reproj_paa", x, infile)
  system(paste0("gdal_calc.py -A ", infile,
                " --calc='(A==", x, ")*1 + (", 
                paste0("(A==", nrm_classes[-x], ")", collapse = " + "), ")*0' --NoDataValue=0",
                " --outfile=", outfile))
}


## >> Create overlap function: area ####
layer_overlap <- function(infile, fire_res, fire_crs, fire_extent, fire_vals, fire_classes, outdir){
  
  ## Create table of areas within each fire class
  class_name <- gsub("regions", "region", basename(tools::file_path_sans_ext(infile)))
  nrm_map <- raster(infile)
  dt <- data.table("nrm_map" = nrm_map[],
                   "fire_severity" = fire_vals)
  
  df <- data.frame(matrix(ncol = length(fire_classes) + 2))
  colnames(df) <- c("NRM_class", paste0("Fire_Class_", fire_classes), 
                    "Total_Area")
  df[, 1] <- class_name
  df[, 2:(ncol(df)-1)] <- sapply(fire_classes, FUN = function(x) dt[nrm_map == 1 & fire_severity == x, length(fire_severity) * 250 * 250 / 1000000])
  df[, ncol(df)] <- dt[nrm_map == 1, length(nrm_map)* 250 * 250 / 1000000]
  
  
  ## Save output as csv
  write.csv(df, file = paste0(file.path(outdir, class_name), ".csv"), row.names = FALSE)
}


## >> Run overlap analysis in parallel: doMC ####
nrm_files <- list.files(file.path(output_dir, "nrm_regions"), 
                         pattern = "nrm_regions+\\d", 
                         full.names = TRUE)
overlap_dir <- file.path(bugs_dir, "outputs", "nrm_regions", "fire_overlap")

registerDoMC(length(nrm_files))
foreach(i = nrm_files,
        .combine = rbind,
        .errorhandling = "pass",
        .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
          
          layer_overlap(infile = i,
                        fire_res = fire_res,
                        fire_crs = fire_crs,
                        fire_extent = fire_extent,
                        fire_vals = fire_vals,
                        fire_classes = fire_classes,
                        outdir = overlap_dir)
        }


## >> Error checking and reruns ####
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
length(csvfiles)
length(nrm_files)
nrm_files <- nrm_files[!gsub("regions", "region", basename(tools::file_path_sans_ext(nrm_files))) %in% basename(tools::file_path_sans_ext(csvfiles))]
registerDoMC(length(nrm_files))
foreach(i = nrm_files,
        .combine = rbind,
        .errorhandling = "pass",
        .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
          
          layer_overlap(infile = i,
                        fire_res = fire_res,
                        fire_crs = fire_crs,
                        fire_extent = fire_extent,
                        fire_vals = fire_vals,
                        fire_classes = fire_classes,
                        outdir = overlap_dir)
        }


## >> Output table ####
## Merge csv files
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
out <- do.call("rbind", lapply(csvfiles, fread)); dim(out)
out$High_Severity_Area <- out$Fire_Class_4 + out$Fire_Class_5

## Add NRM names and save table
setDT(out, key = "NRM_class")
out <- merge(out, nrm_names, by = "NRM_class")
names(out)[7] <- "Total_Fire_Area"
write.csv(out, file = file.path(output_dir, "nrm_regions",
                                "nrm_regions_fireoverlap.csv"), 
          row.names = FALSE)


## Checks
## >> Total overlapped area should be <= Species Polygon
message(cat("Check if area overlapping with fire is always <= Total area: "),
        all(rowSums(out[,.(Fire_Class_1, Fire_Class_2, Fire_Class_3, Fire_Class_4, Fire_Class_5)]) <= out$Total_Area))

## >> Look for NAs in table - shoudln't be any
message(cat("Check for NAs: "),
        sum(is.na(out)))




## Calculate area in each NRM class in Australia ####
infile <- file.path(output_dir, "nrm_regions", "nrm_all.tif")

## Create table of areas within each fire class
class_name <- paste0("nrm_", nrm_classes)
nrm_map <- raster(infile)
dt <- data.table("nrm_map" = nrm_map[])
df <- data.frame(matrix(nrow = length(nrm_classes)))
rownames(df) <- class_name
df[1:nrow(df),] <- sapply(nrm_classes, FUN = function(x) dt[nrm_map == x, length(nrm_map) * 250 * 250 / 1000000])
colnames(df) <- "Area_in_AUS"
df$NRM_class <- rownames(df)
df <- df[,c(2,1)]


## Add NRM names and save output
setDT(df, key = "NRM_class")
df <- merge(df, nrm_names, by = "NRM_class")
write.csv(df, file = file.path(output_dir, "nrm_regions",
                               "nrm_classes_ausareas.csv"), 
          row.names = FALSE)







