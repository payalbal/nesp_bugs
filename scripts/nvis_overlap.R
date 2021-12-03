## NVIS
## Source: https://www.environment.gov.au/land/native-vegetation/national-vegetation-information-system/data-products

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

if(!dir.exists(file.path(bugs_dir, "outputs", "native_vegetation"))){
  dir.create(file.path(bugs_dir, "outputs", "native_vegetation"))
}

if(!dir.exists(file.path(bugs_dir, "outputs", "native_vegetation", "fire_overlap"))){
  dir.create(file.path(bugs_dir, "outputs", "native_vegetation", "fire_overlap"))
}

source(file.path("/tempdata/workdir/nesp_bugs/", "scripts", "gdal_calc.R"))  # by jgarber


## >> Load mask ####
ausmask <- raster(file.path(output_dir, "masks", "ausmask_noaa_250mAlbersEA_NA.tif"))
aus.crs <- crs(ausmask)
aus.res <- res(ausmask)


## >> Load in fire severity raster and get unique classes ####
fire_severity <- raster(file.path(output_dir, "fire", "severity5_eqar250_native_paa.tif"))
fire_vals <- fire_severity[]
fire_classes <- sort(unique(na.omit(fire_vals)))
fire_res <- res(fire_severity)
fire_crs <- as.character(crs(fire_severity))
fire_extent <- extent(fire_severity)


## >> NVIS major vegetationn group names ####
mvg <- as.data.table(read_excel(file.path(bugs_dir, "native_vegetation", "NVIScategories_jw.xlsx"), sheet = "table"))

mvg <- mvg[,1:2]
names(mvg) <- gsub(" ", "_", names(mvg))
mvg$NVIS_class <- paste0("nvis_", mvg$NVIS_class)
setDT(mvg, key = "NVIS_class")



## Prepare NVIS layer ####
## >> Reproject according to Australia mask ####
infile <- file.path(bugs_dir, "native_vegetation", "GRID_NVIS6_0_AUST_EXT_MVG/aus6_0e_mvg/z001001.adf")
outfile <- file.path(output_dir, "native_vegetation", "nvis_all.tif")

system(paste0("gdalwarp -overwrite -ot Byte -tr ", 
              paste(aus.res, collapse = " "),
              " -t_srs '", aus.crs, "' ",
              infile, " ", outfile))
gdalUtils::gdalinfo(outfile)

raster(outfile)
nvis_classes <- sort(unique(na.omit(raster(outfile))))
nvis_classes
nvis.extent <- extent(raster(outfile))


## >> Clip raster with Preliminary analysis area shapefile ####
infile <- outfile
outfile <- gsub(".tif", "_paa.tif", infile)

paa_file <- file.path(output_dir, "fire", "prelim_analysis_areas_dissolve_eqar.shp") 

system(paste0("gdalwarp -overwrite ", "-te ", 
              paste(nvis.extent[1], nvis.extent[3], 
                    nvis.extent[2], nvis.extent[4]), 
              " -of GTiff -cutline ", paa_file, 
              " -cl prelim_analysis_areas_dissolve_eqar ",
              infile, " ", outfile))

gdalinfo(outfile)
raster(outfile)



## Calculate fire overlap for each NVIS class in PAA #### 
## >> Create separate raster for each NVIS class ####
nvis_classes <- nvis_classes[-grep("25|28|99", nvis_classes)]

infile <- file.path(output_dir, "native_vegetation", "nvis_all_paa.tif")

for (x in nvis_classes){
  outfile <- gsub("all", x, infile)
  system(paste0("gdal_calc.py -A ", infile,
                " --calc='(A==", x, ")*1 + (", 
                paste0("(A==", nvis_classes[-x], ")", collapse = " + "), ")*0' --NoDataValue=0",
                " --outfile=", outfile))
}


## >> Create overlap function ####
nvis_overlap <- function(nvis_infile, fire_res, fire_crs, fire_extent, fire_vals, fire_classes, outdir){
  
  ## Create table of areas within each fire class
  nvis_name <- gsub("_paa", "", basename(tools::file_path_sans_ext(nvis_infile)))
  nvis_map <- raster(nvis_infile)
  dt <- data.table("nvis_map" = nvis_map[],
                   "fire_severity" = fire_vals)
  
  df <- data.frame(matrix(ncol = length(fire_classes) + 2))
  colnames(df) <- c("NVIS_class", paste0("Fire_Class_", fire_classes), 
                    "Total_Area")
  df[, 1] <- nvis_name
  df[, 2:(ncol(df)-1)] <- sapply(fire_classes, FUN = function(x) dt[nvis_map == 1 & fire_severity == x, length(fire_severity) * 250 * 250 / 1000000])
  df[, ncol(df)] <- dt[nvis_map == 1, length(nvis_map)* 250 * 250 / 1000000]
  
  
  ## Save output as csv
  write.csv(df, file = paste0(file.path(outdir, nvis_name), ".csv"), row.names = FALSE)
}


# >> Run overlap analysis in parallel: doMC ####
nvis_files <- list.files(file.path(output_dir, "native_vegetation"), 
                         pattern = "nvis_+\\d", 
                         full.names = TRUE)
overlap_dir <- file.path(bugs_dir, "outputs", "native_vegetation", "fire_overlap")

registerDoMC(length(nvis_files))
system.time(foreach(i = nvis_files,
                    .combine = rbind,
                    .errorhandling = "pass",
                    .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                      
                      nvis_overlap(nvis_infile = i,
                                   fire_res = fire_res,
                                   fire_crs = fire_crs,
                                   fire_extent = fire_extent,
                                   fire_vals = fire_vals,
                                   fire_classes = fire_classes,
                                   outdir = overlap_dir)
                    })


## >> Output table ####
## Merge csv files
csvfiles <- list.files(overlap_dir, pattern = ".csv$",
                       full.names = TRUE, all.files = TRUE)
out <- do.call("rbind", lapply(csvfiles, fread)); dim(out)
out$High_Severity_Area <- out$Fire_Class_4 + out$Fire_Class_5

## Add MVG names and save table
setDT(out, key = "NVIS_class")
out <- merge(out, mvg, by = "NVIS_class")
write.csv(out, file = file.path(output_dir, "native_vegetation",
                                "nvis_classes_fireoverlap.csv"), 
          row.names = FALSE)

## Checks
## >> Total overlapped area should be <= Species Polygon
message(cat("Check if area overlapping with fire is always <= Total area: "),
        all(rowSums(out[,.(Fire_Class_1, Fire_Class_2, Fire_Class_3, Fire_Class_4, Fire_Class_5)]) <= out$Total_Area))

## >> Look for NAs in table - shoudln't be any
message(cat("Check for NAs: "),
        sum(is.na(out)))




## Calculate area in each NVIS class in Australia ####
infile <- file.path(output_dir, "native_vegetation", "nvis_all.tif")

## Create table of areas within each fire class
nvis_name <- paste0("nvis_", nvis_classes)
nvis_map <- raster(infile)
dt <- data.table("nvis_map" = nvis_map[])
df <- data.frame(matrix(nrow = length(nvis_classes)))
rownames(df) <- nvis_name
df[1:nrow(df),] <- sapply(nvis_classes, FUN = function(x) dt[nvis_map == x, length(nvis_map) * 250 * 250 / 1000000])
colnames(df) <- "Area_in_AUS"
df$NVIS_class <- rownames(df)
df <- df[,c(2,1)]


## Add MVG names and save output
setDT(df, key = "NVIS_class")
df <- merge(df, mvg, by = "NVIS_class")
write.csv(df, file = file.path(output_dir, "native_vegetation",
                               "nvis_classes_ausareas.csv"), 
          row.names = FALSE)







