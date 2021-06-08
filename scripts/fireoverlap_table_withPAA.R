## Combinign fire overlap results


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")
x <- c("data.table", "readxl")
lapply(x, require, character.only = TRUE)
rm(x)

## File paths and folders
bugs_dir = "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")
corr_dir <- file.path(bugs_dir, "data_corrections")

poly <- fread(file.path(output_dir, "species_polygon_fireoverlap.csv"))
point <- fread(file.path(output_dir, "species_points_fireoverlap.csv"))


## Combine output tables ####
## Merge rows for species with polygon overlap innfo (n = 29029)
out1 <- merge(point, poly, by = "spfile")
dim(out1)

## Get rows for species without polygon informatiom (n = 29948)
sum(!(point$spfile %in% out1$spfile))
out2 <- point[!(point$spfile %in% out1$spfile)]
dim(out2)

## Add empty polygon information columns to out2 
dt <- setNames(data.table(matrix(nrow = nrow(out2), ncol = length(names(poly)[2:10]))), names(poly)[2:10])
out2 <- cbind(out2, dt)
dim(out2)
rm(dt)

## Check
dim(out1)[1] + dim(out2)[1] == dim(point)[1]
ncol(out1) == ncol(out2)

## Combine both tables
out <- rbind(out1, out2)
rm(out1, out2, point, poly)

## Checks on merged table
sum(is.na(out$Species_Polygon))
sum(!is.na(out$Species_Polygon))

## Reorder columns
names(out)[c(1:14, 36:40, 42:44, 41, 15:35)]
out <- out[, c(1:14, 36:40, 42:44, 41, 15:35)]
setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, "invert_fire_paa_overlap.csv"), 
          row.names = FALSE)