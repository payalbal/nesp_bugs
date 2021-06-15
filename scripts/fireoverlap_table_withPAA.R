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

poly <- fread(file.path(output_dir, "species_polygon_fireoverlap.csv")); dim(poly)
point <- fread(file.path(output_dir, "species_points_fireoverlap.csv")); dim(point)


## Combine output tables ####
## Merge rows for species with polygon overlap innfo (n = 29029)
out1 <- merge(point, poly, by = "spfile")
dim(out1)

## Get rows for species without polygon informatiom (n = 29948)
sum(!(point$spfile %in% out1$spfile))
out2 <- point[!(point$spfile %in% out1$spfile)]
dim(out2)

## Add empty polygon information columns to out2 
dt <- setNames(data.table(matrix(nrow = nrow(out2), ncol = length(names(poly)[-1]))), names(poly)[-1])
out2 <- cbind(out2, dt)
dim(out2)
rm(dt)

## Check
dim(out1)[1] + dim(out2)[1] == dim(point)[1]
ncol(out1) == ncol(out2)

## Combine both tables
out <- rbind(out1, out2); dim(out)
rm(out1, out2, point, poly)
setDT(out, key = "spfile")

## Total records within PAA
grep("paa", names(out), value = TRUE)
num <- grep("paa", names(out))
out$PAA_Points <- rowSums(out[, ..num])
nrow(out[PAA_Points == 0])

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

## Check if total points in PAA = 0, the fire overlap is also = 0
nrow(out[PAA_Points == 0])
all(out[PAA_Points == 0]$Total_fire_points == 0)

## Check if total polygon area in PAA = 0, the fire overlap is also = 0
nrow(out[PAA_Points == 0 & !is.na(Total_fire_polygon)])
all(out[PAA_Points == 0]$Total_fire_polygon == 0, na.rm = TRUE)

all(out[PAA_Points == 0 & !is.na(Total_fire_polygon)]$Species_Polygon != 0)
nrow(out[PAA_Points == 0 & Species_Polygon != 0])

out[PAA_Points == 0 & Species_Polygon != 0][,.(spfile, Total_fire_points, Occurrence_Points, Total_fire_polygon, Species_Polygon, EOO, PAA_Points)]
  ## species polygon does not overlap with fire, but might not be zero because it is within the PAA extent. These aren't a problem. 

## Checks on merged table
sum(is.na(out$Species_Polygon))
sum(!is.na(out$Species_Polygon))

## Reorder columns
names(out)[c(1:10, 11:13, 56, 14, 46:50, 52:54, 57, 51, 16, 15, 17:45, 55)]
out <- out[, c(1:10, 11:13, 56, 14, 46:50, 52:54, 57, 51, 16, 15, 17:45, 55)]

## Save output table
setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, paste0("invert_overlap_", Sys.Date(), ".csv")), 
          row.names = FALSE)



## Combining trait table: Fire_impacted_invert_traits_09.06.csv ####
## Read in trait table
trait <- as.data.table(fread(file.path(bugs_dir, "JM_traits", "Fire_impacted_invert_traits_09.06.csv")))
setDT(trait, key = "spfile")

## Move spfile column to the front
grep("spfile", names(trait))
trait <- trait[, c(8, 1:7, 9:173)]

## Check duplicates in spfile
trait_sp <- unique(trait$spfile)
length(trait_sp)
sum(duplicated(trait))
sum(duplicated(trait$spfile))

nrow(trait) == length(trait_sp) + sum(duplicated(trait$spfile))

trait[duplicated(trait$spfile)]$spfile
nrow(trait[grep("#N/A", trait$spfile)])
  ## NAs found need to be reatined and stiched at end of the table
trait[grep("#N/A", trait$spfile)]$scientificName
trait[grep("#N/A", trait$spfile)]$scientificName %in% out$scientificName
  ## The corresponding scientific name isn't foud in our output table


## Check duplicates in scientificName
length(unique(trait$scientificName))
sum(duplicated(trait$scientificName))
trait[duplicated(trait$scientificName)]$scientificName
trait[scientificName == trait[duplicated(trait$scientificName)]$scientificName]$spfile
  ## can be ignored as spfile is different

## Check spfile not found
length(trait_sp)
all(trait_sp %in% out$spfile)
trait_sp[which(!(trait_sp %in% out$spfile))]
  ## Only NAs, these will need to be added at the end of merging

  # ## Check against data, shows the same
  # data <- fread(file.path(output_dir, "data_ALAnonALA_wgs84_corrected.csv"))
  # data_sp <- unique(data$spfile)
  # length(data_sp)
  # all(trait_sp %in% data_sp)
  # trait_sp[which(!(trait_sp %in% data_sp))]

## Combine output and trait table
dim(out)
dim(trait)

trait1 <- merge(trait, out, by = "spfile"); dim(trait1)

## Get rows from trait table where spfile = "#N/A" (23 rows)
trait2 <- trait[grep("#N/A", trait$spfile)]; dim(trait2)

## Add empty columns to trait2
dt <- setNames(data.table(matrix(nrow = nrow(trait2), ncol = length(names(out)[-1]))), names(out)[-1])
trait2 <- cbind(trait2, dt)
dim(trait2)
rm(dt)

## Check
dim(trait1)[1] + dim(trait2)[1] == dim(trait)[1]
ncol(trait1) == ncol(trait2)

## Combine both tables
names(trait1) == names(trait2)
names(trait1)[!names(trait1) == names(trait2)]
names(trait2)[!names(trait1) == names(trait2)]
names(trait2) = names(trait1)

trait <- rbind(trait1, trait2); dim(trait)
setDT(trait, key = "spfile")

## Save table
names(trait)[grep("\\.x", names(trait))] <- 
  gsub("\\.x$", "_OLD", names(trait)[grep("\\.x", names(trait))])
names(trait)[grep("\\.y", names(trait))] <- 
  gsub("\\.y$", "_NEW", names(trait)[grep("\\.y", names(trait))])
write.csv(trait, file = file.path(output_dir, 
                                paste0("invert_overlap_traits_", 
                                       Sys.Date(), ".csv")), 
          row.names = FALSE)

  # ## Checks
  # names(out)[grep("_OLD|_NEW", names(out))]
  # 
  # all(out$scientificName_NEW == out$scientificName_OLD)
  # which(out$scientificName_NEW != out$scientificName_OLD)
  # out[out$scientificName_NEW != out$scientificName_OLD][, .(scientificName_OLD, scientificName_NEW)]
  # 
  # all(out$order_NEW == out$order_OLD)
  # sum(out$order_NEW != out$order_OLD, na.rm = TRUE)
  # out[out$order_NEW != out$order_OLD][, .(order_OLD, order_NEW)]
  # 
  # ## Save taxonomic information table
  # out <- fread(file.path(output_dir, "invert_overlap_traits_2021-06-11.csv"))
  # names(out)[grep("_OLD|_NEW", names(out))]
  # 
  # x <- out[!duplicated(data[,.(spfile, scientificName, class, order, family)])][, .(spfile, scientificName, class, order, family)]


## Save table truncated to species with > 0 records in PAA ####
## Check species with Species_Polygon == 0
message(cat("Number of species off PAA extent (removed from outputs): "),
        nrow(out[PAA_Points == 0]))
dim(out); out <- out[PAA_Points > 0]; dim(out)
setDT(out, key = "spfile")
write.csv(out, file = file.path(output_dir, 
                                paste0("invert_overlap_PAAonly_", 
                                       Sys.Date(), ".csv")), 
          row.names = FALSE)
