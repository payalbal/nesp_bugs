## Correcting output table for fire overlap results

## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")
x <- c("data.table")
lapply(x, require, character.only = TRUE)
rm(x)

## File paths and folders
bugs_dir = "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")
expert_data_dir = file.path(output_dir, "outputs_for_expert_data_only")
updated_sp_dir = file.path(bugs_dir, "outputs", "outputs_for_updated_species_only")

## Load data ####
## >> Output table V02 for all species
data <- fread(file.path(output_dir, "invert_fireoverlap_v02.csv"))

## >> Output data for species with expert data
expert <- fread(file.path(output_dir, "outputs_for_expert_data_only", "invert_fireoverlap_expertdata.csv"))


## Update output table ####
## >> Remove species from old output table ####
x <- expert$spfile[expert$spfile %in% data$spfile]
nrow(data) - nrow(data[!spfile %in% x]) == length(x)
dim(data); data <- data[!spfile %in% x]; dim(data)

## >> Add updated species to output table
ncol(data) == ncol(expert)
all(names(data) == names(expert))
data <- rbind(data, expert); dim(data)

## >> Add column to indicate expert data
data$expert_data <- rep(numeric(), nrow(data))
data[spfile %in% expert$spfile]$expert_data = 1
rm(expert)

## >> Save table ####
names(data); dim(data)
length(unique(data$spfile))
length(unique(data$scientificName))

setDT(data, key = "spfile", "scientificName")
setorder(data, "expert_data", na.last = TRUE)
write.csv(data, file = file.path(output_dir, "invert_fireoverlap_v03.csv"), 
          row.names = FALSE)


message(cat("Total number of invertebrate species: "),
        nrow(data))
message(cat("Number of species with < 3 unique records: "),
        sum(data$Occurrence_Points < 3))
message(cat("Number of species without polygons: "),
        sum(is.na(data$Species_Polygon)))

message(cat("Number of species with >= 3 unique records: "),
        sum(data$Occurrence_Points >= 3))
message(cat("Number of species with polygons: "),
        sum(!is.na(data$Species_Polygon)))

## Checks
nrow(data[Occurrence_Points < 3][!is.na(Species_Polygon)])
nrow(data[Occurrence_Points >= 3][is.na(Species_Polygon)])

## Species with 3 or more data points but without a polygon
x <- data[Occurrence_Points >= 3][is.na(Species_Polygon)]$spfile
offext <- fread(file.path(output_dir, "species_offextent.csv"))$x
x[which(!x %in% offext)]
## 303 species with >=3 unique records but without polygons. 

## All but 6 of these spcies are included in the species_offextent.csv indicating these fall off the extent of the fire map, i.e.m on islands not included in the fire map. 

## The 6 remaining species are the same species as those identified previously showing errors, i.e., for which polygons could not be created.

