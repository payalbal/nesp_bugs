## Summarise alpha hull overlap

## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")
x <- c("data.table")
lapply(x, require, character.only = TRUE)
rm(x)

## File paths and folders
bugs_data = "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_data, "outputs")


## Alpha hull polygons
out <- fread(file.path(output_dir, "ala_polygons_areas_alphahull.csv"))

message(cat("#species with EOOs: "),
        nrow(out[!is.na(EOO)]))
message(cat("#species without EOOs: "),
        nrow(out[is.na(EOO)]))
message(cat("max #records for species without EOOs: "),
        max(out[is.na(EOO)]$Nbe_unique_occ., na.rm = TRUE))

## PROBLEM HERE >> shouldn't need to=he na.rm = TRUE..
out[which(is.na(out[is.na(EOO)]$Nbe_unique_occ.)),]


## EOO overlaps
out <- fread(file.path(output_dir, "EOO_fireoverlap_alphahull.csv"))

message(cat("NA in SpeciesName: "),
        length(which(is.na(out$SpeciesName))))

message(cat("Total number of species: "),
        nrow(out))

message(cat("Proportion of species showing (any) overlap: "),
        round(nrow(out[Total_Overlap > 0])/nrow(out), 3))

message(cat("Proportion of species showing no fire overlap: "),
        round(nrow(out[Total_Overlap == 0])/nrow(out), 4))

message(cat("Number of species showing 90% or more fire overlap: "),
        sum(out$Total_Overlap/out$Species_Polygon >= 0.9, na.rm = TRUE))

message(cat("Species showing 90% or more fire overlap: "))
out[which(out$Total_Overlap/out$Species_Polygon >= 0.9),]

message(cat("Proportion of species showing 50% or more fire overlap: "),
        sum(out$Total_Overlap/out$Species_Polygon >= 0.5, na.rm = TRUE)/nrow(out))

message(cat("Species showing 50% or more fire overlap: "))
out[which(out$Total_Overlap/out$Species_Polygon >= 0.5),]

message(cat("Proportion of species showing overlap by high fire severity: "),
        sum(out$Fire_Class_3/out$Total_Overlap >= 0.5, na.rm = TRUE)/nrow(out))

message(cat("Proportion of species showing overlap by high fire severity: "),
        sum(out$Fire_Class_2/out$Total_Overlap >= 0.5, na.rm = TRUE)/nrow(out))

message(cat("Proportion of species showing overlap by low fire severity: "),
        sum(out$Fire_Class_1/out$Total_Overlap >= 0.5, na.rm = TRUE)/nrow(out))




## Point overlaps
out <- fread(file.path(output_dir, "Points_fireoverlap_alphahull.csv"))

## Summarize outputs ####
message(cat("NA in SpeciesName: "),
        length(which(is.na(out$SpeciesName))))

message(cat("Total number of species: "),
        nrow(out))

message(cat("Number of species showing overlap: "))
out[, .N, by = Total_Overlap]

message(cat("Proportion of species showing overlap: "))
x <- out[, .N, by = Total_Overlap]
x$N/nrow(out)

message(cat("Proportion of species showing no fire overlap: "),
        round(nrow(out[Total_Overlap == 0])/nrow(out), 4))

message(cat("Proportion of species showing 100% fire overlap: "),
        round(nrow(out[Total_Overlap == Occurrence_Points])/nrow(out), 4))

message(cat("Proportion of species showing 50% fire overlap: "),
        round(nrow(out[Total_Overlap == (Occurrence_Points/2)])/nrow(out), 4))

        