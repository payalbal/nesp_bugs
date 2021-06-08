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
corr_dir <- file.path(bugs_dir, "data_corrections")
output_dir = file.path(bugs_dir, "outputs")
new_output_dir = file.path(bugs_dir, "outputs", "outputs_for_updated_species_only")

# x <- ""
# list.files(x)
# file.remove(file.path(x, dir(path = x)))
# unlink(x, recursive = TRUE)

## Remove species from old output table ####
dat1 <- fread(file.path(output_dir, "invert_fireoverlap.csv")); dim(dat1)
dat2 <- fread(file.path(corr_dir, "invert_fireoverlap_corrected.csv")); dim(dat2)
all(sort(dat1$spfile) == sort(dat2$spfile))
rm(dat1)

x <- fread(file.path(corr_dir, "delete_species_fromoutputs.csv"))$x; length(x)
nrow(dat2) - nrow(dat2[!spfile %in% x]) == length(x)
dat2 <- dat2[!spfile %in% x]
dat2[, c("marine", "invasive", "name_issues", 
         "delete_sp", "spell_variants", "name_corrections") := NULL]
names(dat2); dim(dat2)

## Add updated species to output table
dat1 <- fread(file.path(new_output_dir, "invert_fireoverlap_updatedsp.csv")); dim(dat1)
ncol(dat1) == ncol(dat2)
dat <- rbind(dat1, dat2); dim(dat)
rm(dat1, dat2)

## Save table ####
names(dat); dim(dat)
length(unique(dat$spfile))
length(unique(dat$scientificName))

setDT(dat, key = "spfile", "scientificName")
write.csv(dat, file = file.path(output_dir, "invert_fireoverlap_v02.csv"), 
          row.names = FALSE)

## Remove marine & exotics ####
exotic <- fread(file.path(corr_dir, "exotics_JM.csv"))$spfile
marine <- fread(file.path(corr_dir, "invert_fireoverlap_v02_marine_JM.csv"))
marine <- marine[, .(spfile, Marine)][Marine == 1]$spfile

dim(dat); dat <- dat[!spfile %in% c(exotic, marine)]; dim(dat)

message(cat("Number of unique species in updated data: "),
        length(unique(dat$spfile)))

rm(exotic, marine)

## Save table ####
names(dat); dim(dat)
length(unique(dat$spfile))
length(unique(dat$scientificName))

setDT(dat, key = "spfile", "scientificName")
write.csv(dat, file = file.path(output_dir, "invert_fireoverlap_v04.csv"), 
          row.names = FALSE)


summary(dat)

message(cat("Total number of invertebrate species: "),
        nrow(dat))

message(cat("Number of species with < 3 unique records: "),
        sum(dat$Occurrence_Points < 3))
message(cat("Number of species without polygons: "),
        sum(is.na(dat$Species_Polygon)))

message(cat("Number of species with >= 3 unique records: "),
        sum(dat$Occurrence_Points >= 3))
message(cat("Number of species with polygons: "),
        sum(!is.na(dat$Species_Polygon)))

## Checks
nrow(dat[Occurrence_Points < 3][!is.na(Species_Polygon)])
nrow(dat[Occurrence_Points >= 3][is.na(Species_Polygon)])

## Species with 3 or more data points but without a polygon
x <- dat[Occurrence_Points >= 3][is.na(Species_Polygon)]$spfile
offext <- fread(file.path(output_dir, "species_offextent.csv"))$x
x[which(!x %in% offext)]
## 303 species with >=3 unique records but without polygons. 

## All but 6 of these spcies are included in the species_offextent.csv indicating these fall off the extent of the fire map, i.e.m on islands not included in the fire map. 

## The 6 remaining species are the same species as those identified previously showing errors, i.e., for which polygons could not be created.

