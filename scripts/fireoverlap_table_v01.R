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
write.csv(out, file = file.path(output_dir, "invert_fireoverlap.csv"), 
          row.names = FALSE)


## Add column and info for marine species ####
## Marine.csv provided by JM
out$marine <- rep(character(), nrow(out))
temp <- fread(file.path(corr_dir, "Marine.csv"))
out[out$spfile %in% temp$spfile]$marine = 1


## Add column and info for invasive species ####
out$invasive <- rep(character(), nrow(out))

## Check against GRIIS
temp <- fread(file.path(
  bugs_dir, "ALA",
  "GRIIS_Global_Register_of_Introduced_and_Invasive_Species_Australia.csv"))
sum(out$scientificName %in% temp$`Supplied Name`)
sum(out$scientificName %in% temp$scientificName)



## Add columns and info for naming issues ####
## >> Incomplete/improper names using remove_improper_names.R function ####
out$name_issues <- rep(character(), nrow(out))

source("/tempdata/workdir/nesp_bugs/scripts/remove_improper_names.R")

temp <- remove_improper_names(as.character(out$scientificName),
                                        allow.higher.taxa = FALSE,
                                        allow.subspecies = TRUE)
message(cat("Improper species names found: "),
        length(temp$improper_species))
message(cat("Incomplete species names found: "),
        length(temp$incomplete_species))
message(cat("NAs in cleaned ALA species list: "),
        sum(is.na(temp$updated_list)))
message(cat("Duplicates in cleaned ALA species list: "),
        length(temp$updated_list[duplicated(temp$updated_list)]))

setDT(out, key = c("spfile", "scientificName"))
temp <- c(temp$improper_species, temp$incomplete_species)
for (sp in temp){
  out[scientificName == sp]$name_issues  = 1
}

length(temp)
sum(out$name_issues == 1, na.rm = TRUE)


## >> Incomplete names identified by JW ####
##  NOTE: '1' in column dubious in species_polygon_fireoverlap_allinfo_jw_3_dubious.csv
out$delete_sp <- rep(character(), nrow(out))
temp <- fread(file.path(
  corr_dir, "species_polygon_fireoverlap_allinfo_jw_3_dubious.csv"))
temp <- temp[, c(1:3)]
temp <- temp[dubious == 1]

for (sp in temp$spfile){
  out[spfile == sp]$delete_sp  = 1
}

length(temp$spfile)
sum(out$delete_sp == 1, na.rm = TRUE)


## >> Spelling variantes identified by JW ####
## NOTE: 'x' in column dubious in species_polygon_fireoverlap_allinfo_jw_3_dubious.csv
out$spell_variants <- rep(character(), nrow(out))

temp <- fread(file.path(
  corr_dir, "species_polygon_fireoverlap_allinfo_jw_3_dubious.csv"))
temp <- temp[, c(1:3)]
temp <- temp[dubious == "x"]

for (sp in temp$spfile){
  out[spfile == sp]$spell_variants  = 1
}

length(temp$spfile)
sum(out$spell_variants == 1, na.rm = TRUE)


## >> Column for corrected name text [to be populated by JW & JM] ####
out$name_corrections <- rep(character(), nrow(out))


## Save table ####
names(out)
setDT(out, key = "spfile", "scientificName")
write.csv(out, file = file.path(output_dir, "invert_fireoverlap_v01.csv"), 
          row.names = FALSE)
summary(out)
