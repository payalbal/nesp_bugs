## Data corrections for ALA-nonALA data

## Set working environment ####
## _______________________________________________

rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "rje", "stringr", 
       "sp", "raster", "sf", "lubridate", 
       "future", "future.apply")
lapply(x, require, character.only = TRUE)
rm(x)


## Files and folder
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")
corr_dir = file.path(bugs_dir, "data_corrections")




## Species names A - L ####
temp <- fread(file.path(corr_dir, "invert_fireoverlap_ALnameissues_JRM.csv"))
setDT(temp, key = "spfile")
dim(temp)


## >> Subset table to A - L (upto 32981 when arranged by spfile) ####
temp[32981]$spfile
temp[32982]$spfile

temp <- temp[1:32981, ]
dim(temp)


## >> Quality checks ####
temp[, .N, delete_sp]
temp[, .N, name_issues]
temp[, .N, spell_variants]
nrow(temp[!is.na(name_corrections)])
  ## spell_variants and name_corrections should be equal
  ## i.e., for a record if spell_variants = 1, text string should be provided in name_corrections


## >> Find NA in name_corrections ####
nrow(temp[!is.na(spell_variants) & is.na(name_corrections) & is.na(delete_sp)])


## >> Find species with 'group/complex/etc.' in name ####
grep("group|Group|grp.|grp|Grp.|Grp|complex", temp$scientificName, value = TRUE)
  # x <- temp[grep("group|Group|grp.|grp|Grp.|Grp|complex", temp$scientificName)][is.na(delete_sp)][, .(spfile, scientificName, class, order, family, delete_sp, spell_variants, name_corrections)]
  # write.csv(x, file.path(corr_dir, "JM_names_correction1.csv"), row.names = FALSE)

## Assign 1 in delete_sp as per JM_names_correction1_JRM.csv
x <- fread(file.path(corr_dir, "JM_names_correction1_JRM.csv"))
  ## Check
  temp[spfile %in% x[!is.na(delete_sp)]$spfile]$spfile == x[!is.na(delete_sp)]$spfile
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$delete_sp = 1
rm(x)


## >> Find species with '?' in name ####
  # x <- temp[grep("\\?", temp$scientificName)][is.na(delete_sp)][, .(spfile, scientificName, class, order, family, delete_sp, spell_variants, name_corrections)]
  # write.csv(x, file.path(corr_dir, "JM_names_correction2.csv"), row.names = FALSE)

## Assign 1 in delete_sp as per JM_names_correction2_JRM.csv
x <- fread(file.path(corr_dir, "JM_names_correction2_JRM.csv"))
  ## Check
  temp[spfile %in% x[!is.na(delete_sp)]$spfile]$spfile == x[!is.na(delete_sp)]$spfile
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$delete_sp = 1
rm(x)


## >> Save data table
temp1 <- data.table::copy(temp)
rm(temp, x)



## Species names M - Z  ####
temp <- fread(file.path(corr_dir, "invert_fireoverlap_MZnameissues_JW_JM.csv"))
setDT(temp, key = "spfile")
dim(temp)

## >> Subset table to M - Z (from 32982 when arranged by spfile) ####
temp[32981]$spfile
temp[32982]$spfile

temp <- temp[32982:nrow(temp), ]
dim(temp)

## >> Correct delete_sp in JW sheet ####
temp[, .N, delete_sp]
temp[, .N, `JW notes`]
temp[, .N, `JM notes`]
temp[`JM notes` == "remove"]$delete_sp
temp[, `JW notes` := NULL]
temp[, `JM notes` := NULL]

## >> Quality checks ####
temp[, .N, name_issues]
temp[, .N, spell_variants]
nrow(temp[!is.na(name_corrections)])
  ## spell_variants and name_corrections should be equal
  ## i.e., for a record if spell_variants = 1, text string should be provided in name_corrections


## >> Find NA in name_corrections ####
nrow(temp[!is.na(spell_variants) & is.na(name_corrections) & is.na(delete_sp)])
  # x <- temp[!is.na(spell_variants) & is.na(name_corrections) & is.na(delete_sp)][, .(spfile, scientificName, class, order, family, delete_sp, spell_variants, name_corrections)]
  # write.csv(x, file.path(corr_dir, "JW_1spellvariants_NAnamecorrections.csv"), row.names = FALSE)

  # grep("mandjelia_myg438_wanjarri_1969", temp$spfile)
  # temp[grep("mandjelia_myg438_wanjarri_1969", temp$spfile)]
  # grep("mandjelia_myg438_wanjarri_1969", temp$name_corrections)
  # temp[grep("mandjelia_myg438_wanjarri_1969", temp$name_corrections)]

## >> Correct NAs in name_corrections ####
## NOTE: These are records with the correct species name, 
##  so name_corrections column can be populated with spfile
##  associated records with the incorrect species inndicated in name_correctionns already
temp[!is.na(spell_variants) & is.na(name_corrections) & is.na(delete_sp)]$name_corrections = temp[!is.na(spell_variants) & is.na(name_corrections) & is.na(delete_sp)]$spfile

## Check again
nrow(temp[!is.na(spell_variants) & is.na(name_corrections) & is.na(delete_sp)])

## >> Find species with 'group/complex/etc.' in name ####
x <- temp[grep("group|Group|grp.|grp|Grp.|Grp|complex", temp$scientificName)][is.na(delete_sp)][, .(spfile, scientificName, class, order, family, delete_sp, spell_variants, name_corrections)]
write.csv(x, file.path(corr_dir, "JW_names_correction1.csv"), row.names = FALSE)

## Assign 1 in delete_sp as per JW_names_correction1_JRM.csv
x <- fread(file.path(corr_dir, "JW_names_correction1_JRM.csv"))
## Check
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$spfile == x[!is.na(delete_sp)]$spfile
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$delete_sp = 1
rm(x)


## >> Find species with '?' in name ####
x <- temp[grep("\\?", temp$scientificName)][is.na(delete_sp)][, .(spfile, scientificName, class, order, family, delete_sp, spell_variants, name_corrections)]
write.csv(x, file.path(corr_dir, "JW_names_correction2.csv"), row.names = FALSE)

## Assign 1 in delete_sp as per JW_names_correction2_JRM.csv
x <- fread(file.path(corr_dir, "JW_names_correction2_JRM.csv"))
## Check
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$spfile == x[!is.na(delete_sp)]$spfile
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$delete_sp = 1
rm(x)


## >> Save data table
temp2 <- data.table::copy(temp)
rm(temp, x)



## Combine tables ####
temp <- rbind(temp1, temp2)
setDT(temp, key = "spfile")
rm(temp1, temp2)



temp1 <- data.table::copy(temp)

## Check if delete_sp have values in corresponding name correction columns
temp[, .N, delete_sp]
temp[delete_sp == 1][, .N, name_issues]
temp[delete_sp == 1][, .N, name_corrections]
temp[delete_sp == 1][, .N, spell_variants]

## Correcting for mismarked delete_sp acc to name_corrections
temp[delete_sp == 1][, .N, name_corrections]
temp[delete_sp == 1 & !is.na(name_corrections)][, c(spfile, name_corrections)]
grep("kwonkan_goongarriensis_39854", temp$name_corrections)
temp[grep("kwonkan_goongarriensis_39854", temp$name_corrections)]
grep("kwonkan_goongarriensis_39854", temp$spfile) 
temp[grep("kwonkan_goongarriensis_39854", temp$spfile)]$delete_sp = NA
  ## Check again
  temp[delete_sp == 1 & name_issues == 1][, .N, name_corrections]

## Correcting for mismarked delete_sp acc to spell_variants
temp[delete_sp == 1][, .N, spell_variants]
x <- temp[delete_sp == 1 & spell_variants == 1]$spfile
y <- gsub("_\\d+$", "", x)

i <- 1
temp[grep(x[i], temp$spfile)][,.(spfile, scientificName, class, order, family, 
                                 delete_sp, name_issues, spell_variants, name_corrections)]
temp[grep(y[i], temp$spfile)][,.(spfile, scientificName, class, order, family, 
                                  delete_sp, name_issues, spell_variants, name_corrections)]

...


## Get list of dubious species (to be removed from output table/data)
temp <- temp[delete_sp == 1]$spfile
write.csv(temp, file.path(corr_dir, "dubious_species.csv"))
rm(temp)



## List species with name corrections to be updated in output table/data ####
temp[, .N, name_issues]
temp[, .N, spell_variants]
temp[, .N, name_corrections]


## Quality check name_corrections and spell_variants columns
## NOTE: The number of records/rows with values should be the same for the two columns
nrow(temp[!is.na(name_corrections)])
nrow(temp[!is.na(spell_variants)])
nrow(temp[!is.na(name_corrections) & !is.na(spell_variants)])
  ## This is currently not the case, so we need to update the corresponding columns

## >> 1. Correcting when !is.na(name_corrections) & is.na(spell_variants)
nrow(temp[!is.na(name_corrections) & is.na(spell_variants)])
temp[!is.na(name_corrections) & is.na(spell_variants)][, c(spfile, name_corrections)]
temp[grep("protogamasellas_massula_18140", temp$spfile)]$spell_variants = 1
temp[grep("protogamasellus_massula_2501", temp$spfile)]$name_corrections = "protogamasellus_massula_2501"

nrow(temp[!is.na(name_corrections)])
nrow(temp[!is.na(name_corrections) & is.na(spell_variants)])

## >> 2. Correcting name_corrections column
## is.na(name_corrections) & !is.na(spell_variants)
nrow(temp[is.na(name_corrections) & !is.na(spell_variants)])
  ## Check
  nrow(temp[is.na(name_corrections) & !is.na(spell_variants)]) + 
    nrow(temp[!is.na(name_corrections) & !is.na(spell_variants)]) ==
    nrow(temp[!is.na(spell_variants)])

temp[is.na(name_corrections) & !is.na(spell_variants)][,spfile]
temp[grep("yalkara_spp_grp_sp_02_3524", temp$name_corrections)][, c(spfile, name_corrections)]
temp[grep("yalkara_spp_grp_sp_02_3524", temp$spfile)][, c(spfile, name_corrections)]

temp[grep("gaius_villosus_1272", temp$name_corrections)][, c(spfile, name_corrections)]
temp[grep("gaius_villosus_1272", temp$spfile)][, c(spfile, name_corrections)]




## List of marine species to be removed from output table/data ####
temp <- fread(file.path(corr_dir, "invert_fireoverlap_JRM_marine.csv"))
temp[, .N, marine]

## Check if marine species also occur under delete_species
temp[marine == 1][, .N, delete_sp]
temp[delete_sp == 1][, .N, marine]
dim(temp[marine == 1 & delete_sp ==1])

## Check if marine species have identified name issue and/or corrections
temp[marine == 1][, .N, name_issues]
temp[marine == 1][, .N, name_corrections]

## Get list of marine species  (to be removed from output table/data)
temp <- temp[marine == 1]$spfile
write.csv(temp, file.path(corr_dir, "marine_species.csv"), row.names = FALSE)
rm(temp)



## EXTRAS
## >> Find NA in name_corrections ####
## Corrections made directly in invert_fireoverlap_ALnameissues_JRM.csv
nrow(temp[!is.na(spell_variants) & is.na(name_corrections) & is.na(delete_sp)])
# x <- temp[!is.na(spell_variants) & is.na(name_corrections) & is.na(delete_sp)][, .(spfile, scientificName, class, order, family, delete_sp, spell_variants, name_corrections)]
# write.csv(x, file.path(corr_dir, "JM_1spellvariants_NAnamecorrections.csv"), row.names = FALSE)
