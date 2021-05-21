## Data corrections for ALA-nonALA data


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table")
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
message(cat("Number of name_corrections records to be resolved: "),
        nrow(temp[!is.na(spell_variants)
                  & is.na(name_corrections)
                  & is.na(delete_sp)]))


## >> Find species with 'group/complex/etc.' in name ####
grep("group|Group|grp.|grp|Grp.|Grp|complex", temp$scientificName, value = TRUE)
  # x <- temp[grep("group|Group|grp.|grp|Grp.|Grp|complex", temp$scientificName)][is.na(delete_sp)][, .(spfile, scientificName, class, order, family, delete_sp, spell_variants, name_corrections)]
  # write.csv(x, file.path(corr_dir, "JM_names_correction1.csv"), row.names = FALSE)

## Assign 1 in delete_sp as per JM_names_correction1_JRM.csv
x <- fread(file.path(corr_dir, "JM_names_correction1_JRM.csv"))
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$spfile == x[!is.na(delete_sp)]$spfile
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$delete_sp = 1
rm(x)


## >> Find species with '?' in name ####
  # x <- temp[grep("\\?", temp$scientificName)][is.na(delete_sp)][, .(spfile, scientificName, class, order, family, delete_sp, spell_variants, name_corrections)]
  # write.csv(x, file.path(corr_dir, "JM_names_correction2.csv"), row.names = FALSE)

## Assign 1 in delete_sp as per JM_names_correction2_JRM.csv
x <- fread(file.path(corr_dir, "JM_names_correction2_JRM.csv"))
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$spfile == x[!is.na(delete_sp)]$spfile
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$delete_sp = 1
rm(x)


## >> Save data table ####
temp1 <- data.table::copy(temp)
rm(temp)



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
  ## JM already added 1 to delete_sp inn data sheet
temp[, `JW notes` := NULL]
temp[, `JM notes` := NULL]

## >> Quality checks ####
temp[, .N, name_issues]
temp[, .N, spell_variants]
nrow(temp[!is.na(name_corrections)])
  ## spell_variants and name_corrections should be equal
  ## i.e., for a record if spell_variants = 1, text string should be provided in name_corrections


## >> Find NA in name_corrections ####
message(cat("Number of name_corrections records to be resolved: "),
        nrow(temp[!is.na(spell_variants)
                  & is.na(name_corrections)
                  & is.na(delete_sp)]))
  # x <- temp[!is.na(spell_variants) & is.na(name_corrections) & is.na(delete_sp)][, .(spfile, scientificName, class, order, family, delete_sp, spell_variants, name_corrections)]
  # write.csv(x, file.path(corr_dir, "JW_1spellvariants_NAnamecorrections.csv"), row.names = FALSE)

  # grep("mandjelia_myg438_wanjarri_1969", temp$spfile)
  # temp[grep("mandjelia_myg438_wanjarri_1969", temp$spfile)]
  # grep("mandjelia_myg438_wanjarri_1969", temp$name_corrections)
  # temp[grep("mandjelia_myg438_wanjarri_1969", temp$name_corrections)]

## >> Correct NAs in name_corrections ####
## NOTE: These are records with the correct species name, 
##  so name_corrections column can be populated with corresponding spfile;
##  associated records with the incorrect species inndicated in name_correctionns already
temp[!is.na(spell_variants) & is.na(name_corrections) & is.na(delete_sp)]$name_corrections = temp[!is.na(spell_variants) & is.na(name_corrections) & is.na(delete_sp)]$spfile
message(cat("Number of name_corrections records to be resolved: "),
        nrow(temp[!is.na(spell_variants)
                  & is.na(name_corrections)
                  & is.na(delete_sp)]))

## >> Find species with 'group/complex/etc.' in name ####
  # x <- temp[grep("group|Group|grp.|grp|Grp.|Grp|complex", temp$scientificName)][is.na(delete_sp)][, .(spfile, scientificName, class, order, family, delete_sp, spell_variants, name_corrections)]
  # write.csv(x, file.path(corr_dir, "JW_names_correction1.csv"), row.names = FALSE)

## Assign 1 in delete_sp as per JW_names_correction1_JM.csv
x <- fread(file.path(corr_dir, "JW_names_correction1_JM.csv"))
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$spfile == x[!is.na(delete_sp)]$spfile
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$delete_sp = 1
rm(x)


## >> Find species with '?' in name ####
  # x <- temp[grep("\\?", temp$scientificName)][is.na(delete_sp)][, .(spfile, scientificName, class, order, family, delete_sp, spell_variants, name_corrections)]
  # write.csv(x, file.path(corr_dir, "JW_names_correction2.csv"), row.names = FALSE)

## Assign 1 in delete_sp as per JW_names_correction2_JM.csv
x <- fread(file.path(corr_dir, "JW_names_correction2_JM.csv"))
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$spfile == x[!is.na(delete_sp)]$spfile
temp[spfile %in% x[!is.na(delete_sp)]$spfile]$delete_sp = 1
rm(x)


## >> Save data table
temp2 <- data.table::copy(temp)
rm(temp)



## Combine tables ####
temp <- rbind(temp1, temp2)
setDT(temp, key = "spfile")
rm(temp1, temp2)

temp1 <- data.table::copy(temp)


## Check table and correct values ####
temp[, .N, delete_sp]
temp[is.na(delete_sp)][, .N, name_issues]
temp[is.na(delete_sp)][, .N, spell_variants]
temp[is.na(delete_sp)][, .N, name_corrections]

## Assign spell_variants as NA if delete_sp = 1
temp[!is.na(delete_sp)][!is.na(spell_variants)]$spell_variants = NA

## Assign spell_variants as 1 if !is.na(name_corrections) and delete_sp != 1
temp[is.na(delete_sp)][!is.na(name_corrections)]$spell_variants = 1

## Checks
nrow(temp[is.na(delete_sp)][!is.na(spell_variants)])
nrow(temp[is.na(delete_sp)][!is.na(name_corrections)])

message(cat("Number of records for which name_corrections are provided but they are not marked as spell_variants: "),
        nrow(temp[is.na(delete_sp)][is.na(spell_variants)][!is.na(name_corrections)]))

message(cat("Number of species marked as spell_variants but for which name_corrections are not provided: "),
        nrow(temp[is.na(delete_sp)][!is.na(spell_variants)][is.na(name_corrections)]))

temp[!is.na(delete_sp)][, .N, spell_variants]
temp[!is.na(delete_sp)][, .N, name_corrections]


## Correct values for individual species
grep("kwonkan_goongarriensis_39854", temp$name_corrections)
temp[grep("kwonkan_goongarriensis_39854", temp$name_corrections)]
temp[grep("kwonkan_goongarriensis_39854", temp$name_corrections)]$delete_sp = NA
temp[grep("kwonkan_goongarriensis_39854", temp$name_corrections)]$spell_variants = 1

grep("kwonkan_goongarriensis_39854", temp$spfile)
temp[grep("kwonkan_goongarriensis_39854", temp$spfile)]


## Assign spell_variants as 1 for all name_corrections found in spfile
nrow(temp[spell_variants == 1])
for (i in unique(temp$name_corrections)){
  temp[spfile == i]$spell_variants = 1
}
nrow(temp[spell_variants == 1])

## Update name_corrections for new records found form last step
nrow(temp[is.na(delete_sp)][!is.na(spell_variants)])
nrow(temp[is.na(delete_sp)][!is.na(name_corrections)])
temp[is.na(delete_sp)][!is.na(spell_variants)][is.na(name_corrections)]

temp[is.na(delete_sp)][!is.na(spell_variants)][is.na(name_corrections)]$name_corrections =
  temp[is.na(delete_sp)][!is.na(spell_variants)][is.na(name_corrections)]$spfile

nrow(temp[is.na(delete_sp)][!is.na(spell_variants)])
nrow(temp[is.na(delete_sp)][!is.na(name_corrections)])
temp[is.na(delete_sp)][!is.na(spell_variants)][is.na(name_corrections)]


## Final checks
temp[, .N, delete_sp]
temp[is.na(delete_sp)][, .N, name_issues]
temp[is.na(delete_sp)][, .N, spell_variants]
temp[is.na(delete_sp)][, .N, name_corrections]

nrow(temp[is.na(delete_sp)][!is.na(spell_variants)])
nrow(temp[is.na(delete_sp)][!is.na(name_corrections)])

nrow(temp[is.na(delete_sp)][is.na(spell_variants)][!is.na(name_corrections)])
nrow(temp[is.na(delete_sp)][!is.na(spell_variants)][is.na(name_corrections)])

temp[!is.na(delete_sp)][, .N, spell_variants]
temp[!is.na(delete_sp)][, .N, name_corrections]



## Add marine species information ####
sp1 <- fread(file.path(corr_dir, "invert_fireoverlap_JRM_marine.csv"))
setDT(sp1, key = "spfile")
sp1[, .N, marine]
message(cat("All marine species found in data: "),
        all(temp[spfile %in% sp1[marine == 1]$spfile]$spfile == 
              sp1[marine == 1]$spfile))
temp[spfile %in% sp1[marine == 1]$spfile]$marine = 1
rm(sp1)


## Save table ####
write.csv(temp, file.path(corr_dir, "invert_fireoverlap_corrected.csv"), 
          row.names = FALSE)


## Create list of species to update ####
x <- temp[spell_variants == 1]
length(unique(x$spfile))
length(unique(x$name_corrections))

x <- x[, .(spfile, scientificName, class, order, family, marine, delete_sp, name_issues, spell_variants, name_corrections)]

## Checks
unique(x$marine)
unique(x$delete_sp)
length(x$spfile) == length(unique(x$spfile))

write.csv(x, file.path(corr_dir, "update_species.csv"), row.names = FALSE)


## Create list of species to remove from data_ALAnonALA_wgs84.csv ####
s <- temp[marine == 1 | delete_sp == 1]$spfile
length(s)
length(unique(s))
write.csv(s, file.path(corr_dir, "delete_species_fromdata.csv"), 
          row.names = FALSE)

## Create list of species to remove from output table ####
s <- c(temp[marine == 1 | delete_sp == 1]$spfile, x$spfile)
length(s)
length(unique(s))
write.csv(s, file.path(corr_dir, "delete_species_fromoutputs.csv"), 
          row.names = FALSE)
rm(s, x, temp)


## Update data_ALAnonALA_wgs84.csv ####
## >> Correct data_ALAnonALA_wgs84.csv with updated species names ####
data <- fread(file.path(output_dir, "data_ALAnonALA_wgs84.csv"))
new_sp <- fread(file.path(corr_dir, "update_species.csv"))

length(new_sp$spfile)
length(unique(new_sp$name_corrections))

## Check all species are found in data
all(sort(unique(data[spfile %in% new_sp$spfile]$spfile)) == sort(new_sp$spfile))

## Add new columns to data table
data$order <- rep(character(), nrow(data))

## Change spfile in data according to name_corrections in new_sp
for (i in unique(new_sp$name_corrections)){
  data[spfile %in% new_sp[name_corrections == i]$spfile]$spfile = i
}

## Update class, order, family information according to new_sp
for (i in unique(new_sp$name_corrections)){
  data[spfile %in% i]$class = new_sp[spfile == i]$class
  data[spfile %in% i]$order = new_sp[spfile == i]$order
  data[spfile %in% i]$family = new_sp[spfile == i]$family
}

## Checks
length(unique(data[spfile %in% new_sp$spfile]$spfile)) == length(unique(new_sp$name_corrections))
all(sort(length(unique(data[spfile %in% new_sp$spfile]$spfile))) == sort(length(unique(new_sp$name_corrections))))

## >> Remove species from data_ALAnonALA_wgs84.csv
delete_sp <- fread(file.path(corr_dir, "delete_species_fromdata.csv"))$x

length(delete_sp)

length(unique(data$spfile)) - length(delete_sp)
length(unique(data[!spfile %in% delete_sp]$spfile))
dim(data)
data <- data[!spfile %in% delete_sp]

message(cat("Number of unique species in updated data: "),
        length(unique(data$spfile)))
message(cat("Total number of records in updated data: "),
        nrow(data))

## >> Save updated data table ####
names(data)
data <- data[, c(1:6, 12, 7:11)]
setDT(data, key = "spfile")
write.csv(data, file.path(output_dir, "data_ALAnonALA_wgs84_corrected.csv"), 
          row.names = FALSE)




## EXTRAS
# ## >> Find NA in name_corrections ####
# ## Corrections made directly in invert_fireoverlap_ALnameissues_JRM.csv
# nrow(temp[!is.na(spell_variants) & is.na(name_corrections) & is.na(delete_sp)])
# x <- temp[!is.na(spell_variants) & is.na(name_corrections) & is.na(delete_sp)][, .(spfile, scientificName, class, order, family, delete_sp, spell_variants, name_corrections)]
# write.csv(x, file.path(corr_dir, "JM_1spellvariants_NAnamecorrections.csv"), row.names = FALSE)


# ## Assign spell_variants as 1 for species found with spell_variants = NA
# ## Individually: 
# ## latrodectus_hasseltii_1809, lychas_bituberculatus_1859, 
# ##  lychas_hairy_tail_1868, protogamasellus_massula_2501
# grep(sp, temp$spfile)
# grep(sp, temp$name_corrections)
# temp[grep(sp, temp$name_corrections)]
# temp[grep(sp, temp$name_corrections)]$spell_variants = 1
# temp[is.na(delete_sp)][is.na(spell_variants)][!is.na(name_corrections)]
# 
# ## Assign spell_variants as 1 for species found with spell_variants = NA
# ## In a loop:
# x <- temp[is.na(delete_sp)][is.na(spell_variants)][!is.na(name_corrections)]$spfile
# for (sp in x){
#   temp[grep(sp, temp$name_corrections)]$spell_variants = 1
# }
# rm(x, sp)
