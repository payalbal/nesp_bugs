## Clean downloaded ALA data and summarise
## Notes: Each occurrence record has a record id, we can use `occurrence_details` to get additional information if you need something extra later


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "rje", "stringr")
lapply(x, require, character.only = TRUE)
rm(x)

## Server paths
output_dir = file.path(getwd(), "nesp_bugs", "outputs")
source(file.path(getwd(),"nesp_bugs", "scripts/remove_improper_names.R"))
source(file.path(getwd(),"nesp_bugs", "scripts/get_AFDsynonyms.R"))

# ## Local paths
# output_dir = "/Volumes/uom_data/nesp_bugs_data/outputs"
# source(file.path(getwd(), "scripts/remove_improper_names.R"))
# source(file.path(getwd(), "scripts/get_AFDsynonyms.R"))

## Functions
'%!in%' <- function(x,y)!('%in%'(x,y))

## Load AFD taxonomic checklist
afd_taxonomy <- fread(file.path(output_dir, "afd_species_clean.csv"))
afd_species <- sort(unique(afd_taxonomy$VALID_NAME))
message(cat("Number of species in AFD checklist: "),
        length(afd_species))

## Load ALA data
ala_raw <- readRDS(file.path(output_dir, "merged_ala_2020-10-02.rds"))
ala_species <- sort(unique(ala_raw$scientificName))
message(cat("Number of unique species in ALA data: "),
        length(ala_species))
message(cat("NAs in ALA species list: "),
        sum(is.na(ala_species)))


## ALA data cleaning ####
## >> Find improper & incomplete names in ALA data ####
species_record <- remove_improper_names(as.character(ala_species),
                                        allow.higher.taxa = FALSE,
                                        allow.subspecies = TRUE)
message(cat("# Improper species names found in ALA data: "),
        length(species_record$improper_species))
# message("Improper species names found in ALA data: ")
# species_record$improper_species
message(cat("# Incomplete species names found in ALA data: "),
        length(species_record$incomplete_species))
message(cat("NAs in cleaned ALA species list: "),
        sum(is.na(species_record$updated_list)))
message(cat("Duplicates in cleaned ALA species list: "),
        length(species_record$updated_list[duplicated(species_record$updated_list)]))


## >> Remove improper & incomplete names from ALA data ####
ala_species <- as.character(na.omit(species_record$updated_list))
ala_dat <- ala_raw[which(ala_raw$scientificName %in% ala_species),]

message(cat("Number of records in raw ALA data: "),
        nrow(ala_raw))
message(cat("Number of records in cleaned ALA data: "),
        nrow(ala_dat))
message(cat("Prop of records lost in cleaning ALA data: "),
        (nrow(ala_raw) - nrow(ala_dat))/nrow(ala_raw))
message(cat("Proprotion of species removed: "),
        (length(unique(ala_raw$scientificName))-
           length(ala_species))/length(unique(ala_raw$scientificName)))

## Checks
length(ala_species) == length(unique(ala_dat$scientificName))
rje::is.subset(ala_species, unique(ala_dat$scientificName))
rje::is.subset(unique(ala_dat$scientificName), ala_species)
sum(ala_species==sort(unique(ala_dat$scientificName)))


## Identify ALA species not found in AFD checklist ####
## ALA scientificName compared against VALID_NAME and SYNONYMS in AFD checklist
## Species names categorised by number of words in the name for comparisons  
ala_species <- sort(unique(ala_dat$scientificName))
message(cat("Are all ALA species contained in the AFD checklist: "),
        rje::is.subset(ala_species, afd_species))

## >> ALA species: 5-word names ####
sp_words <- sapply(strsplit(as.character(ala_species), " "), length)
unique(sp_words)

n = 5

length(ala_species[which(sp_words == n)])
message(cat("Species found in AFD: "),
        ala_species[which(sp_words == n)] %in% afd_species)
ala_species[which(sp_words == n)]

## Check for species manually in ALA
grep("Metapenaeus endeavouri", ala_species)
ala_species[grep("Metapenaeus endeavouri", ala_species)]
ala_dat[grep("Metapenaeus endeavouri", ala_dat$scientificName)]$scientificName

grep("Metapenaeus ensis", ala_species)
ala_species[grep("Metapenaeus ensis", ala_species)]
ala_dat[grep("Metapenaeus ensis", ala_dat$scientificName)]$scientificName

ala_dat[grep("Metapenaeus endeavouri & Metapenaeus ensis", ala_dat$scientificName)]$scientificName

# temp <- ala_dat[grep("Metapenaeus endeavouri & Metapenaeus ensis", ala_dat$scientificName)]
# write.csv(temp, file.path(output_dir, "names5_records.csv"))
# rm(temp)

## Check for species manually in AFD
grep("Metapenaeus endeavouri", afd_species)
afd_taxonomy[VALID_NAME == "Metapenaeus endeavouri"]$SYNONYMS
grep("Penaeopsis endeavouri", afd_taxonomy$VALID_NAME)
grep("Penaeopsis endeavouri", afd_taxonomy$COMPLETE_NAME)

grep("Metapenaeus ensis", afd_species)
afd_taxonomy[VALID_NAME == "Metapenaeus ensis"]$SYNONYMS
grep("Penaeus ensis", afd_taxonomy$VALID_NAME)
grep("Metapenaeus philippinensis", afd_taxonomy$VALID_NAME)
grep("Penaeus incisipes", afd_taxonomy$VALID_NAME)
grep("Penaeus mastersii", afd_taxonomy$VALID_NAME)

## Remove species from dataset: No matches found [check with JM]
message(cat("Number of records to be removed: "),
        nrow(ala_dat[which(ala_dat$scientificName %in% ala_species[which(sp_words == n)]),]))
dim(ala_dat)
ala_dat <- ala_dat[which(ala_dat$scientificName %!in% ala_species[which(sp_words == n)]),]
dim(ala_dat)

## >> ALA species: 4-word names ####
ala_species <- sort(unique(ala_dat$scientificName))
sp_words <- sapply(strsplit(as.character(ala_species), " "), length)
unique(sp_words)

n = 4

length(ala_species[which(sp_words == n)])
message(cat("Number of species not found in AFD: "),
        sum(ala_species[which(sp_words == n)] %!in% afd_species))
ala_names <- sort(ala_species[which(sp_words == n)][ala_species[which(sp_words == n)] %!in% afd_species])

temp <- ala_dat[scientificName %in% ala_names]
write.csv(temp, file.path(output_dir, "names4_records.csv"))
rm(temp)

## Checking for species manually
matches <- get_AFDsynonyms(ala_names, afd_taxonomy)

message(cat("Synonyms found for full species names for: "),
        sum(!is.na(sapply(matches, "[[", 1))), " species")

message(cat("Synonyms found for genus (only) names for: "),
        sum(!is.na(sapply(matches, "[[", 2))), " species")

message(cat("Synonyms found for species (only) names for: "),
        sum(!is.na(sapply(matches, "[[", 3))), " species")

## Save outputs
y <- rbindlist(matches, fill=FALSE)
y <- cbind(ala_names, y)
write.csv(y, file.path(output_dir, paste0("names", n, ".csv")))

## Remove species from dataset: No matches found [check with JM]
message(cat("Number of records to be removed: "),
        length(which(ala_dat$scientificName %in% ala_names)))

dim(ala_dat)
ala_dat <- ala_dat[which(ala_dat$scientificName %!in% ala_names),]
dim(ala_dat)


## >> ALA species: 3-word names ####
ala_species <- sort(unique(ala_dat$scientificName))
sp_words <- sapply(strsplit(as.character(ala_species), " "), length)
unique(sp_words)

n = 3

length(ala_species[which(sp_words == n)])
message(cat("Number of species not found in AFD: "),
        sum(ala_species[which(sp_words == n)] %!in% afd_species))
ala_names <- sort(ala_species[which(sp_words == n)][ala_species[which(sp_words == n)] %!in% afd_species])

## Checking for species manually
matches <- get_AFDsynonyms(ala_names, afd_taxonomy)

message(cat("Synonyms found for full species names for: "),
        sum(!is.na(sapply(matches, "[[", 1))), " species")
names(matches[!is.na(sapply(matches, "[[", 1))])

message(cat("Synonyms found for genus (only) names for: "),
        sum(!is.na(sapply(matches, "[[", 2))), " species")
names(matches[!is.na(sapply(matches, "[[", 2))])

message(cat("Synonyms found for species (only) names for: "),
        sum(!is.na(sapply(matches, "[[", 3))), " species")
names(matches[!is.na(sapply(matches, "[[", 3))])

## Save outputs
y <- rbindlist(matches, fill=FALSE)
y <- cbind(ala_names, y)
write.csv(y, file.path(output_dir, paste0("names", n, ".csv")))

## Record and keep species in dataset: Unmacthed & partially matched species
message(cat("Number of records to be marked for checking: "),
        length(which(ala_dat$scientificName %in% ala_names)))

# dim(ala_dat)
# ala_dat <- ala_dat[which(ala_dat$scientificName %!in% ala_names),]
# dim(ala_dat)

ala_dat <- cbind(ala_dat, rep(NA, nrow(ala_dat)))
names(ala_dat)[ncol(ala_dat)] <- paste0("names", n)

setkey(ala_dat, scientificName)
ala_dat[ala_names, paste0("names", n) := 1]
ala_dat[scientificName %!in% ala_names, paste0("names", n) := 0]

## Check
nrow(ala_dat[names3 == TRUE]) == length(which(ala_dat$scientificName %in% ala_names))


## >> ALA species: 2-word names ####
ala_species <- sort(unique(ala_dat$scientificName))
sp_words <- sapply(strsplit(as.character(ala_species), " "), length)
unique(sp_words)

n = 2

length(ala_species[which(sp_words == n)])
message(cat("Number of species not found in AFD: "),
        sum(ala_species[which(sp_words == n)] %!in% afd_species))
ala_names <- sort(ala_species[which(sp_words == n)][ala_species[which(sp_words == n)] %!in% afd_species])

## Checking for species manually
matches <- get_AFDsynonyms(ala_names, afd_taxonomy)

message(cat("Synonyms found for full species names for: "),
        sum(!is.na(sapply(matches, "[[", 1))), " species")
names(matches[!is.na(sapply(matches, "[[", 1))])

message(cat("Synonyms found for genus (only) names for: "),
        sum(!is.na(sapply(matches, "[[", 2))), " species")
names(matches[!is.na(sapply(matches, "[[", 2))])

message(cat("Synonyms found for species (only) names for: "),
        sum(!is.na(sapply(matches, "[[", 3))), " species")
names(matches[!is.na(sapply(matches, "[[", 3))])

## Save outputs
y <- rbindlist(matches, fill=FALSE)
y <- cbind(ala_names, y)
write.csv(y, file.path(output_dir, paste0("names", n, ".csv")))

## Record and keep species in dataset: Unmacthed & partially matched species
message(cat("Number of records to be marked for checking: "),
        length(which(ala_dat$scientificName %in% ala_names)))

# dim(ala_dat)
# ala_dat <- ala_dat[which(ala_dat$scientificName %!in% ala_names),]
# dim(ala_dat)

ala_dat <- cbind(ala_dat, rep(NA, nrow(ala_dat)))
names(ala_dat)[ncol(ala_dat)] <- paste0("names", n)

setkey(ala_dat, scientificName)
ala_dat[ala_names, paste0("names", n) := 1]
ala_dat[scientificName %!in% ala_names, paste0("names", n) := 0]

## Check
nrow(ala_dat[names2 == TRUE]) == length(which(ala_dat$scientificName %in% ala_names))


## Save cleaned ALA data ####
saveRDS(ala_dat, file = file.path(output_dir, paste0("clean_ala_", Sys.Date(),".rds")))
write.csv(ala_dat, file = file.path(output_dir, paste0("clean_ala_", Sys.Date(),".csv")))


## Cleaned ALA data summary ####
message(cat("Number of records in raw ALA data: "),
        dim(ala_raw)[1])
message(cat("Number of species in raw ALA data: "),
        length(unique(ala_raw$scientificName)))
message(cat("Raw ALA data file: \n"),
        paste0(list.files(output_dir, 
                          pattern = "merged_ala", 
                          full.names = TRUE), sep = "\n"))


message(cat("Number of records in cleaned ALA data: "),
        dim(ala_dat)[1])
message(cat("Number of species in cleaned ALA data: "),
        length(unique(ala_dat$scientificName)))
message(cat("Cleaned ALA data file: \n"),
        paste0(list.files(output_dir, 
                          pattern = "clean_ala", 
                          full.names = TRUE), sep = "\n"))


message(cat("Proportion of records lost in cleaning ALA data: "),
        (dim(ala_raw)[1] - dim(ala_dat)[1])/(dim(ala_raw)[1]))
message(cat("Proportion of species lost in cleaning ALA data: "),
        (length(unique(ala_raw$scientificName)) - 
           length(unique(ala_dat$scientificName)))/
          (length(unique(ala_raw$scientificName))))


## Number of species
ala_species <- sort(unique(ala_dat$scientificName))
message(cat("Number of species in AFD checklist: "),
        length(afd_species))
message(cat("Number of unique species in ALA data: "),
        length(ala_species))


## Species with data
message(cat("Number of AFD species found in ALA: "),
        sum(afd_species %in% ala_species))
message(cat("Proportion of AFD species found in ALA: "),
        sum(afd_species %in% ala_species)/length(afd_species))


## Species without data
message(cat("Number of AFD species not found in ALA: "),
        sum(afd_species %!in% ala_species))
message(cat("Proportion of AFD species not found in ALA: "),
        sum(afd_species %!in% ala_species)/length(afd_species))

