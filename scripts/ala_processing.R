## Clean downloaded ALA data and save by species
## Notes: Each occurrence record has a record id, we can use `occurrence_details` to get additional information if you need something extra later


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "sp", "raster", "rje", "stringr")
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

ala_dir <- file.path(output_dir, "ala_data")

## Functions
'%!in%' <- function(x,y)!('%in%'(x,y))

## Load AFD taxonomic checklist
afd_taxonomy <- fread(file.path(output_dir, "afd_species_clean.csv"))
afd_species <- unique(afd_taxonomy$VALID_NAME)
message(cat("Number of species in AFD checklist: "),
        length(afd_species))

## Load ALA data
ala_raw <- readRDS(file.path(output_dir, "merged_ala_2020-10-02.rds"))
ala_species <- unique(ala_raw$scientificName)
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
        nrow(ala_dat)/nrow(ala_raw))
message(cat("Proprotion of species removed: "),
        (length(unique(ala_raw$scientificName))-
           length(ala_species))/length(unique(ala_raw$scientificName)))

## Checks
length(ala_species) == length(unique(ala_dat$scientificName))
rje::is.subset(ala_species, unique(ala_dat$scientificName))
rje::is.subset(unique(ala_dat$scientificName), ala_species)
sum(ala_species==unique(ala_dat$scientificName))


## Identify ALA species not found in AFD checklist ####
## ALA scientificName compared against VALID_NAME and SYNONYMS in AFD checklist
## Species names categorised by number of words in the name for comparisons  
ala_species <- unique(ala_dat$scientificName)
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
ala_species <- unique(ala_dat$scientificName)
sp_words <- sapply(strsplit(as.character(ala_species), " "), length)
unique(sp_words)

n = 4

length(ala_species[which(sp_words == n)])
message(cat("Number of species not found in AFD: "),
        sum(ala_species[which(sp_words == n)] %!in% afd_species))
ala_names <- sort(ala_species[which(sp_words == n)][ala_species[which(sp_words == n)] %!in% afd_species])


## Checking for species manually
matches <- get_AFDsynonyms(ala_names, afd_taxonomy)

message(cat("Synonyms found for full species names for: "),
        sum(!is.na(sapply(matches, "[[", 1))), " species")

message(cat("Synonyms found for genus (only) names for: "),
        sum(!is.na(sapply(matches, "[[", 2))), " species")

message(cat("Synonyms found for species (only) names for: "),
        sum(!is.na(sapply(matches, "[[", 3))), " species")

## Save otputs
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
ala_species <- unique(ala_dat$scientificName)
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

## Save otputs
y <- rbindlist(matches, fill=FALSE)
y <- cbind(ala_names, y)
write.csv(y, file.path(output_dir, paste0("names", n, ".csv")))

## Remove species from dataset: No matches found [check with JM]
message(cat("Number of records to be removed: "),
        length(which(ala_dat$scientificName %in% ala_names)))

dim(ala_dat)
ala_dat <- ala_dat[which(ala_dat$scientificName %!in% ala_names),]
dim(ala_dat)


## >> ALA species: 2-word names ####
ala_species <- unique(ala_dat$scientificName)
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

## Save otputs
y <- rbindlist(matches, fill=FALSE)
y <- cbind(ala_names, y)
write.csv(y, file.path(output_dir, paste0("names", n, ".csv")))

## Remove species from dataset: No matches found [check with JM]
message(cat("Number of records to be removed: "),
        length(which(ala_dat$scientificName %in% ala_names)))


ala_dat_pre2 <- ala_dat
dim(ala_dat)
ala_dat <- ala_dat[which(ala_dat$scientificName %!in% ala_names),]
dim(ala_dat)


## Summary stats ####




## Species with data
ala_species
ala_dat1 <- ala_dat[scientificName %in% afd_species] or synonyms...
## too simplistic? shoudl we be lookign at other fields as well?
## download script was set up to download bysearching for text AFD::VALID_NAME (==ALA::scientificName)...
sp_ala1 <- unique(ala_dat1$scientificName)
message(cat("Prop of ALA species found in AFD: "),
        length(sp_ala1)/length(ala_species))
length(sp_ala1)


## Species without data
ala_dat0 <- ala_dat[scientificName %!in% afd_species]
sp_ala0 <- unique(ala_dat0$scientificName)
length(sp_ala0)
message(cat("Prop of ALA species NOT found in AFD: "),
        length(sp_ala0)/length(ala_species))
length(sp_ala0)


sp_nodat[1:100]
sp_words <- sapply(strsplit(as.character(sp_nodat), " "), length)
unique(sp_words)
## Remove species with one word
length(sp_nodat[which(sp_words == 1)])
idx_more1 <- which(sp_words != 1)

sp_nodat[which(sp_words == 0)]
idx_0 <- which(sp_words = 0)
length(sp_nodat[which(sp_words == 4)])

sp_nodat[which(sp_words == 4)]


length(sp_nodat[which(sp_words == 3)])
idx_3 <- which(sp_words == 3)
length(sp_nodat[which(sp_words == 2)])
idx_2 <- which(sp_words == 2)


dim(ala_dat)[1]+dim(ala_nodat)[1]==dim(ala_raw)[1]

sp_nodata <- afd_species %!in% sp_withdata
length(sp_nodata)

## Mask
reg.mask.file = file.path(output_dir, "ausmask_WGS.tif")



## duplicates
# ## Remove duplicates
# if(remove_duplicates == TRUE){
#   ala_df <- ala_df[!duplicated(ala_df[ , c("longitude", "latitude")]), ]
# }
JM...  museum specimens with duplicated lat-long



## Count files ####
## Species with data
sp_data <- list.files(file.path(ala_dir, "maps"), include.dirs = FALSE)
# list.files(ala_dir, include.dirs = FALSE) ## lists "maps" folder as well
sp_data <- gsub(".pdf", "", sp_data)
length(sp_data)

## Species without data
## No data species txt file
nodatalog <- file.path(output_dir, "nodataspecies_log.txt")
writeLines(c("species0"), nodatalog)

sp_nodata <- read.csv(nodatalog)
sp_nodata <- sp_nodata$species0
length(sp_nodata)

## Clean by species...

## Get rid of unusable long lat vals
ala_df <- ala_df[ala_df$longitude > -180 &
                   ala_df$longitude < 180 &
                   ala_df$latitude > -90 &
                   ala_df$latitude < 90, ]

# ## Remove generalised data (as much as possible)
# if(any(grepl("dataGeneralizations", names(ala_df)))) {
#   ala_df <- ala_df[ala_df$dataGeneralizationsOriginal == FALSE,]
# }

# ## Remove duplicates
# if(remove_duplicates == TRUE){
#   ala_df <- ala_df[!duplicated(ala_df[ , c("longitude", "latitude")]), ]
# }



# for (n in dat_cols) {
#   class(f[, n]) <- coltypes[n]
# }

# f[,verbatimEventDate:=NULL]
# f[,verbatimEventDate:=NULL]
# 
# f[,verbatimEventDate := lubridate::as_date(verbatimEventDate)]
