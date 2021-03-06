## Create cleaned AFD checklist

## Set working environment ####
data_path <- "/Volumes/uom_data/nesp_bugs_data"
output_dir  <-  "/Volumes/uom_data/nesp_bugs_data/outputs"

## Functions ####
source("./scripts/remove_improper_names.R")
'%!in%' <- function(x,y)!('%in%'(x,y))

## Australian Faunal Directory taxonomy
## Species lists for selected phyla created by AFD; australianfaunaldirectory@awe.gov.au

  # ## Merge files
  # afd_path <- paste0(data_path, "/AFD/AFD_invertebrate_ranks")
  # afd_files <- list.files(afd_path, full.names = TRUE)
  # afd_species <- do.call("rbind",lapply(afd_files, FUN = function(files){ read.csv(files)}))
  # readr::write_csv(afd_species, "./output/afd_specieslist_full.csv")

afd_species <- read.csv(file.path(output_dir, "afd_splist_full.csv"))
all_species <- afd_species$VALID_NAME


## ------------------------------ ##
## Remove improper names ####
## ------------------------------ ##
species_record <- remove_improper_names(as.character(all_species),
                               allow.higher.taxa = FALSE,
                               allow.subspecies = TRUE)
sum(is.na(species_record$updated_list))
str(species_record)

## Update AFD taxonomy based on selected species
species <- as.character(na.omit(species_record$updated_list))
afd_species <- afd_species[which(afd_species$VALID_NAME %in% species),]
  ## Note:  mismatch between length(species) and dim(afd_species), 
  ##        possibly due to duplicates being removed. 


## ------------------------------ ##
## Remove invasive species ####
## ------------------------------ ## 
## Source: https://lists.ala.org.au/speciesListItem/list/dr9884#list
griis_species <- read.csv(file.path(data_path, "ALA/GRIIS_Global_Register_of_Introduced_and_Invasive_Species_Australia.csv"))

message(cat("number of AFD species listed in GRIIS: "),
        length(which(afd_species$VALID_NAME %in% griis_species$Supplied.Name)))

message("AFD species in GRIIS - Global Register of Introduced and Invasive Species - Australia: ")
afd_species$VALID_NAME[which(afd_species$VALID_NAME %in% griis_species$Supplied.Name)]

message("Removing AFD species in GRIIS ...")
afd_species <- afd_species[which(afd_species$VALID_NAME %!in% griis_species$Supplied.Name),]


## Checks for special characters
length(afd_species$VALID_NAME[grep("\"", afd_species$VALID_NAME, fixed = TRUE)])
length(afd_species$VALID_NAME[grep("\'", afd_species$VALID_NAME, fixed = TRUE)])
length(afd_species$VALID_NAME[grep("(", afd_species$VALID_NAME, fixed = TRUE)])
length(afd_species$VALID_NAME[grep("[", afd_species$VALID_NAME, fixed = TRUE)])


## ------------------------------ ##
## Identify duplicates ####
## ------------------------------ ##

## List duplicates comparing all columns
##  Note: # gives zero = no duplicated rows
# afd_species[duplicated(afd_species),] 

## Look for duplicates in specific columns
sum(is.na(afd_species$VALID_NAME))
length(afd_species$VALID_NAME)
length(unique(afd_species$VALID_NAME))
length(unique(afd_species$COMPLETE_NAME))
  ## JM - use COMPLETE_NAME for finding duplicates

## Duplicates in COMPLETE_NAME (excluding first appearance)
message(cat("Number of duplicated COMPLETE_NAME (excluding first appearance): "), 
            length(afd_species$COMPLETE_NAME[duplicated(afd_species$COMPLETE_NAME)]))
message("duplicated COMPLETE_NAME: ")
afd_species$COMPLETE_NAME[duplicated(afd_species$COMPLETE_NAME)]

## Duplicates in COMPLETE_NAME (including first appearance)
temp <- afd_species[which(
  duplicated(afd_species$COMPLETE_NAME) | 
    duplicated(afd_species$COMPLETE_NAME[
      length(afd_species$COMPLETE_NAME):1])
  [length(afd_species$COMPLETE_NAME):1]),]

message(cat("#duplicates in COMPLETE_NAME (including first appearance) : ", dim(temp)))
message("duplicated COMPLETE_NAME: ")
temp$COMPLETE_NAME
# readr::write_csv(temp, "./output/afd_completename_repeats.csv")

## Resolve duplicates from ALA list *in consultation with JM*
## using TAXON_GUID from afd_completename_repeats_JRM.csv
# afd_species <- unique(afd_species$COMPLETE_NAME)
removed_dups <- c("b05771ae-bda7-497a-87c4-b55a0ebc4ca1",
                  "03acc9d4-a209-4bf0-9972-bc7d35d56aea",
                  "83d18631-e160-42ad-8332-89e4b8ba82b6",
                  "c05506f8-0188-4850-8136-7b45ea35638e",
                  "0bb19498-874f-4c6c-a637-124ec9878130")

afd_species <- afd_species[which(afd_species$TAXON_GUID %!in% removed_dups),]
readr::write_csv(afd_species, "./outputs/afd_species_clean.csv")

## Checks
sp_words <- sapply(strsplit(as.character(afd_species$VALID_NAME), " "), length)
length(afd_species$VALID_NAME[which(sp_words == 5)])
length(afd_species$VALID_NAME[which(sp_words == 4)])
length(afd_species$VALID_NAME[which(sp_words == 3)])
length(afd_species$VALID_NAME[which(sp_words == 2)])
length(afd_species$VALID_NAME[which(sp_words == 1)])

## Checks
length(afd_species$VALID_NAME[grep("\"", afd_species$VALID_NAME, fixed = TRUE)])
length(afd_species$VALID_NAME[grep("\'", afd_species$VALID_NAME, fixed = TRUE)])
