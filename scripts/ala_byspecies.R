## Save species files


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "sp", "raster")
lapply(x, require, character.only = TRUE)
rm(x)

## Server paths
output_dir = file.path(getwd(), "nesp_bugs", "outputs")

# ## Local paths
# output_dir = "/Volumes/uom_data/nesp_bugs_data/outputs"

## Functions
'%!in%' <- function(x,y)!('%in%'(x,y))

## Load AFD taxonomic checklist
afd_taxonomy <- fread(file.path(output_dir, "afd_species_clean.csv"))
afd_species <- sort(unique(afd_taxonomy$VALID_NAME))

## Load ALA data
ala_dat <- list.files(output_dir, 
                      pattern = "clean_ala*.*.rds$", 
                      full.names = TRUE)
ala_dat_all <- ala_dat <- readRDS(ala_dat)
ala_species <- sort(unique(ala_dat$scientificName))


## Clean and save data by species

## Check for unusable long lat vals
message(cat("Number of records with unusable lat-long:"),
        nrow(ala_dat[longitude < -180 |
                       longitude > 180 |
                       latitude < -90 |
                       latitude > 90, ]))

## No date - mark 
dim(ala_dat[,is.na(ala_dat$eventDate)])

## Imprecise records... mark


## Sensitive species
# ## Remove generalised data (as much as possible)/Sensitive
# if(any(grepl("dataGeneralizations", names(ala_df)))) {
#   ala_df <- ala_df[ala_df$dataGeneralizationsOriginal == FALSE,]
# }
grep("General", names(ala_dat), value = TRUE)


## Clean by QA assertionns 
qa <- as.data.frame(read.csv(file.path(output_dir, "qa_assertions.csv")))

exclude <- qa[which(qa$exclude == 1),]$name
keep <- qa[which(qa$keep == 1),]$name
drop_qa <- qa[which(qa$not_relevant == 1),]$name



# ## Remove duplicates
# if(remove_duplicates == TRUE){
#   ala_df <- ala_df[!duplicated(ala_df[ , c("longitude", "latitude")]), ]
# }


## Fire extent..next script


## Mask including islands and offshore territories + buffer
ausmask = raster(file.path(output_dir, "aus_mainland_WGS.tif"))

# ## Example
# plot(ausmask)
# lat <- ala_dat[scientificName == "Inquisitor flindersianus"]$latitude
# long <- ala_dat[scientificName == "Inquisitor flindersianus"]$longitude
# points(long, lat)
