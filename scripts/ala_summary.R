## By ALA species


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
source(file.path(getwd(),"nesp_bugs", "scripts/get_ala_taxondata.R"))

## Load AFD taxonomic checklist
afd_taxonomy <- fread(file.path(output_dir, "afd_species_clean.csv"))
afd_species <- unique(afd_taxonomy$VALID_NAME)
message(cat("Number of species in AFD checklist: "),
        length(afd_species))

## Load ALA data
ala_dat <- readRDS(file.path(output_dir, "clean_ala_2020-10-07.rds"))
ala_species <- unique(ala_dat$scientificName)
message(cat("Number of unique species in ALA data: "),
        length(ala_species))


## Species with data
message(cat("Number of species with data (in ALA): "),
        length(ala_species))
message(cat("Proportion of species with data (in ALA): "),
        length(ala_species)/length(afd_species))

## Species without data
count0 <- afd_species[afd_species %!in% ala_species]
message(cat("Number of species without data (in ALA): "),
        length(count0))
message(cat("Proportion of species without data (in ALA): "),
        length(count0)/length(afd_species))


## Species by number of records (> 0)
counts <- ala_dat[,.N,by = scientificName]
nrow(counts) == length(ala_species)

count1 <- counts[which(counts$N == 1)]
message(cat("Number of species with 1 record: "),
        nrow(count1))

countLTE20 <- counts[which(counts$N > 1 & counts$N <= 20) , ]
message(cat("Number of species with less than or equal to 20 records: "),
        nrow(countLTE20))

countMT20 <- counts[which(counts$N > 20)]
message(cat("Number of species with more than 20 records: "),
        nrow(countMT20))

nrow(count1) + nrow(countLTE20) + nrow(countMT20) == nrow(counts)

## Species by year
age <- ala_dat[,.N, by = as.numeric(format(ala_dat$eventDate,'%Y'))]
names(age) <- c("year", "N")
age <- age[order(year)]

message(cat("Number of records without date: "),
        age[is.na(year)]$N)

message(cat("Number of old records (before 1990): "),
        sum(age[year < 1990]$N))

message(cat("Number of recent records (after 1990): "),
        sum(age[year >= 1990]$N))

age[is.na(year)]$N + sum(age[year < 1990]$N) + sum(age[year >= 1990]$N) == sum(age$N)
sum(age$N) == dim(ala_dat)[1]


## Obervation versus specimen data
typedat_count <- lapply(ala_species, get_ala_taxondata, 
                        get_counts_only = TRUE, 
                        specimens_only = TRUE)
typedat_count <- as.data.frame(typedat_count)
colnames(typedat_count) <- ala_species
saveRDS(typedat_count, file.path(output_dir, "typedat_count.rds"))
write.csv(typedat_count, file.path(output_dir, "typedat_count.csv"))


## Save species files



## Clean by QA assertionns 
qa <- as.data.frame(read.csv(file.path(output_dir, "qa_assertions.csv")))

exclude <- qa[which(qa$exclude == 1),]$name
keep <- qa[which(qa$keep == 1),]$name
drop_qa <- qa[which(qa$not_relevant == 1),]$name

# ## Remove generalised data (as much as possible)
# if(any(grepl("dataGeneralizations", names(ala_df)))) {
#   ala_df <- ala_df[ala_df$dataGeneralizationsOriginal == FALSE,]
# }


## No date



## Mask
reg.mask.file = file.path(output_dir, "ausmask_WGS.tif")


## Get rid of unusable long lat vals
ala_df <- ala_df[ala_df$longitude > -180 &
                   ala_df$longitude < 180 &
                   ala_df$latitude > -90 &
                   ala_df$latitude < 90, ]


# ## Remove duplicates
# if(remove_duplicates == TRUE){
#   ala_df <- ala_df[!duplicated(ala_df[ , c("longitude", "latitude")]), ]
# }
JM...  museum specimens with duplicated lat-long


## Fire extent
