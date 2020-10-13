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
afd_species <- sort(unique(afd_taxonomy$VALID_NAME))
message(cat("Number of species in AFD checklist: "),
        length(afd_species))


## Load ALA data
ala_dat <- list.files(output_dir, 
                      pattern = "clean_ala*.*.rds$", 
                      full.names = TRUE)
ala_dat <- readRDS(ala_dat)
ala_species <- sort(unique(ala_dat$scientificName))
message(cat("Number of unique species in ALA data: "),
        length(ala_species))


## Obervation versus specimen data in ALA database ####

## Get counts from ALA database
# typecounts <- lapply(afd_species, get_ala_taxondata, 
#                         get_counts_only = TRUE, 
#                         specimens_only = TRUE)
# 
# typecounts <- t(as.data.frame(typecounts))
# typecounts <- cbind(afd_species, typecounts)
# typecounts <- typecounts[ order(typecounts$species), ]
# typecounts <- as.data.table(typecounts)
# colnames(typecounts)[1] <- "species"
# saveRDS(typecounts, file.path(output_dir, "typecounts.rds"))
# write.csv(typecounts, file.path(output_dir, "typecounts.csv"))
typecounts <- as.data.table(readRDS(file.path(output_dir, "typecounts.rds")))

## Summarise: All data
message(cat("Number of species with 0 records in ALA db: "),
        nrow(typecounts[all == 0]))
message(cat("Number of species with 1 record in ALA db: "),
        nrow(typecounts[all == 1]))
message(cat("Number of species with more than 1 and less than or equal to 20 records in ALA db: "),
        nrow(typecounts[all > 1 & all <= 20]))
message(cat("Number of species with more than 20 records in ALA db: "),
        nrow(typecounts[all > 20]))

## Summarise: Observation data
message(cat("Number of species with 0 observation records in ALA db: "),
        nrow(typecounts[observation == 0]))
message(cat("Number of species with 1 observation record in ALA db: "),
        nrow(typecounts[observation == 1]))
message(cat("Number of species with more than 1 and less than or equal to 20 observation records in ALA db: "),
        nrow(typecounts[observation > 1 & observation <= 20]))
message(cat("Number of species with more than 20 observation records in ALA db: "),
        nrow(typecounts[observation > 20]))

## Summarise: Specimen data
message(cat("Number of species with 0 specimen records in ALA db: "),
        nrow(typecounts[specimen == 0]))
message(cat("Number of species with 1 specimen record in ALA db: "),
        nrow(typecounts[specimen == 1]))
message(cat("Number of species with more than 1 and less than or equal to 20 specimen records in ALA db: "),
        nrow(typecounts[specimen > 1 & specimen <= 20]))
message(cat("Number of species with more than 20 specimen records in ALA db: "),
        nrow(typecounts[specimen > 20]))


## Summarise cleaned ALA data ####

message(cat("Number of species with records in cleaned ALA data: "),
        length(ala_species))
message(cat("Number of AFD species found in ALA: "),
        sum(afd_species %in% ala_species))
message(cat("Number of AFD species not found in ALA: "),
        sum(afd_species %!in% ala_species))

sum(afd_species %in% ala_species) + sum(afd_species %!in% ala_species) == length(afd_species)

## Species with 0 records
count0 <- afd_species[afd_species %!in% ala_species]
message(cat("Number of AFD species without records in cleaned ALA data: "),
        length(count0))
write.csv(count0, file = file.path(output_dir, "nodata_AFDspecies.csv"))

## Species by number of records (> 0)
counts <- ala_dat[,.N,by = scientificName]
nrow(counts) == length(ala_species)
write.csv(counts, file = file.path(output_dir, "datacounts_ALAspecies.csv"))

count1 <- counts[which(counts$N == 1)]
message(cat("Number of species with 1 record in cleaned ALA data: "),
        nrow(count1))

countLTE20 <- counts[which(counts$N > 1 & counts$N <= 20) , ]
message(cat("Number of species with more than 1 and less than or equal to 20 records in cleaned ALA data: "),
        nrow(countLTE20))

countMT20 <- counts[which(counts$N > 20)]
message(cat("Number of species with more than 20 records in cleaned ALA data: "),
        nrow(countMT20))

nrow(count1) + nrow(countLTE20) + nrow(countMT20) == nrow(counts)

## Species by year
age <- ala_dat[,.N, by = as.numeric(format(ala_dat$eventDate,'%Y'))]
names(age) <- c("year", "N")
age <- age[order(year)]
write.csv(age, file = file.path(output_dir, "yearcounts_ALAspecies.csv"))

message(cat("Number of records without date in cleaned ALA data: "),
        age[is.na(year)]$N)

message(cat("Number of old records (before 1990) in cleaned ALA data: "),
        sum(age[year < 1990]$N))

message(cat("Number of recent records (after 1990) in cleaned ALA data: "),
        sum(age[year >= 1990]$N))

age[is.na(year)]$N + sum(age[year < 1990]$N) + sum(age[year >= 1990]$N) == sum(age$N)
sum(age$N) == dim(ala_dat)[1]