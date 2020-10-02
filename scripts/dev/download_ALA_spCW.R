## Download and clean ALA data

# Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "ALA4R", "sp", "raster", "future", "future.apply")
lapply(x, require, character.only = TRUE)
rm(x)

## Server paths
source(file.path(getwd(),"nesp_bugs", "scripts/get_ala_taxondata.R"))
output_dir = file.path(getwd(), "nesp_bugs", "outputs")

# ## Local paths
# source(file.path(getwd(), "scripts/get_ala_taxondata.R"))
# output_dir = "/Volumes/uom_data/nesp_bugs_data/outputs"

ala_dir <- file.path(output_dir, "ala_data2")
if(!dir.exists(ala_dir)) dir.create(ala_dir)


## Get download and cleaning scripts from github repo
## Ref: https://github.com/cwarecsiro/gdmEngine/tree/master/gdmEngine
system(paste0("curl https://raw.githubusercontent.com/cwarecsiro/gdmEngine/master/gdmEngine/R/download_ala.R -o ", "./scripts/download_ala.R"))
system(paste0("curl https://raw.githubusercontent.com/cwarecsiro/gdmEngine/master/gdmEngine/R/download_taxalist.R -o ", "./scripts/download_ala_bytaxa.R"))
## Change line in download_ala_bytaxa.R
x <- readLines("./scripts/download_ala_bytaxa.R")
x[179] <- "        print(paste0('Searching the ALA for records of ', spp))"
cat(x, file="./scripts/download_ala_bytaxa.R", sep="\n")
system(paste0("curl https://raw.githubusercontent.com/cwarecsiro/gdmEngine/master/gdmEngine/R/merge_downloads.R -o ", "./scripts/merge_ala_downloads.R"))
system(paste0("curl https://raw.githubusercontent.com/cwarecsiro/gdmEngine/master/gdmEngine/R/filter_ALA_data.R -o ", "./scripts/filter_ala_data.R"))


## Load AFD taxonomic checklist ####
afd_taxonomy <- fread(file.path(output_dir, "afd_species_clean.csv"))
afd_taxon <- unique(afd_taxonomy$PHYLUM)
afd_species <- unique(afd_taxonomy[PHYLUM == "GNATHOSTOMULIDA"]$VALID_NAME)


## Download ALA data using species list ####
library(httr)
library(assertthat)
# library(parallel)
source("./scripts/download_ala.R")
source("./scripts/download_ala_bytaxa.R")

splist <- afd_species

download_taxalist(specieslist = splist, #trial: splist[111:112], 
                  dst = ala_dir, 
                  parallel = FALSE, 
                  background = FALSE)
## parallel = TRUE gives error because package ‘gdmEngine’ is not available (for R version 3.6.2)


invisible(future.apply::future_lapply(splist,
                                      function(x){
                                        expr = download_taxalist(x,
                                                                 dst = ala_dir, 
                                                                 parallel = FALSE, 
                                                                 background = FALSE)
                                      }))


## Merge downloaded data ###
## ***NOTE: Think of merging data by phyla to run filter function on and merge final filtered data.
library(data.table)
library(dplyr)
source("./scripts/merge_ala_downloads.R")
ala.data <- merge_downloads(src = file.path(ala_dir, "raw_files/"), output.folder = ala_dir,
                            output.name = paste0("merged_data_", Sys.Date()),
                            keep_unzip = FALSE,
                            parallel = FALSE, 
                            verbose = TRUE)


temp <- load("/Volumes/uom_data/nesp_bugs_data/outputs/ala_data2/ALA_download_log_2020-10-01.RData")


## Filter merged data
library(sp)
library(raster)
aus.mask <- readRDS(file.path(data_path,"RData", "mask_aus.rds"))
ala.data <- read.csv(list.files(ala_dir, pattern = c("merged.*\\.csv"), full.names = TRUE))
source("./scripts/filter_ala_data.R")
filtered.data <- filter_ALA_data(ALA.download.data = ala.data$data,             
                                 output.folder = ala_dir,       
                                 output.name = "filtered_data_",  
                                 domain.mask = aus.mask,                   
                                 earliest.year = 1950,
                                 spatial.uncertainty.m = 1000,
                                 select.fields = NULL,
                                 verbose=TRUE)

## Subset filtered data to species with >=20 records
dat <- as.data.table(filtered.data)
dat.counts <- dat[,.N, by = scientificName]
sp.names <- dat.counts[N >= 20]$scientificName
dat <- dat[scientificName %in% sp.names]

saveRDS(dat, file.path(ala_dir, "ala_data.rds"))
write.csv(dat, file.path(ala_dir, "ala_data.csv"), row.names=FALSE) ## larger file.  



