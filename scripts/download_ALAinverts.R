## References:
## https://github.com/Doi90/bushfireSOS/blob/master/R/load_pres_bg_data.R
## https://cloud.r-project.org/web/packages/ALA4R/ALA4R.pdf


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "ALA4R", "sp", "raster")
lapply(x, require, character.only = TRUE)
rm(x)

## Server paths
source(file.path(getwd(),"nesp_bugs", "scripts/get_ala_data.R"))
output_dir = file.path(getwd(), "nesp_bugs", "outputs")

## Local paths
source(file.path(getwd(), "scripts/get_ala_data.R"))
output_dir = "/Volumes/uom_data/nesp_bugs_data/outputs"

ala_dir <- file.path(output_dir, "ala_data")
if(!dir.exists(ala_dir)) dir.create(ala_dir)


## Load AFD taxonnomic checklist ####
afd_taxonomy <- fread(file.path(output_dir, "afd_species_clean.csv"))
afd_species <- unique(afd_taxonomy$VALID_NAME)

# ## Trial species
# afd_species <- unique(afd_taxonomy[FAMILY == toupper("Salticidae")]$VALID_NAME)
# species = "Zenodorus metallescens"
# species = "Sondra variabilis"
# ## Check subspecies downloads...
# afd_species <- afd_species[grep("(", afd_species, fixed = TRUE)]
# species = afd_species[110]


## Download data by species
for (species in afd_species[1:10]){
  
  # get_ala_data(species,
  #              extra_fields = TRUE,
  #              specimens_only = TRUE,
  #              remove_duplicates = TRUE,
  #              dst = ala_dir,
  #              save.map = TRUE,
  #              reg.mask.file = file.path(output_dir, "ausmask_WGS.tif"),
  #              email = "bal.payal@gmail.com")
  
  tryCatch(get_ala_data(species,
                        extra_fields = TRUE,
                        specimens_only = TRUE,
                        remove_duplicates = TRUE,
                        dst = ala_dir,
                        save.map = TRUE,
                        reg.mask.file = file.path(output_dir, "ausmask_WGS.tif"),
                        email = "bal.payal@gmail.com"),
           error = function(e){ 
             paste("\nNot run: no records found for", species, "in ALA")
           })
}

## Get data ####



## Check for duplicates


## Cleaning
## Check taxonnomic issues
## Check geographic issues
## Check for assertions
## Check for generalisations...
if(any(grepl("assertions", names(df)))){
  names(df)[which(grepl("assertions", names(df)))]
}

## Check recorded issues with geographic or taxonomic fields
## Fields: "taxonomic_kosher","geospatial_kosher"

## Save species data



## Test plot
aus.mask <- raster(file.path(output_dir, "ausmask_WGS.tif"))
plot(aus.mask, col = "grey", axes = FALSE, box = FALSE, legend = FALSE)
points(ala_df[,.(longitude, latitude)], pch = 4, col = "red", cex = 0.5)


## Explore ALAL fieldas
ala_fields("occurrence", as_is=TRUE)
names(ala_fields("occurrence"))
ala_fields("occurrence")$name
"assertions" %in% ala_fields("occurrence")$name
ala_fields()$name[grep("assertions", ala_fields("occurrence")$name)] 
ala_fields()$description[grep("assertions", ala_fields("occurrence")$name)]


library(spocc)
library(rgbif)

key <- name_suggest(q='Gnathifera', rank='phylum')$key[1]
occ_search(taxonKey=key, limit=20)
occ_data(scientificName=..., limit=20)

spocc::occ(query = "Chordata",
           from = "gbif",
           geometry = "POLYGON((112.76 -10.23, 155.48 -10.23, 155.48 -44.28, 112.76 -44.28, 112.76 -10.23))",
           limit = 10)


## List names of native species 
data_sp <- read.csv(file.path(data_path, "all_sp_records-2020-03-19.csv"))
data_sp <- data_sp[data_sp$Invasive %in% "",]
data_sp <- data_sp[,c("Species.Name", "Kingdom", 
                      "Phylum", "Class", "Order", 
                      "Family", "Genus", "Conservation")]




## EXTRAS: From CWare's ala_functions
## Merge downloaded data
## system(paste0("curl https://raw.githubusercontent.com/cwarecsiro/gdmEngine/master/gdmEngine/R/merge_downloads.R -o ", "./scripts/merge_ala_downloads.R"))
library(data.table)
library(dplyr)
source("./scripts/merge_ala_downloads.R")
ala.data <- merge_downloads(src = file.path(ala_path, "raw_files/"), output.folder = ala_path,
                            output.name = paste0("merged_data_", Sys.Date()),
                            keep_unzip = FALSE,
                            parallel = FALSE, 
                            verbose = TRUE)

## Filter merged data
## system(paste0("curl https://raw.githubusercontent.com/cwarecsiro/gdmEngine/master/gdmEngine/R/filter_ALA_data.R -o ", "./scripts/filter_ala_data.R"))
library(sp)
library(raster)
aus.mask <- readRDS(file.path(data_path,"RData", "mask_aus.rds"))
ala.data <- read.csv(list.files(ala_path, pattern = c("merged.*\\.csv"), full.names = TRUE))
source("./scripts/filter_ala_data.R")
filtered.data <- filter_ALA_data(ALA.download.data = ala.data$data,             
                                 output.folder = ala_path,       
                                 output.name = "filtered_data_",  
                                 domain.mask = aus.mask,                   
                                 earliest.year = 1950,
                                 spatial.uncertainty.m = 2000,
                                 select.fields = NULL,
                                 verbose=TRUE)

## Subset filtered data to species with >=20 records
dat <- as.data.table(filtered.data)
dat.counts <- dat[,.N, by = scientificName]
sp.names <- dat.counts[N >= 20]$scientificName
dat <- dat[scientificName %in% sp.names]

saveRDS(dat, file.path(ala_path, "ala_data.rds"))
write.csv(dat, file.path(ala_path, "ala_data.csv"), row.names=FALSE) ## larger file.  



