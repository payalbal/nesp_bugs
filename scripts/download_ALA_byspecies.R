## DOWNLOAD ALA DATA BY SPECIES NAMES

## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "ALA4R", "sp", "raster", "future", "future.apply")
lapply(x, require, character.only = TRUE)
rm(x)
future::availableCores()

## Server paths
source(file.path(getwd(),"nesp_bugs", "scripts/get_ala_spdata.R"))
output_dir = file.path(getwd(), "nesp_bugs", "outputs")

# ## Local paths
# source(file.path(getwd(), "scripts/get_ala_spdata.R"))
# output_dir = "/Volumes/uom_data/nesp_bugs_data/outputs"

ala_dir <- file.path(output_dir, "ala_data")
if(!dir.exists(ala_dir)) dir.create(ala_dir)

## No data species txt file
nodatalog <- file.path(output_dir, "nodataspecies_log.txt")
writeLines(c("species0"), nodatalog)


## Load AFD taxonnomic checklist ####
afd_taxonomy <- fread(file.path(output_dir, "afd_species_clean.csv"))
afd_species <- unique(afd_taxonomy$VALID_NAME)
length(afd_species)

# ## Trial species
# afd_species <- unique(afd_taxonomy[FAMILY == toupper("Salticidae")]$VALID_NAME)
# species = "Zenodorus metallescens"
# species = "Sondra variabilis"
# ## Check subspecies downloads...
# afd_species <- afd_species[grep("(", afd_species, fixed = TRUE)]
# species = afd_species[110]

## Download data by species
plan(multiprocess, workers = future::availableCores()-2)
options(future.globals.maxSize = +Inf) ## CAUTION: Set this to a value, e.g. availablecores-1?/RAM-10?

start.time <- Sys.time()
invisible(future.apply::future_lapply(afd_species,
                                      function(x){
                                        tmp <- tryCatch(expr = get_ala_spdata(x,
                                                                              extra_fields = TRUE,
                                                                              specimens_only = TRUE,
                                                                              remove_duplicates = TRUE,
                                                                              dst = ala_dir,
                                                                              save.map = TRUE,
                                                                              reg.mask.file = file.path(output_dir, "ausmask_WGS.tif"),
                                                                              email = paste0("bal.payal+", sample(1:100, 1), "@gmail.com")),
                                                        error = function(e){ 
                                                          print(paste("\nNot run: no records for", x))
                                                          
                                                          cat(paste(x, "\n"),
                                                              file = nodatalog, 
                                                              append = TRUE)
                                                        })
                                      }))
end.time <- Sys.time()
end.time - start.time


## Count files ####
## Species with data
sp_data <- list.files(file.path(ala_dir, "maps"), include.dirs = FALSE)
# list.files(ala_dir, include.dirs = FALSE) ## lists "maps" folder as well
sp_data <- gsub(".pdf", "", sp_data)
length(sp_data)

## Species without data
sp_nodata <- read.csv(nodatalog)
sp_nodata <- sp_nodata$species0
length(sp_nodata)




## EXTRA
## Explore ALAL fieldas
ala_fields("occurrence", as_is=TRUE)
names(ala_fields("occurrence"))
ala_fields("occurrence")$name
"assertions" %in% ala_fields("occurrence")$name
ala_fields()$name[grep("assertions", ala_fields("occurrence")$name)] 
ala_fields()$description[grep("assertions", ala_fields("occurrence")$name)]






