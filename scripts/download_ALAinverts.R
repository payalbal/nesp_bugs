## References:
## https://github.com/Doi90/bushfireSOS/blob/master/R/load_pres_bg_data.R
## https://cloud.r-project.org/web/packages/ALA4R/ALA4R.pdf


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
source(file.path(getwd(),"nesp_bugs", "scripts/get_ala_data.R"))
output_dir = file.path(getwd(), "nesp_bugs", "outputs")

# ## Local paths
# source(file.path(getwd(), "scripts/get_ala_data.R"))
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
                                        tmp <- tryCatch(expr = get_ala_data(x,
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
                                                              append = T)
                                                        })
                                      }))
end.time <- Sys.time()
end.time - start.time




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






