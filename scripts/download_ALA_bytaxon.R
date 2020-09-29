## References:
## https://cloud.r-project.org/web/packages/ALA4R/ALA4R.pdf


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "ALA4R", "sp", "raster", "future", "future.apply")
lapply(x, require, character.only = TRUE)
rm(x)

# ## Server paths
# source(file.path(getwd(),"nesp_bugs", "scripts/get_ala_taxondata.R"))
# output_dir = file.path(getwd(), "nesp_bugs", "outputs")

## Local paths
source(file.path(getwd(), "scripts/get_ala_taxondata.R"))
output_dir = "/Volumes/uom_data/nesp_bugs_data/outputs"

ala_dir <- file.path(output_dir, "ala_data")
if(!dir.exists(ala_dir)) dir.create(ala_dir)

## No data species txt file
nodatalog <- file.path(output_dir, "nodata_log.txt")
writeLines(c("species0"), nodatalog)


## Load AFD taxonomic checklist ####
afd_taxonomy <- fread(file.path(output_dir, "afd_species_clean.csv"))
afd_taxon <- unique(afd_taxonomy$PHYLUM)
length(afd_taxon)


## Count records only ####
taxon.counts <- lapply(afd_taxon,
                       function(x){
                         tmp <- tryCatch(expr = get_ala_taxondata(x,
                                                                  get_counts_only = TRUE,
                                                                  specimens_only = TRUE,
                                                                  dst = ala_dir),
                                         error = function(e){ 
                                           print(paste("\nNot run: no records for", x))
                                           
                                           cat(paste(x, "\n"),
                                               file = nodatalog, 
                                               append = T)
                                         })
                       })

taxon.counts <- as.data.frame(taxon.counts)
colnames(taxon.counts) <- afd_taxon
write.csv(taxon.counts, file.path(output_dir, "taxon_counts.csv"))


## Download data by taxon ####
fields <-  c("id","data_resource_uid","data_resource",
             "institution_uid","institution_name",
             "collection_uid","collection_name",
             "license","catalogue_number",
             "taxon_concept_lsid",
             "raw_taxon_name","raw_common_name",
             "taxon_name","common_name","rank",
             "kingdom","phylum","class","order",
             "family","genus","species","subspecies",
             "institution_code","collection_code",
             "raw_locality","raw_datum",
             "raw_latitude","raw_longitude",
             "latitude","longitude",
             "coordinate_precision","coordinate_uncertainty",
             "country","state","cl959","cl21","cl1048",
             "min_elevation_d","max_elevation_d",
             "min_depth_d","max_depth_d",
             "individual_count","collector",
             "occurrence_date","year","month",
             "verbatim_event_date",
             "basis_of_record","raw_basis_of_record",
             "occurrence_status",
             "raw_sex","preparations",
             "outlier_layer",
             "taxonomic_kosher","geospatial_kosher")

future::availableCores()
plan(multiprocess, workers = length(afd_taxon))
options(future.globals.maxSize = +Inf) ## CAUTION: Set this to a value, e.g. availablecores-1?/RAM-10?

start.time <- Sys.time()
invisible(future.apply::future_lapply(afd_taxon,
                                      function(x){
                                        tmp <- tryCatch(expr = get_ala_taxondata(x,
                                                                                 get_counts_only = FALSE,
                                                                                 fields = fields,
                                                                                 extra_fields = TRUE,
                                                                                 specimens_only = TRUE,
                                                                                 dst = ala_dir,
                                                                                 email = paste0("bal.payal+", sample(1:100, 1), "@gmail.com")),
                                                        error = function(e){ 
                                                          print(paste("\nNot run: no records for", x))
                                                          
                                                          cat(paste(x, "\n"),
                                                              file = nodatalog, 
                                                              append = T)
                                                        })
                                      }))
end.time <- Sys.time()
end.time - start.time ## Time difference of 58.61595 mins

# 
# ## EXTRA
# ## Explore ALAL fieldas
# ala_fields("occurrence", as_is=TRUE)
# names(ala_fields("occurrence"))
# ala_fields("occurrence")$name
# "assertions" %in% ala_fields("occurrence")$name
# ala_fields()$name[grep("assertions", ala_fields("occurrence")$name)] 
# ala_fields()$description[grep("assertions", ala_fields("occurrence")$name)]






