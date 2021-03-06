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

## Server paths
source(file.path(getwd(),"nesp_bugs", "scripts/get_ala_taxondata.R"))
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")

# ## Local paths
# source(file.path(getwd(), "scripts/get_ala_taxondata.R"))
# output_dir = "/Volumes/uom_data/nesp_bugs_data/outputs"

ala_dir <- file.path(bugs_dir, "ALA", "download_bytaxon")
if(!dir.exists(ala_dir)) dir.create(ala_dir)

'%!in%' <- function(x,y)!('%in%'(x,y))

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
                                                                                 include_assertions = TRUE,
                                                                                 specimens_only = TRUE,
                                                                                 dst = ala_dir,
                                                                                 email = paste0("bal.payal+", sample(1:100, 1), "@gmail.com")),
                                                        error = function(e){ 
                                                          print(paste("\nNot run: no records for", x))
                                                          
                                                          cat(paste(x, "\n"),
                                                              file = nodatalog, 
                                                              append = T)
                                                        })
                                      }, future.seed = TRUE))
end.time <- Sys.time()
end.time - start.time


## Checks
## Check field names across all downnloads
ala <- list.files(file.path(ala_dir), include.dirs = FALSE, full.names = TRUE)
dat_cols <- names(readRDS(ala[1])$data)
for (i in 2:length(ala)) {
  f <- readRDS(ala[i])$data
  message(cat("Checking dataset ", i, " :", ala[i], " ...\n"),
          cat("field names as per specified list = "),
          all(dat_cols==names(f)))
}

all(fields %in% names(f$data)) ## beacuse names are different even if fields correspond
fields[which(fields %!in% names(f$data))]


## Check field classes across all downloads
ala <- list.files(file.path(ala_dir), include.dirs = FALSE, full.names = TRUE)
ala <- names(sort(sapply(ala, file.size)))
f <- readRDS(ala[1])$data
dat_cols <- names(f)
coltypes <- sapply(f[,..dat_cols], class)

for (i in 2:length(ala)) {
  f <- readRDS(ala[i])$data
  f_coltypes <- sapply(f[,..dat_cols], class)
  message(cat("Checking dataset ", i, " :", ala[i], " ...\n"),
          cat("field classes as per specified list = "),
          all(coltypes==f_coltypes))
}


## Merge downloaded data ####
ala <- list.files(file.path(ala_dir), include.dirs = FALSE, full.names = TRUE)

## Sort files by size
ala <- names(sort(sapply(ala, file.size), decreasing = TRUE))

## Create data table with Arthropoda dataset (biggest dataset)
f <- readRDS(ala[1])
dat_cols <- names(f$data)
dat_counts <- t(as.data.frame(f$counts))
dat_counts
colnames(dat_counts) <- names(f$counts)
ala_merged <- as.data.table(f$data)
dim(ala_merged)
coltypes <- sapply(ala_merged[,..dat_cols], class)
rm(f)

## Merge all datasets
for (i in 2:length(ala)) {
  f <- ala[i]
  f <- readRDS(f)
  c <- t(as.data.frame(f$counts))
  f <- as.data.table(f$data)
  
  message(cat("Processing dataset ", i, " :", ala[i], " ..."))
  
  dat_counts <- rbind(dat_counts, c)      
  message(cat("Total number of clean records: "),
          sum(dat_counts[,4]))
  
  message(cat("Matching column classes..."))
  f_coltypes <- as.character(sapply(f[,..dat_cols], class))
  f_mismatch <- which(!(coltypes == f_coltypes))
  for (k in f_mismatch) set(f, j = k, value = eval(parse(text=paste0("as.", coltypes[k], "(f[[k]])"))))
  
  f_coltypes <- as.character(sapply(f[,..dat_cols], class))
  message(cat("Checking columns classes are same... "),
          all(f_coltypes==coltypes))
  
  message(cat("Merging dataset ..."))
  ala_merged <- rbind(ala_merged, f, use.names = TRUE, fill=TRUE)
  message(cat("Dimensions of merged data: "),
          dim(ala_merged)[1])
  message("\n")
  rm(c,f) 
}


## Check
message(cat("#rows in merged data = sum of cleaned records : "),
        nrow(ala_merged) == sum(dat_counts[,4]))

## Save outputs
rownames(dat_counts) <- gsub(".rds", "", basename(ala))
saveRDS(dat_counts, file = file.path(output_dir, "ala_counts.rds"))

saveRDS(ala_merged, file = file.path(output_dir, paste0("merged_ala_", Sys.Date(),".rds")))
write.csv(ala_merged, file = file.path(output_dir, paste0("merged_ala_", Sys.Date(),".csv")), row.names = FALSE)



# ## EXTRA
# ## Explore ALA fields
# ala_fields("occurrence", as_is=TRUE)
# names(ala_fields("occurrence"))
# ala_fields("occurrence")$name
# "assertions" %in% ala_fields("occurrence")$name
# ala_fields()$name[grep("assertions", ala_fields("occurrence")$name)] 
# ala_fields()$description[grep("assertions", ala_fields("occurrence")$name)]

## ALA assertions
ala_fields("assertions",as_is=TRUE)
write.csv(ala_fields("assertions",as_is=TRUE), file = "./output/assertions.csv")






