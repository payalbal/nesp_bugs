## Clean downloaded ALA data and save by species
## Notes: Each occurrence record has a record id, we can use `occurrence_details` to get additional information if you need something extra later


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

ala_dir <- file.path(output_dir, "ala_data")

## Functions
'%!in%' <- function(x,y)!('%in%'(x,y))


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
write.csv(ala_merged, file = file.path(output_dir, paste0("merged_ala_", Sys.Date(),".csv")))



# names(temp)[names(temp) %!in% names(temp2)]
# for (n in f_class){
#   class(f[,..n]) <- coltypes[n]
# }
# f <- f %>%  
#   mutate(names(f_class[j]) = eval(paste0("as.", org_class[j], "(", names(f_class[j]), ")")))names(temp2)[names(temp2) %!in% names(temp)]


## Subset species as by AFD checklist ####
ala_merged_raw <- ala_merged <- readRDS(file.path(output_dir, paste0("merged_ala_", Sys.Date(),".rds")))
length(unique(ala_merged_raw$scientificName))

## Load AFD taxonomic checklist
afd_taxonomy <- fread(file.path(output_dir, "afd_species_clean.csv"))
afd_taxon <- unique(afd_taxonomy$PHYLUM)
afd_species <- unique(afd_taxonomy$VALID_NAME)
# length(afd_species[grep("(", afd_species, fixed = TRUE)])
# sp_words <- sapply(strsplit(as.character(afd_species), " "), length)
# length(afd_species[which(sp_words == 3)])

ala_merged <- ala_merged[scientificName %in% afd_species]

## Species with data
sp_withdata <- unique(ala_merged$scientificName)
length(sp_withdata)

## Species without data
temp <- ala_merged_raw[scientificName %!in% afd_species]
sp_nodata <- afd_species %!in% sp_withdata
length(sp_nodata)

## Mask
reg.mask.file = file.path(output_dir, "ausmask_WGS.tif")



## duplicates
# ## Remove duplicates
# if(remove_duplicates == TRUE){
#   ala_df <- ala_df[!duplicated(ala_df[ , c("longitude", "latitude")]), ]
# }
JM...  museum specimens with duplicated lat-long



## Count files ####
## Species with data
sp_data <- list.files(file.path(ala_dir, "maps"), include.dirs = FALSE)
# list.files(ala_dir, include.dirs = FALSE) ## lists "maps" folder as well
sp_data <- gsub(".pdf", "", sp_data)
length(sp_data)

## Species without data
## No data species txt file
nodatalog <- file.path(output_dir, "nodataspecies_log.txt")
writeLines(c("species0"), nodatalog)

sp_nodata <- read.csv(nodatalog)
sp_nodata <- sp_nodata$species0
length(sp_nodata)

## Clean by species...

## Get rid of unusable long lat vals
ala_df <- ala_df[ala_df$longitude > -180 &
                   ala_df$longitude < 180 &
                   ala_df$latitude > -90 &
                   ala_df$latitude < 90, ]

# ## Remove generalised data (as much as possible)
# if(any(grepl("dataGeneralizations", names(ala_df)))) {
#   ala_df <- ala_df[ala_df$dataGeneralizationsOriginal == FALSE,]
# }

# ## Remove duplicates
# if(remove_duplicates == TRUE){
#   ala_df <- ala_df[!duplicated(ala_df[ , c("longitude", "latitude")]), ]
# }



# for (n in dat_cols) {
#   class(f[, n]) <- coltypes[n]
# }

# f[,verbatimEventDate:=NULL]
# f[,verbatimEventDate:=NULL]
# 
# f[,verbatimEventDate := lubridate::as_date(verbatimEventDate)]
