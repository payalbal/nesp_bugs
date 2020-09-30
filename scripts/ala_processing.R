## Clean downloaded ALA data and save by species


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

# ## https://stackoverflow.com/questions/49215193/r-error-cant-join-on-because-of-incompatible-types
# matchColClasses <- function(df1, df2) {
#   
#   sharedColNames <- names(df1)[names(df1) %in% names(df2)]
#   sharedColTypes <- sapply(df1[,sharedColNames], class)
#   
#   for (n in sharedColNames) {
#     class(df2[, n]) <- sharedColTypes[n]
#   }
#   
#   return(df2)
# }


## Load AFD taxonomic checklist ####
afd_taxonomy <- fread(file.path(output_dir, "afd_species_clean.csv"))
afd_taxon <- unique(afd_taxonomy$PHYLUM)
afd_species <- unique(afd_taxonomy$VALID_NAME)


## Merge downloaded data ####
ala <- list.files(file.path(ala_dir), include.dirs = FALSE, full.names = TRUE)

## Sort files by size
ala <- names(sort(sapply(ala, file.size)))

## Create data tables with first dataset
f <- readRDS(ala[1])
dat_cols <- names(f$data)
dat_counts <- t(as.data.frame(f$counts))
colnames(dat_counts) <- names(f$counts)
ala_merged <- as.data.table(f$data)
dim(ala_merged)
coltypes <- sapply(ala_merged[,..dat_cols], class)
rm(f)

## Merge all datasets
for (i in 2:3) {
  f <- ala[i]
  f <- readRDS(f)
  c <- t(as.data.frame(f$counts))
  f <- as.data.table(f$data)
  
  dat_counts <- rbind(dat_counts, c)      
  
  message(cat("Processing dataset ", i, " :", ala[i], " ...\n"))
  
  message(cat("Matchinng column classes..."))
  f_coltypes <- sapply(f[,..dat_cols], class)
  f_mismatch <- f_coltypes[!(coltypes == f_coltypes)]
  
  for (n in names(f_mismatch)) {
    class(f[, n]) <- coltypes[n]
  }
  
  # for (n in dat_cols) {
  #   class(f[, n]) <- coltypes[n]
  # }
  
  message(cat("Merging dataset ..."))
  ala_merged <- merge(ala_merged, f, all = TRUE)
  message(cat("Dimensions of merged data: "),
          dim(ala_merged)[1])
  rm(c,f) 
}

all(names(ala_merged)==names(f))


names(dat_counts) <- afd_taxon
saveRDS(ala_merged, file = file.path(output_dir, "merged_ala.rds"))
write.csv(ala_merged, file = file.path(output_dir, "merged_ala.csv"))





# names(temp)[names(temp) %!in% names(temp2)]
# for (n in f_class){
#   class(f[,..n]) <- coltypes[n]
# }
# f <- f %>%  
#   mutate(names(f_class[j]) = eval(paste0("as.", org_class[j], "(", names(f_class[j]), ")")))names(temp2)[names(temp2) %!in% names(temp)]


merge(temp,temp2, by = names(temp))
## subset to species


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
