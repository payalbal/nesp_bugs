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
# source(file.path(getwd(), "scripts/get_ala_spdata.R"))
# output_dir = "/Volumes/uom_data/nesp_bugs_data/outputs"

ala_dir <- file.path(output_dir, "ala_data")

## Load AFD taxonomic checklist ####
afd_taxonomy <- fread(file.path(output_dir, "afd_species_clean.csv"))
afd_taxon <- unique(afd_taxonomy$PHYLUM)
afd_species <- unique(afd_taxonomy$VALID_NAME)
length(afd_species)

## Mask
reg.mask.file = file.path(output_dir, "ausmask_WGS.tif")

## Merge downloaded data
alafiles <- list.files(file.path(ala_dir), include.dirs = FALSE, full.names = TRUE)

temp <- readRDS(alafiles[16])$data
dim(temp)
temp2 <- readRDS(alafiles[17])$data
dim(temp2)

'%!in%' <- function(x,y)!('%in%'(x,y))


names(temp)[names(temp) %!in% names(temp2)]
names(temp2)[names(temp2) %!in% names(temp)]


merge(temp,temp2, by = names(temp))
## subset to species



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
