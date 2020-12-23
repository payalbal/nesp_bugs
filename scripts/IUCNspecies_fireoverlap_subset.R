## IUCN priority species subset
library(data.table)
library(stringr)

setwd("./nesp_bugs/")
output_dir <- "./outputs"
# source("./scripts/remove_improper_names.R")
# source("./scripts/get_AFDsynonyms.R")

## IUCN.eval output table for EOO and AOO estimates, inlcuding NA
## Gives list of ALA species (with at least 1 record, masked by land)
## See 'Species polygons (range): ALA data' in data processing document
ala_polys <- fread(file.path(output_dir, "ala_polygons_areas.csv"))

## Species names without EOO/AOO estimates 
# spnames <-fread(file.path(output_dir, "ala_noEOOspecies.csv"))

## IUCN priority species list (provided by JM)
iucn_species <- fread("./IUCN_specieslist.csv") 
iucn_species$ScientificName <- paste(iucn_species$Genus, iucn_species$Species)
iucn_species$simple_name <- stringr::word(iucn_species$ScientificName, 1,2)

## Match ScientificName against ala_polys
dim(iucn_species[iucn_species$ScientificName  %in% ala_polys$X])
dim(iucn_species[!iucn_species$ScientificName  %in% ala_polys$X])

## Match simple_name against ala_polys 
dim(iucn_species[iucn_species$simple_name %in% ala_polys$X])
dim(iucn_species[!(iucn_species$simple_name %in% ala_polys$X)])

## Write updated IUCN list 
y <- iucn_species[iucn_species$simple_name %in% ala_polys$X]
x <- iucn_species[!(iucn_species$simple_name %in% ala_polys$X)]
dat <- rbind(y, x)
dat$nesp_data <- c(rep(1, nrow(y)), rep(0, nrow(x)))
dat$nesp_names <- rep(character(), nrow(dat))
dat[nesp_data == 1]$nesp_names = dat[nesp_data == 1]$simple_name
write.csv(dat, file = file.path(output_dir, "IUCN_specieslist_updated.csv"), 
          row.names = FALSE)

## Check for names not found manually and update csv file...
x <- iucn_species[!(iucn_species$simple_name %in% ala_polys$X)]
for (i in 121:124){
  print(i)
  
  message(cat("Looking for Genus: "),
          x[i,]$ScientificName)
  print(x[i,])
  
  message("Macthes found in ALA data: ")
  ## Subset by genus matches
  z <- ala_polys[grep(stringr::word(x[i,]$ScientificName, 1,1), ala_polys$X)]$X
  ## Find species matches in above
  print(z[grep(stringr::word(x[i,]$ScientificName, 2,2), z)])
  
  print("------------------------------------------------------------")
  print("------------------------------------------------------------")
}


z <- ala_polys[grep("Lasioglossum", ala_polys$X)]$X
z[grep("grumiculum", z)]


## Reload updated list ####
iucn_species <- fread(file.path(output_dir, "IUCN_specieslist_updated.csv")) 

## Match updates nesp_names against ala_polys 
dim(iucn_species[iucn_species$nesp_names %in% ala_polys$X])
dim(iucn_species[!(iucn_species$nesp_names %in% ala_polys$X)])

iucn_speciessub <- iucn_species[iucn_species$nesp_names %in% ala_polys$X]$nesp_names

iucn_polys <- ala_polys[ala_polys$X %in% iucn_speciessub]
dim(iucn_polys[is.na(EOO)])
dim(iucn_polys[!is.na(EOO)])
