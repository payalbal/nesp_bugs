## Compare species in AFD against GBIF to find extent of mismatch

## Set working environmet ####
x <- c("sp", "raster", "data.table")
lapply(x, require, character.only = TRUE)

gsdms_path <- "/Volumes/uom_data/gsdms_data/gbif" 
data_path <- "/Volumes/uom_data/nesp_bugs_data" 
source("./scripts/remove_improper_names.R")
'%!in%' <- function(x,y)!('%in%'(x,y))
nullval <- function(x){ifelse(is.null(x), NA, list(x))}
out <-  list() ## output object

## Load taxonomy checlists ####
gbif_taxonomy <- fread(file.path(data_path, "GBIF/gbif_invert_taxonomy.csv"))
afd_taxonomy <- fread("./output/afd_species_clean.csv")
## FOR ALA see: headings.csv in ./data folder (copied over form any ALA data download)


## Test taxa ####
## Family Salticidae. This is a speciose family and has had some new 
##    species added in 2020, so should be a good test for 
##    completeness of AFD species lists and comparison with GBIF.
## Family Charopidae (snails), which includes somewhere between 
##    400 to 1000 Oz species
## Genera Rhophodon (~ 10 spp.) and Gyrocochlea (~ 30 spp.) 
##    both in the family Charopidae; Genus Austrochloritis (~ 30 spp.) 
##    in family Camaenidae. These genera happen to have quite a few 
##    fire-affected spp.
## Austrochloritis darwin core: https://www.gbif.org/occurrence/download/0057383-200613084148143
test_taxon <- c("Austrochloritis", "Gyrocochlea", "Rhophodon", "Charopidae", "Salticidae")

for (i in 1:length(test_taxon)) {
  message("-------------------------------------------")
  message(cat("Processing test_taxon: ", test_taxon[i]))
  message("-------------------------------------------")
  
  ## GBIF ####
  if (test_taxon[i] %in% c("Rhophodon", "Gyrocochlea", "Austrochloritis")) {
    test_taxon_gbif <- gbif_taxonomy[genus == test_taxon[i]]
  } else{
    test_taxon_gbif <- gbif_taxonomy[family == test_taxon[i]]
  }
  
  message(cat("dim(test_taxon_gbif) = ", 
              dim(test_taxon_gbif)))
  
  test_taxon_gbif <- test_taxon_gbif[taxonRank=="species"] # only comparing species names
  
  ## >> GBIF taxonomy checklist cleaning ####
  ## Remove improper names
  all_species <- test_taxon_gbif$canonicalName
  
  message(cat("Dimensions for raw checklist: ", dim(test_taxon_gbif)))
  message(cat("Unique canonicalName in raw checklist: ", 
              length(unique(all_species))))
  
  species_record <- remove_improper_names(as.character(all_species),
                                          allow.higher.taxa = FALSE,
                                          allow.subspecies = TRUE)
  
  ## Update taxonomy based on selected species
  species <- as.character(na.omit(species_record$updated_list))
  test_taxon_gbif <- test_taxon_gbif[which(test_taxon_gbif$canonicalName %in% species),]
  
  
  ## Remove invasive species as per GRIIS
  ## Source: https://lists.ala.org.au/speciesListItem/list/dr9884#list
  griis_species <- read.csv(file.path(data_path, "ALA/GRIIS_Global_Register_of_Introduced_and_Invasive_Species_Australia.csv"))
  invasives <- which(test_taxon_gbif$canonicalName %in% griis_species$Supplied.Name)
  
  message("Cleaning checklist for invasive species as per GRIIS - Australia...")
  
  if (length(invasives) !=0) {
    message(cat("Number of species listed in GRIIS - Australia: ", 
                length(invasives)))
    message(cat("GBIF species included in GRIIS - Australia: ", 
                test_taxon_gbif$canonicalName[invasives]))
    '%!in%' <- function(x,y)!('%in%'(x,y))
    test_taxon_gbif <- test_taxon_gbif[which(test_taxon_gbif$canonicalName %!in% griis_species$Supplied.Name),]
  } else {
    message("No invasive species (as per GRIIS) listed in checklist")
  }
  
  
  ## >> Cleaned GBIF taxonomy species list ####
  ## Darwin core: https://dwc.tdwg.org/list/#dwc_scientificName
  ## GBIF occurrence API: https://www.gbif.org/developer/occurrence#p_scientificName
  message(cat("Dimensions for cleaned checklist: ", dim(test_taxon_gbif)))
  
  gbif_tax_scientificName <- unique(test_taxon_gbif$scientificName)
  gbif_tax_canonicalName <- unique(test_taxon_gbif$canonicalName)
  
  
  ## >> GBIF occurrence data cleaning ####
  ## See meta.xml file from any GBIF data download for list of terms
  ## Download occuurrence data by bounding box in GBIF
  ## Rerun query > Download Darwin Core 
  ## Ref for terms in DC: https://dwc.tdwg.org/list/
  dat <- fread(file.path(data_path, "db_compare" , test_taxon[i], "darwincore","occurrence.txt"))
  # unique(dat$taxonRank)
  dat <- dat[dat$taxonRank == "SPECIES",]
  message(cat("Dimensions of data: ", 
              dim(dat)))
  
  ## Plot data on mask
  message("Plotting data...")
  aus.mask <- raster("./output/ausmask_WGS.tif")
  plot(aus.mask, col = "grey", axes = FALSE, box = FALSE, legend = FALSE)
  points(dat[,.(decimalLongitude,decimalLatitude)], pch = 4, col = "red", cex = 0.5)
  
  
  ## Filter data by mask extent
  dat <- dat[which(decimalLongitude > extent(aus.mask)@xmin)]
  dat <- dat[which(decimalLongitude < extent(aus.mask)@xmax)]
  dat <- dat[which(decimalLatitude > extent(aus.mask)@ymin)]
  dat <- dat[which(decimalLatitude < extent(aus.mask)@ymax)]
  message(cat("Dimensions of data: ", 
              dim(dat)))
  
  
  ## Filter data by location on spatial grid
  sp <- SpatialPoints(dat[,.(decimalLongitude,decimalLatitude)])
  grd.pts<-extract(aus.mask, sp)
  dat <- dat[!is.na(grd.pts),]
  points(dat[,.(decimalLongitude,decimalLatitude)], col = "blue")
  message(cat("Dimensions of data: ", 
              dim(dat)))  
  
  ## >> Cleaned GBIF data species list ####
  ## Darwin core: https://dwc.tdwg.org/list/#dwc_scientificName
  ## GBIF occurrence API: https://www.gbif.org/developer/occurrence#p_scientificName
  gbif_dat_scientificName <- unique(dat$scientificName)
  message(cat("# unique 'scientificName' in GBIF data:", 
              length(gbif_dat_scientificName)))
  
  gbif_dat_species <- unique(dat$species) 
  message(cat("# unique 'species' in GBIF data:",
              length(gbif_dat_species)))
  
  
  
  ## Compare: Species in GBIF data versus GBIF taxonomy ####
  ## gbif_dat_species versus gbif_tax_canonicalName
  message(cat("Is the list of 'species' from GBIF occurrence data a subset of GBIF taxonomy 'canonicalName': "),
          rje::is.subset(gbif_dat_species, gbif_tax_canonicalName))
  
  message(cat("How many 'species' from GBIF occurrence data are not in GBIF taxonomy 'canonicalName': "),
          sum(gbif_dat_species %!in% gbif_tax_canonicalName))
  
  message(cat("Which 'species' from GBIF occurrence data are not in GBIF taxonomy 'canonicalName': "),
          if (length(gbif_dat_species[which(gbif_dat_species %!in% gbif_tax_canonicalName)]) == 0){
            "None"} else{
              gbif_dat_species[which(gbif_dat_species %!in% gbif_tax_canonicalName)]
            })
  
  ## gbif_dat_scientificName versus gbif_tax_scientificName
  message(cat("Is the list of 'scientificName' from GBIF occurrence data a subset of GBIF taxonomy 'scientificName': "),
          rje::is.subset(gbif_dat_scientificName, gbif_tax_scientificName))
  
  message(cat("How many 'scientificName' from GBIF occurrence data are not in GBIF taxonomy 'scientificName': "),
          sum(gbif_dat_scientificName %!in% gbif_tax_scientificName))
  
  message(cat("Which 'species' from GBIF occurrence data are not in GBIF taxonomy 'scientificName': "),
          if (length(gbif_dat_scientificName[which(gbif_dat_scientificName %!in% gbif_tax_scientificName)]) == 0){
            "None"} else{
              gbif_dat_scientificName[which(gbif_dat_scientificName %!in% gbif_tax_scientificName)]
            })
  
  # dat$scientificName[which(dat$scientificName %!in% gbif_tax_scientificName)]...
  # rje::is.subset(sp_dat$acceptedScientificName, gbif_tax_scientificName)
  
  
  ## AFD ####
  if (test_taxon[i] %in% c("Rhophodon", "Gyrocochlea", "Austrochloritis")) {
    test_taxon_afd <- afd_taxonomy[GENUS == test_taxon[i]]
  } else{
    test_taxon_afd <- afd_taxonomy[FAMILY == toupper(test_taxon[i])]
  }
  message(cat("dim(test_taxon_afd) = ", 
              dim(test_taxon_afd)))
  
  afd_tax_completename <- unique(test_taxon_afd$COMPLETE_NAME)
  message(cat("# unique 'COMPLETE_NAME' in AFD checklist:", 
              length(afd_tax_completename)))
  
  afd_tax_validname <- unique(test_taxon_afd$VALID_NAME)
  message(cat("# unique 'VALID_NAME' in AFD checklist:",
              length(afd_tax_validname)))
  
  
  ## Compare: Species in GBIF data versus AFD ####
  ## gbif_dat_species versus afd_tax_validname
  message(cat("Are all 'species' from GBIF occurrence data in AFD taxonomy 'VALID_NAME': "),
          rje::is.subset(gbif_dat_species, afd_tax_validname))
  
  message(cat("How many 'species' from GBIF occurrence data are not in AFD taxonomy 'VALID_NAME': "),
          sum(gbif_dat_species %!in% afd_tax_validname))
  
  message(cat("Which 'species' from GBIF occurrence data are not in AFD taxonomy 'VALID_NAME': "),
          if (length(gbif_dat_species[which(gbif_dat_species %!in% afd_tax_validname)]) == 0){
            "None"} else{
              gbif_dat_species[which(gbif_dat_species %!in% afd_tax_validname)]
            })
  
  ## gbif_dat_scientificName versus afd_tax_completename
  message(cat("Are all 'scientificName' from GBIF occurrence data in AFD taxonomy 'COMPLETE_NAME': "),
          rje::is.subset(gbif_dat_scientificName, afd_tax_completename))
  
  message(cat("How many 'scientificName' from GBIF occurrence data are not in AFD taxonomy 'COMPLETE_NAME': "),
          sum(gbif_dat_scientificName %!in% afd_tax_completename))
  
  message(cat("Which 'scientificName' from GBIF occurrence data are not in AFD taxonomy 'COMPLETE_NAME': "),
          if (length(gbif_dat_scientificName[which(gbif_dat_scientificName %!in% afd_tax_completename)]) == 0){
            "None"} else{
              gbif_dat_scientificName[which(gbif_dat_scientificName %!in% afd_tax_completename)]
            })
  
  ## Store outputs ####
  out[[i]] <- c(test_taxon = test_taxon[i], 
                   gbif_tax_scientificName = length(gbif_tax_scientificName), 
                   gbif_tax_canonicalName = length(gbif_tax_canonicalName),
                   gbif_dat_scientificName = length(gbif_dat_scientificName),
                   gbif_dat_species = length(gbif_dat_species),
                   afd_tax_completename = length(afd_tax_completename),
                   afd_tax_validname = length(afd_tax_validname),
                   GBIF_AFD_species = sum(gbif_dat_species %!in% afd_tax_validname),
                   GBIF_AFD_scientificname = sum(gbif_dat_scientificName %!in% afd_tax_completename),
                   GBIFnotAFD_species = nullval(gbif_dat_species[which(gbif_dat_species %!in% afd_tax_validname)]),
                   GBIFnotAFD_scientificname = nullval(gbif_dat_scientificName[which(gbif_dat_scientificName %!in% afd_tax_completename)]),
                   AFD_GBIF_species = sum(afd_tax_validname %!in% gbif_dat_species),
                   AFD_GBIF_scientificname = sum(afd_tax_completename %!in% gbif_dat_scientificName),
                   AFDnotGBIF_species = nullval(afd_tax_validname[which(afd_tax_validname %!in% gbif_dat_species)]),
                   AFDnotGBIF_scientificname = nullval(afd_tax_completename[which(afd_tax_completename %!in% gbif_dat_scientificName)]))
}


names(out) <- test_taxon
saveRDS(out, "./output/db_mismatch.rds")

## EXTRAS...
## GBIF species list
## Download by bounding box in GBIF
## Salticidae doi: https://www.gbif.org/occurrence/download/0041394-200613084148143
## Download species list
sp_dat <- fread(file.path(data_path, "db_compare","0041394-200613084148143.csv"))
dim(sp_dat)
names(sp_dat)



unique(sp_dat$scientificName)[1:5] ## https://dwc.tdwg.org/list/#dwc_scientificName; https://www.gbif.org/developer/occurrence#p_scientificName
unique(sp_dat$acceptedScientificName)[1:5] ## https://dwc.tdwg.org/list/#dwc_acceptedScientificName
unique(sp_dat$species)[1:5] ## https://www.gbif.org/developer/species#p_name

length(unique(sp_dat$scientificName)) 
length(unique(sp_dat$acceptedScientificName))
length(unique(sp_dat$species))

# length(setdiff(unique(sp_dat$scientificName), unique(sp_dat$acceptedScientificName)))
# length(setdiff(unique(sp_dat$acceptedScientificName), unique(sp_dat$scientificName)))

## See: https://data-blog.gbif.org/post/gbif-backbone-taxonomy/
## same canonical names with different authors are allowed twice, but only one is ever accepted... the algorithm identifies the homotypic group of names sharing the same basionym. Only one name is accepted per homotypic group and all the other names are marked as synonyms as they are homotypic. The most trusted first name added stays as the accepted name.





# '%!in%' <- function(x,y)!('%in%'(x,y))
# if (length(unique(gbif_tax_scientificName)) != length(unique(gbif_tax_canonicalName))){
#   gbif_tax_scientificName... only get first words before capital (use Augusts code here)
#   length(which(unique(gbif_tax_canonicalName) %!in% unique(gbif_tax_scientificName)))
# }



# ## Only extract by boundinng box: NOT GOOD
# sp <- as.matrix(dat[,.(decimalLongitude,decimalLatitude)])
# x <- matrix(NA,4,2)
# x[1,] <- c(bbox(aus.mask)[1,1], bbox(aus.mask)[2,1])
# x[2,] <- c(bbox(aus.mask)[1,1], bbox(aus.mask)[2,2])
# x[3,] <- c(bbox(aus.mask)[1,2], bbox(aus.mask)[2,1])
# x[4,] <- c(bbox(aus.mask)[1,2], bbox(aus.mask)[2,2])
# 
# grd.pts <- mgcv::in.out(x, sp)
# dat <- dat[!is.na(grd.pts),]
# dim(dat)
# points(dat[,.(decimalLongitude,decimalLatitude)], col = "blue")
