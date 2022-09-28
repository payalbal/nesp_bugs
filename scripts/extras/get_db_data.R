## source: https://raw.githubusercontent.com/Doi90/bushfireSOS/master/R/load_pres_bg_data.R

#' Load Presence-Background Data
#'
#' @param species
#' @param guild
#' @param fields
#' @param BoR_filters basis of record filters - https://dwc.tdwg.org/list/#dwc_basisOfRecord + categories listed in https://biocache.ala.org.au/#tab_advanceSearch
#' @param region
#' @param save.map
#' @param mapfile_directory
#' @param email
#' @return
#' @export
#'
#' @examples

# test <- occ(query = "Bertmainius colonus", from = c("gbif","ala","inat","ecoengine","vertnet","idigbio"))
# testALA <- occurrences(taxon = "text:\"Bertmainius colonus\"", download_reason_id = 5, method="offline", email = "tianxiaoh@student.unimelb.edu.au")


load_db_data <- function(species,
                         # guild = "ignore",
                         #clean = TRUE, #not sure if this needs to be an option because users SHOULD look at maps at least - maybe we should do "autoclean = TRUE"?
                         region = "all",
                         fields = ...,
                         BoRfilters = c("PreservedSpecimen", "LivingSpecimen", 
                                        "MachineObservation", "EnvironmentalDNA",
                                        "GenomicDNA"),
                         get.gbif = TRUE,
                         save.map = TRUE,
                         map.directory = ".",
                         email #needed for ALA4R 'offline' download
){
  
  
  ## Remove subspecies part of the name for download
  ## e.g. "Rhamphobrachium (Spinigerium) pyriforme" as "Rhamphobrachium pyriforme"
  ## Remove name within round brackets
  ## - assuming the brackets denote subpopulation, and not taxonomic notations
  species <- stringr::str_remove(afd_species[991],
                                 "\\(.*\\)")
  
  ## Remove trailing whitespace
  species <- stringr::str_squish(species)
  
  
  ## ALA data ####
  ## Count records in ALA
  n.obs <- ALA4R::occurrences(taxon = sprintf('text:"%s"', species),
                              record_count_only = TRUE,
                              download_reason_id = 5, 
                              email = email,
                              fq = "basis_of_record:HumanObservation")
  
  
  n.spec <- ALA4R::occurrences(taxon = sprintf('text:"%s"', species),
                               record_count_only = TRUE,
                               download_reason_id = 5, 
                               email = email,
                               fq = paste0("basis_of_record:", 
                                           BoRfilters, collapse = " OR "))
  
  n.ala <- list(observation = n.obs,  specimen =  n.spec)
  
  
  ## Download records from ALA
  occ_ala <- ALA4R::occurrences(taxon = sprintf('text:"%s"', species),
                                download_reason_id = 5, 
                                email = email,
                                fq = paste0("basis_of_record:", 
                                            BoRfilters, collapse = " OR "),
                                fields = fields)
  
  
  ## Convert eventDate to a date object
  if(nrow(occ_ala$data) > 0){
    occ_ala$data$eventDate <- lubridate::as_date(occ_ala$data$eventDate)
  }
  
  
  ## GBIF data ####
  if (get.gbif == TRUE){
    
    
    ## Check species name exists in GBIF backbone taxonomy
    if (species %in% gbif.taxonomy){
      
      
      ## Count records in GBIF
      key <- rgbif::name_suggest(q=species, rank='species')$data$key
      
      n.all <- rgbif::occ_search(taxonKey = key)$meta$count
      
      n.obs1 <- rgbif::occ_search(taxonKey = key, 
                                  basisOfRecord = "HUMAN_OBSERVATION")$meta$count
      n.obs2 <- rgbif::occ_search(taxonKey = key, 
                                  basisOfRecord = "OBSERVATION")$meta$count
      n.spec1 <- rgbif::occ_search(taxonKey = key, 
                                   basisOfRecord = "LIVING_SPECIMEN")$meta$count
      n.spec2 <- rgbif::occ_search(taxonKey = key, 
                                   basisOfRecord = "PRESERVED_SPECIMEN")$meta$count
      
      n.gbif <- list(all = n.all, observation = n.obs1 + nobs2,  specimen =  n.spec1 + nspec2)
      
      
      ## Download records from GBIF
      occ_gbif1 <- rgbif::occ_data(taxonKey = key, 
                                   basisOfRecord = "LIVING_SPECIMEN")$data
      occ_gbif2 <- rgbif::occ_search(taxonKey = key, 
                                     basisOfRecord = "PRESERVED_SPECIMEN")$data
      
      occ_gbif <- rbind(occ_gbif1, occ_gbif2)
      
      
      ## Convert eventDate to a date object
      if(nrow(occ_gbif) > 0){
        occ_gbif$eventDate <- lubridate::as_date(occ_gbif$eventDate)
      }
      
      # ## If using spocc package
      # occ_spocc <- spocc::occ(query = species,
      #                         from = "gbif",
      #                         limit = 100000)
      # if(nrow(occ_spocc$gbif$data[[1]]) > 0){
      #   
      #   occ_spocc$gbif$data[[1]]$eventDate <- lubridate::as_date(occ_spocc$gbif$data[[1]]$eventDate)
      #   
      # }
      
      
    } else {
      message("Species name not found in GBIF taxonomy. Skipping GBIF download...")
    }
    
    ## If no data is returned from either databases, terminate function
    if(nrow(occ_ala$data) == 0 & nrow(occ_gbif) == 0){
      stop("Not run: no records found")
    }
  }
  
  ## If no data is returned from ALA, terminate function
  if(nrow(occ_ala$data) == 0){
    stop("Not run: no records found in ALA")
  }
  
  
  # ## Using spocc:  
  # if(nrow(occ_ala$data) == 0 & nrow(occ_spocc$gbif$data[[1]]) == 0){
  #   stop("Not run: no records found")
  # }
  
  # #define guild-based database parameter (for now ignore)
  # invert_databases <- c("gbif","inat","ecoengine","idigbio")
  # bird_databases <- c(invert_databases,"ebird","vertnet")
  # other_vert_databases <- c(invert_databases,"vertnet")
  # #search based on guild
  # if(guild %in% c("Spiny crayfish","Invertebrates")){
  #     occ_spocc <- spocc::occ(query = species, from = invert_databases, limit = 100000)
  # }else{
  # if(guild == "Birds"){
  #     occ_spocc <- spocc::occ(query = species, from = bird_databases, limit = 100000, ebirdopts = list(key = "ggpmhljvtpuf"))
  # }else{
  # occ_spocc <- spocc::occ(query = species, from = other_vert_databases, limit = 100000)
  # }
  # }
  
  
  ## Remove generalised data (as much as possible)
  
  #we first do this through generalisation column (not always there)
  if("dataAreGeneralised" %in% colnames(occ_ala$data)){
    occ_ala$data <- occ_ala$data[occ_ala$data$dataAreGeneralised == FALSE,]
  }
  if("dataGeneralizations" %in% colnames(occ_spocc$gbif$data[[1]])){
    occ_spocc$gbif$data[[1]] <- occ_spocc$gbif$data[[1]][!grepl("generalised",occ_spocc$gbif$data[[1]]$dataGeneralizations,),]
  }#this may be a bit dodgy - the GBIF column may not necessarily contain full info about data generalisation (ie also used for taxonomic comments), but this may be the best we can do\
  if("informationWithheld" %in% colnames(occ_spocc$gbif$data[[1]])){
    occ_spocc$gbif$data[[1]] <- occ_spocc$gbif$data[[1]][is.na(occ_spocc$gbif$data[[1]]$informationWithheld),]
  }
  
  #then we remove data from known collections that consistently generalise their coordinates (also because we have the original data)
  occ_ala$data <- occ_ala$data[occ_ala$data$dataResourceName %nin% c("OEH Atlas of NSW Wildlife",
                                                                     "WildNet - Queensland Wildlife Data",
                                                                     "Victorian Biodiversity Atlas"),]
  occ_spocc$gbif$data[[1]] <- occ_spocc$gbif$data[[1]][occ_spocc$gbif$data[[1]]$collectionCode != "BioNet Atlas of NSW Wildlife",]#VBA and WildNet are not identifiable in GBIF, this is the best we can do, after a quick check this seems ok (ie remaining records at least look ok)
  
  
  ## Merging ALA and GBIF
  
  ### Add missing columns full of NAs
  
  for(ALA_col in c("eventDate",
                   "basisOfRecord",
                   "locality",
                   "institution",
                   "collection",
                   "coordinateUncertaintyInMetres")){
    
    if(ALA_col %nin% colnames(occ_ala$data)){
      
      occ_ala$data[ , ALA_col] <- rep(NA, nrow(occ_ala$data))
      
    }
  }
  
  for(GBIF_col in c("eventDate",
                    "basisOfRecord",
                    "locality",
                    "institutionCode",
                    "collectionCode",
                    "coordinateUncertaintyInMeters")){
    
    if(GBIF_col %nin% colnames(occ_spocc$gbif$data[[1]])){
      
      occ_spocc$gbif$data[[1]][ , GBIF_col] <- rep(NA, nrow(occ_spocc$gbif$data[[1]]))
      
    }
  }
  
  ### Merge data.frames
  
  if(nrow(occ_ala$data) > 0 & nrow(occ_spocc$gbif$data[[1]]) > 0){
    
    merged_df <- data.frame("ID" = seq_len(nrow(occ_ala$data) + nrow(occ_spocc$gbif$data[[1]])),
                            "Origin" = c(rep("ALA", nrow(occ_ala$data)),
                                         rep("GBIF", nrow(occ_spocc$gbif$data[[1]]))),
                            "Species" = c(rep(species, nrow(occ_ala$data)),
                                          rep(species, nrow(occ_spocc$gbif$data[[1]]))),#we are assuming the search returned all correct species - this needs looking at later on
                            "Longitude" = c(occ_ala$data$longitude,#note ALA data may have GDA94 original long lat, but they are processed to be wgs84
                                            occ_spocc$gbif$data[[1]]$longitude),
                            "Latitude" = c(occ_ala$data$latitude,
                                           occ_spocc$gbif$data[[1]]$latitude),
                            #add date for duplicate processing
                            "Date" = c(occ_ala$data$eventDate,
                                       occ_spocc$gbif$data[[1]]$eventDate),
                            "Basis.of.Record" = c(occ_ala$data$basisOfRecord,
                                                  occ_spocc$gbif$data[[1]]$basisOfRecord),
                            #this as commentary - not sure how reliable this is
                            #so this column is not always present for all species - need to think about how to best use this information
                            #for now not considering it in the main dataframe, but can dig out in saved raw dataframes
                            # "Data.Underwent.Generalisation" = c(as.character(occ_ala$data$dataAreGeneralised),
                            #                       occ_spocc$gbif$data[[1]]$informationWithheld),
                            "Locality" = c(occ_ala$data$locality,
                                           occ_spocc$gbif$data[[1]]$locality),
                            #same information with collection column, and gbif sometimes do not have this column thus giving problems
                            # "Dataset" = c(occ_ala$data$dataResourceName,
                            #                       occ_spocc$gbif$data[[1]]$datasetName),
                            "Institute" = c(occ_ala$data$institution,
                                            occ_spocc$gbif$data[[1]]$institutionCode),
                            "Collection" = c(occ_ala$data$collection,
                                             occ_spocc$gbif$data[[1]]$collectionCode),
                            "Coordinate.Uncertainty.in.Metres" = c(occ_ala$data$coordinateUncertaintyInMetres,
                                                                   occ_spocc$gbif$data[[1]]$coordinateUncertaintyInMeters),
                            stringsAsFactors = FALSE)
    
  } else { #if one of the search is empty, then don't merge, but use the merged dataset structure for subsequent cleaning
    
    if(nrow(occ_ala$data) > 0 & nrow(occ_spocc$gbif$data[[1]]) == 0){
      
      merged_df <- data.frame("ID" = seq_len(nrow(occ_ala$data)),
                              "Origin" = rep("ALA", nrow(occ_ala$data)),
                              "Species" = rep(species, nrow(occ_ala$data)),#we are assuming the search returned all correct species - this needs looking at later on
                              "Longitude" = occ_ala$data$longitude,
                              "Latitude" = occ_ala$data$latitude,
                              #add date for duplicate processing
                              "Date" = occ_ala$data$eventDate,
                              "Basis.of.Record" = occ_ala$data$basisOfRecord,
                              # #this as commentary - not sure how reliable this is
                              # "Data.Underwent.Generalisation" = as.character(occ_ala$data$dataAreGeneralised),
                              "Locality" = occ_ala$data$locality,
                              # "Dataset" = occ_ala$data$dataResourceName,
                              "Institute" = occ_ala$data$institution,
                              "Collection" = occ_ala$data$collection,
                              "Coordinate.Uncertainty.in.Metres" = occ_ala$data$coordinateUncertaintyInMetres,
                              stringsAsFactors = FALSE)
      
    } else {
      
      merged_df <- data.frame("ID" = seq_len(nrow(occ_spocc$gbif$data[[1]])),
                              "Origin" = rep("GBIF", nrow(occ_spocc$gbif$data[[1]])),
                              "Species" = rep(species, nrow(occ_spocc$gbif$data[[1]])),#we are assuming the search returned all correct species - this needs looking at later on
                              "Longitude" = occ_spocc$gbif$data[[1]]$longitude,
                              "Latitude" = occ_spocc$gbif$data[[1]]$latitude,
                              #add date for duplicate processing
                              "Date" = occ_spocc$gbif$data[[1]]$eventDate,
                              "Basis.of.Record" = occ_spocc$gbif$data[[1]]$basisOfRecord,
                              # #this as commentary - not sure how reliable this is
                              # "Data.Underwent.Generalisation" = occ_spocc$gbif$data[[1]]$informationWithheld,
                              "Locality" = occ_spocc$gbif$data[[1]]$locality,
                              # "Dataset" = occ_spocc$gbif$data[[1]]$datasetName,
                              "Institute" = occ_spocc$gbif$data[[1]]$institutionCode,
                              "Collection" = occ_spocc$gbif$data[[1]]$collectionCode,
                              "Coordinate.Uncertainty.in.Metres" = occ_spocc$gbif$data[[1]]$coordinateUncertaintyInMeters,
                              stringsAsFactors = FALSE)
      
    }
  }
  
  #####################
  ### Data Cleaning ###
  #####################
  
  ## Remove spatial duplicates (other duplicate types may matter, think later)
  
  merged_df$Longitude <- as.numeric(merged_df$Longitude)
  
  merged_df$Latitude <- as.numeric(merged_df$Latitude)
  
  merged_df <- merged_df[!duplicated(merged_df[ , c("Longitude", "Latitude")]), ]
  
  ## Get rid of missing or incomplete long and lats
  
  merged_df <- merged_df[!is.na(merged_df$Longitude) | !is.na(merged_df$Latitude), ]
  
  ## Get rid of unusable long lat vals
  ###  (Roozbeh says can save some data here will look into it later)
  
  merged_df <- merged_df[merged_df$Longitude > -180 &
                           merged_df$Longitude < 180 &
                           merged_df$Latitude > -90 &
                           merged_df$Latitude < 90, ]
  
  ## Check if any record left
  
  if(nrow(merged_df) == 0){
    stop("Not run: no data with legitimate coordinates found")
  }
  
  # ## Clean records using coord cleaner
  #
  # merged_df <- CoordinateCleaner::clean_coordinates(merged_df,
  #                                                   lon = "Longitude",
  #                                                   lat = "Latitude",
  #                                                   species = "Species",
  #                                                   tests = c("capitals",
  #                                                             "centroids",
  #                                                             "equal",
  #                                                             "gbif",
  #                                                             "institutions",
  #                                                             "seas",
  #                                                             "zeros"),
  #                                                   #skip urban test - keeps giving proj4string errors, will look into later
  #                                                   # urban_ref = as_Spatial(read_sf("Data/GIS/ne_50m_urban_areas/ne_50m_urban_areas.shp")),
  #                                                   seas_ref =  NULL, #as_Spatial(read_sf("Data/GIS/ne_50m_land/ne_50m_land.shp")),
  #
  #                                                   #ignore outliers for now
  #                                                   # outliers_method = "distance",
  #                                                   # outliers_td = 1500, #outlier bit probably needs tweaking, its curently set to be very conservative
  #                                                   value = "clean")
  
  ## Check if duplicate long or lat - could be signal of rounding
  
  suspect.rounding <- ifelse(any(anyDuplicated(merged_df$Longitude),
                                 anyDuplicated(merged_df$Latitude)),
                             "duplicate long/lat found - suspect rounding",
                             NA)
  
  # ####################
  # ### Plot Records ###
  # ####################
  #
  # ## Visualise those with fewer than 1k records
  # ### (can tweak this - I just think there isn't much point to manual input
  # ###  when looking at more than 1k data)
  #
  # if(nrow(merged_df) <= 1000){
  #
  #   sp.sf <- sf::st_as_sf(merged_df,
  #                     coords = (4:5),
  #                     crs = sp::CRS("+proj=longlat +datum=WGS84"))#all ALA and GBIF coord should be in wgs84 - but this needs attention when adding more dataset in the future (and also some of ALA may be gda94 but incorrectly labelled according to Lee Belbin (I think?) - but this may be beyond our ability to fix)
  #
  #   sp.map <- mapview::mapview(sp.sf,
  #                     layer.name = species,
  #                     homebutton = FALSE)
  #
  #   if(save.map == TRUE){
  #
  #     map_filename <- sprintf("%s/%s.html",
  #                               map.directory,
  #                               gsub(" ",
  #                                    "_",
  #                                    species))
  #
  #     htmlwidgets::saveWidget(sp.map@map,
  #                             file = map_filename)
  #
  #     cat(paste0("Map is saved to ", map_filename), "\n")
  #
  #   }
  #
  # } else {
  #
  #   sp.map <- "more than 1k records, not mapped"
  #
  # }
  
  return(list(raw.ala.data = occ_ala$data,
              raw.gbif.data = occ_spocc$gbif$data[[1]],
              processed.data = merged_df,
              rounding.comment = suspect.rounding))
  
}

# #test run
# test_run <- load_pres_bg_data("Atrichornis rufescens", email = "tianxiaoh@student.unimelb.edu.au", guild = "Birds")

# load_pres_bg_data("Atrichornis rufescens",
#                   email = "davidpw@student.unimelb.edu.au")


library(spocc)
library(rgbif)

key <- name_suggest(q='Gnathifera', rank='phylum')$key[1]
occ_search(taxonKey=key, limit=20)
occ_data(scientificName=..., limit=20)

spocc::occ(query = "Chordata",
           from = "gbif",
           geometry = "POLYGON((112.76 -10.23, 155.48 -10.23, 155.48 -44.28, 112.76 -44.28, 112.76 -10.23))",
           limit = 10)

