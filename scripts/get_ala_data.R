#' Load ALA data 
#'
#' @param species
#' @param extra_fields darwin core fields: https://biocache.ala.org.au/ws/index/fields
#' @param BoR_filters basis of record filters - https://dwc.tdwg.org/list/#dwc_basisOfRecord + categories listed in https://biocache.ala.org.au/#tab_advanceSearch
#' @param remove_duplicates
#' @param dst
#' @param save.map
#' @param reg.mask.file
#' @param email
#' @return
#' @export
#'
#' @examples

# test <- spocc::occ(query = "Zenodorus metallescens", from = c("ala"))
# testALA <- ALA4R::occurrences(taxon = "text:\"Zenodorus metallescens\"", download_reason_id = 5, email = "...")


get_ala_data <- function(species,
                         extra_fields = TRUE,
                         specimens_only = TRUE,
                         remove_duplicates = TRUE,
                         dst = NULL,
                         save.map = TRUE,
                         reg.mask.file,
                         email #needed for ALA4R 'offline' download
                         # generate_doi = TRUE
){
  
  
  ## Define function parameters ####
  ## Specimen only filters
  BoRfilters <-  c("PreservedSpecimen", "LivingSpecimen", 
                   "MachineObservation", "EnvironmentalDNA",
                   "GenomicDNA")
  
  ## Extra DWC fields for download
  dwc_fields <- c("el1055", "el1056", #endemism
                  "el682", #migratory
                  "disposition", 
                  "assertions","assertions_unchecked", 
                  "raw_data_generalizations",
                  "duplicate_record", "duplicate_status",
                  "sensitive","taxonomic_issue")
  
  ## Data destination directory
  if(is.null(dst)){
    stop("Warning: Destination directory for ALA data not specified...\n")
  } else {
    if(!dir.exists(dst)){
      dir.create(dst)
    } 
  }
  
  ## Map directory
  map_dir = file.path(dst, "maps")
  if(is.null(map_dir)){
    stop("Warning: Destination directory for maps not specified...\n")
  } else {
    if(!dir.exists(map_dir)){
      dir.create(map_dir)
    }
  }
  
  ## Remove subspecies part of the name for download
  ## e.g. "Rhamphobrachium (Spinigerium) pyriforme" as "Rhamphobrachium pyriforme"
  ## Remove name within round brackets
  ## - assuming the brackets denote subpopulation, and not taxonomic notations
  species <- stringr::str_remove(species,
                                 "\\(.*\\)")
  
  ## Remove trailing whitespace
  species <- stringr::str_squish(species)
  
  
  ## Count records in ALA ####
  n.all <- ALA4R::occurrences(taxon = sprintf('text:"%s"', species), #includes synonyms
                              record_count_only = TRUE)
  
  n.obs <- ALA4R::occurrences(taxon = sprintf('text:"%s"', species),
                              record_count_only = TRUE,
                              fq = "basis_of_record:HumanObservation")
  
  
  n.spec <- ALA4R::occurrences(taxon = sprintf('text:"%s"', species),
                               record_count_only = TRUE,
                               fq = paste0("basis_of_record:", 
                                           BoRfilters, collapse = " OR "))
  
  n.ala <- list(all = n.all, observation = n.obs,  specimen =  n.spec)
  
  
  ## Download records from ALA ####
  if (specimens_only == TRUE) {
    
    if (extra_fields == TRUE) {
      occ_ala <- ALA4R::occurrences(taxon = sprintf('text:"%s"', species),
                                    download_reason_id = 5, 
                                    email = email,
                                    fq = paste0("basis_of_record:", 
                                                BoRfilters, collapse = " OR "),
                                    extra = dwc_fields)$data
    } else {
      occ_ala <- ALA4R::occurrences(taxon = sprintf('text:"%s"', species),
                                    download_reason_id = 5, 
                                    email = email,
                                    fq = paste0("basis_of_record:", 
                                                BoRfilters, collapse = " OR "))$data
    }
  } else {
    if (extra_fields == TRUE) {
      occ_ala <- ALA4R::occurrences(taxon = sprintf('text:"%s"', species),
                                    download_reason_id = 5, 
                                    email = email,
                                    extra = dwc_fields)$data
    } else {
      occ_ala <- ALA4R::occurrences(taxon = sprintf('text:"%s"', species),
                                    download_reason_id = 5, 
                                    email = email)$data
    }
  }
  
  ## If no data is returned from ALA, terminate function
  if(nrow(occ_ala) == 0){
    stop(paste("\nNot run: no records found for", species, "in ALA"))
  }
  
  ## Convert eventDate to a date object
  if(nrow(occ_ala) > 0){
    occ_ala$eventDate <- lubridate::as_date(occ_ala$eventDate)
  }
  
  ## Processindg downloaded data ####
  ala_df <- data.table::as.data.table(occ_ala)
  
  ## Convert lat long colums to numeric
  ala_df$longitude <- as.numeric(ala_df$longitude)
  ala_df$latitude <- as.numeric(ala_df$latitude)
  
  ## Get rid of missing or incomplete long and lats
  ala_df <- na.omit(ala_df, cols = c("latitude", "longitude"))
  
  ## Remove duplicates
  if(remove_duplicates == TRUE){
    ala_df <- ala_df[!duplicated(ala_df[ , c("longitude", "latitude")]), ]
  }
  
  ## Get rid of unusable long lat vals
  ala_df <- ala_df[ala_df$longitude > -180 &
                     ala_df$longitude < 180 &
                     ala_df$latitude > -90 &
                     ala_df$latitude < 90, ]
  
  # ## Remove generalised data (as much as possible)
  # if(any(grepl("dataGeneralizations", names(ala_df)))) {
  #   ala_df <- ala_df[ala_df$dataGeneralizationsOriginal == FALSE,]
  # }
  
  ## Check if any record left
  if(nrow(ala_df) == 0){
    stop(paste("Not run: No data with legitimate coordinates found for", species))
  }
  
  ## Check if duplicate long or lat - could be signal of rounding
  suspect.rounding <- ifelse(any(anyDuplicated(ala_df$longitude),
                                 anyDuplicated(ala_df$latitude)),
                             "duplicate long/lat found - suspect rounding",
                             NA)
  
  
  ### Save species plots ####
  if(save.map == TRUE){
    
    map_filename <- sprintf("%s/%s.pdf",
                            map_dir,
                            gsub(" ",
                                 "_",
                                 tolower(species)))
    pdf(map_filename) 
    
    reg.mask <- raster(reg.mask.file)
    plot(reg.mask, col = "grey", axes = FALSE, box = FALSE, legend = FALSE)
    points(ala_df[,.(longitude, latitude)], pch = 4, col = "blue", cex = 0.5)
    
    dev.off() 
    
    cat(paste0("Map is saved to ", map_filename), "\n")
    
  } 
  
  ## Save results ####
  saveRDS((list(raw.data = occ_ala,
                processed.data = ala_df,
                record.counts = n.ala,
                rounding.comment = suspect.rounding)),
          file = file.path(dst, 
                           paste0(gsub(" ", "_", tolower(species)), ".rds")))
  
  
  ## Clear workspace
  rm(n.ala, occ_ala, ala_df, suspect.rounding)
  
}

## Test



## EXTRA
## Default ALA fields: 
# fields = c("id","data_resource_uid","data_resource",
#            "institution_uid","institution_name",
#            "collection_uid","collection_name",
#            "license","catalogue_number",
#            "taxon_concept_lsid",
#            "raw_taxon_name","raw_common_name",
#            "taxon_name","common_name","rank",
#            "kingdom","phylum","class","order",
#            "family","genus","species","subspecies",
#            "institution_code","collection_code",
#            "raw_locality","raw_datum",
#            "raw_latitude","raw_longitude",
#            "latitude","longitude",
#            "coordinate_precision","coordinate_uncertainty",
#            "country","state","cl959","cl21","cl1048",
#            "min_elevation_d","max_elevation_d",
#            "min_depth_d","max_depth_d",
#            "individual_count","collector",
#            "occurrence_date","year","month",
#            "verbatim_event_date",
#            "basis_of_record","raw_basis_of_record",
#            "occurrence_status",
#            "raw_sex","preparations",
#            "outlier_layer",
#            "taxonomic_kosher","geospatial_kosher")
# 
# 
# ## Usinng htmlwidgets::saveWidget - NOT WORKING
# ## Visualise those with fewer than 1k records
# if(nrow(ala_df) <= 1000){
#   
#   sp.sf <- sf::st_as_sf(ala_df,
#                         coords = c("longitude", "latitude"),
#                         crs = sp::CRS("+proj=longlat +datum=WGS84")) 
#   
#   ## Comments from VErt team: all ALA and GBIF coord should be in wgs84
#   ## - but this needs attention when adding more dataset in the future 
#   ##y(and also some of ALA may be gda94 but incorrectly labelled according 
#   ## to Lee Belbin (I think?) - but this may be beyond our ability to fix)
#   
#   sp.map <- mapview::mapview(sp.sf,
#                              layer.name = species,
#                              homebutton = FALSE)
#   
#   map_filename <- sprintf("%s/%s.html",
#                           map_dir,
#                           gsub(" ",
#                                "_",
#                                tolower(species)))
#   
#   map_filename <- paste0(getwd(), 
#                          "/output/", 
#                          gsub(" ", "_",tolower(species)),
#                          ".html")
#   
#   htmlwidgets::saveWidget(sp.map@map,
#                           file = map_filename)
# 
#   cat(paste0("Map is saved to ", map_filename), "\n")
#   
# } else {
#   
#   sp.map <- "more than 1k records, not mapped"
#   
# }
# 
# 
# ## Possible fix for htmlwidgets::saveWidget - NOT WORKING EITHER
# saveWidgetFix <- function (widget,file,...) {
#   ## A wrapper to saveWidget which compensates for arguable BUG in
#   ## saveWidget which requires `file` to be in current working
#   ## directory.
#   wd<-getwd()
#   on.exit(setwd(wd))
#   outDir<-dirname(file)
#   file<-basename(file)
#   setwd(outDir);
#   htmlwidgets::saveWidget(widget,file=file,selfcontained = TRUE)
# }
# 
# 
# ## Clean records using coord cleaner
# ala_df <- CoordinateCleaner::clean_coordinates(ala_df,
#                                                   lon = "longitude",
#                                                   lat = "latitude",
#                                                   species = "species",
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
