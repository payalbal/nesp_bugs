#' Save individual species data as rds files and mapped pdfs from cleaned ALA data
#' Masking not done at this stage 
#' Duplicates are removed from species data
#' 
#' @author Payal Bal
#'
#' @param species_name name of species as per 'scientificName' in ALA data
#' @param ala_data cleaned ALA data 
#' @param maskfile mask file for Australia
#' @param data_dir directory where rds files will be stored
#' @param map_dir directory where pdf maps will be storeds
#' @param specieslog text file for recording number of duplicated records lost while processing
#' @return dat as .rds, maps as .pdf and logs as .txt files for each species
#'
#' @examples
# test <- save_spdata(species = "Inquisitor flindersianus", 
#                     ala_data = ..., 
#                     maskfile = ..., 
#                     data_dir = "./", 
#                     map_dir = "./"
#                     specieslog = "./Inquisitor_flindersianus_log.txt")




save_spdata <- function(species_name, ala_data, maskfile, data_dir, map_dir){
  
  ## Get species data
  temp <- ala_dat[scientificName == species_name]
  n1 <- nrow(temp)
  
  ## Get species file name
  spfile <- unique(temp$spfile)
  
  if (length(spfile) > 1){
    stop("Error: More than 1 unique spfile for naming species file...")
  }
  
  ## Initialise log file for species
  specieslog <- paste0(data_dir, "/", spfile, "_log",gsub("-", "", Sys.Date()), ".txt")
  cat(paste("Processing species: ", species_name, "...\n"),
      file = specieslog, append = T)
  cat(paste("Species file name: ", spfile, ".rds\n"),
      file = specieslog, append = T)
  cat(paste0("\nThis is the ", which(ala_species == species_name),"^th species of ", length(ala_species),"\n"),
      file = specieslog, append = T)
  cat(paste("-------------------------------------------\n\n"),
      file = specieslog, append = T)
  cat(paste("Number of records in cleaned ALA data:", n1, "\n"),
      file = specieslog, append = T)
  
  # ## Remove duplicate records - by name-lat-long-date
  # ## NOTE: Too many records lost with this approach - DICARD STEP
  # temp1 <- temp[is.na(eventDate)] ## store records with eventDate = NA
  # temp2 <- temp[!is.na(eventDate)] ## find duplicates for records with eventDate != NA
  # temp2 <- temp2[!duplicated(temp2[ , c("scientificName", "longitude", "latitude", "eventDate")]), ]
  # temp <- as.data.table(rbind(temp1, temp2))
  # n2 <- nrow(temp)
  
  ## Remove duplicate records - c("catalogueNumber", "collectionID", "institutionID")
  temp <- temp[!duplicated(temp[ , c("catalogueNumber", "collectionID", "institutionID")]), ]
  n2 <- nrow(temp)
  cat(paste("Number of duplicate records removed:", n1-n2, "\n"),
      file = specieslog, append = T)
  cat(paste("Number of records retained:", n2, "\n"),
      file = specieslog, append = T)
  
  
  ## Save species file
  saveRDS(as.data.table(temp),
          file = file.path(data_dir, paste0(spfile, ".rds")))
  
  ## Save species map
  map_filename <- sprintf("%s/%s.pdf",
                          map_dir,
                          spfile)
  pdf(map_filename)
  reg.mask <- raster(maskfile)
  plot(reg.mask, col = "peru", axes = FALSE, box = FALSE, legend = FALSE)
  points(temp[,.(longitude, latitude)], pch = 4, col = "blue", cex = 0.5)
  
  # ## Example
  # plot(ausmask)
  # lat <- ala_data[scientificName == "Inquisitor flindersianus"]$latitude
  # long <- ala_data[scientificName == "Inquisitor flindersianus"]$longitude
  # points(long, lat)
  
  dev.off()
}


