## 
#' Mask species data & record number of occurrence points lost
#' 
#' @author Payal Bal
#'
#' @param species_filename name of species as per 'scientificName' in ALA data
#' @param maskfile mask file for Australia
#' @param data_dir directory where rds files will be stored
#' @param specieslog text file for recording number of duplicated records lost while processing
#' @return (1) species data as _masked.rds and (2) species log with number of records before and after masking as .txt
#'
#' @examples
#' spfile <- "/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs/ala_data/spdata/plakobranchus_ocellatus.rds"
# test <- save_spdata(species_filename = spfile, 
#                     maskfile = , 
#                     data_dir = "./", 
#                     map_dir = "./"
#                     specieslog = "./Inquisitor_flindersianus_log.txt")


mask_spdat <- function(species_filename, mask_file, data_dir) {
  
  ## Read data
  dat <- as.data.table(readRDS(species_filename))
  spname <- unique(dat$spfile)
  
  ## Original number of records, before masking
  n_clean2 <- nrow(dat)
  
  ## Mask
  
  
  ## Number of records after masking
  n_masked <- nrow(dat)
  
  ## Save species file
  saveRDS(as.data.table(dat),
          file = file.path(data_dir, paste0(spname, "_masked.rds")))
  
  ## Write to log file for species
  specieslog <- paste0(data_dir, "/", spname, "_log", gsub("-", "", Sys.Date()), ".txt")
  cat(c("species", "n_clean2", "n_masked", "\n"),
      file = specieslog, append = T)
  cat(c(spfile, n_clean2, n_masked, "\n"),
      file = specieslog, append = T)
}
