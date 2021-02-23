## 
#' Mask species data & record number of occurrence points lost
#' 
#' @author Payal Bal
#'
#' @param species_filename name of species as per 'scientificName' in ALA data
#' @param mask_file mask file for Australia
#' @param data_dir directory where rds files will be stored
#' @return (1) species data as _masked.rds; (2) species log with number of records before and after masking as .csv
#'
#' @examples
#' spfile = ...
#' mask_file = ...
#' data_dir = "./
#' mask_spdat(species_filename, mask_file = mask_file, data_dir = spmasked_dir)


mask_spdat <- function(species_filename, mask_file, data_dir) {
  
  ## Read data
  dat_all <- dat <- as.data.table(readRDS(species_filename))
  spname <- unique(dat$spfile)
  
  ## Load mask
  domain.mask <- raster(mask_file)
  
  ## Create species log file
  specieslog <- paste0(data_dir, "/", spname, "_log", gsub("-", "", Sys.Date()), ".csv")
  cat(c("species", "n_clean2", "n_masked", "\n"),
      file = specieslog, append = T)
  
  ## Clip data by mask extent 
  dat <- dat[which(longitude >= domain.mask@extent@xmin)]
  dat <- dat[which(longitude <= domain.mask@extent@xmax)]
  dat <- dat[which(latitude >= domain.mask@extent@ymin)]
  dat <- dat[which(latitude <= domain.mask@extent@ymax)]
  
  if (nrow(dat) == 0) { 
    
    ## Write to species log file: #records in updated data = 0
    cat(c(spname, nrow(dat_all), 0, "\n"),
        file = specieslog, append = T)
    
  } else {
    
    ## Clip data/occurrence points if they fall outside mask polygon(s)
    sp <- SpatialPoints(dat[,c("longitude", "latitude")], 
                        proj4string = crs(domain.mask))
    grd.pts <-extract(domain.mask, sp)
    
    ## If at least one species point falls within mask, then..
    ## Also check for if (nrow(dat) > 0)
    if (any(!is.na(grd.pts))){
      
      ## Subset data
      dat <- dat[!is.na(grd.pts),]
      
      ## Save species data file
      saveRDS(as.data.table(dat),
              file = file.path(data_dir, paste0(spname, "_masked.rds")))
      
      ## Write to species log file: #records in updated data = nrow(dat)
      cat(c(spname, nrow(dat_all), nrow(dat), "\n"),
          file = specieslog, append = T)
      
    } else {
      
      ## Write to species log file: #records in updated data = 0
      cat(c(spname, nrow(dat_all), 0, "\n"),
          file = specieslog, append = T)
    }
  }
}


# ## Debugging code:
# ausmask <- raster(mask_file)
# clearPlot()
# quickPlot::Plot(ausmask, 
#                 title = "",
#                 axes = FALSE, 
#                 legend = FALSE,
#                 col = "khaki", 
#                 addTo = "ausmask", 
#                 new = TRUE)
# ## Unmasked data
# sp_xy <- SpatialPoints(dat[,c("longitude", "latitude")], proj4string = crs(ausmask))
# Plot(sp_xy, pch = 17, 
#      col = "darkcyan", 
#      title = "", 
#      addTo = "ausmask")
# 
# ## Masked data
# vals <- extract(ausmask, sp_xy)
# in_mask <- !is.na(vals)
# dat_masked <- dat[in_mask, ]
# sp_xy <- SpatialPoints(dat_masked[,c("longitude", "latitude")], proj4string = crs(ausmask))
# Plot(sp_xy, 
#      pch = 17, 
#      col = "darkred", 
#      title = "", 
#      addTo = "ausmask")
# 
# ## To read logfiles (for later)
# temp <- read.csv(specieslog, header = TRUE, sep = "")

