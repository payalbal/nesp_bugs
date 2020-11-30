## ConR::IUCN.eval function for parallel processing

conr_iucn_eval <- function(species_filename, maskfile, basemap_path, working_dir, iucn_outpath) {
  
  ## Set wd for IUCN-eval function
  setwd(working_dir)
  
  ## Read species data
  dat <- as.data.table(readRDS(species_filename))
  spname <- unique(dat$spfile)
  dat <- dat[ , .(latitude, longitude, scientificName, family, year)]
  names(dat) <- c("latitude", "longitude", "tax", "family", "coly")
  
  ## Run ConR function
  out <- IUCN.eval(dat, 
                   method.range = "alpha.hull",
                   alpha = 2, 
                   Cell_size_AOO = 2,
                   country_map = basemap,
                   SubPop = FALSE,
                   DrawMap = TRUE,
                   write_file_option = "csv", 
                   file_name = spname,
                   export_shp = TRUE, 
                   write_shp = TRUE, 
                   write_results = TRUE)
  
  ## Save output
  saveRDS(out, file = paste0(iucn_outpath, "/", spname, ".rds"))
  
  # if (write_shp2 == TRUE) {
  #   if(!is.null(out[[1]][[2]])) {
  #     ## Write shapefiles
  #     ## Note: Functionality in IUCN.eval(..., write_shp = TRUE) gives errors
  #     writeOGR(out[[1]][[2]], dsn = file.path(iucn_outpath, "shapesIUCN"), 
  #              layer= spname, driver="ESRI Shapefile")
  #   }
  # }
  
  
  ## Clear objects
  rm(dat, spname, out)
}
