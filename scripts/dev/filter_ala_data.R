#'@title Filter downloaded ALA data
#'
#'@description Filters data downloaded from the ALA, producing a refined dataset. 
#'
#'@param ALA.download.data (dataframe) A dataframe holding the downloaded ALA data.
#'@param output.folder (string) A folder to save the outputs to. If none specified, no file is written.
#'@param output.name (string), A name to use in saving the outputs. Default: 'filtered_data'.
#'@param domain.mask, (raster layer) A raster layer specifying the analysis domain
#'@param earliest.year (integer) The earliest year for which data will be retained  Default: 1970.
#'@param spatial.uncertainty.m (float) The distance (m) for spatial uncertainty below which data will be retained. Default: 2000.
#'@param verbose (boolean) Print messages to console. Default TRUE.
#'
#'@return Dataframe.
#'
#'@examples output = filter_ALA_data(My.ALA.data, output.folder = 'C:/Users/processed_data', output.name = My.filtered.ALA.data, domain.mask = Aust.ras)
#'
#'@export
filter_ALA_data = function(ALA.download.data,             
                           output.folder = NULL,       
                           output.name = "filtered_data",  
                           domain.mask = NULL,                   
                           earliest.year = 1970,
                           spatial.uncertainty.m = 2000,
                           select.fields = NULL,
                           verbose=TRUE)
  
  ## Change log
  ## ----------
  ## substituted year for eventDate in returned data.frame
  ## set domain.mask to an optional arg
  ## made cols to keep an argument
  ## now uses specificEpithet if available and defaults toscientificName 
  ## when not (like specificEpithet doesn't ship consistently anymore...)

  {
  ## Read in the data
  ALA.data <- ALA.download.data 
  
  # catch the original number of records
  n.recs.start <- nrow(ALA.data)
  
  ## Filter the data by QA assertions
  ALA.data<-ALA.data[!ALA.data$Name.not.supplied=='true',]
  ALA.data<-ALA.data[!ALA.data$Name.not.recognised=='true',]
  ALA.data<-ALA.data[!ALA.data$Cultivated...escapee=='true',]
  ALA.data<-ALA.data[!ALA.data$Unparseable.verbatim.coordinates=='true',]
  ALA.data<-ALA.data[!ALA.data$Decimal.latitude.longitude.conversion.failed=='true',]
  ALA.data<-ALA.data[!ALA.data$Unable.to.convert.UTM.coordinates=='true',]
  ALA.data<-ALA.data[!ALA.data$Supplied.coordinates.are.zero=='true',]
  ALA.data<-ALA.data[!ALA.data$Zero.latitude=='true',]
  ALA.data<-ALA.data[!ALA.data$Zero.longitude=='true',]
  ALA.data<-ALA.data[!ALA.data$Coordinates.centre.of.country=='true',]
  ALA.data<-ALA.data[!ALA.data$Supplied.coordinates.centre.of.state=='true',]
  ALA.data<-ALA.data[!ALA.data$processing.Error=='true',]
  
  ## Filter by taxonomic resolution ##ADD AN ARGUMENT TO SPECIFY THIS 
  ALA.data <- ALA.data[(ALA.data$taxonRank == 'species' | ALA.data$taxonRank == 'subspecies'),]
  
  ## Filter by date range 
  ALA.data <- ALA.data[(ALA.data$year >= earliest.year),]
  
  ## Filter by spatial Uncertainty
  ALA.data <- ALA.data[(ALA.data$coordinateUncertaintyInMeters <= spatial.uncertainty.m),]
  
  ## Filter by presence of spatial coordinates (necessary data)
  ALA.data <- ALA.data[!is.na(ALA.data$decimalLatitude),]
  ALA.data <- ALA.data[!is.na(ALA.data$decimalLongitude),]
  
  ## Filter by spatial domain 
  ALA.data <- ALA.data[(ALA.data$decimalLongitude > domain.mask@extent@xmin),]
  ALA.data <- ALA.data[(ALA.data$decimalLongitude < domain.mask@extent@xmax),]
  ALA.data <- ALA.data[(ALA.data$decimalLatitude > domain.mask@extent@ymin),]
  ALA.data <- ALA.data[(ALA.data$decimalLatitude < domain.mask@extent@ymax),]
  
  ## Filter by location on spatial grid
  if(!is.null(domain.mask)){
    pts<-cbind(ALA.data$decimalLongitude,ALA.data$decimalLatitude)
    sp <- SpatialPoints(pts)  
    grd.pts<-extract(domain.mask, sp)
    ALA.data <- ALA.data[!is.na(grd.pts),]
  }
  
  ## Create cleaned data file for modelling + log file of how it was created
  # select only the data we will/might use later
  #ALA.data <- ALA.data[,c(1,8,10,11:15)] # "occurrenceID", "scientificName", "taxonRank", "eventDate", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters"
  if(is.null(select.fields)){
    
    has_epithet = length(grep('specificEpithet', names(ALA.data))) > 0
    if (has_epithet){
      select.fields = c("occurrenceID", "specificEpithet", "taxonRank", 
                            "eventDate", "decimalLatitude", "decimalLongitude", 
                            "coordinateUncertaintyInMeters")      
    } else {
      select.fields = c("occurrenceID", "scientificName", "taxonRank", 
                        "eventDate", "decimalLatitude", "decimalLongitude", 
                        "coordinateUncertaintyInMeters")
    }
    
    check_fields_exist = lapply(select.fields, grep, names(ALA.data))
    valid_fields = unlist(lapply(check_fields_exist, function(x)
      length(x) > 0))
    
    # this shouldn't ever be called... but it's there as a trigger 
    # that something has become inconsistent
    if (!all(valid_fields)) {
      select.fields = select.fields[valid_fields]
      warning(sprintf('Default select.field(s): %s were not found - returning subset only', 
                      select.fields[!valid_fields]))
    }
  
  } else {
    check_fields = all(select.fields %in% names(ALA.data))
    if(!check_fields){
      
      # which fields are available?
      avaialble = selected.fields %in% names(ALA.data) 
      select.fields = select.fields[available]
      
      # generate default fields 
      has_epithet = length(grep('specificEpithet', names(ALA.data))) > 0
      if (has_epithet){
        select.fields.def = c("occurrenceID", "specificEpithet", "taxonRank", 
                          "eventDate", "decimalLatitude", "decimalLongitude", 
                          "coordinateUncertaintyInMeters")      
      } else {
        select.fields.def = c("occurrenceID", "scientificName", "taxonRank", 
                          "eventDate", "decimalLatitude", "decimalLongitude", 
                          "coordinateUncertaintyInMeters")
      }
      
      # combine available with defualt
      select.fields = c(select.fields.def, select.fields)
      
      # warrants a warning
      warning('Specified select.fields were not found in the dataset - returning default fields instead')
    }
  }
  ALA.data = subset(ALA.data, select = select.fields)
  # changing 'specificEpithet' to 'scientificName' for convenience 
  # (later functions assume 'scientificName') 
  colnames(ALA.data)[2]='scientificName' 
  
  # write the data to file, if an output folder is specified
  if(!is.null(output.folder))
  {
    if(!dir.exists(output.folder))
    {
      dir.create(output.folder)
    }# end if !dir.exists
    out.path <- file.path(output.folder,paste0(output.name,"_",Sys.Date(),".csv")) 
    write.csv(ALA.data, out.path, row.names=FALSE)
    # write a log file describing how the data was created *************************************
    fileConn<-file(file.path(output.folder,paste0(output.name,"_",Sys.Date(),"_log_file.txt")),'w')
    writeLines("#######################################################################",con = fileConn)
    writeLines("###",con = fileConn)
    writeLines("### ALA data filtration log file ",con = fileConn)
    writeLines("###",con = fileConn)
    writeLines(paste0("### Created ",Sys.time()," using the filter_ALA_data() function."),con = fileConn)
    writeLines("###",con = fileConn)
    writeLines("#######################################################################",con = fileConn)
    writeLines("",con = fileConn)
    writeLines(paste0("Output data file = ", out.path),con = fileConn)
    writeLines(paste0("Domain mask applied = ", domain.mask@file@name),con = fileConn)
    writeLines(paste0("Data restricted to after ", earliest.year),con = fileConn)
    writeLines(paste0("Data restricted to spatial uncertainty < ",spatial.uncertainty.m, "m"),con = fileConn)
    writeLines(paste0("Number of records before filtering = ", n.recs.start),con = fileConn)
    writeLines(paste0("Number of records after filtering = ", nrow(ALA.data)),con = fileConn)
    writeLines("#######################################################################",con = fileConn)
    close(fileConn) #**************************************************************************
  } # end if !is.null
  
  # write some feedback to the terminal
  if(verbose)
  {
    msg1 = 'Returned object is a dataframe.'
    msg2 = paste('These data have been also been written to ', out.path)
    cat(paste(msg1, msg2, sep = '\n'))
  }# end if verbose
  
  return(ALA.data)
  
} # end filter_ALA_data function
