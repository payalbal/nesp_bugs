
#'@title Download occurrences from the ALA
#'
#'@description Simplified and customised ALA occurrence download function. 
#'Largely a hack of the ALA4R function 'occurrences'
#'
#'@param taxon (string) Can be a free text search, or taxonomic rank can be used to focus search.
#'@param wkt (WKT formatted string) Bounding box to filter geographic locations of occurrences to (optional).
#'@param fields (CSV) Fields to return. Defaults to subset of fields.
#'@param qa (CSV) Quality assertion fields. Currently set to 'includeall.'
#'@param method (string) One of 'offline' or indexed. 'offline' only method available currently.
#'@param email (string) Email address to receive path to .zip archive.
#'@param download_reason (int) Valid ALA download reason. See website. Accepts numeric codes only (0:12). 
#'@param dwcHeaders (bool) Return column names as Darwin Core formatted (where available). See website. Default = TRUE.
#'@param dst (string) File path to a directory to save downloads in. If NULL, defaults to tempdir().
#'@param read (bool) Read file into memory once downloaded? Only relevant if method = indexed, and dst is supplied. Default = TRUE.
#'@param dry_run (bool) Return URL to be requested without sending the request. Default: FALSE
#'@param verbose (bool) Switch print statements and status checking from the ALA servers on. D
#'Default: TRUE. 
#'
#'@return if verbose, query status
#'
#'@note strips away much of the functionality of the ALA4R function
#'
#'@seealso ALA4R
#'
#'@export
#'
#'@examples 
#' download_occurrences(taxon="genus:Wandella", wkt="POLYGON((112.0+44.0,154.0+44.0,154.0+9.0,112.0+9.0,112.0+44.0))", 
#' method = "offline",email="karel.mokany@csiro.au", download_reason_id="7", verbose = TRUE)
#'
download_occurrences <- function (taxon, wkt, fields, qa, method = "offline", 
                                  email, download_reason_id, dwcHeaders = TRUE, dst = NULL, read = TRUE,
                                  dry_run = FALSE, verbose = TRUE) {

  pkg_check = suppressWarnings(lapply(c('httr', 'assertthat'), require, character.only = TRUE))
  if(!all(unlist(pkg_check))) stop('Could not load one of required packages httr or asserthat')
  
  ## possibly leave this in case smaller downloads are required
  method <- match.arg(tolower(method), c("offline", "indexed"))
  if (method == "indexed") {
    valid_fields_type <- "occurrence_stored"
    #stop('indexed currently not available: use ALA4R package instead')
  } else {
    valid_fields_type <- "occurrence"
  } 
  
  is.notempty.string = function(x) {
    is.string(x) && !is.na(x) && nchar(x)>0
  }
  
  ## query container
  this_query <- list()
  
  ## must be included
  if (!missing(taxon)) {
    if (is.factor(taxon)) {
      taxon <- as.character(taxon)
    }
    assert_that(is.notempty.string(taxon))
    this_query$q <- taxon
  }
  
  # optional
  if (!missing(wkt)) {
    assert_that(is.notempty.string(wkt))
    this_query$wkt <- wkt
  }
  
  ## ammend if I leave out the above
  if (length(this_query) == 0) {
    stop("invalid request: need at least one of taxon, fq, or wkt to be specified")
  }
  
  ## leave for now
  if (method == "offline") {
    #if (record_count_only) 
    #    stop("record_count_only can only be used with method=\"indexed\"")
    if (missing(email) || !is.string(email) || nchar(email) < 1) 
      stop("email is required for method=offline")
  }
  
  ## here I think I can just assume that a numeric reason is always entered.
  ## could even hard-code one.
  reason_ok <- !is.na(download_reason_id)
  if (reason_ok) {
    ## Should take a code from 0-12
    reason_ok = reason_ok %in% seq(0:12)
  }
  
  ## modify later
  if (!reason_ok) {
    stop("download_reason_id must be a valid reason_id. See...")
  }
  
  ## This guy. I think for our purposes we just need a standard set of 
  ## field names. These could be called from a file, or loaded in the 
  ## function. Assume for now it is loaded into memory and called by 
  ## fields arg.
  if (!missing(fields)) {
    assert_that(is.character(fields))
    this_query$fields <- paste(fields, collapse = ",")
  } else {
    if(verbose) cat('... Requesting default fields', sep = '\n')
    fields = as.character(c('occurrenceID',
                            'kingdom.p',
                            'phylum.p',
                            'classs.p',
                            'order.p',
                            'family.p',
                            'genus.p',
                            'subgenus.p',
                            #'specificEpithet',
							'species.p',
                            'scientificName.p',
                            'acceptedNameUsage',
                            'taxonConceptID.p',
                            'taxonRank.p',
                            'eventDate.p',
                            'year.p',
                            'decimalLatitude.p',
                            'decimalLongitude.p',
                            'coordinateUncertaintyInMeters.p'))
    this_query$fields <- paste(fields, collapse = ",")
    
  }
  
  ## Think this should error if missing. As above it will probably be a file
  ## that needs to be in memory (or set to a hard-coded path).
  if (!missing(qa)){
    assert_that(is.character(qa))
    this_query$qa <- paste(qa, collapse = ",")
  } else {    
    if(verbose) cat('... Requesting default quality assertion fields', sep = '\n')
    qa = as.character(c('zeroCoordinates',
                        #'nameNotSupplied',
                        'nameNotRecognised',
                        'decimalLatLongCalculationFromEastingNorthingFailed',
                        'zeroLatitude',
                        #'decimalLatLongCalculationFromVerbatimFailed',
                        'coordinatesCentreOfCountry',
                        'processingError',
                        'occCultivatedEscapee',
                        'coordinatesCentreOfStateProvince',
                        'decimalLatLongConversionFailed',
                        'zeroLongitude'))
    qa = 'includeall'
    this_query$qa <- paste(qa, collapse = ",")
  }
  
  if(dwcHeaders){
    this_query$dwcHeaders = 'true'
  } else {
    this_query$dwcHeaders = 'false'
  }
  
  if (method == "offline") this_query$email <- email
  this_query$reasonTypeId <- download_reason_id
  #this_query$sourceTypeId <- ala_sourcetypeid()
  this_query$esc <- "\\"
  ## This guy! 
  #this_query$sep <- "\t"
  this_query$sep <- ","
  ## Make the file name something intuitive
  #this_query$file <- "data"
  ## Build file name from taxon search and date
  fn = gsub("[[:punct:]]", "_", taxon)
  fn = gsub("[[:punct:]]", "_", fn)
  fn = paste(fn, Sys.Date(), sep = '_')
  fn = gsub(' ', '_', fn)
  this_query$file <- fn

  ## url builder (source utilities_internal.R)
  build_url_from_parts <- function(base_url,path=NULL,query=list()) {
    this_url <- parse_url(base_url)
    this_url$path <- clean_path(this_url$path,path)
    if (length(query)>0) {
      this_url$query <- query
    }
    build_url(this_url)
  }    
  
  ## internal url builder function (source utilities_internal.R)
  clean_path <- function(...,sep="/") {
    path1 <- sapply(list(...),FUN=function(z)paste(z,sep=sep,collapse=sep)) ## collapse individual arguments
    ## workaround to avoid replacing "http://" with "http:/", since this is now used in GUID strings (July 2016)
    path <- paste(path1,sep=sep,collapse=sep) ## paste parts together
    path <- gsub("http://","http:@@",path,fixed=TRUE)
    path <- gsub(paste0("[",sep,"]+"),sep,path) ## remove multiple slashes
    path <- gsub("http:@@","http://",path,fixed=TRUE)
    sub(paste0("^",sep),"",path) ## remove leading slash
  }
  
  ## assign this directly
  base_url = 'http://biocache.ala.org.au/ws/'
  # getOption("ALA4R_server_config")$base_url_biocache    
  
  ## I think this is all good. 
  ## If I want to simply, remove 'indexed' option
  if (method == "indexed"){ 
    this_url <- build_url_from_parts(base_url, c("occurrences", "index", "download"), 
                                     query = this_query)
  } else {
    this_url <- build_url_from_parts(base_url, c("occurrences", "offline", "download"),
                                     query = this_query)
  }
  
  if(dry_run) {
    
    return(this_url)
    
  } else {
    
    ## Build in a pinger to the ALA server.
    if (method == "offline") {
      ## send request	
      result = GET(this_url)
      if(verbose){
        if(status_code(result) == 200) {
          msg = 'ALA query is being processed'
          fmat = paste(rep('-', nchar(msg)), collapse = '')
          requestStatus = paste('Request status: ', content(result)$status, sep = '')
          queueSize = paste('Queue size....: ', content(result)$queueSize, sep = '')
          statusCheck = paste('Status check..: ', content(result)$statusUrl, sep = '')
          cat('', fmat, msg, fmat, requestStatus, queueSize, statusCheck, sep = '\n')
          shell.exec(paste(content(result)$statusUrl))
        } else {
          msg = paste(': Something has gone wrong with the request ',
                      '(status ', status_code(result), ')', sep = '')
          errorType = content(result)$errorType
          cat(errorType, msg, sep = '')
        }
      }
    
    } else {
      
      if(is.null(dst)){
        tmp = tempfile()
        response = GET(this_url, write_disk(tmp, overwrite=TRUE))
        if(response$status_code != 200) cat(response$status_code)
        dump_into = tempdir()
        ## assumes data.csv is always first...
        df <- unzip(tmp, exdir = dump_into)[1] %>% read.csv(stringsAsFactors=FALSE)
        return(df)
      } else {
        dst = check_filepath(dst)
        dst = paste0(dst, this_query$file, '.zip')
        response = GET(this_url, write_disk(dst, overwrite=TRUE))
        if(response$status_code != 200) cat(response$status_code)
        if(read){
          dump_into = tempdir()
          ## assumes data.csv is always first...
          df <- unzip(dst, exdir = dump_into)[1] %>% read.csv(stringsAsFactors=FALSE)
          return(df)
        }
      }
    }
  }
}

check_filepath = function(filepath){
  ## check for trailing forwardslash and backslashes
  check_nchar = substr(filepath, nchar(filepath), nchar(filepath))
  if(check_nchar == '/'){
    return(filepath)
  } else if(check_nchar == '\\') {
    return(filepath)
  } else {
    return(paste0(filepath, '/'))
  }
}




