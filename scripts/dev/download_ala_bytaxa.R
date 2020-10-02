#'@title Download occurrences for a list of taxa from the ALA 
#'
#'@description Takes a vector of taxonomic names and searches the ALA for these. Can save the data and/or load to memory. 
#'
#'@param specieslist (vector) Vector of taxonomic names to search for.
#'@param dst (string) Optional dstination filepath to write data to. If missing, tempdir is used.
#'@param parallel (boolean) Split list across multiple CPUs (all available -2). Default FALSE.
#'@param background (boolean) Runs function in background freeing up the gui.
#'@param ... Additional named arguments to pass to gdmEngine::download_ala which is called internally.
#'
#'@return std.output
#'
#'@examples download_taxalist(c('spp1', 'spp2'), 'tempdir()'C:/Users/xyx, read = FALSE)
#'
#'@export
download_taxalist = function(specieslist, dst = NULL, parallel = FALSE,
                             background = FALSE, ...){
  
  ## add in start-fresh or overwrite flag
  ## make an option for just downloading zip files (probably with a log file).
  
  ## first check if function is to be run in background (to free up gui)
  if(background) {
    
    if(length(list(...))) {
      
      ## to do...
      stop('Cannot run in background currently if ... args used')
      
    } else {
      
      ## get call
      this_call = match.call()
	  spp_obj = as.character(this_call$specieslist)  
	  this_call$background = FALSE
      this_call = paste0(deparse(this_call), collapse = '')
      
      ## set up input arg in tmp dir/file
      tmp_dir = tempdir()
      tmp_file = paste0(tmp_dir, '/tmp_spplist.csv')
      write.csv(specieslist, tmp_file, row.names = FALSE)
      tmp_file = gsub('\\', '\\\\', tmp_file, fixed = TRUE)
      
      ## dst file 
      if(!is.null(dst)) {
        dst = gsub('\\', '\\\\', dst, fixed = TRUE)
      }
      
      ## sink script to run func in tmp dir
      tmp_script = paste0(tmp_dir, '/tmp_script.R')
      sink(tmp_script)
      #cat('#!/usr/bin/env Rscript')
      cat('library(gdmEngine)', sep = '\n')
      cat('library(parallel)', sep = '\n')
      cat(sprintf('%s = read.csv("%s", stringsAsFactors = FALSE)', spp_obj, tmp_file), sep = '\n')
	  cat(sprintf('%s = as.character(%s[, 1])', spp_obj, spp_obj), sep = '\n')
      if(!is.null(dst)) cat(paste0('dst = ', '"', dst, '"'), sep = '\n')
      cat(this_call, sep = '\n')
      sink()
      closeAllConnections()
      
      ## execute
      make_call = paste0('R CMD BATCH ', tmp_script)
	  ## get system pid so that progress can be tracked
	  before = get_pids()
	  system(make_call, wait = FALSE)
	  after = get_pids()
	 
      heads_up = paste0('Searching the ALA for records. Files will be downloaded here: ', '\n',
                        dst)
      cat(heads_up, sep = '\n')
	  cat('Use job_status(pid) check stutus of this download')
      ## done
      return(outersect(before, after))
      
    }
  }
  
  ## check args
  if(!is.null(dst)){
    if(!dir.exists(dst)) dir.create(dst)
  } else {
    dst = tempdir()
  }  
  ## in case the list is derived from a data.frame
  if(is(specieslist, 'factor')){
    specieslist = as.character(paste(specieslist))
  }
  ## check filepath ending
  dst = check_filepath(dst)
  
  ## config dst for raw downloads
  raw_files = paste0(dst, 'raw_files')
  if(dir.exists(raw_files)){
    cat('Warning: dst exists and contents will be written over...')
  } else {
    dir.create(raw_files)
  }
  
  ## default args
  download_args = list(
    method = "indexed", 
    download_reason_id = 7,
    dst = raw_files,
    read = FALSE,
    verbose = FALSE)
  
  ## check for user download args
  user_args = list(...)
  if (length(user_args)) {
    ## all opts of gdmEngine::download_occurrences
    opts = formals(download_occurrences)
    for (i in 1:length(user_args)) {
      this_opt <- names(user_args)[i]
      if (! (this_opt %in% names(opts))) {
        cat(paste0("'", this_opt, "'", ' is not a valid option. Should be one of:'), 
            names(opts), sep = '\n')
      } else {
        download_args[[this_opt]] = paste(user_args[i])
      }
    } # end user_opts 
  } # end 
  
  ## log container
  log = list()
  log$ALA_DOWNLOAD = Sys.Date()
  log_f = paste0(dst, 'ALA_download_log_', Sys.Date(), '.RData')
  save(log, file = log_f)
  
  ## run
  try(repeat{
    
    load(log_f)
    not_done <- outersect(specieslist, names(log)[-1])
    
    ## Break check
    if(length(not_done)==0) break 
    
    ## Not finished...? Start looping again.
    
    if(parallel){
      no_cores = parallel::detectCores() - 2
      cl = parallel::makeCluster(no_cores)
      clusterExport(cl, list(), envir=environment())
      #clusterExport(cl, list('download_occurrences', 'download_args',
      #                       'check_filepath', 'counter', 'outersect'))
      #clusterEvalQ(cl, library(ALA4R))
      clusterEvalQ(cl, library(gdmEngine))
      
      cat(paste0('Searching the ALA for records of ', length(not_done),
                 ' taxa...'))

      check = parLapply(cl, not_done, function(x){
        tryCatch({
          do.call(download_occurrences, 
                  c(list(taxon = x), download_args))
        }, 
        error = function(e) {
          e
        })
      })
      
      ## log
      check = lapply(check, function(x)
        if(is.null(x)) x = 'Successfully downloaded file')
      log = c(log, check)
      names(log)[2:length(log)] = not_done
      
      gc()
      
      stopCluster(cl = cl)
      
    } else { # end if parallel
      
      ## loop
      for(spp in not_done){
        
        ## counter to print to console
        print(paste0('Searching the ALA for records of ', spp))
        
        ## Download records from ALA 
        ## Log any errors in downloads by catching them and passing them
        ## through to the log. 
        check <- tryCatch(ala <- do.call(download_occurrences, 
                                         c(list(taxon = spp), download_args)),
                          error = function(e) e)
        
        ## log
        if(!is.null(check)){ ## signifies error
          log[[spp]] = check
        } else {
          log[[spp]] = 'Successfully downloaded file'
        } 
        
        ## Give the ALA a break  
        ## Sys.sleep(5)
        
      } # end spp loop
      
      gc()
      
    } # end else serial
    
    ## save log
    save(log, file = log_f)
    
  })
  
  ## summary
  search_summary = paste('Found records for ', 
                         length(log[log == 'Successfully downloaded file']),
                         'of', length(specieslist), 'listed taxa')
  cat('\n')
  cat('Completed occurrence searches', sep = '\n')
  cat(search_summary)
  
}

# (hacky) code to return pid from system call
#'@title Get list of pids
#'@export
get_pids = function(){
  now = grep("^Rterm.exe",readLines(textConnection(system('tasklist',intern=TRUE))),value=TRUE)
  pids = NULL
  for (i in now){
    p = lapply(strsplit(i, ' ')[[1]], function(x) 
      Filter(haschar, x))
    pids = c(pids, unlist(p)[2])
  }
  
  return(pids)
}

haschar = function(x) ifelse(x == "", FALSE, TRUE)

outersect = function(x, y){
  sort(c(setdiff(x, y), setdiff(y, x)))
}

#'@title Check status of background process
#'@export
job_status = function(pid){
  now = get_pids()
  if(pid %in% now){
    cat(sprintf('Process %s still running', pid), sep = '\n')
  } else {
    cat(sprintf('Process %s not running', pid), sep = '\n')
  }
}
