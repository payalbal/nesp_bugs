#'@title Merge datasets downloaded from the ALA
#'
#'@description Unzip and load CSV files downloaded from the ALA. 
#'
#'@param src (string) Filepath to directory where loadeds are saved. Must not be other .zip files stored in src.
#'@param output.folder (string) A folder to save the outputs to. If none specified, no file is written.
#'@param output.name (string), A name to use in saving the outputs. Default: 'merged_taxa_data'.
#'@param keep_unzip (boolean) Save the unzipped contents of each file?
#'@param parallel (boolean) If TRUE will use all CPU (-2) available to load data. (default = FALSE)
#'@param verbose (boolean) Print messages to console. Default TRUE.
#'
#'@return List. $data is a data.frame with the combined records. $log is a data.frame with the taxa names searched, 
#'records returned, and any errors.
#'
#'@examples output = merge_downloads('C:/Users/raw_files')
#'
#'@importFrom magrittr %>%
#'@importFrom parallel detectCores makeCluster clusterExport clusterEvalQ parLapply stopCluster
#'@importFrom data.table rbindlist
#'
#'@export
merge_downloads = function(src, 
                           output.folder = NULL,       
                           output.name = "merged_taxa_data",
                           keep_unzip = FALSE,
                           parallel = FALSE, 
                           verbose = TRUE)
  {
  
  filelist = list.files(src, full.names = TRUE, pattern = '.zip')  
  
  unzipper = function(fn, memory_only = TRUE){
    temp_dump = paste(dirname(fn), gsub('.zip', '', basename(fn)), sep = '/')
    dir.create(temp_dump)
    df = tryCatch(unzip(fn, exdir = temp_dump)[1] %>% 
                  read.csv(stringsAsFactors=FALSE),
                  error = function(e) e)
    if(memory_only) unlink(temp_dump, recursive = TRUE) 
    return(df)
  }
  
  
  if(parallel){
    no_cores = parallel::detectCores() - 2
    cl = parallel::makeCluster(no_cores)
    clusterExport(cl, list(), envir=environment())
    #clusterExport(cl, list('unzipper', 'filelist'))
    clusterEvalQ(cl, library(dplyr))  
    clusterEvalQ(cl, library(gdmEngine))  
    cat(paste0('Extracting data from ', length(filelist),
               ' files...'))
    
    ptm = proc.time()
    datasets = parLapply(cl, filelist, unzipper)
    proc.time()-ptm
    
    gc()
    
    stopCluster(cl = cl)
    
  } else { # end if parallel
    
    datasets = lapply(filelist, unzipper)
    
  } # end if serial
  
  ## check for anything that is not a data.frame
  check_df = sapply(datasets, function(x) is(x, 'data.frame'))
  not_df = which(!check_df)
  
  ## create log
  log = data.frame(matrix(nrow = length(filelist), ncol = 0))
  log$taxa = sapply(filelist, function(x) gsub('.zip', '', basename(x)))
  log$n_records = sapply(datasets, nrow)
  
  ## deal with not_df
  for (x in not_df){
    if(is(datasets[[x]], 'simpleError')){
      log$n_records[[x]] = gsub(' ', '_', datasets[[x]]$message)
    } else {
      log$n_records[[x]] = 'ERROR'
    }
  }
  
  ## rm NULL datasets and rbind
  if(length(not_df)>0) #added 24/11/17
    {datasets = datasets[-c(not_df)]}
  data_bound = rbindlist(datasets)
  
  ## output
  bundle = list(data = as.data.frame(data_bound),
                log = log)

  # write the data to file, if an output folder is specified #***********************************
  if(!is.null(output.folder))
    {
    if(!dir.exists(output.folder))
      {
      dir.create(output.folder)
      }# end if !dir.exists
    out.path <- file.path(output.folder,paste0(output.name,"_",Sys.Date(),".csv")) 
    write.csv(as.data.frame(data_bound), out.path, row.names=FALSE)
    # write a log file describing how the data was created *************************************
    fileConn<-file(file.path(output.folder,paste0(output.name,"_",Sys.Date(),"_log_file.txt")),'w')
    writeLines("#######################################################################",con = fileConn)
    writeLines("###",con = fileConn)
    writeLines("### ALA data filtration log file ",con = fileConn)
    writeLines("###",con = fileConn)
    writeLines(paste0("### Created ",Sys.time()," using the merge_downloads() function."),con = fileConn)
    writeLines("###",con = fileConn)
    writeLines("#######################################################################",con = fileConn)
    writeLines("",con = fileConn)
    writeLines(paste0("Output data file = ", out.path),con = fileConn)
    writeLines(paste0("Number of taxa combined = ", length(filelist)),con = fileConn)
    writeLines(paste0("Total number of records = ", nrow(data_bound)),con = fileConn)
    writeLines("#######################################################################",con = fileConn)
    close(fileConn) #**************************************************************************
  } # end if !is.null #************************************************************************
  
  
    
  # Write some messages out to the console
  if(verbose){
    msg1 = 'Returned object is a list.'
    msg2 = paste('$data is a data.frame with ', nrow(data_bound), 'records')
    msg3 = '$log is a summary data.frame.'
    cat(paste(msg1, msg2, msg3, sep = '\n'))
  }
  
  return(bundle)
  
} # end merge_downloads()
