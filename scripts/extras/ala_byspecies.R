## >> Clean II for downloaded ALA data ####

## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "sp", "raster", "stringr", "rnaturalearth")
lapply(x, require, character.only = TRUE)
rm(x)

## Server paths
output_dir <- file.path("/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs")
data_dir = file.path(output_dir, "ala_data" ,"spdata")
dir.create(data_dir)
map_dir = file.path(data_dir, "spmaps_unmasked")
dir.create(map_dir)

# ## Local paths
# output_dir = "/Volumes/uom_data/nesp_bugs_data/outputs"
# data_dir = file.path(output_dir,  "ala_data" ,"spdata")
# dir.create(data_dir)
# map_dir = file.path(data_dir, "spmaps_unmasked")
# dir.create(map_dir)


## Load cleaned ALA data ####
ala_dat <- list.files(output_dir, 
                      pattern = "clean1_ala*.*.rds$", 
                      full.names = TRUE)
ala_dat <- readRDS(ala_dat)
message(cat("Number of records in data: "),
        nrow(ala_dat))


## Drop qa columns without values (i.e. all FALSE values) ####
qa <- as.data.frame(read.csv(file.path(output_dir, "qa_assertions.csv")))
x <- qa$name

drop_cols <- c()
for(i in x){
  if(eval(parse(text = paste0("sum(ala_dat$", i, ")"))) == 0){
    drop_cols <- c(drop_cols, i)
  }
}

message("Columns being dropped:")
drop_cols
message(cat("Number of columns being dropped: "),
        length(drop_cols))
ala_dat[, (drop_cols) := NULL]
message(cat("Number of columns in updated data: "),
        dim(ala_dat)[2])


## Drop records with issues ####
## Issues as commented by JM
exclude <- qa[which(qa$exclude == 1),]$name
out <- c()

## Find number of records with issues 
for(i in exclude){
  message(cat(paste0("Number of records with issue - ", i, ": ")),
          eval(parse(text = paste0("sum(ala_dat$", i, ")"))))
  out <- c(out, eval(parse(text = paste0("sum(ala_dat$", i, ")"))))
}

## Remove records
for(i in 1: length(exclude)){
  if(any(out[i] > 0)){
    ala_dat <- eval(parse(text = paste0("ala_dat[", exclude[i], " == TRUE]")))
  } else {
    message(cat("No records found with issue: "),
            exclude[i])
  }
}


## Create species file names & add to data as column ####
## NOTE: This step revelas duplicates in species names in the data;
## These are corrected in the data, not dropped
ala_species <- sort(unique(ala_dat$scientificName))
message(cat("Number of species with data: "),
        length(ala_species))
message(cat("Duplicates found in ala_species: "),
        sum(duplicated(ala_species)))

## Text modification
spfilename <- stringr::str_replace_all(ala_species, " ", "00xx00")
message(cat("Duplicates found in spfilename: "),
        sum(duplicated(spfilename)))

spfilename <- str_replace_all(spfilename, "[^[:alnum:]]", "")
message(cat("Duplicates found in spfilename: "),
        sum(duplicated(spfilename)))

spfilename <- tolower(gsub("00xx00", "_", spfilename))
message(cat("Duplicates found in spfilename: "),
        sum(duplicated(spfilename)))
spfilename <- unique(spfilename)
length(unique(spfilename))

## Identify duplicates in species name in data
spfilename[duplicated(spfilename)]
grep("incertae", spfilename)
ala_species[grep("incertae", spfilename)]
temp <- ala_species[grep("incertae", spfilename)]
dim(ala_dat[scientificName == temp[1]])
dim(ala_dat[scientificName == temp[2]])

## Modify data to correct duplicates in species name; no records dropped
ala_dat[scientificName == temp[2], scientificName := temp[1]]
dim(ala_dat[scientificName == temp[1]]) ## check
dim(ala_dat[scientificName == temp[2]]) ## check

## Add species file names column to data
y <- ala_dat$scientificName
y <- str_replace_all(y, " ", "00xx00")
y <- str_replace_all(y, "[^[:alnum:]]", "")
y <- tolower(gsub("00xx00", "_", y))
x <- sort(unique(y))
sum(x == sort(spfilename)) ## Check
ala_dat[, spfile := y]
dim(ala_dat)

## Display fields used to identify duplicates & check for NAs within ####
grep("ID|id", names(ala_dat), value = TRUE)
grep("catalogue", names(ala_dat), value = TRUE)
sum(is.na(ala_dat$collectionID)) ## https://dwc.tdwg.org/list/#dwc_collectionID
sum(is.na(ala_dat$institutionID))
sum(is.na(ala_dat$catalogueNumber))

## Check 'duplicateStatus' fields - NOT USEFUL
## This field is not used to subset data
grep("dup", names(ala_dat), value = TRUE)
unique(ala_dat$duplicateStatus) ## https://github.com/AtlasOfLivingAustralia/ala-dataquality/wiki/duplicate_status
ala_dat[,.N,by = duplicateStatus]

## Summary of cleaned data ####
message(cat("Number of records in cleaned data: "),
        nrow(ala_dat))
ala_species <- sort(unique(ala_dat$scientificName))
message(cat("Number of unique species in cleaned data: "),
        length(ala_species))

## Save updated data (all) ####
saveRDS(ala_dat, file = file.path(output_dir, paste0("clean2_ala_", Sys.Date(),".rds")))
write.csv(ala_dat, file = file.path(output_dir, paste0("clean2_ala_", Sys.Date(),".csv")))



## >> Save data by species ####

## Set working environment ####
x <- c("data.table", "sp", "raster", "stringr", "future", "future.apply")
lapply(x, require, character.only = TRUE)
rm(x)

## Server paths
output_dir <- file.path("/tempdata/research-cifs/uom_data/nesp_bugs_data/outputs")
data_dir = file.path(output_dir, "ala_data" ,"spdata")
# data_dir = file.path("/tempdata/workdir/nesp_bugs/temp/spdata")
dir.create(data_dir)
map_dir = file.path(data_dir, "spmaps_unmasked")
dir.create(map_dir)

source(file.path(getwd(), "nesp_bugs/scripts/save_spdata.R"))

## Load data ####
ala_dat <- list.files(output_dir,
                      pattern = "clean2_ala*.*.rds$",
                      full.names = TRUE)
ala_dat <- as.data.table(readRDS(ala_dat))
ala_species <- sort(unique(ala_dat$scientificName))
mask.file = file.path(output_dir, "masks","ausmask_noaa_1kmWGS_NA.tif")


## Loop through species to save files ####

## In parallel ####
plan(multiprocess, workers = future::availableCores()-2)
options(future.globals.maxSize = +Inf) ## CAUTION: Set this to a value, e.g. availablecores-1?/RAM-10?

## Error log file
errorlog <- paste0(output_dir, "/errorlog_ala_byspeciesR_", gsub("-", "", Sys.Date()), ".txt")
# errorlog <- paste0("/tempdata/workdir/nesp_bugs/temp/errorlog_ala_byspeciesR_", gsub("-", "", Sys.Date()), ".txt") 
writeLines(c(""), errorlog)

start.time <- Sys.time()
invisible(future.apply::future_lapply(ala_species,
                                      function(x){
                                        tmp <- tryCatch(expr = save_spdata(species_name = x, 
                                                                           ala_data = ala_dat, 
                                                                           maskfile = mask.file, 
                                                                           data_dir, 
                                                                           map_dir),
                                                        error = function(e){ 
                                                          print(paste("\nError: More than 1 unique spfile for naming species file for...", x))
                                                          cat(paste(x, "\n"),
                                                              file = errorlog, 
                                                              append = TRUE)
                                                        })
                                      }))
end.time <- Sys.time()
end.time - start.time


## Check files
length(list.files(data_dir, pattern = ".txt$"))
length(list.files(data_dir, pattern = ".rds$"))
length(list.files(map_dir, pattern = ".pdf$"))
lapply(list.files(data_dir, pattern = ".rds$", full.names = TRUE)[344:355], object.size)

n <- sample(1:length(ala_species), 1)
readRDS(list.files(data_dir, pattern = ".rds$", full.names = TRUE)[n]) ## read rds file
browseURL(list.files(data_dir, pattern = ".rds$", full.names = TRUE)[n]) ## download data as zip file
browseURL(list.files(map_dir, pattern = ".pdf$", full.names = TRUE)[n]) ## open pdf map in new tab


# ## In sequence ####
# n <- data.frame()
# ctr <- 0
# 
# start <- Sys.time()
# for (i in ala_species){
#   ctr <- ctr + 1
# 
#   ## Get species data
#   message(cat("Step ", ctr, "of ", length(ala_species), "..."))
#   message(cat("Processing species: "),
#           i)
#   temp <- ala_dat[scientificName == i]
#   n1 <- nrow(temp)
# 
#   # ## Remove duplicate records - by name-lat-long-date
#   # ## NOTE: Too many records lost with this approach - DICARD STEP
#   # temp1 <- temp[is.na(eventDate)] ## store records with eventDate = NA
#   # temp2 <- temp[!is.na(eventDate)] ## find duplicates for records with eventDate != NA
#   # temp2 <- temp2[!duplicated(temp2[ , c("scientificName", "longitude", "latitude", "eventDate")]), ]
#   # temp <- as.data.table(rbind(temp1, temp2))
#   # n2 <- nrow(temp)
# 
#   ## Remove duplicate records - c("catalogueNumber", "collectionID", "institutionID")
#   temp <- temp[!duplicated(temp[ , c("catalogueNumber", "collectionID", "institutionID")]), ]
#   n2 <- nrow(temp)
# 
#   ## Tabulate number of records lost
#   n <- rbind(n, c(n1, n2, n1-n2))
# 
#   ## Save species file
#   spfile <- unique(temp$spfile)
#   if (length(spfile) > 1){
#     stop("Error: More than 1 unique spfile for naming species file...")
#   }
#   saveRDS(as.data.table(temp),
#           file = file.path(data_dir, paste0(spfile, ".rds")))
# 
#   ## Save species map
#   map_filename <- sprintf("%s/%s.pdf",
#                           map_dir,
#                           spfile)
#   pdf(map_filename)
# 
#   reg.mask <- raster(mask.file)
#   plot(reg.mask, col = "grey", axes = FALSE, box = FALSE, legend = FALSE)
#   points(temp[,.(longitude, latitude)], pch = 4, col = "blue", cex = 0.5)
# 
#   # ## Example
#   # plot(ausmask)
#   # lat <- ala_dat[scientificName == "Inquisitor flindersianus"]$latitude
#   # long <- ala_dat[scientificName == "Inquisitor flindersianus"]$longitude
#   # points(long, lat)
# 
#   dev.off()
# }
# 
# ## Record time taken to complete data sving by species
# end <- Sys.time()
# end - start
# 
# ## Save number of duplicated records lost for each species
# names(n) <- c("org", "final", "duplicates")
# rownames(n) <- ala_species
# 
# # ## Explore nnumber of duplicate records lost
# # range(n$duplicates)
# # plost <- n$duplicates/n$org
# # range(plost)
# 
# saveRDS(n, file = file.path(output_dir, "duplicate_counts.rds"))
# write.csv(n, file = file.path(output_dir, "duplicate_counts.csv"))


