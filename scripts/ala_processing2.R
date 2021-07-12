## >> Clean II for downloaded ALA data ####

## >>>>>> PREVIOUS SCRIPT <<<<<<< ####
file.edit("/tempdata/workdir/nesp_bugs/scripts/ala_processing1.R")


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


## >>>>>> NEXT SCRIPT <<<<<<< ####
file.edit("/tempdata/workdir/nesp_bugs/scripts/nonala_processing.R")