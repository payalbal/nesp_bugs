## Integrating ALA and nonALA data for species found in non ALA data


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "rje", "stringr", "sp", "raster")
lapply(x, require, character.only = TRUE)
rm(x)


## Server paths
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
nonala_dir = file.path(bugs_dir, "nonALA")
output_dir = file.path(bugs_dir, "outputs")

newdata_dir <- file.path(output_dir, "combined_data")
dir.create(newdata_dir)

dat_cols <- c("data_source", "id", "class", "family", "scientificName", "latitude", "longitude", "year", "sensitive", "habitat")


## Load datasets ####
## ALA data cleaned data: clean2_ala_
## >> All ALA cleaned data (as rds files)
spmasked_dir <- file.path(output_dir, "ala_data" ,"spdata_masked")
datfiles <- list.files(spmasked_dir, pattern = "_masked.rds$",
                       full.names = TRUE, all.files = TRUE)
datspecies <- basename(tools::file_path_sans_ext(datfiles))
datspecies <- gsub("_masked", "", datspecies)
message(cat("Number of ALA species data files: "),
        length(datspecies))

## >> Non ALA cleaned data: clean1_nonala_
nonala_data <- fread(list.files(output_dir,
                                pattern = "clean1_nonala*.*csv$",
                                full.names = TRUE))


## >> Find species in non ALA data also found in ALA
nonala_sp <- unique(nonala_data$spfile)
sum(nonala_species %in% datspecies)
nonala_species <- nonala_species[nonala_species %in% datspecies]
length(nonala_species)
datfiles1 <- datfiles[datspecies %in% nonala_species]
length(datfiles1)

  ## Check
  all(sort(nonala_species) == sort(gsub("_masked", "", basename(tools::file_path_sans_ext(datfiles1)))))

  # datfiles1 <- datfiles[grep(paste(species,collapse="|"), datfiles, perl = TRUE)]
  #   ## regular expression is too large to match for grep
  
  # ## >> Copy files for non ALA data species found in ALA files - TO BE DONE ONCE
  # dir.create(file.path(output_dir, "combined_data", "ala_files"))
  # all(file.copy(datfiles1, file.path(output_dir, "combined_data", "ala_files"),
  #           overwrite = FALSE, recursive = FALSE,
  #           copy.mode = TRUE, copy.date = TRUE))


## >> Load ALA data for identified species
ala_data <- do.call("rbind", lapply(datfiles1 , readRDS))
ala_data <- setDT(ala_data)
dim(ala_data)
length(unique(ala_data$spfile))
length(datfiles1)

## >> Format ALA data table
names(ala_data)
ala_data <- ala_data[ , .(id, class, family, scientificName, latitude, longitude, year, spfile)]
length(unique(ala_data$spfile))
ala_data$data_source <- rep("ALA", nrow(ala_data))
ala_data$sensitive <- rep(0, nrow(ala_data))
ala_data$habitat <- rep("NA", nrow(ala_data))
ala_data <- ala_data[,c(9,1:7,10,11,8)]
names(ala_data) <- names(nonala_data)

## >> Add ALA to nonALA data ####
dat <- rbind(ala_data, nonala_data)
length(unique(dat$scientificName))
length(unique(dat$spfile))

## >> Create new column to give unique values by scientificName
## Create new_id column to give unique ID by scientific name
setDT(dat)[, new_id := .GRP, by = scientificName]
length(unique(dat$new_id))
range(unique(dat$new_id))
## Merge spfile and new_ID
dat$spfile <- paste0(dat$spfile, "_", dat$new_id)
length(unique(dat$scientificName))
length(unique(dat$spfile))
dat[,new_id := NULL]


## Find duplicates and prioritise ALA records for removal ####
dim(dat)
message(cat("Number of unique scientificName in new dataset: "),
        length(unique(dat$scientificName)))
message(cat("Number of unique spfile in new dataset: "),
        length(unique(dat$spfile)))

sum(duplicated(dat[,c("scientificName", "latitude", "longitude")]))
dat[, .N, by = sort(data_source)]


  # temp <- dat[duplicated(dat[,c("scientificName", "latitude", "longitude")]),]
  # unique(temp$scientificName)[6:10]
  # dim(dat[scientificName == "Acercella falcipes"])
  # temp <- dat[scientificName == "Acercella falcipes"]
  # temp1 <- temp[duplicated(temp[,.(scientificName, latitude, longitude)]), ]
  # temp1 <- temp1[,.(data_source, scientificName, latitude, longitude)]
  # temp2 <- temp[!duplicated(temp[,.(scientificName, latitude, longitude)]), ]
  # temp2 <- temp2[,.(data_source, scientificName, latitude, longitude)]
  # temp3 <- setDT(temp)[order(-data_source), .SD[1L] ,.(scientificName, latitude, longitude)]
  # temp3 <- temp3[,.(data_source, scientificName, latitude, longitude)]

dat <- setDT(dat)[order(-data_source), .SD[1L] ,.(scientificName, latitude, longitude)]
  ## How this works: 
  ## > setDT - set as data.table
  ## > order in decreasing order by data_source (i.e. so that alphabetically ALA comes last)
  ##    this is same as setorder(dat, -data_source)
  ## > .SD[1L] get the first observation for each group by .(scientificName, latitude, longitude)
  ## > .(scientificName, latitude, longitude) defines the groups
  ## Ref: https://cran.r-project.org/web/packages/data.table/vignettes/datatable-sd-usage.html
  ## Ref: https://stackoverflow.com/questions/8508482/what-does-sd-stand-for-in-data-table-in-r
dat[, .N, by = sort(data_source)]


## >> Save combined data table
## This dataset contains all nonALA data and a subset of the ALA data 
## i.e., subset of species in ALA found in nonALA data.
dat <- setDT(dat)
setorder(dat, scientificName)
dim(dat)
message(cat("Number of unique scientificName in new dataset: "),
        length(unique(dat$scientificName)))
message(cat("Number of unique spfile in new dataset: "),
        length(unique(dat$spfile)))

write.csv(dat, file = file.path(newdata_dir, "data_ALAnonALA_1.csv"), row.names = FALSE)


## Bring in rest of ALA data ####
## >> Find ALA files not included in the above dataset
sum(!(datspecies %in% nonala_species))
ala_species <- datspecies[!(datspecies %in% nonala_species)]
length(ala_species)
datfiles2 <- datfiles[datspecies %in% ala_species]
length(datfiles2)

  ## Check
  all(sort(ala_species) == sort(gsub("_masked", "", basename(tools::file_path_sans_ext(datfiles2)))))

  
## >> Load ALA data for identified species
ala_data2 <- do.call("rbind", lapply(datfiles2 , readRDS))
ala_data2 <- setDT(ala_data2)
dim(ala_data2)
length(unique(ala_data2$spfile))
length(datfiles2)

## >> Format ALA data table
names(ala_data2)
ala_data2 <- ala_data2[ , .(id, class, family, scientificName, latitude, longitude, year, spfile)]
length(unique(ala_data2$spfile))
ala_data2$data_source <- rep("ALA", nrow(ala_data2))
ala_data2$sensitive <- rep(0, nrow(ala_data2))
ala_data2$habitat <- rep("NA", nrow(ala_data2))
ala_data2 <- ala_data2[,c(9,1:7,10,11,8)]
names(ala_data2) <- names(nonala_data)

## Apply 1990 filter


## Save rds files by species ####


## For polygons
names(data) <- c("ddlat", "ddlon", "tax", "family", "coly", "spfile")




# ## EXTRAS
# dat <- data.frame("id" = 1:10,
#                   "data_source" = c(rep("ALA", 4), 
#                                     rep("Other", 3), 
#                                     rep("VBA", 3)),
#                   "scientificName" = c(rep("pinkus unicornius", 2), 
#                                        rep("bunyips ferocii", 6), 
#                                        rep("nerdius quanticus", 2)),
#                   "latitude" = c(-37, rep(-37.5, 2), rep(-38, 3), 
#                                  rep(-37.5, 2), rep(-38, 2)),
#                   "longitude" = c(rep(145, 2), 145.5, rep(142, 3), 
#                                   rep(145, 2), rep(145.5, 2)))
# 
# dat2 <- setDT(dat)
# 
# dat4 <- setorder(dat2[order(-data_source) , .SD[1L] ,.(scientificName, latitude, longitude)], id)
#   # ## same as...
#   # dat5 <- setorder(dat2, -data_source)
#   # dat5 <- dat5[!duplicated(dat[,.(scientificName, latitude, longitude)])]
# 
# dat3 <- unique(dat2, by = c("data_source", "scientificName", "latitude", "longitude"))
# dat5 <- unique(dat3, by = c("scientificName", "latitude", "longitude"))


