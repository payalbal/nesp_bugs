## Data preparation for ALA-ARDC project
## Based on NESP data and scripts: https://github.com/payalbal/nesp_bugs
## Step 1: Get  clean2_ala_2020-10-28.csv as prepared in https://github.com/payalbal/nesp_bugs/blob/master/scripts/ala_processing1.R
## Step 2: Run this script based on https://github.com/payalbal/nesp_bugs/blob/master/scripts/data_ALAnonALA.R


## --------------------------------------------------------- ##
## Set working environment ####
## --------------------------------------------------------- ##

rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "rje", "stringr", 
       "sp", "raster", "sf", "lubridate", 
       "future", "future.apply")
lapply(x, require, character.only = TRUE)
rm(x)


## Server paths
bugs_dir = "/tempdata/research-cifs/6300-payalb/uom_data/nesp_bugs_data"
nonala_dir = file.path(bugs_dir, "nonALA")
output_dir = file.path(bugs_dir, "outputs")




## --------------------------------------------------------- ##
## Load datasets ####
## --------------------------------------------------------- ##

## >> Non ALA cleaned data: clean1_nonala_
nonala_data <- fread(list.files(output_dir,
                                pattern = "clean1_nonala*.*csv$",
                                full.names = TRUE))
setDT(nonala_data); dim(nonala_data)


## >> ALA data cleaned data: clean2_ala_
ala_data <- fread(list.files(output_dir,
                             pattern = "clean2_ala*.*csv$",
                             full.names = TRUE))
setDT(ala_data); dim(ala_data)

## >> ALA data summary
## from https://github.com/payalbal/nesp_bugs/blob/master/scripts/ala_summary.R




## --------------------------------------------------------- ##
## Complete year info for ALA data where possible ####
## --------------------------------------------------------- ##
names(ala_data)[grep("Date", names(ala_data))]
sum(is.na(ala_data$eventDate))
sum(is.na(ala_data$year))
sum(ala_data$year == "", na.rm = TRUE)
nrow(ala_data[is.na(eventDate) & is.na(year)])
range(ala_data$year, na.rm = TRUE)


## >> Find records for which year can be extracted
sum(is.na(ala_data$year) & !is.na(ala_data$eventDate))
id <- which(is.na(ala_data$year) & !is.na(ala_data$eventDate))


## >> Specify year based in eventDate for records found
ala_data$eventDate[id]
range(year(ymd(ala_data$eventDate[id])))
ala_data$year[id]
ala_data$year[id] <- year(ymd(ala_data$eventDate[id]))
range(ala_data$year[id])
sum(is.na(ala_data$year))

## >> Summary
message(cat("Proportion of ALA records without evenDate info: "),
        nrow(ala_data[is.na(eventDate)])/nrow(ala_data))
message(cat("Proportion of ALA records without year info: "),
        nrow(ala_data[is.na(year)])/nrow(ala_data))
message(cat("Number of records with collection Date listed as invalid: "),
        sum(ala_data$invalidCollectionDate))
sum(is.na(ala_data$verbatimEventDate)) ## "" instead of NAs




## --------------------------------------------------------- ##
## Format ALA data table ####
## --------------------------------------------------------- ##
names(ala_data)

## >> Subset table
ala_data <- ala_data[ , .(id, class, family, scientificName, latitude, longitude, year, spfile)]

## >> Specify data_source
ala_data$data_source <- rep("ALA", nrow(ala_data))

## >> Indicate sensitive records
ala_data$sensitive <- rep(0, nrow(ala_data))

## >> Populate habitat column
ala_data$habitat <- rep("NA", nrow(ala_data))

## >> Rearrange data.table & name columns
ala_data <- ala_data[,c(9,1:7,10,11,8)]
names(ala_data) <- names(nonala_data)




## --------------------------------------------------------- ##
## Remove duplicates in ALA data ####
## --------------------------------------------------------- ##
message(cat("Proportion of data duplicated in ALA BEFORE de-duplication: "),
        sum(duplicated(ala_data[,c("scientificName", 
                              "latitude", 
                              "longitude")]))/dim(ala_data)[1])

ala_data_dedup <- setDT(ala_data)[order(scientificName), .SD[1L] ,.(scientificName, latitude, longitude)]

message(cat("Proportion of data duplicated in ALA AFTER de-duplication: "),
        sum(duplicated(ala_data_dedup[,c("scientificName", 
                           "latitude", 
                           "longitude")])))
message(cat("Number of records lost in de-duplication: "),
        nrow(ala_data) - nrow(ala_data_dedup))




## --------------------------------------------------------- ##
## Combine ALA and non-ALA datasets ####
## --------------------------------------------------------- ##
nonala_sp <- unique(nonala_data$scientificName)
ala_sp <- unique(ala_data_dedup$scientificName)

dat <- rbind(ala_data_dedup, nonala_data)

## >> Check that total number of species is unique ALA species + unique non-ALA species not found in
length(unique(dat$scientificName)) == length(ala_sp) + length(nonala_sp[!(nonala_sp %in% ala_sp)])

message(cat("Number of unique scientificName in combined dataset: "),
        length(unique(dat$scientificName)))
message(cat("Number of unique spfile in combined dataset: "),
        length(unique(dat$spfile)))
message(cat("Number of records in combined dataset: "),
        nrow(dat))




## --------------------------------------------------------- ##
## Mask data ####
## --------------------------------------------------------- ##
## >> Load mask in WGS ####
mask_file <- file.path(output_dir, "masks", "ausmask_noaa_1kmWGS_NA.tif")
ausmask <- raster::raster(mask_file)
wgs_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## >> Remove points falling off extent (if any) ####
n <- dim(dat)[1]
dat <- dat[which(longitude >= ausmask@extent@xmin)]
dat <- dat[which(longitude <= ausmask@extent@xmax)]
dat <- dat[which(latitude >= ausmask@extent@ymin)]
dat <- dat[which(latitude <= ausmask@extent@ymax)]

message(cat("Proportion of records lost by clipping to mask extent: "),
        (n-dim(dat)[1])/n)
message(cat("Number of records lost by clipping to mask extent: "),
        n-dim(dat)[1])

## >> Clip data/occurrence points if they fall outside mask polygon(s) ####
sp <- SpatialPoints(dat[,c("longitude", "latitude")], 
                    proj4string = CRS(wgs_crs))
grd.pts <- extract(ausmask, sp)
dat0 <- dat[is.na(grd.pts),]
dat <- dat[!is.na(grd.pts),]

message(cat("Proportion of records lost due to falling on NAs within the mask: "),
        dim(dat0)[1]/dim(dat)[1])
message(cat("Number of records lost due to falling on NAs within the mask: "),
        dim(dat0)[1])

## >> Precise plotting
points1 <- sp::SpatialPoints(dat[,.(longitude, latitude)], proj4string = CRS(wgs_crs))
points0 <- sp::SpatialPoints(dat0[,.(longitude, latitude)], proj4string = CRS(wgs_crs))
plot(ausmask, axes = FALSE, legend = FALSE, box = FALSE)
plot(points1, add = TRUE, pch = 18, col = "tomato3", cex = 0.5)
plot(points0, add = TRUE, pch = 18, col = "green", cex = 0.5)





## --------------------------------------------------------- ##
## Find duplicates in combined data and prioritise non-ALA records for removal ####
## --------------------------------------------------------- ##
message(cat("Proportion of data duplicated in combined data before de-duplications: "),
        sum(duplicated(dat[,c("scientificName", 
                              "latitude", 
                              "longitude")]))/dim(dat)[1])
dat0 <- setDT(dat)[order(data_source), .SD[1L] ,.(scientificName, latitude, longitude)]
## How this works: 
## > setDT to set as data.table
## > order in increasing order by data_source so that alphabetically ALA comes last
##    this is same as setorder(dat, data_source)
## > .SD[1L] get the first observation for each group by .(scientificName, latitude, longitude)
## > .(scientificName, latitude, longitude) defines the groups
message(cat("Proportion of data duplicated in combined data after de-duplication: "),
        sum(duplicated(dat0[,c("scientificName", 
                               "latitude", 
                               "longitude")]))/dim(dat0)[1])

dat[, .N, by = data_source]
dat0[, .N, by = data_source]




## --------------------------------------------------------- ##
## Apply year filter ####
## --------------------------------------------------------- ##
plot(dat[!is.na(year)][, .N, year], xaxp = c(1630, 2020, 10), pch = 20)
range(dat$year, na.rm = TRUE)

message(cat("Proportion of records with year = NA: "),
        sum(is.na(dat$year))/nrow(dat))
message(cat("Proportion of records with year < 1990: "),
        nrow(dat[year < 1990])/nrow(dat))
message(cat("Proportion of records lost if applying year filer on records with year=NA or year < 1990: "),
        nrow(dat[is.na(year) | year < 1990])/nrow(dat))

## >> Number of records lost with year filter
t1 <- dat[, .N, scientificName]
t2 <- dat[!is.na(year) & year >= 1990][, .N, scientificName]
t <- merge(t1, t2, by = "scientificName", all.x = TRUE)
names(t)[2:3] <- c("n.all", "n.sub")
t[which(is.na(n.sub))]$n.sub = 0
sum(is.na(t$n.sub))
setorder(t, n.sub)

message(cat("Number of species with < 3 records to begin with: "),
        nrow(t[n.all < 3]))
message(cat("Number of species with < 3 records after filter for 1990 and NAs: "),
        nrow(t[n.sub < 3])); nrow(t[n.sub < 3])/nrow(t)
message(cat("Number of species with >= 3 records after filter for 1990 and NAs: "),
        nrow(t[n.sub >= 3])); nrow(t[n.sub >= 3])/nrow(t)
message(cat("Number of species with < 3 records after filter but which has >=3 records before filter: "),
        nrow(t[n.sub < 3 & n.all >=3]))

## >> Remove records based on filter rule: 
##  if >= 3 records after filter, remove NA and <1990
##  if < 3 records after filter, keep NA and <1990
sp_applyfilter <- t[n.sub >= 3]$scientificName
length(sp_applyfilter)

dim(dat)
dat0 <- dat[scientificName %in% sp_applyfilter][!is.na(year) & year >= 1990]
dat1 <- dat[!(scientificName %in% sp_applyfilter)]

## Checks
sum(dat0[, .N, scientificName]$N < 3)
dim(dat0)[1] + dim(dat1)[1]
length(unique(dat0$scientificName)) + 
  length(unique(dat1$scientificName)) == length(unique(dat$scientificName))

n <- dim(dat)[1]
dat <- rbind(dat0, dat1)
points(dat[!is.na(year)][, .N, year], xaxp = c(1630, 2020, 10), pch = 20, col = "tomato3")

message(cat("Number of records lost with year-filer: "),
        n-dim(dat)[1])
message(cat("Proportion of data lost with year-filer: "),
        (n-dim(dat)[1])/n)
message(cat("Number of unique species in data: "),
        length(unique(dat$scientificName)))




## --------------------------------------------------------- ##
## Complete taxonomic information where possible  ####
## --------------------------------------------------------- ##
## >> List records with incomplete class/family columns ####
message(cat("Number of records with NA class: "),
        nrow(dat[is.na(class)]))
message(cat("Number of records with NA family: "),
        nrow(dat[is.na(family)]))
message(cat("Species with NA class: "))
unique(dat[is.na(class)]$scientificName)
## No class information listed in ALA

message(cat("Number of records with blank string for family: "),
        nrow(dat[family == ""]))
message(cat("Number of records with blank string for class: "),
        nrow(dat[class == ""]))
message(cat("Number of records with blank string for class & family: "),
        nrow(dat[class == "" & family == ""]))

dat[class == "" & family == ""][, .N, data_source]
unique(dat[class == "" & family == ""]$scientificName)

## >> Delete records as per incomplete_taxinfo_JRM.csv ####
taxinfo <- fread(file.path(output_dir, "incomplete_taxinfo_JRM.csv"))
dropspecies <- taxinfo[exclude != ""]$species
taxinfo <- taxinfo[exclude == ""]

nrow(dat[class == "" & family == "" & scientificName %in% dropspecies])
nrow(dat[scientificName %in% dropspecies])
nrow(dat) - nrow(dat[!(scientificName %in% dropspecies)])

dat <- dat[!(scientificName %in% dropspecies)]

## >> Populate class/family columns as per incomplete_taxinfo_JRM.csv ####
for (sp in taxinfo$species){
  dat[class == "" & family == "" & scientificName %in% sp]$class = taxinfo[species == sp]$class
  dat[class == "" & family == "" & scientificName %in% sp]$family = taxinfo[species == sp]$family
}

## >> Add taxonominc info for anic ####
## Coenobiodes, Insecta, Tortricidae : provided by JM
dat[class == "" & family == ""][, .N, data_source]
dat[class == "" & family == ""]
dat[class == "" & family == ""]$class <- "Insecta"
dat[class == "" & family == ""]$family <- "Tortricidae"

message(cat("Number of records with blank string for class AND family: "),
        nrow(dat[class == "" & family == ""]))
dat[family == "" | class == ""][, .N, data_source]
unique(dat[family == "" & data_source != "ALA"]$scientificName)

## >> Populate class/family columns as per naclass_nonala_JRM.csv ####
taxinfo <- fread(file.path(output_dir, "naclass_nonala_JRM.csv"))
for (sp in taxinfo$species){
  dat[(class == "" | family == "") & scientificName %in% sp]$class = taxinfo[species == sp]$class
  dat[(class == "" | family == "") & scientificName %in% sp]$family = taxinfo[species == sp]$family
}

dat[family == "" | class == ""][, .N, data_source]

dat[(family == "" | class == "") & data_source == "anic_moths"]
dat[(family == "" | class == "") & data_source == "anic_moths"]$family <- "Tortricidae"

dat[(family == "" | class == "") & data_source == "SA_Churchett_Q26508-3"]
dat[(family == "" | class == "") & data_source == "SA_Churchett_Q26508-3"]$family <- "Formicidae"

message(cat("Number of records with blank string for family OR class: "),
        nrow(dat[family == "" | class == ""]))

dat[family == "" | class == ""][, .N, data_source]
unique(dat[family == "" | class == ""]$scientificName)

## >> Populate class/family columns as per naclassfamily_ala_JRM.csv ####
taxinfo <- fread(file.path(output_dir, "naclassfamily_ala_JRM.csv"))
dropspecies <- taxinfo[exclude != ""]$species
taxinfo <- taxinfo[exclude == ""]
dat <- dat[!(scientificName %in% dropspecies)]

for (sp in taxinfo$species){
  dat[(class == "" | family == "") & scientificName %in% sp]$class = taxinfo[species == sp]$class
  dat[(class == "" | family == "") & scientificName %in% sp]$family = taxinfo[species == sp]$family
}

message(cat("Number of records with blank string for family OR class: "),
        nrow(dat[family == "" | class == ""]))
message(cat("Number of species with blank string for family OR class: "),
        length(unique(dat[family == "" | class == ""]$scientificName)))
## leftover species do not have class or family values assigned yet

## >> Formatting class & family columns ####
head(dat)
dat$class <- tolower(dat$class)
dat$family <- tolower(dat$family)




## --------------------------------------------------------- ##
## Add sensitive species information as per JM's list ####
## --------------------------------------------------------- ##
sensitive_sp <- fread(file.path(bugs_dir, "aus_listed", "Aus_listed_spp.csv"))$Species

## Fix funny formatting issues in data sheet
grep('\\\\', sensitive_sp)
sensitive_sp[92] <- 'Charopidae "Skemps"'
sensitive_sp[118] <- "Enchymus sp.nov."
sensitive_sp[164] <- "Lissotes menalcas"
sensitive_sp[172] <- "Miselaoma weldii"
grep('\\\\', sensitive_sp)

sensitive_sp <- stringi::stri_enc_toutf8(sensitive_sp, validate = TRUE) 
sensitive_sp[1:3] <- gsub("_", "", sensitive_sp[1:3])
sensitive_sp <- trimws(sensitive_sp)

dat_sp <- unique(dat$scientificName)
length(sensitive_sp)
sum(sensitive_sp %in% dat_sp)
sum(!(sensitive_sp %in% dat_sp))

## Look again for species not found...
for (sp in sensitive_sp[!(sensitive_sp %in% dat_sp)]) {
  print(paste0("Lookign for speceis: ", sp, " ..."))
  if (length(grep(sp, dat$scientificName)) == 0) {
    print("NOT FOUND")
  } else{
    print(grep(sp, dat$scientificName))
  }
}
sensitive_sp1 <- sensitive_sp[sensitive_sp %in% dat_sp]

## See aus_listed_notindata_JRM.csv
sensitive_sp2 <- fread(file.path(bugs_dir, "aus_listed", "aus_listed_notindata_JRM.csv"))
dropspecies <- sensitive_sp2[exclude != ""]$species
dat[(scientificName %in% dropspecies)] ## none found
dat <- dat[!(scientificName %in% dropspecies)] 

sensitive_sp2 <- sensitive_sp2[exclude == ""]
sensitive_sp2[name_in_ala != "", species := name_in_ala]
sensitive_sp2 <- sensitive_sp2$species

sensitive_sp <- c(sensitive_sp1, sensitive_sp2)

for (sp in sensitive_sp){
  dat[scientificName == sp]$sensitive  <- 1
}

message(cat("Number of records marked as sensitive: "),
        nrow(dat[sensitive == 1])) 
message(cat("Number of species with sensitive records: "),
        length(unique(dat[sensitive == 1]$scientificName)))
  # note that some of these species also have records not markes as sensitive
x <- unique(dat[sensitive == 1]$scientificName)
y <- unique(dat[sensitive == 0]$scientificName)
sum(x%in%y)




## --------------------------------------------------------- ##
## Remove non natives ####
## --------------------------------------------------------- ##
## see data_corrections/exotics_alanonala.csv for list generated below
exotics <- fread(file.path(bugs_dir, "ALA", 
                           "GRIIS_Global_Register_of_Introduced_and_Invasive_Species_Australia.csv"))
n <- nrow(dat)

dat_sp <- unique(dat$scientificName)
sum(dat_sp %in% exotics$`Supplied Name`)
sum(dat_sp %in% exotics$scientificName)
exotic1 <- dat_sp[dat_sp %in% exotics$`Supplied Name`]
exotic2 <- dat_sp[dat_sp %in% exotics$scientificName]

exotics <- c(exotic1, exotic2)
exotics <- exotics[!duplicated(exotics)]

message(cat("Number of species in dataset identified non-native: "),
        length(unique((dat[(scientificName %in% exotics)]))))

message(cat("Proportion of records belonging to identified non-native species: "),
        (n - nrow(dat[!(scientificName %in% exotics)]))/n)
message(cat("Number of records belonging to identified non-native species: "),
        nrow(dat[!(scientificName %in% exotics)]))

dat <- dat[!(scientificName %in% exotics)]





## --------------------------------------------------------- ##
## Create new column to give unique ID by scientificName ####
## --------------------------------------------------------- ##
setDT(dat)[, new_id := .GRP, by = c("scientificName", "class", "family")]
length(unique(dat$new_id))
range(unique(dat$new_id))
length(unique(dat$scientificName))

## >> Merge spfile and new_ID (overwrite existing spfile column)
dat$spfile <- paste0(dat$spfile, "_", dat$new_id)
message(cat("Number of unique scientificName in new dataset: "),
        length(unique(dat$scientificName)))
message(cat("Number of unique spfile in new dataset: "),
        length(unique(dat$spfile)))
dat[,new_id := NULL]
write.csv(dat, file = file.path(output_dir, "ala_data_for_ALA_ARDC/NEWdata_ALAnonALA_wgs84.csv"), 
          row.names = FALSE)




## --------------------------------------------------------- ##
## Subset data to ALA data only & save ####
## --------------------------------------------------------- ##
datALA <- dat[data_source == "ALA"]
setorder(datALA, scientificName)
write.csv(datALA, file = file.path(output_dir, "ala_data_for_ALA_ARDC/NESPdata_ALA_wgs84.csv"), 
          row.names = FALSE)





## --------------------------------------------------------- ##
## Summary ####
## --------------------------------------------------------- ##
message(cat("Number of records in cleaned data: "),
        dim(datALA)[1])
message(cat("Number of unique scientificName in new dataset: "),
        length(unique(datALA$scientificName)))

message(cat("Number of records with blank string for family OR class: "),
        nrow(datALA[family == "" | class == ""]))
message(cat("Number of species with blank string for family OR class: "),
        length(unique(datALA[family == "" | class == ""]$scientificName)))

message(cat("Duplicated records: "),
        sum(duplicated(datALA[,c("scientificName", "latitude", "longitude")])))

message(cat("Records without year info: "),
        sum(is.na(datALA$year)))
