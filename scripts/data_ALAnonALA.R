## Integrating ALA and nonALA data for species found in non ALA data


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "rje", "stringr", "sp", "raster", "lubridate", "future", "future.apply")
lapply(x, require, character.only = TRUE)
rm(x)


## Server paths
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
nonala_dir = file.path(bugs_dir, "nonALA")
output_dir = file.path(bugs_dir, "outputs")


## Load datasets ####
## >> Non ALA cleaned data: clean1_nonala_
nonala_data <- fread(list.files(output_dir,
                                pattern = "clean1_nonala*.*csv$",
                                full.names = TRUE))
setDT(nonala_data)
dim(nonala_data)
length(unique(nonala_data$scientificName))
length(unique(nonala_data$spfile))

sum(is.na(nonala_data$year))
sum(nonala_data$year == "", na.rm = TRUE)
  ## There are duplicates in spfile for distinct scientificName.
  ## This problem arises due to simplification of species name for the
  ## improper names found in state/museum data.
  ## To be fixed further down...

## >> ALA data cleaned data: clean2_ala_
ala_data <- fread(list.files(output_dir,
                      pattern = "clean2_ala*.*csv$",
                      full.names = TRUE))
setDT(ala_data)
dim(ala_data)
length(unique(ala_data$scientificName))
length(unique(ala_data$spfile))


## Extract year for ALA data ####
names(ala_data)[grep("Date", names(ala_data))]

sum(is.na(ala_data$eventDate))
sum(is.na(ala_data$year))
sum(ala_data$year == "", na.rm = TRUE)
nrow(ala_data[is.na(eventDate) & is.na(year)])
range(ala_data$year, na.rm = TRUE)

## Find records for which year can be extracted
sum(is.na(ala_data$year) & !is.na(ala_data$eventDate))
id <- which(is.na(ala_data$year) & !is.na(ala_data$eventDate))

## Specify year based in eventDate for records found
ala_data$eventDate[id]
range(year(ymd(ala_data$eventDate[id])))
ala_data$year[id]
ala_data$year[id] <- year(ymd(ala_data$eventDate[id]))
ala_data$year[id]
sum(is.na(ala_data$year))

message(cat("Proportion of ALA records without evenDate info: "),
        nrow(ala_data[is.na(eventDate)])/nrow(ala_data))
message(cat("Proportion of ALA records without year info: "),
        nrow(ala_data[is.na(year)])/nrow(ala_data))

# message(cat("Number of records with collection Date listed as invalid: "),
#         sum(ala_data$invalidCollectionDate))
# sum(is.na(ala_data$verbatimEventDate)) ## "" instead of NAs

## Format ALA data table ####
names(ala_data)
ala_data <- ala_data[ , .(id, class, family, scientificName, latitude, longitude, year, spfile)]
ala_data$data_source <- rep("ALA", nrow(ala_data))
ala_data$sensitive <- rep(0, nrow(ala_data))
ala_data$habitat <- rep("NA", nrow(ala_data))
ala_data <- ala_data[,c(9,1:7,10,11,8)]
names(ala_data) <- names(nonala_data)


## Find species in non ALA data also found in ALA ####
nonala_sp <- unique(nonala_data$scientificName)
message(cat("Number of species in non ALA data: "),
        length(nonala_sp))

ala_sp <- unique(ala_data$scientificName)
message(cat("Number of species in ALA data (some repetitions here*): "),
        length(ala_sp))
## * see spfile section

message(cat("Number of non ALA species found in ALA data: "),
        length(nonala_sp[nonala_sp %in% ala_sp]))

message(cat("Number of non ALA species NOT found in ALA data: "),
        length(nonala_sp[!(nonala_sp %in% ala_sp)]))

message(cat("Number of ALA species NOT found in non ALA data: "),
        length(ala_sp[!(ala_sp %in% nonala_sp)]))


  # ## Split ALA data  -- NOT NEEDED ####
  # ## >> ALA data for non ALA species
  # ala_sub1 <- ala_data[spfile %in% nonala_sp[nonala_sp %in% ala_sp]]
  # ala_sub1 <- setDT(ala_sub1)
  # dim(ala_sub1)
  # length(unique(ala_sub1$spfile))
  # length(unique(ala_sub1$scientificName))
  # 
  # ## >> ALA data remaining
  # ala_sub2 <- ala_data[spfile %in% ala_sp[!(ala_sp %in% nonala_sp)]]
  # ala_sub2 <- setDT(ala_sub2)
  # dim(ala_sub2)
  # length(unique(ala_sub2$spfile))
  # length(unique(ala_sub2$scientificName))


## Combine ALA and non ALA for species found in both datasets ####
dat <- rbind(ala_data, nonala_data)
message(cat("Number of unique scientificName in new dataset: "),
        length(unique(dat$scientificName)))
message(cat("Number of unique spfile in new dataset: "),
        length(unique(dat$spfile)))
  ## To be fixed further down...

## Mask data ####
## >> Load mask
mask_file <- file.path(output_dir, "masks", "ausmask_noaa_1kmWGS_NA.tif")
ausmask <- raster::raster(mask_file)
wgs_crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## >> Check for out-of-extent lat/longs
dim(dat[which(longitude < ausmask@extent@xmin)])
dim(dat[which(longitude > ausmask@extent@xmax)])
dim(dat[which(latitude < ausmask@extent@ymin)])
dim(dat[which(latitude > ausmask@extent@ymax)])

## >> Remove points falling off extent (if any)
n <- dim(dat)[1]
dat <- dat[which(longitude >= ausmask@extent@xmin)]
dat <- dat[which(longitude <= ausmask@extent@xmax)]
dat <- dat[which(latitude >= ausmask@extent@ymin)]
dat <- dat[which(latitude <= ausmask@extent@ymax)]

message(cat("Proportion of records lost by clipping to mask extent: "),
        (n-dim(dat)[1])/n)

## >> Clip data/occurrence points if they fall outside mask polygon(s)
sp <- SpatialPoints(dat[,c("longitude", "latitude")], 
                    proj4string = CRS(wgs_crs))
grd.pts <-extract(ausmask, sp)

## Subset data - HERE THERE MIGHT BE PROBLEM WITH ALIGNMENT OF DATA AND MASK
dat0 <- dat[is.na(grd.pts),]
dat <- dat[!is.na(grd.pts),]

message(cat("Proportion of records lost due to falling on NAs within the mask: "),
        dim(dat0)[1]/dim(dat)[1])

## >> Precise plotting
points1 <- sp::SpatialPoints(dat[,.(longitude, latitude)], proj4string = CRS(wgs_crs))
points0 <- sp::SpatialPoints(dat0[,.(longitude, latitude)], proj4string = CRS(wgs_crs))
plot(ausmask, axes = FALSE, legend = FALSE, box = FALSE)
plot(points1, add = TRUE, pch = 18, col = "tomato3", cex = 0.5)
plot(points0, add = TRUE, pch = 18, col = "green", cex = 0.5)


## Find duplicates and prioritise ALA records for removal ####
message(cat("Proportion of data duplicated: "),
        sum(duplicated(dat[,c("scientificName", 
                               "latitude", 
                               "longitude")]))/dim(dat)[1])
dat[, .N, by = sort(data_source)]
dat <- setDT(dat)[order(-data_source), .SD[1L] ,.(scientificName, latitude, longitude)]
dat[, .N, by = sort(data_source)]
  ## How this works: 
  ## > setDT - set as data.table
  ## > order in decreasing order by data_source (i.e. so that alphabetically ALA comes last)
  ##    this is same as setorder(dat, -data_source)
  ## > .SD[1L] get the first observation for each group by .(scientificName, latitude, longitude)
  ## > .(scientificName, latitude, longitude) defines the groups
  ## Ref: https://cran.r-project.org/web/packages/data.table/vignettes/datatable-sd-usage.html
  ## Ref: https://stackoverflow.com/questions/8508482/what-does-sd-stand-for-in-data-table-in-r

message(cat("Number of records left in cleaned data: "),
        dim(dat)[1])
message(cat("Number of unique scientificName in new dataset: "),
        length(unique(dat$scientificName)))
message(cat("Number of unique spfile in new dataset: "),
        length(unique(dat$spfile)))
  ## To be fixed further down...


## Apply year filter ####
## >> Find records without year information
plot(dat[!is.na(year)][, .N, year], xaxp = c(1630, 2020, 10), pch = 20)

range(dat$year, na.rm = TRUE)

message(cat("Proportion of records with year = NA: "),
        sum(is.na(dat$year))/nrow(dat))
sum(is.na(dat$year))
sum(dat$year == "")

message(cat("Proportion of records with year < 1990: "),
        nrow(dat[year < 1990])/nrow(dat))

message(cat("Proportion of records lost if applying year filer on records with year=NA or year < 1990: "),
        nrow(dat[is.na(year) | year < 1990])/nrow(dat))

## >> Number of records lost with year filter
t1 <- dat[, .N, scientificName]
t2 <- dat[!is.na(year) & year >= 1990][, .N, scientificName]
t <- merge(t1, t2, by = "scientificName", all.x = TRUE)
dim(t)
names(t)[2:3] <- c("n.all", "n.sub")
t[which(is.na(n.sub))]$n.sub = 0
sum(is.na(t$n.sub))
setorder(t, n.sub)
# t$p.lost <- (t$n.all - t$n.sub)/t$n.all

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


## Final formatting
head(dat)
dat$class <- tolower(dat$class)
dat$family <- tolower(dat$family)

## Create new column to give unique ID by scientificName ####
setDT(dat)[, new_id := .GRP, by = c("scientificName", "class", "family")]
length(unique(dat$new_id))
range(unique(dat$new_id))

## Merge spfile and new_ID (overwrite existing apfile column)
dat$spfile <- paste0(dat$spfile, "_", dat$new_id)
message(cat("Number of unique scientificName in new dataset: "),
        length(unique(dat$scientificName)))
message(cat("Number of unique spfile in new dataset: "),
        length(unique(dat$spfile)))
dat[,new_id := NULL]



## Save combined data table ####
## This dataset contains all nonALA data and a subset of the ALA data 
## i.e., subset of species in ALA found in nonALA data.
setorder(dat, scientificName)
write.csv(dat, file = file.path(output_dir, "data_ALAnonALA.csv"), row.names = FALSE)


## Save rds files by species ####
spdata_dir <- file.path(output_dir, "ala_nonala_data" ,"spdata")
if(!dir.exists(spdata_dir)){dir.create(spdata_dir)}

## Save by species (reduced) function
save_spdata2 <- function(species_uid, data, data_dir){
  
  temp <- dat[spfile == species_uid]
  spfile <- unique(temp$spfile)
  
  if (length(spfile) > 1){
    stop("Error: More than 1 unique spfile for naming species file...")
  }
  
  saveRDS(as.data.table(temp),
          file = file.path(data_dir, paste0(spfile, ".rds")))
}

## In parallel ####
plan(multiprocess, workers = future::availableCores()-2)
options(future.globals.maxSize = +Inf)

## Error log file
errorlog <- paste0(output_dir, "/errorlog_data_ALAnonALA_", gsub("-", "", Sys.Date()), ".txt")
# errorlog <- paste0("/tempdata/workdir/nesp_bugs/temp/errorlog_ala_byspeciesR_", gsub("-", "", Sys.Date()), ".txt") 
writeLines(c(""), errorlog)

dat <- fread(file.path(output_dir, "data_ALAnonALA.csv"))
all_species <- unique(dat$spfile)

start.time <- Sys.time()
invisible(future.apply::future_lapply(all_species,
                                      function(x){
                                        tmp <- tryCatch(expr = save_spdata2(species_uid = x, 
                                                                            data = dat, 
                                                                            data_dir = spdata_dir),
                                                        error = function(e){ 
                                                          print(paste("\nError: More than 1 unique spfile for naming species file for...", x))
                                                          cat(paste(x, "\n"),
                                                              file = errorlog, 
                                                              append = TRUE)
                                                        })
                                      }))
end.time <- Sys.time()
end.time - start.time


## Check files - should be 58584
length(list.files(spdata_dir, pattern = ".rds$"))
length(all_species)







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
