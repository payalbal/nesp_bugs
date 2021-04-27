## Priority listed invertebrate species


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")
x <- c("readxl", "data.table", "ALA4R", "stringr")    
lapply(x, require, character.only = TRUE)
rm(x)

bugs_dir <- "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")
polygons_dir = file.path(output_dir, "species_polygons")

priority_dir <- file.path(output_dir, "priority_inverts")
if(!dir.exists(priority_dir)) dir.create(priority_dir)

priority_polys = file.path(priority_dir, "species_polygons") 
if(!dir.exists(priority_polys)) dir.create(priority_polys)

source(file.path(getwd(), "nesp_bugs/scripts", "get_ala_spdata.R"))
source(file.path(getwd(), "nesp_bugs/scripts", "remove_improper_names.R"))


## Get priority species list (provided by Darren S) ####
priority_sp <- file.path(bugs_dir, "all_priority_invertebrates_June2020_DS.xlsx")
priority_sp <- as.data.table(read_excel(priority_sp, sheet = "Combined"))
priority_sp <- priority_sp[Inclusions == 1,]$`Species name`

message(cat("Number of priority species:"), length(priority_sp))


## Priority species in downloaded/cleaned ALA + nonALA data ####
data <- fread(file.path(output_dir, "data_ALAnonALA_wgs84.csv"))
alldat_counts <- data[, .N, by = spfile]

priority_spfile <- stringr::str_replace_all(priority_sp, " ", "00xx00")
priority_spfile <- stringr::str_replace_all(priority_spfile, "[^[:alnum:]]", "")
priority_spfile <- tolower(gsub("00xx00", "_", priority_spfile))

y <- alldat_counts$spfile
y <- gsub("_\\d+$", "", y) ## Remove number at the end of string
##  gsub("^\\d+|\\d+$", "", words)    ## number at beginning at end of string

message(cat("Number of priority species NOT found in downloaded/cleaned ALA+nonALA data:"),
        length(priority_spfile[!priority_spfile %in% y]))
message(cat("Number of priority species found in downloaded/cleaned ALA+nonALA data:"),
        length(priority_spfile[priority_spfile %in% y]))

message(cat("Of the species found, number of species with >= 20 records:"),
        sum(alldat_counts[y %in% priority_spfile]$N >= 20))
message(cat("Of the species found, number of species with < 20 records:"),
        sum(alldat_counts[y %in% priority_spfile]$N < 20))


## Extract data for species found
data$scientificnameString <- gsub("_\\d+$", "", data$spfile)
setDT(data, key = "scientificnameString")

priority_sp_indata <- priority_spfile[priority_spfile %in% y]

dat1 <- data[scientificnameString %in% priority_sp_indata]
length(unique(dat1$scientificName))
length(unique(dat1$spfile))
length(unique(dat1$scientificnameString))

dim(dat1)
dat1[, scientificnameString := NULL]
write.csv(dat1, file = file.path(priority_dir, "priority_inverts_data.csv"), 
          row.names = FALSE)

## Table of number of records for species found
dat2 <- dat1[, .N, scientificName]
write.csv(dat2, file = file.path(priority_dir, "priority_inverts_datacounts.csv"), 
          row.names = FALSE)
dat2[N < 20]
dat2[N >= 20]

## Polygons for species with < 20 records
dat3 <- dat1[, .N, spfile]
dat3[N < 20]
dat3[N >= 20]

polys <- dat3[N < 20]$spfile
x <- gsub("_\\d+$", "", polys)
sum(duplicated(x))

polyfiles <- list.files(polygons_dir, pattern = ".rds$",
                        full.names = TRUE, all.files = TRUE)
x <- basename(tools::file_path_sans_ext(polyfiles))
polyfiles <- polyfiles[x %in% polys]

# file.remove(file.path(priority_polys, dir(path = priority_polys)))
file.copy(polyfiles, priority_polys,
          overwrite = FALSE, recursive = TRUE,
          copy.mode = TRUE, copy.date = TRUE)










## EXTRAS ####
## Find improper names in priority list
priority_names <- remove_improper_names(priority_sp)
priority_names0 <- c(priority_names$improper_species, priority_names$incomplete_species)
message(cat("Improper names found in list:",
            priority_names0, sep = "\n"))


## Text formating for species nameas
priority_names1 <- as.character(na.omit(priority_names$updated_list))
priority_names1 <- stringr::str_replace_all(priority_names1, " ", "00xx00")
priority_names1 <- stringr::str_replace_all(priority_names1, "[^[:alnum:]]", "")
priority_names1 <- tolower(gsub("00xx00", "_", priority_names1))
priority_names1 <- sort(unique(priority_names1))


## Compare priority list with cleaned ALA species files
spfiles <- list.files(spdata_dir, recursive = FALSE, full.names = TRUE)
spfiles <- spfiles[!file.info(spfiles)$isdir]
length(spfiles)
spfiles <- tools::file_path_sans_ext(basename(spfiles))
spfiles <- sort(spfiles)

message(cat("Number of priority species found in cleaned ALA data:"),
        length(priority_names1[priority_names1 %in% spfiles]))
message(cat("Priority species found in cleaned ALA data:",
            priority_names1[priority_names1 %in% spfiles], sep = "\n"))

message(cat("Number of priority species NOT found in cleaned ALA data:"),
        length(priority_names1[!priority_names1 %in% spfiles]))
message(cat("Priority species NOT found in cleaned ALA data:",
            priority_names1[!priority_names1 %in% spfiles], sep = "\n"))


# ## Download data from online ALA database ####
# ## Load clean AFD taxonomic checklist
# afd_taxonomy <- fread(file.path(output_dir, "afd_species_clean.csv"))
# afd_species <- unique(afd_taxonomy$VALID_NAME)
# length(afd_species)
# 
# ## Create log txt file
# nodatalog <- file.path(priority_dir, "nodata_log.txt")
# writeLines(c("species0"), nodatalog)
# 
# ## Download data for priority species
# plan(multiprocess, workers = future::availableCores()-2)
# options(future.globals.maxSize = +Inf) ## CAUTION: Set this to a value, e.g. availablecores-1?/RAM-10?
# 
# start.time <- Sys.time()
# invisible(future.apply::future_lapply(afd_species,
#                                       function(x){
#                                         tmp <- tryCatch(expr = get_ala_spdata(x,
#                                                                               extra_fields = TRUE,
#                                                                               specimens_only = TRUE,
#                                                                               remove_duplicates = TRUE,
#                                                                               dst = ala_dir,
#                                                                               save.map = TRUE,
#                                                                               reg.mask.file = file.path(output_dir, "ausmask_WGS.tif"),
#                                                                               email = paste0("bal.payal+", sample(1:100, 1), "@gmail.com")),
#                                                         error = function(e){ 
#                                                           print(paste("\nNot run: no records for", x))
#                                                           
#                                                           cat(paste(x, "\n"),
#                                                               file = nodatalog, 
#                                                               append = T)
#                                                         })
#                                       }))
# end.time <- Sys.time()
# end.time - start.time
# 
# ## Count files
# ## Species with data
# sp_data <- list.files(file.path(ala_dir, "maps"), include.dirs = FALSE)
# # list.files(ala_dir, include.dirs = FALSE) ## lists "maps" folder as well
# sp_data <- gsub(".pdf", "", sp_data)
# length(sp_data)
# 
# ## Species without data
# sp_nodata <- read.csv(nodatalog)
# sp_nodata <- sp_nodata$species0
# length(sp_nodata)




## EXTRAS ####
## Priority species in online ALA database
alaDB_counts <- fread(file.path(output_dir, "typecounts.csv"))

message(cat("Number of priority species WITHOUT valid records in online ALA database:"),
        length(priority_sp[!priority_sp %in% alaDB_counts$species]))
message(cat("Priority species WITHOUT valid records in online ALA database:",
            sort(priority_sp[!priority_sp %in% alaDB_counts$species]), sep = "\n"))
# x <- sort(priority_sp[!priority_sp %in% alaDB_counts$species])
# write.csv(x, file.path(output_dir, "prioritysp_NOTIN_ALAdb.csv"), row.names = FALSE)

alaDB_counts <- alaDB_counts[species %in% priority_sp, ]
alaDB_counts <- alaDB_counts[order(specimen)]
message(cat("Number of priority species WITH valid records in online ALA database:"),
        dim(alaDB_counts)[1])
message(cat("Priority species WITHOUT valid records in online ALA database:",
            sort(alaDB_counts$species), sep = "\n"))
# write.csv(alaDB_counts, file.path(output_dir, "prioritysp_IN_ALAdb.csv"), row.names = FALSE)


## Priority species in downloaded/cleaned ALA data
alaDAT_counts <- fread(file.path(output_dir, "datacounts_ALAspecies.csv"))

message(cat("Number of priority species NOT found in downloaded/cleaned ALA data:"),
        length(priority_sp[!priority_sp %in% alaDAT_counts$scientificName]))
message(cat("Priority species NOT found in downloaded/cleaned ALA data:",
            sort(priority_sp[!priority_sp %in% alaDAT_counts$scientificName]), sep = "\n"))
# x <- sort(priority_sp[!priority_sp %in% alaDAT_counts$scientificName])
# write.csv(x, file.path(output_dir, "prioritysp_NOTIN_ALAcleandata.csv"), row.names = FALSE)

alaDAT_counts <- alaDAT_counts[scientificName %in% priority_sp, ]
message(cat("Number of priority species found in downloaded/cleaned ALA data:"),
        dim(alaDAT_counts)[1])
message(cat("Priority species found in downloaded/cleaned ALA data:",
            sort(alaDAT_counts$scientificName), sep = "\n"))
# write.csv(alaDAT_counts, file.path(output_dir, "prioritysp_IN_ALAcleandata.csv"), row.names = FALSE)
