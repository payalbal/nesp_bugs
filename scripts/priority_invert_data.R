## Priority listed invertebrate species


## Set working environment
x <- c("readxl", "data.table", "ALA4R", "sp", "raster", "future", "future.apply")    
lapply(x, require, character.only = TRUE)
rm(x)


# ## Local paths
# data_path <- "/Volumes/6300-payalb/uom_data/nesp_bugs_data"
# output_path <- "/Volumes/6300-payalb/uom_data/nesp_bugs_data/outputs/"


## Server paths 
bugs_data <- "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_data, "outputs")
spdata_dir = file.path(output_dir, "ala_data" ,"spdata/spdata")
priority_dir <- file.path(output_dir, "ala_data", "priority_inverts")
if(!dir.exists(priority_dir)) dir.create(priority_dir)

source(file.path(getwd(), "nesp_bugs/scripts", "get_ala_spdata.R"))
source(file.path(getwd(), "nesp_bugs/scripts", "remove_improper_names.R"))


## Get priority species list (provided by Darren S)
priority_sp <- file.path(bugs_data, "all_priority_invertebrates_June2020_DS.xlsx")
priority_sp <- as.data.table(read_excel(priority_sp, sheet = "Combined"))
priority_sp <- priority_sp[Inclusions == 1,]$`Species name`


## Priority species in online ALA database
db_counts <- fread(file.path(output_dir, "typecounts.csv"))

message(cat("Number of priority species WITHOUT valid records in online ALA database:"),
        length(priority_sp[!priority_sp %in% db_counts$species]))
message(cat("Priority species WITHOUT valid records in online ALA database:",
            sort(priority_sp[!priority_sp %in% db_counts$species]), sep = "\n"))
x <- sort(priority_sp[!priority_sp %in% db_counts$species])
write.csv(x, file.path(output_dir, "prioritysp_NOTIN_ALAdb.csv"), row.names = FALSE)

db_counts <- db_counts[species %in% priority_sp, ]
db_counts <- db_counts[order(specimen)]
message(cat("Number of priority species WITH valid records in online ALA database:"),
        dim(db_counts)[1])
message(cat("Priority species WITHOUT valid records in online ALA database:",
            sort(db_counts$species), sep = "\n"))
write.csv(db_counts, file.path(output_dir, "prioritysp_IN_ALAdb.csv"), row.names = FALSE)


## Priority species in downloaded/cleaned ALA data
data_counts <- fread(file.path(output_dir, "datacounts_ALAspecies.csv"))

message(cat("Number of priority species NOT found in downloaded/cleaned ALA data:"),
        length(priority_sp[!priority_sp %in% data_counts$scientificName]))
message(cat("Priority species NOT found in downloaded/cleaned ALA data:",
            sort(priority_sp[!priority_sp %in% data_counts$scientificName]), sep = "\n"))
x <- sort(priority_sp[!priority_sp %in% data_counts$scientificName])
write.csv(x, file.path(output_dir, "prioritysp_NOTIN_ALAcleandata.csv"), row.names = FALSE)

data_counts <- data_counts[scientificName %in% priority_sp, ]
message(cat("Number of priority species found in downloaded/cleaned ALA data:"),
        dim(data_counts)[1])
message(cat("Priority species found in downloaded/cleaned ALA data:",
            sort(data_counts$scientificName), sep = "\n"))
write.csv(data_counts, file.path(output_dir, "prioritysp_IN_ALAcleandata.csv"), row.names = FALSE)





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

