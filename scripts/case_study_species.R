

## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table")
lapply(x, require, character.only = TRUE)
rm(x)


## Files and folder
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")
casestudy_dir = file.path(output_dir, "case_study")
if (!dir.exists(casestudy_dir)) {dir.create(casestudy_dir)}


## Datasets
data_all <- fread(file.path(output_dir, "data_ALAnonALA_wgs84_corrected.csv"))
data_expert <- fread(file.path(output_dir, "sp_with_expert_data.csv"))


## Q1. Adequacy of ALA data
## Compare species distributions & fire overlap based on ALA only versus ALA+other (state/museum) data ####
rm(data)
data <- data_all

## >> Subset by number of records ####
counts <- data[, .N, spfile]
message(cat("Number of species with more than 10 records: "),
        nrow(counts[N > 10]))
message(cat("Number of species with more than 20 records: "),
        nrow(counts[N > 20]))
message(cat("Number of species with more than 50 records: "),
        nrow(counts[N > 50]))

data <- data[spfile %in% counts[N > 50]$spfile]
nrow(counts[N > 50]); length(unique(data$spfile))

## >> Subset by number of records by data source ####
unique(data$data_source)
sources <- fread(file.path(output_dir, "data_sources.csv"))[,1:2] ## file created manually based on unique data_source in data
unique(sources$type)

## Summarize data by species and data source 
out <- data[, .N, by=.(spfile, data_source)]
length(unique(out$spfile))

out$data_type <- rep(character(), nrow(out))
out[data_source %in% sources[type == "state"]$data_source]$data_type = "state"
out[data_source %in% sources[type == "museum"]$data_source]$data_type = "museum"
out[data_source %in% sources[type == "ala"]$data_source]$data_type = "ala"
out[data_source %in% sources[type == "private"]$data_source]$data_type = "private"

out <- out[, .(N=sum(N)),
           by=.(spfile, data_type)]
out <- dcast(out, spfile ~ data_type, value.var = "N")
setDT(out, key = "spfile")
out[is.na(out)] <- 0

## Add tax info
tax <- setDT(data, key = "spfile")[, .SD[1L] ,.(scientificName, class, order, family, spfile)]
tax <- tax[,.(scientificName, class, order, family, spfile)]
all(out$spfile %in% tax$spfile)

out <- merge(out, tax, by = "spfile")
out <- out[, c(1, 5:8, 2:4)]
setDT(out, key = "spfile")

## Subset to species with data accross all sources
out <- out[ala != 0 & (museum != 0 | state != 0)]
out[museum == 0 & state == 0]

data <- data[spfile %in% out$spfile]
length(out$spfile); length(unique(data$spfile))

## >> Differences in species diastributions (area) ####
## Create polygons
with ala + other...[existing polygons]
with ala only...




## Histogram differences in EOO
alldat <- fread(file.path(output_dir, "species_EOO_AOO_ahullareas.csv"))
alldat <- alldat[spfile %in% data%spfile]....

alaonly <- fread(file.path(casestudy_dir, "species_EOO_AOO_ahullareas_ALAonly.csv"))




## Differences in fire overlaps (area) ####
fireoverlaps with two types of polygons...

fire_sp <- fread(file.path(output_dir, "invert_overlap_PAAonly_2021-06-11.csv"))




## Histogram differences fire overlap > 0%
fire_sp0 <- fire_sp[Overlap_Polygons_Fire2345_GEEBAM2_as_burnt > 0]$spfile
data1 <- data[spfile %in% fire_sp0]; dim(data1)
length(fire_sp0); length(unique(data1$spfile))
unique(data1$spfile)

## Histogram differences fire overlap > 30%
fire_sp30 <- fire_sp[Overlap_Polygons_Fire2345_GEEBAM2_as_burnt > 30]$spfile
data2 <- data[spfile %in% fire_sp30]; dim(data1)
length(fire_sp30); length(unique(data2$spfile))
unique(data2$spfile)



## Q2. Reliability of observatonal data in ALA?
## Compare outputs from observational versus specimen in ALA
rm(data)
data <- data_all


## Q1. Change in species distributions (ALA and other)
## >> By number of records ####
counts <- data[, .N, spfile]
message(cat("Number of species with more than 10 records: "),
        nrow(counts[N > 10]))
message(cat("Number of species with more than 20 records: "),
        nrow(counts[N > 20]))
message(cat("Number of species with more than 50 records: "),
        nrow(counts[N > 50]))

data <- data[spfile %in% counts[N > 50]$spfile]
nrow(counts[N > 50]); length(unique(data$spfile))



## Summary stats

## Can identify

## Cannot identify




## >> By fire impact ####
# fire_sp <- fread(file.path(bugs_dir, "JM_traits", "FireInvert_scored_expertANDimpacted14.06.csv"))
# fire_sp <- fire_sp[,c(4:7)]
# names(fire_sp)[1] <- "spfile"
# fire_sp <- fire_sp[mean_severe_use_pt_if_pt12 > 30]$spfile

fire_sp <- fread(file.path(output_dir, "invert_overlap_PAAonly_2021-06-11.csv"))
fire_sp <- fire_sp[Overlap_Polygons_Fire2345_GEEBAM2_as_burnt > 30]$spfile

data <- data[spfile %in% fire_sp]; dim(data)
length(fire_sp); length(unique(data$spfile))

## >> By taxonomic group: order = Lepidoptera - not applied yet
nrow(data[order == "lepidoptera"])
length(grep("lepidop", data$order))

data[grep("kosciuscola_tristis", data$spfile)]

butterflies <- tolower(c("Lycaenidae", "Papilionidae", "Nymphalidae", "Pieridae", "Hesperiidae"))



## Species selection from expert dataset ####
##  >>>>> Not incorporated into data_ALAnonALA_wgs84_corrected.csv <<<<<
rm(data)
data <- data_expert

dim(data)
unique(data$spfile)
unique(data$scientificName)
unique(data$data_source)

## Add to dataset
data[grep("kosciuscola_tristis", data$spfile)]
k.tristis_data <- fread(file.path(bugs_dir, "data_corrections", "kosciuscola_tristis_expert_data.csv"))
  ## Need sub-species identified

## >> By fire impact - NOT RUN (leaves only 1 species) ####
fire_sp <- fread(file.path(output_dir, "invert_overlap_PAAonly_2021-06-11.csv"))
fire_sp <- fire_sp[Overlap_Polygons_Fire2345_GEEBAM2_as_burnt > 30]$spfile

data <- data[spfile %in% fire_sp]; dim(data)
length(fire_sp); length(unique(data$spfile))

## >> By number of records ####
counts <- data[, .N, spfile]
message(cat("Number of species with more than 5 records: "),
        nrow(counts[N > 5]))
message(cat("Number of species with more than 10 records: "),
        nrow(counts[N > 10]))
message(cat("Number of species with more than 20 records: "),
        nrow(counts[N > 20]))
message(cat("Number of species with more than 50 records: "),
        nrow(counts[N > 50]))

data <- data[spfile %in% counts[N > 5]$spfile]
nrow(counts[N > 5]); length(unique(data$spfile))

## >> By number of records by data source ####
unique(data$data_source)
sources <- fread(file.path(output_dir, "data_sources.csv"))[,1:2] ## file created manually based on unique data_source in data
unique(sources$type)

## Summarize data by species and data source 
out <- data[, .N, by=.(spfile, data_source)]
length(unique(out$spfile))

out$data_type <- rep(character(), nrow(out))
out[data_source %in% sources[type == "state"]$data_source]$data_type = "state"
out[data_source %in% sources[type == "museum"]$data_source]$data_type = "museum"
out[data_source %in% sources[type == "ala"]$data_source]$data_type = "ala"
out[data_source %in% sources[type == "private"]$data_source]$data_type = "private"

out <- out[, .(N=sum(N)),
           by=.(spfile, data_type)]
out <- dcast(out, spfile ~ data_type, value.var = "N")
setDT(out, key = "spfile")
out[is.na(out)] <- 0

## Add tax info
tax <- setDT(data, key = "spfile")[, .SD[1L] ,.(scientificName, class, order, family, spfile)]
tax <- tax[,.(scientificName, class, order, family, spfile)]
all(out$spfile %in% tax$spfile)

out <- merge(out, tax, by = "spfile")
out <- out[, c(1, 4:7, 2:3)]
setDT(out, key = "spfile")

## Subset to species with data accross all sources
out
out[ala != 0 & private != 0]

## Check if these species are fire impacted
message(cat("Species with expert data impacted by fire: "),
        out[ala != 0 & private != 0][out[ala != 0 & private != 0]$spfile %in% fire_sp]$spfile)


# ## Function to replace NA in large data tables
# ## Ref: https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
# f_dowle2 = function(DT) {
#   for (i in names(DT))
#     DT[is.na(get(i)), (i):=0]
# }
