## Integrating ALA and nonALA data


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "rje", "stringr", "lubridate", "sp", "raster")
lapply(x, require, character.only = TRUE)
rm(x)

## Server paths
bugs_data = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
nonala_data = file.path(bugs_data, "nonALA")
output_dir = file.path(bugs_data, "outputs")





## ALA cleaned data
spmasked_dir <- file.path(output_dir, "ala_data" ,"spdata_masked")
datfiles <- list.files(spmasked_dir, pattern = "_masked.rds$",
                       full.names = TRUE, all.files = TRUE)

temp <- datfiles[1:2]
## Format data table
dat1 <- do.call("rbind", lapply(temp, readRDS))


dat1 <- dat1[ , .(id, class, family, scientificName, latitude, longitude, year, spfile)]
names(dat1) <- c("ddlat", "ddlon", "tax", "family", "coly", "spfile")
unique(dat1$spfile)
dat1$data_source <- rep("ALA", nrow(dat1))
dat1 <- dat1[,c(1:5,7,6)]
