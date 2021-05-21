## Correcting output table for fire overlap results

## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")
x <- c("data.table")
lapply(x, require, character.only = TRUE)
rm(x)

## File paths and folders
bugs_dir = "~/gsdms_r_vol/tempdata/research-cifs/uom_data/nesp_bugs_data"
corr_dir <- file.path(bugs_dir, "data_corrections")
output_dir = file.path(bugs_dir, "outputs")
new_output_dir = file.path(bugs_dir, "outputs", "outputs_for_updated_species_only")

# x <- ""
# list.files(x)
# file.remove(file.path(x, dir(path = x)))
# unlink(x, recursive = TRUE)

## Remove species from old output table ####
dat1 <- fread(file.path(output_dir, "invert_fireoverlap.csv")); dim(dat1)
dat2 <- fread(file.path(corr_dir, "invert_fireoverlap_corrected.csv")); dim(dat2)
all(sort(dat1$spfile) == sort(dat2$spfile))
rm(dat1)

x <- fread(file.path(corr_dir, "delete_species_fromoutputs.csv"))$x; length(x)
nrow(dat2) - nrow(dat2[!spfile %in% x]) == length(x)
dat2 <- dat2[!spfile %in% x]
dat2[, c("marine", "invasive", "name_issues", 
         "delete_sp", "spell_variants", "name_corrections") := NULL]
names(dat2); dim(dat2)

## Add updated species to output table
dat1 <- fread(file.path(new_output_dir, "invert_fireoverlap_updatedsp.csv")); dim(dat1)
ncol(dat1) == ncol(dat2)
dat <- rbind(dat1, dat2); dim(dat)
rm(dat1, dat2)

## Save table ####
names(dat); dim(dat)
length(unique(dat$spfile))
length(unique(dat$scientificName))

setDT(dat, key = "spfile", "scientificName")
write.csv(dat, file = file.path(output_dir, "invert_fireoverlap_v02.csv"), 
          row.names = FALSE)
summary(dat)
