## Merging species list downloaded from ALA

data_path <- "/Volumes/uom_data/nesp_bugs_data"
ala_path <- paste0(data_path, "/ALA/species_lists") # downloaded as per selected phyla from ALA
ala_files <- list.files(ala_path, full.names = TRUE)

## Merge files
ala_species <- do.call("rbind",lapply(ala_files, FUN = function(files){ read.csv(files)}))
write_csv(ala_species, "./output/ala_splist_full.csv")

## We needn't remove improper names and dupplicates here.
## We do it for AFD and then subset this list based on species %>% AFD list

