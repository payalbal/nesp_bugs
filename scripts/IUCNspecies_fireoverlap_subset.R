library(data.table)

iucn_species <- fread("/Volumes/6300-payalb/uom_data/nesp_bugs_data/IUCN_species/IUCN_species_list.csv")

iucn_species$ScientificName <- paste(iucn_species$Genus, iucn_species$Species)

ala_polys <- fread("/Volumes/6300-payalb/uom_data/nesp_bugs_data/outputs/ala_polygons_areas.csv")


rbind(ala_polys, c("")
iucn_species$ScientificName[iucn_species$ScientificName %in% ala_polys$X]
iucn_species$ScientificName[!(iucn_species$ScientificName %in% ala_polys$X)]

