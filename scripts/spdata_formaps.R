## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "sp", "raster", "fields")
lapply(x, require, character.only = TRUE)
rm(x)

## Files and folder
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")

## ALAnonALA data
data <- fread(file.path(output_dir, "data_ALAnonALA_wgs84_corrected.csv"))
data_sp <- unique(data$spfile)


## Trait data ####
trait <- as.data.table(fread(file.path(bugs_dir, "JM_traits", "FireInvert_scored_expertANDimpacted14.06.csv")))
trait_sp <- unique(trait$SpFile)
length(trait_sp)

## Trait data species found in ALAnonALA data
all(trait_sp %in% data_sp)
trait_sp[which(!(trait_sp %in% data_sp))]

## >> Two species not found were renamed as per problem_species_jw.csv: 
## "procambridgea_montana_12273" > "procambridgea_montana_2461"
## "desognaphosa_yabbra_15963" > "desognaphosa_yabbra_1043"
trait_sp[grep("procambridgea_montana_12273", trait_sp)] <- "procambridgea_montana_2461"
trait_sp[grep("desnognaphosa_yabbra_15963", trait_sp)] <- "desognaphosa_yabbra_1043"

## >> NA spfiles, not found
trait_sp[which(!(trait_sp %in% data_sp))]
trait[grep("#N/A", trait$spfile)]$scientificName %in% unique(data$scientificName)
  ## Corresponding scientificName not found in our data either

## >> Remove duplicates
trait_sp[duplicated(trait_sp)]
trait_sp <- trait_sp[!duplicated(trait_sp)]

## Subset ALAnonALA data for trait data species
trait_data <- data[spfile %in% trait_sp]

## Check
length(trait_sp)
length(unique(trait_data$spfile))

trait_sp[which(!trait_sp %in% trait_data$spfile)]

## Save data for mapping (removing any sensitive records)
names(trait_data)
trait_data[, .N, sensitive]

trait_data <- trait_data[sensitive == 0]
dim(trait_data)
length(unique(trait_data$spfile))
length(unique(trait_data$scientificName))
  ## Only 978 species from 1052 species

write.csv(trait_data, file = file.path(output_dir, "trait_data.csv"),
          row.names = FALSE)

## EPBC species ####
x <- fread(file.path(bugs_dir, "maps_figures", "EPBCA_species.csv"))$spfile
length(x)
sum(duplicated(x))

all(x %in% data_sp)

xdat <- data[spfile %in% x]; dim(xdat)
length(unique(xdat$spfile))
length(unique(xdat$scientificName))

write.csv(xdat, file = file.path(output_dir, "epbc_spdata_all.csv"), 
          row.names = FALSE)

nrow(xdat[sensitive == 0])
write.csv(xdat[sensitive ==0], file = file.path(output_dir, "epbc_spdata_sensitive0.csv"))

bugs <- xdat[, .N, spfile][N>10]$spfile
xdat <- data[spfile %in% bugs]; dim(xdat)
write.csv(xdat, file = file.path(output_dir, "epbc_spdata_MT10.csv"), 
          row.names = FALSE)

## Write species shapefiles
shapefile_dir <- file.path(output_dir, "species_shapefiles", "epbc_species")

species_maps <- readRDS(file.path(output_dir, "species_ahullEOOspdf.rds"))
polygon_list <- names(species_maps)
polygon_list <- polygon_list[polygon_list %in% bugs]

registerDoMC(length(bugs))
system.time(log <- foreach(species_name = polygon_list,
                           .combine = rbind,
                           .errorhandling = "pass",
                           .packages = c('sp', 'raster', 'rgdal', 'data.table')) %dopar%{
                             
                             ## Write spatial data to disk if coming from RDS file
                             writeOGR(species_maps[[species_name]], 
                                      dsn = shapefile_dir, 
                                      layer = species_name, 
                                      driver = "ESRI Shapefile", 
                                      overwrite_layer = TRUE)
                           })
