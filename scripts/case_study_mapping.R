## Mapping species outputs from ALA only and ALA+state/museum datasets
## ALA onnly referred to as 'ALAonly'
## ALA+state/museum referred to as 'ALA plus'


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "sp", "raster", 
       "rgdal", "rgeos", "gdalUtils",
       "doMC", "future", "future.apply", "parallel")
lapply(x, require, character.only = TRUE)
rm(x)


## Files and folder
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_dir, "outputs")

## Q1. Adequacy of ALA data ####
## To compare species distributions & fire overlap based on ALA only versus ALA+other (state/museum) data
## Spatial distribution (square kms) based on EOO
## See firediffdata_comboplot.xlsx and firediffdata.xlsx for species selection
casestudy_dir = file.path(output_dir, "case_study", "datasources")



## I. Map species showing LARGE POSITIVE differences in EOO ####
bugs <- c("cherax_destructor_3729", "hypolimnas_bolina_nerina_4017",
            "scolopendra_laeta_2571", "hoggicosa_bicolor_1438",
            "vanessa_kershawi_3660", "danaus_petilia_3938",
            "latrodectus_hasseltii_1809", "orthetrum_caledonicum_3617",
            "euploea_corinna_3968", "nyssus_coloripes_2236",
            "belenois_java_teutonia_3899")

## Write data as csv
data <- fread(file.path(casestudy_dir, "data_casestudy_Q1.csv"))
data <- data[spfile %in% bugs]
data[, .N, by = spfile]
write.csv(data, file = file.path(casestudy_dir, "EOOinc_data_ALAplus.csv"), 
          row.names = FALSE)

data[data_type == "ala"][, .N, by = scientificName]
write.csv(data[data_type == "ala"], file = file.path(casestudy_dir, "EOOinc_data_ALAonly.csv"), 
          row.names = FALSE)


# ## Fire severity raster 
# fire_severity <- raster(file.path(output_dir, "fire", "severity5_eqar250_native_paa.tif"))
# fire_res <- res(fire_severity)
# fire_crs <- as.character(crs(fire_severity))
# fire_extent <- extent(fire_severity)

## Write species shapefiles using ALAonly data
shapefile_dir <- file.path(casestudy_dir, "species_shapefiles", "EOO_inc", "ALAonly_shapefiles")

species_maps <- readRDS(file.path(casestudy_dir, "species_ahullEOOspdf.rds"))
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
                             
                             # ## Reproject species boundaries to match fire severity raster
                             # system(paste0("gdal_rasterize -at -burn 1 -ot Byte -tr .0025 .0025 -l ",
                             #               species_name, " ",
                             #               file.path(shapefile_dir, species_name), ".shp ",
                             #               file.path(shapefile_dir, species_name), ".tif"))
                             # 
                             # system(paste0("gdalwarp -overwrite -ot Byte -tr ", 
                             #               paste(fire_res, collapse = " "), " -te ", 
                             #               paste(fire_extent[1], fire_extent[3], 
                             #                     fire_extent[2], fire_extent[4]), 
                             #               " -s_srs 'EPSG:4326' -t_srs '", fire_crs, "' ",
                             #               file.path(shapefile_dir, species_name), ".tif ",
                             #               file.path(shapefile_dir, species_name), "_p.tif"))
                           })

## Write species shapefiles using ALAplus data
shapefile_dir <- file.path(casestudy_dir, "species_shapefiles", "EOO_inc", "ALAplus_shapefiles")

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
                             
                             # ## Reproject species boundaries to match fire severity raster
                             # system(paste0("gdal_rasterize -at -burn 1 -ot Byte -tr .0025 .0025 -l ",
                             #               species_name, " ",
                             #               file.path(shapefile_dir, species_name), ".shp ",
                             #               file.path(shapefile_dir, species_name), ".tif"))
                             # 
                             # system(paste0("gdalwarp -overwrite -ot Byte -tr ", 
                             #               paste(fire_res, collapse = " "), " -te ", 
                             #               paste(fire_extent[1], fire_extent[3], 
                             #                     fire_extent[2], fire_extent[4]), 
                             #               " -s_srs 'EPSG:4326' -t_srs '", fire_crs, "' ",
                             #               file.path(shapefile_dir, species_name), ".tif ",
                             #               file.path(shapefile_dir, species_name), "_p.tif"))
                           })



## II. Map species showing LARGE NEGATIVE differences in EOO ####
bugs <- c("allodessus_bistrigatus_3694" , "iridomyrmex_rufoniger_3757",
           "onthophagus_parvus_4489", "rhytidoponera_metallica_4553",
           "austrolestes_leda_3895", "camponotus_aurocinctus_3710")

## Write data as csv
data <- fread(file.path(casestudy_dir, "data_casestudy_Q1.csv"))
data <- data[spfile %in% bugs]
data[, .N, by = spfile]
write.csv(data, file = file.path(casestudy_dir, "EOOdec_data_ALAplus.csv"), 
          row.names = FALSE)

data[data_type == "ala"][, .N, by = scientificName]
write.csv(data[data_type == "ala"], file = file.path(casestudy_dir, "EOOdec_data_ALAonly.csv"), 
          row.names = FALSE)

## Write species shapefiles using ALAonly data
shapefile_dir <- file.path(casestudy_dir, "species_shapefiles", "EOO_dec", "ALAonly_shapefiles")

species_maps <- readRDS(file.path(casestudy_dir, "species_ahullEOOspdf.rds"))
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

## Write species shapefiles using ALAplus data
shapefile_dir <- file.path(casestudy_dir, "species_shapefiles", "EOO_dec", "ALAplus_shapefiles")

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



## III. Map species showing LARGE POSITIVE differences in fire overlap ####
bugs <- c("heteronympha_merope_3592", "atrax_robustus_4643",
          "vanessa_itea_3659", "junonia_villida_3760",
          "petalura_litorea_4745")

## Write data as csv
data <- fread(file.path(casestudy_dir, "data_casestudy_Q1.csv"))
data <- data[spfile %in% bugs]
data[, .N, by = spfile]
write.csv(data, file = file.path(casestudy_dir, "FireInc_data_ALAplus.csv"), 
          row.names = FALSE)

data[data_type == "ala"][, .N, by = scientificName]
write.csv(data[data_type == "ala"], file = file.path(casestudy_dir, "FireInc_data_ALAonly.csv"), 
          row.names = FALSE)

## Write species shapefiles using ALAonly data
shapefile_dir <- file.path(casestudy_dir, "species_shapefiles", "Fire_Inc", "ALAonly_shapefiles")

species_maps <- readRDS(file.path(casestudy_dir, "species_ahullEOOspdf.rds"))
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

## Write species shapefiles using ALAplus data
shapefile_dir <- file.path(casestudy_dir, "species_shapefiles", "Fire_Inc", "ALAplus_shapefiles")

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



## III. Map species showing LARGE NEGATIVE differences in fire overlap ####
bugs <- c("austroargiolestes_calcaris_3547", "catopsilia_pyranthe_crokera_3913",
          "synlestes_weyersii_3639", "hyridella_hyridella_depressa_9023",
          "austroargiolestes_icteromelas_icteromelas_3548",
          "myrmecia_brevinoda_4441")

## Write data as csv
data <- fread(file.path(casestudy_dir, "data_casestudy_Q1.csv"))
data <- data[spfile %in% bugs]
data[, .N, by = spfile]
write.csv(data, file = file.path(casestudy_dir, "FireDec_data_ALAplus.csv"), 
          row.names = FALSE)

data[data_type == "ala"][, .N, by = scientificName]
write.csv(data[data_type == "ala"], file = file.path(casestudy_dir, "FireDec_data_ALAonly.csv"), 
          row.names = FALSE)

## Write species shapefiles using ALAonly data
shapefile_dir <- file.path(casestudy_dir, "species_shapefiles", "Fire_Dec", "ALAonly_shapefiles")

species_maps <- readRDS(file.path(casestudy_dir, "species_ahullEOOspdf.rds"))
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

## Write species shapefiles using ALAplus data
shapefile_dir <- file.path(casestudy_dir, "species_shapefiles", "Fire_Dec", "ALAplus_shapefiles")

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


## NA species
bugs <- c("limborelia_exquisita_4701", 
          "opinorelia_howeinsulae_4725",
          "placostylus_maoristylus_bivaricosus_11975",
          "charopella_wilkinsoni_4648",
          "goweroconcha_waterhousiae_4673",
          "goweroconcha_wilsoni_4675",
          "innesoconcha_catletti_4692",
          "melloconcha_delecta_4707",
          "melloconcha_flavescens_4708",
          "melloconcha_rosacea_4711",
          "gudeoconcha_sophiae_4680",
          "howearion_belli_4689",
          "howearion_hilli_4690",
          "allenella_formalis_4625",
          "dignamoconcha_dulcissima_4661",
          "semilaoma_lidgbirdensis_4761",
          "palaina_capillacea_4729",
          "palaina_deliciosa_4730",
          "palaina_intercollis_4731",
          "palaina_macgillivrayi_4734",
          "palaina_waterhousei_4735")

data <- fread(file.path(casestudy_dir, "data_casestudy_Q1.csv"))
data <- data[spfile %in% bugs]
write.csv(data, file = file.path(casestudy_dir, "NA_species.csv"), row.names = FALSE)

