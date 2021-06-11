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

## Trait data
trait <- as.data.table(fread(file.path(bugs_dir, "JM_traits", "Fire_impacted_invert_traits_09.06.csv")))
trait_sp <- unique(trait$spfile)
length(trait_sp)

## ALAnonALA data
data <- fread(file.path(output_dir, "data_ALAnonALA_wgs84_corrected.csv"))
data_sp <- unique(data$spfile)

## Trait data species found in ALAnonALA data
all(trait_sp %in% data_sp)
trait_sp[which(!(trait_sp %in% data_sp))]
  ## NA spfiles, not found 
trait[grep("#N/A", trait$spfile)]$scientificName %in% unique(data$scientificName)
  ## Corresponding scientificName not foudn in our data either

## Subset ALAnonALA data for trait data species
trait_data <- data[spfile %in% trait_sp]

## Check
length(unique(trait_data$spfile)) == sum(trait$spfile %in% data$spfile)
