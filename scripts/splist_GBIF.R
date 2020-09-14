library(data.table)
library(stringr)

## Set path to data folder
  # server_path <- "/tempdata/workdir/data"

gsdms_path <- "/Volumes/uom_data/gsdms_data/gbif" 
data_path <- "/Volumes/uom_data/nesp_bugs_data" 

## GBIF backbone taxonomy
## GBIF Secretariat (2017). GBIF Backbone Taxonomy. Checklist dataset https://doi.org/10.15468/39omei accessed via GBIF.org on 2019-08-26.
  # system(paste0("curl https://hosted-datasets.gbif.org/datasets/backbone/backbone-current.zip -o", server_path, "/gbif_taxonomy.zip"))
  # unzip(file.path(gsdms_path, "gbif_taxonomy.zip"), list = TRUE)
  # unzip(file.path(gsdms_path, "gbif_taxonomy.zip"), files = 'Taxon.tsv', exdir = gsdms_path)
backbone.raw <-fread(file.path(gsdms_path, "Taxon.tsv"))

## Write out all phyla in GBIF backbone taxonomy
  # unique(backbone.raw$kingdom)
  # backbone <- backbone.raw[kingdom == "Animalia"]
  # # sum(backbone$kingdom == "Animalia")
  # # sum(backbone$kingdom =="Animalia" & backbone$taxonRank == "species")
  # write.table(sort(unique(backbone$phylum)), "./output/gbif_phyla.csv", row.names = FALSE, quote = FALSE, col.names = "phyla")

## Subset to selected phyla - as per invert_phyla_list.xlsx
selected_phyla <- c("Acanthocephala", "Annelida", "Arthropoda", "Brachiopoda", "Bryozoa", 
                    "Cnidaria", "Entoprocta", "Gastrotricha", "Mollusca", "Myxozoa",
                    "Nematoda", "Nematomorpha", "Nemertea", "Onychophora", "Placozoa", 
                    "Platyhelminthes", "Porifera", "Rotifera", "Tardigrada", "Xenacoelomorpha")
backbone <- backbone.raw[phylum %in% selected_phyla]
readr::write_csv(backbone, file.path(data_path, "GBIF/gbif_invert_taxonomy.csv"))

## Check selected phyla
unique(backbone$phylum)

## Checks for # unique species
dim(backbone)
unique(backbone$taxonRank)
length(backbone$taxonRank == "species")
length(unique(backbone$taxonID))
length(unique(backbone$canonicalName))
length(unique(backbone$scientificName))

## Find duplicates in scientificName
length(backbone$scientificName[duplicated(backbone$scientificName)])

# List duplicates for scientificName (exclusing first appearance)
dim(backbone[duplicated(backbone$scientificName),])

## List all duplicates for scientificName (including first appearance)
temp <- backbone[which(duplicated(backbone$scientificName) | duplicated(backbone$scientificName[length(backbone$scientificName):1])[length(backbone$scientificName):1]),]
dim(temp)
readr::write_csv(temp, "./output/gbif_backbone_repeats.csv")

## Create list of scientific names without the author and year from backbone$scientificName
## NOTE: Use scientificName not canonicalName to 
##        get maximum number of unique species
##        scientificName = canoncial name + author + year
scientific_names <- unique(backbone$scientificName) 
bold_names <- grep("BOLD", scientific_names, value = TRUE)
new_scientific_names <- substr(scientific_names[grep(" [A-Z]", scientific_names)], 1, regexpr(" [A-Z]", scientific_names[grep(" [A-Z]", scientific_names)], perl=TRUE)-1)

length(scientific_names)
length(new_scientific_names)

## Match canonicalName and scientificName & find duplicates
canonical_name <- unique(backbone$canonicalName)

distinct()...
union(new_scientific_names, canonical_name)

## EXTRAS
# '%!in%' <- function(x,y)!('%in%'(x,y))
# "%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
# kingdom[, c(taxrank() %w/o% c("kingdom", "species", "infraspecific")) := NULL]



## Create matrix of name components
max_words <- max(sapply(strsplit(scientific_names, " "), length))
df <- stringr::str_split_fixed(scientific_names,
                               pattern = " |,",
                               n = max_words)
## Any element (except first) that contains a capital letter are
## replaced with ""
df[ , -1][grepl("[A-Z]", df[ , -1])] <- ""

## Find index of first "" in each row
rind <- c()
for (i in 1:nrow(df)){
  rind[i] <- min(which(df[i, ]==""))-1
}
## e.g. df[which(rind>5),]

## Create new names by pasting results uptil a "" together
## Need to trim whitespace for pasting blank second name
new_scientific_names <- stringr::str_trim(paste(df[ ,1],
                                                df[ ,2]))
