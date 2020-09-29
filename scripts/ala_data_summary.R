## Summarize ALA data


## Cleaning
## Check taxonnomic issues
## Check geographic issues
## Check for assertions
## Check for generalisations...
if(any(grepl("assertions", names(df)))){
  names(df)[which(grepl("assertions", names(df)))]
}

## Check recorded issues with geographic or taxonomic fields
## Fields: "taxonomic_kosher","geospatial_kosher"


## Remove duplicates
# if(remove_duplicates == TRUE){
#   ala_df <- ala_df[!duplicated(ala_df[ , c("longitude", "latitude")]), ]
# }


## Test plot
aus.mask <- raster(file.path(output_dir, "ausmask_WGS.tif"))
plot(aus.mask, col = "grey", axes = FALSE, box = FALSE, legend = FALSE)
points(ala_df[,.(longitude, latitude)], pch = 4, col = "red", cex = 0.5)
