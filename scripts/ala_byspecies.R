## Save species files
...


## Clean by QA assertionns 
qa <- as.data.frame(read.csv(file.path(output_dir, "qa_assertions.csv")))

exclude <- qa[which(qa$exclude == 1),]$name
keep <- qa[which(qa$keep == 1),]$name
drop_qa <- qa[which(qa$not_relevant == 1),]$name

# ## Remove generalised data (as much as possible)
# if(any(grepl("dataGeneralizations", names(ala_df)))) {
#   ala_df <- ala_df[ala_df$dataGeneralizationsOriginal == FALSE,]
# }


## No date



## Mask
reg.mask.file = file.path(output_dir, "ausmask_WGS.tif")


## Get rid of unusable long lat vals
ala_df <- ala_df[ala_df$longitude > -180 &
                   ala_df$longitude < 180 &
                   ala_df$latitude > -90 &
                   ala_df$latitude < 90, ]


# ## Remove duplicates
# if(remove_duplicates == TRUE){
#   ala_df <- ala_df[!duplicated(ala_df[ , c("longitude", "latitude")]), ]
# }
JM...  museum specimens with duplicated lat-long


## Fire extent
