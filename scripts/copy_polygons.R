bugs_data = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
output_dir = file.path(bugs_data, "outputs")
polygons_dir = file.path(output_dir,"polygons_alphahull_masked_10k")


shpfiles <- list.files(file.path(polygons_dir, "shapesIUCN"), 
                            pattern = ".shp$",
                            full.names = TRUE, all.files = TRUE)
shpfilesall <- list.files(file.path(polygons_dir, "shapesIUCN"),
                          full.names = TRUE, all.files = TRUE)

length(shpfiles)
length(shpfilesall)

x <- sample(shpfiles, 50)
y <- basename(tools::file_path_sans_ext(x))

shpcopy <- grep(paste(y,collapse="|"), shpfilesall, value = TRUE)
length(shpcopy)

file.copy(shpcopy, file.path(bugs_data, "polygons_Aaron"),
          overwrite = FALSE, recursive = TRUE,
          copy.mode = TRUE, copy.date = TRUE)

length(list.files(file.path(bugs_data, "polygons_Aaron")))


## TO FIX
grep("Aedes_(Ochlerotatus)", shpfilesall, value = TRUE)...