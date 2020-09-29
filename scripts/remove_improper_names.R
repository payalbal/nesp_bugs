## From August's fun_fix_species_name


## Function to clean a character vector of species names
## Improper names replaced with NA
## Returns an updated species list and a list of removed species


remove_improper_names <- function(name_vector,
                                  allow.higher.taxa = FALSE,
                                  allow.subspecies = TRUE,
                                  improper.species.list = TRUE){
  
  message("Cleaning checklist for improper species names...")
  
  ## Check if name_vector is in expected format
  if (!is.character(name_vector)){
    stop("name_vector is not a character vector")
  }
  
  ## Clean trailing and double spaces
  name_vector <- stringr::str_squish(name_vector)
  originalN <- length(name_vector)
  
  # ## Remove names with brackets and quotation marks
  # name_vector[grep(" (", name_vector, fixed = TRUE)] <- 
  #   substr(name_vector[grep(" (", name_vector, fixed = TRUE)], 1, 
  #          regexpr(" (", name_vector[grep(" (", name_vector, fixed = TRUE)], 
  #                  fixed=TRUE)-1)
  # 
  # name_vector[grep("[", name_vector, fixed = TRUE)] <- 
  #   substr(name_vector[grep("[", name_vector, fixed = TRUE)], 1, 
  #          regexpr("[", name_vector[grep("[", name_vector, fixed = TRUE)], 
  #                  fixed=TRUE)-1)
  # 
  # name_vector[grep(" \"", name_vector, fixed = TRUE)] <- 
  #   substr(name_vector[grep(" \"", name_vector, fixed = TRUE)], 1, 
  #          regexpr(" \"", name_vector[grep(" \"", name_vector, fixed = TRUE)], 
  #                  fixed=TRUE)-1)
  # 
  # name_vector[grep(" \'", name_vector, fixed = TRUE)] <- 
  #   substr(name_vector[grep(" \'", name_vector, fixed = TRUE)], 1, 
  #          regexpr(" \'", name_vector[grep(" \'", name_vector, fixed = TRUE)], 
  #                  fixed=TRUE)-1)
  
  ## Record removed species
  if (improper.species.list){
    improper_species <- name_vector[(
      Reduce(union, 
             list(
               grep("Unplaced", name_vector, fixed = TRUE),
               grep("?", name_vector, fixed = TRUE),
               grep(" ex ", name_vector, fixed = TRUE),
               grep("sp.", name_vector, fixed = TRUE),
               grep("Sp.", name_vector, fixed = TRUE),
               grep("spp.", name_vector, fixed = TRUE),
               grep("aff.", name_vector, fixed = TRUE),
               grep("cf.", name_vector, fixed = TRUE),
               grep("indet.", name_vector, fixed = TRUE),
               grep(" sp ", name_vector, fixed = TRUE),
               grep(" sp$", name_vector),
               grep("aff ", name_vector, fixed = TRUE),
               grep("cf ", name_vector, fixed = TRUE),
               grep("indet ", name_vector, fixed = TRUE),
               grep(" or ", name_vector, fixed = TRUE),
               grep("[", name_vector, fixed = TRUE),
               grep("sensu", name_vector, fixed = TRUE),
               grep("species$", name_vector),
               grep("sens.", name_vector, fixed = TRUE),
               # grep("\"", name_vector, fixed = TRUE),
               # grep("\'", name_vector, fixed = TRUE),
               # grep("(", name_vector, fixed = TRUE),
               grep("-$", name_vector),
               grep(" - ", name_vector, fixed = TRUE),
               grep("/", name_vector, fixed = TRUE),
               grep("s.", name_vector, fixed = TRUE),
               grep("group", name_vector, fixed = TRUE),
               grep("etc.", name_vector, fixed = TRUE),
               grep(" x$", name_vector),
               grep("Unidentifi", name_vector),
               grep("spec.", name_vector, fixed = TRUE),
               grep("affin.", name_vector, fixed = TRUE),
               grep("species ", name_vector, fixed = TRUE),
               grep("taxon", name_vector, fixed = TRUE),
               grep("spec.nov.", name_vector, fixed = TRUE),
               grep("cf,", name_vector, fixed = TRUE),
               grep(" and ", name_vector, fixed = TRUE),
               grep(" with ", name_vector, fixed = TRUE)
             ))
    )]
    
  }
  
  
  ##  Set to NA all names with taxa modifiers (this list keeps subspecies and variety)
  name_vector[(
    Reduce(union, 
           list(        
             grep("Unplaced", name_vector, fixed = TRUE),
             grep("?", name_vector, fixed = TRUE),
             grep(" ex ", name_vector, fixed = TRUE),
             grep("sp.", name_vector, fixed = TRUE),
             grep("Sp.", name_vector, fixed = TRUE),
             grep("spp.", name_vector, fixed = TRUE),
             grep("aff.", name_vector, fixed = TRUE),
             grep("cf.", name_vector, fixed = TRUE),
             grep("indet.", name_vector, fixed = TRUE),
             grep(" sp ", name_vector, fixed = TRUE),
             grep(" sp$", name_vector),
             grep("aff ", name_vector, fixed = TRUE),
             grep("cf ", name_vector, fixed = TRUE),
             grep("indet ", name_vector, fixed = TRUE),
             grep(" or ", name_vector, fixed = TRUE),
             grep("[", name_vector, fixed = TRUE),
             grep("sensu", name_vector, fixed = TRUE),
             grep("species$", name_vector),
             grep("sens.", name_vector, fixed = TRUE),
             # grep("\"", name_vector, fixed = TRUE),
             # grep("\'", name_vector, fixed = TRUE),
             # grep("(", name_vector, fixed = TRUE),
             grep("-$", name_vector),
             grep(" - ", name_vector, fixed = TRUE),
             grep("/", name_vector, fixed = TRUE),
             grep("s.", name_vector, fixed = TRUE),
             grep("group", name_vector, fixed = TRUE),
             grep("etc.", name_vector, fixed = TRUE),
             grep(" x$", name_vector),
             grep("Unidentifi", name_vector),
             grep("spec.", name_vector, fixed = TRUE),
             grep("affin.", name_vector, fixed = TRUE),
             grep("species ", name_vector, fixed = TRUE),
             grep("taxon", name_vector, fixed = TRUE),
             grep("spec.nov.", name_vector, fixed = TRUE),
             grep("cf,", name_vector, fixed = TRUE),
             grep(" and ", name_vector, fixed = TRUE),
             grep(" with ", name_vector, fixed = TRUE)
           ))
  )] <- NA
  
  
  ##   [Optional] Remove single word names (likely higher taxa without species suffix)
  if (!allow.higher.taxa){
    # name_vector[grep(" ", name_vector, fixed = TRUE, invert = TRUE)] <- NA
    ##  set all names witout a space to NA, which should work because we fixed double and trailing space 
    
    name_vector[which(sapply(strsplit(as.character(name_vector), " "), length) <= 1)] <- NA
    ##  set names with less than or equal to one word as NA
  }
  
  # ## [Optional] Remove subspecies and variety suffices
  # if (!allow.subspecies){
  #   name_vector <- stringr::word(name_vector, 1,2)
  #   ##   this line keeps only the first two words of every non-NA name
  # }
  
  if (length(improper_species) != 0){
    message(cat("# Improper names (indicated by NAs) in updated species list: "), 
            sum(is.na(species_record$updated_list)))
    
    ## Print messages on screen
    print(paste0("Number of species removed: ", (originalN - length(name_vector))))
    print(paste0("Proprotion of species removed: ", (originalN - length(name_vector))/originalN))
    print(paste0("Number of species retained: ", length(name_vector)))
    message(cat("Is #species = #original species - #removed species? : ", 
                length(name_vector) == (length(name_vector) - length(improper_species))))
    
  }else {
    message("No improper species found in checklist")
  }
  
  return(list(updated_list = name_vector,improper_species = improper_species))
}
