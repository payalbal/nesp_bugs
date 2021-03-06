## ---------------------------
##
## Retrieving name source information for cleaned AFD checklist
##
##
## Author: Matilda Stevenson
## Email: matilda.stevenson@csiro.au
##
## Date Created: 13-10-2020
##
## ---------------------------
##
## Notes: 
## Uses `ala_taxa` function for name search, currently in development
## and available at https://github.com/AtlasOfLivingAustralia/ALA4R/blob/dev/R/ala_taxa.R
## ---------------------------

get_source <- function(sp_names) {
  
  require(dplyr)
  require(assertthat)
  require(httr)
  require(jsonlite)
  
  sp_names <- as.data.frame(sp_names)
  
  ## Define functions
  find_name_source <- function(taxon_id) {
    url <- parse_url("https://bie-ws.ala.org.au")
    url$path <- c('ws', 'species', taxon_id)
    resp <- fromJSON(build_url(url))
    return(resp$taxonConcept$nameAuthority)
  }
  
  query_caab <- function(taxon_id) {
    if (!is.na(as.numeric(taxon_id))) {
      url <- parse_url("http://www.cmar.csiro.au/")
      url$path <- c("data","caab","api")
      url$query$caab_code <- taxon_id
      resp <- fromJSON(build_url(url))
      return(resp$habitat)
    }
    return(NA)
  }
  
  ## Find names matches in ala, data sources, and habitat info
  output <- sp_names %>%
    mutate(taxon_id = ala_taxa(sp_names)$taxonConceptID) %>%
    rowwise() %>%
    mutate(name_source = find_name_source(taxon_id)) %>%
    rowwise() %>%
    mutate(caab_habitat = query_caab(taxon_id))
  
  return(as.data.table(output))
}