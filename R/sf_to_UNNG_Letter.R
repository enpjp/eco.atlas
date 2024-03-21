#' Simple Feature Coordinates to UKNG Letter
#'
#' Accepts a Simple Features object and returns the enclosing tetrad with
#' leading letters.
#'
#' @param sf_coords Simple features object.
#'
#' @return A dataframe of tetrads the same length as the input.
#'
#' @export sf_to_UKNGR_Letter
#'
#'
sf_to_UKNGR_Letter <- function(sf_coords) {

  df_working <- sf::st_coordinates(sf_coords) %>% as.data.frame()

  df_working$Easting_trimmed <- df_working$X %>% round(digits = 0) %>% as.character %>% substr(2,6)
  df_working$Easting_lookup <- df_working$X %>% as.numeric() %>% substr(1,1)
  #Easting_trimmed_vector <- df_working$Easting_trimmed
  df_working$Northing_trimmed <- df_working$Y %>% round(digits = 0) %>% as.character  %>% substr(2,6)
  df_working$Northing_lookup <- df_working$Y %>% as.numeric() %>% substr(1,1)
  #Northing_trimmed_vector <- df_working$Northing_trimmed

  # Need to look up the grid letter from the National Grid Lookup
  UKNG_Lookup <- array(  c(
    "SV", "SQ", "SL", "SF", "SA", "NV", "NQ", "NL", "NF", "NA", "HV", "HQ", "HL",
    "SW", "SR", "SM", "SG", "SB", "NW", "NR", "NM", "NG", "NB", "HW", "HR", "HM",
    "SX", "SS", "SN", "SH", "SC", "NX", "NS", "NN", "NH", "NC", "HX", "HS", "HN",
    "SY", "ST", "SO", "SJ", "SD", "NY", "NT", "NO", "NJ", "ND", "HY", "HT", "HO",
    "SZ", "SU", "SP", "SK", "SE", "NZ", "NU", "NP", "NK", "NE", "HZ", "HU", "HO",
    "TV", "TQ", "TL", "TF", "TA", "OV", "OQ", "OL", "OF", "OA", "JV", "JQ", "JL",
    "TW", "TR", "TM", "TG", "TB", "OW", "OR", "OM", "OG", "OB", "JW", "JR", "JM"
  ),
  dim = c(13, 7), dimnames = NULL
  ) %>% as.data.frame()
  # lookup Matrix

  for (my.row.number in 1: nrow(df_working) ) {

    #grid_letters <- UKNG_Lookup[df_working$Northing_lookup[my.row.number],df_working$Easting_lookup[my.row.number]] %>% as.character()
    easting <- df_working$Easting_lookup[my.row.number] %>% as.numeric()
    northing <- df_working$Northing_lookup[my.row.number] %>% as.numeric()
    grid_letters <- UKNG_Lookup[northing+1,easting+1] %>% as.character()
    df_working$GridLetters[my.row.number] <- grid_letters
    df_working$GridRef[my.row.number] <- paste(grid_letters,df_working$Easting_trimmed[my.row.number], df_working$Northing_trimmed[my.row.number], sep ="")

    #my.data$GeoTag[my.row.number] <- df_working$GridRef[my.row.number]

  }

  return(df_working$GridRef)


}
