

#' Lattitude Longitude To UKNG
#'
#' @param lattitude as it says
#' @param longitude as it says
#'
#' @return GBNG value
#' @export lat_long_to_GBNG
#'
#' @examples  lat_long_to_GBNG(52.665137, -0.6906808)
#'
lat_long_to_GBNG <- function(lattitude, longitude){

  coords <- NULL
  coords$x <- longitude
  coords$y <- lattitude

  df.coords <- as.data.frame(coords)
  df.coords$x %>% as.numeric
  df.coords$y %>% as.numeric


  sp.coords.sf  <- sf::st_as_sf(x = df.coords,
                 coords = c("x", "y"),
                 crs = sf::st_crs(4326))

  #sp.coords.sf <- sf::st_as_sf(sp.coords)

  sp.coords.sf.ng  <-  sf::st_transform(sp.coords.sf, crs = sf::st_crs(27700))

  GBNG.out <- eco.atlas::sf_to_UKNGR_Letter(sp.coords.sf.ng)

  return(GBNG.out)

}


