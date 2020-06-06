#' Bounding Box To SpatialPolyon
#'
#'Converting a bounding box to a spatial polygon allows for easier filtering of
#'data. Use the construct: SpatialPointsDataFrame BoundingBoxPolygon, to
#'filter data. Convert back to a dataframe using %>% as.data.frame  for
#'plotting.
#'
#'
#' @param bbox Bounding box
#' @param scale.factor A numeric value to scale the bounding box .5 zooms in
#' @param CRS Map projection default sp::CRS("+init=epsg:4326")
#'
#' @return SpatialPolyon
#' @export bb_to_polygon
#'
#' @examples
#'  leics.bb <- rbind(
#'  'x' = c(min =-1.831381, max = -0.1945425 ),
#'  'y' = c(min = 52.275062, max =53.0947564 ) )
#'  leics.bb.matrix <- data.matrix(leics.bb)
#' leics.bb.polygon <- bb_to_polygon(leics.bb.matrix)
#'
bb_to_polygon <- function(bbox, scale.factor = 1, CRS= sp::CRS("+init=epsg:4326")) {

  # Make sure we know the bounding box format
  Location.bb.bb <- tmaptools::bb(bbox, ext= scale.factor, output = "bbox")

  # Note make sure the coordinates are in the correct order
  temp.coords <- cbind(
    Location.bb.bb[c(1,1,3,3, 1)],
    Location.bb.bb[c(2,4,4,2,2)]
  )
  row.names(temp.coords) <- NULL

  # Make the polygon

 sp.poly <- sp::SpatialPolygons(
    list(sp::Polygons(list(sp::Polygon(temp.coords)), "id")),
    proj4string = CRS)

 return(sp.poly)
}
