#' GGMAP Bounding Box to Bounding Box
#'
#' The ggmap bounding box needs transforming so it may be used to guid visual
#' exploration of data. This function helps by recreating bounding boxes derived
#' from existing maps. The user can enter bew coordinates to centre the bounding
#' box, then increase the zoom value to centre on the data.
#'
#' @param ggmap_object ggmap object from which the bounding box is derived.
#' @param xcoord x cordinates in the non specific units.
#' @param ycoord y coordinates in non specific units.
#' @param zoom_factor Zoom factor. Defaults to 1. Bigger numbers zoom in.
#'
#' @return A bounding box
#' @export ggmap_to_bbox
#'
#'
ggmap_to_bbox <- function(ggmap_object, xcoord , ycoord, zoom_factor = 1){
  # Get the attributes
  ggmap_attr_bb <- attr(ggmap_object, "bb")

  Location.bb <- tmaptools::bb(ggmap::bb2bbox(ggmap_attr_bb) ,
                relative = FALSE,
                output = "matrix")

  #Estimated center for a map

   bb.sides <- abs( abs(Location.bb[,2]) - abs(Location.bb[,1]))
  # Location.bb.new <- tmaptools::bb(width = bb.sides[1]*.1, height = bb.sides[2]*.1,

# Calculate the height
  height_box <- abs(abs(ggmap_attr_bb[[2]]) - abs(ggmap_attr_bb[[4]]))
  # Calculate width box
  width_box <- abs(abs(ggmap_attr_bb[[1]]) - abs(ggmap_attr_bb[[3]]))
# Now calculate a bbox

  bb.new <- tmaptools::bb(width = bb.sides[1] / zoom_factor,
                          height = bb.sides[2] / zoom_factor,
                                   cx = xcoord ,cy = ycoord,
                                   relative = FALSE,
                                   output = "matrix"
  )
  return(bb.new)
}
