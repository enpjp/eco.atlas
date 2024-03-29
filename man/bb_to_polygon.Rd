% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bb_to_polygon.R
\name{bb_to_polygon}
\alias{bb_to_polygon}
\title{Bounding Box To SpatialPolyon}
\usage{
bb_to_polygon(bbox, scale.factor = 1, CRS = sp::CRS("+init=epsg:4326"))
}
\arguments{
\item{bbox}{Bounding box}

\item{scale.factor}{A numeric value to scale the bounding box .5 zooms in}

\item{CRS}{Map projection default sp::CRS("+init=epsg:4326")}
}
\value{
SpatialPolyon
}
\description{
Converting a bounding box to a spatial polygon allows for easier filtering of
data. Use the construct: SpatialPointsDataFrame BoundingBoxPolygon, to
filter data. Convert back to a dataframe using \%>\% as.data.frame  for
plotting.
}
\examples{
 leics.bb <- rbind(
 'x' = c(min =-1.831381, max = -0.1945425 ),
 'y' = c(min = 52.275062, max =53.0947564 ) )
 leics.bb.matrix <- data.matrix(leics.bb)
leics.bb.polygon <- bb_to_polygon(leics.bb.matrix)

}
