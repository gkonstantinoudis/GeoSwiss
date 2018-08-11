

# Created 11.07.2018


# Documentation of the shapefiles of Switzerland


############################################################################################################

#' Shapefiles of Switzerland in 2017
#'
#' Data was retrieved by https://www.swisstopo.admin.ch/ and includes 
#' the shapefiles of the land, cantons and municipalities of Switzerland in 2017.
#' @docType data
#'
#' @usage data(LandShapefile)
#' data(CantonShapefile)
#' data(MunicipalityShapefile)
#' 
#' @format An object of class SpatialPolygonDataFrame 
#'
#' @keywords datasets
#'
#' @source https://www.swisstopo.admin.ch/ 
#'
#' @examples
#' data(CantonShapefile)
#' plot(CantonShapefile)
#' # to get an idea about the crude population counts
#' spplot(pol_canton2017, "EINWOHNERZ")
#' # and population density
#' pol_canton2017$popdens <- pol_canton2017$EINWOHNERZ/area(pol_canton2017)
#' spplot(pol_canton2017, "popdens")
"CantonShapefile"
