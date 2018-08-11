
#' Aggregate per spatial unit
#'
#' This function take as arguements geocoded locations and some spatial units and aggregates the geocodes on the selected spatial units. 
#' @param geocodes a SpatialPoints object
#' @param shapefile a shapefile object or raster object defining the desirable spatial units
#' @param density if TRUE then the function computes the density of the points per spatial unit
#' @examples library(spatstat)
#'  library(maptools)
#'  set.seed(123456789)
#'  x <- runif(100, -5, 5)
#'  y <- runif(100, -5, 5)
#'  df <- data.frame(x=x, y=y) 
#'  # need a boundary window: 
#'  W <- owin(xrange=c(-5,5), yrange=c(-5, 5))
#'  # create point pattern 
#'  X <- as.ppp(df, W=W) 
#'  plot(X) 
#'  # compute Dirichlet triangles 
#'  Y <- dirichlet(X) 
#'  plot(Y) 
#'  points(df$x, df$y)
#'  # convert 
#'  t <- as(Y, "SpatialPolygons") 
#'  newdata <- data.frame(layer = 1:length(x))
#'  row.names(newdata) <- 1:length(x)
#'  polygons <- SpatialPolygonsDataFrame(t, newdata)
#'  # and make it a spatialdataframe
#'  newdata <- data.frame(layer = 1:length(x))
#'  row.names(newdata) <- 1:length(x)
#'  polygons <- SpatialPolygonsDataFrame(t, newdata)
#'  # now aggregate sample points on this domain 
#'  set.seed(123456789)
#'  x_1 <- runif(1000, -5, 5)
#'  y_1 <- runif(1000, -5, 5)
#'  points(x_1, y_1, cex = .5, col = "red")
#'  # and now use Agg2Spatial for the aggregation
#'  geosp <- SpatialPoints(cbind(x_1, y_1))
#'  aggobj <- Agg2Spatial(geocodes = geosp, shapefile = polygons, density = F)
#'  spplot(aggobj, "x")
#' Agg2Spatial()



Agg2Spatial <- function(geocodes, shapefile, density = FALSE){
  
  require(sp); require(maptools)
  # it takes snc data as dataframe with only coordinates
  shapefile$ID <- 1:nrow(shapefile)
  spat.points.temp <- SpatialPoints(geocodes, proj4string = shapefile@proj4string)
  over.tes <- over(spat.points.temp, shapefile)
  over.tes$x <-  spat.points.temp@coords[,1]
  over.tes$y <-  spat.points.temp@coords[,2]
  
  # add 1 as counts to be aggregated
  
  over.tes$count <- 1
  temp.ag <- aggregate(over.tes$count, by = list(ID = over.tes$ID), sum)
  
  temp.merge <- sp::merge(shapefile, temp.ag, by.x= "ID", by.y = "ID")
  # everything flagged as out stays NA, everything in becomes 0
  temp.merge$x[is.na(temp.merge$x)] <- 0
  
  if(density == TRUE){
  temp.merge$dens <- temp.merge$x/unlist(lapply(temp.merge@polygons, function(X) slot(X, "area")))
  }
  
  return(temp.merge)
}



