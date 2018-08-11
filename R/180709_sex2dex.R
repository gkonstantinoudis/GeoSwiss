
#' Convert CH coordinates to WGS
#'
#' This function allows you to convert the swiss based coordinates to latitute and longitute
#' @param x the value of the xaxis
#' @param y the value of the yaxis
#' @examples
#' ch2wgs()

## Convert CH y/x to WGS lat
ch2wgs <- function (x, y){
  
  ## Converts military to civil and  to unit = 1000km
  ## Auxiliary values (% Bern)
  y_aux <- (y - 600000)/1000000
  x_aux <- (x - 200000)/1000000
  
  ## Process lat
  lat <- {16.9023892 +
      3.238272 * x_aux -
      0.270978 * (y_aux^2) -
      0.002528 * (x_aux^2) -
      0.0447   * (y_aux^2) * x_aux -
      0.0140   * (x_aux^3)}
  
  lng <- {2.6779094 +
      4.728982 * y_aux +
      0.791484 * y_aux * x_aux +
      0.1306   * y_aux * (x_aux^2) -
      0.0436   * (y_aux^3)}
  
  ## Unit 10000" to 1 " and converts seconds to degrees (dec)
  lat <- lat * 100/36
  lng <- lng * 100/36
  
  return(c(lat = lat, lng = lng))  
}


