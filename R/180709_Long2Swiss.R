

## Convert WGS lat/long (? dec) to CH
#' A WGS lat/long to CH
#'
#' This function allows you to covert the latitude and longitute to swiss coordinates
#' @param lat A vector of latitute values
#' @param long A vector of longitute values
#' @examples
#' WGS2CH()


WGS2CH <- function(lat, lng){
  ## Converts decimal degrees to sexagesimal seconds

  lat <- dex2sex(lat)
  lng <- dex2sex(lng)
  
  ## Auxiliary values (% Bern)
  lat_aux <- (lat - 169028.66)/10000
  lng_aux <- (lng - 26782.5)/10000
  
  ## 
  x <- {200147.07 +
      308807.95 * lat_aux + 
      3745.25 * (lng_aux^2) +
      76.63 * (lat_aux^2) -
      194.56 * (lng_aux^2) * lat_aux +
      119.79 * (lat_aux^3)}
  
  y <- {600072.37 +
      211455.93 * lng_aux -
      10938.51 * lng_aux * lat_aux -
      0.36 * lng_aux * (lat_aux^2) -
      44.54 * (lng_aux^3)}
  
  return(c(CH.x = x, CH.y = y))
}

