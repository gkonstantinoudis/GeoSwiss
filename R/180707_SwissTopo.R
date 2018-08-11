

#' Use swiss topo for geocodes
#'
#' @description  This function take as address information as arguements and retrieves the geocodes using the swiss topo API. 
#' The minimum information required is the postal code. When insufficient information is provided then the function returns the central
#' point of all possible addresses with a precision metric. The presicion metric is the distance of the mid-point retrieved to the most
#' remote address of this area. Precision 0 means precise geocode. If the precision is NA, swiss topo could not identify this address, implying
#' a mistake in the impute, like spelling mistake in the street name or no correspondance between street and postal code. If the incorrect information 
#' cannot be identified, then one could omit the least reliable information and rerun the algorithm to get a centroid with some precision parameter. 
#' The unit of the precision is the same as the unit of the geocodes. 
#' @param street the street name, if not given set to blank
#' @param number the street number, if not given set to blank
#' @param postal the postal code, the minimum requred information
#' @param city the city, if not given set to blank
#' @param precision if TRUE gives a measure about the precision of geocode returned
#' @details The output of this function is a list with two elements. The first is the geocode and the second some additional information. 
#' Additional information is only provided when precition is requested and if the geocode is not of precision NA or 0. The additional infromation
#' gives the alternative geocodes of the given address.
#' @examples # Mittelstrasse 43, 3012, Bern
#' street <-  "Mittelstrasse"
#' number <- "43"
#' postal <- "3012"
#' city <- "Bern"
#' MittelstrasseGeo <- 
#'    SwissTopoGeocodes(
#'    street =  street,
#'    number =number,
#'    postal = postal,
#'    city = city, precision = T
#'  )
#'  # when insufficient information is given
#' street <-  "Mittelstrasse"
#' number <- ""
#' postal <- "3012"
#' city <- ""
#' MittelstrasseGeo <- 
#'    SwissTopoGeocodes(
#'    street =  street,
#'    number =number,
#'    postal = postal,
#'    city = city, precision = T
#'  )
#'plot(MittelstrasseGeo$addinfo$x, MittelstrasseGeo$addinfo$y)
#'points(MittelstrasseGeo$geocode$x, MittelstrasseGeo$geocode$y, col = "red", pch = 19)
#' SwissTopoGeocodes()

###################################################################################

# install.packages("httr")
# install.packages("jsonlite")

SwissTopoGeocodes <- function(street = "", number = "", postal = postal, city = "", precision = F){
  
  require(httr)
  require(jsonlite)
  
  base <- "https://api3.geo.admin.ch/"
  endpoint <- "rest/services/api/SearchServer"
  
  
  address <- paste(street, number, postal, city, sep = " ")
  address_URL <- URLencode (address)
  
  call1 <- paste(base,endpoint,"?","type","=", "locations&searchText=", address_URL, sep="")
  
  get_add <- GET(call1)
  get__text <- content(get_add, "text", encoding = "UTF-8")
  get__add <- fromJSON(get__text, flatten = TRUE)
  
  temp <- (get__add$results)

  if(is.null(temp) == TRUE | length(temp) == 0){
    warning("No address could be retrieved. Check the if the information given is correct (for example, 
            check the spelling of the street name)")
    temp <- data.frame(Address = address, lon =NA, lat = NA, x = NA, y = NA, 
                       precision = NA)
    more.info <- ""
  }else{
    if(nrow(temp) == 1){
      temp <- data.frame(Address = temp$attrs.detail, lon = temp$attrs.lon, lat = temp$attrs.lat, x = temp$attrs.x, y = temp$attrs.y, 
                         precision = 0)
      more.info <- ""
    }
    
    if(nrow(temp) != 1){
      
      if(temp$attrs.detail[1] == postal){temp <- temp[-1,]}
      
      temp <- data.frame(Address = temp$attrs.detail, lon = temp$attrs.lon, lat = temp$attrs.lat, x = temp$attrs.x, y = temp$attrs.y, 
                         precision = NA)
      more.info <- ""

      if(precision == T){
        
        # temp.dist <- dist(cbind(temp$x, temp$y))
        # max(temp.dist)
        # define a centroid
        temp.centroid <- c((max(temp$x) - (max(temp$x)-min(temp$x))/2), (max(temp$y) - (max(temp$y)-min(temp$y))/2))
        temp.dist <- apply(cbind(temp$x, temp$y), 1, function(X){sqrt((X[1] - temp.centroid[1])^2 + (X[2] - temp.centroid[2])^2)})
        
        more.info <- temp
        more.info$precision <- temp.dist
        temp <- data.frame(Address = temp$Address[which.min(temp.dist)], 
                           lon = temp$lon[which.min(temp.dist)], 
                           lat = temp$lat[which.min(temp.dist)], 
                           x = temp$x[which.min(temp.dist)], 
                           y = temp$y[which.min(temp.dist)], 
                           precision = max(temp.dist))

      }
      
    }
    Obj2ret <- list()
    Obj2ret$geocode <- temp
    Obj2ret$addinfo <- more.info
        }
  

  return(Obj2ret)
}

