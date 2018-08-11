
#' Convert decimal degrees to sexagesimal seconds
#'
#' This function allows you convert decimal degrees to sexagesimal seconds
#' @param angle An vector of angles to be converted to sexagesimal second
#' @examples
#' cat_function()

## Convert decimal degrees to sexagesimal seconds
dex2sex <- function(angle){
  
  ## Extract DMS
  angle_chr <- as.character(angle)
  deg <- as.numeric(strsplit(angle_chr, "\\.")[[1]][1])
  min <- as.numeric(strsplit(as.character((angle-deg)*60), "\\.")[[1]][1])
  sec <- (((angle-deg)*60) - min) * 60
  
  ## Result in seconds
  return(sec + min*60 + deg*3600)
  
}
