
#' An Umlaut function
#'
#' This function identifies the umlaut of a word and replaces it with the same word omitting the umlauts
#' @param x a character type word with umlauts
#' @examples
#' rmuml()

rmuml <- function (x) {
  x <- gsub("?", "a", x, ignore.case=TRUE)
  x <- gsub("?", "o", x, ignore.case=TRUE)
  x <- gsub("?", "u", x, ignore.case=TRUE)
  x <- gsub("?", "e", x, ignore.case=TRUE)
  x <- gsub("?", "e", x, ignore.case=TRUE)
  x <- gsub("?", "a", x, ignore.case=TRUE)
  x <- gsub("?", "e", x, ignore.case=TRUE)   
  x}
