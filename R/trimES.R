#' Deleting extra white spaces
#'
#' Deleting extra white spaces from a \code{character} object.
#'
#' \code{tableTag} is an internal routine of \code{bibliometrics} package.
#'
#' @param x is a \code{character} object.

#' @return an object of class \code{character}
#' @examples
#'
#' char <- c("Alfred  BJ", "Mary    Beth", "John      John")
#' char
#' trimES(char)
#' 
#' @export
trimES <- function( x ) {
  gsub("\\s+", " ", x)
}