#' Deleting leading white spaces
#'
#' Deleting leading white spaces from a \code{character} object.
#'
#' \code{tableTag} is an internal routine of \code{bibliometrics} package.
#'
#' @param x is a \code{character} object.

#' @return an object of class \code{character}
#' @examples
#'
#' char <- c("  Alfred", "Mary", " John")
#' char
#' trim.leading(char)
#' 
#' @export
trim.leading <- function (x){

sub("^\\s+", "", x)  ## function to delete leading spaces in a string
}
