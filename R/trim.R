#' Deleting leading and ending white spaces
#'
#' Deleting leading and ending white spaces from a \code{character} object.
#'
#' \code{tableTag} is an internal routine of \code{bibliometrics} package.
#'
#' @param x is a \code{character} object.

#' @return an object of class \code{character}
#' @examples
#'
#' char <- c("  Alfred", "Mary", " John")
#' char
#' trim(char)
#' 
#' @export
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}