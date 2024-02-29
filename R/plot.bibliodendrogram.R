#' Plotting dendrogram resulting from Conceptual Structure Analysis
#'
#' \code{plot} method for class '\code{bibliodendrogram}'
#' @param x is the object for which plots are desired.
#' @param ... is a generic param for plot functions.
#' @return The function \code{plot} draws a dendrogram. 
#' 
#'
#' @method plot bibliodendrogram
#' @export


plot.bibliodendrogram <- function(x, ...){
  plot(x$dend)
  abline(h =x$line, lty=2)
}

