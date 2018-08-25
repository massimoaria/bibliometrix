#' Shiny UI for bibliometrix package
#'
#' \code{biblioshiny} performs science mapping analysis using the main functions of the bibliometrix package.
#'
#' @examples
#' 
#' #biblioshiny()
#'
#' 
#' @export

biblioshiny <- function(){
  runApp(system.file("biblioshiny",package="bibliometrix"),launch.browser = TRUE)
}