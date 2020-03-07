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

biblioshiny <- function(port=8080, browser=TRUE){
  
  runApp(system.file("biblioshiny",package="bibliometrix"),port = port,launch.browser = browser)
}
