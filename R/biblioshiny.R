#' Shiny UI for bibliometrix package
#'
#' \code{biblioshiny} performs science mapping analysis using the main functions of the bibliometrix package.
#'
#' @param port is the TCP port that the application should listen on. If the port is not specified, 
#' and the shiny.port option is set (with options(shiny.port = XX)), then that port will be used. 
#' Otherwise, use a random port.
#' 
#' @param launch.browser If true, the system's default web browser will be launched automatically 
#' after the app is started. Defaults to true in interactive sessions only. This value of 
#' this parameter can also be a function to call with the application's URL.
#' 
#' @param host The IPv4 address that the application should listen on. 
#' Defaults to the shiny.host option, if set, or "127.0.0.1" if not.
#' 
#' @examples
#' 
#' #biblioshiny()
#'
#' 
#' @export

biblioshiny <- function(host = "127.0.0.1", port = NULL, launch.browser = TRUE){
  
  runApp(system.file("biblioshiny",package="bibliometrix"),launch.browser = launch.browser, port = port, host = getOption("shiny.host", host))
}
