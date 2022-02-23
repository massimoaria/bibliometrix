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
#' @param maxUploadSize is a integer. The max upload file size argument. Default value is 200 (megabyte)
#' @param theme.type is a character. The argument indicates one of the bootstrap 5 themes included in bslib package.
#' @param font.type is a character. The argument indicates one of the Google fonts you can use in Biblioshiny.
#' @param font.size is a numeric. The argument indicates the font size. Default value is 1.
#' 
#' @examples
#' 
#' #biblioshiny()
#'
#' 
#' @export

biblioshiny <- function(host = "127.0.0.1", port = NULL, 
                        launch.browser = TRUE, maxUploadSize=200,
                        theme.type = "yeti",
                        font.type = "Jost", font.size = 1.05){
  
  shinyOptions(maxUploadSize = maxUploadSize,
               theme.type = theme.type,
               font.type = font.type,
               font.size = font.size)
  
  runApp(system.file("biblioshiny",package="bibliometrix"),launch.browser = launch.browser, port = port, host = getOption("shiny.host", host))
}
