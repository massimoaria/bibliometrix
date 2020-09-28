#' Open a bibliometrix network in VosViewer
#'
#' \code{net2VOSviewer} plots a network created with \code{\link{networkPlot}} using \href{https://www.vosviewer.com/}{VOSviewer} by Nees Jan van Eck and Ludo Waltman.
#'
#' The function \code{\link{networkPlot}} can plot a bibliographic network previously created by \code{\link{biblioNetwork}}.
#' The network map can be plotted using internal R routines or using \href{https://www.vosviewer.com/}{VOSviewer} by Nees Jan van Eck and Ludo Waltman.
#' 
#' 
#' @param net is an object created by networkPlot function.
#' @param vos.path is a character indicating the full path whre VOSviewer.jar is located.
#' @return It write a .net file that can be open in VOSviewer 
#' 
#' @examples
#' # EXAMPLE 
#' 
#' # VOSviewer.jar have to be present in the working folder
#'
#' # data(scientometrics)
#'
#' # NetMatrix <- biblioNetwork(scientometrics, analysis = "co-citation", 
#' # network = "references", sep = ";")
#' 
#' # net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Co-Citation",labelsize=0.5) 
#' 
#' # net2VOSviewer(net)
#' 
#' @seealso \code{\link{biblioNetwork}} to compute a bibliographic network.
#' @seealso \code{\link{networkPlot}} to create and plot a network object
#' 
#' @export


net2VOSviewer <- function(net, vos.path = NULL){
  
  net <- net$graph_pajek
  V(net)$id <- V(net)$name
  
  if (is.null(vos.path)) {
    vos.path <- getwd()
  }
  if (sum(dir(vos.path) %in% "VOSviewer.jar") == 0) {
    cat(
      paste(
        "VOSviewer.jar does not exist in the path",
        vos.path,
        "\n\nPlese download it from https://www.vosviewer.com/download",
        "\n(Java version for other systems)\n"
      )
    )
  }
  else{
    netfile <- paste(vos.path, "/", "vosnetwork.net", sep = "")
    VOScommand <- paste("java -jar ",
                        vos.path,
                        "/",
                        "VOSviewer.jar -pajek_network ",
                        netfile,
                        sep = "")
    write.graph(graph = net,
                file = netfile,
                format = "pajek")
    system(VOScommand, wait = FALSE)
  }

}