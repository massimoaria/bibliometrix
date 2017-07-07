#' Plotting historical co-citation network
#'
#' \code{histPlot} plots a historical co-citation network.
#'
#' The function \code{\link{histPlot}} can plot a historical co-citation network previously created by \code{\link{histNetwork}}.
#' @param histResults is an object of \code{class} "list" containing the following components:
#'
#' \tabular{lll}{
#' NetMatrix \tab  \tab the historical co-citation network matrix\cr
#' Degree \tab       \tab the min degree of the network\cr
#' histData \tab      \tab the set of n most cited references}
#' 
#' is a network matrix obtained by the function \code{\link{biblioNetwork}}. 
#' @param size is logical. If TRUE the point size of each vertex is proportional to its degree.
#' @param labelsize is an integer. It indicates the label size in the plot. Default is \code{labelsize=1}
#' @return It is a network object of the class \code{igraph}.
#' 
#' @examples
#' # EXAMPLE Co-citation network
#'
#' library(igraph)
#' data(scientometrics)
#'
#' histResults <- histNetwork(scientometrics, n = 20, sep = ";")
#' 
#' net <- histPlot(histResults, size = TRUE)
#' 
#' @seealso \code{\link{histNetwork}} to compute a historical co-citation network.
#' @seealso \code{\link{cocMatrix}} to compute a co-occurrence matrix.
#' @seealso \code{\link{biblioAnalysis}} to perform a bibliometric analysis.
#' 
#' @export
histPlot<-function(histResults, size = F, labelsize = 1){
  
  NET=histResults$NetMatrix
  
  # Create igraph object
  bsk.network <- graph.adjacency(NET,mode="undirected",weighted=TRUE)
  V(bsk.network)$id <- colnames(NET)
  
  # Compute node degrees (#links) and use that to set node size:
  deg <- degree(bsk.network, mode="all")
  if (isTRUE(size)){V(bsk.network)$size <- (deg/max(deg)[1])*20}
  else{V(bsk.network)$size=rep(5,length(V(bsk.network)))}
  
  # Remove loops
  bsk.network <- simplify(bsk.network, remove.multiple = T, remove.loops = T) 
  
  
  V(bsk.network)$color <- 'red'
  
  # delete not linked vertices
  #bsk.network <- delete.isolates(bsk.network, mode = 'in')
  
  # Choose Network layout
  l <- layout.fruchterman.reingold(bsk.network) #default
  l[,2]=histResults[[3]]$Year
  # Plot the chronological co-citation network
  plot(bsk.network,layout = l, vertex.label.dist = 0.5, vertex.frame.color = 'black', vertex.label.color = 'darkblue', vertex.label.font = 1, vertex.label = row.names(histResults[[3]]), vertex.label.cex = labelsize, edge.arrow.size=0.1, main="Historical co-citation network")
  cat("\n Legend\n\n")
  print(histResults[[3]])
  
  return(bsk.network)}

delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0) - 1
  delete.vertices(graph, isolates)
}