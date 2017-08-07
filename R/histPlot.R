#' Plotting historical co-citation network
#'
#' \code{histPlot} plots a historical co-citation network.
#'
#' The function \code{\link{histPlot}} can plot a historical co-citation network previously created by \code{\link{histNetwork}}.
#' @param histResults is an object of \code{class} "list" containing the following components:
#'
#' \tabular{lll}{
#' NetMatrix \tab  \tab the historical citation network matrix\cr
#' Degree \tab       \tab the min degree of the network\cr
#' histData \tab      \tab the set of n most cited references
#' M \tab      \tab the bibliographic data frame}
#' 
#' is a network matrix obtained by the function \code{\link{histNetwork}}. 
#' @param remove.isolates is logical. If TRUE isolates vertices are not plotted.
#' @param size is logical. If TRUE the point size of each vertex is proportional to its degree.
#' @param labelsize is an integer. It indicates the label size in the plot. Default is \code{labelsize=1}
#' @param label is logical. If TRUE vertex labels are plotted. 
#' @param arrowsize is numerical. It indicates the edge arrow size.
#' @return It is a network object of the class \code{igraph}.
#' 
#' @examples
#' # EXAMPLE Citation network
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
histPlot<-function(histResults, remove.isolates=FALSE, size = F, labelsize = 0.8,label=TRUE,arrowsize=0.1){
  
  NET=histResults$NetMatrix
  
  # Create igraph object
  bsk.network=graph_from_adjacency_matrix(NET, mode = c("directed"),weighted = NULL)
  
  if (isTRUE(label)){
    R=strsplit(row.names(NET),",")
    RR=lapply(R,function(l){
      l=l[1:2]
      l=paste(l[1],l[2],sep=",")})
    V(bsk.network)$id <- unlist(RR)}else{V(bsk.network)$id=row.names(histResults[[3]])}
  
  # Compute node degrees (#links) and use that to set node size:
  deg <- histResults$LCS
  if (isTRUE(size)){V(bsk.network)$size <- (deg/max(deg)[1])*20}
  else{V(bsk.network)$size=rep(3,length(V(bsk.network)))}
  
  # Remove loops
  bsk.network <- simplify(bsk.network, remove.multiple = T, remove.loops = T) 
  
  
  #V(bsk.network)$color <- 'red'
  
    # Choose Network layout
  if (!isTRUE(remove.isolates)){
  l <- layout.fruchterman.reingold(bsk.network) #default
  l[,2]=(histResults[[3]]$Year)*-1
  
  edges1=colSums(NET)
  edges2=rowSums(NET)
  ind=which((edges1==0) & (edges2==0))
  ma=max(l[-ind,1])
  mi=min(l[-ind,1])
  l[ind,1]=sample(seq(ma,ma+((ma-mi)/3),length.out = length(ind)),size=length(ind))}
  else{
    l <- layout.fruchterman.reingold(bsk.network) #default
    l[,2]=(histResults[[3]]$Year)*-1
    edges1=colSums(NET)
    edges2=rowSums(NET)
    ind=which((edges1==0) & (edges2==0))
    bsk.network=delete.vertices(bsk.network, ind)
    l=l[-ind,]
    
  }
  l[,1]=l[,1]*2
  # Plot the chronological co-citation network
  plot(bsk.network,layout = l, vertex.color="lightblue", vertex.label.dist = 0.3, vertex.frame.color = 'black', vertex.label.color = 'darkblue', vertex.label.font = 1, vertex.label = V(bsk.network)$id, vertex.label.cex = labelsize, edge.arrow.size=arrowsize, main="Historical citation network")
  cat("\n Legend\n\n")
  print(histResults[[3]])
  
  return(bsk.network)
  }
  