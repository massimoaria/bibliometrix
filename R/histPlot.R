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
#' histData \tab      \tab the set of n most cited references\cr
#' M \tab      \tab the bibliographic data frame}
#' 
#' is a network matrix obtained by the function \code{\link{histNetwork}}. 
#' @param n is integer. It defines the numebr of vertices to plot.
#' @param size.cex is logical. If TRUE the point size of each vertex is proportional to its degree. Default value is TRUE.
#' @param size is integer. It define the point size of the vertices. Default value is 5.
#' @param labelsize is an integer. It indicates the label size in the plot. Default is \code{labelsize=1}
#' @param arrowsize is numerical. It indicates the edge arrow size.
#' @return It is a network object of the class \code{igraph}.
#' 
#' @examples
#' # EXAMPLE Citation network
#'
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
histPlot<-function(histResults, n=20, size.cex=TRUE, size = 5, labelsize = 0.8,arrowsize=0.1){
  
  ## legacy with old argument size
  if (isTRUE(size)){
    size.cex=TRUE
    size=5
  }
  
  LCS=histResults$LCS
  NET=histResults$NetMatrix
  
  ## selecting the first n vertices
  s=sort(LCS,decreasing = TRUE)[n]
  ind=which(LCS>=s)
  NET=NET[ind,ind]
  LCS=LCS[ind]
  
  # Create igraph object
  bsk.network=graph_from_adjacency_matrix(NET, mode = c("directed"),weighted = NULL)
  
  R=strsplit(names(V(bsk.network)),",")
  RR=lapply(R,function(l){
      l=l[1:2]
      l=paste(l[1],l[2],sep=",")})
  V(bsk.network)$id <- unlist(RR)
  
  # Compute node degrees (#links) and use that to set node size:
  deg <- LCS
  if (isTRUE(size.cex)){V(bsk.network)$size <- (deg/max(deg)[1])*size}else{
    V(bsk.network)$size=rep(size,length(V(bsk.network)))}
  
  # Remove loops
  bsk.network <- simplify(bsk.network, remove.multiple = T, remove.loops = T) 
  
  
  
  #V(bsk.network)$color <- 'red'
  
  # define network layout
  Years=histResults$histData$Year[ind]
  L <- histLayout(NET,bsk.network,Years)
  l <- L$l
  bsk.network<- L$bsk.network
  
  # Plot the chronological co-citation network
  l=layout.norm(l)
  plot(bsk.network,rescale=T,asp=0,ylim=c(-1,1),xlim=c(-1,1),layout = l, vertex.color="lightblue", vertex.label.dist = 0.3, vertex.frame.color = 'black', vertex.label.color = 'black', vertex.label.font = 1, vertex.label = V(bsk.network)$id, vertex.label.cex = labelsize, edge.arrow.size=arrowsize, main="Historical citation network")
  cat("\n Legend\n\n")
  
  Data=histResults$histData
  Data=Data[ind,]
  print(Data)
  
  return(bsk.network)
}


### layout function
histLayout <- function(NET,bsk.network,Years){
  
  diag(NET)=0
  
  up=triu(NET)
  
  V(bsk.network)$cited=colSums(NET)-(rowSums(up)/max(NET))
  V(bsk.network)$citing=rowSums(NET)
  V(bsk.network)$year=Years
  V(bsk.network)$rank=rank(-V(bsk.network)$cited,ties="random")
  l=matrix(0,dim(NET)[1],2)
  l[,1]=V(bsk.network)$year
  l[,2]=V(bsk.network)$rank
  
  edges=get.edgelist(bsk.network)
  
  A=apply(NET,2,function(x){
    x[x>0]=sum(x)
    return(x)
  })
  edgesize=2
  E(bsk.network)$width=log(t(A)[t(A)>0],base=exp(1))*edgesize
  
  L=list(l=l,bsk.network=bsk.network)
  return(L)
}
