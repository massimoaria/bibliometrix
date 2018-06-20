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
#' @param size is an integer. It define the point size of the vertices. Default value is 5.
#' @param labelsize is an integer. It indicates the label size in the plot. Default is \code{labelsize=1}
#' @param arrowsize is numerical. It indicates the edge arrow size.
#' @param edgesize is numerical. It indicates the edge size.
#' @param color is logical. If TRUE, egdes are colored according to citing references.
#' @return It is a network object of the class \code{igraph}.
#' 
#' @examples
#' # EXAMPLE Citation network
#'
#' data(scientometrics)
#'
#' histResults <- histNetwork(scientometrics, sep = ";")
#' 
#' net <- histPlot(histResults, n=20, size.cex=TRUE, size = 5, arrowsize=0.3)
#' 
#' @seealso \code{\link{histNetwork}} to compute a historical co-citation network.
#' @seealso \code{\link{cocMatrix}} to compute a co-occurrence matrix.
#' @seealso \code{\link{biblioAnalysis}} to perform a bibliometric analysis.
#' 
#' @export
histPlot<-function(histResults, n=20, size.cex=TRUE, size = 5, labelsize = 0.8,arrowsize=0.1, edgesize=2, color=TRUE){
  
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
  bsk.network <- histLayout(NET,bsk.network,Years,color=color,edgesize=edgesize)
  
  layout_m <- create_layout(bsk.network, layout = 'nicely')#, algorithm = 'kk')
  layout_m$x=Years
  
  g=ggraph(layout_m) +
    geom_edge_arc(aes(width = 0.2, color=as.factor(E(bsk.network)$color)), curvature = 0.1, 
          check_overlap = T, edge_alpha = 0.2, 
          arrow = grid::arrow(angle = 10, unit(0.3, "inches")))+
    scale_edge_colour_discrete(as.factor(E(bsk.network)$color))+
    geom_node_text(aes(label=V(bsk.network)$id, size=labelsize),repel = TRUE)+
    geom_node_point(aes(size = V(bsk.network)$size,color = "grey", alpha=0.1))+
    scale_color_viridis(discrete = TRUE) +
    theme_minimal()+
    theme(legend.position='none', panel.background = element_rect(fill='gray97', color='grey97'),
          axis.line.y = element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(),
          axis.title.y=element_blank(), axis.title.x=element_blank(),
          panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())+
    labs(title = "Historical Direct Citation Network")
  
  plot(g)
  
  
    # Plot the chronological co-citation network
 # l=layout.norm(l)
 # plot(bsk.network,rescale=T,asp=0,ylim=c(-1,1),xlim=c(-1,1),layout = l, vertex.color="lightblue", 
 #      vertex.label.dist = 0.3, vertex.frame.color = 'black', vertex.label.color = 'black', 
 #      vertex.label.font = 1, vertex.label = V(bsk.network)$id, vertex.label.cex = labelsize, 
 #      edge.arrow.size=arrowsize, main="Historical Direct Citation Network",edge.curved=T)
  
  cat("\n Legend\n\n")
  
  Data=histResults$histData
  Data=Data[ind,]
  print(Data)
  
  return(bsk.network)
}


### layout function
histLayout <- function(NET,bsk.network,Years,color=color,edgesize=edgesize){
  
  diag(NET)=0
  
  up=triu(NET)
  
  V(bsk.network)$cited=colSums(NET)-(rowSums(up)/max(NET))
  V(bsk.network)$citing=rowSums(NET)
  V(bsk.network)$year=Years
  
  edges=get.edgelist(bsk.network)
  
  A=apply(NET,2,function(x){
    x[x>0]=sum(x)
    return(x)
  })
   E(bsk.network)$width=log(t(A)[t(A)>0],base=exp(1))*edgesize
  
  ### color
  if (isTRUE(color)){
  B=as.matrix(NET)
  for (i in 1:dim(NET)[2]){
    ind=which(B[,i]>0)
    if (length(ind)>0){B[ind,i]=i}
  }
  
  E(bsk.network)$color=suppressWarnings(t(B)[t(B)>0])
  }else{E(bsk.network)$color="#E8E8E8"}
  
 
  return(bsk.network)
}
