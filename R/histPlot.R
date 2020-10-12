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
#' @param n is integer. It defines the number of vertices to plot.
#' @param size is an integer. It defines the point size of the vertices. Default value is 5.
#' @param labelsize is an integer. It indicates the label size in the plot. Default is \code{labelsize=5}
#' @param title_as_label is a logical. If TRUE document titles are plotted instead of short labels.
#' @param verbose is logical. If TRUE, results and plots are printed on screen.
#' @return It is list containing: a network object of the class \code{igraph} and a plot object of the class \code{ggraph}.
#' 
#' @examples
#' # EXAMPLE Citation network
#'
#' data(management)
#'
#' histResults <- histNetwork(management, sep = ";")
#' 
#' net <- histPlot(histResults, n=20, labelsize = 5)
#' 
#' @seealso \code{\link{histNetwork}} to compute a historical co-citation network.
#' @seealso \code{\link{cocMatrix}} to compute a co-occurrence matrix.
#' @seealso \code{\link{biblioAnalysis}} to perform a bibliometric analysis.
#' 
#' @export
histPlot<-function(histResults, n=20, size = 5, labelsize = 5, title_as_label = FALSE, verbose = TRUE){
  
  colorlist <-  c(brewer.pal(9, 'Set1')[-6], brewer.pal(8, 'Set2')[-7], brewer.pal(12, 'Paired')[-11],brewer.pal(12, 'Set3')[-c(2,8,12)])
  ## legacy with old argument size
  if (isTRUE(size)){
    size <- 5
  }
  
  #LCS=histResults$LCS
  LCS <- colSums(histResults$NetMatrix)
  NET <- histResults$NetMatrix
  ## selecting the first n vertices or all if smaller
  s=sort(LCS,decreasing = TRUE)[min(n, length(LCS))]
  ind=which(LCS>=s)
  NET=NET[ind,ind]
  LCS=LCS[ind]
  
  # Create igraph object
  bsk.network <- graph_from_adjacency_matrix(NET, mode = c("directed"),weighted = NULL)
  
  R <- strsplit(names(V(bsk.network)),",")
  RR <- lapply(R,function(l){
    l=l[1:2]
    l=paste(l[1],l[2],sep=",")})
  
  # add titles
  V(bsk.network)$title <- histResults$histData$Title[ind]
  
  if (isTRUE(title_as_label)){
    V(bsk.network)$id <- tolower(paste(substr(V(bsk.network)$title,1,50),"...",sep=""))
  } else {
    V(bsk.network)$id <- tolower(unlist(RR))
  }
  
  # Compute node degrees (#links) and use that to set node size:
  deg <- LCS
  V(bsk.network)$size <- size
    #rep(size,length(V(bsk.network)))}
  
  #Years=histResults$histData$Year[ind]
  Years <- as.numeric(unlist(str_extract_all(unlist(RR),"[[:digit:]]{4}$")))
  V(bsk.network)$years <- Years
  
  # Remove loops
  bsk.network <- simplify(bsk.network, remove.multiple = T, remove.loops = T) 
  
  
  
  #V(bsk.network)$color <- 'red'
  
  # define network layout
  
  E(bsk.network)$color <- "slategray1"
  
  bsk.network <- delete.isolates(bsk.network)
  dg <- decompose.graph(bsk.network)

  layout_m <- create_layout(bsk.network, layout = 'nicely')
  layout_m$cluster <- 0
  rr <- 0
  for (k in 1:length(dg)){
    bsk <- dg[[k]]
    
    a <- ifelse(layout_m$name %in% V(bsk)$name,k,0)
    layout_m$cluster <- layout_m$cluster+a
    Min <- min(layout_m$y[layout_m$cluster==k])-1
    layout_m$y[layout_m$cluster==k] <- layout_m$y[layout_m$cluster==k]+(rr-Min)
    rr <- max(layout_m$y[layout_m$cluster==k])
  }
  bsk <- bsk.network
  wp <- membership(cluster_infomap(bsk,modularity = FALSE))
  layout_m$x <- layout_m$years
  
  layout_m$y <- (diff(range(layout_m$x))/diff(range(layout_m$y)))*layout_m$y

  
  g <- ggraph(layout_m) +
    geom_edge_arc(width = 1, strength = 0.05, 
          check_overlap = T, edge_alpha = 0.5, color="grey",
          arrow = grid::arrow(angle = 10, unit(0.15, "inches")))+
    geom_node_text(aes(label=V(bsk)$id), size=labelsize,repel = TRUE, color=colorlist[wp],alpha=0.8)+
    geom_node_point(size = V(bsk)$size,color = "royalblue4", alpha=0.15)+
    scale_color_brewer() +
    scale_x_continuous(labels=as.character(seq(min(Years),max(Years))),breaks=seq(min(Years),max(Years)))+
    theme_minimal()+
    theme(legend.position='none', panel.background = element_rect(fill='gray97', color='grey97'),
          axis.line.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y=element_blank(),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(), axis.text.x=element_text(face="bold", angle = 90, size=6)) +
    labs(title = "Historical Direct Citation Network")
  
  

  
  
  if (isTRUE(verbose)) {
    plot(g)
    
    cat("\n Legend\n\n")
    
    label <- data.frame(Label = names(V(bsk.network)), stringsAsFactors = FALSE)
    Data <-  histResults$histData
    
    Data <- left_join(label,Data, by = c("Label" = "Paper"))
    
    print(Data[,-2])
  }
  
  results <- list(net=bsk.network, g=g)
  return(results)
}


# ### layout function
# histLayout <- function(NET,bsk.network,Years,edgesize=edgesize){
#   
#   diag(NET)=0
#   
#   up=triu(NET)
# 
#   A=apply(NET,2,function(x){
#     x[x>0]=sum(x)
#     return(x)
#   })
#    E(bsk.network)$width=log(t(A)[t(A)>0],base=exp(1))*edgesize
#   
#    return(bsk.network)
# }

delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0) - 1
  delete.vertices(graph, names(isolates))
}
