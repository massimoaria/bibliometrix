#' Plotting Bibliographic networks
#'
#' \code{networkPlot} plots a bibliographic network.
#'
#' The function \code{\link{networkPlot}} can plot a bibliographic network previously created by \code{\link{biblioNetwork}}.
#' The network map can be plotted using internal R routines or using \href{http://www.vosviewer.com/}{VOSviewer} by Nees Jan van Eck and Ludo Waltman.
#' @param NetMatrix is a network matrix obtained by the function \code{\link{biblioNetwork}}. 
#' @param n is an integer. It indicates the number of vertices to plot.
#' @param type is a character object. It indicates the network map layout:
#' 
#' \tabular{lll}{
#' \code{type="circle"}\tab   \tab Circle layout\cr
#' \code{type="sphere"}\tab   \tab Sphere layout\cr
#' \code{type="mds"}\tab   \tab Multidimensional Scaling layout\cr
#' \code{type="fruchterman"}\tab   \tab Fruchterman-Reingold layout\cr
#' \code{type="kamada"}\tab   \tab  Kamada-Kawai layout\cr
#' \code{type="vosviewer"}\tab   \tab Network is plotted using VOSviewer software\cr}
#' 
#' @param Title is a character indicating the plot title. 
#' @param vos.path is a character indicating the full path whre VOSviewer.jar is located.
#' @param size is logical. If TRUE the point size of each vertex is proportional to its degree. 
#' @param noloops is logical. If TRUE loops in the network are deleted.
#' @param remove.multiple is logical. If TRUE multiple links are plotted using just one edge.
#' @param labelsize is an integer. It indicates the label size in the plot. Default is \code{labelsize=1}
#' @return It is a network object of the class \code{igraph}.
#' 
#' @examples
#' # EXAMPLE Co-citation network
#'
#' data(scientometrics)
#'
#' NetMatrix <- biblioNetwork(scientometrics, analysis = "co-citation", 
#' network = "references", sep = ";")
#' 
#' net <- networkPlot(NetMatrix, n = 20, type = "kamada", Title = "Co-Citation")
#' 
#' @seealso \code{\link{biblioNetwork}} to compute a bibliographic network.
#' @seealso \code{\link{cocMatrix}} to compute a co-occurrence matrix.
#' @seealso \code{\link{biblioAnalysis}} to perform a bibliometric analysis.
#' 
#' @export
networkPlot<-function(NetMatrix, n=20,Title="Plot", type="kamada", labelsize=1, vos.path=NULL, size=FALSE, noloops=TRUE, remove.multiple=TRUE){

NET=NetMatrix

# Create igraph object
bsk.network <- graph.adjacency(NET,mode="undirected")
V(bsk.network)$id <- colnames(NET)

# Compute node degrees (#links) and use that to set node size:
deg <- degree(bsk.network, mode="all")
if (isTRUE(size)){V(bsk.network)$size <- (deg/max(deg)[1])*20}
else{V(bsk.network)$size=rep(5,length(V(bsk.network)))}

# Select number of vertices to plot
if (n>dim(NET)[1]) {n <- dim(NET)[1]}
NetDegree <- unname(sort(deg,decreasing=TRUE)[n])
bsk.network <- delete.vertices(bsk.network,which(degree(bsk.network)<NetDegree))

# Remove loops and multiple edges
bsk.network <- simplify(bsk.network, remove.multiple = remove.multiple, remove.loops = noloops) 

# delete not linked vertices
#bsk.network <- delete.isolates(bsk.network, mode = 'in')

# Choose Network layout
l <- layout.kamada.kawai(bsk.network) #default
switch(type,
       circle={l <- layout.circle(bsk.network)},
       sphere={l <- layout.sphere(bsk.network)},
       mds={l <- layout.mds(bsk.network)},
       fruchterman={l <- layout.fruchterman.reingold(bsk.network)},
       kamada={l <- layout.kamada.kawai(bsk.network)},
       vosviewer={
         if (is.null(vos.path)){vos.path=getwd()}
         if (sum(dir(vos.path) %in% "VOSviewer.jar")==0){cat(paste("VOSviewer.jar does not exist in the path",vos.path,"\n\nPlese download it from http://www.vosviewer.com/download","\n(Java version for other systems)\n"))}
         else{
         netfile=paste(vos.path,"/","vosnetwork.net",sep="")
         VOScommand=paste("java -jar ",vos.path,"/","VOSviewer.jar -pajek_network ",netfile,sep="")
         write.graph(graph = bsk.network, file = netfile, format = "pajek")
         system(VOScommand)}
         })



if (type!="vosviewer"){
  
  net_groups <- cluster_walktrap(bsk.network)
  V(bsk.network)$color <- brewer.pal(12, 'Set3')[membership(net_groups)]
## Plot the network
plot(bsk.network,layout = l, vertex.label.dist = 0.5, vertex.frame.color = 'black', vertex.label.color = 'black', vertex.label.font = 1, vertex.label = V(bsk.network)$name, vertex.label.cex = labelsize, main=Title)}

return(bsk.network)}

delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0) - 1
  delete.vertices(graph, names(isolates))
  }

