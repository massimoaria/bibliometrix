#' Plotting Bibliographic networks
#'
#' \code{networkPlot} plots a bibliographic network.
#'
#' The function \code{\link{networkPlot}} can plot a bibliographic network previously created by \code{\link{biblioNetwork}}.
#' The network map can be plotted using internal R routines or using \href{http://www.vosviewer.com/}{VOSviewer} by Nees Jan van Eck and Ludo Waltman.
#' @param NetMatrix is a network matrix obtained by the function \code{\link{biblioNetwork}}. 
#' @param normalize is a character. It can be "association", "jaccard", "inclusion","salton" or "equivalence" to obtain Association Strength, Jaccard, 
#' Inclusion, Salton or Equivalence similarity index respectively. The default is type = NULL.
#' @param n is an integer. It indicates the number of vertices to plot.
#' @param degree is an integer. It idicates the min frequency of a vertex. If degree is not NULL, n is ignored.
#' @param type is a character object. It indicates the network map layout:
#' 
#' \tabular{lll}{
#' \code{type="auto"}\tab   \tab Automatic layout selection\cr
#' \code{type="circle"}\tab   \tab Circle layout\cr
#' \code{type="sphere"}\tab   \tab Sphere layout\cr
#' \code{type="mds"}\tab   \tab Multidimensional Scaling layout\cr
#' \code{type="fruchterman"}\tab   \tab Fruchterman-Reingold layout\cr
#' \code{type="kamada"}\tab   \tab  Kamada-Kawai layout\cr
#' \code{type="vosviewer"}\tab   \tab Network is plotted using VOSviewer software\cr}
#' 
#' @param Title is a character indicating the plot title. 
#' @param vos.path is a character indicating the full path whre VOSviewer.jar is located.
#' @param size is integer. It defines the size of each vertex. Default is \code{size=3}. 
#' @param size.cex is logical. If TRUE the size of each vertex is proportional to its degree.  
#' @param noloops is logical. If TRUE loops in the network are deleted.
#' @param remove.isolates is logical. If TRUE isolates vertices are not plotted.
#' @param remove.multiple is logical. If TRUE multiple links are plotted using just one edge.
#' @param label is logical. If TRUE vertex labels are plotted.
#' @param labelsize is an integer. It indicates the label size in the plot. Default is \code{labelsize=1}
#' @param label.color is logical. If TRUE, for each vertex, the label color is the same as its cluster. 
#' @param label.cex is logical. If TRUE the label size of each vertex is proportional to its degree.  
#' @param halo is logical. If TRUE communities are plotted using different colors. Default is \code{halo=FALSE}
#' @param cluster is a character. It indicates the type of cluster to perform among ("none", optimal", "lovain","infomap","edge_betweenness","walktrap").
#' @param curved is a logical or a number. If TRUE edges are plotted with an optimal curvature. Default is \code{curved=FALSE}. Curved values are any numbers from 0 to 1.
#' @param weighted This argument specifies whether to create a weighted graph from an adjacency matrix. 
#' If it is NULL then an unweighted graph is created and the elements of the adjacency matrix gives the number of edges between the vertices. 
#' If it is a character constant then for every non-zero matrix entry an edge is created and the value of the entry is added as an edge attribute 
#' named by the weighted argument. If it is TRUE then a weighted graph is created and the name of the edge attribute will be weight.
#' @param edgesize is an integer. It indicates the network edge size.
#' @param edges.min is an integer. It indicates the min frequency of edges between two vertices. If edge.min=0, all edges are plotted.
#' @param label.n is an integer. It indicates the number of vertex labels to draw.
#' @param alpha is a number. Legal alpha values are any numbers from 0 (transparent) to 1 (opaque). The default alpha value usually is 0.5.
#' @return It is a list containing the following elements:
#' \tabular{lll}{
#' \code{graph} \tab  \tab a network object of the class \code{igraph}\cr
#' \code{cluster_obj} \tab  \tab a \code{\link{communities}} object of the package \code{igraph}\cr
#' \code{cluster_res} \tab  \tab a data frame with main results of clustering procedure.\cr}
#' 
#' 
#' @examples
#' # EXAMPLE Co-citation network
#'
#' data(scientometrics)
#'
#' NetMatrix <- biblioNetwork(scientometrics, analysis = "co-citation", 
#' network = "references", sep = ";")
#' 
#' net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Co-Citation",labelsize=0.5) 
#' 
#' @seealso \code{\link{biblioNetwork}} to compute a bibliographic network.
#' @seealso \code{\link{cocMatrix}} to compute a co-occurrence matrix.
#' @seealso \code{\link{biblioAnalysis}} to perform a bibliometric analysis.
#' 
#' @export
networkPlot<-function(NetMatrix, normalize=NULL, n=NULL, degree=NULL, Title="Plot", type="kamada", label=TRUE, labelsize=1, label.cex=FALSE, label.color=FALSE, label.n=NULL, halo=FALSE, cluster="walktrap", vos.path=NULL, size=3, size.cex=FALSE, curved=FALSE, noloops=TRUE, remove.multiple=TRUE,remove.isolates=FALSE,weighted=NULL,edgesize=1,edges.min=0,alpha=0.5){
  
  NET=NetMatrix
  bsk.S=TRUE
  l=NA
  net_groups=list()
  if (type=="vosviewer"){cluster="none"}
  
  if (!is.null(normalize)){
    S=normalizeSimilarity(NetMatrix, type = normalize)
    bsk.S <- graph.adjacency(S,mode="undirected",weighted=T)
  }
  
  ## legacy with version <1.9.4
  if (isTRUE(size)){
    size=20
    size.cex=T
  }
  
  if (alpha<0 & alpha>1){alpha=0.5}
  
  # Create igraph object
  bsk.network <- graph.adjacency(NET,mode="undirected",weighted=weighted)
  
  
  # vertex labels 
  V(bsk.network)$name <- colnames(NET)
  
  
  # Compute node degrees (#links) and use that to set node size:
  deg <- degree(bsk.network, mode="all")
  V(bsk.network)$deg<-deg
  if (isTRUE(size.cex)){V(bsk.network)$size <- (deg/max(deg)[1])*size}else{
    V(bsk.network)$size=rep(size,length(V(bsk.network)))}
  
  # label size
  if (isTRUE(label.cex)){
    V(bsk.network)$label.cex <- log(1+(deg/max(deg)[1])*labelsize)}else{
      V(bsk.network)$label.cex <- labelsize}
  
 # Select number of vertices to plot
 if (!is.null(degree)){
    Deg=deg-diag(NET)
    Vind=Deg<degree
    if (sum(!Vind)==0){cat("\ndegree argument is to high!\n\n")
      return()}
    bsk.network <- delete.vertices(bsk.network,which(Vind))
    if (!isTRUE(bsk.S)){bsk.S <- delete.vertices(bsk.S,which(Vind))}
  } else { 
    if (n>dim(NET)[1]) {n <- dim(NET)[1]}
    NetDegree <- unname(sort(deg,decreasing=TRUE)[n])
    bsk.network <- delete.vertices(bsk.network,which(deg<NetDegree))
    if (!isTRUE(bsk.S)){bsk.S <- delete.vertices(bsk.S,which(deg<NetDegree))}
  }
  
  # Remove loops and multiple edges
  if (edges.min>1){remove.multiple=FALSE}
  bsk.network <- simplify(bsk.network, remove.multiple = remove.multiple, remove.loops = noloops) 
  if (!isTRUE(bsk.S)){bsk.S <- simplify(bsk.S, remove.multiple = remove.multiple, remove.loops = noloops)}
  
  # delete not linked vertices
  if (isTRUE(remove.isolates)){
    bsk.network <- delete.isolates(bsk.network, mode = 'all')
    if (!isTRUE(bsk.S)){bsk.S <- delete.isolates(bsk.S, mode = 'all')}
  }
  
    # Choose Network layout
  if (!isTRUE(bsk.S)){
    l <- switchLayout(bsk.S,type,vos.path)
  } else{
    l <- switchLayout(bsk.network,type,vos.path)
    }
  
  ### graph to write in pajek format ###
  bsk.save=bsk.network
  V(bsk.save)$id=V(bsk.save)$name
  
  ### 
  
  # Clustering
  if (type!="vosviewer"){
    
    cl <- clusteringNetwork(bsk.network,cluster)
    
    bsk.network <- cl$bsk.network
    if (!isTRUE(bsk.S)){V(bsk.S)$color=V(bsk.network)$color}
    net_groups <- cl$net_groups
    
    ## Labelling the network
    LABEL=""
    if (isTRUE(label)){
      LABEL=V(bsk.network)$name
      if (!is.null(label.n)){
        q=1-(label.n/length(V(bsk.network)$deg))
        if (q>1){q=1}
        if (q<0){q=0}
        q=quantile(V(bsk.network)$deg,q)
        LABEL[V(bsk.network)$deg<q]=""
      }
    }
    
    
    ## Edge size
    E(bsk.network)$num=E(bsk.save)$num=count_multiple(bsk.network, eids = E(bsk.network))
    if (is.null(weighted)){E(bsk.save)$weight=E(bsk.save)$num}
    
    if (!is.null(weighted)){
      E(bsk.network)$width <- (E(bsk.network)$weight + min(E(bsk.network)$weight))/max(E(bsk.network)$weight + min(E(bsk.network)$weight)) *edgesize
    } else{
      if(isTRUE(remove.multiple)){E(bsk.network)$width=edgesize} 
      else{
        edges=E(bsk.network)$num
        E(bsk.network)$width=edges/max(edges)*edgesize}
    }
    
    bsk.network1=delete.edges(bsk.network, which(E(bsk.network)$num<edges.min))
    
    if (isTRUE(label.color)){
      lab.color=V(bsk.network)$color
    }else{lab.color="black"}
    
    
    ## Plot the network
    l=layout.norm(l)
    
    if (isTRUE(halo) & cluster!="none"){
      plot(net_groups,bsk.network1, rescale=T, asp=0, ylim=c(-1,1), xlim=c(-1,1), layout = l, edge.curved=curved, 
           vertex.label.dist = 0.7, vertex.frame.color = adjustcolor('black',alpha), vertex.label.color = adjustcolor('black',min(c(1,alpha+0.1))),
           vertex.color=adjustcolor(V(bsk.network1)$color,alpha),
           vertex.label.font = 2, vertex.label = tolower(LABEL), main=Title)
      
    }else{
      plot(bsk.network1, rescale=T, asp=0, ylim=c(-1,1), xlim=c(-1,1), layout = l, edge.curved=curved, 
           vertex.label.dist = 0.7, vertex.frame.color = adjustcolor('black',alpha), 
           vertex.color=adjustcolor(V(bsk.network1)$color,alpha),vertex.label.color = adjustcolor(lab.color, min(c(1,alpha+0.2))), 
           vertex.label.font = 2, vertex.label = tolower(LABEL), main=Title, edge.color=adjustcolor(E(bsk.network1)$color,alpha/2))
    }
    
  }else{net_groups$modularity=rep(1,vcount(bsk.network))} 
  
  ## Output
  if (cluster!="none" & type!="vosviewer"){
    cluster_res=data.frame(net_groups$names,net_groups$membership,as.numeric(betweenness(bsk.network,directed = F,normalized = F)))
    names(cluster_res)=c("vertex","cluster","btw_centrality")
    cluster_res=cluster_res[order(cluster_res$cluster),]
  } else {cluster_res=NA}
  
  
  net=list(graph=bsk.network, graph_pajek=bsk.save, cluster_obj=net_groups, cluster_res=cluster_res,layout=l)
  
  return(net)}






### internal functions:

### deleteIsolates

delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0) - 1
  delete.vertices(graph, names(isolates))
}

### clusteringNetwork

clusteringNetwork <- function(bsk.network,cluster){
  colorlist= c(brewer.pal(9, 'Set1'), brewer.pal(8, 'Set2'),brewer.pal(12, 'Set3'),brewer.pal(12, 'Paired'))
  
  switch(cluster,
         none={
           
           net_groups=list(membership=rep(1,vcount(bsk.network)))},
         optimal={
           net_groups <- cluster_optimal(bsk.network)},
         louvain={
           net_groups <- cluster_louvain(bsk.network)},
         infomap={
           net_groups <- cluster_infomap(bsk.network)},
         edge_betweenness={
           net_groups <- cluster_edge_betweenness(bsk.network)},
         walktrap={
           net_groups <- cluster_walktrap(bsk.network)},
         
         ## default statement
         {cat("\nUnknown cluster argument. Using default algorithm\n")
         net_groups <- cluster_walktrap(bsk.network)}
  )
  
  V(bsk.network)$color <- colorlist[net_groups$membership]
  ### set egde intra-class colors
  V(bsk.network)$community <- net_groups$membership
  El=as.data.frame(get.edgelist(bsk.network,names=F))
  
  
  E(bsk.network)$color <- apply(El, 1, function(x){
                        colorlist= c(brewer.pal(9, 'Set1'), brewer.pal(8, 'Set2'),brewer.pal(12, 'Set3'),brewer.pal(12, 'Paired'))
                        if (V(bsk.network)$community[x[1]] == V(bsk.network)$community[x[2]]){
                          C=colorlist[V(bsk.network)$community[x[1]]]
                        }else{C='#E8E8E8'}
                        return(C)
                        })
  ### end
  
  cl=list()
  cl$bsk.network=bsk.network
  cl$net_groups=net_groups
  return(cl)
}

### switchLayout

switchLayout <- function(bsk.network,type,vos.path){
  switch(type,
         auto={l <- layout.auto(bsk.network)},
         circle={l <- layout.circle(bsk.network)},
         star={l <- layout.star(bsk.network)},
         sphere={l <- layout.sphere(bsk.network)},
         mds={l <- layout.mds(bsk.network)},
         fruchterman={l <- layout.fruchterman.reingold(bsk.network)},
         kamada={l <- layout.kamada.kawai(bsk.network)},
         vosviewer={
           V(bsk.network)$id=V(bsk.network)$name
           if (is.null(vos.path)){vos.path=getwd()}
           if (sum(dir(vos.path) %in% "VOSviewer.jar")==0){cat(paste("VOSviewer.jar does not exist in the path",vos.path,"\n\nPlese download it from http://www.vosviewer.com/download","\n(Java version for other systems)\n"))}
           else{
             netfile=paste(vos.path,"/","vosnetwork.net",sep="")
             VOScommand=paste("java -jar ",vos.path,"/","VOSviewer.jar -pajek_network ",netfile,sep="")
             write.graph(graph = bsk.network, file = netfile, format = "pajek")
             system(VOScommand,wait=FALSE)}
         })
  
  if (type!="vosviewer"){l=layout.norm(l)}else{l=NA}
  return(l)
}

### shortlabel
labelShort <- function(NET,db="isi"){
  LABEL<-colnames(NET)
  YEAR=suppressWarnings(as.numeric(sub('.*(\\d{4}).*', '\\1', LABEL)))
  YEAR[is.na(YEAR)]=""
  switch(db,
         isi={
           AU=strsplit(LABEL," ")
           AU=unlist(lapply(AU, function(l){paste(l[1]," ",l[2],sep="")}))
           LABEL=paste0(AU, " ", YEAR, sep="")
         },
         scopus={
           AU=strsplit(LABEL,"\\. ")
           AU=unlist(lapply(AU, function(l){l[1]}))
           LABEL=paste0(AU, ". ", YEAR, sep="")
         })
  
  ## assign an unique name to each label
  tab=sort(table(LABEL),decreasing=T)
  dup=names(tab[tab>1])
  for (i in 1:length(dup)){
    ind=which(LABEL %in% dup[i])
    if (length(ind)>0){
      LABEL[ind]=paste0(LABEL[ind],"-",as.character(1:length(ind)),sep="")
    }
  }
  
  
  return(LABEL)
}
  
  
