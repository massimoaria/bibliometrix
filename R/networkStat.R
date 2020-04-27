#' Calculating network summary statistics
#'
#' \code{networkStat} calculates main network statistics.
#'
#' The function \code{\link{networkStat}} can calculate the main network statistics from a bibliographic network previously created by \code{\link{biblioNetwork}}.
#' @param object is a network matrix obtained by the function \code{\link{biblioNetwork}} or an graph object of the class \code{igraph}. 
#' @param stat is a character. It indicates which statistics are to be calculated. \code{stat = "network"} calculates the statistics related to the network; 
#' \code{stat = "all"} calculates the statistics related to the network and the individual nodes that compose it. Default value is \code{stat = "network"}.
#' @param type is a character. It indicates which centrality index is calculated. type values can be c("degree", "closeness", "betweenness","eigenvector","pagerank","hub","authority"). Default is "degree".
#' @return It is a list containing the following elements:
#' \tabular{lll}{
#' \code{graph} \tab  \tab a network object of the class \code{igraph}\cr
#' \code{network} \tab  \tab a \code{\link{communities}} a list with the main statistics of the network\cr
#' \code{vertex} \tab  \tab a data frame with the main measures of centrality and prestige of vertices.\cr}
#' 
#' 
#' @examples
#' # EXAMPLE Co-citation network
#' 
#' # to run the example, please remove # from the beginning of the following lines
#' # data(scientometrics)
#'
#' # NetMatrix <- biblioNetwork(scientometrics, analysis = "co-citation", 
#' #      network = "references", sep = ";")
#' 
#' # netstat <- networkStat(NetMatrix, stat = "all", type = "degree")
#' 
#' @seealso \code{\link{biblioNetwork}} to compute a bibliographic network.
#' @seealso \code{\link{cocMatrix}} to compute a co-occurrence matrix.
#' @seealso \code{\link{biblioAnalysis}} to perform a bibliometric analysis.
#' 
#' @export
networkStat<-function(object, stat="network",type="degree"){
  
  if (class(object)!="igraph"){
  # Create igraph object
  net <- graph.adjacency(object,mode="undirected",weighted=NULL)
  V(net)$id <- colnames(object)}else{
    net <- object
    V(net)$id=V(net)$name}
  
  net <- simplify(net, remove.loops = T) 
  
  ### network statistics
  networkSize <- vcount(net)
 
  # The proportion of present edges from all possible edges in the network.
  networkDensity=edge_density(net, loops = FALSE)
  
  # Transitivity
  # global - ratio of triangles (direction disregarded) to connected triples.
  TR <- transitivity(net, type="global")
  # local - ratio of triangles to connected triples each vertex is part of.
  #TRL=transitivity(net, type="local")
  
  # Diameter
  # A network diameter is the longest geodesic distance 
  # (length of the shortest path between two nodes) in the network
  DIAM <- diameter(net, directed=F, weights=NA)
  
  # Degree distribution
  deg <- degree(net, mode="all")
  deg.dist <- degree_distribution(net, cumulative=T, mode="all")
  
  #plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
  #      xlab="Degree", ylab="Cumulative Frequency")
  
  
  # Network centralization
  # Degree (number of ties)degree
  NCD=NCC=NCB=NCE=pagerank=hub=authority=NA
  switch(type,
    degree={NCD <- centr_degree(net, mode="all", normalized=T)$centralization},
    closeness={# Closeness (centrality based on distance to others in the graph)
      NCC <- suppressWarnings(centr_clo(net, mode="all", normalized=T)$centralization)},
    betweenness={# Betweenness (centrality based on a broker position connecting others)
      NCB <- centr_betw(net,directed=F, normalized=T)$centralization},
    eigenvector={# Eigenvector (centrality proportional to the sum of connection centralities)
      NCE <- centr_eigen(net, directed=F, normalized=T)$centralization},
    pagerank={NCD <- centr_degree(net, mode="all", normalized=T)$centralization},
    hub={NCD <- centr_degree(net, mode="all", normalized=T)$centralization},
    authority={NCD <- centr_degree(net, mode="all", normalized=T)$centralization}
  )
  # Average path length 
  # the mean of the shortest distance between each pair of nodes in the network (in both directions for directed graphs).
  meanDistance <- mean_distance(net, directed=F)
  
  networkResults <- list(networkSize=networkSize,
               networkDensity=networkDensity,
               networkTransitivity=TR,
               networkDiameter=DIAM,
               networkDegreeDist=deg.dist,
               networkCentrDegree=NCD,
               networkCentrCloseness=NCC,
               networkCentrEigen=NCE,
               networkCentrbetweenness=NCB,
               NetworkAverPathLeng=meanDistance)
  
  ### Centrality and Prestige of vertices
  if (stat=="all"){
  
   DC=CC=BC=EC=PR=HS=AS=NA
    switch(type,
           degree={
             # Degree centrality. 
             # The simplest way to quantify a node'simportance is to consider the number of nodes it is incident with, 
             # with high numbers interpreted to be of higher importance. 
             # standardized index is: Ei/(N-1)
             DC <- degree(net, v = V(net), mode = c("all"), loops = TRUE, normalized = TRUE)
           },
           closeness={
             # Closeness centrality. 
             # Nodes can also be indexed by con-sidering their geodesic distance to each other.
             CC <- suppressWarnings(closeness(net, vids = V(net), mode = c("all"), normalized = TRUE))
           },
           betweenness={
             # Betweenness centrality. 
             # Another way to gauge a node's influence is to consider its role in linking other nodes together in the network.
             BC <- betweenness(net, v = V(net), directed = FALSE, weights = NULL, nobigint = TRUE, normalized = TRUE)
           },
           eigenvector={
             # Eigenvector centrality. 
             # Eigenvector centrality is another measure of centrality. 
             # The eigenvector centrality ofeach node can be found by computing the leading
             EC <- eigen_centrality(net, directed = FALSE, scale = TRUE, weights = NULL, options = arpack_defaults)$vector
           },
           pagerank={
             # PageRank ranking of vertices
             PR <- page_rank(net, algo = c("prpack"), vids = V(net),
                             directed = FALSE, damping = 0.85, personalized = NULL, weights = NULL,
                             options = NULL)$vector
           },
           hub={
             ### Hubs and authorities
             # The hubs and authorities algorithm developed by Jon Kleinberg was initially used to examine web pages. 
             # Hubs were expected to contain catalogs with a large number of outgoing links; while authorities would get 
             # many incoming links from hubs, presumably because of their high-quality relevant information.
             
             # Hubs
             HS <- hub_score(net, weights=NA)$vector
           },
           authority={
             # Authorities
             AS <- authority_score(net, weights=NA)$vector
           }
    )  
  
  vertexResults <- data.frame(vertexID=V(net)$id,
                     vertexCentrDegree=DC,
                     vertexCentrCloseness=CC,
                     vertexCentrEigen=EC,
                     vertexCentrBetweenness=BC,
                     vertexPageRank=PR,
                     vertexHub=HS,
                     vertexAuthority=AS)
  
  #res=PCA(vertexResults[,-1],graph=FALSE)
  
  #R=rank(-res$ind$coord[,1]-min(res$ind$coord[,1]))
  
  #vertexResults$Ranking=R
  
  }else{vertexResults=NA}
  netstat <- list(graph=net,network=networkResults,vertex=vertexResults,stat=stat,type=type)
  class(netstat)="bibliometrix_netstat"
  return(netstat)
  
}