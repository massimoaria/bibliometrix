#' Splitting Network communities
#'
#' \code{networkPlot} Create a network plot with separated communities.
#'
#' The function \code{\link{splitCommunities}} splits communitites in separated subnetworks from a bibliographic network plot previously created by \code{\link{networkPlot}}.
#' 
#' @param graph is a network plot obtained by the function \code{\link{networkPlot}}. 
#' @param n is an integer. It indicates the number of vertices to plot for each community.
#' @return It is a network object of the class \code{igraph}
#' 
#' 
#' @examples
#' # EXAMPLE Keywordd co-occurrence network
#'
#' data(management, package = "bibliometrixData")
#'
#' NetMatrix <- biblioNetwork(management, analysis = "co-occurrences", 
#' network = "keywords", sep = ";")
#' 
#' net <- networkPlot(NetMatrix, n = 30, type = "auto", 
#'                Title = "Co-occurrence Network",labelsize=1, verbose=FALSE) 
#' 
#' graph <- splitCommunities(net$graph, n = 30)
#' 
#' @seealso \code{\link{biblioNetwork}} to compute a bibliographic network.
#' @seealso \code{\link{networkPlot}} to plot a bibliographic network.
#' @seealso \code{\link{net2VOSviewer}} to export and plot the network with VOSviewer software.
#' @seealso \code{\link{cocMatrix}} to compute a co-occurrence matrix.
#' @seealso \code{\link{biblioAnalysis}} to perform a bibliometric analysis.
#' 
#' @export
splitCommunities <- function(graph, n=NULL){
  
df <- data.frame(label = V(graph)$name, size = V(graph)$deg, group = V(graph)$community)

if (!is.null(n)){
  labels <- df %>%
    group_by(.data$group) %>%
    top_n(n = n, wt = .data$size) %>%
    as.data.frame()}
else{
  labels <- df %>%
    group_by(.data$group) %>%
    as.data.frame()
}

# remove inter-cluster edges
ind <- which(E(graph)$color==adjustcolor("gray70", alpha.f=graph$alpha / 2))
coGraph <- igraph::delete_edges(graph, E(graph)[ind])
ind <- which(V(coGraph)$name %in% labels$label)
V(coGraph)$label[-ind]=""
igraph::graph_attr(coGraph, "layout") <- igraph::layout_with_fr(coGraph)
#plot(coGraph)
return(coGraph)
}


