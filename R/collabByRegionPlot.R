utils::globalVariables(c("countries","continent"))
#' Country Collaboration Networks by Region
#' 
#' A function to create and plot country collaboration networks by Region
#' 
#' @param NetMatrix is a country collaboration matrix obtained by the function \code{\link{biblioNetwork}}. 
#' @param normalize is a character. It can be "association", "jaccard", "inclusion","salton" or "equivalence" to obtain Association Strength, Jaccard, 
#' Inclusion, Salton or Equivalence similarity index respectively. The default is type = NULL.
#' @param n is an integer. It indicates the number of vertices to plot.
#' @param degree is an integer. It indicates the min frequency of a vertex. If degree is not NULL, n is ignored.
#' @param type is a character object. It indicates the network map layout: 
#' 
#' \tabular{lll}{
#' \code{type="auto"}\tab   \tab Automatic layout selection\cr
#' \code{type="circle"}\tab   \tab Circle layout\cr
#' \code{type="sphere"}\tab   \tab Sphere layout\cr
#' \code{type="mds"}\tab   \tab Multidimensional Scaling layout\cr
#' \code{type="fruchterman"}\tab   \tab Fruchterman-Reingold layout\cr
#' \code{type="kamada"}\tab   \tab  Kamada-Kawai layout}
#'  
#' @param vos.path is a character indicating the full path where VOSviewer.jar is located.
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
#' @param cluster is a character. It indicates the type of cluster to perform among ("none", "optimal", "louvain","leiden", "infomap","edge_betweenness","walktrap", "spinglass", "leading_eigen", "fast_greedy").
#' @param community.repulsion is a real. It indicates the repulsion force among network communities. It is a real number between 0 and 1. Default is \code{community.repulsion = 0.1}.
#' @param curved is a logical or a number. If TRUE edges are plotted with an optimal curvature. Default is \code{curved=FALSE}. Curved values are any numbers from 0 to 1.
#' @param weighted This argument specifies whether to create a weighted graph from an adjacency matrix. 
#' If it is NULL then an unweighted graph is created and the elements of the adjacency matrix gives the number of edges between the vertices. 
#' If it is a character constant then for every non-zero matrix entry an edge is created and the value of the entry is added as an edge attribute 
#' named by the weighted argument. If it is TRUE then a weighted graph is created and the name of the edge attribute will be weight.
#' @param edgesize is an integer. It indicates the network edge size.
#' @param edges.min is an integer. It indicates the min frequency of edges between two vertices. If edge.min=0, all edges are plotted.
#' @param label.n is an integer. It indicates the number of vertex labels to draw.
#' @param alpha is a number. Legal alpha values are any numbers from 0 (transparent) to 1 (opaque). The default alpha value usually is 0.5.
#' @param verbose is a logical. If TRUE, network will be plotted. Default is \code{verbose = TRUE}.
#' @return It is a list containing the following elements:
#' \tabular{lll}{
#' \code{graph} \tab  \tab a network object of the class \code{igraph}\cr
#' \code{cluster_obj} \tab  \tab a \code{communities} object of the package \code{igraph}\cr
#' \code{cluster_res} \tab  \tab a data frame with main results of clustering procedure.\cr}
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' data(management, package="bibliometrixData")
#' 
#' management <- metaTagExtraction(management, Field = "AU_CO")
#' 
#' NetMatrix <- biblioNetwork(management, analysis = "collaboration", network = "countries") 
#' 
#' net <- collabByRegionPlot(NetMatrix, edgesize = 4, label.cex = TRUE, labelsize=2.5, 
#'                           weighted = TRUE, size=0.5, size.cex=TRUE, community.repulsion = 0, 
#'                          verbose=FALSE)
#' 
#' cbind(names(net))
#' 
#' plot(net[[4]]$graph)
#' }
#' 
#' @export
#' 
collabByRegionPlot <- function(
    NetMatrix,
    normalize = NULL,
    n = NULL,
    degree = NULL,
    type = "auto",
    label = TRUE,
    labelsize = 1,
    label.cex = FALSE,
    label.color = FALSE,
    label.n = Inf,
    halo = FALSE,
    cluster = "walktrap",
    community.repulsion = 0,
    vos.path = NULL,
    size = 3,
    size.cex = FALSE,
    curved = FALSE,
    noloops = TRUE,
    remove.multiple = TRUE,
    remove.isolates = FALSE,
    weighted = NULL,
    edgesize = 1,
    edges.min = 0,
    alpha = 0.5,
    verbose = TRUE){
  
  row.names(NetMatrix) <- colnames(NetMatrix) <- tolower(row.names(NetMatrix))
  labelCo <- data.frame(countries= row.names(NetMatrix))
  data("countries",envir=environment())
  countries <- countries %>% mutate(countries = tolower(countries))
  
  labelCo <- labelCo %>% 
    left_join(countries, by = "countries")
  
  if (nrow(labelCo)<2){
    message("The argument NetMatrix is not a country collaboration network matrix")
    return(NA)
  }
  
  regions <- unique(labelCo$continent)
  n_regions <- length(regions)
  
  net <- list()

  for (i in regions){
    reg_co <- labelCo %>% 
      dplyr::filter(continent %in% i) %>% 
      select(countries)
    NetMatrix_reg <- NetMatrix[reg_co$countries,reg_co$countries]

    if (!is.null(nrow(NetMatrix_reg))){
      net[[i]] <- networkPlot(NetMatrix_reg, normalize=normalize, n = Inf,degree = degree,Title = i,type = type,
                              label = label, labelsize = labelsize, label.cex = label.cex, label.color = label.color, label.n = label.n,
                              halo = halo, cluster = cluster, community.repulsion = community.repulsion, vos.path = NULL, size = size,
                              size.cex = size.cex, curved = curved, noloops = noloops, remove.multiple = remove.multiple, remove.isolates = remove.isolates,
                              weighted = weighted, edgesize = edgesize, edges.min = edges.min, alpha = alpha, verbose = FALSE)
      net[[i]]$NetMatrix_reg <- NetMatrix_reg
      }
    
  }
  
  if (verbose){ 
    l <- ceiling(length(net)/2)
    par(mfrow=c(l,2))
    # for (i in 1:length(net)){
    #   plot(net[[i]]$graph)
    # }
    lapply(net, function(x){plot(x$graph)})
  }
  return(net)
}
