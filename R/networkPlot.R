#' Plotting Bibliographic networks
#'
#' \code{networkPlot} plots a bibliographic network.
#'
#' The function \code{\link{networkPlot}} can plot a bibliographic network previously created by \code{\link{biblioNetwork}}.
#' 
#' @param NetMatrix is a network matrix obtained by the function \code{\link{biblioNetwork}}. 
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
#' @param cluster is a character. It indicates the type of cluster to perform among ("none", optimal", "louvain","infomap","edge_betweenness","walktrap", "spinglass", "leading_eigen", "fast_greedy").
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
#' \code{cluster_obj} \tab  \tab a \code{\link{communities}} object of the package \code{igraph}\cr
#' \code{cluster_res} \tab  \tab a data frame with main results of clustering procedure.\cr}
#' 
#' 
#' @examples
#' # EXAMPLE Keywordd co-occurrence network
#'
#' data(management)
#'
#' NetMatrix <- biblioNetwork(management, analysis = "co-occurrences", 
#' network = "keywords", sep = ";")
#' 
#' net <- networkPlot(NetMatrix, n = 30, type = "auto", Title = "Co-occurrence Network",labelsize=1) 
#' 
#' @seealso \code{\link{biblioNetwork}} to compute a bibliographic network.
#' @seealso \code{\link{net2VOSviewer}} to export and plot the network with VOSviewer software.
#' @seealso \code{\link{cocMatrix}} to compute a co-occurrence matrix.
#' @seealso \code{\link{biblioAnalysis}} to perform a bibliometric analysis.
#' 
#' @export
networkPlot <-
  function(NetMatrix,
           normalize = NULL,
           n = NULL,
           degree = NULL,
           Title = "Plot",
           type = "auto",
           label = TRUE,
           labelsize = 1,
           label.cex = FALSE,
           label.color = FALSE,
           label.n = NULL,
           halo = FALSE,
           cluster = "louvain",
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
           verbose = TRUE) {
    
    S <- NULL
    
    colnames(NetMatrix) <- rownames(NetMatrix) <- tolower(colnames(NetMatrix))
    bsk.S <- TRUE
    l <- NA
    net_groups <- list()
    #if (type == "vosviewer") {
    #  cluster <- "none"
    #}
    
    if (!is.null(normalize)) {
      S <- normalizeSimilarity(NetMatrix, type = normalize)
      bsk.S <- graph.adjacency(S, mode = "undirected", weighted = T)
    }
    
    ## legacy with version <1.9.4
    if (isTRUE(size)) {
      size <- 20
      size.cex <- T
    }
    
    if (alpha < 0 & alpha > 1) {
      alpha <- 0.5
    }
    
    # Create igraph object
    bsk.network <-
      graph.adjacency(NetMatrix, mode = "undirected", weighted = weighted)
    
    
    # vertex labels
    V(bsk.network)$name <- colnames(NetMatrix)
    
    
    # Compute node degrees (#links) and use that to set node size:
    deg <- degree(bsk.network, mode = "all")
    V(bsk.network)$deg <- deg
    if (isTRUE(size.cex)) {
      V(bsk.network)$size <- (deg / max(deg)[1]) * size
    } else{
      V(bsk.network)$size <- rep(size, length(V(bsk.network)))
    }
    
    # label size
    if (isTRUE(label.cex)) {
      lsize <- log(1 + (deg / max(deg)[1])) * labelsize
      lsize[lsize < 0.5] <- 0.5  ### min labelsize is fixed to 0.5
      V(bsk.network)$label.cex <- lsize
    } else{
      V(bsk.network)$label.cex <- labelsize
    }
    
    # Select number of vertices to plot
    if (!is.null(degree)) {
      Deg <- deg - diag(NetMatrix)
      Vind <- Deg < degree
      if (sum(!Vind) == 0) {
        cat("\ndegree argument is to high!\n\n")
        return()
      }
      bsk.network <- delete.vertices(bsk.network, which(Vind))
      if (!isTRUE(bsk.S)) {
        bsk.S <- delete.vertices(bsk.S, which(Vind))
      }
    } else {
      if (n > dim(NetMatrix)[1]) {
        n <- dim(NetMatrix)[1]
      }
      nodes <- names(sort(deg, decreasing = TRUE)[1:n])
      
      bsk.network <- delete.vertices(bsk.network, which(!(V(bsk.network)$name %in% nodes)))
      if (!isTRUE(bsk.S)) {
        bsk.S <- delete.vertices(bsk.S,  which(!(V(bsk.S)$name %in% nodes)))
      }
    }
    
    # Remove loops and multiple edges
    if (edges.min > 1) {
      remove.multiple = FALSE
    }
    bsk.network <-
      simplify(bsk.network,
               remove.multiple = remove.multiple,
               remove.loops = noloops)
    if (!isTRUE(bsk.S)) {
      bsk.S <-
        simplify(bsk.S,
                 remove.multiple = remove.multiple,
                 remove.loops = noloops)
    }
    
    ### graph to write in pajek format ###
    bsk.save <- bsk.network
    V(bsk.save)$id <- V(bsk.save)$name
    
    ###
    
    # Clustering
    #if (type != "vosviewer") {
      ## Edge size
      E(bsk.network)$num <- E(bsk.save)$num <- count_multiple(bsk.network, eids = E(bsk.network))
      if (is.null(weighted)) {
        E(bsk.save)$weight <- E(bsk.save)$num
      }
      
      if (!is.null(weighted)) {
        E(bsk.network)$width <-
          (E(bsk.network)$weight + min(E(bsk.network)$weight)) / max(E(bsk.network)$weight + min(E(bsk.network)$weight)) *
          edgesize
      } else{
        if (isTRUE(remove.multiple)) {
          E(bsk.network)$width <- edgesize
        }
        else{
          edges <- E(bsk.network)$num
          E(bsk.network)$width <- edges / max(edges) * edgesize
        }
      }
      
      bsk.network <- delete.edges(bsk.network, which(E(bsk.network)$num < edges.min))
      if (!isTRUE(bsk.S)) {
        bsk.S <- delete.edges(bsk.S, which(E(bsk.network)$num < edges.min))
      }
      
      # delete not linked vertices
      if (isTRUE(remove.isolates)) {
        bsk.network <- delete.isolates(bsk.network, mode = 'all')
        if (!isTRUE(bsk.S)) {
          bsk.S <-
            delete.vertices(bsk.S, which(V(bsk.S)$name %in% setdiff(
              V(bsk.S)$name, V(bsk.network)$name
            )))
        }
      }
      
      # Choose Network layout
      if (!isTRUE(bsk.S)) {
        l <- switchLayout(bsk.S, type)
      } else{
        l <- switchLayout(bsk.network, type)
      }
      
      cl <- clusteringNetwork(bsk.network, cluster)
      
      bsk.network <- cl$bsk.network
      if (!isTRUE(bsk.S)) {
        V(bsk.S)$color = V(bsk.network)$color
      }
      net_groups <- cl$net_groups
      
      ## Labelling the network
      LABEL = ""
      if (isTRUE(label)) {
        LABEL <- V(bsk.network)$name
        if (!is.null(label.n)) {
          q <- 1 - (label.n / length(V(bsk.network)$deg))
          if (q <= 0) {
            LABEL <- rep("", length(LABEL))
            V(bsk.network)$labelsize <- 10
          } else {
            if (q > 1) {
              q <- 1
            }
            q <- quantile(V(bsk.network)$deg, q)
            LABEL[V(bsk.network)$deg < q] <- ""
            V(bsk.network)$labelsize <- 10
            V(bsk.network)$labelsize[V(bsk.network)$deg < q] <- 0
          }
        }
      }
      
      
      
      
      if (isTRUE(label.color)) {
        lab.color <- V(bsk.network)$color
      } else{
        lab.color <- "black"
      }
      
      
      
      #l <- layout.norm(l)
      
      ## Setting Network Attributes
      igraph::graph_attr(bsk.network, "alpha") <- alpha
      igraph::graph_attr(bsk.network, "ylim") <- c(-1,1)
      igraph::graph_attr(bsk.network, "xlim") <- c(-1,1)
      igraph::graph_attr(bsk.network, "rescale") <- TRUE
      igraph::graph_attr(bsk.network, "asp") <- 0
      igraph::graph_attr(bsk.network, "layout") <- l
      igraph::graph_attr(bsk.network, "main") <- Title
      E(bsk.network)$curved = curved
      V(bsk.network)$label.dist = 0.7
      V(bsk.network)$frame.color = adjustcolor('black', alpha)
      V(bsk.network)$color <- adjustcolor(V(bsk.network)$color, alpha)
      V(bsk.network)$label.color <- adjustcolor('black', min(c(1, alpha + 0.1)))
      V(bsk.network)$label.font = 2
      V(bsk.network)$label = LABEL
      
      
      ## Plot the network
      if (isTRUE(halo) & cluster != "none") {
        
        if (isTRUE(verbose)){plot(net_groups,bsk.network)}
        
      } else{
        E(bsk.network)$color = adjustcolor(E(bsk.network)$color, alpha / 2)
        
        if (isTRUE(verbose)){plot(bsk.network)}
        
      }
      
    ## Output
    if (cluster != "none") {
      cluster_res <- data.frame(net_groups$names,
                               net_groups$membership,
                               as.numeric(betweenness(
                                 bsk.network, directed = F, normalized = F
                               )),
                               suppressWarnings(as.numeric(closeness(
                                 bsk.network))),
                               as.numeric(page.rank(bsk.network)$vector))
      names(cluster_res) <- c("vertex", "cluster", "btw_centrality", "clos_centrality","pagerank_centrality")
      cluster_res <- cluster_res[order(cluster_res$cluster), ]
    } else {
      cluster_res <- NA
    }
    
    
    net <- list(
      graph = bsk.network,
      graph_pajek = bsk.save,
      cluster_obj = net_groups,
      cluster_res = cluster_res,
      layout = l,
      S = S,
      nodeDegree = sort(deg, decreasing = T) 
    )
    
    return(net)
  }






### internal functions:

### deleteIsolates

delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0) - 1
  delete.vertices(graph, names(isolates))
}

### clusteringNetwork

clusteringNetwork <- function(bsk.network, cluster) {
  colorlist <- c(
    brewer.pal(9, 'Set1')[-6],
    brewer.pal(8, 'Set2')[-7],
    brewer.pal(12, 'Paired')[-11],
    brewer.pal(12, 'Set3')[-c(2, 8, 12)]
  )
  
  switch(
    cluster,
    none = {
      net_groups = list(membership = rep(1, vcount(bsk.network)))
    },
    optimal = {
      net_groups <- cluster_optimal(bsk.network)
    },
    louvain = {
      net_groups <- cluster_louvain(bsk.network)
    },
    fast_greedy = {
      net_groups <- cluster_fast_greedy(bsk.network)
    },
    leading_eigen = {
      net_groups <- cluster_leading_eigen(bsk.network)
    },
    spinglass = {
      net_groups <- cluster_spinglass(bsk.network)
    },
    infomap = {
      net_groups <- cluster_infomap(bsk.network)
    },
    edge_betweenness = {
      net_groups <- cluster_edge_betweenness(bsk.network)
    },
    walktrap = {
      net_groups <- cluster_walktrap(bsk.network)
    },
    
    ## default statement
    {
      cat("\nUnknown cluster argument. Using default algorithm\n")
      net_groups <- cluster_walktrap(bsk.network)
    }
  )
  
  V(bsk.network)$color <- colorlist[net_groups$membership]
  
  ### set egde intra-class colors
  V(bsk.network)$community <- net_groups$membership
  El <- as.data.frame(get.edgelist(bsk.network, names = F))
  
  colorlist <- c(
    brewer.pal(9, 'Set1')[-6],
    brewer.pal(8, 'Set2')[-7],
    brewer.pal(12, 'Paired')[-11],
    brewer.pal(12, 'Set3')[-c(2, 8, 12)]
  )
  E(bsk.network)$color <- apply(El, 1, function(x) {
    if (V(bsk.network)$community[x[1]] == V(bsk.network)$community[x[2]]) {
      C <- colorlist[V(bsk.network)$community[x[1]]]
    } else{
      C <- 'gray70'
    }
    return(C)
  })
  E(bsk.network)$lty <- 1
  E(bsk.network)$lty[E(bsk.network)$color == "gray70"] <- 5
  ### end
  
  cl <- list()
  cl$bsk.network <- bsk.network
  cl$net_groups <- net_groups
  return(cl)
}

### switchLayout

switchLayout <- function(bsk.network, type) {
  switch(
    type,
    auto = {
      l <- layout.auto(bsk.network)
    },
    circle = {
      l <- layout.circle(bsk.network)
    },
    star = {
      l <- layout.star(bsk.network)
    },
    sphere = {
      l <- layout.sphere(bsk.network)
    },
    mds = {
      l <- layout.mds(bsk.network)
    },
    fruchterman = {
      l <- layout.fruchterman.reingold(bsk.network)
    },
    kamada = {
      l <- layout.kamada.kawai(bsk.network)
    }
  )
  l <- layout.norm(l)
 
  return(l)
}
