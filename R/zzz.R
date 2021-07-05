utils::globalVariables("where")
#utils::globalVariables("any_of")
#utils::globalVariables("if_all")

#' @import stats
#' @import dimensionsR
#' @import ggplot2
#' @import bibliometrixData
#' @import forcats
#' @import ggrepel
# @import ggnetwork
#' @import pubmedR
#' @import shiny
#' @import readr
#' @import readxl
#' @import tidytext
# @importFrom plyr .
#' @importFrom dplyr %>%
#' @importFrom dplyr across
#' @importFrom dplyr tibble
#' @importFrom dplyr as_tibble
#' @importFrom dplyr between
## @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr do
#' @importFrom dplyr n
#' @importFrom dplyr slice
#' @importFrom dplyr if_all
#' @importFrom dplyr any_of
#' @importFrom dplyr cummean
#' @importFrom dplyr desc
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr rename
#' @importFrom dplyr rowwise
#' @importFrom dplyr summarise
#' @importFrom dplyr summarize
#' @importFrom dplyr select
#' @importFrom dplyr anti_join
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr right_join
#' @importFrom dplyr top_n
#' @importFrom dplyr relocate
#' @importFrom dplyr slice_head
#' @importFrom dplyr slice_tail
#' @importFrom plotly add_annotations
#' @importFrom plotly add_lines
#' @importFrom plotly config
#' @importFrom plotly ggplotly
#' @importFrom plotly layout
#' @importFrom plotly plot_ly
#' @importFrom plotly subplot
#' @importFrom plotly toRGB
# #' @import shinycssloaders
# #' @import shinythemes
#' @importFrom openxlsx write.xlsx
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr replace_na
#' @importFrom tidyr separate
#' @importFrom tidyr drop_na
#' @importFrom tidyr unite
#' @importFrom tidyr starts_with
#' @importFrom tidyr unnest
#' @importFrom grDevices adjustcolor
#' @importFrom grDevices dev.off
#' @importFrom grDevices pdf
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom stringdist stringdistmatrix
#' @importFrom rscopus affiliation_retrieval
#' @importFrom rscopus author_df_orig
#' @importFrom rscopus author_search
#' @importFrom rscopus get_complete_author_info
#' @importFrom RColorBrewer brewer.pal
#' @importFrom FactoMineR MCA
#' @importFrom FactoMineR CA
#' @importFrom FactoMineR PCA
#' @importFrom factoextra get_mca_var
#' @importFrom factoextra get_mca_ind
#' @importFrom factoextra get_ca_row
#' @importFrom factoextra get_ca_col
#' @importFrom factoextra fviz_nbclust
#' @importFrom factoextra fviz_cluster
#' @importFrom factoextra fviz_dend
#' @importFrom factoextra hcut
#' @importFrom igraph get.edgelist
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph as_adjacency_matrix
#' @importFrom igraph graph.adjacency
#' @importFrom igraph degree
#' @importFrom igraph plot.igraph
#' @importFrom igraph delete.vertices
#' @importFrom igraph decompose.graph
#' @importFrom igraph E
#' @importFrom igraph E<-
#' @importFrom igraph V
#' @importFrom igraph V<-
#' @importFrom igraph vcount
#' @importFrom igraph graph_attr
#' @importFrom igraph edge_density
#' @importFrom igraph transitivity
#' @importFrom igraph diameter
#' @importFrom igraph degree_distribution
#' @importFrom igraph centr_degree
#' @importFrom igraph centr_clo
#' @importFrom igraph centr_betw
#' @importFrom igraph centr_eigen
#' @importFrom igraph mean_distance
#' @importFrom igraph closeness
#' @importFrom igraph page.rank
#' @importFrom igraph eigen_centrality
#' @importFrom igraph arpack_defaults
#' @importFrom igraph authority_score
#' @importFrom igraph page_rank
#' @importFrom igraph hub_score
#' @importFrom igraph graph_from_incidence_matrix
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom igraph simplify
#' @importFrom igraph layout.auto
#' @importFrom igraph layout.circle
#' @importFrom igraph layout.sphere
#' @importFrom igraph layout.mds
#' @importFrom igraph layout.kamada.kawai
#' @importFrom igraph layout.fruchterman.reingold
#' @importFrom igraph layout.star
#' @importFrom igraph write.graph
#' @importFrom igraph cluster_walktrap
#' @importFrom igraph cluster_optimal
#' @importFrom igraph cluster_infomap
#' @importFrom igraph cluster_edge_betweenness
#' @importFrom igraph cluster_fast_greedy
#' @importFrom igraph cluster_louvain
#' @importFrom igraph cluster_leading_eigen
#' @importFrom igraph cluster_spinglass
#' @importFrom igraph count_multiple
#' @importFrom igraph membership
#' @importFrom igraph layout.norm
#' @importFrom igraph delete.edges
#' @importFrom igraph betweenness
#' @importFrom Matrix %&%
#' @importFrom Matrix abIseq
#' @importFrom Matrix abIseq1
#' @importFrom Matrix all.equal
#' @importFrom Matrix anyDuplicatedT
#' @importFrom Matrix Arith
#' @importFrom Matrix as.array
#' @importFrom Matrix as.matrix
#' @importFrom Matrix band
#' @importFrom Matrix bandSparse
#' @importFrom Matrix bdiag
#' @importFrom Matrix cbind2
#' @importFrom Matrix chol
#' @importFrom Matrix chol2inv
#' @importFrom Matrix Cholesky
#' @importFrom Matrix coerce
#' @importFrom Matrix colMeans
#' @importFrom Matrix colSums
#' @importFrom Matrix Compare
#' @importFrom Matrix condest
#' @importFrom Matrix cov2cor
#' @importFrom Matrix crossprod
#' @importFrom Matrix det
#' @importFrom Matrix determinant
#' @importFrom Matrix diag
#' @importFrom Matrix diag<-
#' @importFrom Matrix diagN2U
#' @importFrom Matrix Diagonal
#' @importFrom Matrix diagU2N
#' @importFrom Matrix diff
#' @importFrom Matrix drop
#' @importFrom Matrix drop0
#' @importFrom Matrix expand
#' @importFrom Matrix expm
#' @importFrom Matrix fac2sparse
#' @importFrom Matrix forceSymmetric
#' @importFrom Matrix format
#' @importFrom Matrix formatSparseM
#' @importFrom Matrix formatSpMatrix
#' @importFrom Matrix graph2T
#' @importFrom Matrix head
#' @importFrom Matrix image
#' @importFrom Matrix invPerm
#' @importFrom Matrix is.null.DN
#' @importFrom Matrix isDiagonal
#' @importFrom Matrix isLDL
#' @importFrom Matrix isSymmetric
#' @importFrom Matrix isTriangular
#' @importFrom Matrix kronecker
#' @importFrom Matrix Logic
#' @importFrom Matrix lu
#' @importFrom Matrix Math
#' @importFrom Matrix Math2
#' @importFrom Matrix Matrix
#' @importFrom Matrix MatrixClass
#' @importFrom Matrix mean
#' @importFrom Matrix nnzero
#' @importFrom Matrix norm
#' @importFrom Matrix onenormest
#' @importFrom Matrix Ops
#' @importFrom Matrix pack
#' @importFrom Matrix print
#' @importFrom Matrix printSpMatrix
#' @importFrom Matrix printSpMatrix2
#' @importFrom Matrix qr
#' @importFrom Matrix qr.coef
#' @importFrom Matrix qr.fitted
#' @importFrom Matrix qr.Q
#' @importFrom Matrix qr.qty
#' @importFrom Matrix qr.qy
#' @importFrom Matrix qr.R
#' @importFrom Matrix qr.resid
#' @importFrom Matrix qrR
#' @importFrom Matrix rankMatrix
#' @importFrom Matrix rbind2
#' @importFrom Matrix rcond
#' @importFrom Matrix readHB
#' @importFrom Matrix readMM
#' @importFrom Matrix rep2abI
#' @importFrom Matrix rowMeans
#' @importFrom Matrix rowSums
#' @importFrom Matrix rsparsematrix
#' @importFrom Matrix show
#' @importFrom Matrix skewpart
#' @importFrom Matrix solve
#' @importFrom Matrix sparse.model.matrix
#' @importFrom Matrix sparseMatrix
#' @importFrom Matrix sparseVector
#' @importFrom Matrix spMatrix
#' @importFrom Matrix summary
#' @importFrom Matrix Summary
#' @importFrom Matrix symmpart
#' @importFrom Matrix t
#' @importFrom Matrix T2graph
#' @importFrom Matrix tail
#' @importFrom Matrix tcrossprod
#' @importFrom Matrix tril
#' @importFrom Matrix triu
#' @importFrom Matrix uniqTsparse
#' @importFrom Matrix unname
#' @importFrom Matrix unpack
#' @importFrom Matrix update
#' @importFrom Matrix updown
#' @importFrom Matrix which
#' @importFrom Matrix writeMM
#' @importFrom stringr str_locate_all
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_replace_all
#' @importFrom graphics barplot
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics plot
#' @importFrom graphics par
#' @importFrom grDevices colorRampPalette
#' @importFrom grDevices as.raster
#' @importFrom utils capture.output
#' @importFrom utils data
#' @importFrom utils adist
#' @importFrom utils read.csv
#' @importFrom SnowballC wordStem
#' @importFrom SnowballC getStemLanguages
# @importFrom rio import
.onAttach<-function(...){
  packageStartupMessage("To cite bibliometrix in publications, please use:\n\nAria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, 
                                 Journal of Informetrics, 11(4), pp 959-975, Elsevier.
                        \n\nhttps://www.bibliometrix.org\n
                        \nFor information and bug reports:
                        - Send an email to info@bibliometrix.org   
                        - Write a post on https://github.com/massimoaria/bibliometrix/issues
                        \nHelp us to keep Bibliometrix free to download and use by contributing with a small donation to support our research team (https://bibliometrix.org/donate.html)\n
                        \nTo start with the shiny web-interface, please digit:
biblioshiny()\n")
}


### extract data from igraph class object
### Credits to François Briatte. Function is a fork of the package ggnetwork
dataFromIgraph <- function(
  model,
  data = NULL,
  layout = igraph::nicely(),
  arrow.gap = ifelse(igraph::is.directed(model), 0.025, 0),
  by = NULL,
  scale = TRUE,
  stringsAsFactors = getOption("stringsAsFactors"),
  ...
) {
  # node placement
  if (inherits(layout, "matrix") && identical(dim(layout), c(igraph::gorder(model), 2L))) {
    nodes <- layout[, 1:2 ]
  } else if (inherits(layout, "matrix")) {
    stop("layout matrix dimensions do not match network size")
  } else {
    nodes <- igraph::layout_(model, layout, ...)
  }
  
  format_igraph(
    model = model,
    nodes = nodes,
    weights = "none",
    arrow.gap = arrow.gap,
    by = by,
    scale = scale,
    stringsAsFactors = stringsAsFactors,
    .list_vertex_attributes_fun = igraph::list.vertex.attributes,
    .get_vertex_attributes_fun = igraph::get.vertex.attribute,
    .list_edges_attributes_fun = igraph::list.edge.attributes,
    .get_edges_attributes_fun = igraph::get.edge.attribute,
    .as_edges_list_fun = igraph::as_edgelist
  )
}

### Credits to François Briatte. Function is a fork of the package ggnetwork
format_igraph <- function(
  model,
  nodes = NULL,
  weights = NULL,
  arrow.gap = ifelse(network::is.directed(model), 0.025, 0),
  by = NULL,
  scale = TRUE,
  stringsAsFactors = getOption("stringsAsFactors"),
  .list_vertex_attributes_fun = NULL,
  .get_vertex_attributes_fun = NULL,
  .list_edges_attributes_fun = NULL,
  .get_edges_attributes_fun = NULL,
  .as_edges_list_fun = NULL
) {
  # store coordinates
  nodes <- data.frame(nodes)
  colnames(nodes) <- c("x", "y")
  
  # rescale coordinates
  if (scale) {
    nodes$x <- scale_data(nodes$x)
    nodes$y <- scale_data(nodes$y)
  }
  
  # import vertex attributes
  if (length(.list_vertex_attributes_fun(model)) > 0) {
    nodes <- cbind.data.frame(
      nodes,
      sapply(
        X = .list_vertex_attributes_fun(model),
        Y = model,
        FUN = function(X, Y) .get_vertex_attributes_fun(Y, X),
        simplify = FALSE
      ),
      stringsAsFactors = stringsAsFactors
    )
  }
  
  # edge list
  if (inherits(model, "igraph")) {
    edges <- .as_edges_list_fun(model, names = FALSE)
  } else {
    edges <- .as_edges_list_fun(model, attrname = weights)
  }
  
  # edge list (if there are duplicated rows)
  if (nrow(edges[, 1:2, drop = FALSE]) > nrow(unique(edges[, 1:2, drop = FALSE]))) {
    warning("duplicated edges detected")
  }
  
  edges <- data.frame(nodes[edges[, 1], c("x", "y")], nodes[edges[, 2], c("x", "y")])
  colnames(edges) <- c("x", "y", "xend", "yend")
  
  # arrow gap (thanks to @heike and @ethen8181 for their work on this issue)
  if (arrow.gap > 0) {
    x.length <- edges$xend - edges$x
    y.length <- edges$yend - edges$y
    arrow.gap <- arrow.gap / sqrt(x.length^2 + y.length^2)
    edges$xend <- edges$x + (1 - arrow.gap) * x.length
    edges$yend <- edges$y + (1 - arrow.gap) * y.length
  }
  
  # import edge attributes
  if (length(.list_edges_attributes_fun(model)) > 0) {
    edges <- cbind.data.frame(
      edges,
      sapply(
        X = .list_edges_attributes_fun(model),
        Y = model,
        FUN = function(X, Y) .get_edges_attributes_fun(Y, X),
        simplify = FALSE
      ),
      stringsAsFactors = stringsAsFactors
    )
  }
  
  if (nrow(edges) > 0) {
    # drop "na" columns created by 'network' methods
    # this is to ensure consistency with 'igraph' methods
    if ("na" %in% colnames(nodes)) nodes$na <- NULL
    if ("na" %in% colnames(edges)) edges$na <- NULL
    
    # merge edges and nodes data
    edges <- merge(nodes, edges, by = c("x", "y"), all = TRUE)
    
    # add missing columns to nodes data
    nodes$xend <- nodes$x
    nodes$yend <- nodes$y
    # names(nodes) <- names(edges)[1:ncol(nodes)] # columns are already named from 'nodes' and 'edges'
    
    # make nodes data of identical dimensions to edges data
    nodes[, setdiff(names(edges), names(nodes))] <- NA
    
    # panelize nodes (for temporal networks)
    if (!is.null(by)) {
      nodes <- lapply(sort(unique(edges[, by])), function(x) {
        y <- nodes
        y[, by] <- x
        y
      })
      nodes <- do.call(rbind, nodes)
    }
    
    return(unique(rbind(edges[!is.na(edges$xend), ], nodes)))
  } else {
    # add missing columns to nodes data
    nodes$xend <- nodes$x
    nodes$yend <- nodes$y
    return(nodes)
  }
}


### scale coordinates
### Credits to François Briatte. Function is a fork of the package ggnetwork
scale_data <- function(x, scale = diff(range(x))) {
  if (!scale) {
    x <- rep(0.5, length.out = length(x))
  } else {
    x <- scale(x, center = min(x), scale = scale)
  }
  as.vector(x)
}


## Plot edges using ggplot2
### Credits to François Briatte. Function is a fork of the package ggnetwork
geom_network_edges <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  arrow = NULL,
  curvature = 0,
  angle = 90,
  ncp = 5,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  if (!curvature) {
    geom <- ggplot2::GeomSegment
    params <- list(arrow = arrow, na.rm = na.rm, ...)
  } else {
    geom <- ggplot2::GeomCurve
    params <- list(
      arrow = arrow,
      curvature = curvature,
      angle = angle,
      ncp = ncp,
      na.rm = na.rm,
      ...
    )
  }
  
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatEdges,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

### Credits to François Briatte. Function is a fork of the package ggnetwork
geom_network_nodes <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatNodes,
    geom = ggplot2::GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

### Credits to François Briatte. Function is a fork of the package ggnetwork
StatEdges <- ggplot2::ggproto("StatEdges", ggplot2::Stat,
                              compute_layer = function(data, scales, params) {
                                unique(subset(data, !(x == xend & y == yend)))
                              }
)

### Credits to François Briatte. Function is a fork of the package ggnetwork
StatNodes <- ggplot2::ggproto("StatNodes", ggplot2::Stat,
                              compute_layer = function(data, scales, params) {
                                if (all(c("xend", "yend") %in% names(data))) {
                                  unique(subset(data, select = c(-xend, -yend)))
                                } else {
                                  unique(data)
                                }
                              }
)