utils::globalVariables(c("group", "CL1", "CL2", "value", "visEdges", "visIgraphLayout", "visNetwork", "visOptions",
                         "visPhysics"))
#' Plot Thematic Evolution Network
#'
#' Visualizes the thematic evolution of clusters over time using a temporal network layout.
#' Nodes are positioned along the x-axis according to time slices and vertically to minimize overlap.
#' Edges represent links between themes across time periods, with customizable weights and styles.
#' Nodes and Edges objects are the results of a Thematic Evolution Analysis performed using the \code{\link{thematicEvolution}} function.
#' 
#' @param Nodes is a list of nodes obtained by \code{\link{thematicEvolution}} function.
#' @param Edges is a list of edges obtained by \code{\link{thematicEvolution}} function.
#' @param min.flow Numeric. Minimum threshold for edge weight (i.e., flow) to be included in the network visualization. Default is `0`.
#' @param measure Character. The method to define edge weight: `"inclusion"`, `"stability"`, or `"weighted"` (default is `"weighted"`).
#' @param node_shape Character. Shape of the nodes in the network. Options are `"dot"` (default), `"box"`, `"circle"`, or `"ellipse"`.
#' @param label_size Numeric. Font size of the node labels. Default is `15`.
#' @param edge_scale Numeric. Scaling factor for edge width. Default is `10`.
#' @param node_scale Numeric. Scaling factor for node size. Default is `30`.
#' 
#' @return A `visNetwork` object displaying a time-structured thematic evolution network.
#' 
#'
#' @examples
#' 
#' \dontrun{
#' data(managemeent, package = "bibliometrixData")
#' years=c(2004,2015)
#' 
#' nexus <- thematicEvolution(management,field="ID",years=years,n=100,minFreq=2)
#' 
#' plotThematicEvolution(nexus$Nodes,nexus$Edges)
#' }
#' 
#' @seealso \code{\link{thematicMap}} function to create a thematic map based on co-word network analysis and clustering.
#' @seealso \code{\link{thematicMap}} function to perform a thematic evolution analysis.
#' @seealso \code{\link{networkPlot}} to plot a bibliographic network.
#'
#' @export

plotThematicEvolution <- function(Nodes,
                                  Edges,
                                  min.flow = 0,
                                  measure = "weighted", #measure=("inclusion","stability", "weighted")
                                  node_shape = "box",         # "box", "circle", "ellipse"
                                  label_size = 5,
                                  edge_scale = 10,
                                  node_scale = 30) {
  
  switch(measure, 
         inclusion = {
           edge_weight_var = "Inclusion"
         }, 
         stability = {
           edge_weight_var = "Stability"
         }, 
         weighted = {
           edge_weight_var = "Inc_Weighted"
         }
  )
  
  Edges$Stability <- Edges$Stability*10 
  
  # 1. X coordinate by time
  unique_slices <- sort(unique(Nodes$slice))
  x_positions <- normalize_to_minus1_1(setNames((seq_along(unique_slices)-1), unique_slices))
  Nodes <- Nodes %>%
    mutate(x = x_positions[as.character(slice)])
  
  # 2. Y coordinates to avoid overlap
  Nodes<- Nodes %>%
    group_by(slice) %>%
    arrange(name) %>%
    mutate(y = normalize_to_minus1_1(seq(from = 100, by = 100, length.out = n()))) %>%
    ungroup()
  
  # 4. Prepare nodes for visNetwork
  Nodes_vis <- Nodes %>%
    mutate(shape = node_shape,
           size = sum * node_scale,
           value = sum) %>%
    select(id, label, group, color, x, y, shape, size, value)
  
  # Add fonts as a list for each line (label below)
  #Nodes_vis$font <- map(seq_len(nrow(Nodes_vis)), ~ list(size = label_size, vadjust = -0.1))
  
  if (node_shape %in% c("dot", "square")) {
    vadjust <- -2 * (Nodes_vis$size / (max(Nodes_vis$size) - min(Nodes_vis$size))) * label_size
    font_sizes <- rep(label_size, nrow(Nodes_vis))
  } else {
    # Scala il font in base al valore del nodo (sum) tra min e max specificati
    min_font <- 10
    max_font <- label_size*4
    font_sizes <- min_font + (Nodes_vis$value - min(Nodes_vis$value)) / (max(Nodes_vis$value) - min(Nodes_vis$value)) * (max_font - min_font)
    vadjust <- rep(0, nrow(Nodes_vis))
  } 
  
  Nodes_vis$font <- purrr::map2(font_sizes, vadjust, ~ list(size = .x, vadjust = .y))
  
  Nodes_vis$fixed.x <- TRUE
  
  # 5. Prepare edges
  edges_vis <- Edges %>%
    mutate(width = !!sym(edge_weight_var) * edge_scale,
           value = !!sym(edge_weight_var)) %>%
    dplyr::filter(value>=min.flow)
  
  edges_vis$color <- lapply(1:nrow(edges_vis), function(i) list(color = "#D3D3D3", highlight = "#35343370", hover = "#35343370"))
  
  ## layout
  coords = as.matrix(Nodes %>% select(x,y))
  
  # 6. Build the network
  VIS <- visNetwork(Nodes_vis, edges_vis, type="full", smooth=TRUE) %>%
    #visNodes(scaling=list(min = 10, max = node_scale)) %>% 
    visEdges(smooth = list(type="horizontal"), arrows = "to", scaling=list(min = 1, max = edge_scale)) %>%
    visIgraphLayout(layout = "layout.norm", layoutMatrix = coords, type = "full") %>% 
    #visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
    visPhysics(enabled = FALSE) %>%
    visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 1, 
        algorithm = "all", hideColor = "rgba(200, 200, 200, 0.90)"), nodesIdSelection = FALSE) %>%
    #visNetwork::visOptions(highlightNearest =list(enabled = T, hover = T, degree=max(as.numeric(unique_slices))), nodesIdSelection = T) %>%
    visNetwork::visInteraction(dragNodes = TRUE, navigationButtons = F, hideEdgesOnDrag = TRUE, zoomSpeed = 0.4) %>%
    visNetwork::visOptions(manipulation = FALSE, height ="100%", width = "100%")
  
  return(VIS)
}

normalize_to_minus1_1 <- function(v) {
  2 * (v - min(v)) / (max(v) - min(v)) - 1
}