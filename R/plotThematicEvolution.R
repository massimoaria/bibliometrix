utils::globalVariables(c(
  "group", "CL1", "CL2", "value", "visEdges", "visIgraphLayout", "visNetwork", "visOptions",
  "visPhysics", "title","fixed.x", "fixed.y"
))
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
#' @param label_size Numeric. Font size of the node labels. Default is `15`.
#' @param edge_scale Numeric. Scaling factor for edge width. Default is `10`.
#' @param node_scale Numeric. Scaling factor for node size. Default is `30`.
#'
#' @return A `visNetwork` object displaying a time-structured thematic evolution network.
#'
#'
#' @examples
#' \dontrun{
#' data(managemeent, package = "bibliometrixData")
#' years <- c(2004, 2015)
#'
#' nexus <- thematicEvolution(management, field = "ID", years = years, n = 100, minFreq = 2)
#'
#' plotThematicEvolution(nexus$Nodes, nexus$Edges)
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
                                  measure = "weighted", # "inclusion", "stability", "weighted"
                                  label_size = 5,
                                  edge_scale = 10,
                                  node_scale = 30) {
  switch(measure,
         inclusion = {
           edge_weight_var <- "Inclusion"
         },
         stability = {
           edge_weight_var <- "Stability"
         },
         weighted = {
           edge_weight_var <- "Inc_Weighted"
         })
  
  Edges$Stability <- Edges$Stability * 10
  
  # Coordinate X per slice temporale
  unique_slices <- sort(unique(Nodes$slice))
  x_positions <- normalize_to_minus1_1(setNames((seq_along(unique_slices) - 1), unique_slices))
  Nodes <- Nodes %>%
    mutate(
      x = x_positions[as.character(slice)],
      label = sub("--\\d{4}-\\d{4}$", "", label) # rimuove periodo dalla label
    )
  
  # Coordinate Y per evitare sovrapposizione
  Nodes <- Nodes %>%
    group_by(slice) %>%
    arrange(name) %>%
    mutate(y = normalize_to_minus1_1(seq(from = 100, by = 100, length.out = n()))) %>%
    ungroup()
  
  # Prepara nodi per visNetwork
  Nodes_vis <- Nodes %>%
    mutate(
      shape = "box",
      size = sum * node_scale,
      value = sum,
      fixed.x = TRUE,
      title = label,
      fixed.y = FALSE
    ) %>%
    select(id, label, title, group, color, x, y, shape, size, value, fixed.x, fixed.y) %>%
    data.frame()
  
  # Font size coerente con size (altezza)
  min_font <- 15
  max_font <- 25
  min_size <- min(Nodes_vis$size)
  max_size <- max(Nodes_vis$size)
  
  Nodes_vis$font <- purrr::map(Nodes_vis$size, function(s) {
    size_font <- min_font + (s - min_size) / (max_size - min_size) * (max_font - min_font)
    list(size = size_font, vadjust = 0)
  })
  
  # Aggiungi nodi intestazione (per ogni periodo)
  header_nodes <- Nodes %>%
    distinct(slice, .keep_all = TRUE) %>%
    mutate(
      id = 10000 + row_number(),  # id sicuramente unico
      label = as.character(group),
      x = x_positions[as.character(slice)],
      y = -1.3,
      shape = "text",
      size = 1,
      value = NA,
      color = "rgba(255,255,255,0)",
      fixed.x = TRUE,
      fixed.y = TRUE
    ) %>%
    select(id, label, group, color, x, y, shape, size, value, fixed.x, fixed.y)
  
  header_nodes$font <- purrr::map(rep(25, nrow(header_nodes)), ~ list(size = .x, vadjust = -30))
  
  Nodes_vis <- bind_rows(Nodes_vis, header_nodes)
  
  # Edges
  edges_vis <- Edges %>%
    mutate(
      width = !!sym(edge_weight_var) * edge_scale,
      value = !!sym(edge_weight_var)
    ) %>%
    dplyr::filter(value >= min.flow)
  
  edges_vis$color <- lapply(1:nrow(edges_vis), function(i) list(color = "#D3D3D3", highlight = "#35343370", hover = "#35343370"))
  
  coords <- as.matrix(Nodes_vis %>% select(x, y))
  
  # Net Building
  VIS <- visNetwork(Nodes_vis, edges_vis, type = "full", smooth = TRUE) %>%
    visNodes(shape = "box", widthConstraint = list(minimum = 200, maximum = 200)) %>%
    visEdges(smooth = list(type = "horizontal"), arrows = "to", scaling = list(min = 1, max = edge_scale)) %>%
    visIgraphLayout(layout = "layout.norm", layoutMatrix = coords, type = "full") %>%
    visPhysics(enabled = FALSE) %>%
    visOptions(highlightNearest = list(
      enabled = TRUE, hover = TRUE, degree = 1,
      algorithm = "all", hideColor = "rgba(200, 200, 200, 0.90)"
    ), nodesIdSelection = FALSE) %>%
    visInteraction(dragNodes = TRUE, navigationButtons = FALSE, hideEdgesOnDrag = TRUE, zoomSpeed = 0.4) %>%
    visOptions(manipulation = FALSE, height = "100%", width = "100%")
  
  return(VIS)
}

normalize_to_minus1_1 <- function(v) {
  2 * (v - min(v)) / (max(v) - min(v)) - 1
}