utils::globalVariables(c(
  "CL1",
  "CL2",
  "color_assigned",
  "color_final",
  "color_from",
  "color_lineage",
  "color_to",
  "freq",
  "measure",
  "total_in",
  "weight"
))

#' Plot a Thematic Evolution Analysis
#'
#' It plot a Thematic Evolution Analysis performed using the \code{\link{thematicEvolution}} function.
#'
#' @param Nodes is a list of nodes obtained by \code{\link{thematicEvolution}} function.
#' @param Edges is a list of edges obtained by \code{\link{thematicEvolution}} function.
#' @param measure is a character. It is deprecated and has no effect. The weight of the flows is always calculated using lineage_strength.
#' @param min.flow is numerical. It indicates the minimum value of measure to plot a flow.
#' @return a sankeyPlot
#'
#'
#' @examples
#'
#' \dontrun{
#' data(management, package = "bibliometrixData")
#' years=c(2004,2008,2015)
#'
#' nexus <- thematicEvolution(management,field="DE",years=years,n=100,minFreq=2)
#'
#' plotThematicEvolution(nexus$Nodes,nexus$Edges)
#' }
#'
#' @seealso \code{\link{thematicMap}} function to create a thematic map based on co-word network analysis and clustering.
#' @seealso \code{\link{thematicEvolution}} function to perform a thematic evolution analysis.
#' @seealso \code{\link{networkPlot}} to plot a bibliographic network.
#'
#' @export
#'
plotThematicEvolution <- function(
  Nodes,
  Edges,
  measure = "lineage_strength",
  min.flow = 0
) {
  # Calculate relative frequency for nodes if not already present
  if (!"rel_freq" %in% names(Nodes)) {
    Nodes <- Nodes %>%
      group_by(group) %>%
      mutate(rel_freq = freq / sum(freq, na.rm = TRUE) * 100) %>%
      ungroup()
  } else {
    # Convert to percentage if in [0,1] range
    if (max(Nodes$rel_freq, na.rm = TRUE) <= 1) {
      Nodes <- Nodes %>%
        mutate(rel_freq = rel_freq * 100)
    }
  }

  # Calculate lineage_strength if not present in Edges
  if (!"lineage_strength" %in% names(Edges)) {
    Edges <- Edges %>%
      # left_join(node_rel_freq, by = c("from" = "id")) %>%
      mutate(lineage_strength = Inc_Weighted)
  }

  # Store original lineage_strength for tooltips
  Edges$lineage_strength_display <- Edges$lineage_strength

  # Flag flows below the threshold. They are NOT removed: plotly assigns the
  # sankey nodes to columns from the graph topology, so dropping edges would
  # collapse the period layering and a cluster could be drawn in the wrong
  # period (e.g. a 2016-2025 topic shown among the 2011-2015 ones). We keep
  # every edge so the topology stays intact and hide the weak flows visually
  # instead (zero width + transparent colour, see below).
  Edges$is_weak <- !is.na(Edges$lineage_strength) &
    Edges$lineage_strength < min.flow

  # Use lineage_strength as weight
  Edges <- Edges %>%
    mutate(weight = lineage_strength)

  # Calculate coordinates
  Kx <- length(unique(Nodes$group))
  group_coords <- setNames(
    seq(from = 0, to = 1, length.out = Kx),
    sort(unique(Nodes$group))
  )

  Nodes <- Nodes %>%
    mutate(
      coordX = group_coords[as.character(group)],
      coordY = 0.1
    )

  # Scale weights for visualization
  Edges$weight <- Edges$weight * 100

  # Weak flows are kept only to preserve the layout topology: give them a
  # negligible width so they take no visible space.
  Edges$weight[Edges$is_weak] <- 1e-6

  # NOW reassign node IDs for plotly
  Nodes$old_id <- Nodes$id
  Nodes$id <- (1:nrow(Nodes)) - 1

  # Create mapping old_id -> new_id
  id_mapping <- setNames(Nodes$id, Nodes$old_id)

  # Update edge references using the mapping
  Edges$from <- id_mapping[as.character(Edges$from)]
  Edges$to <- id_mapping[as.character(Edges$to)]

  # Remove any edges with NA
  Edges <- Edges[!is.na(Edges$from) & !is.na(Edges$to), ]

  # Default edge color if not present
  if (!"color" %in% names(Edges)) {
    Edges$color <- "#D3D3D380"
  }

  # Hide the weak flows: kept for topology only, rendered fully transparent.
  Edges$color[Edges$is_weak] <- "rgba(0,0,0,0)"

  node_labels <- Nodes$name

  # Prepare tooltips
  node_customdata <- matrix(
    c(Nodes$freq, Nodes$rel_freq),
    nrow = nrow(Nodes),
    ncol = 2
  )

  node_hovertemplate <- paste0(
    "<b>%{label}</b><br>",
    "Frequency: %{customdata[0]:.0f}<br>",
    "Relative Frequency: %{customdata[1]:.2f}%<br>",
    "<extra></extra>"
  )

  # Prepare edge labels
  edge_labels <- character(nrow(Edges))
  edge_lineage <- numeric(nrow(Edges))

  for (i in 1:nrow(Edges)) {
    from_idx <- which(Nodes$id == Edges$from[i])[1]
    to_idx <- which(Nodes$id == Edges$to[i])[1]

    if (!is.na(from_idx) && !is.na(to_idx)) {
      edge_labels[i] <- paste0(
        Nodes$name[from_idx],
        " \u2192 ",
        Nodes$name[to_idx]
      )
      edge_lineage[i] <- Edges$lineage_strength_display[i]
    }
  }

  edge_customdata <- lapply(1:nrow(Edges), function(i) {
    list(edge_labels[i], edge_lineage[i])
  })

  # Plotly margins
  m <- list(l = 50, r = 50, b = 100, t = 100, pad = 4)

  # Create Sankey
  fig <- plotly::plot_ly(
    type = "sankey",
    arrangement = "snap",
    node = list(
      label = node_labels,
      x = Nodes$coordX,
      y = Nodes$coordY,
      color = Nodes$color,
      pad = 4,
      customdata = node_customdata,
      hovertemplate = node_hovertemplate
    ),
    link = list(
      source = Edges$from,
      target = Edges$to,
      value = Edges$weight,
      color = Edges$color,
      customdata = edge_customdata,
      hovertemplate = paste0(
        "<b>%{customdata[0]}</b><br>",
        "Lineage Strength: %{customdata[1]:.4f}<br>",
        "<extra></extra>"
      )
    )
  )

  fig <- fig %>%
    layout(margin = m) %>%
    plotly::add_annotations(
      x = unique(Nodes$coordX),
      y = 1.08,
      text = sort(unique(Nodes$group)),
      showarrow = FALSE,
      xanchor = "center",
      font = list(color = 'Dark', family = "TimesNewRoman", size = 18)
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        'toImage',
        'sendDataToCloud',
        'pan2d',
        'select2d',
        'lasso2d',
        'toggleSpikelines',
        'hoverClosestCartesian',
        'hoverCompareCartesian'
      )
    )

  return(fig)
}
