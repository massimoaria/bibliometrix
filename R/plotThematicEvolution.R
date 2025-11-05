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
#' @param measure is a character. It can be \code{measure=("inclusion","stability", "weighted")}.
#' @param min.flow is numerical. It indicates the minimum value of measure to plot a flow.
#' @return a sankeyPlot
#'
#'
#' @examples
#'
#' \dontrun{
#' data(scientometrics, package = "bibliometrixData")
#' years=c(2000)
#'
#' nexus <- thematicEvolution(scientometrics,field="ID",years=years,n=100,minFreq=2)
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
  measure = "inclusion",
  min.flow = 0
) {
  Kx <- length(unique(Nodes$group))
  Ky <- nrow(Nodes)
  Nodes <- Nodes %>%
    mutate(
      coordX = rep(
        seq(from = 0, to = 1, by = 1 / (Kx - 0.8)),
        as.numeric(table(.data$group))
      ),
      coordY = rep(0.1, Ky)
    )

  switch(
    measure,
    inclusion = {
      Edges = Edges[-c(4, 5)]
    },
    stability = {
      Edges = Edges[-c(3, 4)]
    },
    weighted = {
      Edges = Edges[, -c(3, 5)]
    }
  )
  names(Edges)[3] = "weight"
  Edges = Edges[Edges$weight >= min.flow, ]

  # Before multiplying by 100, balance edges with sum
  if ("sum" %in% names(Nodes)) {
    # For each node, calculate the sum of incoming edges
    in_flow <- Edges %>%
      group_by(to) %>%
      summarise(total_in = sum(weight), .groups = "drop")

    # For the last period, add the necessary difference
    periods <- unique(Nodes$group)
    last_period <- tail(periods, 1)
    last_nodes <- Nodes %>% dplyr::filter(group == last_period)

    for (i in 1:nrow(last_nodes)) {
      node_id <- last_nodes$id[i]
      node_sum <- last_nodes$sum[i]

      # Sum of incoming edges for this node
      current_in <- in_flow %>% dplyr::filter(to == node_id) %>% pull(total_in)
      if (length(current_in) == 0) {
        current_in <- 0
      }

      # If there's a discrepancy, add a balancing edge from the previous period
      diff <- node_sum - current_in
      if (abs(diff) > 0.001) {
        # Find a node from the previous period to start the flow from
        prev_period <- periods[length(periods) - 1]
        prev_nodes <- Nodes %>% dplyr::filter(group == prev_period)

        if (nrow(prev_nodes) > 0) {
          # Use the largest node from the previous period
          source_node <- prev_nodes %>%
            arrange(desc(sum)) %>%
            slice(1) %>%
            pull(id)

          balance_edge <- data.frame(
            from = source_node,
            to = node_id,
            weight = diff,
            group = paste0("balance_", node_id),
            color = "rgba(211,211,211,0.1)",
            stringsAsFactors = FALSE
          )
          Edges <- bind_rows(Edges, balance_edge)
        }
      }
    }
  }

  Edges$weight = Edges$weight * 100

  Nodes$id <- (1:nrow(Nodes)) - 1

  ## Identify and remove nodes with empty edges
  ind <- setdiff(Nodes$id, unique(c(Edges$from, Edges$to)))
  if (length(ind) > 0) {
    Nodes <- Nodes[-(ind + 1), ]
    Nodes$idnew <- (1:nrow(Nodes)) - 1
    ## Replace old edge ids with new ones
    for (i in 1:nrow(Nodes)) {
      old <- Nodes$id[i]
      new <- Nodes$idnew[i]
      Edges$from[Edges$from == old] <- new
      Edges$to[Edges$to == old] <- new
    }
  }

  # If color column doesn't exist in edges, use grey
  if (!"color" %in% names(Edges)) {
    Edges$color <- "#D3D3D380"
  }

  # Prepare custom data for node tooltips showing name and freq
  if ("freq" %in% names(Nodes)) {
    node_customdata <- Nodes$freq
    node_hovertemplate <- paste0(
      "<b>%{label}</b><br>",
      "Tot Occurrences: %{customdata}<br>",
      "<extra></extra>"
    )
  } else {
    node_customdata <- NULL
    node_hovertemplate <- NULL
  }

  # Plotly margins
  m <- list(
    l = 50,
    r = 50,
    b = 100,
    t = 100,
    pad = 4
  )

  plotly::plot_ly(
    type = "sankey",
    arrangement = "snap",
    node = list(
      label = Nodes$name,
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
      color = Edges$color
    )
  ) %>%
    layout(margin = m) %>%
    plotly::add_annotations(
      x = Nodes$coordX,
      y = 1.08,
      text = factor(Nodes$group),
      showarrow = F,
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
}

# assignEvolutionColors <- function(
#   Nodes,
#   Edges,
#   threshold = 0.5,
#   palette = NULL,
#   use_measure = "inclusion"
# ) {
#   switch(
#     use_measure,
#     inclusion = {
#       Edges$measure = Edges[, 3]
#     },
#     stability = {
#       Edges$measure = Edges[, 5]
#     },
#     weighted = {
#       Edges$measure = Edges[, 4]
#     }
#   )
#
#   # Initialize palette with enough unique colors
#   if (is.null(palette)) {
#     palette <- c(
#       "#E41A1C",
#       "#377EB8",
#       "#4DAF4A",
#       "#984EA3",
#       "#FF7F00",
#       "#A65628",
#       "#F781BF",
#       # "#999999",
#       "#66C2A5",
#       "#FC8D62",
#       "#8DA0CB",
#       "#E78AC3",
#       "#A6D854",
#       "#FFD92F",
#       # "#B3B3B3",
#       "#A6CEE3",
#       "#1F78B4",
#       "#B2DF8A",
#       "#33A02C",
#       "#FB9A99",
#       "#E31A1C",
#       "#FDBF6F",
#       "#FF7F00",
#       "#CAB2D6",
#       "#6A3D9A",
#       "#B15928",
#       "#8DD3C7",
#       "#BEBADA",
#       "#FB8072",
#       "#80B1D3",
#       "#FDB462",
#       "#B3DE69",
#       # "#D9D9D9",  # rimosso grigio
#       "#BC80BD",
#       "#CCEBC5",
#       "#FFB6C1",
#       "#DDA0DD",
#       "#F0E68C",
#       "#87CEEB",
#       "#98D8C8",
#       "#F7DC6F",
#       "#FAD7A0",
#       "#D7BDE2",
#       "#A9DFBF",
#       "#F8B88B",
#       "#AED6F1",
#       "#F9E79F",
#       "#C39BD3",
#       "#73C6B6",
#       "#F5B7B1"
#     )
#   }
#
#   # Prepare data structure
#   Nodes <- Nodes %>%
#     mutate(
#       color_assigned = FALSE,
#       color_lineage = NA_integer_,
#       color_final = NA_character_
#     )
#
#   # Extract strong connections
#   strong_edges <- Edges %>%
#     dplyr::filter(measure >= threshold) %>%
#     select(from, to, weight = measure)
#
#   # PHASE 1: Identify lineages starting from the first period
#   periods <- unique(Nodes$group)
#   lineage_id <- 1
#
#   for (i in 1:(length(periods) - 1)) {
#     current_period <- periods[i]
#     next_period <- periods[i + 1]
#
#     current_nodes <- Nodes %>% dplyr::filter(group == current_period)
#     next_nodes <- Nodes %>% dplyr::filter(group == next_period)
#
#     for (node_id in current_nodes$id) {
#       # Find strong connections for this node
#       connections <- strong_edges %>%
#         dplyr::filter(from == node_id) %>%
#         arrange(desc(weight))
#
#       if (nrow(connections) > 0) {
#         # Take the successor with the highest weight
#         main_successor <- connections$to[1]
#
#         # If the current node already has a lineage, propagate it
#         current_lineage <- Nodes$color_lineage[Nodes$id == node_id]
#
#         if (is.na(current_lineage)) {
#           # New lineage
#           Nodes$color_lineage[Nodes$id == node_id] <- lineage_id
#           current_lineage <- lineage_id
#           lineage_id <- lineage_id + 1
#         }
#
#         # Propagate to successor (if it doesn't already have a stronger lineage)
#         successor_idx <- which(Nodes$id == main_successor)
#         if (is.na(Nodes$color_lineage[successor_idx])) {
#           Nodes$color_lineage[successor_idx] <- current_lineage
#         } else {
#           # Conflict: check which connection is stronger
#           existing_lineage <- Nodes$color_lineage[successor_idx]
#           existing_weight <- strong_edges %>%
#             dplyr::filter(
#               to == main_successor,
#               from %in%
#                 (Nodes %>%
#                   dplyr::filter(color_lineage == existing_lineage) %>%
#                   pull(id))
#             ) %>%
#             pull(weight) %>%
#             max(na.rm = TRUE)
#
#           if (connections$weight[1] > existing_weight) {
#             Nodes$color_lineage[successor_idx] <- current_lineage
#           }
#         }
#       }
#     }
#   }
#
#   # PHASE 1.5: Assign same lineage to nodes with same name across periods
#   # (only if they don't have strong edges to other nodes)
#   unique_names <- unique(Nodes$name)
#
#   for (node_name in unique_names) {
#     # Get all nodes with this name across all periods
#     same_name_nodes <- Nodes %>% dplyr::filter(name == node_name)
#
#     if (nrow(same_name_nodes) > 1) {
#       # Check which nodes already have a lineage assigned
#       assigned_lineages <- unique(na.omit(same_name_nodes$color_lineage))
#
#       if (length(assigned_lineages) == 0) {
#         # No lineage assigned yet: create a new one for all
#         new_lineage <- lineage_id
#         lineage_id <- lineage_id + 1
#         Nodes$color_lineage[Nodes$name == node_name] <- new_lineage
#       } else if (length(assigned_lineages) == 1) {
#         # One lineage already assigned: propagate to unassigned nodes
#         target_lineage <- assigned_lineages[1]
#         unassigned_indices <- which(
#           Nodes$name == node_name & is.na(Nodes$color_lineage)
#         )
#         Nodes$color_lineage[unassigned_indices] <- target_lineage
#       }
#       # If length(assigned_lineages) > 1: nodes have different strong connections,
#       # keep them separate
#     }
#   }
#
#   # PHASE 2: Assign unique colors to lineages (no color reuse)
#   unique_lineages <- unique(na.omit(Nodes$color_lineage))
#
#   for (i in seq_along(unique_lineages)) {
#     lineage <- unique_lineages[i]
#
#     # Get next available color from palette (each lineage gets unique color)
#     if (i <= length(palette)) {
#       color <- paste0(palette[i], "80")
#     } else {
#       # Generate random color if palette exhausted
#       color <- paste0(
#         sprintf(
#           "#%02X%02X%02X",
#           sample(50:255, 1),
#           sample(50:255, 1),
#           sample(50:255, 1)
#         ),
#         "80"
#       )
#     }
#
#     Nodes$color_final[Nodes$color_lineage == lineage] <- color
#   }
#
#   # PHASE 3: Assign unique colors to isolated nodes (no color reuse)
#   isolated_nodes <- which(is.na(Nodes$color_lineage))
#   color_idx <- length(unique_lineages) + 1
#
#   for (idx in isolated_nodes) {
#     if (color_idx <= length(palette)) {
#       color <- paste0(palette[color_idx], "80")
#     } else {
#       # Generate random color if palette exhausted
#       color <- paste0(
#         sprintf(
#           "#%02X%02X%02X",
#           sample(50:255, 1),
#           sample(50:255, 1),
#           sample(50:255, 1)
#         ),
#         "80"
#       )
#     }
#
#     Nodes$color_final[idx] <- color
#     color_idx <- color_idx + 1
#   }
#
#   # PHASE 4: Assign colors to edges
#   Edges <- Edges %>%
#     left_join(
#       Nodes %>% select(id, color_from = color_final),
#       by = c("from" = "id")
#     ) %>%
#     left_join(
#       Nodes %>% select(id, color_to = color_final),
#       by = c("to" = "id")
#     ) %>%
#     mutate(
#       color = if_else(color_from == color_to, color_from, "#D3D3D380")
#     ) %>%
#     select(-color_from, -color_to, -measure)
#
#   # Return Nodes with updated color column
#   Nodes <- Nodes %>%
#     mutate(color = color_final) %>%
#     select(-color_assigned, -color_lineage, -color_final)
#
#   return(list(Nodes = Nodes, Edges = Edges))
# }
