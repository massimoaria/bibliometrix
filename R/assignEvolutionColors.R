utils::globalVariables(c(
  "cluster",
  "color_original",
  "values",
  "vertex"
))

#' Assign Colors to Thematic Evolution Nodes Based on Lineages
#'
#' This function assigns colors to nodes in a thematic evolution analysis based on
#' their lineages across time periods. Nodes connected by strong edges (above a threshold)
#' receive the same color to visualize thematic continuity. Nodes with the same name
#' across periods that are not strongly connected to other nodes are also colored identically.
#'
#' @param nexus A list object returned by \code{\link{thematicEvolution}} containing:
#'   \itemize{
#'     \item \code{Nodes}: data frame with node information (name, group, id, sum, freq, etc.)
#'     \item \code{Edges}: data frame with edge information (from, to, weight measures)
#'     \item \code{TM}: list of thematic maps for each period
#'   }
#' @param threshold Numeric. The minimum weight value for an edge to be considered a
#'   "strong connection" (default: 0.5). Edges with weights >= threshold will propagate
#'   the same color to connected nodes across periods.
#' @param palette Character vector. Optional custom color palette as hex codes. If NULL,
#'   uses a default palette of 50+ distinct colors. Colors are assigned sequentially
#'   without reuse.
#' @param use_measure Character. The measure to use for determining edge strength.
#'   Can be one of:
#'   \itemize{
#'     \item \code{"inclusion"}: uses the Inclusion measure (column 3 of Edges)
#'     \item \code{"stability"}: uses the Stability measure (column 5 of Edges)
#'     \item \code{"weighted"}: uses the weighted Inclusion measure (column 4 of Edges)
#'   }
#'   Default is "inclusion".
#'
#' @return Returns the modified \code{nexus} object with updated colors:
#'   \itemize{
#'     \item \code{Nodes$color}: updated with lineage-based colors
#'     \item \code{Edges$color}: edges connecting same-colored nodes receive the node color,
#'       others are grey
#'     \item \code{TM}: thematic maps updated with new cluster colors
#'   }
#'
#' @details
#' The function uses a multi-phase algorithm:
#' \enumerate{
#'   \item \strong{Phase 1}: Identifies lineages by following strong connections (weight >= threshold)
#'     from the first period forward. When a node has multiple strong connections, the
#'     strongest one determines the lineage.
#'   \item \strong{Phase 1.5}: Assigns the same lineage to nodes with identical names across
#'     periods if they are not already part of different strong connections.
#'   \item \strong{Phase 2}: Assigns unique colors from the palette to each identified lineage.
#'   \item \strong{Phase 3}: Assigns unique colors to isolated nodes (those without any lineage).
#'   \item \strong{Phase 4}: Colors edges based on their connected nodes - same color if both
#'     nodes share a color, grey otherwise.
#'   \item \strong{Final}: Updates thematic maps with the new color scheme.
#' }
#'
#' Each lineage receives a unique color from the palette. No color is reused across
#' different lineages, ensuring clear visual distinction between independent thematic streams.
#'
#' @examples
#' \dontrun{
#' data(scientometrics, package = "bibliometrixData")
#' years <- c(2000, 2010)
#'
#' nexus <- thematicEvolution(scientometrics, field = "ID",
#'                             years = years, n = 100, minFreq = 2)
#'
#'
#' # Use custom threshold and measure
#' nexus <- assignEvolutionColors(nexus, threshold = 0.6, use_measure = "weighted")
#'
#' }
#'
#' @seealso \code{\link{thematicEvolution}} to perform thematic evolution analysis.
#' @seealso \code{\link{plotThematicEvolution}} to visualize the colored evolution.
#'
#' @export

assignEvolutionColors <- function(
  nexus,
  threshold = 0.5,
  palette = NULL,
  use_measure = "weighted"
) {
  # Prepare data structure
  Nodes <- nexus$Nodes %>%
    mutate(
      color_assigned = FALSE,
      color_lineage = NA_integer_,
      color_final = NA_character_
    )

  Edges <- nexus$Edges

  switch(
    use_measure,
    inclusion = {
      Edges$measure = Edges[, 3]
    },
    stability = {
      Edges$measure = Edges[, 5]
    },
    weighted = {
      Edges$measure = Edges[, 4]
    }
  )

  # Initialize palette with enough unique colors
  if (is.null(palette)) {
    palette <- c(
      "#E41A1C",
      "#377EB8",
      "#4DAF4A",
      "#984EA3",
      "#FF7F00",
      "#A65628",
      "#F781BF",
      # "#999999",
      "#66C2A5",
      "#FC8D62",
      "#8DA0CB",
      "#E78AC3",
      "#A6D854",
      "#FFD92F",
      # "#B3B3B3",
      "#A6CEE3",
      "#1F78B4",
      "#B2DF8A",
      "#33A02C",
      "#FB9A99",
      "#E31A1C",
      "#FDBF6F",
      "#FF7F00",
      "#CAB2D6",
      "#6A3D9A",
      "#B15928",
      "#8DD3C7",
      "#BEBADA",
      "#FB8072",
      "#80B1D3",
      "#FDB462",
      "#B3DE69",
      # "#D9D9D9",  # rimosso grigio
      "#BC80BD",
      "#CCEBC5",
      "#FFB6C1",
      "#DDA0DD",
      "#F0E68C",
      "#87CEEB",
      "#98D8C8",
      "#F7DC6F",
      "#FAD7A0",
      "#D7BDE2",
      "#A9DFBF",
      "#F8B88B",
      "#AED6F1",
      "#F9E79F",
      "#C39BD3",
      "#73C6B6",
      "#F5B7B1"
    )
  }

  # Extract strong connections
  strong_edges <- Edges %>%
    dplyr::filter(measure >= threshold) %>%
    select(from, to, weight = measure)

  # PHASE 1: Identify lineages starting from the first period
  periods <- unique(Nodes$group)
  lineage_id <- 1

  for (i in 1:(length(periods) - 1)) {
    current_period <- periods[i]
    next_period <- periods[i + 1]

    current_nodes <- Nodes %>% dplyr::filter(group == current_period)
    next_nodes <- Nodes %>% dplyr::filter(group == next_period)

    for (node_id in current_nodes$id) {
      # Find strong connections for this node
      connections <- strong_edges %>%
        dplyr::filter(from == node_id) %>%
        arrange(desc(weight))

      if (nrow(connections) > 0) {
        # Take the successor with the highest weight
        main_successor <- connections$to[1]

        # If the current node already has a lineage, propagate it
        current_lineage <- Nodes$color_lineage[Nodes$id == node_id]

        if (is.na(current_lineage)) {
          # New lineage
          Nodes$color_lineage[Nodes$id == node_id] <- lineage_id
          current_lineage <- lineage_id
          lineage_id <- lineage_id + 1
        }

        # Propagate to successor (if it doesn't already have a stronger lineage)
        successor_idx <- which(Nodes$id == main_successor)
        if (is.na(Nodes$color_lineage[successor_idx])) {
          Nodes$color_lineage[successor_idx] <- current_lineage
        } else {
          # Conflict: check which connection is stronger
          existing_lineage <- Nodes$color_lineage[successor_idx]
          existing_weight <- strong_edges %>%
            dplyr::filter(
              to == main_successor,
              from %in%
                (Nodes %>%
                  dplyr::filter(color_lineage == existing_lineage) %>%
                  pull(id))
            ) %>%
            pull(weight) %>%
            max(na.rm = TRUE)

          if (connections$weight[1] > existing_weight) {
            Nodes$color_lineage[successor_idx] <- current_lineage
          }
        }
      }
    }
  }

  # PHASE 1.5: Assign same lineage to nodes with same name across periods
  # (only if they don't have strong edges to other nodes)
  unique_names <- unique(Nodes$name)

  for (node_name in unique_names) {
    # Get all nodes with this name across all periods
    same_name_nodes <- Nodes %>% dplyr::filter(name == node_name)

    if (nrow(same_name_nodes) > 1) {
      # Check which nodes already have a lineage assigned
      assigned_lineages <- unique(na.omit(same_name_nodes$color_lineage))

      if (length(assigned_lineages) == 0) {
        # No lineage assigned yet: create a new one for all
        new_lineage <- lineage_id
        lineage_id <- lineage_id + 1
        Nodes$color_lineage[Nodes$name == node_name] <- new_lineage
      } else if (length(assigned_lineages) == 1) {
        # One lineage already assigned: propagate to unassigned nodes
        target_lineage <- assigned_lineages[1]
        unassigned_indices <- which(
          Nodes$name == node_name & is.na(Nodes$color_lineage)
        )
        Nodes$color_lineage[unassigned_indices] <- target_lineage
      }
      # If length(assigned_lineages) > 1: nodes have different strong connections,
      # keep them separate
    }
  }

  # PHASE 2: Assign unique colors to lineages (no color reuse)
  unique_lineages <- unique(na.omit(Nodes$color_lineage))

  for (i in seq_along(unique_lineages)) {
    lineage <- unique_lineages[i]

    # Get next available color from palette (each lineage gets unique color)
    if (i <= length(palette)) {
      color <- paste0(palette[i], "80")
    } else {
      # Generate random color if palette exhausted
      color <- paste0(
        sprintf(
          "#%02X%02X%02X",
          sample(50:255, 1),
          sample(50:255, 1),
          sample(50:255, 1)
        ),
        "80"
      )
    }

    Nodes$color_final[Nodes$color_lineage == lineage] <- color
  }

  # PHASE 3: Assign unique colors to isolated nodes (no color reuse)
  isolated_nodes <- which(is.na(Nodes$color_lineage))
  color_idx <- length(unique_lineages) + 1

  for (idx in isolated_nodes) {
    if (color_idx <= length(palette)) {
      color <- paste0(palette[color_idx], "80")
    } else {
      # Generate random color if palette exhausted
      color <- paste0(
        sprintf(
          "#%02X%02X%02X",
          sample(50:255, 1),
          sample(50:255, 1),
          sample(50:255, 1)
        ),
        "80"
      )
    }

    Nodes$color_final[idx] <- color
    color_idx <- color_idx + 1
  }

  # PHASE 4: Assign colors to edges
  Edges <- Edges %>%
    left_join(
      Nodes %>% select(id, color_from = color_final),
      by = c("from" = "id")
    ) %>%
    left_join(
      Nodes %>% select(id, color_to = color_final),
      by = c("to" = "id")
    ) %>%
    mutate(
      color = if_else(color_from == color_to, color_from, "#D3D3D380")
    ) %>%
    select(-color_from, -color_to, -measure)

  # Return Nodes with updated color column
  Nodes <- Nodes %>%
    mutate(color = color_final) %>%
    select(-color_assigned, -color_lineage, -color_final)

  nexus$Nodes <- Nodes
  nexus$Edges <- Edges

  for (i in 1:length(nexus$TM)) {
    res <- nexus$TM[[i]]

    ## Update Thematic Maps with new colors
    newdf <- Nodes %>%
      dplyr::filter(slice == i) %>%
      select(name, color)

    df <- res$clusters %>%
      select(-color) %>%
      left_join(newdf, by = "name")

    ## replace color NA with light grey
    df$color[is.na(df$color)] <- "#D3D3D3"

    gplot <- buildTMPlot(
      df = df,
      meandens = res$axisOrigin[2],
      meancentr = res$axisOrigin[1],
      xlimits = res$axisLimits[1:2],
      ylimits = res$axisLimits[3:4],
      annotations = res$annotations,
      size = res$params %>%
        dplyr::filter(params == "size") %>%
        pull(values) %>%
        as.numeric(),
      repel = nexus$TM[[i]]$params %>%
        dplyr::filter(params == "repel") %>%
        pull(values) %>%
        as.logical()
    )
    nexus$TM[[i]]$map <- gplot

    g <- nexus$Net[[i]]$graph
    # net <- nexus$TM[[i]]$net

    clusters <- nexus$Net[[i]]$cluster_res %>%
      group_by(cluster) %>%
      slice_head(n = 1) %>%
      select(vertex, cluster) %>%
      rename(name = vertex, slice = cluster) %>%
      ungroup() %>%
      left_join(
        data.frame(name = V(g)$name, color_original = V(g)$color),
        by = "name"
      ) %>%
      left_join(newdf, by = "name")

    add_colors <- setdiff(palette, substr(clusters$color, 1, 7))
    clusters$color[is.na(clusters$color)] <- paste0(
      add_colors[1:length(which(is.na(clusters$color)))],
      80
    )

    V(g)$color <- clusters$color[match(V(g)$community, clusters$slice)]

    # using clusters, replace color_original with color in edges
    old_edge_colors <- data.frame(
      from = as.character(igraph::as_edgelist(g)[, 1]),
      to = as.character(igraph::as_edgelist(g)[, 2]),
      color_original = E(g)$color
    )
    new_edge_colors <- old_edge_colors %>%
      left_join(
        clusters %>%
          select(color_original, color) %>%
          mutate(color_original = paste0(substr(color_original, 1, 7), "40")),
        by = "color_original"
      ) %>%
      mutate(
        color = ifelse(is.na(color), "#B3B3B380", color),
        color = ifelse(color_original == "#B3B3B340", "#B3B3B380", color)
      )

    # Create a color map
    color_map <- setNames(
      paste0(substr(new_edge_colors$color, 1, 7), "40"),
      new_edge_colors$color_original
    )

    # Replace original
    E(g)$color <- color_map[E(g)$color]

    nexus$Net[[i]]$graph <- g
  }

  return(nexus)
}
