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
#' @param alpha Numeric. Balancing parameters between inclusion and importance measures. Default value is 0.5.
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
  alpha = 0.5
) {
  # Prepare data structure
  Nodes <- nexus$Nodes %>%
    mutate(
      color_assigned = FALSE,
      color_lineage = NA_integer_,
      color_final = NA_character_,
      has_natural_successor = FALSE,
      is_natural_lineage = FALSE # NUOVO FLAG
    )

  Edges <- nexus$Edges

  # # Calculate relative frequency for each node
  # Nodes <- Nodes %>%
  #   group_by(group) %>%
  #   mutate(rel_freq = freq / sum(freq, na.rm = TRUE)) %>%
  #   ungroup()

  # Add lineage strength to edges
  Edges <- Edges %>%
    # left_join(
    #   Nodes %>% select(id, rel_freq_from = rel_freq),
    #   by = c("from" = "id")
    # ) %>%
    mutate(
      lineage_strength = (alpha * Inc_Weighted) + (1 - alpha) * PageRankIndex
    )

  strong_edges <- Edges %>%
    dplyr::filter(Inc_Weighted >= threshold) %>%
    select(from, to, weight = lineage_strength, inc_weighted = Inc_Weighted)

  if (is.null(palette)) {
    palette <- c(
      "#E41A1C",
      "#377EB8",
      "#4DAF4A",
      "#984EA3",
      "#FF7F00",
      "#A65628",
      "#F781BF",
      "#66C2A5",
      "#FC8D62",
      "#8DA0CB",
      "#E78AC3",
      "#A6D854",
      "#FFD92F",
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

  periods <- unique(Nodes$group)
  lineage_id <- 1

  # ============================================================================
  # PHASE 1: Natural lineages (same label across periods)
  # ============================================================================

  unique_names <- unique(Nodes$name)

  for (node_name in unique_names) {
    same_name_nodes <- Nodes %>%
      dplyr::filter(name == node_name) %>%
      arrange(group)

    if (nrow(same_name_nodes) > 1) {
      # Assign the same lineage to all nodes with this name
      Nodes$color_lineage[Nodes$name == node_name] <- lineage_id
      Nodes$is_natural_lineage[Nodes$name == node_name] <- TRUE # NUOVO

      # Mark nodes that have a natural successor
      for (j in 1:(nrow(same_name_nodes) - 1)) {
        node_id <- same_name_nodes$id[j]
        Nodes$has_natural_successor[Nodes$id == node_id] <- TRUE
      }

      lineage_id <- lineage_id + 1
    }
  }

  # ============================================================================
  # PHASE 2: Algorithm-based lineages using lineage strength
  # ============================================================================

  for (i in 1:(length(periods) - 1)) {
    current_period <- periods[i]
    next_period <- periods[i + 1]

    current_nodes <- Nodes %>% dplyr::filter(group == current_period)

    for (node_id in current_nodes$id) {
      # SKIP nodes that already have a natural successor
      if (Nodes$has_natural_successor[Nodes$id == node_id]) {
        next
      }

      connections <- strong_edges %>%
        dplyr::filter(from == node_id) %>%
        arrange(desc(weight))

      if (nrow(connections) > 0) {
        main_successor <- connections$to[1]
        main_weight <- connections$weight[1]

        current_lineage <- Nodes$color_lineage[Nodes$id == node_id]
        successor_idx <- which(Nodes$id == main_successor)
        successor_lineage <- Nodes$color_lineage[successor_idx]
        successor_is_natural <- Nodes$is_natural_lineage[successor_idx] # NUOVO

        if (is.na(current_lineage)) {
          if (is.na(successor_lineage)) {
            Nodes$color_lineage[Nodes$id == node_id] <- lineage_id
            Nodes$color_lineage[successor_idx] <- lineage_id
            lineage_id <- lineage_id + 1
          } else {
            # NON sovrascrivere se il successore ha un lineage naturale
            if (!successor_is_natural) {
              # MODIFICATO
              Nodes$color_lineage[Nodes$id == node_id] <- successor_lineage
            }
          }
        } else {
          if (is.na(successor_lineage)) {
            Nodes$color_lineage[successor_idx] <- current_lineage
          } else if (successor_lineage != current_lineage) {
            # NON sovrascrivere se il successore ha un lineage naturale
            if (!successor_is_natural) {
              # NUOVO CONTROLLO
              competing_edges <- strong_edges %>%
                dplyr::filter(to == main_successor)

              if (nrow(competing_edges) > 0) {
                existing_weight <- max(competing_edges$weight, na.rm = TRUE)

                if (
                  main_weight > existing_weight ||
                    (main_weight == existing_weight &&
                      !any(
                        competing_edges$from[
                          competing_edges$weight == existing_weight
                        ] !=
                          node_id
                      ))
                ) {
                  Nodes$color_lineage[successor_idx] <- current_lineage
                }
              }
            }
          }
        }
      }
    }
  }

  # ============================================================================
  # PHASE 3: Assign unique colors to lineages
  # ============================================================================

  unique_lineages <- unique(na.omit(Nodes$color_lineage))

  for (i in seq_along(unique_lineages)) {
    lineage <- unique_lineages[i]

    if (i <= length(palette)) {
      color <- paste0(palette[i], "80")
    } else {
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

  # ============================================================================
  # PHASE 4: Assign unique colors to isolated nodes
  # ============================================================================

  isolated_nodes <- which(is.na(Nodes$color_lineage))
  color_idx <- length(unique_lineages) + 1

  for (idx in isolated_nodes) {
    if (color_idx <= length(palette)) {
      color <- paste0(palette[color_idx], "80")
    } else {
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

  # ============================================================================
  # PHASE 5: Assign colors to edges
  # ============================================================================

  Edges <- Edges %>%
    left_join(
      Nodes %>% select(id, color_from = color_final),
      by = c("from" = "id")
    ) %>%
    left_join(
      Nodes %>% select(id, color_to = color_final),
      by = c("to" = "id")
    ) %>%
    mutate(color = if_else(color_from == color_to, color_from, "#D3D3D380")) %>%
    select(-color_from, -color_to)

  Nodes <- Nodes %>%
    mutate(color = color_final) %>%
    select(
      -color_assigned,
      -color_lineage,
      -color_final,
      # -rel_freq,
      -has_natural_successor,
      -is_natural_lineage
    ) # AGGIORNATO

  nexus$Nodes <- Nodes
  nexus$Edges <- Edges

  # ============================================================================
  # Update Thematic Maps
  # ============================================================================

  for (i in 1:length(nexus$TM)) {
    res <- nexus$TM[[i]]
    newdf <- Nodes %>% dplyr::filter(slice == i) %>% select(name, color)
    df <- res$clusters %>% select(-color) %>% left_join(newdf, by = "name")
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

    color_map <- setNames(
      paste0(substr(new_edge_colors$color, 1, 7), "40"),
      new_edge_colors$color_original
    )

    E(g)$color <- color_map[E(g)$color]
    nexus$Net[[i]]$graph <- g
  }

  return(nexus)
}
