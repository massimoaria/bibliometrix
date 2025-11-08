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
