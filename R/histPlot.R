utils::globalVariables(c(
  "from", "to", "color", "from_name", "from_title", "from_keywords",
  "from_keywordsplus", "from_id", "from_size", "from_years",
  "to_name", "to_title", "to_keywords", "from2", "x", "y", "color.x",
  "to_keywordsplus", "to_id", "to_size", "to_years", "color.y",
  "Title", "DOI", "LCS", "GCS", "xend", "yend", "text", "color_v", "id"
))
#' Plotting historical co-citation network
#'
#' \code{histPlot} plots a historical co-citation network.
#'
#' The function \code{\link{histPlot}} can plot a historical co-citation network previously created by \code{\link{histNetwork}}.
#' @param histResults is an object of \code{class} "list" containing the following components:
#'
#' \tabular{lll}{
#' NetMatrix \tab  \tab the historical citation network matrix\cr
#' Degree \tab       \tab the min degree of the network\cr
#' histData \tab      \tab the set of n most cited references\cr
#' M \tab      \tab the bibliographic data frame}
#'
#' is a network matrix obtained by the function \code{\link{histNetwork}}.
#' @param n is integer. It defines the number of vertices to plot.
#' @param size is an integer. It defines the point size of the vertices. Default value is 5.
#' @param labelsize is an integer. It indicates the label size in the plot. Default is \code{labelsize=5}.
#' @param remove.isolates is logical. If TRUE isolates vertices are not plotted.
#' @param title_as_label is a logical. DEPRECATED
#' @param label is a character. It indicates which label type to use as node id in the historiograph. It can be \code{label=c("short", "title", "keywords", "keywordsplus")}.
#' Default is \code{label = "short"}.
#' @param verbose is logical. If TRUE, results and plots are printed on screen.
#' @return It is list containing: a network object of the class \code{igraph} and a plot object of the class \code{ggraph}.
#'
#' @examples
#' # EXAMPLE Citation network
#' \dontrun{
#' data(management, package = "bibliometrixData")
#'
#' histResults <- histNetwork(management, sep = ";")
#'
#' net <- histPlot(histResults, n = 20, labelsize = 5)
#' }
#'
#' @seealso \code{\link{histNetwork}} to compute a historical co-citation network.
#' @seealso \code{\link{cocMatrix}} to compute a co-occurrence matrix.
#' @seealso \code{\link{biblioAnalysis}} to perform a bibliometric analysis.
#'
#' @export
histPlot <- function(histResults, n = 20, size = 5, labelsize = 5, remove.isolates = TRUE, title_as_label = FALSE, label = "short", verbose = TRUE) {
  params <- list(
    n = n,
    size = size,
    labelsize = labelsize,
    title_as_label = title_as_label,
    label = label
  )

  colorlist <- colorlist()
  # c(brewer.pal(9, 'Set1')[-6], brewer.pal(8, 'Set2')[-7], brewer.pal(12, 'Paired')[-11],brewer.pal(12, 'Set3')[-c(2,8,12)])
  ## legacy with old argument size
  if (isTRUE(size)) {
    size <- 5
  }

  # LCS=histResults$LCS
  LCS <- colSums(histResults$NetMatrix)
  NET <- histResults$NetMatrix
  ## selecting the first n vertices or all if smaller
  s <- sort(LCS, decreasing = TRUE)[min(n, length(LCS))]
  ind <- which(LCS >= s)
  # NET=NET[a,names(ind)]
  NET <- NET[names(ind), names(ind)]
  LCS <- LCS[ind]

  # Create igraph object
  bsk.network <- graph_from_adjacency_matrix(NET, mode = c("directed"), weighted = NULL)

  R <- strsplit(names(V(bsk.network)), ",")
  RR <- lapply(R, function(l) {
    l <- l[1:2]
    l <- paste(l[1], l[2], sep = ",")
  })

  # add label to nodes
  V(bsk.network)$title <- histResults$histData$Title[ind]
  V(bsk.network)$keywords <- histResults$histData$Author_Keywords[ind]
  V(bsk.network)$keywordsplus <- histResults$histData$KeywordsPlus[ind]

  switch(label,
    title = {
      title <- strsplit(stringi::stri_trans_totitle(V(bsk.network)$title), " ")
      V(bsk.network)$id <- unlist(lapply(title, function(l) {
        n <- floor(length(l) / 2)
        paste0(paste(l[1:n], collapse = " ", sep = ""), "\n", paste(l[(n + 1):length(l)], collapse = " ", sep = ""))
      }))
    },
    keywords = {
      kw <- strsplit(stringi::stri_trans_totitle(V(bsk.network)$keywords), ";")
      kw[is.na(kw)] <- "Not Available"
      V(bsk.network)$id <- unlist(lapply(kw, function(l) {
        if (length(l) > 1) {
          n <- floor(length(l) / 2)
          l <- trimws(l)
          paste0(paste(l[1:n], collapse = "; ", sep = ""), "\n", paste(l[(n + 1):length(l)], collapse = "; ", sep = ""))
        } else {
          l
        }
      }))
    },
    keywordsplus = {
      kw <- strsplit(stringi::stri_trans_totitle(V(bsk.network)$keywordsplus), ";")
      kw[is.na(kw)] <- "Not Available"
      V(bsk.network)$id <- unlist(lapply(kw, function(l) {
        if (length(l) > 1) {
          n <- floor(length(l) / 2)
          l <- trimws(l)
          paste0(paste(l[1:n], collapse = "; ", sep = ""), "\n", paste(l[(n + 1):length(l)], collapse = "; ", sep = ""))
        } else {
          l
        }
      }))
    },
    {
      V(bsk.network)$id <- tolower(unlist(RR))
    }
  )

  # Compute node degrees (#links) and use that to set node size:
  deg <- LCS
  V(bsk.network)$size <- size
  # rep(size,length(V(bsk.network)))}

  # Years=histResults$histData$Year[ind]
  Years <- as.numeric(unlist(stringi::stri_extract_all_regex(unlist(RR), "[[:digit:]]{4}$")))
  V(bsk.network)$years <- Years

  # Remove loops
  bsk.network <- igraph::simplify(bsk.network, remove.multiple = T, remove.loops = T)


  # define network layout

  E(bsk.network)$color <- "slategray1"

  if (isTRUE(remove.isolates)) bsk.network <- delete.isolates(bsk.network)

  dg <- decompose.graph(bsk.network)

  layout_m <- as.data.frame(layout.fruchterman.reingold(bsk.network))
  names(layout_m) <- c("x", "y")
  layout_m$name <- V(bsk.network)$name
  layout_m$years <- V(bsk.network)$years
  layout_m$cluster <- 0
  rr <- 0
  for (k in 1:length(dg)) {
    bsk <- dg[[k]]

    a <- ifelse(layout_m$name %in% V(bsk)$name, k, 0)
    layout_m$cluster <- layout_m$cluster + a
    Min <- min(layout_m$y[layout_m$cluster == k]) - 1
    layout_m$y[layout_m$cluster == k] <- layout_m$y[layout_m$cluster == k] + (rr - Min)
    rr <- max(layout_m$y[layout_m$cluster == k])
  }
  # bsk <- bsk.network
  wp <- membership(cluster_infomap(bsk.network, modularity = FALSE))
  layout_m$color <- colorlist[wp]
  layout_m$x <- layout_m$years
  layout_m$y <- (diff(range(layout_m$x)) / diff(range(layout_m$y))) * layout_m$y

  df_net <- igraph::as_long_data_frame(bsk.network)
  df_net$color <- "slategray1"

  ID <- setdiff(df_net$to, df_net$from)

  df_from <- df_net %>%
    select(from, to, color, from_name, from_title, from_keywords, from_keywordsplus, from_id, from_size, from_years)

  df_to <- df_net %>%
    dplyr::filter(to %in% ID) %>%
    mutate(from2 = to) %>%
    select(from2, to, color, to_name, to_title, to_keywords, to_keywordsplus, to_id, to_size, to_years)

  df_to <- df_to[!duplicated(df_to$to), ]

  label <- c("from", "to", "color", "name", "title", "keywords", "keywordsplus", "id", "size", "years")

  names(df_from) <- label
  names(df_to) <- label

  df_net <- rbind(df_from, df_to)

  layout_norm <- layout_m %>%
    mutate(
      x = (x - min(x)) / (max(x) - min(x)),
      y = (y - min(y)) / (max(y) - min(y))
    )
  df_net <- left_join(df_net, layout_norm[c("name", "color", "x", "y")], by = c("name" = "name")) %>%
    rename(
      color = color.x,
      color_v = color.y
    )

  df_coord <- layout_norm %>%
    mutate(to = row_number()) %>%
    select(to, x, y) %>%
    rename(
      xend = x,
      yend = y
    )

  df_net <- df_net %>%
    left_join(df_coord, by = "to")

  ylength <- diff(range(df_net$years)) + 1
  Ylabel <- (as.character(seq(min(df_net$years), max(df_net$years), length.out = ylength)))
  Breaks <- (seq(0, 1, length.out = ylength))

  df_net <- df_net %>%
    left_join(histResults$histData, by = c("name" = "Paper")) # %>%

  #  Title <- strsplit(df_net$title, "(?<=.{40})", perl = TRUE)
  Title <- gsub("(.{40})", "\\1\n", df_net$title)
  df_net$Title <- unlist(lapply(Title, function(x) {
    paste(x, "\n", collapse = "", sep = "")
  }))

  df_net <- df_net %>%
    mutate(text = paste(tolower(Title), "doi: ",
      DOI, "\nLCS: ",
      LCS, "    GCS: ",
      GCS,
      sep = ""
    ))

  g <- ggplot(df_net, aes(x = x, y = y, xend = xend, yend = yend, text = text)) +
    geom_network_edges(color = "grey", size = 0.4, alpha = 0.4) +
    geom_network_nodes(aes(color = color_v), size = size, alpha = 0.5) +
    geom_text(aes(label = id, color = color_v),
      size = labelsize,
      nudge_x = 0,
      nudge_y = 0.02,
      check_overlap = FALSE, alpha = 0.7
    ) +
    scale_x_continuous(labels = Ylabel, breaks = Breaks) +
    guides(size = "none", color = "none") +
    theme_minimal() +
    theme(
      legend.position = "none", panel.background = element_rect(fill = "white", color = "white"),
      axis.line.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      axis.title.y = element_blank(), axis.title.x = element_blank(),
      panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(adjustcolor(col = "grey", alpha.f = 0.2), linetype = 2, linewidth = 0.5),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(face = "bold", angle = 90, size = labelsize + 4)
    ) +
    labs(title = "Historical Direct Citation Network")

  ### logo coordinates
  data("logo", envir = environment())
  logo <- grid::rasterGrob(logo, interpolate = TRUE)

  a <- ggplot_build(g)$data

  ymin <- unlist(lapply(a, function(l) {
    if ("y" %in% names(l)) {
      min(l["y"])
    }
  })) %>% min(na.rm = TRUE)

  ymax <- unlist(lapply(a, function(l) {
    if ("y" %in% names(l)) {
      max(l["y"])
    }
  })) %>% max(na.rm = TRUE)

  xmin <- unlist(lapply(a, function(l) {
    if ("x" %in% names(l)) {
      min(l["x"])
    }
  })) %>% min(na.rm = TRUE)

  xmax <- unlist(lapply(a, function(l) {
    if ("x" %in% names(l)) {
      max(l["x"])
    }
  })) %>% max(na.rm = TRUE)

  x <- c(xmax - 0.02 - diff(c(xmin, xmax)) * 0.125, xmax - 0.02)
  y <- c(ymin, ymin + diff(c(ymin, ymax)) * 0.125) + 0.02

  g <- g +
    annotation_custom(logo, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2])

  label <- data.frame(Label = names(V(bsk.network)))
  Data <- histResults$histData

  Data <- left_join(label, Data, by = c("Label" = "Paper"))

  if (isTRUE(verbose)) {
    plot(g)

    cat("\n Legend\n\n")

    print(Data[, -2])
  }

  results <- list(net = bsk.network, g = g, graph.data = Data, layout = layout_m, axis = data.frame(label = Ylabel, values = Breaks), params = params)
  return(results)
}


delete.isolates <- function(graph, mode = "all") {
  isolates <- which(degree(graph, mode = mode) == 0) - 1
  delete.vertices(graph, names(isolates))
}
