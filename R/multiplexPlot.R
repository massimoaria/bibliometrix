utils::globalVariables(c(
  "themes_lab", "roots_lab", "res", "n_lab", "text", "x", "y", "xend", "yend", "size",
  "structure", "label", "quadrant", "show", "x_first", "y_first", "x_last", "y_last",
  "trend", "pair", "id", "group", "code", "cohesion_R", "cohesion_T", "layer", "relation", "short"
))

#' Plot a multiplex coupling
#'
#' It draws the results of \code{\link{multiplexClusters}} and
#' \code{\link{multiplexEvolution}}.
#'
#' \tabular{lll}{
#' \code{"matrix"}     \tab \tab roots x themes: documents and standardized residuals; a link is a red cell with at least \code{min.link} documents\cr
#' \code{"links"}      \tab \tab roots on the left, themes on the right, one flow per link: a root with several flows is branching, a theme with several flows is convergent (interactive: a sankey diagram, with nodes that can be dragged)\cr
#' \code{"plane"}      \tab \tab pairs of roots by references and topic proximity; the pairs linked to the same theme are named with it\cr
#' \code{"clusters"}   \tab \tab roots and themes by cohesion in the two layers\cr
#' \code{"trajectory"} \tab \tab the pairs of roots that change area (see \code{\link{multiplexPairs}}), through their periods, from the first to the last (needs \code{\link{multiplexEvolution}})\cr
#' \code{"animation"}  \tab \tab one pair of roots moving period by period on the same plane (needs \code{\link{multiplexEvolution}}; interactive only)\cr
#' \code{"network"}    \tab \tab the documents, with the edges coloured by layer (interactive only)}
#'
#' @param x an object of class \code{"biblioMultiplex"} with clusters, or of
#'   class \code{"biblioMultiplexEvolution"} for \code{type = "trajectory"}
#'   and \code{"animation"}.
#' @param type is a character. The plot, see Details. Default is \code{"matrix"}.
#' @param interactive is logical. If TRUE, a plotly widget (a visNetwork widget
#'   for \code{type = "network"}) instead of a ggplot. Default is FALSE.
#' @param n.labels is an integer. For \code{type = "plane"} and
#'   \code{"trajectory"}, the number of pairs named besides those linked to the
#'   same theme. Default is 12.
#' @param min.size is an integer. For \code{type = "plane"}, the minimum
#'   number of documents of the two roots of a pair. Default is 10.
#' @param pair is an integer. For \code{type = "animation"}, the pair of
#'   roots, by its number in \code{\link{multiplexPairs}} (the numbers of
#'   \code{type = "trajectory"}). Default is 1.
#' @param max.nodes is an integer. For \code{type = "network"}, the maximum
#'   number of documents drawn. Default is 500.
#'
#' @return a ggplot object, or a plotly or visNetwork widget.
#'
#' @examples
#' \donttest{
#' data(management, package = "bibliometrixData")
#' mc <- multiplexClusters(multiplexCoupling(management, n = 300, n.perm = 19))
#' multiplexPlot(mc, "links")
#' }
#'
#' @seealso \code{\link{multiplexCoupling}}, \code{\link{multiplexClusters}},
#'   \code{\link{multiplexEvolution}}
#'
#' @export
multiplexPlot <- function(x,
                          type = c("matrix", "links", "plane", "clusters", "trajectory", "animation", "network"),
                          interactive = FALSE,
                          n.labels = 12,
                          min.size = 10,
                          max.nodes = 500,
                          pair = 1) {
  type <- match.arg(type)
  if (type %in% c("trajectory", "animation")) {
    if (!inherits(x, "biblioMultiplexEvolution")) {
      stop("multiplexPlot(type = \"", type, "\") needs the result of multiplexEvolution()", call. = FALSE)
    }
    if (type == "animation") return(mpTrajectoryAnimation(x, pair))
    return(mpTrajectoryPlot(x, interactive, n.labels))
  }
  if (!inherits(x, "biblioMultiplex") || is.null(x$clusters)) {
    stop("multiplexPlot() needs the result of multiplexClusters()", call. = FALSE)
  }
  switch(type,
    matrix = mpMatrixPlot(x, interactive),
    links = mpLinksPlot(x, interactive),
    plane = mpPlanePlot(x, interactive, n.labels, min.size),
    clusters = mpClustersPlot(x, interactive),
    network = mpNetwork(x, max.nodes)
  )
}

MP_COLORS <- c(consolidation = "#1B9E77", branching = "#D95F02", convergence = "#7570B3",
               detachment = "#AAAAAA", dispersed = "#999999")
MP_EDGE_COLORS <- c(both = "#1B9E77", references_only = "#D95F02", topics_only = "#7570B3")

mpFirstLabel <- function(x) sub(";.*", "", x)

mpRootLabels <- function(cl) sprintf("R%d %s", cl$roots$cluster, mpFirstLabel(cl$roots$terms))
mpThemeLabels <- function(cl) sprintf("T%d %s", cl$themes$cluster, mpFirstLabel(cl$themes$terms))

mpTheme <- function(base) {
  base +
    ggplot2::theme(
      text = ggplot2::element_text(color = "#333333"),
      plot.title = ggplot2::element_text(face = "bold")
    )
}

# roots x themes, themes ordered by the root they are most over-represented in
mpMatrixPlot <- function(mc, interactive = FALSE) {
  cl <- mc$clusters
  R <- cl$residuals
  col_order <- order(apply(R, 2, which.max), -apply(R, 2, max))
  D <- as.data.frame(as.table(R), stringsAsFactors = FALSE)
  names(D) <- c("roots", "themes", "res")
  D$n <- as.vector(unclass(cl$contingency))
  lr <- mpRootLabels(cl)
  lt <- mpThemeLabels(cl)
  D$roots_lab <- factor(lr[as.integer(D$roots)], levels = rev(lr))
  D$themes_lab <- factor(lt[as.integer(D$themes)], levels = lt[col_order])
  D$n_lab <- ifelse(D$n > 0, D$n, "")
  D$text <- sprintf("%s x %s\n%d documents, residual %.1f", D$roots_lab, D$themes_lab, D$n, D$res)
  lim <- max(abs(D$res))
  g <- suppressWarnings(
    ggplot2::ggplot(D, ggplot2::aes(themes_lab, roots_lab, fill = res, text = text)) +
      ggplot2::geom_tile(colour = "white") +
      ggplot2::geom_text(ggplot2::aes(label = n_lab), size = 2.6) +
      ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", limits = c(-lim, lim),
                                    name = "standardized\nresidual") +
      ggplot2::labs(x = "themes (topic clusters)", y = "roots (references clusters)",
                    title = "Roots x themes",
                    subtitle = sprintf("documents per cell; red = more than expected (NMI %.2f)", cl$NMI)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  )
  g <- mpTheme(g)
  if (interactive) plotly::ggplotly(g, tooltip = "text") else g
}

# roots on the left, themes on the right, one line per link (width = documents)
mpLinksPlot <- function(mc, interactive = FALSE) {
  if (interactive) return(mpLinksSankey(mc))
  cl <- mc$clusters
  lk <- cl$links
  S <- cl$roots
  Tm <- cl$themes
  # roots by number, themes at the mean height of their roots (weighted by
  # documents) so that lines cross as little as possible; themes with no link last
  S$y <- -seq_len(nrow(S))
  bary <- if (nrow(lk)) {
    tapply(S$y[lk$root] * lk$n, lk$theme, sum) / tapply(lk$n, lk$theme, sum)
  } else {
    numeric(0)
  }
  Tm$bary <- bary[as.character(Tm$cluster)]
  ord <- order(is.na(Tm$bary), -Tm$bary, -Tm$size)
  Tm$y <- NA_real_
  Tm$y[ord] <- -seq_len(nrow(Tm)) * nrow(S) / nrow(Tm)
  struct <- c(branching = "branching root", consolidation = "consolidated",
              convergence = "convergent theme", dispersed = "no link")
  cols <- c("branching root" = MP_COLORS[["branching"]], consolidated = MP_COLORS[["consolidation"]],
            "convergent theme" = MP_COLORS[["convergence"]], "no link" = MP_COLORS[["dispersed"]])
  N <- rbind(
    data.frame(x = 0, y = S$y, size = S$size, structure = struct[S$structure],
               label = mpRootLabels(cl),
               text = sprintf("Root R%d: %s\n%d documents, %d linked themes", S$cluster, S$terms, S$size,
                              S$n_themes), stringsAsFactors = FALSE),
    data.frame(x = 1, y = Tm$y, size = Tm$size, structure = struct[Tm$structure],
               label = mpThemeLabels(cl),
               text = sprintf("Theme T%d: %s\n%d documents, %d linked roots", Tm$cluster, Tm$terms, Tm$size,
                              Tm$n_roots), stringsAsFactors = FALSE)
  )
  E <- data.frame(x = 0, y = S$y[lk$root], xend = 1, yend = Tm$y[match(lk$theme, Tm$cluster)], n = lk$n,
                  text = sprintf("R%d -> T%d: %d documents (%.0f%% of the root, %.0f%% of the theme), residual %.1f",
                                 lk$root, lk$theme, lk$n, 100 * lk$share_root, 100 * lk$share_theme, lk$residual))
  min.link <- cl$params$values[cl$params$params == "min.link"]
  g <- suppressWarnings(
    ggplot2::ggplot() +
      ggplot2::geom_segment(data = E, ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                                                   linewidth = n, text = text),
                            colour = "grey55", alpha = 0.55, lineend = "round") +
      ggplot2::geom_point(data = N, ggplot2::aes(x, y, size = size, colour = structure, text = text)) +
      ggplot2::geom_text(data = N[N$x == 0, ], ggplot2::aes(x - 0.04, y, label = label), hjust = 1, size = 3.3) +
      ggplot2::geom_text(data = N[N$x == 1, ], ggplot2::aes(x + 0.04, y, label = label), hjust = 0, size = 3.3) +
      ggplot2::annotate("text", x = c(0, 1), y = 0, label = c("ROOTS (references)", "THEMES (topics)"),
                        fontface = "bold", size = 3.8, hjust = c(1, 0)) +
      ggplot2::scale_linewidth(range = c(0.4, 5), name = "documents") +
      ggplot2::scale_size_area(max_size = 7, guide = "none") +
      ggplot2::scale_colour_manual(values = cols, name = NULL) +
      ggplot2::scale_x_continuous(limits = c(-0.9, 1.9)) +
      ggplot2::labs(title = "Roots and the themes they are linked to",
                    subtitle = sprintf("one line per link: more documents than expected (residual > 2), at least %s of them",
                                       min.link)) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "bottom")
  )
  mpTheme(g)
}

# themes linked to both roots of each pair ("-> T1 patents"), "" when none
mpSharedTheme <- function(cl, A, B) {
  lt <- mpThemeLabels(cl)
  linked <- split(cl$links$theme, factor(cl$links$root, levels = seq_len(nrow(cl$roots))))
  vapply(seq_along(A), function(i) {
    both <- intersect(linked[[A[i]]], linked[[B[i]]])
    if (length(both) == 0) return("")
    paste0("-> ", paste(lt[both], collapse = ", "))
  }, character(1))
}

# a log2 lift in words: 1 is "2x closer" than two random documents, -1 "2x
# farther", 0 "as random"; at the floor, "or more"
mpLiftWords <- function(v, floor = -Inf) {
  w <- ifelse(abs(v) < 1e-9, "as random",
              ifelse(v > 0, sprintf("%gx closer", round(2^v, 1)), sprintf("%gx farther", round(2^-v, 1))))
  ifelse(v <= floor, paste(w, "or more"), w)
}

# the four areas of the plane of the pairs of roots, with the colour and
# the name of each (the colours of the relations and trends that lead into it)
MP_AREAS <- data.frame(
  label = c("CLOSE IN TOPICS ONLY", "CLOSE IN BOTH", "CLOSE IN REFERENCES ONLY", "CLOSE IN NEITHER"),
  right = c(FALSE, TRUE, TRUE, FALSE), up = c(TRUE, TRUE, FALSE, FALSE),
  fill = c("#EAE9F4", "#E3F2EE", "#FBEADF", "#F2F2F2"),
  colour = c("#7570B3", "#1B9E77", "#D95F02", "#888888"),
  stringsAsFactors = FALSE
)
MP_AXIS_R <- "References: shared cited works, compared with two random documents"
MP_AXIS_T <- "Topics: shared keywords, compared with two random documents"

# the plane for points at x, y (log2 lifts): its ranges, always with both
# sides of "as random" in view so that every area shows, and its areas
mpAreas <- function(x, y) {
  xr <- range(c(x, -0.75, 0.75)) + c(-0.35, 0.35)
  yr <- range(c(y, -0.75, 0.75)) + c(-0.35, 0.35)
  A <- MP_AREAS
  A$x0 <- ifelse(A$right, 0, xr[1])
  A$x1 <- ifelse(A$right, xr[2], 0)
  A$y0 <- ifelse(A$up, 0, yr[1])
  A$y1 <- ifelse(A$up, yr[2], 0)
  list(xr = xr, yr = yr, A = A)
}

# no tick at the edges, where the names of the areas are; half steps (1.4x)
# when a short range would have fewer than three ticks
mpAreaTicks <- function(r) {
  v <- seq(ceiling(r[1] + 0.25), floor(r[2] - 0.25))
  if (length(v) < 3) v <- seq(ceiling(2 * r[1] + 0.5), floor(2 * r[2] - 0.5)) / 2
  v
}

# the areas, the axes in words and (static plots) the names of the areas, as
# ggplot layers drawn below the data
mpAreaLayers <- function(ar, names = TRUE, floor = -Inf) {
  A <- ar$A
  scale <- function(r) {
    b <- mpAreaTicks(r)
    list(limits = r, breaks = b, labels = mpLiftWords(b, floor), expand = c(0, 0))
  }
  c(
    list(ggplot2::annotate("rect", xmin = A$x0, xmax = A$x1, ymin = A$y0, ymax = A$y1, fill = A$fill)),
    if (names) {
      list(ggplot2::annotate("text", x = ifelse(A$right, Inf, -Inf), y = ifelse(A$up, Inf, -Inf),
                             label = A$label, colour = A$colour, fontface = "bold", size = 3.2,
                             hjust = ifelse(A$right, 1.05, -0.05), vjust = ifelse(A$up, 1.5, -0.6)))
    },
    list(ggplot2::geom_hline(yintercept = 0, colour = "grey60"),
         ggplot2::geom_vline(xintercept = 0, colour = "grey60"),
         do.call(ggplot2::scale_x_continuous, scale(ar$xr)),
         do.call(ggplot2::scale_y_continuous, scale(ar$yr)),
         ggplot2::labs(x = MP_AXIS_R, y = MP_AXIS_T))
  )
}

# the names of the areas as plotly annotations, in the corners
mpAreaNotes <- function(ar) {
  A <- ar$A
  lapply(seq_len(nrow(A)), function(k) {
    list(x = if (A$right[k]) ar$xr[2] else ar$xr[1], y = if (A$up[k]) ar$yr[2] else ar$yr[1],
         xref = "x", yref = "y",
         xanchor = if (A$right[k]) "right" else "left", yanchor = if (A$up[k]) "top" else "bottom",
         text = paste0("<b>", A$label[k], "</b>"), showarrow = FALSE,
         font = list(size = 13, color = A$colour[k]))
  })
}

# pairs of roots by references and topic proximity relative to two random documents
mpPlanePlot <- function(mc, interactive = FALSE, n.labels = 12, min.size = 10, floor = 1 / 16) {
  cl <- mc$clusters
  P <- cl$plane
  P <- P[P$size_A >= min.size & P$size_B >= min.size, ]
  if (!nrow(P)) stop("multiplexPlot(): no pair of roots has at least ", min.size, " documents each", call. = FALSE)
  # two roots sharing no reference at all have lift 0: drawn at the floor
  P$x <- log2(pmax(P$lift_R, floor))
  P$y <- log2(pmax(P$lift_T, floor))
  P$size <- P$size_A + P$size_B
  P$pair <- paste0(mpFirstLabel(P$label_A), " / ", mpFirstLabel(P$label_B))
  P$theme <- mpSharedTheme(cl, P$A, P$B)
  # the relation of the pair, in words that are not those of the typology of
  # roots and themes (the pair "consolidation" is not a consolidated root)
  rel <- c(consolidation = "close in both", branching = "close in references only",
           convergence = "close in topics only", detachment = "close in neither")
  P$relation <- factor(rel[as.character(P$quadrant)], levels = rel)
  rel_cols <- stats::setNames(MP_COLORS[names(rel)], rel)
  top <- order(-(abs(P$x) + abs(P$y)) * (P$quadrant != "detachment"))[seq_len(min(n.labels, nrow(P)))]
  top <- union(top, which(P$theme != ""))
  P$show <- ifelse(seq_len(nrow(P)) %in% top,
                   ifelse(P$theme == "", P$pair, paste0(P$pair, "\n", P$theme)), "")
  P$text <- sprintf("%s\nroots: %s than random\ntopics: %s than random\n%s%s", P$pair,
                    mpLiftWords(P$x, log2(floor)), mpLiftWords(P$y, log2(floor)), P$relation,
                    ifelse(P$theme == "", "", paste0("\n", P$theme)))
  P$text <- gsub("as random than random", "as random", P$text, fixed = TRUE)
  ar <- mpAreas(P$x, P$y)
  g <- suppressWarnings(
    ggplot2::ggplot(P, ggplot2::aes(x = x, y = y, text = text)) +
      mpAreaLayers(ar, names = !interactive, floor = log2(floor)) +
      ggplot2::geom_point(ggplot2::aes(size = size, colour = relation), alpha = 0.7) +
      ggplot2::scale_colour_manual(values = rel_cols, drop = FALSE) +
      ggplot2::scale_size_area(max_size = 12, guide = "none") +
      ggplot2::labs(colour = NULL, title = "Pairs of roots",
                    subtitle = sprintf(paste0("pairs of roots with at least %d documents each; ",
                                              "-> theme both roots are linked to"), min.size)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid = ggplot2::element_blank())
  )
  if (interactive) {
    # plotly draws no repelled labels: the pairs that meet on a theme are
    # named with it above their point, all the others on hover
    lab <- P[P$theme != "", ]
    lab$short <- sub("^-> ", "", lab$theme)
    if (nrow(lab)) {
      g <- g + ggplot2::geom_text(data = lab, ggplot2::aes(x = x, y = y, label = short),
                                  nudge_y = 0.05, size = 3, colour = "#333333", inherit.aes = FALSE)
    }
    p <- plotly::plotly_build(plotly::ggplotly(mpTheme(g), tooltip = "text"))
    # the areas and the axes of "as random" are lines: no tooltip on them
    p$x$data <- lapply(p$x$data, function(t) {
      if (identical(t$mode, "lines")) t$hoverinfo <- "skip"
      t
    })
    return(plotly::layout(p, annotations = mpAreaNotes(ar)))
  }
  mpTheme(g + ggrepel::geom_text_repel(ggplot2::aes(label = show), size = 3, max.overlaps = 30))
}

# roots and themes by cohesion in the two layers. The diagonal is "as
# cohesive in references as in topics": a root (a references cluster) far
# below it shares its references more than its topics, as a branching root
# does; a theme (a topic cluster) far above it shares its topics more than its
# references, as a
# convergent theme does. The axes read in words, as the other planes
mpClustersPlot <- function(mc, interactive = FALSE, floor = 1 / 16) {
  cl <- mc$clusters
  D <- rbind(
    data.frame(layer = "root", code = paste0("R", cl$roots$cluster),
               cl$roots[, c("size", "terms", "cohesion_R", "cohesion_T")],
               structure = cl$roots$structure, stringsAsFactors = FALSE),
    data.frame(layer = "theme", code = paste0("T", cl$themes$cluster),
               cl$themes[, c("size", "terms", "cohesion_R", "cohesion_T")],
               structure = cl$themes$structure, stringsAsFactors = FALSE)
  )
  # a cluster of one document has no cohesion; one sharing nothing (or almost)
  # is drawn at the floor
  D <- D[is.finite(D$cohesion_R) & is.finite(D$cohesion_T), ]
  D$x <- log2(pmax(D$cohesion_R, floor))
  D$y <- log2(pmax(D$cohesion_T, floor))
  kind <- c(branching = "branching", consolidation = "consolidated", convergence = "convergent",
            dispersed = "dispersed")
  D$group <- paste(kind[D$structure], D$layer)
  groups <- c("branching root", "consolidated root", "dispersed root",
              "convergent theme", "consolidated theme", "dispersed theme")
  groups <- groups[groups %in% D$group]
  D$group <- factor(D$group, levels = groups)
  gcol <- c(`branching root` = MP_COLORS[["branching"]], `consolidated root` = MP_COLORS[["consolidation"]],
            `dispersed root` = MP_COLORS[["dispersed"]], `convergent theme` = MP_COLORS[["convergence"]],
            `consolidated theme` = MP_COLORS[["consolidation"]], `dispersed theme` = MP_COLORS[["dispersed"]])
  D$label <- paste(D$code, mpFirstLabel(D$terms))
  D$text <- sprintf("<b>%s %s</b> (%s)<br>%d documents<br>references: %s than random<br>topics: %s than random",
                    D$layer, D$label, D$group, D$size, mpLiftWords(D$x, log2(floor)), mpLiftWords(D$y, log2(floor)))
  D$text <- gsub("as random than random", "as random", D$text, fixed = TRUE)
  # one range for both axes, so that the diagonal is the diagonal
  r <- range(c(D$x, D$y, 0)) + c(-0.35, 0.35)
  ticks <- mpAreaTicks(r)
  up <- data.frame(x = c(r[1], r[2], r[1]), y = c(r[1], r[2], r[2]))
  down <- data.frame(x = c(r[1], r[2], r[2]), y = c(r[1], r[2], r[1]))
  xlab <- "References: how close its documents are in cited works, compared with random documents"
  ylab <- "Topics: how close its documents are in keywords, compared with random documents"
  title <- "Cohesion of roots and themes"
  sub <- paste0("a root below the diagonal shares its references more than its topics (branching); ",
                "a theme above it shares its topics more than its references (convergence)")
  if (interactive) {
    p <- plotly::plot_ly()
    for (g in groups) {
      d <- D[D$group == g, ]
      p <- plotly::add_trace(
        p, data = d, x = ~x, y = ~y, type = "scatter", mode = "markers+text", name = g,
        text = ~code, textposition = "top center", textfont = list(size = 10, color = gcol[[g]]),
        marker = list(color = gcol[[g]], symbol = if (grepl("root", g)) "circle" else "triangle-up",
                      size = 8 + 22 * sqrt(d$size / max(D$size)), opacity = 0.8,
                      line = list(color = "white", width = 1)),
        hovertext = ~text, hoverinfo = "text"
      )
    }
    path <- function(P) paste0("M ", paste(P$x, P$y, collapse = " L "), " Z")
    shapes <- list(
      list(type = "path", path = path(up), fillcolor = "#EAE9F4", line = list(width = 0), layer = "below"),
      list(type = "path", path = path(down), fillcolor = "#FBEADF", line = list(width = 0), layer = "below"),
      list(type = "line", x0 = r[1], y0 = r[1], x1 = r[2], y1 = r[2], line = list(color = "#888888", width = 1.5),
           layer = "below")
    )
    notes <- list(
      list(x = r[1], y = r[2], xanchor = "left", yanchor = "top", showarrow = FALSE,
           text = "<b>MORE COHESIVE IN TOPICS</b>", font = list(size = 13, color = MP_COLORS[["convergence"]])),
      list(x = r[2], y = r[1], xanchor = "right", yanchor = "bottom", showarrow = FALSE,
           text = "<b>MORE COHESIVE IN REFERENCES</b>", font = list(size = 13, color = MP_COLORS[["branching"]])),
      list(x = r[2], y = r[2], xanchor = "right", yanchor = "top", showarrow = FALSE, textangle = 0,
           text = "equally cohesive", font = list(size = 11, color = "#666666"))
    )
    axis <- function(title) {
      list(title = title, range = r, tickvals = ticks, ticktext = mpLiftWords(ticks, log2(floor)), zeroline = TRUE,
           zerolinecolor = "#BBBBBB", zerolinewidth = 1, showgrid = FALSE)
    }
    return(plotly::layout(
      p,
      title = list(text = paste0(title, "<br><sup>", sub, "</sup>"), x = 0.02),
      xaxis = axis(xlab), yaxis = axis(ylab), shapes = shapes, annotations = notes, plot_bgcolor = "white",
      legend = list(font = list(size = 11)), margin = list(t = 70)
    ))
  }
  shape <- stats::setNames(ifelse(grepl("root", groups), 16, 17), groups)
  g <- ggplot2::ggplot(D, ggplot2::aes(x, y)) +
    ggplot2::annotate("polygon", x = up$x, y = up$y, fill = "#EAE9F4") +
    ggplot2::annotate("polygon", x = down$x, y = down$y, fill = "#FBEADF") +
    ggplot2::annotate("segment", x = r[1], y = r[1], xend = r[2], yend = r[2], colour = "#888888") +
    ggplot2::annotate("text", x = c(-Inf, Inf), y = c(Inf, -Inf),
                      label = c("MORE COHESIVE IN TOPICS", "MORE COHESIVE IN REFERENCES"),
                      colour = c(MP_COLORS[["convergence"]], MP_COLORS[["branching"]]), fontface = "bold",
                      size = 3.2, hjust = c(-0.05, 1.05), vjust = c(1.5, -0.6)) +
    ggplot2::geom_point(ggplot2::aes(size = size, colour = group, shape = group), alpha = 0.8) +
    ggrepel::geom_text_repel(ggplot2::aes(label = label, colour = group), size = 2.8, max.overlaps = 25,
                             show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = gcol[groups]) +
    ggplot2::scale_shape_manual(values = shape) +
    ggplot2::scale_size_area(max_size = 12, guide = "none") +
    ggplot2::scale_x_continuous(limits = r, breaks = ticks, labels = mpLiftWords(ticks, log2(floor)), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = r, breaks = ticks, labels = mpLiftWords(ticks, log2(floor)), expand = c(0, 0)) +
    ggplot2::labs(x = xlab, y = ylab, colour = NULL, shape = NULL, title = title,
                  subtitle = sub(" (branching); ", " (branching);\n", sub, fixed = TRUE)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
  mpTheme(g)
}

MP_TREND_COLORS <- c(converging = "#7570B3", diverging = "#D95F02", `drifting apart` = "#666666",
                     consolidating = "#1B9E77", `closer in references` = "#E7298A",
                     `farther in references` = "#A6761D", stable = "#AAAAAA")

#' The pairs of roots of a multiplex evolution, numbered
#'
#' The pairs of roots followed in at least two periods, with the areas of the
#' plane they cross (close in both, in topics only, in references only, in
#' neither), in the order and with the numbers of
#' \code{multiplexPlot(type = "trajectory")} and \code{"animation"}. First come
#' the pairs that change area and have a trend, the ones those plots draw;
#' then the others, the stable ones last. Within each group, first the pairs
#' followed in more periods (a trend over two periods is a single difference,
#' larger and noisier than a slope over three or more), then by the strength
#' of the topic trend.
#'
#' @param ev an object of class \code{"biblioMultiplexEvolution"}, obtained by
#'   \code{\link{multiplexEvolution}}.
#'
#' @return the data frame \code{ev$trajectories}, ordered, with the number
#'   (\code{id}) and the name (\code{pair}) of every pair, the areas it is in
#'   period by period (\code{areas}) and whether it changes area
#'   (\code{moves}).
#'
#' @seealso \code{\link{multiplexEvolution}}, \code{\link{multiplexPlot}}
#'
#' @export
multiplexPairs <- function(ev) {
  tr <- ev$trajectories
  L <- ev$long[order(ev$long$period), ]
  area <- mpArea(L$x, L$y)
  path <- tapply(area, paste(L$A, L$B), function(a) paste(a[c(TRUE, a[-1] != a[-length(a)])], collapse = " -> "))
  tr$areas <- unname(path[paste(tr$A, tr$B)])
  tr$moves <- grepl(" -> ", tr$areas, fixed = TRUE)
  tr <- tr[order(!(tr$moves & tr$trend != "stable"), tr$trend == "stable", -tr$periods, -abs(tr$slope_T)), ]
  rownames(tr) <- NULL
  tr$id <- seq_len(nrow(tr))
  tr$pair <- paste0(mpFirstLabel(tr$label_A), " / ", mpFirstLabel(tr$label_B))
  tr
}

# the area of the plane of a point (log2 lifts of references and topics)
mpArea <- function(x, y) {
  ifelse(x > 0, ifelse(y > 0, "close in both", "close in references only"),
         ifelse(y > 0, "close in topics only", "close in neither"))
}

# pairs of roots through their periods: one path per pair, from a hollow
# point (first period) to an arrow (last period); the pairs are numbered. The
# plane is split into its four areas, and the axes read in words (how many
# times closer or farther than two random documents)
mpTrajectoryPlot <- function(ev, interactive = FALSE, n.pairs = 12) {
  # the pairs that change area: a trend within one area changes no relation
  tr <- multiplexPairs(ev)
  tr <- tr[tr$moves & tr$trend != "stable", ]
  if (!nrow(tr)) stop("multiplexPlot(): no pair of roots with a trend changes area", call. = FALSE)
  tr <- utils::head(tr, n.pairs)
  cols <- MP_TREND_COLORS
  L <- ev$long
  key <- match(paste(L$A, L$B), paste(tr$A, tr$B))
  L <- L[!is.na(key), ]
  L$id <- key[!is.na(key)]
  L <- L[order(L$id, L$period), ]
  L$pair <- tr$pair[L$id]
  L$trend <- tr$trend[L$id]
  first <- L$period == stats::ave(L$period, L$id, FUN = min)
  last <- L$period == stats::ave(L$period, L$id, FUN = max)
  L$step <- ifelse(first, "first", ifelse(last, "last", "middle"))
  ar <- mpAreas(L$x, L$y)
  title <- sprintf("Pairs of roots, %s to %s", ev$periods[1], ev$periods[length(ev$periods)])
  if (interactive) {
    L$text <- sprintf("<b>%d. %s</b> (%s)<br>%s<br>references: %s than random<br>topics: %s than random<br>documents %d + %d",
                      L$id, L$pair, L$trend, L$label, mpLiftWords(L$x), mpLiftWords(L$y), L$n_A, L$n_B)
    L$text <- gsub("as random than random", "as random", L$text, fixed = TRUE)
    p <- plotly::plot_ly()
    for (i in tr$id) {
      d <- L[L$id == i, ]
      col <- cols[[tr$trend[i]]]
      last <- d$step == "last"
      # the arrow points along the last segment (angleref "previous"), which
      # leaves the first point undrawn: it is a trace of its own, in the same
      # legend group so that the legend hides and isolates the whole pair
      p <- plotly::add_trace(
        p, data = d, x = ~x, y = ~y, type = "scatter", mode = "lines+markers+text",
        name = sprintf("%d. %s", i, tr$pair[i]), legendgroup = i,
        line = list(color = col, width = 2.5),
        # the outline too, or plotly gives it a colour of its own palette
        marker = list(color = col, size = ifelse(last, 22, 10), symbol = ifelse(last, "arrow", "circle"),
                      angleref = "previous", line = list(color = col, width = 1)),
        text = ifelse(last, i, ""), textposition = "top right", textfont = list(size = 14, color = col),
        hovertext = ~text, hoverinfo = "text"
      )
      p <- plotly::add_trace(
        p, data = d[d$step == "first", ], x = ~x, y = ~y, type = "scatter", mode = "markers",
        legendgroup = i, showlegend = FALSE,
        marker = list(color = "white", size = 12, symbol = "circle", line = list(color = col, width = 2.5)),
        hovertext = ~text, hoverinfo = "text"
      )
    }
    A <- ar$A
    areas <- lapply(seq_len(nrow(A)), function(k) {
      list(type = "rect", xref = "x", yref = "y", x0 = A$x0[k], x1 = A$x1[k], y0 = A$y0[k], y1 = A$y1[k],
           fillcolor = A$fill[k], line = list(width = 0), layer = "below")
    })
    axis <- function(r, title) {
      v <- mpAreaTicks(r)
      list(title = title, range = r, tickvals = v, ticktext = mpLiftWords(v), zeroline = TRUE,
           zerolinecolor = "#999999", zerolinewidth = 1.5, showgrid = FALSE)
    }
    shown <- intersect(names(cols), tr$trend)
    key <- paste(sprintf("<span style='color:%s'><b>&#9644; %s</b></span>", cols[shown], shown), collapse = "   ")
    return(plotly::layout(
      p,
      title = list(text = paste0(title, "<br><sup>each pair from its first period (hollow point) to its last (arrow); ",
                                 "click a pair in the legend to hide it, double-click to see it alone<br>",
                                 key, "</sup>"),
                   x = 0.02),
      xaxis = axis(ar$xr, MP_AXIS_R), yaxis = axis(ar$yr, MP_AXIS_T),
      shapes = areas, annotations = mpAreaNotes(ar), plot_bgcolor = "white",
      # a narrow legend leaves the room to the plane: the trend is in the
      # colour, whose key is under the title
      legend = list(title = list(text = "<b>pairs of roots</b>", font = list(size = 10)),
                    font = list(size = 9), tracegroupgap = 0, itemwidth = 30, itemsizing = "constant"),
      margin = list(t = 85)
    ))
  }
  ends <- L[L$step == "last", ]
  ends$label <- sprintf("%d. %s", ends$id, ends$pair)
  mpTheme(
    ggplot2::ggplot(L, ggplot2::aes(x, y, colour = trend, group = id)) +
      mpAreaLayers(ar) +
      ggplot2::geom_path(arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "cm"), type = "closed"),
                         linewidth = 0.9) +
      ggplot2::geom_point(data = L[L$step == "first", ], shape = 21, fill = "white", size = 2.8, stroke = 1.1) +
      ggplot2::geom_point(data = L[L$step == "middle", ], size = 2.2) +
      ggrepel::geom_text_repel(data = ends, ggplot2::aes(label = label), size = 2.8, max.overlaps = 30,
                               show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = cols) +
      ggplot2::labs(colour = NULL, title = title,
                    subtitle = "each pair from its first period (hollow point) to its last (arrow), through the periods between") +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid = ggplot2::element_blank())
  )
}

# one pair of roots moving slowly, period by period, on the plane of the
# four areas. Every frame has a point per period: the periods not reached yet
# sit on the current one, so that the move to the next period draws the new
# segment as the point travels (plotly interpolates points with the same id)
mpTrajectoryAnimation <- function(ev, pair = 1) {
  tr <- multiplexPairs(ev)
  if (length(pair) != 1 || !pair %in% tr$id) {
    stop("multiplexPlot(): pair is a number between 1 and ", nrow(tr), call. = FALSE)
  }
  t <- tr[tr$id == pair, ]
  d <- ev$long[ev$long$A == t$A & ev$long$B == t$B, ]
  d <- d[order(d$period), ]
  n <- nrow(d)
  col <- MP_TREND_COLORS[[t$trend]]
  ar <- mpAreas(d$x, d$y)
  # trail: frame k, point j at the position of period min(j, k)
  K <- expand.grid(j = seq_len(n), k = seq_len(n))
  at <- pmin(K$j, K$k)
  trail <- data.frame(frame = d$label[K$k], id = paste0("p", K$j), x = d$x[at], y = d$y[at],
                      size = ifelse(K$j < K$k, 9, 0), label = ifelse(K$j < K$k, d$label[K$j], ""),
                      stringsAsFactors = FALSE)
  trail$frame <- factor(trail$frame, levels = d$label)
  now <- data.frame(frame = factor(d$label, levels = d$label), id = "now", x = d$x, y = d$y,
                    label = d$label,
                    text = sprintf("<b>%s</b><br>references: %s than random<br>topics: %s than random<br>documents %d + %d",
                                   d$label, mpLiftWords(d$x), mpLiftWords(d$y), d$n_A, d$n_B),
                    stringsAsFactors = FALSE)
  now$text <- gsub("as random than random", "as random", now$text, fixed = TRUE)
  p <- plotly::plot_ly()
  # the whole route, faint, so that the eye knows where the pair is going
  p <- plotly::add_trace(p, x = d$x, y = d$y, type = "scatter", mode = "lines", hoverinfo = "skip",
                         line = list(color = col, width = 1.5, dash = "dot"), opacity = 0.35,
                         showlegend = FALSE)
  p <- plotly::add_trace(p, data = trail, x = ~x, y = ~y, frame = ~frame, ids = ~id, type = "scatter",
                         mode = "lines+markers+text", text = ~label, textposition = "bottom center",
                         textfont = list(size = 11, color = "#555555"), hoverinfo = "skip",
                         line = list(color = col, width = 3),
                         marker = list(color = col, size = ~size, line = list(color = col, width = 1)),
                         showlegend = FALSE)
  p <- plotly::add_trace(p, data = now, x = ~x, y = ~y, frame = ~frame, ids = ~id, type = "scatter",
                         mode = "markers+text", text = ~label, textposition = "top center",
                         textfont = list(size = 15, color = col), hovertext = ~text, hoverinfo = "text",
                         marker = list(color = col, size = 26, line = list(color = "white", width = 2)),
                         showlegend = FALSE)
  A <- ar$A
  areas <- lapply(seq_len(nrow(A)), function(k) {
    list(type = "rect", xref = "x", yref = "y", x0 = A$x0[k], x1 = A$x1[k], y0 = A$y0[k], y1 = A$y1[k],
         fillcolor = A$fill[k], line = list(width = 0), layer = "below")
  })
  axis <- function(r, title) {
    v <- mpAreaTicks(r)
    list(title = title, range = r, tickvals = v, ticktext = mpLiftWords(v), zeroline = TRUE,
         zerolinecolor = "#999999", zerolinewidth = 1.5, showgrid = FALSE)
  }
  p <- plotly::layout(
    p,
    title = list(text = sprintf("%d. %s (%s)<br><sup>%s: %s; press Play, or drag the slider</sup>",
                                t$id, t$pair, t$trend, paste(d$label, collapse = " -> "), t$areas), x = 0.02),
    xaxis = axis(ar$xr, MP_AXIS_R), yaxis = axis(ar$yr, MP_AXIS_T),
    shapes = areas, annotations = mpAreaNotes(ar), plot_bgcolor = "white", margin = list(t = 70)
  )
  # redraw at the end of each move: the labels of the periods left behind are
  # text, which a transition alone does not update
  p <- plotly::animation_opts(p, frame = 3500, transition = 2000, easing = "cubic-in-out", redraw = TRUE)
  plotly::animation_slider(p, currentvalue = list(prefix = "Period: ", font = list(color = col)))
}

# The links as a sankey diagram, as plotThematicEvolution() and
# threeFieldsPlot() draw theirs: roots on the left, themes on the right, one
# flow per link (width = documents). Every root has its colour and its flows
# take it, so the themes a branching root feeds can be followed; themes are
# coloured by their structure. Nodes can be dragged.
mpLinksSankey <- function(mc) {
  cl <- mc$clusters
  lk <- cl$links
  if (!nrow(lk)) stop("multiplexPlot(): no root is linked to a theme", call. = FALSE)
  S <- cl$roots[sort(unique(lk$root)), ]
  Tm <- cl$themes[sort(unique(lk$theme)), ]
  # roots by number, themes at the mean rank of their roots (weighted by
  # documents), so that flows cross as little as possible
  S$rank <- seq_len(nrow(S))
  sr <- S$rank[match(lk$root, S$cluster)]
  bary <- tapply(sr * lk$n, lk$theme, sum) / tapply(lk$n, lk$theme, sum)
  Tm <- Tm[order(bary[as.character(Tm$cluster)], -Tm$size), ]
  pos <- function(k) (seq_len(k) - 0.5) / k
  root_cols <- colorlist()[((S$cluster - 1) %% length(colorlist())) + 1]
  theme_cols <- c(convergence = MP_COLORS[["convergence"]], consolidation = MP_COLORS[["consolidation"]],
                  dispersed = MP_COLORS[["dispersed"]])[Tm$structure]
  struct_root <- c(branching = "branching root", consolidation = "consolidated root",
                     dispersed = "dispersed root")
  struct_theme <- c(convergence = "convergent theme", consolidation = "consolidated theme",
                    dispersed = "dispersed theme")
  # tooltips: the name, the three main labels, then the figures
  bullets <- function(terms) {
    vapply(strsplit(terms, "; ", fixed = TRUE), function(t) {
      paste0("&#8226; ", t, collapse = "<br>")
    }, "")
  }
  linkedNames <- function(ids, labels) {
    vapply(strsplit(ids, ",", fixed = TRUE), function(k) {
      if (!length(k)) "none" else paste0("<br>&#8226; ", labels[as.integer(k)], collapse = "")
    }, "")
  }
  node_text <- c(
    sprintf("<b>%s</b><br><br><i>Main labels</i><br>%s<br><br>Documents: %d<br>Structure: %s<br><br><i>Linked themes (%d)</i>%s",
            mpRootLabels(cl)[S$cluster], bullets(S$terms), S$size, struct_root[S$structure],
            S$n_themes, linkedNames(S$themes, mpThemeLabels(cl))),
    sprintf("<b>%s</b><br><br><i>Main labels</i><br>%s<br><br>Documents: %d<br>Structure: %s<br><br><i>Linked roots (%d)</i>%s",
            mpThemeLabels(cl)[Tm$cluster], bullets(Tm$terms), Tm$size, struct_theme[Tm$structure],
            Tm$n_roots, linkedNames(Tm$roots, mpRootLabels(cl)))
  )
  src <- match(lk$root, S$cluster) - 1
  tgt <- nrow(S) + match(lk$theme, Tm$cluster) - 1
  link_text <- sprintf(
    "<b>%s</b> &#8594; <b>%s</b><br><br>Documents: %d<br>Share of the root: %.0f%%<br>Share of the theme: %.0f%%<br>Standardized residual: %.1f",
    mpRootLabels(cl)[lk$root], mpThemeLabels(cl)[lk$theme], lk$n,
    100 * lk$share_root, 100 * lk$share_theme, lk$residual
  )
  p <- plotly::plot_ly(
    type = "sankey",
    arrangement = "snap",
    node = list(
      label = c(mpRootLabels(cl)[S$cluster], mpThemeLabels(cl)[Tm$cluster]),
      x = c(rep(0.001, nrow(S)), rep(0.999, nrow(Tm))),
      y = c(pos(nrow(S)), pos(nrow(Tm))),
      color = c(root_cols, unname(theme_cols)),
      pad = 6,
      thickness = 16,
      customdata = node_text,
      hovertemplate = "%{customdata}<extra></extra>"
    ),
    link = list(
      source = src,
      target = tgt,
      value = lk$n,
      color = vapply(root_cols[match(lk$root, S$cluster)], grDevices::adjustcolor, "", alpha.f = 0.45,
                     USE.NAMES = FALSE),
      customdata = link_text,
      hovertemplate = "%{customdata}<extra></extra>"
    )
  )
  p <- plotly::layout(p, margin = list(l = 50, r = 50, b = 60, t = 80, pad = 4))
  p <- plotly::add_annotations(
    p, x = c(0, 1), y = 1.06, text = c("ROOTS (references)", "THEMES (topics)"),
    showarrow = FALSE, xanchor = c("left", "right"), font = list(size = 15)
  )
  p <- plotly::config(p, displaylogo = FALSE)
  mpSankeyHighlight(p)
}

# Clicking a node of the sankey greys out everything not connected to it:
# its flows and the nodes at their other end keep their colour. Clicking the
# same node again, clicking outside the nodes or a double click restores the
# colours. The nodes can be dragged, and plotly then never receives the click
# of a real mouse (no plotly_click): a click is recognised here as a mouse
# press and release on the same node, less than 5 pixels apart. Attached as a
# render hook, which is what htmlwidgets::onRender() does, so that htmlwidgets
# need not be declared (it is installed with plotly).
mpSankeyHighlight <- function(p) {
  js <- "
function(el) {
  var orig = null, selected = null, down = null;
  var arr = function(v) { return [].concat(v); };
  var save = function() {
    if (orig === null) {
      var tr = el.data[0];
      orig = {node: arr(tr.node.color).slice(), link: arr(tr.link.color).slice()};
    }
  };
  var restore = function() {
    if (orig === null || selected === null) return;
    selected = null;
    Plotly.restyle(el, {'node.color': [orig.node], 'link.color': [orig.link]}, [0]);
  };
  var highlight = function(k) {
    save();
    selected = k;
    var tr = el.data[0];
    var src = arr(tr.link.source), tgt = arr(tr.link.target), keep = {};
    keep[k] = true;
    var lc = src.map(function(s, i) {
      var on = (s === k || tgt[i] === k);
      if (on) { keep[s] = true; keep[tgt[i]] = true; }
      return on ? orig.link[i] : 'rgba(220,220,220,0.35)';
    });
    var nc = orig.node.map(function(c, i) { return keep[i] ? c : 'rgba(205,205,205,0.6)'; });
    Plotly.restyle(el, {'node.color': [nc], 'link.color': [lc]}, [0]);
  };
  el.addEventListener('mousedown', function(e) {
    var g = e.target.closest ? e.target.closest('.sankey-node') : null;
    var k = (g && g.__data__ && g.__data__.node) ? g.__data__.node.pointNumber : null;
    down = {k: k, x: e.clientX, y: e.clientY};
  }, true);
  el.addEventListener('mouseup', function(e) {
    if (down === null) return;
    var moved = Math.abs(e.clientX - down.x) + Math.abs(e.clientY - down.y);
    var k = down.k;
    down = null;
    if (moved > 5) return;
    if (k === null || k === undefined || selected === k) restore(); else highlight(k);
  }, true);
  el.addEventListener('dblclick', restore);
}"
  p$jsHooks$render <- c(p$jsHooks$render, list(list(code = js, data = NULL)))
  p
}

# the documents, coloured by root, edges coloured by layer
mpNetwork <- function(mc, max.nodes = 500, seed = 1234) {
  rng <- saveRNG()
  on.exit(restoreRNG(rng), add = TRUE)
  g <- mc$layers$union
  nodes <- mc$nodes
  memb <- mc$clusters$membership$root
  keep <- seq_len(nrow(nodes))
  if (length(keep) > max.nodes) keep <- order(-(nodes$degree_R + nodes$degree_T))[seq_len(max.nodes)]
  g <- igraph::induced_subgraph(g, keep)
  set.seed(seed)
  lay <- igraph::layout_with_fr(g, weights = igraph::E(g)$s_R + igraph::E(g)$s_T + 1e-6)
  ci <- nodes$CI[keep]
  ci[is.na(ci)] <- 0
  cols <- ifelse(is.na(memb[keep]), "#CCCCCC", colorlist()[((memb[keep] - 1) %% length(colorlist())) + 1])
  e <- igraph::as_edgelist(g, names = FALSE)
  type_lab <- c(both = "both layers", references_only = "references only", topics_only = "topics only")
  vn_nodes <- data.frame(
    id = seq_along(keep), label = "",
    title = sprintf("%s<br>%s | convergence index %.2f", nodes$node[keep],
                    ifelse(is.na(memb[keep]), "in no root", paste0("root R", memb[keep])), ci),
    color = cols, value = 1 + 10 * ci^2, x = lay[, 1] * 100, y = lay[, 2] * 100,
    stringsAsFactors = FALSE
  )
  vn_edges <- data.frame(
    from = e[, 1], to = e[, 2], color = MP_EDGE_COLORS[igraph::E(g)$edge_type],
    group = type_lab[igraph::E(g)$edge_type], title = type_lab[igraph::E(g)$edge_type],
    stringsAsFactors = FALSE
  )
  vn <- visNetwork::visNetwork(vn_nodes, vn_edges)
  vn <- visNetwork::visPhysics(vn, enabled = FALSE)
  visNetwork::visOptions(vn, highlightNearest = TRUE)
}
