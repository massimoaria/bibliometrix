utils::globalVariables(c(
  "themes_lab", "schools_lab", "res", "n_lab", "text", "x", "y", "xend", "yend", "size",
  "structure", "label", "quadrant", "show", "x_first", "y_first", "x_last", "y_last",
  "trend", "pair", "cohesion_R", "cohesion_T", "layer", "relation", "short"
))

#' Plot a multiplex coupling
#'
#' It draws the results of \code{\link{multiplexClusters}} and
#' \code{\link{multiplexEvolution}}.
#'
#' \tabular{lll}{
#' \code{"matrix"}     \tab \tab schools x themes: documents and standardized residuals; a link is a red cell with at least \code{min.link} documents\cr
#' \code{"links"}      \tab \tab schools on the left, themes on the right, one flow per link: a school with several flows is branching, a theme with several flows is convergent (interactive: a sankey diagram, with nodes that can be dragged)\cr
#' \code{"plane"}      \tab \tab pairs of schools by roots and topic proximity; the pairs linked to the same theme are named with it\cr
#' \code{"clusters"}   \tab \tab schools and themes by cohesion in the two layers\cr
#' \code{"trajectory"} \tab \tab pairs of schools from their first to their last period (needs \code{\link{multiplexEvolution}})\cr
#' \code{"network"}    \tab \tab the documents, with the edges coloured by layer (interactive only)}
#'
#' @param x an object of class \code{"biblioMultiplex"} with clusters, or of
#'   class \code{"biblioMultiplexEvolution"} for \code{type = "trajectory"}.
#' @param type is a character. The plot, see Details. Default is \code{"matrix"}.
#' @param interactive is logical. If TRUE, a plotly widget (a visNetwork widget
#'   for \code{type = "network"}) instead of a ggplot. Default is FALSE.
#' @param n.labels is an integer. For \code{type = "plane"} and
#'   \code{"trajectory"}, the number of pairs named besides those linked to the
#'   same theme. Default is 12.
#' @param min.size is an integer. For \code{type = "plane"}, the minimum
#'   number of documents of the two schools of a pair. Default is 10.
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
                          type = c("matrix", "links", "plane", "clusters", "trajectory", "network"),
                          interactive = FALSE,
                          n.labels = 12,
                          min.size = 10,
                          max.nodes = 500) {
  type <- match.arg(type)
  if (type == "trajectory") {
    if (!inherits(x, "biblioMultiplexEvolution")) {
      stop("multiplexPlot(type = \"trajectory\") needs the result of multiplexEvolution()", call. = FALSE)
    }
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
MP_EDGE_COLORS <- c(both = "#1B9E77", roots_only = "#D95F02", topics_only = "#7570B3")

mpFirstLabel <- function(x) sub(";.*", "", x)

mpSchoolLabels <- function(cl) sprintf("S%d %s", cl$schools$cluster, mpFirstLabel(cl$schools$terms))
mpThemeLabels <- function(cl) sprintf("T%d %s", cl$themes$cluster, mpFirstLabel(cl$themes$terms))

mpTheme <- function(base) {
  base +
    ggplot2::theme(
      text = ggplot2::element_text(color = "#333333"),
      plot.title = ggplot2::element_text(face = "bold")
    )
}

# schools x themes, themes ordered by the school they are most over-represented in
mpMatrixPlot <- function(mc, interactive = FALSE) {
  cl <- mc$clusters
  R <- cl$residuals
  col_order <- order(apply(R, 2, which.max), -apply(R, 2, max))
  D <- as.data.frame(as.table(R), stringsAsFactors = FALSE)
  names(D) <- c("schools", "themes", "res")
  D$n <- as.vector(unclass(cl$contingency))
  lr <- mpSchoolLabels(cl)
  lt <- mpThemeLabels(cl)
  D$schools_lab <- factor(lr[as.integer(D$schools)], levels = rev(lr))
  D$themes_lab <- factor(lt[as.integer(D$themes)], levels = lt[col_order])
  D$n_lab <- ifelse(D$n > 0, D$n, "")
  D$text <- sprintf("%s x %s\n%d documents, residual %.1f", D$schools_lab, D$themes_lab, D$n, D$res)
  lim <- max(abs(D$res))
  g <- suppressWarnings(
    ggplot2::ggplot(D, ggplot2::aes(themes_lab, schools_lab, fill = res, text = text)) +
      ggplot2::geom_tile(colour = "white") +
      ggplot2::geom_text(ggplot2::aes(label = n_lab), size = 2.6) +
      ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", limits = c(-lim, lim),
                                    name = "standardized\nresidual") +
      ggplot2::labs(x = "themes (topic clusters)", y = "schools (roots clusters)",
                    title = "Schools x themes",
                    subtitle = sprintf("documents per cell; red = more than expected (NMI %.2f)", cl$NMI)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  )
  g <- mpTheme(g)
  if (interactive) plotly::ggplotly(g, tooltip = "text") else g
}

# schools on the left, themes on the right, one line per link (width = documents)
mpLinksPlot <- function(mc, interactive = FALSE) {
  if (interactive) return(mpLinksSankey(mc))
  cl <- mc$clusters
  lk <- cl$links
  S <- cl$schools
  Tm <- cl$themes
  # schools by number, themes at the mean height of their schools (weighted by
  # documents) so that lines cross as little as possible; themes with no link last
  S$y <- -seq_len(nrow(S))
  bary <- if (nrow(lk)) {
    tapply(S$y[lk$school] * lk$n, lk$theme, sum) / tapply(lk$n, lk$theme, sum)
  } else {
    numeric(0)
  }
  Tm$bary <- bary[as.character(Tm$cluster)]
  ord <- order(is.na(Tm$bary), -Tm$bary, -Tm$size)
  Tm$y <- NA_real_
  Tm$y[ord] <- -seq_len(nrow(Tm)) * nrow(S) / nrow(Tm)
  struct <- c(branching = "branching school", consolidation = "consolidated",
              convergence = "convergent theme", dispersed = "no link")
  cols <- c("branching school" = MP_COLORS[["branching"]], consolidated = MP_COLORS[["consolidation"]],
            "convergent theme" = MP_COLORS[["convergence"]], "no link" = MP_COLORS[["dispersed"]])
  N <- rbind(
    data.frame(x = 0, y = S$y, size = S$size, structure = struct[S$structure],
               label = mpSchoolLabels(cl),
               text = sprintf("School S%d: %s\n%d documents, %d linked themes", S$cluster, S$terms, S$size,
                              S$n_themes), stringsAsFactors = FALSE),
    data.frame(x = 1, y = Tm$y, size = Tm$size, structure = struct[Tm$structure],
               label = mpThemeLabels(cl),
               text = sprintf("Theme T%d: %s\n%d documents, %d linked schools", Tm$cluster, Tm$terms, Tm$size,
                              Tm$n_schools), stringsAsFactors = FALSE)
  )
  E <- data.frame(x = 0, y = S$y[lk$school], xend = 1, yend = Tm$y[match(lk$theme, Tm$cluster)], n = lk$n,
                  text = sprintf("S%d -> T%d: %d documents (%.0f%% of the school, %.0f%% of the theme), residual %.1f",
                                 lk$school, lk$theme, lk$n, 100 * lk$share_school, 100 * lk$share_theme, lk$residual))
  min.link <- cl$params$values[cl$params$params == "min.link"]
  g <- suppressWarnings(
    ggplot2::ggplot() +
      ggplot2::geom_segment(data = E, ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                                                   linewidth = n, text = text),
                            colour = "grey55", alpha = 0.55, lineend = "round") +
      ggplot2::geom_point(data = N, ggplot2::aes(x, y, size = size, colour = structure, text = text)) +
      ggplot2::geom_text(data = N[N$x == 0, ], ggplot2::aes(x - 0.04, y, label = label), hjust = 1, size = 3.3) +
      ggplot2::geom_text(data = N[N$x == 1, ], ggplot2::aes(x + 0.04, y, label = label), hjust = 0, size = 3.3) +
      ggplot2::annotate("text", x = c(0, 1), y = 0, label = c("SCHOOLS (roots)", "THEMES (topics)"),
                        fontface = "bold", size = 3.8, hjust = c(1, 0)) +
      ggplot2::scale_linewidth(range = c(0.4, 5), name = "documents") +
      ggplot2::scale_size_area(max_size = 7, guide = "none") +
      ggplot2::scale_colour_manual(values = cols, name = NULL) +
      ggplot2::scale_x_continuous(limits = c(-0.9, 1.9)) +
      ggplot2::labs(title = "Schools and the themes they are linked to",
                    subtitle = sprintf("one line per link: more documents than expected (residual > 2), at least %s of them",
                                       min.link)) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "bottom")
  )
  mpTheme(g)
}

# themes linked to both schools of each pair ("-> T1 patents"), "" when none
mpSharedTheme <- function(cl, A, B) {
  lt <- mpThemeLabels(cl)
  linked <- split(cl$links$theme, factor(cl$links$school, levels = seq_len(nrow(cl$schools))))
  vapply(seq_along(A), function(i) {
    both <- intersect(linked[[A[i]]], linked[[B[i]]])
    if (length(both) == 0) return("")
    paste0("-> ", paste(lt[both], collapse = ", "))
  }, character(1))
}

# pairs of schools by roots and topic proximity relative to two random documents
mpPlanePlot <- function(mc, interactive = FALSE, n.labels = 12, min.size = 10, floor = 1 / 16) {
  cl <- mc$clusters
  P <- cl$plane
  P <- P[P$size_A >= min.size & P$size_B >= min.size, ]
  if (!nrow(P)) stop("multiplexPlot(): no pair of schools has at least ", min.size, " documents each", call. = FALSE)
  # two schools sharing no reference at all have lift 0: drawn at the floor
  P$x <- log2(pmax(P$lift_R, floor))
  P$y <- log2(pmax(P$lift_T, floor))
  P$size <- P$size_A + P$size_B
  P$pair <- paste0(mpFirstLabel(P$label_A), " / ", mpFirstLabel(P$label_B))
  P$theme <- mpSharedTheme(cl, P$A, P$B)
  # the relation of the pair, in words that are not those of the typology of
  # schools and themes (the pair "consolidation" is not a consolidated school)
  rel <- c(consolidation = "close in both", branching = "close in roots only",
           convergence = "close in topics only", detachment = "close in neither")
  P$relation <- factor(rel[as.character(P$quadrant)], levels = rel)
  rel_cols <- stats::setNames(MP_COLORS[names(rel)], rel)
  top <- order(-(abs(P$x) + abs(P$y)) * (P$quadrant != "detachment"))[seq_len(min(n.labels, nrow(P)))]
  top <- union(top, which(P$theme != ""))
  P$show <- ifelse(seq_len(nrow(P)) %in% top,
                   ifelse(P$theme == "", P$pair, paste0(P$pair, "\n", P$theme)), "")
  P$text <- sprintf("%s\nroots lift %.2f | topics lift %.2f\n%s%s", P$pair, P$lift_R, P$lift_T, P$relation,
                    ifelse(P$theme == "", "", paste0("\n", P$theme)))
  g <- suppressWarnings(
    ggplot2::ggplot(P, ggplot2::aes(x = x, y = y, text = text)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
      ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
      ggplot2::geom_point(ggplot2::aes(size = size, colour = relation), alpha = 0.7) +
      ggplot2::scale_colour_manual(values = rel_cols, drop = FALSE) +
      ggplot2::scale_size_area(max_size = 12, guide = "none") +
      ggplot2::scale_x_continuous(breaks = function(l) unique(c(log2(floor), pretty(l))),
                                  labels = function(b) ifelse(b == log2(floor), paste0("<=", log2(floor)), b)) +
      ggplot2::labs(x = "Roots proximity (log2 lift)", y = "Topic proximity (log2 lift)", colour = NULL,
                    title = "Pairs of schools",
                    subtitle = sprintf(paste0("pairs of schools with at least %d documents each; ",
                                              "-> theme both schools are linked to"), min.size)) +
      ggplot2::theme_minimal()
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
    return(plotly::ggplotly(mpTheme(g), tooltip = "text"))
  }
  mpTheme(g + ggrepel::geom_text_repel(ggplot2::aes(label = show), size = 3, max.overlaps = 30))
}

# schools and themes by cohesion in the two layers
mpClustersPlot <- function(mc, interactive = FALSE) {
  cl <- mc$clusters
  D <- rbind(
    data.frame(layer = "school (roots cluster)", cl$schools[, c("cluster", "size", "terms", "cohesion_R", "cohesion_T")],
               structure = cl$schools$structure, stringsAsFactors = FALSE),
    data.frame(layer = "theme (topic cluster)", cl$themes[, c("cluster", "size", "terms", "cohesion_R", "cohesion_T")],
               structure = cl$themes$structure, stringsAsFactors = FALSE)
  )
  # a cluster of one document has no cohesion; one sharing nothing has 0
  D <- D[is.finite(log2(D$cohesion_R)) & is.finite(log2(D$cohesion_T)), ]
  D$label <- mpFirstLabel(D$terms)
  D$text <- sprintf("%s %d: %s\n%d documents | roots cohesion %.2f | topic cohesion %.2f\n%s",
                    D$layer, D$cluster, D$terms, D$size, D$cohesion_R, D$cohesion_T, D$structure)
  g <- suppressWarnings(
    ggplot2::ggplot(D, ggplot2::aes(log2(cohesion_R), log2(cohesion_T), text = text)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
      ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
      ggplot2::geom_point(ggplot2::aes(size = size, colour = structure, shape = layer), alpha = 0.75) +
      ggplot2::scale_colour_manual(values = MP_COLORS[c("consolidation", "branching", "convergence", "dispersed")]) +
      ggplot2::scale_size_area(max_size = 12, guide = "none") +
      ggplot2::labs(x = "Roots cohesion (log2 lift)", y = "Topic cohesion (log2 lift)", colour = NULL, shape = NULL,
                    title = "Cohesion of schools and themes",
                    subtitle = "schools: read the height (low = shared roots, different topics); themes: read the position (left = different roots)") +
      ggplot2::theme_minimal()
  )
  if (interactive) {
    return(plotly::ggplotly(mpTheme(g), tooltip = "text"))
  }
  mpTheme(g + ggrepel::geom_text_repel(ggplot2::aes(label = label), size = 3, max.overlaps = 25))
}

# pairs of schools from their first to their last period
mpTrajectoryPlot <- function(ev, interactive = FALSE, n.pairs = 12) {
  tr <- ev$trajectories
  tr <- tr[tr$trend != "stable", ]
  if (!nrow(tr)) stop("multiplexPlot(): every pair of schools is stable", call. = FALSE)
  tr <- utils::head(tr[order(-abs(tr$slope_T)), ], n.pairs)
  tr$pair <- paste0(mpFirstLabel(tr$label_A), " / ", mpFirstLabel(tr$label_B))
  cols <- c(converging = MP_COLORS[["convergence"]], diverging = MP_COLORS[["branching"]],
            `drifting apart` = "grey40", consolidating = MP_COLORS[["consolidation"]])
  if (interactive) {
    L <- ev$long
    L$pair <- paste0(mpFirstLabel(ev$schools$terms[L$A]), " / ", mpFirstLabel(ev$schools$terms[L$B]))
    L <- L[paste(L$A, L$B) %in% paste(tr$A, tr$B), ]
    L$trend <- tr$trend[match(paste(L$A, L$B), paste(tr$A, tr$B))]
    L$size <- L$n_A + L$n_B
    p <- plotly::plot_ly(L, x = ~x, y = ~y, frame = ~label, text = ~pair, color = ~trend, colors = cols,
                         size = ~size, type = "scatter", mode = "markers")
    return(plotly::layout(p, xaxis = list(title = "Roots proximity (log2 lift)"),
                          yaxis = list(title = "Topic proximity (log2 lift)")))
  }
  mpTheme(
    ggplot2::ggplot(tr) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
      ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
      ggplot2::geom_segment(ggplot2::aes(x = x_first, y = y_first, xend = x_last, yend = y_last, colour = trend),
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")), linewidth = 0.8) +
      ggrepel::geom_text_repel(ggplot2::aes(x = x_last, y = y_last, label = pair), size = 2.8, max.overlaps = 30) +
      ggplot2::scale_colour_manual(values = cols) +
      ggplot2::labs(x = "Roots proximity (log2 lift)", y = "Topic proximity (log2 lift)", colour = NULL,
                    title = sprintf("Pairs of schools, %s to %s", ev$periods[1], ev$periods[length(ev$periods)]),
                    subtitle = "arrow: from the first to the last period in which both schools have documents") +
      ggplot2::theme_minimal()
  )
}

# The links as a sankey diagram, as plotThematicEvolution() and
# threeFieldsPlot() draw theirs: schools on the left, themes on the right, one
# flow per link (width = documents). Every school has its colour and its flows
# take it, so the themes a branching school feeds can be followed; themes are
# coloured by their structure. Nodes can be dragged.
mpLinksSankey <- function(mc) {
  cl <- mc$clusters
  lk <- cl$links
  if (!nrow(lk)) stop("multiplexPlot(): no school is linked to a theme", call. = FALSE)
  S <- cl$schools[sort(unique(lk$school)), ]
  Tm <- cl$themes[sort(unique(lk$theme)), ]
  # schools by number, themes at the mean rank of their schools (weighted by
  # documents), so that flows cross as little as possible
  S$rank <- seq_len(nrow(S))
  sr <- S$rank[match(lk$school, S$cluster)]
  bary <- tapply(sr * lk$n, lk$theme, sum) / tapply(lk$n, lk$theme, sum)
  Tm <- Tm[order(bary[as.character(Tm$cluster)], -Tm$size), ]
  pos <- function(k) (seq_len(k) - 0.5) / k
  school_cols <- colorlist()[((S$cluster - 1) %% length(colorlist())) + 1]
  theme_cols <- c(convergence = MP_COLORS[["convergence"]], consolidation = MP_COLORS[["consolidation"]],
                  dispersed = MP_COLORS[["dispersed"]])[Tm$structure]
  struct_school <- c(branching = "branching school", consolidation = "consolidated school",
                     dispersed = "dispersed school")
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
            mpSchoolLabels(cl)[S$cluster], bullets(S$terms), S$size, struct_school[S$structure],
            S$n_themes, linkedNames(S$themes, mpThemeLabels(cl))),
    sprintf("<b>%s</b><br><br><i>Main labels</i><br>%s<br><br>Documents: %d<br>Structure: %s<br><br><i>Linked schools (%d)</i>%s",
            mpThemeLabels(cl)[Tm$cluster], bullets(Tm$terms), Tm$size, struct_theme[Tm$structure],
            Tm$n_schools, linkedNames(Tm$schools, mpSchoolLabels(cl)))
  )
  src <- match(lk$school, S$cluster) - 1
  tgt <- nrow(S) + match(lk$theme, Tm$cluster) - 1
  link_text <- sprintf(
    "<b>%s</b> &#8594; <b>%s</b><br><br>Documents: %d<br>Share of the school: %.0f%%<br>Share of the theme: %.0f%%<br>Standardized residual: %.1f",
    mpSchoolLabels(cl)[lk$school], mpThemeLabels(cl)[lk$theme], lk$n,
    100 * lk$share_school, 100 * lk$share_theme, lk$residual
  )
  p <- plotly::plot_ly(
    type = "sankey",
    arrangement = "snap",
    node = list(
      label = c(mpSchoolLabels(cl)[S$cluster], mpThemeLabels(cl)[Tm$cluster]),
      x = c(rep(0.001, nrow(S)), rep(0.999, nrow(Tm))),
      y = c(pos(nrow(S)), pos(nrow(Tm))),
      color = c(school_cols, unname(theme_cols)),
      pad = 6,
      thickness = 16,
      customdata = node_text,
      hovertemplate = "%{customdata}<extra></extra>"
    ),
    link = list(
      source = src,
      target = tgt,
      value = lk$n,
      color = vapply(school_cols[match(lk$school, S$cluster)], grDevices::adjustcolor, "", alpha.f = 0.45,
                     USE.NAMES = FALSE),
      customdata = link_text,
      hovertemplate = "%{customdata}<extra></extra>"
    )
  )
  p <- plotly::layout(p, margin = list(l = 50, r = 50, b = 60, t = 80, pad = 4))
  p <- plotly::add_annotations(
    p, x = c(0, 1), y = 1.06, text = c("SCHOOLS (roots)", "THEMES (topics)"),
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

# the documents, coloured by school, edges coloured by layer
mpNetwork <- function(mc, max.nodes = 500, seed = 1234) {
  rng <- saveRNG()
  on.exit(restoreRNG(rng), add = TRUE)
  g <- mc$layers$union
  nodes <- mc$nodes
  memb <- mc$clusters$membership$school
  keep <- seq_len(nrow(nodes))
  if (length(keep) > max.nodes) keep <- order(-(nodes$degree_R + nodes$degree_T))[seq_len(max.nodes)]
  g <- igraph::induced_subgraph(g, keep)
  set.seed(seed)
  lay <- igraph::layout_with_fr(g, weights = igraph::E(g)$s_R + igraph::E(g)$s_T + 1e-6)
  ci <- nodes$CI[keep]
  ci[is.na(ci)] <- 0
  cols <- colorlist()[((memb[keep] - 1) %% length(colorlist())) + 1]
  e <- igraph::as_edgelist(g, names = FALSE)
  type_lab <- c(both = "both layers", roots_only = "roots only", topics_only = "topics only")
  vn_nodes <- data.frame(
    id = seq_along(keep), label = "",
    title = sprintf("%s<br>school S%d | convergence index %.2f", nodes$node[keep], memb[keep], ci),
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
