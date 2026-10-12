utils::globalVariables(c(
  "X",
  "Y",
  "x0",
  "y0",
  "x1",
  "y1",
  "xend",
  "yend",
  "angle",
  "hjust",
  "vjust",
  "text",
  "rel_freq",
  "linked",
  "fill",
  "lab_from",
  "lab_to"
))

#' Plot a Thematic Evolution as stacked thematic maps
#'
#' It draws the thematic maps of the periods of a \code{\link{thematicEvolution}}
#' analysis as planes stacked in an oblique view, the oldest period at the
#' bottom. Each theme sits on its plane at its relevance degree (centrality)
#' and development degree (density), as in \code{\link{thematicMap}}, and the
#' themes of adjacent periods that share words are linked. The plot shows how a
#' theme moves across the quadrants while it merges with or splits from other
#' themes: the sankey diagram of \code{\link{plotThematicEvolution}} shows the
#' flows but not the positions, the thematic maps the positions but not the flows.
#'
#' The links that \code{thematicEvolution()} assigns to a lineage
#' (\code{assign.evolution.colors}) are drawn in the colour of the lineage, and
#' so are the themes they join; the other links are thin and faint, the other
#' themes grey. Without lineage colours every link is drawn in grey, with its
#' width proportional to its weight.
#'
#' The position of a theme is relative to its period: the ranks of centrality
#' and density are centred on their mean (the cross of the quadrants) and scaled
#' to the same range on every plane. The size of a theme is its frequency
#' relative to the largest theme of the period.
#'
#' @param nexus is the list returned by \code{\link{thematicEvolution}}.
#' @param min.flow is numerical. Links whose lineage strength (the weight used by
#'   \code{\link{plotThematicEvolution}}) is below \code{min.flow} are not drawn.
#'   Default is \code{min.flow = 0}.
#' @param label.size is numerical. The size of the theme labels in the static plot.
#'   Default is \code{label.size = 3}.
#' @param interactive is logical. If TRUE, a plotly widget with tooltips: a click
#'   on a theme keeps the themes it comes from and those it leads to, fades the
#'   rest and names the selected theme above the plot; a click on the same theme,
#'   or a double click, restores the plot.
#'   Default is \code{interactive = FALSE}, a ggplot object.
#' @return a ggplot object, or a plotly widget if \code{interactive = TRUE}.
#'
#' @examples
#' \dontrun{
#' data(management, package = "bibliometrixData")
#' nexus <- thematicEvolution(management, field = "DE", years = c(2004, 2008, 2013),
#'   n = 250, minFreq = 2)
#' plotThematicLayers(nexus)
#' plotThematicLayers(nexus, interactive = TRUE)
#' }
#' @seealso \code{\link{thematicEvolution}} to perform a thematic evolution analysis.
#' @seealso \code{\link{plotThematicEvolution}} to plot it as a sankey diagram.
#' @seealso \code{\link{thematicMap}} for the thematic map of a single period.
#' @export
plotThematicLayers <- function(
  nexus,
  min.flow = 0,
  label.size = 3,
  interactive = FALSE
) {
  if (
    !is.list(nexus) ||
      !all(c("TM", "Nodes", "Edges") %in% names(nexus)) ||
      isFALSE(nexus$check)
  ) {
    stop(
      "plotThematicLayers(): 'nexus' must be the result of thematicEvolution().",
      call. = FALSE
    )
  }
  need <- c("name", "freq", "rcentrality", "rdensity")
  if (!all(vapply(nexus$TM, function(t) all(need %in% names(t$clusters)), TRUE))) {
    stop(
      "plotThematicLayers(): the clusters of the periods ('nexus$TM[[i]]$clusters') ",
      "must keep the columns ", paste(need, collapse = ", "),
      " returned by thematicEvolution().",
      call. = FALSE
    )
  }
  g <- teLayerGeometry(nexus, min.flow = min.flow)
  if (interactive) teLayersPlotly(g) else teLayersGgplot(g, label.size)
}

TE_QUADRANTS <- c(
  motor = "Motor themes",
  niche = "Niche themes",
  emerging = "Emerging or declining themes",
  basic = "Basic themes"
)
TE_NO_LINEAGE <- "#7F7F7F"
TE_GREY <- "#D9D9D9"

# Planes, axes, labels, themes and links of the stacked thematic maps, already
# projected: a point (x, y) of plane i, both in [-1, 1], goes to
# (x + shear * y, squash * y + gap * (i - 1)).
teLayerGeometry <- function(
  nexus,
  min.flow = 0,
  shear = 0.55,
  squash = 0.38,
  gap = 1.25,
  pad = 0.18
) {
  periods <- sort(unique(nexus$Nodes$group))
  K <- length(periods)
  proj <- function(x, y, i) {
    data.frame(X = x + shear * y, Y = squash * y + gap * (i - 1))
  }
  scale1 <- function(v) {
    v <- v - mean(v)
    m <- max(abs(v))
    if (m == 0) v else v / m
  }
  pts <- bind_rows(lapply(seq_len(K), function(i) {
    cl <- nexus$TM[[i]]$clusters
    if (is.null(cl) || nrow(cl) == 0) {
      return(NULL)
    }
    d <- data.frame(
      name = cl$name,
      freq = cl$freq,
      x = scale1(cl$rcentrality),
      y = scale1(cl$rdensity),
      group = periods[i],
      slice = i,
      stringsAsFactors = FALSE
    )
    d$rel_freq <- d$freq / max(d$freq)
    cbind(d, proj(d$x, d$y, i))
  }))
  pts$label <- paste(pts$name, pts$group, sep = "--")
  pts$quadrant <- unname(ifelse(
    pts$y >= 0,
    ifelse(pts$x >= 0, TE_QUADRANTS["motor"], TE_QUADRANTS["niche"]),
    ifelse(pts$x >= 0, TE_QUADRANTS["basic"], TE_QUADRANTS["emerging"])
  ))

  # links: Edges refers to the nodes by Nodes$id
  ed <- as.data.frame(nexus$Edges)
  ed$weight <- if ("lineage_strength" %in% names(ed)) {
    ed$lineage_strength
  } else {
    ed$Inc_Weighted
  }
  ids <- nexus$Nodes[, c("id", "label")]
  ed <- ed[!is.na(ed$weight) & ed$weight >= min.flow, ]
  ed <- ed %>%
    left_join(rename(ids, from = "id", lab_from = "label"), by = "from") %>%
    left_join(rename(ids, to = "id", lab_to = "label"), by = "to") %>%
    inner_join(
      select(pts, lab_from = "label", x0 = "X", y0 = "Y"),
      by = "lab_from"
    ) %>%
    inner_join(select(pts, lab_to = "label", x1 = "X", y1 = "Y"), by = "lab_to")
  # a link belongs to a lineage when assignEvolutionColors() gave it the
  # colour of the lineage; without lineage colours every link is drawn as one
  col <- toupper(substr(ifelse(is.na(ed$color), "", ed$color), 1, 7))
  ed$lineage <- nchar(col) == 7 & col != "#D3D3D3"
  coloured <- any(ed$lineage)
  if (nrow(ed) > 0 && !coloured) {
    ed$lineage <- TRUE
    col <- rep(TE_NO_LINEAGE, nrow(ed))
  }
  ed$colour <- ifelse(ed$lineage, col, TE_GREY)
  ed <- ed[order(ed$lineage), ]

  # a theme takes the colour of its lineage links (all of them share it);
  # without lineage colours the themes stay grey
  lin <- ed[ed$lineage & coloured, ]
  node_col <- c(
    stats::setNames(lin$colour, lin$lab_from),
    stats::setNames(lin$colour, lin$lab_to)
  )
  pts$lineage <- pts$label %in% names(node_col)
  pts$colour <- ifelse(pts$lineage, node_col[pts$label], TE_GREY)
  pts$linked <- pts$label %in% c(ed$lab_from, ed$lab_to)

  r <- 1 + pad
  planes <- bind_rows(lapply(seq_len(K), function(i) {
    cbind(proj(c(-r, r, r, -r), c(-r, -r, r, r), i), slice = i)
  }))
  axes <- bind_rows(lapply(seq_len(K), function(i) {
    h <- proj(c(-r, r), c(0, 0), i)
    v <- proj(c(0, 0), c(-r, r), i)
    data.frame(
      x = c(h$X[1], v$X[1]),
      y = c(h$Y[1], v$Y[1]),
      xend = c(h$X[2], v$X[2]),
      yend = c(h$Y[2], v$Y[2]),
      slice = i
    )
  }))
  quad <- bind_rows(lapply(seq_len(K), function(i) {
    cbind(
      proj(c(r, -r, -r, r) * 0.97, c(r, r, -r, -r) * 0.94, i),
      text = c("Motor", "Niche", "Emerging/declining", "Basic"),
      hjust = c(1, 0, 0, 1),
      slice = i
    )
  }))
  ang <- atan2(squash, shear) * 180 / pi
  axtitles <- bind_rows(lapply(seq_len(K), function(i) {
    # the density title sits low on the left edge, clear of the plane above
    cbind(
      proj(c(0, -r), c(-r, -0.4 * r), i),
      text = c("Relevance degree (Centrality)", "Development degree (Density)"),
      angle = c(0, ang),
      vjust = c(1.6, -0.6),
      slice = i
    )
  }))
  titles <- bind_rows(lapply(seq_len(K), function(i) {
    cbind(proj(-r, -r, i), text = periods[i])
  }))
  list(
    points = pts,
    edges = ed,
    planes = planes,
    axes = axes,
    quad = quad,
    axtitles = axtitles,
    titles = titles,
    periods = periods
  )
}

teLayersGgplot <- function(g, label.size = 3) {
  pts <- g$points
  pts$fill <- ifelse(pts$lineage, pts$colour, TE_GREY)
  p <- ggplot()
  # plane by plane, from the bottom, so that each covers what lies behind it
  for (i in seq_along(g$periods)) {
    p <- p +
      geom_polygon(
        data = g$planes[g$planes$slice == i, ],
        aes(X, Y),
        fill = "white",
        colour = "grey30",
        alpha = 0.75,
        linewidth = 0.4
      ) +
      geom_segment(
        data = g$axes[g$axes$slice == i, ],
        aes(x, y, xend = xend, yend = yend),
        colour = "grey60",
        linetype = "dashed",
        linewidth = 0.3
      ) +
      geom_text(
        data = g$quad[g$quad$slice == i, ],
        aes(X, Y, label = text, hjust = hjust),
        colour = "grey55",
        size = 2.3,
        fontface = "italic"
      ) +
      geom_text(
        data = g$axtitles[g$axtitles$slice == i, ],
        aes(X, Y, label = text, angle = angle, vjust = vjust),
        colour = "grey45",
        size = 2.4
      )
  }
  weak <- g$edges[!g$edges$lineage, ]
  lin <- g$edges[g$edges$lineage, ]
  p +
    geom_segment(
      data = weak,
      aes(x0, y0, xend = x1, yend = y1),
      colour = "grey50",
      alpha = 0.18,
      linewidth = 0.35
    ) +
    geom_segment(
      data = lin,
      aes(x0, y0, xend = x1, yend = y1, linewidth = weight),
      colour = lin$colour,
      alpha = 0.8,
      lineend = "round"
    ) +
    geom_point(
      data = pts,
      aes(X, Y, size = rel_freq, fill = fill, alpha = linked),
      colour = "grey20",
      shape = 21
    ) +
    ggrepel::geom_text_repel(
      data = pts,
      aes(X, Y, label = name, alpha = linked),
      size = label.size,
      fontface = "bold",
      min.segment.length = Inf,
      box.padding = 0.15,
      seed = 1,
      max.overlaps = Inf
    ) +
    geom_text(
      data = g$titles,
      aes(X, Y, label = text),
      hjust = 1.1,
      vjust = 0,
      size = 3.5,
      fontface = "bold"
    ) +
    scale_fill_identity() +
    scale_linewidth(range = c(0.3, 2.2), guide = "none") +
    scale_size(range = c(1, 6), limits = c(0, 1), guide = "none") +
    scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.4), guide = "none") +
    expand_limits(x = min(g$planes$X) - 0.9) +
    coord_equal(clip = "off") +
    theme_void()
}

teLayersPlotly <- function(g) {
  pts <- g$points
  ed <- g$edges
  n <- nrow(pts)
  idx <- stats::setNames(seq_len(n) - 1, pts$label)
  # tooltip: the themes a theme comes from and those it leads to
  flows <- function(lab, side) {
    e <- if (side == "in") ed[ed$lab_to == lab, ] else ed[ed$lab_from == lab, ]
    if (nrow(e) == 0) {
      return("none")
    }
    e <- e[order(-e$weight), ]
    other <- if (side == "in") e$lab_from else e$lab_to
    paste(
      sprintf(
        "%s (%.2f)",
        pts$name[match(other, pts$label)],
        e$weight
      ),
      collapse = ", "
    )
  }
  tip <- sprintf(
    "<b>%s</b><br>%s, %s<br>Frequency: %s<br>From: %s<br>To: %s",
    pts$name,
    pts$group,
    pts$quadrant,
    pts$freq,
    vapply(pts$label, flows, "", side = "in"),
    vapply(pts$label, flows, "", side = "out")
  )
  nodeOpacity <- ifelse(pts$linked, 1, 0.45)

  p <- plotly::plot_ly()
  for (i in seq_along(g$periods)) {
    pl <- g$planes[g$planes$slice == i, ]
    p <- plotly::add_trace(
      p,
      x = c(pl$X, pl$X[1]),
      y = c(pl$Y, pl$Y[1]),
      type = "scatter",
      mode = "lines",
      fill = "toself",
      fillcolor = "rgba(255,255,255,0.75)",
      line = list(color = "rgb(77,77,77)", width = 1),
      hoverinfo = "skip",
      showlegend = FALSE
    )
    ax <- g$axes[g$axes$slice == i, ]
    p <- plotly::add_trace(
      p,
      x = c(ax$x[1], ax$xend[1], NA, ax$x[2], ax$xend[2]),
      y = c(ax$y[1], ax$yend[1], NA, ax$y[2], ax$yend[2]),
      type = "scatter",
      mode = "lines",
      line = list(color = "rgb(153,153,153)", width = 1, dash = "dash"),
      hoverinfo = "skip",
      showlegend = FALSE
    )
  }
  # one trace per link, so that a click can grey out each of them; the end
  # points (indices of the themes) travel in customdata
  for (j in seq_len(nrow(ed))) {
    e <- ed[j, ]
    p <- plotly::add_trace(
      p,
      x = c(e$x0, e$x1),
      y = c(e$y0, e$y1),
      type = "scatter",
      mode = "lines",
      name = "tl_link",
      customdata = unname(c(idx[e$lab_from], idx[e$lab_to])),
      line = list(
        color = if (e$lineage) e$colour else "rgb(128,128,128)",
        width = if (e$lineage) 1 + 4 * e$weight else 0.6
      ),
      opacity = if (e$lineage) 0.8 else 0.25,
      hoverinfo = "skip",
      showlegend = FALSE
    )
  }
  p <- plotly::add_trace(
    p,
    x = pts$X,
    y = pts$Y,
    type = "scatter",
    mode = "markers+text",
    name = "tl_themes",
    text = pts$name,
    textposition = "top center",
    textfont = list(size = 11, color = "rgb(26,26,26)"),
    hovertext = tip,
    hoverinfo = "text",
    marker = list(
      size = 6 + 16 * pts$rel_freq,
      color = pts$colour,
      opacity = nodeOpacity,
      line = list(color = "rgb(51,51,51)", width = 1)
    ),
    showlegend = FALSE
  )

  ann <- c(
    lapply(seq_len(nrow(g$quad)), function(k) {
      list(
        x = g$quad$X[k],
        y = g$quad$Y[k],
        text = paste0("<i>", g$quad$text[k], "</i>"),
        xanchor = if (g$quad$hjust[k] == 1) "right" else "left",
        font = list(size = 10, color = "rgb(140,140,140)")
      )
    }),
    lapply(seq_len(nrow(g$axtitles)), function(k) {
      list(
        x = g$axtitles$X[k],
        y = g$axtitles$Y[k],
        text = g$axtitles$text[k],
        textangle = -g$axtitles$angle[k],
        yanchor = if (g$axtitles$angle[k] == 0) "top" else "bottom",
        font = list(size = 10, color = "rgb(115,115,115)")
      )
    }),
    lapply(seq_len(nrow(g$titles)), function(k) {
      list(
        x = g$titles$X[k],
        y = g$titles$Y[k],
        text = paste0("<b>", g$titles$text[k], "</b>"),
        xanchor = "right",
        yanchor = "bottom",
        font = list(size = 14)
      )
    })
  )
  ann <- lapply(ann, function(a) c(a, showarrow = FALSE))
  noaxis <- list(
    visible = FALSE,
    showgrid = FALSE,
    zeroline = FALSE,
    fixedrange = FALSE
  )
  p <- plotly::layout(
    p,
    xaxis = c(noaxis, list(range = c(min(g$planes$X) - 0.9, max(g$planes$X) + 0.1))),
    yaxis = c(noaxis, list(scaleanchor = "x", scaleratio = 1)),
    annotations = ann,
    hovermode = "closest",
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    margin = list(l = 10, r = 10, b = 10, t = 35)
  )
  p <- plotly::config(p, displaylogo = FALSE)
  # plotly recycles hoverinfo = "skip" over the points of a trace, and a
  # per-point "skip" does not take the trace out of hover and click: a click on
  # a theme then went to the link that starts there. The planes, axes and links
  # get it back as a single value.
  p <- plotly::plotly_build(p)
  p$x$data <- lapply(p$x$data, function(tr) {
    if (!identical(tr$name, "tl_themes")) tr$hoverinfo <- "skip"
    tr
  })
  teLayersHighlight(p, nodeOpacity, sprintf("%s (%s)", pts$name, pts$group))
}

# A click on a theme keeps the themes it comes from (back to the first period)
# and those it leads to (up to the last), with the links between them; the
# rest fades, and a note above the plot names the selected theme, so that a
# faded theme is not mistaken for a weak one. A click on the same theme, or a
# double click, restores the plot. Attached as a render hook, which is what
# htmlwidgets::onRender() does, so that htmlwidgets need not be declared (it
# is installed with plotly). Shiny renders the widget again (a tab shown, a
# resize) on the same element, and each render runs the hook: the hook removes
# its previous listeners first, otherwise every copy kept its own selection
# and added its own note, and a double click restored one copy only.
teLayersHighlight <- function(p, nodeOpacity, labels) {
  js <- "
function(el, x, data) {
  if (el.__tlClick) {
    el.removeListener('plotly_click', el.__tlClick);
    el.removeListener('plotly_doubleclick', el.__tlRestore);
  }
  var selected = null;
  var links = [], themes = null;
  el.data.forEach(function(tr, i) {
    if (tr.name === 'tl_link') links.push(i);
    if (tr.name === 'tl_themes') themes = i;
  });
  var linkOpacity = links.map(function(i) { return el.data[i].opacity; });
  var ends = links.map(function(i) { return el.data[i].customdata; });
  // the note is found by its name, never by a position kept from an earlier
  // render. Plotly.relayout() inserts an object given at an index that is
  // taken, so an existing note only gets its text changed: setting the whole
  // object stacked a new note at every click, and a double click removed one.
  var noteIndex = function() {
    var ann = el.layout.annotations || [];
    for (var i = 0; i < ann.length; i++) if (ann[i] && ann[i].name === 'tl_note') return i;
    return -1;
  };
  var note = function(text) {
    var i = noteIndex(), u = {};
    if (text === null) {
      if (i < 0) return;
      u['annotations[' + i + ']'] = null;
    } else if (i >= 0) {
      u['annotations[' + i + '].text'] = text;
    } else {
      u['annotations[' + (el.layout.annotations || []).length + ']'] = {
        name: 'tl_note', text: text, xref: 'paper', yref: 'paper', x: 0.5, y: 1,
        xanchor: 'center', yanchor: 'bottom', showarrow: false,
        font: {size: 13, color: 'rgb(51,51,51)'},
        bgcolor: 'rgba(255,243,205,0.95)', bordercolor: 'rgb(214,184,90)',
        borderwidth: 1, borderpad: 4
      };
    }
    Plotly.relayout(el, u);
  };
  var reach = function(k, a, b) {
    var seen = {}, stack = [k];
    seen[k] = true;
    while (stack.length) {
      var u = stack.pop();
      ends.forEach(function(e) {
        if (e[a] === u && !seen[e[b]]) { seen[e[b]] = true; stack.push(e[b]); }
      });
    }
    return seen;
  };
  var restore = function() {
    selected = null;
    Plotly.restyle(el, {opacity: linkOpacity}, links);
    Plotly.restyle(el, {'marker.opacity': [data.nodeOpacity],
      'textfont.color': 'rgb(26,26,26)'}, [themes]);
    note(null);
  };
  var highlight = function(k) {
    selected = k;
    var down = reach(k, 0, 1), up = reach(k, 1, 0);
    var on = function(e) {
      return (down[e[0]] && down[e[1]]) || (up[e[0]] && up[e[1]]);
    };
    Plotly.restyle(el, {opacity: ends.map(function(e, j) {
      return on(e) ? Math.max(linkOpacity[j], 0.8) : 0.12;
    })}, links);
    var keep = data.nodeOpacity.map(function(o, i) { return down[i] || up[i]; });
    Plotly.restyle(el, {
      'marker.opacity': [keep.map(function(on) { return on ? 1 : 0.3; })],
      'textfont.color': [keep.map(function(on) {
        return on ? 'rgb(26,26,26)' : 'rgba(26,26,26,0.35)';
      })]
    }, [themes]);
    note('Selected: <b>' + data.labels[k] + '</b>, with the themes it comes from and leads to.' +
      ' Click it again or double click to show all');
  };
  el.__tlClick = function(d) {
    var pt = d.points[0];
    if (pt.data.name !== 'tl_themes') return;
    if (selected === pt.pointNumber) restore(); else highlight(pt.pointNumber);
  };
  el.__tlRestore = restore;
  el.on('plotly_click', el.__tlClick);
  el.on('plotly_doubleclick', el.__tlRestore);
  // a render may keep the layout of the previous one, with its note
  note(null);
}"
  p$jsHooks$render <- c(
    p$jsHooks$render,
    list(list(code = js, data = list(nodeOpacity = nodeOpacity, labels = labels)))
  )
  p
}
