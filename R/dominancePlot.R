utils::globalVariables(c("Quadrant", "Label", "LabelY", "Articles", "DomFactor", "Multi", "First"))
#' Plot the authors' dominance ranking
#'
#' It draws the output of \code{\link{dominance}} as a quadrant scatter plot:
#' each author is placed by productivity (N. of articles, x axis) and by
#' Dominance Factor (y axis), with point size proportional to the N. of
#' multi-authored articles, which is the denominator of the Dominance Factor.\cr\cr
#'
#' The vertical line is the median productivity of the plotted authors; the
#' horizontal line is a Dominance Factor of 0.5, above which an author is first
#' author in at least half of their multi-authored articles (Kumar & Kumar, 2008).
#' The two lines split the authors into four groups:
#' \tabular{lll}{
#' \code{Prolific leaders}     \tab   \tab above median productivity, DF >= 0.5\cr
#' \code{Leaders}              \tab   \tab below median productivity, DF >= 0.5\cr
#' \code{Prolific co-authors}  \tab   \tab above median productivity, DF < 0.5\cr
#' \code{Co-authors}           \tab   \tab below median productivity, DF < 0.5}
#'
#' @param DF is a data frame returned by \code{\link{dominance}}.
#' @param labels is logical. If TRUE, the authors' names are drawn above their points. When two names would overlap, only the one of the author with the higher Dominance Factor is drawn. Default is \code{labels = TRUE}.
#' @param logo is logical. If TRUE, the bibliometrix logo is drawn in the bottom-right corner. Set it to FALSE for a plot converted with \code{plotly::ggplotly()}, which cannot draw it. Default is \code{logo = TRUE}.
#' @return The function \code{dominancePlot} returns a plot in ggplot2 format.
#'
#' @examples
#' data(scientometrics, package = "bibliometrixData")
#' results <- biblioAnalysis(scientometrics)
#' DF <- dominance(results, k = 20)
#' dominancePlot(DF)
#'
#' @seealso \code{\link{dominance}} to compute the authors' dominance ranking.
#'
#' @export

dominancePlot <- function(DF, labels = TRUE, logo = TRUE) {
  required <- c(
    "Author",
    "Dominance Factor",
    "Tot Articles",
    "Multi-Authored",
    "First-Authored"
  )
  if (!is.data.frame(DF) || !all(required %in% names(DF))) {
    stop("DF has to be a data frame returned by dominance()", call. = FALSE)
  }
  if (nrow(DF) == 0) {
    stop(
      "DF has no authors: dominance() found no author with a multi-authored article",
      call. = FALSE
    )
  }

  df <- data.frame(
    Label = DF$Author,
    DomFactor = DF$`Dominance Factor`,
    Articles = DF$`Tot Articles`,
    Multi = DF$`Multi-Authored`,
    First = DF$`First-Authored`,
    stringsAsFactors = FALSE
  )

  ## Quadrants: median productivity x DF = 0.5 ----
  x_mid <- stats::median(df$Articles)
  prolific <- df$Articles > x_mid
  leader <- df$DomFactor >= 0.5
  quadrants <- c(
    "Prolific leaders",
    "Leaders",
    "Prolific co-authors",
    "Co-authors"
  )
  df$Quadrant <- factor(
    ifelse(
      leader,
      ifelse(prolific, quadrants[1], quadrants[2]),
      ifelse(prolific, quadrants[3], quadrants[4])
    ),
    levels = quadrants
  )
  quadrant_colors <- c(
    "Prolific leaders" = "#2171B5",
    "Leaders" = "#6BAED6",
    "Prolific co-authors" = "#D6604D",
    "Co-authors" = "#F4A582"
  )

  ## Authors with the same articles, DF and multi-authored articles share one
  ## point: draw it once, with their names stacked in one label ----
  key <- paste(df$Articles, df$DomFactor, df$Multi)
  names_by_key <- tapply(df$Label, key, paste, collapse = "\n")
  df <- df[!duplicated(key), ]
  df$Label <- as.character(names_by_key[key[!duplicated(key)]])

  xrange <- range(df$Articles)
  xpad <- logoDelta(xrange, frac = 0.08)
  xmin <- xrange[1] - xpad
  xmax <- xrange[2] + xpad
  # Room above DF = 1 for the names of the top bubbles and the quadrant names
  ylim <- c(-0.16, 1.24)

  g <- ggplot2::ggplot(df, ggplot2::aes(x = Articles, y = DomFactor)) +
    ggplot2::geom_hline(
      yintercept = 0.5,
      linetype = "dashed",
      color = "#666666",
      linewidth = 0.4
    ) +
    ggplot2::geom_vline(
      xintercept = x_mid,
      linetype = "dashed",
      color = "#666666",
      linewidth = 0.4
    ) +
    # Quadrant names, in a band above and below the DF range that the author
    # labels are kept out of, centred in their half: plotly ignores hjust
    ggplot2::annotate(
      "text",
      x = rep(c((x_mid + xmax) / 2, (xmin + x_mid) / 2), 2),
      y = c(1.21, 1.21, -0.13, -0.13),
      label = quadrants,
      fontface = "bold",
      size = 3.5,
      color = quadrant_colors[quadrants],
      alpha = 0.8
    ) +
    # text is the hover label of plotly::ggplotly(); ggplot2 ignores it
    suppressWarnings(ggplot2::geom_point(
      ggplot2::aes(
        size = Multi,
        fill = Quadrant,
        text = paste0(
          Label,
          "\nArticles: ",
          Articles,
          "\nMulti-authored: ",
          Multi,
          "\nFirst-authored: ",
          First,
          "\nDominance Factor: ",
          round(DomFactor, 3)
        )
      ),
      shape = 21,
      color = "white",
      alpha = 0.85
    )) +
    ggplot2::scale_fill_manual(values = quadrant_colors, guide = "none") +
    ggplot2::scale_size_area(
      max_size = 12,
      name = "Multi-authored articles",
      breaks = function(l) unique(round(pretty(l, n = 3)))
    ) +
    ggplot2::guides(
      size = ggplot2::guide_legend(override.aes = list(fill = "#969696"))
    ) +
    ggplot2::scale_y_continuous(
      limits = ylim,
      breaks = seq(0, 1, 0.25)
    ) +
    ggplot2::scale_x_continuous(limits = c(xmin, xmax)) +
    ggplot2::labs(
      x = "N. of Articles",
      y = "Dominance Factor",
      title = "Authors' Dominance",
      subtitle = "Dominance Factor = first-authored / multi-authored articles"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      text = ggplot2::element_text(color = "#333333"),
      plot.title = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(
        size = 10,
        color = "#666666",
        hjust = 0.5
      ),
      axis.title = ggplot2::element_text(size = 12),
      axis.line = ggplot2::element_line(color = "#333333", linewidth = 0.4),
      panel.grid.major = ggplot2::element_line(
        color = "#EBEBEB",
        linewidth = 0.3
      ),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  if (isTRUE(labels)) {
    g <- g +
      ggplot2::geom_text(
        data = dominanceLabels(df, xmin, xmax, ylim),
        ggplot2::aes(y = LabelY, label = Label),
        size = 3.2,
        lineheight = 0.9,
        color = "#333333"
      )
  }

  if (!isTRUE(logo)) {
    return(g)
  }

  ## Logo, bottom right
  data("logo", package = "bibliometrix", envir = environment())
  logoGrid <- grid::rasterGrob(logo, interpolate = TRUE)
  g +
    ggplot2::annotation_custom(
      logoGrid,
      xmin = xmax - logoDelta(c(xmin, xmax), frac = 0.10),
      xmax = xmax,
      ymin = -0.16,
      ymax = -0.02
    )
}

# The labels to draw, one line above each bubble. Going down the Dominance
# Factor, a label is kept only if its box overlaps neither a label already
# kept nor another bubble, so of two close bubbles only the one with the
# higher DF is named. Sizes are in fractions of the panel, for a 3.2 mm font
# on a plot of about 9 x 6.5 inches (panel about 7.8 x 4.7 inches); the
# radius follows scale_size_area(max_size = 12).
dominanceLabels <- function(df, xmin, xmax, ylim) {
  yspan <- diff(ylim)
  n_lines <- lengths(strsplit(df$Label, "\n", fixed = TRUE))
  n_chars <- vapply(
    strsplit(df$Label, "\n", fixed = TRUE),
    function(l) max(nchar(l)),
    numeric(1)
  )
  w <- n_chars * 0.009
  h <- n_lines * 0.03
  radius <- 0.07 * sqrt(df$Multi / max(df$Multi))
  df$LabelY <- df$DomFactor + radius + h * yspan / 2
  cx <- (df$Articles - xmin) / (xmax - xmin)
  cy <- (df$LabelY - ylim[1]) / yspan
  # Bubbles, as boxes around their centres
  py <- (df$DomFactor - ylim[1]) / yspan
  ry <- radius / yspan
  rx <- ry * 4.7 / 7.8

  keep <- logical(nrow(df))
  for (i in order(-df$DomFactor, -df$Articles)) {
    on_label <- keep &
      abs(cx - cx[i]) < (w + w[i]) / 2 &
      abs(cy - cy[i]) < (h + h[i]) / 2
    on_bubble <- seq_along(keep) != i &
      abs(cx - cx[i]) < w[i] / 2 + rx &
      abs(py - cy[i]) < h[i] / 2 + ry
    keep[i] <- !any(on_label) && !any(on_bubble)
  }
  df[keep, , drop = FALSE]
}
