utils::globalVariables(c("Quadrant", "Label", "Articles", "DomFactor", "Multi"))
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
#' @param labels is logical. If TRUE, the authors' names are drawn next to their points. Default is \code{labels = TRUE}.
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

dominancePlot <- function(DF, labels = TRUE) {
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
    # labels are kept out of: left ones at the left edge, right ones at the median
    ggplot2::annotate(
      "text",
      x = c(x_mid, xmin, x_mid, xmin),
      y = c(1.13, 1.13, -0.13, -0.13),
      label = quadrants,
      hjust = -0.05,
      fontface = "bold",
      size = 3.5,
      color = quadrant_colors[quadrants],
      alpha = 0.8
    ) +
    ggplot2::geom_point(
      ggplot2::aes(size = Multi, fill = Quadrant),
      shape = 21,
      color = "white",
      alpha = 0.85
    ) +
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
      limits = c(-0.16, 1.16),
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
      ggrepel::geom_text_repel(
        ggplot2::aes(label = Label),
        size = 3.2,
        color = "#333333",
        point.padding = 0.4,
        box.padding = 0.4,
        min.segment.length = 0.3,
        segment.color = "#999999",
        max.overlaps = Inf,
        ylim = c(-0.06, 1.06),
        seed = 1
      )
  }

  ## Logo, bottom right: the bottom-right quadrant name starts at the median line
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
