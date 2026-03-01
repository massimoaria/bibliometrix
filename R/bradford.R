utils::globalVariables(c(
  "Rank",
  "SO",
  "Freq",
  "cumFreq",
  "Zone",
  "Theoretical",
  "logRank",
  "xmin",
  "ymin"
))
#' Bradford's law
#'
#' It estimates Bradford's law source distribution and tests the goodness of fit.
#'
#' Bradford's Law of Scattering, first formulated by Samuel C. Bradford in 1934,
#' describes the phenomenon of concentration and dispersion in scientific publishing:
#' a small number of core journals account for a disproportionately large share of the
#' literature on a given topic, while the remaining literature is scattered across an
#' increasingly large number of peripheral journals.\cr\cr
#'
#' If journals are ranked in decreasing order of productivity and partitioned into three
#' zones, each containing roughly one-third of the total articles, the number of journals
#' in each zone follows the ratio 1:n:n^2, where n is the Bradford multiplier.\cr\cr
#'
#' The Bradford distribution models the cumulative number of articles C(r) contributed
#' by the top r sources as: C(r) = a + b * log(r)\cr\cr
#'
#' Reference:\cr
#' Bradford, S. C. (1934). Sources of information on specific subjects. Engineering, 137, 85-86.\cr
#'
#' @param M is a bibliographic dataframe.
#' @return The function \code{bradford} returns a list containing the following objects:
#' \tabular{lll}{
#' \code{table}       \tab   \tab a dataframe with the source distribution partitioned in the three zones\cr
#' \code{graph}       \tab   \tab the Bradford bibliograph plot in ggplot2 format\cr
#' \code{graph_shiny} \tab   \tab the Bradford bibliograph plot for biblioshiny (without logo)\cr
#' \code{zoneSummary} \tab   \tab a dataframe summarizing the three Bradford zones\cr
#' \code{stat}        \tab   \tab a list of statistical results (coefficients, R2, KS test, Bradford multiplier)}
#'
#' @examples
#' \dontrun{
#' data(management, package = "bibliometrixData")
#'
#' BR <- bradford(management)
#' }
#'
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis
#' @seealso \code{\link{summary}} method for class '\code{bibliometrix}'
#'
#' @export

bradford <- function(M) {
  ## 1. Sort sources by decreasing productivity ----
  SO <- sort(table(M$SO), decreasing = TRUE)
  nSO <- length(SO)
  N <- sum(SO)

  df <- data.frame(
    SO = names(SO),
    Rank = seq_len(nSO),
    Freq = as.numeric(SO),
    cumFreq = as.numeric(cumsum(SO)),
    stringsAsFactors = FALSE
  )

  ## 2. Fit Bradford's distribution: C(r) = a + b * log(r) ----
  df$logRank <- log(df$Rank)
  fit <- lm(cumFreq ~ logRank, data = df)
  a_coef <- as.numeric(coef(fit)[1])
  b_coef <- as.numeric(coef(fit)[2])
  R2 <- summary(fit)$r.squared
  df$Theoretical <- as.numeric(fitted(fit))

  ## 3. Zone assignment: each zone contains ~N/3 articles ----
  zone1_end <- sum(df$cumFreq <= N / 3) + 1
  zone2_end <- sum(df$cumFreq <= 2 * N / 3) + 1
  zone1_end <- min(zone1_end, nSO)
  zone2_end <- min(zone2_end, nSO)

  Z <- rep("Zone 3", nSO)
  Z[seq_len(zone1_end)] <- "Zone 1"
  if (zone1_end < nSO) {
    Z[(zone1_end + 1):zone2_end] <- "Zone 2"
  }
  df$Zone <- Z

  ## 4. Zone summary ----
  n1 <- sum(Z == "Zone 1")
  n2 <- sum(Z == "Zone 2")
  n3 <- sum(Z == "Zone 3")
  freq1 <- sum(df$Freq[Z == "Zone 1"])
  freq2 <- sum(df$Freq[Z == "Zone 2"])
  freq3 <- sum(df$Freq[Z == "Zone 3"])

  zoneSummary <- data.frame(
    Zone = c("Zone 1", "Zone 2", "Zone 3", "Total"),
    `N. Sources` = c(n1, n2, n3, nSO),
    `% Sources` = round(c(n1, n2, n3, nSO) / nSO * 100, 1),
    `N. Articles` = c(freq1, freq2, freq3, N),
    `% Articles` = round(c(freq1, freq2, freq3, N) / N * 100, 1),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  ## 5. Bradford multiplier ----
  # k estimated as geometric mean of consecutive zone ratios
  if (n1 > 0 && n2 > 0 && n3 > 0) {
    k <- (n2 / n1 + n3 / n2) / 2
  } else {
    k <- NA
  }

  ## 6. Kolmogorov-Smirnov goodness-of-fit test ----
  # Comparing empirical vs theoretical cumulative proportions
  empirical_prop <- df$cumFreq / N
  theoretical_prop <- pmin(pmax(df$Theoretical / N, 0), 1)
  ks_result <- suppressWarnings(
    ks.test(empirical_prop, theoretical_prop, exact = FALSE)
  )

  stat <- list(
    a = a_coef,
    b = b_coef,
    R2 = R2,
    k = k,
    ks.stat = as.numeric(ks_result$statistic),
    ks.pvalue = ks_result$p.value
  )

  ## 7. Bradford bibliograph plot ----
  zone_colors <- c(
    "Zone 1" = "#2171B5",
    "Zone 2" = "#6BAED6",
    "Zone 3" = "#BDD7E7"
  )
  zone_fills <- c(
    "Zone 1" = "#2171B5",
    "Zone 2" = "#6BAED6",
    "Zone 3" = "#BDD7E7"
  )

  # Zone boundary x positions (log scale)
  xz1 <- df$logRank[zone1_end]
  xz2 <- df$logRank[zone2_end]
  xmax <- max(df$logRank)
  ymax <- max(df$cumFreq)

  # Zone rects
  zone_rects <- data.frame(
    xmin = c(0, xz1, xz2),
    xmax = c(xz1, xz2, xmax),
    ymin = 0,
    ymax = ymax,
    Zone = c("Zone 1", "Zone 2", "Zone 3"),
    stringsAsFactors = FALSE
  )

  # Zone labels
  zone_labels <- data.frame(
    x = c(xz1 / 2, (xz1 + xz2) / 2, (xz2 + xmax) / 2),
    y = ymax * 0.95,
    label = c(
      paste0("Core\n(", n1, " sources)"),
      paste0("Zone 2\n(", n2, " sources)"),
      paste0("Zone 3\n(", n3, " sources)")
    ),
    stringsAsFactors = FALSE
  )

  # Subtitle with fit statistics
  subtitle_text <- paste0(
    "C(r) = ",
    round(a_coef, 1),
    " + ",
    round(b_coef, 1),
    " * log(r)",
    "    |    R2 = ",
    round(R2, 4),
    "    |    Bradford multiplier k = ",
    round(k, 2),
    "    |    KS p-value = ",
    format.pval(ks_result$p.value, digits = 3)
  )

  g_shiny <- ggplot2::ggplot(df) +
    # Zone background shading
    ggplot2::geom_rect(
      data = zone_rects,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = Zone
      ),
      alpha = 0.15,
      inherit.aes = FALSE
    ) +
    # Zone boundary lines
    ggplot2::geom_vline(
      xintercept = xz1,
      linetype = "dashed",
      color = "#666666",
      linewidth = 0.4
    ) +
    ggplot2::geom_vline(
      xintercept = xz2,
      linetype = "dashed",
      color = "#666666",
      linewidth = 0.4
    ) +
    # Theoretical fitted line
    ggplot2::geom_line(
      ggplot2::aes(x = logRank, y = Theoretical, group = 1),
      linetype = "dashed",
      color = "#D6604D",
      linewidth = 0.8,
      alpha = 0.8
    ) +
    # Empirical curve
    ggplot2::geom_line(
      ggplot2::aes(
        x = logRank,
        y = cumFreq,
        group = 1,
        text = paste0(
          "Source: ",
          SO,
          "\nRank: ",
          Rank,
          "\nArticles: ",
          Freq,
          "\nCumulative: ",
          cumFreq,
          "\nZone: ",
          Zone
        )
      ),
      color = "#1a1a1a",
      linewidth = 0.6
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = logRank,
        y = cumFreq,
        text = paste0(
          "Source: ",
          SO,
          "\nRank: ",
          Rank,
          "\nArticles: ",
          Freq,
          "\nCumulative: ",
          cumFreq,
          "\nZone: ",
          Zone
        )
      ),
      color = "#1a1a1a",
      size = 0.8,
      alpha = 0.5
    ) +
    # Zone labels
    ggplot2::annotate(
      "text",
      x = zone_labels$x,
      y = zone_labels$y,
      label = zone_labels$label,
      fontface = "bold",
      size = 3.5,
      color = "#333333",
      alpha = 0.7,
      vjust = 1
    ) +
    ggplot2::scale_fill_manual(values = zone_fills, guide = "none") +
    ggplot2::labs(
      x = "Source log(Rank)",
      y = "Cumulative N. of Articles",
      title = "Bradford's Law",
      subtitle = subtitle_text
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      text = ggplot2::element_text(color = "#333333"),
      plot.title = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(
        size = 10,
        face = "bold",
        color = "#666666",
        hjust = 0.5
      ),
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
      axis.title.y = ggplot2::element_text(angle = 90, vjust = 1),
      axis.text = ggplot2::element_text(face = "bold", size = 11),
      axis.line = ggplot2::element_line(color = "#333333", linewidth = 0.4),
      panel.grid.major = ggplot2::element_line(
        color = "#EBEBEB",
        linewidth = 0.3
      ),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none"
    )

  # Version with logo for export
  x_logo <- c(xmax - 0.02 - (xmax - 0) * 0.10, xmax - 0.02)
  y_logo <- c(0, ymax * 0.08)
  data("logo", envir = environment())
  logo <- grid::rasterGrob(logo, interpolate = TRUE)

  g <- g_shiny +
    ggplot2::annotation_custom(
      logo,
      xmin = x_logo[1],
      xmax = x_logo[2],
      ymin = y_logo[1],
      ymax = y_logo[2]
    )

  ## 8. Clean table for output ----
  df_out <- df[, c("SO", "Rank", "Freq", "cumFreq", "Zone")]
  names(df_out) <- c("SO", "Rank", "Freq", "cumFreq", "Zone")

  results <- list(
    table = df_out,
    graph = g,
    graph_shiny = g_shiny,
    zoneSummary = zoneSummary,
    stat = stat
  )
  return(results)
}
