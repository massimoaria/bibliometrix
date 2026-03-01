utils::globalVariables(c(
  "AU",
  "listAU",
  "logo",
  "N.Articles",
  "N.Authors",
  "Theoretical",
  "Empirical",
  "Fitted"
))
#' Lotka's law coefficient estimation
#'
#' It estimates Lotka's law coefficients for scientific productivity and tests the goodness of fit.
#'
#' Lotka's Law, first formulated by Alfred J. Lotka in 1926, describes the frequency distribution
#' of scientific productivity among authors. The law states that the number of authors producing
#' \eqn{n} publications is approximately \eqn{C / n^\beta}, where \eqn{C} is a constant and
#' \eqn{\beta} is the productivity exponent.\cr\cr
#'
#' In the original formulation, Lotka proposed that \eqn{\beta = 2}, meaning that the number of
#' authors who publish \eqn{n} papers is approximately \eqn{1/n^2} of those who publish one paper.
#' The function estimates both the empirical \eqn{\beta} via regression and tests the fit of
#' the theoretical distribution (\eqn{\beta = 2}) using a Kolmogorov-Smirnov test.\cr\cr
#'
#' Reference:\cr
#' Lotka, A. J. (1926). The frequency distribution of scientific productivity. Journal of the
#' Washington Academy of Sciences, 16(12), 317-323.\cr
#'
#' @param M is an object of the class '\code{bibliometrixDB}'.
#' @return The function \code{lotka} returns a list containing the following objects:
#' \tabular{lll}{
#' \code{AuthorProd}    \tab   \tab Authors' Productivity frequency table\cr
#' \code{g}             \tab   \tab Lotka's law plot in ggplot2 format (with logo)\cr
#' \code{g_shiny}       \tab   \tab Lotka's law plot for biblioshiny (without logo)\cr
#' \code{stat}          \tab   \tab list of statistical results (Beta, C, R2, KS tests)\cr
#' \code{Beta}          \tab   \tab Beta coefficient (estimated)\cr
#' \code{C}             \tab   \tab Constant coefficient\cr
#' \code{R2}            \tab   \tab Goodness of Fit (R-squared)\cr
#' \code{fitted}        \tab   \tab Fitted Values\cr
#' \code{p.value}       \tab   \tab p-value of KS test (theoretical Beta=2)}
#'
#' @examples
#' data(management, package = "bibliometrixData")
#' L <- lotka(management)
#' L
#'
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis
#' @seealso \code{\link{summary}} method for class '\code{bibliometrix}'
#'
#' @export

lotka <- function(M) {
  if (!inherits(M, "bibliometrixDB")) {
    cat('\n argument M have to be an object of class "bibliometrixDB"\n')
    return(NA)
  }

  ## 1. Author Productivity table ----
  AUdf <- M %>%
    mutate(
      listAU = stringr::str_split(as.character(AU), ";"),
      listAU = purrr::map(listAU, stringr::str_trim)
    ) %>%
    select(listAU) %>%
    unnest(cols = listAU) %>%
    count(listAU, sort = TRUE, name = "Freq") %>%
    rename(AU = listAU)

  AuthorProd <- aggregate(AUdf, by = list(AUdf$Freq), "length")
  AuthorProd[, 2] <- as.numeric(AuthorProd[, 2])
  AuthorProd[, 3] <- AuthorProd[, 2] / sum(AuthorProd[, 2])
  names(AuthorProd) <- c("N.Articles", "N.Authors", "Freq")

  nAuthors <- sum(AuthorProd$N.Authors)

  ## 2. Fit Lotka's Law: log10(Freq) ~ log10(N.Articles) ----
  LOTKA <- lm(log10(Freq) ~ log10(N.Articles), data = AuthorProd)
  Beta <- abs(as.numeric(coef(LOTKA)[2]))
  C <- 10^(as.numeric(coef(LOTKA)[1]))
  R2 <- summary(LOTKA)$r.squared
  fitted_vals <- 10^LOTKA$fitted.values

  ## 3. Theoretical distribution (Beta = 2) ----
  Yt <- coef(LOTKA)[1] - 2 * log10(AuthorProd[, 1])
  Theoretical_beta2 <- 10^Yt
  Theoretical_beta2 <- Theoretical_beta2 / sum(Theoretical_beta2)

  ## 4. Fitted distribution (empirical Beta) ----
  Fitted_betaEmp <- 10^(log10(C) - Beta * log10(AuthorProd[, 1]))
  Fitted_betaEmp <- Fitted_betaEmp / sum(Fitted_betaEmp)

  ## 5. KS goodness-of-fit tests ----
  # Test 1: Empirical vs theoretical (Beta = 2)
  ks_theoretical <- suppressWarnings(
    ks.test(AuthorProd[, 3], Theoretical_beta2, exact = FALSE)
  )

  # Test 2: Empirical vs fitted (empirical Beta)
  ks_fitted <- suppressWarnings(
    ks.test(AuthorProd[, 3], Fitted_betaEmp, exact = FALSE)
  )

  stat <- list(
    Beta = Beta,
    C = C,
    R2 = R2,
    ks.theo.stat = as.numeric(ks_theoretical$statistic),
    ks.theo.pvalue = ks_theoretical$p.value,
    ks.fit.stat = as.numeric(ks_fitted$statistic),
    ks.fit.pvalue = ks_fitted$p.value
  )

  ## 6. Prepare plot data ----
  AuthorProd$Theoretical <- Theoretical_beta2
  AuthorProd$Fitted <- Fitted_betaEmp

  # Subtitle with statistics
  subtitle_text <- paste0(
    "Estimated: Beta = ",
    round(Beta, 2),
    ", C = ",
    round(C, 4),
    "    |    R2 = ",
    round(R2, 4),
    "    |    KS p-value (Beta=2): ",
    format.pval(ks_theoretical$p.value, digits = 3),
    "    |    KS p-value (fitted): ",
    format.pval(ks_fitted$p.value, digits = 3)
  )

  ## 7. Plot ----
  g_shiny <- ggplot2::ggplot(AuthorProd, ggplot2::aes(x = N.Articles)) +
    # Empirical line
    ggplot2::geom_line(
      ggplot2::aes(
        y = Freq * 100,
        group = 1,
        text = paste0(
          "Documents written: ",
          N.Articles,
          "\nN. of Authors: ",
          N.Authors,
          "\n% of Authors: ",
          round(Freq * 100, 2)
        )
      ),
      color = "#1a1a1a",
      linewidth = 0.7
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        y = Freq * 100,
        text = paste0(
          "Documents written: ",
          N.Articles,
          "\nN. of Authors: ",
          N.Authors,
          "\n% of Authors: ",
          round(Freq * 100, 2)
        )
      ),
      color = "#1a1a1a",
      size = 2
    ) +
    # Theoretical line (Beta = 2)
    ggplot2::geom_line(
      ggplot2::aes(y = Theoretical * 100, group = 1),
      linetype = "dashed",
      color = "#D6604D",
      linewidth = 0.8,
      alpha = 0.8
    ) +
    # Fitted line (empirical Beta)
    ggplot2::geom_line(
      ggplot2::aes(y = Fitted * 100, group = 1),
      linetype = "dotdash",
      color = "#2171B5",
      linewidth = 0.8,
      alpha = 0.8
    ) +
    ggplot2::xlim(0, max(AuthorProd$N.Articles) + 1) +
    ggplot2::labs(
      x = "Documents Written",
      y = "% of Authors",
      title = "Lotka's Law",
      subtitle = subtitle_text
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
      axis.title.y = ggplot2::element_text(angle = 90, vjust = 1),
      axis.line = ggplot2::element_line(color = "#333333", linewidth = 0.4),
      panel.grid.major = ggplot2::element_line(
        color = "#EBEBEB",
        linewidth = 0.3
      ),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none"
    )

  # Version with logo for export
  x_logo <- c(
    max(AuthorProd$N.Articles) - diff(range(AuthorProd$N.Articles)) * 0.10,
    max(AuthorProd$N.Articles)
  ) +
    1
  y_logo <- c(
    min(AuthorProd$Freq * 100),
    min(AuthorProd$Freq * 100) + diff(range(AuthorProd$Freq * 100)) * 0.10
  )

  data("logo", package = "bibliometrix", envir = environment())
  logoGrid <- grid::rasterGrob(logo, interpolate = TRUE)

  g <- g_shiny +
    ggplot2::annotation_custom(
      logoGrid,
      xmin = x_logo[1],
      xmax = x_logo[2],
      ymin = y_logo[1],
      ymax = y_logo[2]
    )

  ## 8. Clean table for output ----
  AuthorProd_out <- AuthorProd[, c("N.Articles", "N.Authors", "Freq")]
  names(AuthorProd_out) <- c(
    "Documents written",
    "N. of Authors",
    "Proportion of Authors"
  )

  L <- list(
    AuthorProd = AuthorProd_out,
    g = g,
    g_shiny = g_shiny,
    stat = stat,
    Beta = Beta,
    C = C,
    R2 = R2,
    fitted = fitted_vals,
    p.value = ks_theoretical$p.value
  )
  return(L)
}
