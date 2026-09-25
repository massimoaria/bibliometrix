#' Authors' dominance ranking
#'
#' It calculates the authors' dominance ranking from an object of the class '\code{bibliometrix}' as proposed by Kumar & Kumar, 2008.
#' @param results is an object of the class '\code{bibliometrix}' for which the analysis of the authors' dominance ranking is desired.
#' @param k is an integer, used for table formatting (number of authors). Default value is 10.
#' @return The function \code{dominance} returns a data frame with cases corresponding to the first \code{k} most productive authors and variables to typical field of a dominance analysis.
#'
#' the data frame variables are:
#' \tabular{lll}{
#' \code{Author} \tab   \tab Author's name\cr
#' \code{Dominance Factor}  \tab   \tab Dominance Factor (DF = FAA / MAA)\cr
#' \code{Tot Articles}   \tab   \tab N. of Authored Articles (TAA)\cr
#' \code{Single Authored}   \tab   \tab N. of Single-Authored Articles (SAA)\cr
#' \code{Multi Authored}   \tab   \tab N. of Multi-Authored Articles (MAA=TAA-SAA)\cr
#' \code{First Authored} \tab   \tab N. of First Authored Articles (FAA)\cr
#' \code{Rank by Articles}    \tab   \tab Author Ranking by N. of Articles\cr
#' \code{Rank by DF}    \tab   \tab Author Ranking by Dominance Factor}
#'
#'
#'
#' @examples
#' data(scientometrics, package = "bibliometrixData")
#' results <- biblioAnalysis(scientometrics)
#' DF <- dominance(results)
#' DF
#'
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis
#' @seealso \code{\link{summary}} method for class '\code{bibliometrix}'
#'
#' @export

dominance <- function(results, k = 10) {
  # Author Rank by Dominance Rank  (Kumar & Kumar, 2008)

  # options(warn=-1)

  if (!inherits(results, "bibliometrix")) {
    cat('\n argument "results" have to be an object of class "bibliometrix"\n')
    return(NA)
  }

  empty_df <- data.frame(
    "Author" = character(0),
    "Dominance Factor" = numeric(0),
    "Tot Articles" = numeric(0),
    "Single-Authored" = numeric(0),
    "Multi-Authored" = numeric(0),
    "First-Authored" = numeric(0),
    "Rank by Articles" = integer(0),
    "Rank by DF" = integer(0),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  if (is.null(results$Authors) || length(results$Authors) == 0 ||
      is.null(results$FirstAuthors) || is.null(results$nAUperPaper)) {
    return(empty_df)
  }

  AU <- names(results$Authors)
  Tot <- as.numeric(results$Authors)

  single_table <- table(results$FirstAuthors[results$nAUperPaper == 1])
  Single <- as.numeric(single_table[AU])
  Single[is.na(Single)] <- 0

  Multi <- Tot - Single

  valid <- Multi > 0
  AU <- AU[valid]
  Tot <- Tot[valid]
  Single <- Single[valid]
  Multi <- Multi[valid]

  if (length(AU) == 0) {
    return(empty_df)
  }

  first_table <- table(results$FirstAuthors[results$nAUperPaper > 1])
  First <- as.numeric(first_table[AU])
  First[is.na(First)] <- 0

  Dominance <- First / Multi

  D <- data.frame(
    "Author" = AU,
    "Dominance Factor" = Dominance,
    "Articles" = Tot,
    "Single-Authored" = Single,
    "Multi-Authored" = Multi,
    "First-Author" = First,
    stringsAsFactors = FALSE
  )

  D <- D[order(-D$Articles), ]
  k_clamped <- max(0L, min(as.integer(k), nrow(D)))
  if (k_clamped > 0) {
    D <- D[1:k_clamped, , drop = FALSE]
  } else {
    return(empty_df)
  }

  D$RankbyArticles <- rank(-D$Articles, ties.method = "min")
  D <- D[order(-D$Dominance.Factor), ]
  D$RankDF <- rank(-D$Dominance.Factor, ties.method = "min")
  names(D) <- c("Author", "Dominance Factor", "Tot Articles", "Single-Authored", "Multi-Authored", "First-Authored", "Rank by Articles", "Rank by DF")
  row.names(D) <- 1:nrow(D)
  return(D)
}
