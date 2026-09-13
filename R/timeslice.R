#' Bibliographic data frame time slice
#'
#' Divide a bibliographic data frame into time slice
#'
#'
#'
#' @param M is a bibliographic data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to manuscripts and variables to Field Tag in the original SCOPUS and Clarivate Analytics WoS file.
#' @param breaks is a numeric vector of two or more unique cut points.
#' @param k is an integer value giving the number of intervals into which the data frame is to be cut. \code{k} is used only in case \code{breaks} argument is not provided. The default is \code{k = 5}.
#' @return the value returned from \code{split} is a list containing the data frames for each sub-period.
#'
#'
#'
#' @examples
#'
#' data(scientometrics, package = "bibliometrixData")
#'
#' list_df <- timeslice(scientometrics, breaks = c(1995, 2005))
#'
#' names(list_df)
#'
#' @seealso \code{\link{convert2df}} to import and convert an ISI or SCOPUS Export file in a bibliographic data frame.
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis.
#' @seealso \code{\link{summary}} to obtain a summary of the results.
#' @seealso \code{\link{plot}} to draw some useful plots of the results.
#'
#' @export
timeslice <- function(M, breaks = NA, k = 5) {
  M$PY <- as.numeric(M$PY)
  period <- list()
  # "breaks not provided" has to cover NULL as well as the NA default, which
  # is how a caller holding no cut points naturally expresses it: NULL[1] is
  # NULL, so is.na() on it is logical(0) and this test stopped with "argument
  # is of length zero" instead of falling back on k. A zero-length numeric
  # already fell back, because numeric(0)[1] is NA.
  if ((length(breaks) == 0 || is.na(breaks[1])) && is.numeric(k)) {
    breaks <- (floor(seq(min(M$PY, na.rm = TRUE) - 1, max(M$PY, na.rm = TRUE), length.out = k + 1)))
  } else {
    breaks <- c(min(M$PY, na.rm = TRUE) - 1, breaks, max(M$PY, na.rm = TRUE))
    # cut() needs its bounds distinct. A cut point on the last year, or on the
    # year before the first, repeats one of the two added here, and cut() then
    # stopped with "'breaks' are not unique" - a complaint about a vector the
    # caller never wrote.
    repeated <- unique(breaks[duplicated(breaks)])
    if (length(repeated) > 0) {
      stop(
        "timeslice(): ",
        paste(repeated, collapse = ", "),
        if (length(repeated) > 1) " are repeated bounds" else " is a repeated bound",
        ". Cut points have to be distinct and to fall inside ",
        min(M$PY, na.rm = TRUE),
        "-",
        max(M$PY, na.rm = TRUE),
        ", the years this collection covers.",
        call. = FALSE
      )
    }
  }
  # Split on the factor, not on as.numeric() of it. A period holding no
  # document -- a gap in the publication years, or a cut point outside them --
  # is a level of the factor but not a group of the split, so the names were
  # one or more longer than the list they were assigned to and the function
  # stopped with "'names' attribute [3] must be the same length as the vector
  # [2]". Splitting on the factor keeps every level, empty ones included, and
  # names them itself.
  df <- split(M, cut(M$PY, breaks))
  return(df)
}
