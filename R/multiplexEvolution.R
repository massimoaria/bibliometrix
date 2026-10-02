#' Evolution of the roots of a multiplex coupling
#'
#' It follows how close the roots of \code{\link{multiplexClusters}} are, in
#' references and in topics, period by period. Documents belong to one period
#' only, so the clusters of different periods cannot be matched through their
#' members: the roots are defined once, on the whole collection, and in each
#' period the proximity of two roots is measured on their documents of that
#' period, relative to two random documents of the same period (lift).
#'
#' The slopes of the two proximities over the periods classify the trajectory
#' of every pair of roots seen in at least two periods:
#' \tabular{lll}{
#' \code{converging}     \tab \tab topics closer, references not\cr
#' \code{diverging}      \tab \tab topics farther, references not\cr
#' \code{consolidating}  \tab \tab both closer\cr
#' \code{drifting apart} \tab \tab both farther\cr
#' \code{closer in references}  \tab \tab references closer, topics stable\cr
#' \code{farther in references} \tab \tab references farther, topics stable\cr
#' \code{stable}         \tab \tab neither slope beyond \code{slope.min}}
#'
#' @param mc is an object of class \code{"biblioMultiplex"} with clusters,
#'   obtained by \code{\link{multiplexClusters}}.
#' @param years is a numeric vector of cut points: the periods end at these
#'   years, e.g. \code{c(2010, 2016)} gives three periods.
#' @param width,step are integers. Alternatively to \code{years}, sliding
#'   windows of \code{width} years every \code{step} years.
#' @param min.docs is an integer. A root is followed in a period when it has
#'   at least \code{min.docs} documents in it. Default is 5.
#' @param slope.min is a number. The slope of the log2 lift per period above
#'   which a trend is called. Default is 0.1.
#'
#' @return an object of class \code{"biblioMultiplexEvolution"}, a list with
#'   \code{long} (one row per pair of roots and period), \code{trajectories}
#'   (one row per pair, with slopes and trend), \code{periods}, \code{roots}
#'   and \code{params}.
#'
#' @examples
#' \donttest{
#' data(management, package = "bibliometrixData")
#' mc <- multiplexClusters(multiplexCoupling(management, n = 500, n.perm = 19))
#' ev <- multiplexEvolution(mc, years = c(2012, 2016))
#' ev
#' }
#'
#' @seealso \code{\link{multiplexClusters}}, \code{\link{multiplexPlot}}
#'
#' @export
multiplexEvolution <- function(mc, years = NULL, width = NULL, step = NULL, min.docs = 5,
                               slope.min = 0.1) {
  if (!inherits(mc, "biblioMultiplex") || is.null(mc$clusters)) {
    stop("multiplexEvolution() needs the result of multiplexClusters()", call. = FALSE)
  }
  cl <- mc$clusters
  memb <- cl$membership$root
  UR <- mpUnitRows(mc$X_R)
  UT <- mpUnitRows(mc$X_T)
  per <- mpPeriods(mc$nodes$PY, years, width, step)

  long <- do.call(rbind, lapply(seq_along(per), function(k) {
    idx <- per[[k]]$idx
    if (length(idx) < 2 * min.docs) return(NULL)
    # documents in no root (NA) are not counted
    tab <- tabulate(memb[idx], max(memb, na.rm = TRUE))
    ok <- which(tab >= min.docs)
    if (length(ok) < 2) return(NULL)
    sub <- idx[memb[idx] %in% ok]
    m <- match(memb[sub], ok)
    # lifts relative to all the documents of the period
    gR <- mpClusterSimilarity(UR[idx, , drop = FALSE], rep(1L, length(idx)))$global
    gT <- mpClusterSimilarity(UT[idx, , drop = FALSE], rep(1L, length(idx)))$global
    sR <- mpClusterSimilarity(UR[sub, , drop = FALSE], m)
    sT <- mpClusterSimilarity(UT[sub, , drop = FALSE], m)
    ij <- which(upper.tri(sR$between), arr.ind = TRUE)
    data.frame(
      period = k, label = per[[k]]$label, A = ok[ij[, 1]], B = ok[ij[, 2]],
      n_A = tab[ok[ij[, 1]]], n_B = tab[ok[ij[, 2]]],
      lift_R = sR$between[ij] / gR, lift_T = sT$between[ij] / gT,
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(long) || !nrow(long)) {
    stop("multiplexEvolution(): no period has two roots with at least ", min.docs,
         " documents", call. = FALSE)
  }

  # trajectory of the pairs seen in at least two periods: slope of the log2
  # lift per period, for references and topics
  floor <- 1 / 16
  long$x <- log2(pmax(long$lift_R, floor))
  long$y <- log2(pmax(long$lift_T, floor))
  traj <- do.call(rbind, lapply(split(long, paste(long$A, long$B)), function(d) {
    if (nrow(d) < 2) return(NULL)
    d <- d[order(d$period), ]
    slope <- function(v) unname(stats::coef(stats::lm(v ~ d$period))[2])
    data.frame(A = d$A[1], B = d$B[1], periods = nrow(d), first = d$label[1], last = d$label[nrow(d)],
               x_first = d$x[1], y_first = d$y[1], x_last = d$x[nrow(d)], y_last = d$y[nrow(d)],
               slope_R = slope(d$x), slope_T = slope(d$y), stringsAsFactors = FALSE)
  }))
  if (is.null(traj)) {
    stop("multiplexEvolution(): no pair of roots is seen in two periods", call. = FALSE)
  }
  traj$trend <- ifelse(
    traj$slope_T >= slope.min & traj$slope_R < slope.min, "converging",
    ifelse(traj$slope_T <= -slope.min & traj$slope_R > -slope.min, "diverging",
      ifelse(traj$slope_T <= -slope.min & traj$slope_R <= -slope.min, "drifting apart",
        ifelse(traj$slope_T >= slope.min & traj$slope_R >= slope.min, "consolidating",
          # topics stable: the trend, if any, is in the references only
          ifelse(traj$slope_R >= slope.min, "closer in references",
            ifelse(traj$slope_R <= -slope.min, "farther in references", "stable")))))
  )
  traj$label_A <- cl$roots$terms[traj$A]
  traj$label_B <- cl$roots$terms[traj$B]
  traj <- traj[order(-abs(traj$slope_T)), ]
  rownames(traj) <- NULL
  structure(
    list(long = long, trajectories = traj, periods = vapply(per, `[[`, "", "label"),
         roots = cl$roots,
         params = list(years = years, width = width, step = step, min.docs = min.docs,
                       slope.min = slope.min)),
    class = "biblioMultiplexEvolution"
  )
}

#' @method print biblioMultiplexEvolution
#' @export
print.biblioMultiplexEvolution <- function(x, ...) {
  cat("Multiplex evolution of", nrow(x$roots), "roots over", length(x$periods), "periods:",
      paste(x$periods, collapse = ", "), "\n")
  cat("Pairs of roots followed in at least two periods:", nrow(x$trajectories), "\n\n")
  print(table(x$trajectories$trend))
  cat("\nStrongest topic trends:\n")
  tr <- utils::head(x$trajectories, 8)
  print(data.frame(pair = paste0(mpFirstLabel(tr$label_A), " / ", mpFirstLabel(tr$label_B)),
                   slope_R = round(tr$slope_R, 2), slope_T = round(tr$slope_T, 2), trend = tr$trend),
        row.names = FALSE)
  invisible(x)
}

# periods from cut points (years) or sliding windows (width, step)
mpPeriods <- function(PY, years = NULL, width = NULL, step = NULL) {
  PY <- as.numeric(PY)
  if (all(is.na(PY))) stop("multiplexEvolution(): the documents have no publication year", call. = FALSE)
  if (!is.null(width)) {
    step <- step %||% width
    starts <- seq(min(PY, na.rm = TRUE), max(PY, na.rm = TRUE) - width + 1, by = step)
    lapply(starts, function(s) {
      list(label = paste0(s, "-", s + width - 1), idx = which(PY >= s & PY <= s + width - 1))
    })
  } else {
    if (is.null(years)) {
      stop("multiplexEvolution(): give the cut points (years) or a window (width, step)", call. = FALSE)
    }
    br <- c(min(PY, na.rm = TRUE) - 1, sort(years), max(PY, na.rm = TRUE))
    lapply(seq_len(length(br) - 1), function(k) {
      list(label = paste0(br[k] + 1, "-", br[k + 1]), idx = which(PY > br[k] & PY <= br[k + 1]))
    })
  }
}
