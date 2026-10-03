#' Robustness of a multiplex coupling to the number of neighbours
#'
#' Roots and themes are clusters of the nearest-neighbour graphs of the two
#' layers, and their number and borders change with \code{k}: fewer neighbours
#' give a sparser graph and finer clusters. \code{multiplexRobustness} repeats
#' the clustering of \code{\link{multiplexClusters}} at other values of
#' \code{k}, with all the other options unchanged, and reports how much of the
#' result persists: the conclusions to rely on are those that do.
#'
#' Only the neighbour graphs depend on \code{k}: the two layers, the documents
#' and the weights do not. The graphs at the other values of \code{k} are
#' rebuilt from the layers stored in \code{mc}, so neither the coupling nor the
#' null model is computed again; the roots and themes found are the same as
#' those of a new analysis with that \code{k}. Their names are not computed.
#'
#' A link persists at another \code{k} when most of its documents (those of the
#' root in the theme) fall in a single cell of the new roots x themes table that
#' is a link too. A root or a theme keeps its structure (branching,
#' convergence, consolidation, dispersed) when most of its documents fall in a
#' new root or theme with the same structure. \code{persistence} is the share of
#' the other values of \code{k} at which this happens: 1 for a link found at
#' every \code{k}.
#'
#' @param mc is an object of class \code{"biblioMultiplex"} with clusters,
#'   obtained by \code{\link{multiplexClusters}}.
#' @param k is a numeric vector. The other numbers of neighbours. Default is
#'   \code{c(5, 20)}; the \code{k} of \code{mc} is left out.
#' @param verbose is logical. If TRUE, a message for each value of \code{k}.
#'
#' @return the object \code{mc}, with a column \code{persistence} in
#'   \code{clusters$links}, \code{clusters$roots} and \code{clusters$themes},
#'   and an element \code{clusters$robustness}, a list with \code{k} (the values
#'   compared), \code{summary} (one row per \code{k}: roots, themes, links,
#'   branching roots, convergent themes, and the adjusted Rand index of roots
#'   and themes with those of \code{mc}) and \code{links} (one column per
#'   \code{k}: whether each link persists).
#'
#' @examples
#' \donttest{
#' data(management, package = "bibliometrixData")
#' mc <- multiplexClusters(multiplexCoupling(management, n = 300, n.perm = 19), openalex = FALSE)
#' mc <- multiplexRobustness(mc, k = c(5, 20))
#' mc$clusters$robustness$summary
#' mc$clusters$links
#' }
#'
#' @seealso \code{\link{multiplexClusters}}, \code{\link{multiplexPlot}}
#'
#' @export
multiplexRobustness <- function(mc, k = c(5, 20), verbose = TRUE) {
  if (!inherits(mc, "biblioMultiplex") || is.null(mc$clusters)) {
    stop("multiplexRobustness() needs the result of multiplexClusters()", call. = FALSE)
  }
  par <- function(p, x) p$values[p$params == x]
  k0 <- as.numeric(par(mc$params, "k"))
  similarity <- par(mc$params, "similarity")
  cp <- mc$clusters$params
  algorithm <- par(cp, "algorithm")
  resolution <- as.numeric(par(cp, "resolution"))
  n.runs <- as.integer(par(cp, "n.runs"))
  res.crit <- as.numeric(par(cp, "res.crit"))
  min.link <- as.numeric(par(cp, "min.link"))
  min.size <- as.numeric(par(cp, "min.size"))
  seed <- as.integer(par(cp, "seed"))
  k <- sort(unique(as.integer(round(k))))
  k <- k[k >= 1 & k != k0]
  if (!length(k)) stop("multiplexRobustness(): no value of k other than ", k0, call. = FALSE)
  rng <- saveRNG()
  on.exit(restoreRNG(rng), add = TRUE)

  cl <- mc$clusters
  mR0 <- cl$membership$root
  mT0 <- cl$membership$theme
  nodes <- mc$nodes$node
  alt <- lapply(k, function(kk) {
    if (isTRUE(verbose)) message("Roots and themes with k = ", kk)
    ER <- mpKnn(mc$X_R, similarity, kk)
    ET <- mpKnn(mc$X_T, similarity, kk)
    gR <- mpEdgeGraph(nodes, ER, ER$s)
    gT <- mpEdgeGraph(nodes, ET, ET$s)
    mR <- mpDropSmall(mpClusterLayer(gR, algorithm, resolution, seed, n.runs)$membership, min.size)
    mT <- mpDropSmall(mpClusterLayer(gT, algorithm, resolution, seed, n.runs)$membership, min.size)
    list(mR = mR, mT = mT, lt = mpLinkTable(mR, mT, res.crit, min.link))
  })

  # the cell, root or theme that holds most of a set of documents
  majority <- function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) NA else names(which.max(table(x)))
  }
  link_persists <- function(a) {
    key <- paste(a$lt$links$root, a$lt$links$theme)
    vapply(seq_len(nrow(cl$links)), function(i) {
      d <- which(mR0 %in% cl$links$root[i] & mT0 %in% cl$links$theme[i])
      cell <- majority(ifelse(is.na(a$mR[d]) | is.na(a$mT[d]), NA, paste(a$mR[d], a$mT[d])))
      !is.na(cell) && cell %in% key
    }, logical(1))
  }
  structure_persists <- function(m0, structure0, m, structure) {
    vapply(seq_along(structure0), function(c0) {
      to <- majority(m[m0 %in% c0])
      !is.na(to) && identical(structure[as.integer(to)], structure0[c0])
    }, logical(1))
  }
  ari <- function(a, b) {
    ok <- !is.na(a) & !is.na(b)
    if (sum(ok) < 2) NA_real_ else igraph::compare(a[ok], b[ok], method = "adjusted.rand")
  }

  L <- vapply(alt, link_persists, logical(nrow(cl$links)))
  L <- matrix(L, nrow = nrow(cl$links), dimnames = list(NULL, paste0("k", k)))
  R <- vapply(alt, function(a) structure_persists(mR0, cl$roots$structure, a$mR, a$lt$structure_roots),
              logical(nrow(cl$roots)))
  Th <- vapply(alt, function(a) structure_persists(mT0, cl$themes$structure, a$mT, a$lt$structure_themes),
               logical(nrow(cl$themes)))
  R <- matrix(R, nrow = nrow(cl$roots))
  Th <- matrix(Th, nrow = nrow(cl$themes))
  cl$links$persistence <- rowMeans(L)
  cl$roots$persistence <- rowMeans(R)
  cl$themes$persistence <- rowMeans(Th)

  summary <- data.frame(
    k = c(k0, k),
    roots = c(nrow(cl$roots), vapply(alt, function(a) length(a$lt$structure_roots), 1L)),
    themes = c(nrow(cl$themes), vapply(alt, function(a) length(a$lt$structure_themes), 1L)),
    links = c(nrow(cl$links), vapply(alt, function(a) nrow(a$lt$links), 1L)),
    branching = c(sum(cl$roots$structure == "branching"),
                  vapply(alt, function(a) sum(a$lt$structure_roots == "branching"), 1L)),
    convergent = c(sum(cl$themes$structure == "convergence"),
                   vapply(alt, function(a) sum(a$lt$structure_themes == "convergence"), 1L)),
    ARI_roots = c(1, vapply(alt, function(a) ari(mR0, a$mR), 1)),
    ARI_themes = c(1, vapply(alt, function(a) ari(mT0, a$mT), 1)),
    links_persisting = c(1, colMeans(L))
  )
  cl$robustness <- list(k = k, summary = summary, links = L)
  mc$clusters <- cl
  mc
}

printMultiplexRobustness <- function(cl) {
  rb <- cl$robustness
  if (is.null(rb)) return(invisible())
  cat(sprintf("Robustness to k (%s): %d of %d links persist at every k; branching roots %d of %d, convergent themes %d of %d\n",
              paste(rb$k, collapse = ", "),
              sum(cl$links$persistence == 1), nrow(cl$links),
              sum(cl$roots$persistence == 1 & cl$roots$structure == "branching"),
              sum(cl$roots$structure == "branching"),
              sum(cl$themes$persistence == 1 & cl$themes$structure == "convergence"),
              sum(cl$themes$structure == "convergence")))
}
