#' Roots and themes of a multiplex coupling
#'
#' It groups the documents of a \code{\link{multiplexCoupling}} into
#' \emph{roots} (clusters of the references layer: documents that cite the same
#' literature) and \emph{themes} (clusters of the topic layer: documents about
#' the same subject), and links them.
#'
#' A root is the intellectual base its documents share, so it is named after
#' the titles of its strong references (the references frequent among its
#' documents and concentrated in it), not after the keywords of its documents,
#' which name the themes. The titles come from the collection, when a reference is one of its
#' documents, from the reference string (Scopus writes the cited title) and from
#' OpenAlex, by DOI or OpenAlex id. OpenAlex is queried only when the user is
#' online and has configured both an OpenAlex API key and an email; the titles
#' are kept in a cache for the R session, so that a repeated analysis downloads
#' nothing. A root with fewer than five reference titles is named after its cited
#' sources and its most representative reference. \code{roots$label_source}
#' tells which, and \code{roots$doc_terms} keeps the keywords of its documents.
#'
#' A root and a theme are \emph{linked} when the theme holds more documents of
#' the root than expected if roots and themes were independent
#' (standardized residual above \code{res.crit}) and at least \code{min.link}
#' of them. The same links classify both sides:
#' \tabular{lll}{
#' \code{branching} root     \tab \tab linked to two or more themes\cr
#' \code{consolidation}      \tab \tab a root linked to one theme, or a theme linked to one root\cr
#' \code{convergence} theme  \tab \tab linked to two or more roots\cr
#' \code{dispersed}          \tab \tab no link}
#'
#' Each layer is clustered with consensus clustering (Lancichinetti & Fortunato,
#' 2012): \code{n.runs} runs of the algorithm are combined, because a single run
#' is not reproducible on the topic layer. \code{agreement} reports the mean
#' adjusted Rand index between the single runs: the lower it is, the weaker the
#' structure of the layer and the more cautiously its clusters should be read.
#'
#' A root or a theme of fewer than \code{min.size} documents is left out:
#' its documents share almost nothing with the rest of the collection, and a
#' cluster of two or three of them would be a root or a theme only in name.
#' Those documents are in no root (or in no theme): their membership is
#' \code{NA}, they are not counted in the roots x themes table, and
#' \code{unclustered} reports how many they are. They still count in the
#' baseline of the lifts (two random documents of the collection).
#'
#' @param mc is an object of class \code{"biblioMultiplex"} obtained by
#'   \code{\link{multiplexCoupling}}.
#' @param algorithm is a character. The clustering algorithm: \code{"louvain"}
#'   (default), \code{"leiden"} or \code{"walktrap"} (deterministic, no
#'   consensus).
#' @param resolution is a number. The resolution of Louvain and Leiden.
#'   Default is 1.
#' @param n.runs is an integer. The number of runs combined by the consensus.
#'   Default is 20.
#' @param res.crit is a number. The standardized residual above which a root
#'   and a theme can be linked. Default is 2.
#' @param min.link is an integer. The minimum number of documents of a link.
#'   Default is 5; on collections much larger than 2,000 documents a higher
#'   value may be preferable.
#' @param min.size is an integer. The minimum number of documents of a root
#'   or a theme. Default is 5.
#' @param n.labels is an integer. The number of terms and references that label
#'   a cluster. Default is 3.
#' @param n.refs is an integer. The number of strong references of a root whose
#'   titles name it. Default is 30.
#' @param email,api.key are characters. The email and the API key for OpenAlex. When
#'   \code{NULL} (default) they are read from the options and environment variables
#'   \code{openalexR.mailto} and \code{openalexR.apikey}, or from the files saved by
#'   Biblioshiny. OpenAlex is queried only when both are available and OpenAlex answers.
#' @param verbose is logical. If TRUE, messages on how the roots are named.
#' @param seed is an integer. The seed of the runs. The random number state of
#'   the session is restored on exit.
#'
#' @return the object \code{mc} with an element \code{clusters}, a list with:
#' \tabular{lll}{
#' \code{roots}, \code{themes} \tab \tab one row per cluster: size, labels, cohesion in the two layers, linked clusters, structure\cr
#' \code{links}       \tab \tab one row per root-theme link: documents, residual, share of the root and of the theme\cr
#' \code{contingency}, \code{residuals} \tab \tab the roots x themes table and its standardized residuals\cr
#' \code{membership}  \tab \tab root and theme of every document (\code{NA} when in a cluster of fewer than \code{min.size} documents)\cr
#' \code{unclustered} \tab \tab the number of documents in no root and in no theme\cr
#' \code{plane}       \tab \tab the pairs of roots, with their references and topic proximity\cr
#' \code{NMI}, \code{ARI} \tab \tab agreement between roots and themes\cr
#' \code{agreement}, \code{modularity} \tab \tab stability and modularity of the two partitions\cr
#' \code{consensus}   \tab \tab for each layer, the iterations of the consensus and whether all the runs agreed (if not after 10 iterations, the first run of the last one is kept)}
#'
#' @references
#' Lancichinetti, A., & Fortunato, S. (2012). Consensus clustering in complex
#' networks. \emph{Scientific Reports}, 2, 336.
#'
#' @examples
#' \donttest{
#' data(management, package = "bibliometrixData")
#' mc <- multiplexCoupling(management, n = 300, n.perm = 19)
#' mc <- multiplexClusters(mc)
#' mc$clusters$links
#' }
#'
#' @seealso \code{\link{multiplexCoupling}}, \code{\link{multiplexPlot}}
#'
#' @export
multiplexClusters <- function(mc,
                              algorithm = c("louvain", "leiden", "walktrap"),
                              resolution = 1,
                              n.runs = 20,
                              res.crit = 2,
                              min.link = 5,
                              min.size = 5,
                              n.labels = 3,
                              n.refs = 30,
                              email = NULL,
                              api.key = NULL,
                              seed = 1234,
                              verbose = TRUE) {
  if (!inherits(mc, "biblioMultiplex")) {
    stop("multiplexClusters() needs the result of multiplexCoupling()", call. = FALSE)
  }
  algorithm <- match.arg(algorithm)
  rng <- saveRNG()
  on.exit(restoreRNG(rng), add = TRUE)
  UR <- mpUnitRows(mc$X_R)
  UT <- mpUnitRows(mc$X_T)
  cR <- mpClusterLayer(mc$layers$references, algorithm, resolution, seed, n.runs)
  cT <- mpClusterLayer(mc$layers$topics, algorithm, resolution, seed, n.runs)
  # clusters smaller than min.size are left out: their documents (in no
  # root, or in no theme) share almost nothing with the others. Clusters are
  # numbered by decreasing size, so the ones kept stay 1, 2, ...
  mR <- mpDropSmall(cR$membership, min.size)
  mT <- mpDropSmall(cT$membership, min.size)
  if (all(is.na(mR)) || all(is.na(mT))) {
    stop("multiplexClusters(): no root or no theme has at least ", min.size,
         " documents; lower min.size", call. = FALSE)
  }

  summarise <- function(memb) {
    sR <- mpClusterSimilarity(UR, memb)
    sT <- mpClusterSimilarity(UT, memb)
    data.frame(
      cluster = seq_along(sR$size), size = sR$size,
      terms = mpClusterLabels(mc$X_T, memb, n.labels),
      references = mpClusterLabels(mc$X_R, memb, n.labels, mpShortRef),
      cohesion_R = sR$within / sR$global, cohesion_T = sT$within / sT$global,
      stringsAsFactors = FALSE
    )
  }
  roots <- summarise(mR)
  themes <- summarise(mT)
  # a root is an intellectual base: it is named after the titles of its
  # strong references; the keywords of its documents are kept as doc_terms
  roots_lab <- mpRootsLabels(mc, mR, n.labels, n.refs, email, api.key, verbose)
  roots$doc_terms <- roots$terms
  roots$terms <- roots_lab$terms
  roots$label_source <- roots_lab$label_source

  # one set of root-theme links, read by row for the roots and by column
  # for the themes: a cell over-represented (residual > res.crit) with at least
  # min.link documents
  tab <- table(roots = mR, themes = mT)
  ctab <- unclass(tab)
  res <- mpStandardizedResiduals(ctab)
  L <- res > res.crit & ctab >= min.link
  by_root <- lapply(seq_len(nrow(L)), function(k) which(L[k, ]))
  by_theme <- lapply(seq_len(ncol(L)), function(k) which(L[, k]))
  lk <- which(L, arr.ind = TRUE)
  links <- data.frame(
    root = unname(lk[, 1]), theme = unname(lk[, 2]), n = ctab[lk], residual = res[lk],
    share_root = (ctab / rowSums(ctab))[lk], share_theme = t(t(ctab) / colSums(ctab))[lk]
  )
  links <- links[order(links$root, -links$n), ]
  rownames(links) <- NULL
  roots$themes <- vapply(by_root, paste, "", collapse = ",")
  roots$n_themes <- lengths(by_root)
  roots$structure <- ifelse(lengths(by_root) >= 2, "branching",
                              ifelse(lengths(by_root) == 1, "consolidation", "dispersed"))
  themes$roots <- vapply(by_theme, paste, "", collapse = ",")
  themes$n_roots <- lengths(by_theme)
  themes$structure <- ifelse(lengths(by_theme) >= 2, "convergence",
                             ifelse(lengths(by_theme) == 1, "consolidation", "dispersed"))

  both <- !is.na(mR) & !is.na(mT)
  mc$clusters <- list(
    roots = roots, themes = themes, links = links,
    contingency = tab, residuals = res,
    membership = data.frame(node = mc$nodes$node, root = mR, theme = mT, stringsAsFactors = FALSE),
    plane = mpClusterPlane(UR, UT, mR, roots),
    NMI = igraph::compare(mR[both], mT[both], method = "nmi"),
    ARI = igraph::compare(mR[both], mT[both], method = "adjusted.rand"),
    unclustered = c(roots = sum(is.na(mR)), themes = sum(is.na(mT))),
    agreement = c(roots = cR$agreement, themes = cT$agreement),
    consensus = data.frame(
      layer = c("roots", "themes"), iterations = c(cR$iterations, cT$iterations),
      converged = c(cR$converged, cT$converged), stringsAsFactors = FALSE
    ),
    openalex = attr(roots_lab, "openalex"),
    modularity = c(roots = cR$modularity, themes = cT$modularity),
    params = data.frame(
      params = c("algorithm", "resolution", "n.runs", "res.crit", "min.link", "min.size", "seed"),
      values = c(algorithm, resolution, n.runs, res.crit, min.link, min.size, seed),
      stringsAsFactors = FALSE
    )
  )
  mc
}

printMultiplexClusters <- function(cl) {
  cat(sprintf("Roots  : %d (%d branching, %d consolidated, %d dispersed)\n", nrow(cl$roots),
              sum(cl$roots$structure == "branching"), sum(cl$roots$structure == "consolidation"),
              sum(cl$roots$structure == "dispersed")))
  cat(sprintf("Themes : %d (%d convergent, %d consolidated, %d dispersed)\n", nrow(cl$themes),
              sum(cl$themes$structure == "convergence"), sum(cl$themes$structure == "consolidation"),
              sum(cl$themes$structure == "dispersed")))
  cat(sprintf("Links  : %d root-theme links; roots vs themes NMI %.2f\n", nrow(cl$links), cl$NMI))
  if (!is.null(cl$unclustered) && any(cl$unclustered > 0)) {
    cat(sprintf("Left out (clusters of fewer than %s documents): %d documents in no root, %d in no theme\n",
                cl$params$values[cl$params$params == "min.size"], cl$unclustered[["roots"]],
                cl$unclustered[["themes"]]))
  }
  if (!is.null(cl$roots$label_source)) {
    oa <- grepl("OpenAlex", cl$roots$label_source)
    ti <- grepl("^titles", cl$roots$label_source)
    cat(sprintf("Root names from the titles of their strong references: %d roots (%d with OpenAlex), from their cited sources: %d\n",
                sum(ti), sum(oa), sum(!ti)))
    if (!is.null(cl$openalex) && nzchar(cl$openalex) && cl$openalex != "not needed") {
      cat("  OpenAlex not used:", cl$openalex, "\n")
    }
  }
  cat(sprintf("Agreement between single runs (ARI): roots %.2f, themes %.2f\n",
              cl$agreement[["roots"]], cl$agreement[["themes"]]))
  if (!is.null(cl$consensus) && !all(cl$consensus$converged)) {
    nc <- cl$consensus[!cl$consensus$converged, ]
    cat(sprintf("Consensus without unanimity after %d iterations (%s): the first run of the last iteration is kept\n",
                nc$iterations[1], paste(nc$layer, collapse = ", ")))
  }
}

## Internal helpers ----

# rows of unit length, so that dot products are cosines
mpUnitRows <- function(X) {
  d <- sqrt(mpSqNorms(X))
  d[d == 0] <- 1
  Matrix::Diagonal(x = 1 / d) %*% X
}

# Cluster-level cosines, exact from the cluster sums S_A of the unit rows:
#   mean similarity between A and B        = <S_A, S_B> / (|A| |B|)
#   mean similarity within A (pairs i != j) = (|S_A|^2 - |A|) / (|A| (|A| - 1))
# No pair is enumerated.
# Documents in no cluster (NA) count in the global baseline only.
mpClusterSimilarity <- function(U, memb) {
  Z <- mpMembershipMatrix(memb)
  K <- nrow(Z)
  G <- Z %*% U
  D <- as.matrix(Matrix::tcrossprod(G))
  sz <- tabulate(memb, K)
  has <- mpSqNorms(U) > 0
  nz <- as.numeric(Z %*% has)
  within <- ifelse(sz > 1, (diag(D) - nz) / (sz * (sz - 1)), NA_real_)
  between <- D / outer(sz, sz)
  diag(between) <- within
  n <- length(memb)
  S <- Matrix::colSums(U)
  list(within = within, between = between, global = (sum(S^2) - sum(has)) / (n * (n - 1)), size = sz)
}

# documents x clusters indicator, clusters by row; a document in no cluster
# (NA) has an empty column
mpMembershipMatrix <- function(memb) {
  ok <- which(!is.na(memb))
  Matrix::sparseMatrix(i = memb[ok], j = ok, x = 1, dims = c(max(memb, na.rm = TRUE), length(memb)))
}

# Consensus clustering of a layer graph: every edge is weighted by the share of
# the n.runs runs that put its ends together, edges below one half are dropped,
# and the graph is clustered again, until all the runs agree
mpClusterLayer <- function(g, algorithm = "louvain", resolution = 1, seed = 1234, n.runs = 20,
                           max.iter = 10) {
  run <- function(h, s) {
    set.seed(s)
    w <- igraph::E(h)$weight
    m <- switch(algorithm,
      louvain = igraph::cluster_louvain(h, weights = w, resolution = resolution),
      leiden = igraph::cluster_leiden(h, weights = w, objective_function = "modularity",
                                      resolution = resolution, n_iterations = 3),
      walktrap = igraph::cluster_walktrap(h, weights = w)
    )
    as.integer(igraph::membership(m))
  }
  if (algorithm == "walktrap") {
    memb <- run(g, seed)
    agreement <- 1
    iterations <- 1L
    converged <- TRUE
  } else {
    seeds <- seed + seq_len(n.runs) - 1
    h <- g
    agreement <- NA_real_
    converged <- FALSE
    for (iter in seq_len(max.iter)) {
      runs <- lapply(seeds, run, h = h)
      if (iter == 1) agreement <- mpMeanARI(runs)
      e <- igraph::as_edgelist(h, names = FALSE)
      together <- Reduce(`+`, lapply(runs, function(m) m[e[, 1]] == m[e[, 2]])) / length(runs)
      if (all(together %in% c(0, 1))) {
        converged <- TRUE
        break
      }
      keep <- together >= 0.5
      h <- igraph::subgraph_from_edges(h, which(keep), delete.vertices = FALSE)
      igraph::E(h)$weight <- together[keep]
    }
    # without unanimity after max.iter iterations, the first run of the last
    # one; the iterations and whether the runs agreed are reported
    iterations <- iter
    memb <- runs[[1]]
  }
  # clusters numbered by decreasing size
  relabel <- match(memb, as.integer(names(sort(table(memb), decreasing = TRUE))))
  list(membership = relabel, agreement = agreement, iterations = iterations, converged = converged,
       modularity = igraph::modularity(g, relabel, weights = igraph::E(g)$weight))
}

# clusters of fewer than min.size documents become NA (in no cluster)
mpDropSmall <- function(memb, min.size) {
  sz <- tabulate(memb)
  memb[sz[memb] < min.size] <- NA_integer_
  memb
}

# mean adjusted Rand index over the pairs of partitions
mpMeanARI <- function(runs) {
  if (length(runs) < 2) return(1)
  ij <- utils::combn(length(runs), 2)
  mean(apply(ij, 2, function(p) igraph::compare(runs[[p[1]]], runs[[p[2]]], method = "adjusted.rand")))
}

# the features that characterise each cluster: frequent in the cluster and
# concentrated in it (in-cluster count x share of the feature's documents in it)
mpClusterLabels <- function(K, memb, n.labels = 3, short = identity) {
  B <- K > 0
  Z <- mpMembershipMatrix(memb)
  inC <- as.matrix(Z %*% B)
  tot <- Matrix::colSums(B)
  vapply(seq_len(nrow(inC)), function(k) {
    score <- inC[k, ] * inC[k, ] / pmax(tot, 1)
    top <- order(-score)[seq_len(min(n.labels, sum(score > 0)))]
    paste(tolower(short(colnames(B)[top])), collapse = "; ")
  }, "")
}

# a reference as author, year and source (cut at 45 characters): author and
# year alone made two papers of the same author and year look the same
mpShortRef <- function(x) {
  ifelse(nchar(x) > 45, paste0(substr(x, 1, 45), "..."), x)
}

mpStandardizedResiduals <- function(tab) {
  n <- sum(tab)
  r <- rowSums(tab)
  c <- colSums(tab)
  E <- outer(r, c) / n
  (tab - E) / sqrt(E * outer(1 - r / n, 1 - c / n))
}

# pairs of roots: mean references and topic similarity between the two roots,
# relative to two random documents (lift 1 = as close as two random documents)
mpClusterPlane <- function(UR, UT, memb, roots) {
  sR <- mpClusterSimilarity(UR, memb)
  sT <- mpClusterSimilarity(UT, memb)
  if (length(sR$size) < 2) return(data.frame())
  ij <- which(upper.tri(sR$between), arr.ind = TRUE)
  P <- data.frame(
    A = ij[, 1], B = ij[, 2], size_A = sR$size[ij[, 1]], size_B = sR$size[ij[, 2]],
    lift_R = sR$between[ij] / sR$global, lift_T = sT$between[ij] / sT$global
  )
  P$quadrant <- factor(
    ifelse(P$lift_R > 1 & P$lift_T > 1, "consolidation",
      ifelse(P$lift_R > 1, "branching", ifelse(P$lift_T > 1, "convergence", "detachment"))
    ),
    levels = c("consolidation", "branching", "convergence", "detachment")
  )
  P$label_A <- roots$terms[P$A]
  P$label_B <- roots$terms[P$B]
  P[order(P$A, P$B), ]
}
