#' Multiplex coupling of references and topics
#'
#' It compares two kinds of closeness between the documents of a collection:
#' sharing cited references (the \emph{references} layer, bibliographic coupling) and
#' sharing content (the \emph{topic} layer, keyword or text similarity). The two
#' layers are built on the same documents, sparsified to the \code{k} nearest
#' neighbours of each document, and compared pair by pair: pairs with common
#' references and similar topics (consolidation), common references and
#' different topics (branching), different references and similar topics
#' (convergence), neither (detachment).
#'
#' Statistics on the relation between the layers (\code{info$association},
#' the alignment index \code{LA}) are computed on all pairs, through a uniform
#' sample of pairs or full rows, not on the neighbour pairs only: a pair is a
#' neighbour pair because one of its similarities is high, and this selection
#' alone induces a negative correlation between the two.
#'
#' The result is the input of \code{\link{multiplexClusters}}, which groups the
#' documents into roots (clusters of the references layer) and themes (clusters
#' of the topic layer) and links them, and of
#' \code{\link{multiplexPlot}}.
#'
#' @section Reference matching (Scopus):
#' The references layer matches cited references as strings. Scopus writes the same
#' reference in different ways, so many shared references are missed. Run
#' \code{\link{applyReferenceMatching}} on a Scopus collection first: on two
#' test collections it doubled the shared references and brought the
#' association between the layers to the level of Web of Science collections.
#'
#' @param M is a bibliographic data frame obtained by \code{\link{convert2df}}.
#'   It needs the cited references (\code{CR}).
#' @param n is an integer. Only the \code{n} most cited documents (see
#'   \code{select.by}) are analysed. \code{NULL} keeps all of them. Default is
#'   \code{n = 1000}.
#' @param select.by is a character. How documents are ranked for \code{n}:
#'   \code{"TC"} (global citations, default) or \code{"LCS"} (local citations,
#'   counted on the whole collection with \code{\link{localCitations}}).
#' @param topic.field is a character. The fields of the topic layer, among
#'   \code{"DE"}, \code{"ID"}, \code{"TI"}, \code{"AB"} (merged by term), or
#'   \code{"auto"} (default): the keywords DE and ID when at least
#'   \code{kw.coverage} of the documents with references have one, titles and
#'   abstracts otherwise. The choice is printed and stored in \code{params}.
#' @param kw.coverage is a number. The keyword coverage required by
#'   \code{topic.field = "auto"}. Default is 0.8.
#' @param k is an integer. The number of nearest neighbours kept for each
#'   document in each layer. Default is \code{k = 10}.
#' @param similarity is a character. The similarity index: \code{"cosine"}
#'   (Salton, default), \code{"association"} or \code{"jaccard"}.
#' @param ref.weight is a character. \code{"none"} (default) or \code{"idf"}
#'   weighting of the cited references.
#' @param standardize is a character. How a pair is called close in a layer:
#'   \code{"nullmodel"} (default; its similarity is higher than in
#'   \code{n.perm} randomisations of the layer at level \code{alpha}),
#'   \code{"percentile"} (above the \code{tau} percentile of all pairs) or
#'   \code{"none"} (above the \code{tau} quantile of the neighbour pairs).
#' @param alpha,tau are numbers. The level of the null model test (default
#'   0.05) and the percentile threshold (default 0.9).
#' @param min.ref.freq,min.term.freq are integers. The minimum number of
#'   documents of a cited reference and of a term. Default is 2 for both: a
#'   feature of a single document links no pair.
#' @param ngrams,stemming,remove.terms,synonyms options of the topic layer when
#'   it is built from titles and abstracts (see \code{\link{termExtraction}}).
#' @param n.sample is an integer. The number of random pairs used for the
#'   association between the layers and the percentiles. Default is 100000.
#' @param n.perm is an integer. The number of randomisations of the null model
#'   (curveball algorithm, which keeps the number of references of every
#'   document and the number of citing documents of every reference). Default
#'   is 99.
#' @param seed is an integer. The seed of the randomisations. The random number
#'   state of the session is restored on exit.
#' @param verbose is logical. If TRUE, progress messages are printed.
#'
#' @return an object of class \code{"biblioMultiplex"}, a list with:
#' \tabular{lll}{
#' \code{pairs}  \tab   \tab the neighbour pairs, with their references and topic similarity (\code{s_R}, \code{s_T}), layer (\code{edge_type}) and typology (\code{quadrant})\cr
#' \code{nodes}  \tab   \tab the documents, with their node indices (\code{LA} alignment, \code{CI} convergence, \code{BI} branching, \code{P} participation)\cr
#' \code{layers} \tab   \tab the igraph graphs of the two layers and of their union\cr
#' \code{X_R}, \code{X_T} \tab \tab the document x reference and document x term matrices\cr
#' \code{info}   \tab   \tab excluded documents, association between the layers, timing\cr
#' \code{params} \tab   \tab the parameters, as a data frame}
#'
#' @references
#' Kessler, M. M. (1963). Bibliographic coupling between scientific papers.
#' \emph{American Documentation}, 14(1), 10-25.
#'
#' Battiston, F., Nicosia, V., & Latora, V. (2014). Structural measures for
#' multiplex networks. \emph{Physical Review E}, 89(3), 032804.
#'
#' Strona, G., Nappo, D., Boccacci, F., Fattorini, S., & San-Miguel-Ayanz, J.
#' (2014). A fast and unbiased procedure to randomize ecological binary matrices
#' with fixed row and column totals. \emph{Nature Communications}, 5, 4114.
#'
#' @examples
#' \donttest{
#' data(management, package = "bibliometrixData")
#' mc <- multiplexCoupling(management, n = 300, n.perm = 19)
#' mc
#' }
#'
#' @seealso \code{\link{multiplexClusters}}, \code{\link{multiplexEvolution}},
#'   \code{\link{multiplexPlot}}, \code{\link{couplingMap}}
#'
#' @export
multiplexCoupling <- function(M,
                              n = 1000,
                              select.by = c("TC", "LCS"),
                              topic.field = "auto",
                              kw.coverage = 0.8,
                              k = 10,
                              similarity = c("cosine", "association", "jaccard"),
                              ref.weight = c("none", "idf"),
                              standardize = c("nullmodel", "percentile", "none"),
                              alpha = 0.05,
                              tau = 0.9,
                              min.ref.freq = 2,
                              min.term.freq = 2,
                              ngrams = 1,
                              stemming = FALSE,
                              remove.terms = NULL,
                              synonyms = NULL,
                              n.sample = 1e5,
                              n.perm = 99,
                              seed = 1234,
                              verbose = TRUE) {
  select.by <- match.arg(select.by)
  similarity <- match.arg(similarity)
  ref.weight <- match.arg(ref.weight)
  standardize <- match.arg(standardize)
  rng <- saveRNG()
  on.exit(restoreRNG(rng), add = TRUE)
  say <- function(...) if (isTRUE(verbose)) message(...)
  t0 <- proc.time()[["elapsed"]]

  ## Documents ----
  M <- as.data.frame(M, stringsAsFactors = FALSE)
  if (!("CR" %in% names(M))) {
    stop("multiplexCoupling() needs the cited references: M has no CR field", call. = FALSE)
  }
  if (!("SR" %in% names(M))) M <- metaTagExtraction(M, Field = "SR")
  if (anyDuplicated(M$SR)) {
    stop("multiplexCoupling(): the document identifiers (SR) of M are not unique", call. = FALSE)
  }
  M_all <- M # the whole collection: titles of the cited references that are documents of it
  # local citations are counted on the whole collection, before any document is left out
  if (select.by == "LCS" && !is.null(n) && nrow(M) > n && !("LCS" %in% names(M))) {
    say("Counting local citations")
    H <- suppressMessages(localCitations(M))$M
    M$LCS <- H$LCS[match(M$SR, H$SR)]
  }
  nonEmpty <- function(x) !is.na(x) & nchar(trimws(x)) > 0
  has_refs <- nonEmpty(M$CR)
  # topic.field = "auto": the keywords when they cover at least kw.coverage of
  # the documents with references, titles and abstracts otherwise
  kw_cov <- NA_real_
  if (identical(topic.field, "auto")) {
    kw <- intersect(c("DE", "ID"), names(M))
    has_kw <- if (length(kw)) {
      Reduce(`|`, lapply(kw, function(f) nonEmpty(M[[f]])))
    } else {
      rep(FALSE, nrow(M))
    }
    kw_cov <- mean(has_kw[has_refs])
    topic.field <- if (length(kw) && isTRUE(kw_cov >= kw.coverage)) {
      kw
    } else {
      intersect(c("TI", "AB"), names(M))
    }
    say("Topic layer: ", paste(topic.field, collapse = "+"),
        sprintf(" (keywords cover %.0f%% of the documents with references)", 100 * kw_cov))
  }
  if (!length(topic.field)) {
    stop("multiplexCoupling(): M has no keywords (DE, ID) nor titles and abstracts (TI, AB)", call. = FALSE)
  }
  missing_fields <- setdiff(topic.field, names(M))
  if (length(missing_fields)) {
    stop("multiplexCoupling(): M has no field ", paste(missing_fields, collapse = ", "), call. = FALSE)
  }
  has_topic <- Reduce(`|`, lapply(topic.field, function(f) nonEmpty(M[[f]])))
  dropped <- c(total = nrow(M), no_references = sum(!has_refs), no_topic = sum(has_refs & !has_topic))
  M <- M[has_refs & has_topic, , drop = FALSE]
  if (nrow(M) < 3) {
    stop("multiplexCoupling(): fewer than 3 documents have both cited references and ",
         paste(topic.field, collapse = "/"), call. = FALSE)
  }
  score <- suppressWarnings(as.numeric(if (select.by == "LCS" && "LCS" %in% names(M)) M$LCS else M$TC))
  score[is.na(score)] <- 0
  if (!is.null(n) && nrow(M) > n) {
    M <- M[order(-score, M$SR)[seq_len(n)], , drop = FALSE]
  }

  ## Layers ----
  say("Building the references layer (CR) and the topic layer (", paste(topic.field, collapse = "+"),
      ") on ", nrow(M), " documents")
  buildLayers <- function(M) {
    list(
      R = mpRootsLayer(M, min.freq = min.ref.freq, weight = ref.weight),
      T = mpTopicLayer(M, topic.field, min.freq = min.term.freq, ngrams = ngrams,
                       stemming = stemming, remove.terms = remove.terms, synonyms = synonyms)
    )
  }
  L <- buildLayers(M)
  # documents that keep at least one reference and one term after the filters
  ok <- Matrix::rowSums(L$R$inc) > 0 & Matrix::rowSums(L$T$inc) > 0
  dropped["empty_after_filters"] <- sum(!ok)
  if (any(!ok)) {
    M <- M[ok, , drop = FALSE]
    L <- buildLayers(M)
  }
  if (nrow(M) < 3) {
    stop("multiplexCoupling(): fewer than 3 documents share a reference and a term with another document",
         call. = FALSE)
  }
  nodes <- data.frame(
    node = M$SR,
    TI = if ("TI" %in% names(M)) M$TI else NA_character_,
    PY = suppressWarnings(as.numeric(M$PY)),
    TC = suppressWarnings(as.numeric(M$TC)),
    stringsAsFactors = FALSE
  )
  XR <- mpLayerMatrix(L$R)
  XT <- mpLayerMatrix(L$T)
  refs <- mpReferenceIndex(M, M_all, colnames(XR))
  nodes$n_refs <- as.numeric(Matrix::rowSums(XR > 0))
  nodes$n_terms <- as.numeric(Matrix::rowSums(XT > 0))

  ## Sparsified layers and their union ----
  say("Keeping the ", k, " nearest neighbours of every document")
  ER <- mpKnn(XR, similarity, k)
  ET <- mpKnn(XT, similarity, k)
  key <- function(E) paste(E$i, E$j)
  U <- unique(rbind(ER[, c("i", "j")], ET[, c("i", "j")]))
  U <- U[order(U$i, U$j), , drop = FALSE]
  pairs <- data.frame(from = nodes$node[U$i], to = nodes$node[U$j], i = U$i, j = U$j)
  pairs$s_R <- mpPairSimilarity(XR, U$i, U$j, similarity)
  pairs$s_T <- mpPairSimilarity(XT, U$i, U$j, similarity)
  pairs$in_R <- key(U) %in% key(ER)
  pairs$in_T <- key(U) %in% key(ET)
  pairs$edge_type <- ifelse(pairs$in_R & pairs$in_T, "both",
                            ifelse(pairs$in_R, "references_only", "topics_only"))

  ## Association between the layers, over all pairs ----
  say("Association between the layers on a sample of ", format(n.sample, big.mark = ","), " pairs")
  assoc <- mpLayerAssociation(XR, XT, similarity, n.sample = n.sample, seed = seed)

  ## Typology of the pairs ----
  pairs$p_R <- mpPercentile(pairs$s_R, assoc$sample$s_R)
  pairs$p_T <- mpPercentile(pairs$s_T, assoc$sample$s_T)
  if (standardize == "nullmodel") {
    say("Null model: ", n.perm, " randomisations per layer")
    nR <- mpNullPairStats(L$R, U$i, U$j, similarity, pairs$s_R, n.perm, seed)
    nT <- mpNullPairStats(L$T, U$i, U$j, similarity, pairs$s_T, n.perm, seed + 1)
    pairs$z_R <- nR$z
    pairs$z_T <- nT$z
    pairs$pval_R <- nR$p
    pairs$pval_T <- nT$p
    high_R <- pairs$pval_R < alpha
    high_T <- pairs$pval_T < alpha
  } else if (standardize == "percentile") {
    high_R <- pairs$p_R > tau
    high_T <- pairs$p_T > tau
  } else {
    high_R <- pairs$s_R > stats::quantile(pairs$s_R, tau)
    high_T <- pairs$s_T > stats::quantile(pairs$s_T, tau)
  }
  pairs$quadrant <- factor(
    ifelse(high_R & high_T, "consolidation",
      ifelse(high_R, "branching", ifelse(high_T, "convergence", "detachment"))
    ),
    levels = c("consolidation", "branching", "convergence", "detachment")
  )

  ## Node indices ----
  nodes <- cbind(nodes, mpNodeIndices(pairs, nrow(nodes)))
  nodes$LA <- mpRowAlignment(XR, XT, similarity)

  ## Graphs ----
  toGraph <- function(E, w) {
    g <- igraph::make_empty_graph(nrow(nodes), directed = FALSE)
    igraph::V(g)$name <- nodes$node
    g <- igraph::add_edges(g, as.vector(t(as.matrix(E[, c("i", "j")]))))
    igraph::E(g)$weight <- w
    g
  }
  gU <- toGraph(pairs, pairs$s_R + pairs$s_T)
  igraph::E(gU)$s_R <- pairs$s_R
  igraph::E(gU)$s_T <- pairs$s_T
  igraph::E(gU)$edge_type <- pairs$edge_type
  igraph::E(gU)$quadrant <- as.character(pairs$quadrant)

  params <- list(
    n = if (is.null(n)) NA else n, select.by = select.by,
    topic.field = paste(topic.field, collapse = ";"), k = k, similarity = similarity,
    ref.weight = ref.weight, standardize = standardize, alpha = alpha, tau = tau,
    min.ref.freq = min.ref.freq, min.term.freq = min.term.freq, topic.tf = L$T$tf,
    n.sample = n.sample, n.perm = n.perm, seed = seed
  )
  res <- list(
    pairs = pairs, nodes = nodes,
    layers = list(references = toGraph(ER, ER$s), topics = toGraph(ET, ET$s), union = gU),
    X_R = XR, X_T = XT, refs = refs,
    info = list(
      dropped = dropped, n_nodes = nrow(nodes), n_refs = ncol(L$R$inc), n_terms = ncol(L$T$inc),
      keyword_coverage = kw_cov,
      association = assoc[names(assoc) != "sample"],
      seconds = proc.time()[["elapsed"]] - t0
    ),
    params = data.frame(params = names(params), values = vapply(params, as.character, ""),
                        row.names = NULL, stringsAsFactors = FALSE)
  )
  class(res) <- "biblioMultiplex"
  res
}

#' @method print biblioMultiplex
#' @export
print.biblioMultiplex <- function(x, ...) {
  p <- stats::setNames(x$params$values, x$params$params)
  pr <- x$pairs
  cat("Multiplex coupling of", x$info$n_nodes, "documents\n")
  cat("  references: CR -", x$info$n_refs, "cited references, weight", p[["ref.weight"]], "\n")
  cat("  topics    :", gsub(";", "+", p[["topic.field"]]), "-", x$info$n_terms, "terms\n")
  if (!is.na(x$info$keyword_coverage)) {
    cat(sprintf("              chosen automatically: keywords cover %.0f%% of the documents with references\n",
                100 * x$info$keyword_coverage))
  }
  cat("  similarity", p[["similarity"]], "| neighbours k =", p[["k"]], "| standardize", p[["standardize"]], "\n")
  d <- x$info$dropped
  cat("  documents:", d[["total"]], "in M;", d[["no_references"]], "without CR;", d[["no_topic"]],
      "without topic terms;", d[["empty_after_filters"]], "empty after the frequency filters\n\n")
  cat("Neighbour pairs: references", sum(pr$in_R), "| topics", sum(pr$in_T), "| both", sum(pr$in_R & pr$in_T), "\n")
  a <- x$info$association
  cat(sprintf("Association of the layers on %s random pairs: Spearman %.3f (QAP p = %.3f)\n",
              format(a$n_pairs, big.mark = ","), a$spearman, a$qap_p))
  cat("\nPair typology:\n")
  print(table(pr$quadrant))
  if (!is.null(x$clusters)) {
    cat("\n")
    printMultiplexClusters(x$clusters)
  }
  cat(sprintf("\nComputed in %.1f s\n", x$info$seconds))
  invisible(x)
}

## Internal helpers ----

# the random number state of the session, saved and restored around the
# seeded steps so that a call does not change the user's stream
saveRNG <- function() {
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  } else {
    NULL
  }
}

restoreRNG <- function(state) {
  if (is.null(state)) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  } else {
    assign(".Random.seed", state, envir = globalenv())
  }
}

# A layer is described by
#   inc  : binary document x feature incidence (dgCMatrix)
#   val  : the value of each non-zero cell of inc, in the order of inc@x
#   colw : a weight per feature (IDF, or 1)
# so that the null model can permute the incidence and rebuild the layer
# exactly as the observed one was built.

# document x item matrix of a field, rows in the order of M
mpFieldMatrix <- function(M, field, binary = TRUE, remove.terms = NULL, synonyms = NULL) {
  W <- suppressWarnings(cocMatrix(M,
    Field = field, type = "sparse", sep = ";", binary = binary,
    remove.terms = remove.terms, synonyms = synonyms
  ))
  if (!inherits(W, "Matrix")) {
    W <- Matrix::sparseMatrix(i = integer(0), j = integer(0), x = numeric(0),
                              dims = c(nrow(M), 0L), dimnames = list(M$SR, character(0)))
  }
  rownames(W) <- M$SR
  methods::as(W, "generalMatrix")
}

# features of at least min.freq documents
mpFilterFeatures <- function(W, min.freq) {
  W[, Matrix::colSums(W > 0) >= min.freq, drop = FALSE]
}

mpRootsLayer <- function(M, min.freq = 2, weight = "none") {
  W <- mpFilterFeatures(mpFieldMatrix(M, "CR", binary = TRUE), min.freq)
  # placeholders that Web of Science writes for unreadable references are not
  # cited works: shared by unrelated documents, they would couple them
  W <- W[, !grepl("^(NO TITLE CAPTURED|\\[?ANONYMOUS\\]?)", colnames(W)), drop = FALSE]
  W@x[] <- 1
  mpMakeLayer(W, weight = weight, tf = "binary")
}

# topic layer from one or more fields, merged by term
mpTopicLayer <- function(M, fields = c("DE", "ID"), min.freq = 2, ngrams = 1, stemming = FALSE,
                         remove.terms = NULL, synonyms = NULL) {
  tf <- if (all(fields %in% c("DE", "ID"))) "binary" else "log"
  parts <- lapply(fields, function(f) {
    if (f %in% c("TI", "AB")) {
      M2 <- suppressMessages(termExtraction(M,
        Field = f, ngrams = ngrams, stemming = stemming,
        remove.terms = remove.terms, synonyms = synonyms, verbose = FALSE
      ))
      # termExtraction() returns the rows in another order
      M2 <- as.data.frame(M2)[match(M$SR, M2$SR), , drop = FALSE]
      mpFieldMatrix(M2, paste0(f, "_TM"), binary = FALSE)
    } else {
      mpFieldMatrix(M, f, binary = TRUE, remove.terms = remove.terms, synonyms = synonyms)
    }
  })
  trip <- do.call(rbind, lapply(parts, function(W) {
    s <- Matrix::summary(W)
    data.frame(i = s$i, term = colnames(W)[s$j], x = s$x, stringsAsFactors = FALSE)
  }))
  terms <- sort(unique(trip$term))
  W <- Matrix::sparseMatrix(
    i = trip$i, j = match(trip$term, terms), x = trip$x, # the same term in two fields is summed
    dims = c(nrow(M), length(terms)), dimnames = list(M$SR, terms)
  )
  # a term of one character, or of digits and punctuation only, carries no
  # meaning by itself: Web of Science exports some keywords split ("Industry 4;
  # 0"), and "0" would name a theme
  keep <- nchar(colnames(W)) >= 2 & !grepl("^[[:digit:][:punct:][:space:]]+$", colnames(W))
  W <- W[, keep, drop = FALSE]
  mpMakeLayer(mpFilterFeatures(W, min.freq), weight = "idf", tf = tf)
}

mpMakeLayer <- function(W, weight, tf) {
  W <- methods::as(W, "generalMatrix")
  val <- switch(tf,
    binary = rep(1, length(W@x)),
    log = 1 + log(W@x)
  )
  inc <- W
  inc@x[] <- 1
  df <- Matrix::colSums(inc)
  colw <- if (weight == "idf") log(nrow(inc) / df) else rep(1, ncol(inc))
  list(inc = inc, val = val, colw = colw, weight = weight, tf = tf)
}

# document x feature matrix of a layer
mpLayerMatrix <- function(layer, inc = layer$inc, val = layer$val) {
  X <- inc
  X@x <- val
  X <- X %*% Matrix::Diagonal(x = layer$colw)
  colnames(X) <- colnames(inc) # the product with Diagonal() drops them
  X <- methods::as(methods::as(X, "CsparseMatrix"), "generalMatrix")
  Matrix::drop0(X)
}

# Similarities from dot products c_ij and squared norms d_i; on binary data they
# equal the indices of normalizeSimilarity(). No dense n x n matrix is built:
# rows are processed in blocks.
mpSimFromDot <- function(c, di, dj, type) {
  s <- switch(type,
    cosine = c / sqrt(di * dj),
    association = c / (di * dj),
    jaccard = c / (di + dj - c)
  )
  s[is.nan(s)] <- 0
  s
}

mpSqNorms <- function(X) {
  if (inherits(X, "Matrix")) as.numeric(Matrix::rowSums(X^2)) else rowSums(X^2)
}

mpBlocks <- function(n, cells = 4e6) {
  size <- max(1L, floor(cells / max(n, 1)))
  split(seq_len(n), ceiling(seq_len(n) / size))
}

# dense block of similarities between the rows b and all the rows, self = NA
mpSimBlock <- function(X, d, b, type) {
  C <- as.matrix(Matrix::tcrossprod(X[b, , drop = FALSE], X))
  S <- mpSimFromDot(C, d[b], rep(d, each = length(b)), type)
  dim(S) <- dim(C)
  S[cbind(seq_along(b), b)] <- NA
  S
}

# k nearest neighbours of every node (positive similarity), as an undirected
# edge list i < j: an edge is kept when either end selects it
mpKnn <- function(X, type = "cosine", k = 10) {
  d <- mpSqNorms(X)
  blocks <- mpBlocks(nrow(X))
  edges <- lapply(blocks, function(b) {
    S <- mpSimBlock(X, d, b, type)
    keep <- lapply(seq_along(b), function(r) {
      s <- S[r, ]
      pos <- which(!is.na(s) & s > 0)
      if (length(pos) > k) pos <- pos[order(-s[pos], pos)[seq_len(k)]]
      pos
    })
    len <- lengths(keep)
    data.frame(from = rep(b, len), to = unlist(keep),
               s = S[cbind(rep(seq_along(b), len), unlist(keep))])
  })
  E <- do.call(rbind, edges)
  if (is.null(E) || nrow(E) == 0) {
    return(data.frame(i = integer(0), j = integer(0), s = numeric(0)))
  }
  E <- data.frame(i = pmin(E$from, E$to), j = pmax(E$from, E$to), s = E$s)
  E <- E[!duplicated(E[, c("i", "j")]), ]
  E[order(E$i, E$j), , drop = FALSE]
}

# exact similarity of a layer on a list of pairs (i, j)
mpPairSimilarity <- function(X, i, j, type = "cosine", d = mpSqNorms(X), chunk = 20000) {
  c <- numeric(length(i))
  for (s in seq(1, length(i), by = chunk)) {
    idx <- s:min(s + chunk - 1, length(i))
    Xi <- X[i[idx], , drop = FALSE]
    Xj <- X[j[idx], , drop = FALSE]
    c[idx] <- if (inherits(X, "Matrix")) as.numeric(Matrix::rowSums(Xi * Xj)) else rowSums(Xi * Xj)
  }
  mpSimFromDot(c, d[i], d[j], type)
}

# Spearman correlation between the full rows of the two layers (a document
# against all the others), in blocks
mpRowAlignment <- function(XR, XT, type = "cosine") {
  n <- nrow(XR)
  dR <- mpSqNorms(XR)
  dT <- mpSqNorms(XT)
  la <- rep(NA_real_, n)
  for (b in mpBlocks(n, cells = 2e6)) {
    SR <- mpSimBlock(XR, dR, b, type)
    ST <- mpSimBlock(XT, dT, b, type)
    for (r in seq_along(b)) {
      x <- SR[r, -b[r]]
      y <- ST[r, -b[r]]
      if (stats::sd(x) > 0 && stats::sd(y) > 0) la[b[r]] <- stats::cor(x, y, method = "spearman")
    }
  }
  la
}

# a uniform sample of distinct unordered pairs
mpSamplePairs <- function(n, size, seed = 1234) {
  set.seed(seed)
  size <- min(size, n * (n - 1) / 2)
  i <- sample.int(n, ceiling(size * 1.2), replace = TRUE)
  j <- sample.int(n, ceiling(size * 1.2), replace = TRUE)
  keep <- i != j
  P <- data.frame(i = pmin(i, j)[keep], j = pmax(i, j)[keep])
  P <- P[!duplicated(P), ]
  P[seq_len(min(size, nrow(P))), ]
}

# mid-rank percentile of s within a reference sample
mpPercentile <- function(s, ref) {
  ref <- sort(ref)
  less <- findInterval(s, ref, left.open = TRUE)
  eq <- findInterval(s, ref) - less
  (less + 0.5 * eq) / length(ref)
}

# association between the two layers over a sample of pairs, with a QAP test
# (the documents of the topic layer are relabelled at random)
mpLayerAssociation <- function(XR, XT, type = "cosine", n.sample = 1e5, n.perm = 99, seed = 1234) {
  P <- mpSamplePairs(nrow(XR), n.sample, seed)
  sR <- mpPairSimilarity(XR, P$i, P$j, type)
  sT <- mpPairSimilarity(XT, P$i, P$j, type)
  # a layer whose sampled similarities are all equal (tiny collections) has no
  # correlation with the other: NA, without the warning of cor()
  safeCor <- function(x, y, method = "pearson") {
    if (length(x) < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) NA_real_ else stats::cor(x, y, method = method)
  }
  rho <- safeCor(sR, sT, "spearman")
  set.seed(seed + 7)
  rho0 <- vapply(seq_len(n.perm), function(p) {
    perm <- sample.int(nrow(XT))
    safeCor(sR, mpPairSimilarity(XT, perm[P$i], perm[P$j], type), "spearman")
  }, numeric(1))
  list(
    spearman = rho, pearson = safeCor(sR, sT), n_pairs = nrow(P),
    qap_p = if (is.na(rho)) NA_real_ else (1 + sum(abs(rho0) >= abs(rho), na.rm = TRUE)) / (n.perm + 1),
    share_R_positive = mean(sR > 0), share_T_positive = mean(sT > 0),
    sample = data.frame(P, s_R = sR, s_T = sT)
  )
}

# Curveball randomisation of an incidence given as a list of rows: it keeps the
# number of features of every document and of documents of every feature
mpCurveball <- function(rows, n.trades) {
  n <- length(rows)
  for (t in seq_len(n.trades)) {
    ab <- sample.int(n, 2L)
    A <- rows[[ab[1]]]
    B <- rows[[ab[2]]]
    inB <- A %in% B
    Ao <- A[!inB]
    Bo <- B[!(B %in% A)]
    if (length(Ao) == 0L || length(Bo) == 0L) next
    pool <- c(Ao, Bo)
    pick <- sample.int(length(pool), length(Ao))
    rows[[ab[1]]] <- c(A[inB], pool[pick])
    rows[[ab[2]]] <- c(A[inB], pool[-pick])
  }
  rows
}

mpRowsToLayer <- function(rows, vals, layer) {
  len <- lengths(rows)
  # the term frequencies of each document are shuffled over its new features
  v <- unlist(lapply(seq_along(rows), function(r) {
    x <- vals[[r]]
    if (length(x) > 1L) x[sample.int(length(x))] else x
  }))
  W <- Matrix::sparseMatrix(
    i = rep(seq_along(rows), len), j = unlist(rows), x = v,
    dims = dim(layer$inc), dimnames = dimnames(layer$inc)
  )
  W <- methods::as(W, "generalMatrix")
  inc <- W
  inc@x[] <- 1
  list(inc = inc, val = W@x)
}

# null distribution of the similarity of the pairs (i, j) of a layer
mpNullPairStats <- function(layer, i, j, type, s_obs, n.perm = 99, seed = 1234) {
  X0 <- layer$inc
  X0@x <- layer$val
  s0 <- Matrix::summary(X0)
  o <- order(s0$i, s0$j)
  f <- factor(s0$i[o], levels = seq_len(nrow(X0)))
  rows <- split(s0$j[o], f)
  vals <- split(s0$x[o], f)
  n.trades <- 5L * length(rows)
  set.seed(seed)
  seeds <- sample.int(.Machine$integer.max, n.perm)
  sims <- vapply(seq_len(n.perm), function(p) {
    set.seed(seeds[p])
    L <- mpRowsToLayer(mpCurveball(rows, n.trades), vals, layer)
    mpPairSimilarity(mpLayerMatrix(layer, inc = L$inc, val = L$val), i, j, type)
  }, numeric(length(i)))
  sims <- matrix(sims, nrow = length(i))
  mu <- rowMeans(sims)
  sdv <- apply(sims, 1, stats::sd)
  z <- (s_obs - mu) / sdv
  z[!is.finite(z)] <- NA
  data.frame(mean = mu, sd = sdv, z = z, p = (1 + rowSums(sims >= s_obs)) / (n.perm + 1))
}

# Node indices on the neighbour pairs:
#   CI : share of the topic neighbours that are not references neighbours (convergence)
#   BI : share of the references neighbours that are not topic neighbours (branching)
#   P  : multiplex participation coefficient (Battiston, Nicosia & Latora 2014)
mpNodeIndices <- function(pairs, n) {
  long <- rbind(
    data.frame(node = pairs$i, in_R = pairs$in_R, in_T = pairs$in_T),
    data.frame(node = pairs$j, in_R = pairs$in_R, in_T = pairs$in_T)
  )
  by_node <- split(long, factor(long$node, levels = seq_len(n)))
  out <- t(vapply(by_node, function(d) {
    kR <- sum(d$in_R)
    kT <- sum(d$in_T)
    ci <- if (kT > 0) sum(d$in_T & !d$in_R) / kT else NA_real_
    bi <- if (kR > 0) sum(d$in_R & !d$in_T) / kR else NA_real_
    o <- kR + kT
    pc <- if (o > 0) 2 * (1 - (kR / o)^2 - (kT / o)^2) else NA_real_
    c(degree_R = kR, degree_T = kT, CI = ci, BI = bi, P = pc)
  }, numeric(5)))
  as.data.frame(out, row.names = NULL)
}
