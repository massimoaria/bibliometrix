# The integrated framework of Aria, D'Aniello, Misuraca & Spano (2026),
# Journal of Informetrics 20, 101877: each quantity is recomputed here from
# its definition in the paper and compared with what the functions return.

lineage_fixture <- function() {
  data(management, package = "bibliometrixData", envir = environment())
  suppressWarnings(suppressMessages(
    thematicEvolution(management, field = "DE", years = c(2004, 2015),
                      n = 250, minFreq = 5)
  ))
}

# co-occurrence counts of a period, with lowercase names as in thematicMap()
period_counts <- function(P) {
  NM <- biblioNetwork(P, analysis = "co-occurrences",
                      network = "author_keywords", n = 250, sep = ";")
  rownames(NM) <- colnames(NM) <- tolower(rownames(NM))
  NM
}

test_that("themes are the Louvain communities of the association-strength network (Eq. 3)", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  data(management, package = "bibliometrixData")
  P <- management[management$PY > 2004 & management$PY <= 2015, ]
  tm <- suppressWarnings(thematicMap(P, field = "DE", n = 250, minfreq = 5))
  NM <- period_counts(P)
  # terms below minfreq (here 2) are left out before the network is built
  # (Sect. 3.1)
  mf <- max(2, floor(5 * nrow(P) / 1000))
  NM <- NM[diag(NM) >= mf, diag(NM) >= mf]
  D <- diag(NM)
  W <- as.matrix(NM) / outer(D, D)
  diag(W) <- 0
  g <- igraph::graph_from_adjacency_matrix(W, mode = "undirected", weighted = TRUE)
  g <- igraph::delete_vertices(g, igraph::degree(g) == 0)
  ref <- clusteringNetwork(g, "louvain", seed = 1234)$net_groups
  got <- tm$net$cluster_obj
  m_ref <- setNames(ref$membership, ref$names)
  m_got <- setNames(got$membership, got$names)
  expect_true(all(D[names(m_got)] >= mf))
  expect_setequal(names(m_ref), names(m_got))
  expect_equal(igraph::compare(m_ref[names(m_got)], m_got, method = "nmi"), 1)
})

test_that("Cluster_PageRank is the PageRank within the cluster subgraph of the counts", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  nx <- lineage_fixture()
  data(management, package = "bibliometrixData")
  P <- management[management$PY > 2004 & management$PY <= 2015, ]
  NM <- period_counts(P)
  D <- diag(NM)
  w <- nx$TM[[2]]$words
  for (d in split(w, w$Cluster)) {
    Wh <- as.matrix(NM[d$Words, d$Words])
    diag(Wh) <- 0
    g <- igraph::graph_from_adjacency_matrix(Wh, mode = "undirected", weighted = TRUE)
    p <- igraph::page_rank(g)$vector
    p[igraph::degree(g) == 0] <- 0.15 / nrow(d)
    expect_equal(d$Cluster_PageRank, unname(p[d$Words]))
  }
})

test_that("memberships follow Eq. 4-5 and sum to the fuzzy cardinality (Eq. 6)", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  nx <- lineage_fixture()
  data(management, package = "bibliometrixData")
  P <- management[management$PY > 2004 & management$PY <= 2015, ]
  tm <- nx$TM[[2]]
  w <- tm$words
  cl <- unique(w$Cluster_Label)
  d2c <- tm$documentToClusters
  U <- as.matrix(d2c[, cl])

  # every document with terms has memberships summing to one; one with no
  # author keyword has none
  has_terms <- d2c$SR %in% P$SR[!is.na(P$DE) & trimws(P$DE) != ""]
  expect_equal(unname(rowSums(U[has_terms, ])), rep(1, sum(has_terms)))
  expect_true(all(rowSums(U[!has_terms, , drop = FALSE]) == 0))

  # Eq. 4-5 on the documents linked to more than one cluster
  multi <- which(rowSums(U > 0) > 1)
  expect_gt(length(multi), 0)
  for (i in head(multi, 10)) {
    terms <- tolower(trimws(strsplit(P$DE[P$SR == d2c$SR[i]], ";")[[1]]))
    s <- vapply(cl, function(h) {
      x <- w[w$Cluster_Label == h & w$Words %in% terms, ]
      sum(x$Cluster_PageRank / x$Occurrences)
    }, numeric(1))
    expect_equal(unname(U[i, ]), unname(s / sum(s)))
  }

  # a document whose terms all lie outside the clusters is spread uniformly
  none <- which(has_terms & apply(U, 1, function(u) length(unique(u)) == 1))
  if (length(none) > 0) {
    expect_equal(unname(U[none[1], ]), rep(1 / length(cl), length(cl)))
  }

  expect_equal(
    tm$clusters$fuzzy_cardinality,
    unname(colSums(U)[tm$clusters$name])
  )
})

test_that("lineage strength combines weighted inclusion and importance (Eq. 7-9)", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  nx <- lineage_fixture()
  w1 <- nx$TM[[1]]$words
  w2 <- nx$TM[[2]]$words
  tot1 <- tapply(w1$Cluster_PageRank, w1$Cluster_Label, sum)
  tot2 <- tapply(w2$Cluster_PageRank, w2$Cluster_Label, sum)
  y1 <- sub(".*--", "", nx$Data$CL1[1])
  links <- nx$Data[sub(".*--", "", nx$Data$CL1) == y1, ]
  expect_gt(nrow(links), 0)
  for (r in seq_len(nrow(links))) {
    h <- sub("--.*", "", links$CL1[r])
    j <- sub("--.*", "", links$CL2[r])
    shared <- intersect(w1$Words[w1$Cluster_Label == h], w2$Words[w2$Cluster_Label == j])
    pr1 <- w1$Cluster_PageRank[w1$Cluster_Label == h][match(shared, w1$Words[w1$Cluster_Label == h])]
    pr2 <- w2$Cluster_PageRank[w2$Cluster_Label == j][match(shared, w2$Words[w2$Cluster_Label == j])]
    Iw <- sum(pr1) / tot1[[h]]
    Om <- sqrt(sum(pr1 * pr2) / (tot1[[h]] * tot2[[j]]))
    expect_equal(links$Inc_Weighted[r], Iw)
    expect_equal(links$PageRankIndex[r], Om)
    expect_equal(links$Lineage_Strength[r], 0.5 * Iw + 0.5 * Om)
    # Inc_index and Stability count the shared words
    expect_equal(links$Inc_index[r], length(shared) / min(sum(w1$Cluster_Label == h), sum(w2$Cluster_Label == j)))
  }
  expect_equal(nx$Edges$lineage_strength, nx$Data$Lineage_Strength)
})

test_that("links are kept by the dual threshold (Eq. 10-11)", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  data(management, package = "bibliometrixData")
  for (k in 1:2) {
    nx <- suppressWarnings(suppressMessages(
      thematicEvolution(management, field = "DE", years = c(2004, 2015),
                        n = 250, minFreq = 5, min.lineage = 0.2, top.k = k)
    ))
    E <- nx$Edges
    rk <- ave(E$lineage_strength, E$from, FUN = function(x) rank(-x, ties.method = "first"))
    expect_equal(E$retained, E$lineage_strength >= 0.2 | rk <= k)
  }
})

test_that("patterns and pathways are read on the evolutionary graph (Sect. 3.5)", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  nx <- lineage_fixture()
  N <- nx$Nodes
  kept <- nx$Edges[nx$Edges$retained, ]
  expect_equal(N$n_out, vapply(N$id, function(i) sum(kept$from == i), integer(1)))
  expect_equal(N$n_in, vapply(N$id, function(i) sum(kept$to == i), integer(1)))
  expect_equal(grepl("\\bsplit\\b", N$pattern), N$n_out > 1)
  expect_equal(grepl("\\bmerge\\b", N$pattern), N$n_in > 1)
  slice <- as.integer(as.character(N$slice))
  expect_equal(grepl("\\bemergent\\b", N$pattern), slice > 1 & N$n_in == 0)

  pw <- nx$pathways
  expect_gt(nrow(pw), 0)
  for (r in seq_len(min(5, nrow(pw)))) {
    lab <- strsplit(pw$pathway[r], " -> ", fixed = TRUE)[[1]]
    id <- N$id[match(lab, N$label)]
    expect_equal(pw$length[r], length(lab))
    s <- prod(vapply(seq_len(length(id) - 1), function(i) {
      kept$lineage_strength[kept$from == id[i] & kept$to == id[i + 1]]
    }, numeric(1)))
    expect_equal(pw$strength[r], s)
    expect_equal(pw$cumulative_size[r], sum(N$fuzzy_cardinality[match(lab, N$label)]))
    # maximal: it starts with no incoming link and ends with no outgoing one
    expect_equal(N$n_in[match(lab[1], N$label)], 0L)
    expect_equal(N$n_out[match(lab[length(lab)], N$label)], 0L)
  }
})

test_that("alpha inside assign.evolution.colors is still accepted", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  data(management, package = "bibliometrixData")
  a <- suppressWarnings(suppressMessages(
    thematicEvolution(management, field = "DE", years = c(2004, 2015),
                      n = 250, minFreq = 5, alpha = 0.3)
  ))
  b <- suppressWarnings(suppressMessages(
    thematicEvolution(management, field = "DE", years = c(2004, 2015),
                      n = 250, minFreq = 5,
                      assign.evolution.colors = list(assign = TRUE, alpha = 0.3))
  ))
  expect_equal(a$Edges$lineage_strength, b$Edges$lineage_strength)
  expect_equal(
    a$Edges$lineage_strength,
    0.3 * a$Edges$Inc_Weighted + 0.7 * a$Edges$PageRankIndex
  )
  expect_error(
    thematicEvolution(management, field = "DE", years = c(2004, 2015), alpha = 2),
    "alpha must be a number between 0 and 1"
  )
})

test_that("the most frequent term of a theme keeps its weight in the lineage", {
  skip_on_cran()
  skip_if_offline()
  M <- suppressMessages(loadBookDataset())
  nx <- suppressWarnings(suppressMessages(
    thematicEvolution(M, field = "DE", years = c(2004, 2010, 2015), n = 250, minFreq = 5)
  ))
  # "citation analysis" names a theme in 2005-2010 and in 2011-2015; on the
  # association strength the term was 26th of 31 by PageRank in its theme and
  # the link between the two was dropped
  w <- nx$TM[[3]]$words
  w <- w[w$Cluster_Label == "citation analysis", ]
  expect_equal(w$Words[which.max(w$Cluster_PageRank)], "citation analysis")
  link <- nx$Data[nx$Data$CL1 == "citation analysis--2005-2010" &
    nx$Data$CL2 == "citation analysis--2011-2015", ]
  expect_equal(nrow(link), 1)
  expect_true(link$Retained)
  out <- nx$Data[nx$Data$CL1 == "citation analysis--2005-2010", ]
  expect_equal(link$Lineage_Strength, max(out$Lineage_Strength))
})

test_that("the documents of a co-occurrence network are assigned to its clusters", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  data(management, package = "bibliometrixData", envir = environment())
  NW <- biblioNetwork(management, analysis = "co-occurrences",
                      network = "author_keywords", n = 50, sep = ";")
  net <- suppressWarnings(networkPlot(NW, n = 50, normalize = "association",
                                      remove.multiple = FALSE, verbose = FALSE))
  w <- clusterWords(NW, net$cluster_res)
  # the occurrences are those of the counts: matched in the case of the
  # matrix, no term found them and every term counted 1
  expect_identical(w$Occurrences,
                   unname(diag(as.matrix(NW))[match(w$Words, tolower(rownames(NW)))]))
  expect_gt(max(w$Occurrences), 1)
  NWl <- NW
  rownames(NWl) <- colnames(NWl) <- tolower(rownames(NW))
  expect_identical(w$Cluster_PageRank, clusterPageRank(NWl, w$Words, w$cluster))
  # clusterAssignment() reads Cluster_PageRank: without it the Documents table
  # of the co-occurrence network failed, and the failure was swallowed
  w$Cluster_Label <- paste("Cluster", w$cluster)
  dtc <- clusterAssignment(management, words = w, field = "DE",
                           remove.terms = NULL, synonyms = NULL, threshold = 0.5)
  expect_true(nrow(dtc) > 0)
  expect_true(all(c("Assigned_cluster", "SR") %in% names(dtc)))
})
