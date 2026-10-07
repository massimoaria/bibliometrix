# Multiplex coupling: multiplexCoupling(), multiplexClusters(),
# multiplexEvolution(), multiplexPlot()

mk <- function(CR, DE, TC = seq_along(CR)) {
  M <- data.frame(
    SR = paste0("D", seq_along(CR)), CR = CR, DE = DE, ID = NA_character_, AU = "A;B",
    TI = paste("title", seq_along(CR)), PY = 2020, TC = TC, DB = "ISI", stringsAsFactors = FALSE
  )
  row.names(M) <- M$SR
  M
}
refs <- function(...) paste(sprintf("SMITH J, %d, J INFORMETR, V1, P1", c(...)), collapse = ";")

# one coupling of management, shared by the tests that only read it
mcFixture <- local({
  mc <- NULL
  function() {
    if (is.null(mc)) {
      data(management, package = "bibliometrixData", envir = environment())
      mc <<- multiplexClusters(multiplexCoupling(management, n = 300, n.perm = 19, verbose = FALSE),
                               openalex = FALSE)
    }
    mc
  }
})

test_that("the references layer reproduces the coupling indices of normalizeSimilarity()", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  data(management, package = "bibliometrixData")
  M <- as.data.frame(management)
  at <- function(S, pr) as.numeric(S[cbind(match(pr$from, rownames(S)), match(pr$to, rownames(S)))])
  for (sim in c(association = "association", cosine = "salton", jaccard = "jaccard")) {
    nm <- names(which(c(association = "association", cosine = "salton", jaccard = "jaccard") == sim))
    mc <- multiplexCoupling(management, n = 300, similarity = nm, k = 5, standardize = "percentile",
                            verbose = FALSE)
    W <- cocMatrix(M[match(mc$nodes$node, M$SR), ], Field = "CR")
    W <- W[, Matrix::colSums(W) >= 2]
    W <- W[, !grepl("^(NO TITLE CAPTURED|\\[?ANONYMOUS\\]?)", colnames(W))] # not cited works
    expect_equal(mc$pairs$s_R, at(normalizeSimilarity(Matrix::tcrossprod(W), type = sim), mc$pairs),
                 info = nm)
  }
})

test_that("neighbour pairs and the union of the layers are consistent", {
  skip_if_not_installed("bibliometrixData")
  mc <- mcFixture()
  pr <- mc$pairs
  expect_s3_class(mc, "biblioMultiplex")
  expect_true(all(pr$in_R | pr$in_T))
  expect_true(all(pr$s_R[pr$in_R] > 0))
  expect_true(all(pr$s_T[pr$in_T] > 0))
  expect_false(any(duplicated(pr[, c("i", "j")])))
  expect_true(all(pr$i < pr$j))
  expect_lte(nrow(mc$nodes), 300)
  expect_equal(igraph::ecount(mc$layers$union), nrow(pr))
  expect_true(all(c("z_R", "z_T", "pval_R", "pval_T") %in% names(pr)))
  expect_output(print(mc), "Multiplex coupling of")
})

test_that("documents without references or terms are left out and counted", {
  M <- mk(
    CR = c(refs(1, 2, 3), refs(1, 2), refs(2, 3), NA, refs(1, 3), refs(1, 2, 3)),
    DE = c("AA;BB", "AA;CC", "BB;CC", "AA;BB", NA, "AA;BB;CC")
  )
  mc <- multiplexCoupling(M, n = NULL, k = 2, n.sample = 100, n.perm = 9, verbose = FALSE)
  expect_equal(mc$info$dropped[["no_references"]], 1)
  expect_equal(mc$info$dropped[["no_topic"]], 1)
  expect_setequal(mc$nodes$node, c("D1", "D2", "D3", "D6"))
})

test_that("collections that cannot be coupled are refused", {
  M <- mk(CR = c(refs(1, 2), refs(1, 2), NA), DE = c("AA", "AA", "AA"))
  expect_error(multiplexCoupling(M, verbose = FALSE), "fewer than 3 documents")
  expect_error(multiplexCoupling(M[, names(M) != "CR"], verbose = FALSE), "no CR field")
  expect_error(multiplexClusters(list()), "multiplexCoupling")
  expect_error(multiplexPlot(list()), "multiplexClusters")
})

test_that("curveball preserves the degrees of documents and features", {
  set.seed(1)
  inc <- Matrix::rsparsematrix(40, 60, density = 0.1)
  inc@x[] <- 1
  s <- Matrix::summary(inc)
  rows <- split(s$j, factor(s$i, levels = 1:40))
  r2 <- mpCurveball(rows, 500)
  expect_equal(lengths(r2), lengths(rows), ignore_attr = TRUE)
  expect_equal(tabulate(unlist(r2), 60), tabulate(unlist(rows), 60))
  expect_false(identical(r2, rows))
  expect_true(all(!vapply(r2, anyDuplicated, 0L)))
})

test_that("the analysis is reproducible and leaves the session's random numbers alone", {
  M <- mk(
    CR = c(refs(1, 2, 3), refs(1, 2), refs(2, 3), refs(1, 3), refs(1, 2, 3), refs(3, 4), refs(2, 4)),
    DE = c("AA;BB", "AA;CC", "BB;CC", "AA;BB", "AA;BB;CC", "CC;DD", "BB;DD")
  )
  set.seed(42)
  expected <- stats::runif(1)
  set.seed(42)
  a <- multiplexCoupling(M, n = NULL, k = 3, n.sample = 100, n.perm = 9, verbose = FALSE)
  # seven documents: every cluster is kept (min.size = 1)
  a <- multiplexClusters(a, min.size = 1, openalex = FALSE)
  expect_equal(stats::runif(1), expected)
  b <- multiplexClusters(multiplexCoupling(M, n = NULL, k = 3, n.sample = 100, n.perm = 9, verbose = FALSE),
                         min.size = 1, openalex = FALSE)
  expect_identical(a$pairs, b$pairs)
  expect_identical(a$clusters$membership, b$clusters$membership)
})

test_that("population percentiles use mid-ranks", {
  expect_equal(mpPercentile(c(0, 1, 5), c(0, 0, 1, 2)), c(0.25, 0.625, 1))
})

test_that("topic fields are merged by term", {
  M <- mk(CR = c(refs(1, 2), refs(1, 2), refs(1, 3)), DE = c("AA;BB", "AA", "BB"))
  M$ID <- c("AA", "CC", "CC")
  L <- mpTopicLayer(M, c("DE", "ID"), min.freq = 1)
  expect_setequal(colnames(L$inc), c("AA", "BB", "CC"))
  expect_equal(as.numeric(L$inc["D1", "AA"]), 1)
})

test_that("topic.field = 'auto' uses keywords when they cover the documents, TI+AB otherwise", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  data(management, package = "bibliometrixData")
  a <- multiplexCoupling(management, n = 150, standardize = "percentile", verbose = FALSE)
  expect_equal(a$params$values[a$params$params == "topic.field"], "DE;ID")
  M <- as.data.frame(management)
  M$DE[1:500] <- NA
  M$ID[1:500] <- NA
  b <- multiplexCoupling(M, n = 150, standardize = "percentile", verbose = FALSE)
  expect_equal(b$params$values[b$params$params == "topic.field"], "TI;AB")
  expect_lt(b$info$keyword_coverage, 0.8)
})

test_that("cluster similarities from the sums of unit rows equal the mean over the pairs", {
  set.seed(3)
  X <- Matrix::rsparsematrix(30, 12, density = 0.3, rand.x = function(n) stats::runif(n))
  X[5, ] <- 0
  memb <- sample(1:4, 30, replace = TRUE)
  U <- mpUnitRows(X)
  S <- as.matrix(Matrix::tcrossprod(U))
  cs <- mpClusterSimilarity(U, memb)
  for (a in 1:4) {
    ia <- which(memb == a)
    W <- S[ia, ia]
    expect_equal(cs$within[a], mean(W[upper.tri(W)]))
    for (b in 1:4) if (a != b) expect_equal(cs$between[a, b], mean(S[ia, memb == b]))
  }
  expect_equal(cs$global, mean(S[upper.tri(S)]))
  # documents in no cluster (NA): left out of the clusters, kept in the baseline
  memb[c(2, 9, 17)] <- NA
  cs <- mpClusterSimilarity(U, memb)
  for (a in 1:4) {
    ia <- which(memb == a)
    W <- S[ia, ia]
    expect_equal(cs$within[a], mean(W[upper.tri(W)]))
    expect_equal(cs$size[a], length(ia))
  }
  expect_equal(cs$global, mean(S[upper.tri(S)]))
})

test_that("standardized residuals match chisq.test()", {
  tab <- matrix(c(20, 5, 3, 7, 18, 4, 2, 6, 25), 3)
  expect_equal(mpStandardizedResiduals(tab), suppressWarnings(stats::chisq.test(tab)$stdres),
               ignore_attr = TRUE)
})

test_that("roots and themes are classified from one set of links", {
  skip_if_not_installed("bibliometrixData")
  mc <- mcFixture()
  cl <- mc$clusters
  m <- cl$membership
  expect_equal(sum(cl$contingency), sum(!is.na(m$root) & !is.na(m$theme)))
  expect_equal(nrow(cl$roots), max(m$root, na.rm = TRUE))
  expect_true(all(cl$roots$size == tabulate(m$root)))
  expect_equal(cl$unclustered, c(roots = sum(is.na(m$root)), themes = sum(is.na(m$theme))))
  expect_true(all(nzchar(cl$roots$terms)))
  # the links counted by the roots and by the themes are the same
  expect_equal(cl$roots$n_themes, tabulate(cl$links$root, nrow(cl$roots)))
  expect_equal(cl$themes$n_roots, tabulate(cl$links$theme, nrow(cl$themes)))
  expect_true(all(cl$links$residual > 2 & cl$links$n >= 5))
  expect_equal(cl$roots$structure == "branching", cl$roots$n_themes >= 2)
  expect_equal(cl$themes$structure == "convergence", cl$themes$n_roots >= 2)
  expect_equal(nrow(cl$plane), choose(nrow(cl$roots), 2))
  expect_true(all(cl$agreement > 0 & cl$agreement <= 1))
  expect_output(print(mc), "Roots  :")
})

test_that("a stricter link rule finds no more links", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  mc <- mcFixture()
  strict <- multiplexClusters(mc, min.link = 10, openalex = FALSE)$clusters$links
  expect_lte(nrow(strict), nrow(mc$clusters$links))
  expect_true(all(strict$n >= 10))
})

test_that("roots and themes smaller than min.size are left out", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  mc <- mcFixture()
  all1 <- suppressMessages(multiplexClusters(mc, min.size = 1, verbose = FALSE, openalex = FALSE))$clusters
  sz <- all1$themes$size
  cut <- sort(unique(sz))[2]
  cl <- suppressMessages(multiplexClusters(mc, min.size = cut, verbose = FALSE, openalex = FALSE))$clusters
  expect_true(all(cl$themes$size >= cut) && all(cl$roots$size >= cut))
  # the clusters kept are the same, with the same numbers
  expect_equal(cl$themes$size, sz[sz >= cut])
  expect_equal(cl$unclustered[["themes"]], sum(sz[sz < cut]))
  expect_equal(is.na(cl$membership$theme), all1$membership$theme %in% which(sz < cut))
  expect_true(is.finite(cl$NMI))
  expect_output(printMultiplexClusters(cl), "Left out")
  m2 <- mc
  m2$clusters <- cl
  ev <- multiplexEvolution(m2, years = c(2012, 2016), min.docs = 3)
  expect_s3_class(ev, "biblioMultiplexEvolution")
  expect_error(multiplexClusters(mc, min.size = 1e6, verbose = FALSE, openalex = FALSE), "lower min.size")
})

test_that("periods from cut points and from sliding windows", {
  PY <- c(2000, 2001, 2005, 2006, 2010)
  p <- mpPeriods(PY, years = c(2002, 2006))
  expect_equal(vapply(p, `[[`, "", "label"), c("2000-2002", "2003-2006", "2007-2010"))
  expect_equal(sort(unlist(lapply(p, `[[`, "idx"))), 1:5)
  w <- mpPeriods(PY, width = 4, step = 2)
  expect_equal(w[[1]]$idx, c(1, 2))
  expect_error(mpPeriods(PY), "cut points")
})

test_that("multiplexEvolution follows pairs of roots and classifies their trend", {
  skip_if_not_installed("bibliometrixData")
  mc <- mcFixture()
  ev <- multiplexEvolution(mc, years = c(2012, 2016, 2018), min.docs = 3)
  expect_s3_class(ev, "biblioMultiplexEvolution")
  expect_true(all(ev$long$A < ev$long$B))
  expect_true(all(ev$trajectories$periods >= 2))
  expect_true(all(ev$trajectories$trend %in%
                    c("converging", "diverging", "drifting apart", "consolidating",
                      "closer in references", "farther in references", "stable")))
  tr <- ev$trajectories
  cv <- tr[tr$trend == "converging", ]
  if (nrow(cv)) expect_true(all(cv$slope_T >= 0.1 & cv$slope_R < 0.1))
  # stable only when neither slope reaches slope.min
  st <- tr[tr$trend == "stable", ]
  if (nrow(st)) expect_true(all(abs(st$slope_T) < 0.1 & abs(st$slope_R) < 0.1))
  cr <- tr[tr$trend %in% c("closer in references", "farther in references"), ]
  if (nrow(cr)) expect_true(all(abs(cr$slope_T) < 0.1 & abs(cr$slope_R) >= 0.1))
  expect_output(print(ev), "Multiplex evolution")
  expect_error(multiplexEvolution(multiplexCoupling(mk(
    CR = c(refs(1, 2), refs(1, 2), refs(1, 2)), DE = c("AA", "AA", "AA")
  ), n.perm = 9, n.sample = 10, verbose = FALSE)), "multiplexClusters")
})

test_that("the plane names shared themes only for pairs close in topics and spreads its labels", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  mc <- mcFixture()
  P <- multiplexPlot(mc, "plane", n.labels = 6)$data
  # a pair close in references only, or in neither, never carries a theme
  expect_true(all(P$theme[!P$quadrant %in% c("consolidation", "convergence")] == ""))
  # the pairs named without a theme are spread over the areas other than
  # "close in neither": no area gets more than its share while another lacks it
  named <- P$show != "" & P$theme == ""
  expect_false(any(named & P$quadrant == "detachment"))
  per_area <- table(factor(P$quadrant[named], levels = c("consolidation", "convergence", "branching")))
  avail <- table(factor(P$quadrant[P$quadrant != "detachment" & P$theme == ""],
                        levels = c("consolidation", "convergence", "branching")))
  expect_true(all(per_area >= pmin(avail, 1)))
})

test_that("every plot renders, static and interactive", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  mc <- mcFixture()
  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (tp in c("matrix", "links", "plane", "clusters")) {
    g <- multiplexPlot(mc, tp)
    expect_s3_class(g, "ggplot")
    expect_no_error(print(g), message = tp)
    expect_s3_class(multiplexPlot(mc, tp, interactive = TRUE), "plotly")
  }
  sk <- plotly::plotly_build(multiplexPlot(mc, "links", interactive = TRUE))
  expect_equal(sk$x$data[[1]]$type, "sankey")
  expect_equal(length(sk$x$data[[1]]$link$value), nrow(mc$clusters$links))
  # clicking a node greys out what is not connected to it
  hooks <- multiplexPlot(mc, "links", interactive = TRUE)$jsHooks$render
  expect_true(any(vapply(hooks, function(h) grepl("sankey-node", h$code, fixed = TRUE), logical(1))))
  expect_s3_class(multiplexPlot(mc, "network"), "visNetwork")
  # a cluster whose documents share almost nothing sits at the floor, not at
  # log2(1e-13), which would squash every other cluster against the edge
  m2 <- mc
  m2$clusters$themes$cohesion_R[1] <- 1e-13
  ch <- plotly::plotly_build(multiplexPlot(m2, "clusters", interactive = TRUE))
  expect_equal(min(unlist(lapply(ch$x$data, function(t) t$x))), -4)
  ev <- multiplexEvolution(mc, years = c(2012, 2016, 2018), min.docs = 3)
  expect_s3_class(multiplexPlot(ev, "trajectory"), "ggplot")
  # no animation: each pair is a path with an arrow at its last period, plus
  # its first period in the same legend group
  tp <- plotly::plotly_build(multiplexPlot(ev, "trajectory", interactive = TRUE))
  expect_null(tp$x$frames)
  paths <- Filter(function(t) !isFALSE(t$showlegend), tp$x$data)
  expect_true(all(vapply(paths, function(t) utils::tail(t$marker$symbol, 1) == "arrow", logical(1))))
  expect_equal(length(tp$x$data), 2 * length(paths))
  # the pairs that change area with a trend come first, those followed in more
  # periods before the others; the trajectories are those pairs
  tr <- multiplexPairs(ev)
  expect_equal(tr$id, seq_len(nrow(tr)))
  drawn <- tr$moves & tr$trend != "stable"
  expect_false(is.unsorted(!drawn))
  expect_false(is.unsorted(rev(tr$periods[drawn])))
  expect_equal(tr$moves, grepl("->", tr$areas))
  expect_length(paths, min(12, sum(drawn)))
  expect_equal(mpArea(c(1, -1, 1, -1), c(1, 1, -1, -1)),
               c("close in both", "close in topics only", "close in references only", "close in neither"))
  # the animation: a frame per period of the pair, the trail growing by one point
  an <- plotly::plotly_build(multiplexPlot(ev, "animation", pair = 1))
  d <- ev$long[ev$long$A == tr$A[1] & ev$long$B == tr$B[1], ]
  expect_length(an$x$frames, nrow(d))
  last <- an$x$frames[[nrow(d)]]$data[[1]]
  expect_equal(as.numeric(last$x), d$x[order(d$period)])
  expect_error(multiplexPlot(ev, "animation", pair = nrow(tr) + 1), "between 1 and")
  expect_error(multiplexPlot(mc, "trajectory"), "multiplexEvolution")
})

test_that("title terms are adjacent words of the same segment, without generic words", {
  # "analysis" is a generic title word: no bigram with it
  expect_false("co-citation analysis" %in% mpTitleTerms("Author co-citation analysis"))
  tt <- mpTitleTerms("Forty years of the Journal of Business & Industrial Marketing: past research")
  expect_false("journal business" %in% tt)
  expect_false("business industrial" %in% tt)
  expect_false("marketing past" %in% tt)
  expect_false("forty" %in% tt)
  expect_true("industrial marketing" %in% tt)
  expect_true("absorptive capacity" %in% mpTitleTerms("Absorptive capacity: a new perspective on learning"))
})

test_that("Scopus references give their title, WoS references none", {
  x <- c("Small, H., Co-citation in the scientific literature: A new measure (1973) J Am Soc Inf Sci, 24, pp. 265-269",
         "Zupic, I., Cater, T., Bibliometric methods in management and organization (2015) Organ Res Methods, 18, pp. 429-472",
         "SMALL H, 1973, J AM SOC INFORM SCI, V24, P265, DOI 10.1002/asi.4630240406")
  t <- mpScopusTitle(x)
  expect_equal(t[1], "Co-citation in the scientific literature: A new measure")
  expect_equal(t[2], "Bibliometric methods in management and organization")
  expect_true(is.na(t[3]))
})

test_that("the reference index keeps DOIs and the titles of the collection", {
  M <- mk(
    CR = c(paste("SMITH J, 2001, J INFORMETR, V1, P1, DOI 10.1000/ABC;", refs(2)),
           paste("SMITH J, 2001, J INFORMETR, V1, P1, DOI 10.1000/abc;", refs(3))),
    DE = c("AA", "BB")
  )
  M$DI <- c("10.1000/abc", NA)
  M$TI <- c("A cited paper", "Another")
  key <- .normalize_cr("SMITH J, 2001, J INFORMETR, V1, P1, DOI 10.1000/ABC")
  idx <- mpReferenceIndex(M, M, key)
  expect_equal(idx$doi, "10.1000/abc")
  expect_equal(idx$title_local, "A cited paper")
})

test_that("Web of Science placeholders are not cited works", {
  M <- mk(
    CR = c(paste("NO TITLE CAPTURED;", refs(1, 2)), paste("NO TITLE CAPTURED;", refs(1, 3)),
           paste("[ANONYMOUS], 2001, J X, V1, P1;", refs(2, 3)), paste("[ANONYMOUS], 2001, J X, V1, P1;", refs(1))),
    DE = c("AA;BB", "AA", "BB", "AA")
  )
  L <- mpRootsLayer(M)
  expect_false(any(grepl("NO TITLE|ANONYMOUS", colnames(L$inc))))
})

test_that("OpenAlex titles come from the cache when they are there", {
  assign("doi:10.9999/cached", "A cached title", envir = .mpCache)
  testthat::local_mocked_bindings(oa_fetch = function(...) stop("no network in this test"), .package = "openalexR")
  got <- mpFetchTitles("10.9999/cached", character(0), "x@y.z", "key", verbose = FALSE)
  expect_equal(unname(got), "A cached title")
  expect_equal(length(mpFetchTitles(character(0), character(0), "x@y.z", "key", verbose = FALSE)), 0)
  rm("doi:10.9999/cached", envir = .mpCache)
})

test_that("roots are named after their references, offline too", {
  skip_if_not_installed("bibliometrixData")
  mc <- mcFixture()
  testthat::local_mocked_bindings(mpOpenAlexReady = function(...) list(ok = FALSE, reason = "offline test"))
  cl <- suppressMessages(multiplexClusters(mc, openalex = FALSE))$clusters
  expect_true(all(nzchar(cl$roots$terms)))
  expect_true(all(nzchar(cl$roots$doc_terms)))
  expect_true(all(grepl("^titles of|^cited sources", cl$roots$label_source)))
  expect_false(any(grepl("OpenAlex", cl$roots$label_source)))
})

test_that("with OpenAlex configured, roots are named from the titles of their references", {
  # opt-in: no download from OpenAlex in R CMD check
  skip_if_not(identical(Sys.getenv("BIBLIOMETRIX_TEST_OPENALEX"), "true"), "OpenAlex tests are opt-in")
  skip_if_offline("api.openalex.org")
  skip_if_not_installed("bibliometrixData")
  skip_if(!isTRUE(mpOpenAlexReady()$ok), "no OpenAlex API key and email configured")
  cl <- suppressMessages(multiplexClusters(mcFixture(), openalex = TRUE))$clusters
  expect_true(any(grepl("OpenAlex", cl$roots$label_source)))
})

test_that("terms of one character, digits or punctuation are not topics", {
  M <- mk(CR = c(refs(1, 2), refs(1, 2), refs(1, 3)), DE = c("INDUSTRY 4;0;5G;C", "INDUSTRY 4;0;5G;C", "4.0;COVID-19"))
  L <- mpTopicLayer(M, "DE", min.freq = 1)
  expect_false(any(c("0", "4.0", "C") %in% colnames(L$inc)))
  expect_true(all(c("INDUSTRY 4", "5G", "COVID-19") %in% colnames(L$inc)))
})

test_that("synonyms and terms to remove apply to the keywords of the topic layer", {
  M <- mk(
    CR = c(refs(1, 2), refs(1, 2), refs(2, 3), refs(2, 3)),
    DE = c("NETWORKS;CO-CITATION;GENERIC", "NETWORK;COCITATION;GENERIC",
           "NETWORK;CO-CITATION", "NETWORKS;COCITATION;GENERIC")
  )
  plain <- multiplexCoupling(M, n = NULL, topic.field = "DE", n.perm = 9, verbose = FALSE)
  clean <- multiplexCoupling(M, n = NULL, topic.field = "DE", n.perm = 9, verbose = FALSE,
                             synonyms = c("NETWORK;NETWORKS", "CO-CITATION;COCITATION"),
                             remove.terms = "GENERIC")
  expect_setequal(colnames(plain$X_T), c("CO-CITATION", "COCITATION", "GENERIC", "NETWORK", "NETWORKS"))
  expect_setequal(colnames(clean$X_T), c("CO-CITATION", "NETWORK"))
  # merged before removal: removing a head removes its synonyms too
  gone <- multiplexCoupling(M, n = NULL, topic.field = "DE", n.perm = 9, verbose = FALSE,
                            synonyms = "NETWORK;NETWORKS", remove.terms = "network")
  expect_false(any(c("NETWORK", "NETWORKS") %in% colnames(gone$X_T)))
})

test_that("synonyms are merged before terms are removed in titles too", {
  M <- mk(CR = c(refs(1, 2), refs(1, 2), refs(2, 3), refs(2, 3)), DE = "AA")
  M$TI <- c("networks of citations", "network of citations",
            "networks and citations", "network and citations")
  terms <- function(...) colnames(mpTopicLayer(M, "TI", min.freq = 1, ...)$inc)
  expect_true(all(c("NETWORK", "NETWORKS") %in% terms()))
  expect_false("NETWORKS" %in% terms(synonyms = "NETWORK;NETWORKS"))
  expect_false(any(c("NETWORK", "NETWORKS") %in%
                     terms(synonyms = "NETWORK;NETWORKS", remove.terms = "network")))
})

test_that("the consensus reports its iterations and whether the runs agreed", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  cl <- mcFixture()$clusters
  expect_s3_class(cl$consensus, "data.frame")
  expect_equal(cl$consensus$layer, c("roots", "themes"))
  expect_true(all(cl$consensus$iterations >= 1 & cl$consensus$iterations <= 10))
  expect_type(cl$consensus$converged, "logical")
})

test_that("the QAP test of the association uses n.perm permutations", {
  M <- mk(
    CR = c(refs(1, 2), refs(1, 2), refs(2, 3), refs(2, 3), refs(3, 4), refs(3, 4)),
    DE = c("AA;BB", "AA;BB", "BB;CC", "BB;CC", "CC;DD", "CC;DD")
  )
  mc <- multiplexCoupling(M, n = NULL, topic.field = "DE", n.perm = 9, verbose = FALSE)
  # p = (1 + #{|rho_p| >= |rho|}) / (n.perm + 1): a multiple of 1/10
  p <- mc$info$association$qap_p
  expect_true(is.na(p) || abs(p * 10 - round(p * 10)) < 1e-9)
})

test_that("multiplexRobustness reports the persistence of links, roots and themes across k", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  mc <- mcFixture()
  rb <- multiplexRobustness(mc, k = c(5, 10, 20), verbose = FALSE)
  cl <- rb$clusters
  # the k of mc is left out
  expect_equal(cl$robustness$k, c(5, 20))
  expect_equal(nrow(cl$robustness$summary), 3)
  expect_equal(dim(cl$robustness$links), c(nrow(cl$links), 2))
  for (x in list(cl$links, cl$roots, cl$themes)) {
    expect_true(all(x$persistence >= 0 & x$persistence <= 1))
  }
  # nothing else changes
  expect_equal(cl$membership, mc$clusters$membership)
  expect_equal(cl$links[, names(mc$clusters$links)], mc$clusters$links)
  expect_error(multiplexRobustness(mc, k = 10), "no value of k")
  expect_output(print(rb), "Robustness to k")
  expect_s3_class(multiplexPlot(rb, "links"), "ggplot")
})

test_that("the roots and themes of multiplexRobustness are those of a new analysis with that k", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  data(management, package = "bibliometrixData")
  mc20 <- multiplexClusters(multiplexCoupling(management, n = 300, n.perm = 19, k = 20, verbose = FALSE),
                            verbose = FALSE, openalex = FALSE)
  ER <- mpKnn(mcFixture()$X_R, "cosine", 20)
  gR <- mpEdgeGraph(mcFixture()$nodes$node, ER, ER$s)
  mR <- mpDropSmall(mpClusterLayer(gR, "louvain", 1, 1234L, 20L)$membership, 5)
  expect_identical(mR, mc20$clusters$membership$root)
})

test_that("greyscale = TRUE adds shapes, line types and link outlines to the static plots", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  mc <- mcFixture()
  for (tp in c("matrix", "links", "clusters")) {
    expect_s3_class(multiplexPlot(mc, tp, greyscale = TRUE), "ggplot")
  }
  g <- multiplexPlot(mc, "links", greyscale = TRUE)
  expect_false(is.null(ggplot2::ggplot_build(g)$plot$scales$get_scales("shape")))
  m <- multiplexPlot(mc, "matrix", greyscale = TRUE)
  expect_match(m$labels$subtitle, "outlined = link")
  expect_no_match(multiplexPlot(mc, "matrix")$labels$subtitle, "outlined")
})

test_that("the title of a Scopus reference is found in both export formats", {
  x <- c("RIBEIRO M.T., SINGH S., GUESTRIN C., WHY SHOULD I TRUST YOU?: EXPLAINING THE PREDICTIONS OF ANY CLASSIFIER, PROC. 22ND ACM SIGKDD INTERNATIONAL CONFERENCE, PP. 1135-1144, (2016)",
         "Ribeiro, M.T., Singh, S., Guestrin, C., Why should I trust you? Explaining the predictions of any classifier (2016) Proc. KDD",
         "ARRIETA A.B., DIAZ-RODRIGUEZ N., EXPLAINABLE AI: CONCEPTS, TAXONOMIES AND CHALLENGES, INF. FUSION, 58, PP. 82-115, (2020)",
         "RIBEIRO M.T., SINGH S., GUESTRIN C. (2016) PP. 1135-1144",
         NA)
  t <- mpScopusTitle(x)
  expect_equal(t[1], "WHY SHOULD I TRUST YOU?: EXPLAINING THE PREDICTIONS OF ANY CLASSIFIER")
  expect_equal(t[2], "Why should I trust you? Explaining the predictions of any classifier")
  expect_equal(t[3], "EXPLAINABLE AI: CONCEPTS, TAXONOMIES AND CHALLENGES")
  expect_true(is.na(t[4]))
  expect_true(is.na(t[5]))
})

test_that("a slow OpenAlex stops the download of the titles at the timeout", {
  # every request fails after 0.2 s: without the timeout the 120 keys would
  # take 3 batches plus 120 single requests, about 25 s
  testthat::local_mocked_bindings(oa_fetch = function(...) {
    Sys.sleep(0.2)
    stop("no answer")
  }, .package = "openalexR")
  doi <- sprintf("10.9999/slow%03d", 1:120)
  t <- system.time(got <- mpFetchTitles(doi, character(0), "x@y.z", "key", verbose = FALSE, timeout = 1))
  expect_lt(t[["elapsed"]], 5)
  expect_true(isTRUE(attr(got, "timed_out")))
  expect_true(all(is.na(got)))
  # the keys not reached stay out of the cache, so a later call asks for them again
  expect_false(all(mpCacheHas(paste0("doi:", doi))))
  rm(list = intersect(ls(.mpCache), paste0("doi:", doi)), envir = .mpCache)
})

test_that("multiplexClusters warns and names the roots when OpenAlex times out", {
  skip_if_not_installed("bibliometrixData")
  mc <- mcFixture()
  testthat::local_mocked_bindings(mpOpenAlexReady = function(...) list(ok = TRUE, email = "x@y.z", api.key = "key", reason = ""))
  testthat::local_mocked_bindings(mpFetchTitles = function(doi, oaid, ...) {
    ck <- c(if (length(doi)) paste0("doi:", doi), if (length(oaid)) paste0("id:", oaid))
    structure(stats::setNames(rep(NA_character_, length(ck)), ck), timed_out = TRUE)
  })
  expect_warning(cl <- multiplexClusters(mc, verbose = FALSE, openalex.timeout = 1)$clusters, "stopped after 1 s")
  expect_true(isTRUE(attr(cl$openalex, "timed_out")))
  expect_true(all(nzchar(cl$roots$terms)))
})
