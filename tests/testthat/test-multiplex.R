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
      mc <<- multiplexClusters(multiplexCoupling(management, n = 300, n.perm = 19, verbose = FALSE))
    }
    mc
  }
})

test_that("the roots layer reproduces the coupling indices of normalizeSimilarity()", {
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
    DE = c("A;B", "A;C", "B;C", "A;B", NA, "A;B;C")
  )
  mc <- multiplexCoupling(M, n = NULL, k = 2, n.sample = 100, n.perm = 9, verbose = FALSE)
  expect_equal(mc$info$dropped[["no_roots"]], 1)
  expect_equal(mc$info$dropped[["no_topic"]], 1)
  expect_setequal(mc$nodes$node, c("D1", "D2", "D3", "D6"))
})

test_that("collections that cannot be coupled are refused", {
  M <- mk(CR = c(refs(1, 2), refs(1, 2), NA), DE = c("A", "A", "A"))
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
    DE = c("A;B", "A;C", "B;C", "A;B", "A;B;C", "C;D", "B;D")
  )
  set.seed(42)
  expected <- stats::runif(1)
  set.seed(42)
  a <- multiplexCoupling(M, n = NULL, k = 3, n.sample = 100, n.perm = 9, verbose = FALSE)
  a <- multiplexClusters(a)
  expect_equal(stats::runif(1), expected)
  b <- multiplexClusters(multiplexCoupling(M, n = NULL, k = 3, n.sample = 100, n.perm = 9, verbose = FALSE))
  expect_identical(a$pairs, b$pairs)
  expect_identical(a$clusters$membership, b$clusters$membership)
})

test_that("population percentiles use mid-ranks", {
  expect_equal(mpPercentile(c(0, 1, 5), c(0, 0, 1, 2)), c(0.25, 0.625, 1))
})

test_that("topic fields are merged by term", {
  M <- mk(CR = c(refs(1, 2), refs(1, 2), refs(1, 3)), DE = c("A;B", "A", "B"))
  M$ID <- c("A", "C", "C")
  L <- mpTopicLayer(M, c("DE", "ID"), min.freq = 1)
  expect_setequal(colnames(L$inc), c("A", "B", "C"))
  expect_equal(as.numeric(L$inc["D1", "A"]), 1)
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
})

test_that("standardized residuals match chisq.test()", {
  tab <- matrix(c(20, 5, 3, 7, 18, 4, 2, 6, 25), 3)
  expect_equal(mpStandardizedResiduals(tab), suppressWarnings(stats::chisq.test(tab)$stdres),
               ignore_attr = TRUE)
})

test_that("schools and themes are classified from one set of links", {
  skip_if_not_installed("bibliometrixData")
  mc <- mcFixture()
  cl <- mc$clusters
  expect_equal(sum(cl$contingency), nrow(mc$nodes))
  expect_equal(nrow(cl$schools), max(cl$membership$school))
  expect_true(all(cl$schools$size == tabulate(cl$membership$school)))
  expect_true(all(nzchar(cl$schools$terms)))
  # the links counted by the schools and by the themes are the same
  expect_equal(cl$schools$n_themes, tabulate(cl$links$school, nrow(cl$schools)))
  expect_equal(cl$themes$n_schools, tabulate(cl$links$theme, nrow(cl$themes)))
  expect_true(all(cl$links$residual > 2 & cl$links$n >= 5))
  expect_equal(cl$schools$structure == "branching", cl$schools$n_themes >= 2)
  expect_equal(cl$themes$structure == "convergence", cl$themes$n_schools >= 2)
  expect_equal(nrow(cl$plane), choose(nrow(cl$schools), 2))
  expect_true(all(cl$agreement > 0 & cl$agreement <= 1))
  expect_output(print(mc), "Schools:")
})

test_that("a stricter link rule finds no more links", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  mc <- mcFixture()
  strict <- multiplexClusters(mc, min.link = 10)$clusters$links
  expect_lte(nrow(strict), nrow(mc$clusters$links))
  expect_true(all(strict$n >= 10))
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

test_that("multiplexEvolution follows pairs of schools and classifies their trend", {
  skip_if_not_installed("bibliometrixData")
  mc <- mcFixture()
  ev <- multiplexEvolution(mc, years = c(2012, 2016, 2018), min.docs = 3)
  expect_s3_class(ev, "biblioMultiplexEvolution")
  expect_true(all(ev$long$A < ev$long$B))
  expect_true(all(ev$trajectories$periods >= 2))
  expect_true(all(ev$trajectories$trend %in%
                    c("converging", "diverging", "drifting apart", "consolidating", "stable")))
  cv <- ev$trajectories[ev$trajectories$trend == "converging", ]
  if (nrow(cv)) expect_true(all(cv$slope_T >= 0.1 & cv$slope_R < 0.1))
  expect_output(print(ev), "Multiplex evolution")
  expect_error(multiplexEvolution(multiplexCoupling(mk(
    CR = c(refs(1, 2), refs(1, 2), refs(1, 2)), DE = c("A", "A", "A")
  ), n.perm = 9, n.sample = 10, verbose = FALSE)), "multiplexClusters")
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
  ev <- multiplexEvolution(mc, years = c(2012, 2016, 2018), min.docs = 3)
  expect_s3_class(multiplexPlot(ev, "trajectory"), "ggplot")
  expect_s3_class(multiplexPlot(ev, "trajectory", interactive = TRUE), "plotly")
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
    DE = c("A", "B")
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
    DE = c("A;B", "A", "B", "A")
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

test_that("schools are named after their roots, offline too", {
  skip_if_not_installed("bibliometrixData")
  mc <- mcFixture()
  testthat::local_mocked_bindings(mpOpenAlexReady = function(...) list(ok = FALSE, reason = "offline test"))
  cl <- suppressMessages(multiplexClusters(mc))$clusters
  expect_true(all(nzchar(cl$schools$terms)))
  expect_true(all(nzchar(cl$schools$doc_terms)))
  expect_true(all(grepl("^titles of|^cited sources", cl$schools$label_source)))
  expect_false(any(grepl("OpenAlex", cl$schools$label_source)))
})

test_that("with OpenAlex configured, schools are named from the titles of their references", {
  skip_on_cran()
  skip_if_offline("api.openalex.org")
  skip_if_not_installed("bibliometrixData")
  skip_if(!isTRUE(mpOpenAlexReady()$ok), "no OpenAlex API key and email configured")
  cl <- suppressMessages(multiplexClusters(mcFixture()))$clusters
  expect_true(any(grepl("OpenAlex", cl$schools$label_source)))
})
