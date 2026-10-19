test_that("KeywordGrowth merges synonyms and removes terms as tableTag", {
  skip_if_not_installed("bibliometrixData")
  data(management, package = "bibliometrixData", envir = environment())
  syn <- c("bibliometrics;bibliometric analysis;bibliometric study",
           "citation analysis;citations;citation;co-citation analysis",
           "innovation;innovations;open innovation")
  rt <- c("bibliometrics", "literature review")
  w <- KeywordGrowth(management, Tag = "DE", top = 10, remove.terms = rt, synonyms = syn)
  tt <- tableTag(management, "DE", remove.terms = rt, synonyms = syn)
  expect_setequal(names(w)[-1], names(tt)[1:10])
  # a stopword stays out when a group merges other terms into it
  expect_false("BIBLIOMETRICS" %in% names(w))
  # cumulative counts never decrease
  expect_true(all(diff(as.matrix(w[, -1])) >= 0))
  # a document counts once for a term, also when it held two of its synonyms
  hit <- vapply(strsplit(toupper(management$DE), ";"), function(k)
    any(trimws(k) %in% c("CITATION ANALYSIS", "CITATIONS", "CITATION", "CO-CITATION ANALYSIS")),
    logical(1))
  expect_equal(max(w[["CITATION ANALYSIS"]]), sum(hit & !is.na(management$PY)))
})

test_that("KeywordGrowth survives a group without synonyms and a short vocabulary", {
  skip_if_not_installed("bibliometrixData")
  data(management, package = "bibliometrixData", envir = environment())
  # a line with no ";" gave the empty regex, which turned every term into NA
  w <- KeywordGrowth(management, Tag = "DE", top = 10,
                     synonyms = "bibliometrics, bibliometric analysis, bibliometric study")
  expect_equal(ncol(w), 11)
  expect_false(anyNA(names(w)))
  # fewer distinct terms than top
  w3 <- KeywordGrowth(management[1:3, ], Tag = "DE", top = 50)
  expect_false(anyNA(names(w3)))
  expect_lt(ncol(w3), 51)
})
