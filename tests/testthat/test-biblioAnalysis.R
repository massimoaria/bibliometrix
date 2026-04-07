# Test per biblioAnalysis, summary e plot

test_that("biblioAnalysis restituisce oggetto bibliometrix", {
  M <- load_wos_fixture()
  res <- biblioAnalysis(M)
  expect_s3_class(res, "bibliometrix")
  expect_true(res$Articles > 0)
  expect_true(res$nAuthors > 0)
  expect_true(is.numeric(res$Years))
})

test_that("biblioAnalysis contiene tutti i componenti attesi", {
  M <- load_wos_fixture()
  res <- biblioAnalysis(M)
  expected <- c("Articles", "Authors", "AuthorsFrac", "FirstAuthors",
                "nAUperPaper", "Appearances", "nAuthors", "MostCitedPapers",
                "Years", "Sources", "DE", "ID")
  expect_true(all(expected %in% names(res)))
})

test_that("biblioAnalysis funziona con dati Scopus", {
  M <- load_scopus_fixture()
  res <- biblioAnalysis(M)
  expect_s3_class(res, "bibliometrix")
  expect_equal(res$Articles, nrow(M))
})

test_that("biblioAnalysis funziona con dati OpenAlex", {
  M <- load_openalex_fixture()
  res <- biblioAnalysis(M)
  expect_s3_class(res, "bibliometrix")
  expect_equal(res$Articles, nrow(M))
})

test_that("summary.bibliometrix funziona senza errori", {
  M <- load_wos_fixture()
  res <- biblioAnalysis(M)
  expect_no_error(
    capture.output(summary(res, k = 5, pause = FALSE, verbose = FALSE))
  )
})

test_that("plot.bibliometrix funziona senza errori", {
  M <- load_wos_fixture()
  res <- biblioAnalysis(M)
  expect_no_error(plot(res, k = 5, pause = FALSE))
})
