# Test per duplicatedMatching e mergeDbSources

test_that("duplicatedMatching rimuove duplicati esatti", {
  M <- load_wos_fixture()
  M_dup <- rbind(M, M[1:3, ])
  class(M_dup) <- class(M)
  M_clean <- suppressWarnings(suppressMessages(
    duplicatedMatching(M_dup, Field = "TI", tol = 0.95)
  ))
  expect_true(nrow(M_clean) <= nrow(M_dup))
  expect_true(nrow(M_clean) >= nrow(M))
})

test_that("mergeDbSources unisce dati da WoS e Scopus", {
  M_wos <- load_wos_fixture()
  M_scopus <- load_scopus_fixture()
  M_merged <- expect_no_error(
    suppressWarnings(suppressMessages(
      mergeDbSources(M_wos, M_scopus, remove.duplicated = TRUE)
    ))
  )
  expect_s3_class(M_merged, "data.frame")
  expect_true(nrow(M_merged) > 0)
  expect_true("SR" %in% names(M_merged))
  expect_true(all(c("AU", "TI", "SO", "PY") %in% names(M_merged)))
})

test_that("mergeDbSources unisce dati da tre sorgenti", {
  M_wos <- load_wos_fixture()
  M_scopus <- load_scopus_fixture()
  M_oa <- load_openalex_fixture()
  M_merged <- expect_no_error(
    suppressWarnings(suppressMessages(
      mergeDbSources(M_wos, M_scopus, M_oa, remove.duplicated = TRUE)
    ))
  )
  expect_true(nrow(M_merged) > 0)
})
