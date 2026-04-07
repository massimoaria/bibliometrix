# Test per termExtraction, tableTag, keywordAssoc, mergeKeywords

test_that("termExtraction estrae termini dal titolo", {
  M <- load_wos_fixture()
  M2 <- suppressWarnings(suppressMessages(
    termExtraction(M, Field = "TI", ngrams = 1, verbose = FALSE)
  ))
  expect_true("TI_TM" %in% names(M2))
  expect_true(any(!is.na(M2$TI_TM)))
})

test_that("termExtraction con stemming funziona", {
  M <- load_wos_fixture()
  M2 <- expect_no_error(
    suppressWarnings(suppressMessages(
      termExtraction(M, Field = "TI", ngrams = 1, stemming = TRUE, verbose = FALSE)
    ))
  )
  expect_true("TI_TM" %in% names(M2))
})

test_that("termExtraction con ngrams=2 funziona", {
  M <- load_wos_fixture()
  M2 <- expect_no_error(
    suppressWarnings(suppressMessages(
      termExtraction(M, Field = "TI", ngrams = 2, verbose = FALSE)
    ))
  )
  expect_true("TI_TM" %in% names(M2))
})

test_that("tableTag tabula frequenze per autori", {
  M <- load_wos_fixture()
  tab <- tableTag(M, Tag = "AU")
  expect_true(is.data.frame(tab) || inherits(tab, "table"))
  expect_true(length(tab) > 0)
})

test_that("tableTag tabula frequenze per DE e ID", {
  M <- load_wos_fixture()
  tab_de <- tableTag(M, Tag = "DE")
  tab_id <- tableTag(M, Tag = "ID")
  expect_true(length(tab_de) > 0)
  expect_true(length(tab_id) > 0)
})

test_that("keywordAssoc restituisce lista di associazioni", {
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  KW <- expect_no_error(keywordAssoc(scientometrics, sep = ";", n = 5))
  expect_type(KW, "list")
})

test_that("mergeKeywords aggiunge campo KW_Merged", {
  M <- load_wos_fixture()
  M2 <- M
  M2$KW_Merged <- NULL
  M3 <- mergeKeywords(M2)
  expect_true("KW_Merged" %in% names(M3))
})
