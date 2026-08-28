# Test per missingData, metaTagExtraction, normalizeCitationScore

test_that("missingData restituisce informazioni sulla completezza", {
  M <- load_wos_fixture()
  res <- missingData(M)
  expect_type(res, "list")
  expect_true(all(c("allTags", "mandatoryTags") %in% names(res)))
  expect_true(is.data.frame(res$allTags))
  expect_true(is.data.frame(res$mandatoryTags))
  expect_true("status" %in% names(res$mandatoryTags))
  valid_statuses <- c("Excellent", "Good", "Acceptable", "Poor", "Critical", "Completely missing")
  expect_true(all(res$mandatoryTags$status %in% valid_statuses))
})

test_that("missingData funziona con dati Scopus", {
  M <- load_scopus_fixture()
  res <- missingData(M)
  expect_type(res, "list")
  expect_true(is.data.frame(res$mandatoryTags))
})

test_that("metaTagExtraction estrae CR_AU", {
  M <- load_wos_fixture()
  M2 <- metaTagExtraction(M, Field = "CR_AU", sep = ";")
  expect_true("CR_AU" %in% names(M2))
})

test_that("metaTagExtraction estrae CR_SO", {
  M <- load_wos_fixture()
  M2 <- metaTagExtraction(M, Field = "CR_SO", sep = ";")
  expect_true("CR_SO" %in% names(M2))
})

test_that("metaTagExtraction estrae AU_CO (paese autore)", {
  M <- load_wos_fixture()
  M2 <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
  expect_true("AU_CO" %in% names(M2))
})

test_that("metaTagExtraction estrae AU1_CO (paese primo autore)", {
  M <- load_wos_fixture()
  M2 <- metaTagExtraction(M, Field = "AU1_CO", sep = ";")
  expect_true("AU1_CO" %in% names(M2))
})

test_that("metaTagExtraction estrae SR (short reference)", {
  M <- load_wos_fixture()
  M2 <- suppressWarnings(metaTagExtraction(M, Field = "SR", sep = ";"))
  expect_true("SR" %in% names(M2))
})

test_that("normalizeCitationScore calcola NCS", {
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  NCS <- expect_no_error(
    suppressWarnings(suppressMessages(
      normalizeCitationScore(scientometrics, field = "documents", impact.measure = "global")
    ))
  )
  expect_true(is.data.frame(NCS))
})

test_that("metaTagExtraction AU1_CO regge una collezione senza affiliazioni", {
  # Le esportazioni Lens.org non contengono mai le affiliazioni: ne' C1 ne' RP.
  M <- load_lens_fixture()
  expect_false("C1" %in% names(M))
  expect_false("RP" %in% names(M))

  M2 <- expect_no_error(suppressWarnings(suppressMessages(
    metaTagExtraction(M, "AU1_CO", ";")
  )))
  expect_true("AU1_CO" %in% names(M2))
  expect_true(all(is.na(M2$AU1_CO)))
  expect_equal(nrow(M2), nrow(M))
})

test_that("metaTagExtraction AU1_CO non cambia con le affiliazioni presenti", {
  M <- load_wos_fixture()
  M2 <- suppressWarnings(suppressMessages(metaTagExtraction(M, "AU1_CO", ";")))
  expect_true(sum(!is.na(M2$AU1_CO)) == nrow(M2))
})
