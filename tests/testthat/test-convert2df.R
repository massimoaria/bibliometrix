# Test per convert2df: import e conversione da diversi database/formati

test_that("convert2df importa WoS plaintext correttamente", {
  M <- load_wos_fixture()
  expect_s3_class(M, "bibliometrixDB")
  expect_s3_class(M, "data.frame")
  expect_equal(nrow(M), 8)
  expect_true(all(c("AU", "TI", "SO", "PY", "TC", "CR", "DB", "SR") %in% names(M)))
  expect_equal(unique(M$DB), "ISI")
  expect_true(is.numeric(M$PY))
  expect_true(is.numeric(M$TC))
})

test_that("convert2df importa Scopus CSV correttamente", {
  M <- load_scopus_fixture()
  expect_s3_class(M, "bibliometrixDB")
  expect_s3_class(M, "data.frame")
  expect_equal(nrow(M), 8)
  expect_true(all(c("AU", "TI", "SO", "PY", "TC", "DB", "SR") %in% names(M)))
  expect_equal(unique(M$DB), "SCOPUS")
  expect_true(is.numeric(M$PY))
  expect_true(is.numeric(M$TC))
})

test_that("convert2df importa OpenAlex CSV correttamente", {
  M <- load_openalex_fixture()
  expect_s3_class(M, "bibliometrixDB")
  expect_s3_class(M, "data.frame")
  expect_equal(nrow(M), 8)
  expect_true(all(c("AU", "TI", "SO", "PY", "TC", "DB", "SR") %in% names(M)))
  expect_equal(unique(M$DB), "OPENALEX")
  expect_true(is.numeric(M$PY))
  expect_true(is.numeric(M$TC))
})

test_that("convert2df importa Lens CSV correttamente", {
  M <- load_lens_fixture()
  expect_s3_class(M, "bibliometrixDB")
  expect_s3_class(M, "data.frame")
  expect_equal(nrow(M), 8)
  expect_true(all(c("AU", "TI", "SO", "PY", "TC", "DB", "SR") %in% names(M)))
  expect_equal(unique(M$DB), "LENS")
  expect_true(is.numeric(M$PY))
  expect_true(is.numeric(M$TC))
})

test_that("convert2df genera SR univoci senza duplicati", {
  M <- load_wos_fixture()
  expect_false(any(duplicated(M$SR)))
})

test_that("convert2df crea campo KW_Merged", {
  M <- load_wos_fixture()
  expect_true("KW_Merged" %in% names(M))
})

test_that("convert2df crea AU_UN per sorgenti con affiliazioni", {
  M <- load_wos_fixture()
  expect_true("AU_UN" %in% names(M))
})

test_that("convert2df segnala dbsource non valido", {
  expect_error(
    suppressWarnings(suppressMessages(
      convert2df("fake.txt", dbsource = "invalid_db", format = "plaintext")
    ))
  )
})
