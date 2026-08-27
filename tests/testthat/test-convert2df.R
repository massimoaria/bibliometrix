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

test_that("normalizeCRisi ricompone l'autore citato scritto con la virgola", {
  # "Cognome, Iniziali, Anno" sfasa di uno tutti i campi del riferimento
  expect_equal(
    normalizeCRisi("AAKER, JL, 1997, J MARKETING RES, V34, P347, DOI 10.2307/3151897"),
    "AAKER JL, 1997, J MARKETING RES, V34, P347, DOI 10.2307/3151897"
  )

  # la forma classica resta invariata
  classic <- "ARIA M, 2017, J INFORMETR, V11, P959, DOI 10.1016/J.JOI.2017.08.007"
  expect_equal(normalizeCRisi(classic), classic)

  # "[ANONYMOUS], 1996, ..." ha gia' l'anno in posizione 2: non va toccato
  anon <- "[ANONYMOUS], 1996, WHOS TOPS WHO DECIDE"
  expect_equal(normalizeCRisi(anon), anon)

  # piu' riferimenti separati da ";" nella stessa stringa
  expect_equal(
    normalizeCRisi("AAKER, JL, 1997, J MARKETING RES;ARIA M, 2017, J INFORMETR;BALMER, JMT, 1991, J GEN MANAG"),
    "AAKER JL, 1997, J MARKETING RES;ARIA M, 2017, J INFORMETR;BALMER JMT, 1991, J GEN MANAG"
  )

  # riferimento senza fonte, l'anno chiude la stringa
  expect_equal(normalizeCRisi("SMITH, JK, 2001"), "SMITH JK, 2001")
})

test_that("normalizeCRisi rimuove il tag DOI ripetuto", {
  expect_equal(
    normalizeCRisi("ABRATT R., 1989, J MARKETING MANAG, V5, P63, DOI DOI 10.1080/X"),
    "ABRATT R., 1989, J MARKETING MANAG, V5, P63, DOI 10.1080/X"
  )

  # forma multi-DOI tra parentesi: le parentesi vengono tolte dopo, in
  # convert2df(), quindi la duplicazione va sciolta qui
  expect_equal(
    normalizeCRisi("BOCK RD, 1997, ED MEAS, DOI [DOI 10.1111/X, 10.1111/x]"),
    "BOCK RD, 1997, ED MEAS, DOI [10.1111/X, 10.1111/x]"
  )
})

test_that("convert2df normalizza i riferimenti dei nuovi export WoS", {
  M <- load_wos_newformat_fixture()
  expect_equal(nrow(M), 3)

  refs <- trimws(unlist(strsplit(M$CR, ";")))
  # l'anno torna in seconda posizione in tutti i riferimenti
  f2 <- trimws(unlist(lapply(strsplit(refs, ",", fixed = TRUE), "[", 2)))
  expect_true(all(grepl("^(19|20)[0-9]{2}$", f2)))
  expect_false(any(grepl("DOI DOI", refs)))
})
