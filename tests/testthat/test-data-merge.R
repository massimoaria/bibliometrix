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

test_that("mergeDbSources segnala lo svuotamento di CR tra database diversi", {
  M_wos <- load_wos_fixture()
  M_scopus <- load_scopus_fixture()

  out <- capture.output(
    M_merged <- suppressWarnings(suppressMessages(
      mergeDbSources(M_wos, M_scopus, remove.duplicated = TRUE, verbose = TRUE)
    ))
  )

  # i riferimenti originali restano in CR_raw, CR e' svuotato
  expect_true("CR_raw" %in% names(M_merged))
  expect_true(all(M_merged$CR == "NA"))
  expect_true(mean(nchar(M_merged$CR_raw), na.rm = TRUE) > 100)

  # l'utente viene avvisato, con il nome del campo di ripiego
  msg <- paste(out, collapse = " ")
  expect_match(msg, "CR has been emptied")
  expect_match(msg, "CR_raw")

  # niente messaggio con verbose = FALSE
  quiet <- capture.output(
    M_quiet <- suppressWarnings(suppressMessages(
      mergeDbSources(M_wos, M_scopus, remove.duplicated = TRUE, verbose = FALSE)
    ))
  )
  expect_false(any(grepl("CR_raw", quiet)))
})

test_that("mergeDbSources non tocca CR con un solo database", {
  M_wos <- load_wos_fixture()
  M_bis <- M_wos
  M_bis$TI <- paste("X", M_bis$TI)
  M_bis$DI <- paste0(M_bis$DI, "-x")

  out <- capture.output(
    M_merged <- suppressWarnings(suppressMessages(
      mergeDbSources(M_wos, M_bis, remove.duplicated = TRUE, verbose = TRUE)
    ))
  )

  expect_false("CR_raw" %in% names(M_merged))
  expect_true(all(nchar(M_merged$CR) > 10))
  expect_false(any(grepl("CR has been emptied", out)))
})
