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

test_that("metaTagExtraction AU_UN non scivola sulla citta' quando l'indirizzo ha numeri civici", {
  # Le parti dell'indirizzo che contengono cifre (numero civico, casella postale,
  # CAP) vanno scartate, ma gli indici della parte con il tag restano riferiti
  # alla stringa intera: prima l'affiliazione slittava sul frammento successivo.
  C1 <- c(
    "3400 SPRUCE ST, UNIV PENN, PHILADELPHIA, PA USA",
    "PO BOX 100, UNIV OSLO, OSLO, NORWAY",
    "BLDG 10 ROOM 2C146, NATL CANCER INST, BETHESDA, MD USA",
    "UNIV PENN, PHILADELPHIA, PA USA"
  )
  M <- data.frame(
    AU = paste0("AUTHOR A", seq_along(C1)),
    C1 = C1,
    RP = NA_character_,
    DB = "ISI",
    stringsAsFactors = FALSE
  )
  class(M) <- c("bibliometrixDB", "data.frame")

  M2 <- suppressWarnings(suppressMessages(metaTagExtraction(M, "AU_UN", ";")))
  expect_equal(
    M2$AU_UN,
    c("UNIV PENN", "UNIV OSLO", "NATL CANCER INST", "UNIV PENN")
  )
})

test_that("metaTagExtraction AU_UN non lascia i marcatori interni nei risultati", {
  # NOTREPORTED e NOTDECLARED marcano le parti dell'indirizzo in cui nessuna
  # istituzione e' riconoscibile. AU_UN li ripuliva, AU1_UN no: comparivano
  # nelle analisi dell'affiliazione dell'autore corrispondente come se fossero
  # il nome di un ente.
  RP <- c(
    "SMITH, J.; EMAIL: SMITH@EXAMPLE.COM",
    "JONES, A.; UNIV OSLO, OSLO, NORWAY; EMAIL: JONES@EXAMPLE.COM",
    "BROWN, K."
  )
  M <- data.frame(
    AU = paste0("AUTHOR A", seq_along(RP)),
    C1 = RP,
    RP = RP,
    DB = "SCOPUS",
    stringsAsFactors = FALSE
  )
  class(M) <- c("bibliometrixDB", "data.frame")

  M2 <- suppressWarnings(suppressMessages(metaTagExtraction(M, "AU_UN", ";")))
  for (field in c("AU_UN", "AU1_UN")) {
    expect_false(any(grepl("NOTREPORTED|NOTDECLARED", M2[[field]])), info = field)
    expect_false(any(M2[[field]] %in% ""), info = field)
  }
  expect_equal(M2$AU1_UN[2], "UNIV OSLO")
  expect_true(is.na(M2$AU1_UN[3]))
})

test_that("metaTagExtraction AU_UN tiene insieme i nomi di ente che contengono virgole", {
  # "National Heart, Lung, and Blood Institute" veniva spezzato sulle virgole e
  # dell'ente restava il solo pezzo con il tag: "AND BLOOD INSTITUTE" (#431).
  C1 <- c(
    "NATIONAL HEART, LUNG, AND BLOOD INSTITUTE, BETHESDA, MD, USA",
    "DEPT OF MEDICINE, NATIONAL HEART, LUNG, AND BLOOD INSTITUTE, BETHESDA, USA",
    "NATL HEART LUNG AND BLOOD INST, BETHESDA, MD USA"
  )
  M <- data.frame(
    AU = paste0("AUTHOR A", seq_along(C1)),
    C1 = C1,
    RP = NA_character_,
    DB = "ISI",
    stringsAsFactors = FALSE
  )
  class(M) <- c("bibliometrixDB", "data.frame")

  M2 <- suppressWarnings(suppressMessages(metaTagExtraction(M, "AU_UN", ";")))
  expect_equal(
    M2$AU_UN,
    c(
      "NATIONAL HEART LUNG AND BLOOD INSTITUTE",
      "NATIONAL HEART LUNG AND BLOOD INSTITUTE",
      "NATL HEART LUNG AND BLOOD INST"
    )
  )
})

test_that("metaTagExtraction AU_UN non scambia una qualifica per un ente", {
  # I tag delle istituzioni sono prefissi: SCI compare in SCIENTIST, RES in
  # RESEARCHER, INST in INSTRUCTOR. "Nurse Scientist" veniva restituito al posto
  # dell'ospedale che seguiva (#431).
  C1 <- c(
    "NURSE SCIENTIST, CHILDRENS HOSPITAL OF PHILADELPHIA, PHILADELPHIA, PA, USA",
    "DATA SCIENTIST, MAYO CLINIC, ROCHESTER, MN, USA",
    "NURSE SCIENTIST, PHILADELPHIA, PA, USA",
    "COLLEGE OF FAMILY PHYSICIANS, TORONTO, CANADA"
  )
  M <- data.frame(
    AU = paste0("AUTHOR A", seq_along(C1)),
    C1 = C1,
    RP = NA_character_,
    DB = "ISI",
    stringsAsFactors = FALSE
  )
  class(M) <- c("bibliometrixDB", "data.frame")

  M2 <- suppressWarnings(suppressMessages(metaTagExtraction(M, "AU_UN", ";")))
  # l'ultima riga non e' una qualifica ma un ente, e resta com'e'
  expect_equal(
    M2$AU_UN,
    c(
      "CHILDRENS HOSPITAL OF PHILADELPHIA",
      "MAYO CLINIC",
      NA_character_,
      "COLLEGE OF FAMILY PHYSICIANS"
    )
  )
})
