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

test_that("mergeDbSources conserva tutte le iniziali degli autori", {
  M_wos <- load_wos_fixture()
  M_scopus <- load_scopus_fixture()

  authors <- function(x) unique(trimws(unlist(strsplit(x, ";"))))
  pre <- authors(c(M_wos$AU, M_scopus$AU))
  expect_true(any(grepl("^\\S+ [A-Z]{2,}$", pre))) # il fixture ha nomi con 2+ iniziali

  M_merged <- suppressWarnings(suppressMessages(
    mergeDbSources(M_wos, M_scopus, remove.duplicated = TRUE, verbose = FALSE)
  ))
  post <- authors(M_merged$AU)

  # nessun nome viene riscritto: quelli sopravvissuti erano gia' presenti
  expect_true(all(post %in% pre))
  # e le iniziali multiple non vengono troncate alla prima
  expect_true(any(grepl("^\\S+ [A-Z]{2,}$", post)))
})

test_that("mergeDbSources non riporta i nomi completi a iniziali", {
  # Author Name Format = Fullname: AU contiene i nomi per esteso e le due
  # sorgenti li punteggiano in modo diverso (Scopus con la virgola).
  M_wos <- load_wos_fixture()
  M_scopus <- load_scopus_fixture()
  M_wos$AU <- "SELVANATHAN ELIYATHAMBY A;JAYASINGHE MANEKA"
  M_scopus$AU <- "REID, MONIQUE; DU PLESSIS, STAN"

  M_merged <- suppressWarnings(suppressMessages(
    mergeDbSources(M_wos, M_scopus, remove.duplicated = TRUE, verbose = FALSE)
  ))
  post <- unique(trimws(unlist(strsplit(M_merged$AU, ";"))))

  expect_true("SELVANATHAN ELIYATHAMBY A" %in% post)
  expect_true("JAYASINGHE MANEKA" %in% post)
  # la virgola di Scopus e' armonizzata, il nome resta per esteso
  expect_true("REID MONIQUE" %in% post)
  expect_true("DU PLESSIS STAN" %in% post)
  expect_false(any(grepl(",", post)))
})

test_that("mergeDbSources non lascia campi paese stantii", {
  # Lens porta un AU_CO vuoto, WoS non ha ancora il campo: dopo bind_rows i
  # documenti WoS resterebbero senza paese e MCP verrebbe sottostimato.
  M_wos <- load_wos_fixture()
  M_lens <- load_lens_fixture()
  expect_true("AU_CO" %in% names(M_lens))
  expect_false("AU_CO" %in% names(M_wos))

  M <- suppressWarnings(suppressMessages(
    mergeDbSources(M_wos, M_lens, remove.duplicated = TRUE, verbose = FALSE)
  ))
  expect_false("AU_CO" %in% names(M))
  expect_false("AU1_CO" %in% names(M))

  # ricalcolato dalle affiliazioni, il lato WoS ritrova i suoi paesi
  M2 <- suppressWarnings(suppressMessages(metaTagExtraction(M, "AU_CO", ";")))
  co <- M2$AU_CO[!is.na(M2$AU_CO) & trimws(M2$AU_CO) != "NA"]
  expect_true(length(co) >= nrow(M_wos))
  expect_true(any(grepl(";", co))) # almeno una collaborazione internazionale
})

# ----- PubMed + OpenAlex (Silver et al. 1991, PMID 10171110) ------------------

pm_silver <- function(...) {
  x <- data.frame(
    AU = "SILVER FH;TRIA AJ;ZAWADSKY JP;DUNN MG",
    AF = "SILVER, F H;TRIA, A J;ZAWADSKY, J P;DUNN, M G",
    TI = "ANTERIOR CRUCIATE LIGAMENT REPLACEMENT: A REVIEW.",
    SO = "JOURNAL OF LONG-TERM EFFECTS OF MEDICAL IMPLANTS",
    JI = "J LONG TERM EFF MED IMPLANTS", PY = 1991, DB = "PUBMED", DE = NA_character_,
    ID = "ANTERIOR CRUCIATE LIGAMENT", CR = NA_character_, DI = NA_character_,
    TC = 0, LA = "ENG", DT = "JOURNAL ARTICLE", C1 = "UMD", RP = NA_character_,
    VL = "1", IS = "2", stringsAsFactors = FALSE
  )
  modifyList(x, list(...)) |> as.data.frame()
}
oa_silver <- function(...) {
  au <- "FREDERICK H. SILVER;ALFRED J. TRIA;JOSEPH P. ZAWADSKY;MICHAEL G. DUNN"
  x <- pm_silver(
    AU = au, AF = au, SO = "PUBMED", JI = NA_character_, DB = "OPENALEX",
    DE = "ACL", TC = 42, LA = "EN", DT = "ARTICLE", VL = "1", IS = "2",
    AB = "AN ABSTRACT"
  )
  modifyList(x, list(...)) |> as.data.frame()
}

test_that("un record OpenAlex con PubMed come fonte cede al record della rivista", {
  M <- mergeDbSources(oa_silver(), pm_silver(), verbose = FALSE)
  expect_equal(nrow(M), 1)
  expect_equal(M$DB, "PUBMED")
  expect_equal(M$SO, "JOURNAL OF LONG-TERM EFFECTS OF MEDICAL IMPLANTS")
  expect_equal(M$AU, "SILVER FH;TRIA AJ;ZAWADSKY JP;DUNN MG")
  expect_equal(M$TC, 42) # le citazioni di OpenAlex non si perdono
})

test_that("tra i duplicati resta il record con piu' metadati, a parita' l'ordine dei db", {
  completo <- oa_silver(SO = "A JOURNAL", DE = "ACL", DI = "10.1/x")
  scarso <- pm_silver(ID = NA_character_, C1 = NA_character_, DI = "10.1/x")
  M <- mergeDbSources(completo, scarso, verbose = FALSE)
  expect_equal(M$DB, "OPENALEX")

  # stesso DOI, stessi campi pieni: vince l'ordine dei database (OpenAlex prima)
  a <- oa_silver(SO = "A JOURNAL", AB = NULL)
  b <- pm_silver(SO = "A JOURNAL", DE = "ACL", TC = 42)
  M <- mergeDbSources(b, a, verbose = FALSE)
  expect_equal(nrow(M), 1)
  expect_equal(M$DB, "OPENALEX")
})

test_that("con un solo database resta il primo record, e TC non cambia", {
  a <- pm_silver(TC = 1, AB = NA_character_)
  b <- pm_silver(TC = 9, AB = "AN ABSTRACT")
  M <- mergeDbSources(a, b, verbose = FALSE)
  expect_equal(nrow(M), 1)
  expect_equal(M$TC, 1)
})

test_that("un file OpenAlex salvato con i nomi in ordine naturale viene riscritto", {
  M <- mergeDbSources(oa_silver(TI = "ANOTHER TITLE"), pm_silver(), verbose = FALSE)
  expect_equal(nrow(M), 2)
  oa <- M[M$DB_Original == "OPENALEX", ]
  expect_equal(oa$AU, "SILVER FH;TRIA AJ;ZAWADSKY JP;DUNN MG")
  expect_match(oa$SR, "^SILVER FH, 1991")
})

test_that("una collezione gia' fusa riscrive i nomi OpenAlex (DB_Original)", {
  fusa <- oa_silver(DB = "ISI", DB_Original = "OPENALEX", DI = "10.1/gia", TI = "ALTRO TITOLO")
  altra <- pm_silver(DI = "10.1/altra")
  M <- mergeDbSources(fusa, altra, verbose = FALSE)
  riga <- M[M$DI %in% "10.1/gia", ]
  expect_equal(riga$AU, "SILVER FH;TRIA AJ;ZAWADSKY JP;DUNN MG")
})

# Senza DOI il titolo e l'anno dovevano coincidere: documenti diversi con lo
# stesso titolo e anno (errata, case report) sparivano, e le copie con anni
# diversi (online e fascicolo) restavano doppie
test_that("titolo e anno uguali ma DOI diversi: restano due documenti", {
  a <- pm_silver(TI = "ERRATUM", DI = "10.1/A")
  b <- oa_silver(TI = "ERRATUM", DI = "10.1/B", SO = "JOURNAL OF LONG-TERM EFFECTS OF MEDICAL IMPLANTS")
  M <- mergeDbSources(a, b, verbose = FALSE)
  expect_setequal(M$DI, c("10.1/A", "10.1/B"))
  # una sola DOI nel gruppo: la copia senza DOI e' ancora un duplicato
  M <- mergeDbSources(pm_silver(DI = "10.1/A"), oa_silver(), verbose = FALSE)
  expect_equal(nrow(M), 1)
  # stesso database, DOI diversi: due documenti anche qui
  M <- mergeDbSources(rbind(pm_silver(TI = "ERRATUM", DI = "10.1/A"), pm_silver(TI = "ERRATUM", DI = "10.1/B")),
                      verbose = FALSE)
  expect_equal(nrow(M), 2)
})

test_that("tra database diversi si uniscono le copie con anni vicini", {
  so_pm <- "ANATOMICAL RECORD (HOBOKEN, N.J. : 2007)"
  a <- pm_silver(PY = 1993, SO = so_pm, AU = "DE SILVER FH;TRIA AJ")
  b <- oa_silver(PY = 1991, SO = "THE ANATOMICAL RECORD")
  M <- mergeDbSources(a, b, verbose = FALSE)
  expect_equal(nrow(M), 1)
  expect_equal(M$TC, 42)

  # restano separati
  sep <- function(...) nrow(mergeDbSources(..., verbose = FALSE))
  # piu' di tre anni
  expect_equal(sep(pm_silver(PY = 1995), oa_silver(SO = "JOURNAL OF LONG-TERM EFFECTS OF MEDICAL IMPLANTS")), 2)
  # fonte diversa (atti di un convegno e articolo)
  expect_equal(sep(pm_silver(PY = 1992), oa_silver(SO = "PROCEEDINGS OF ISSI 1991")), 2)
  # primo autore diverso
  expect_equal(sep(pm_silver(PY = 1992), oa_silver(AU = "MARIO ROSSI", AF = "MARIO ROSSI", SO = "JOURNAL OF LONG-TERM EFFECTS OF MEDICAL IMPLANTS")), 2)
  # titolo di meno di quattro parole
  expect_equal(sep(pm_silver(PY = 1992, TI = "EDITORIAL"),
                   oa_silver(TI = "EDITORIAL", SO = "JOURNAL OF LONG-TERM EFFECTS OF MEDICAL IMPLANTS")), 2)
  # DOI diversi
  expect_equal(sep(pm_silver(PY = 1992, DI = "10.1/A"),
                   oa_silver(DI = "10.1/B", SO = "JOURNAL OF LONG-TERM EFFECTS OF MEDICAL IMPLANTS")), 2)
  # stesso database
  expect_equal(sep(rbind(pm_silver(PY = 1992), pm_silver())), 2)
  # due record per database (una rubrica annuale): nessuno si unisce all'anno vicino
  so <- "JOURNAL OF LONG-TERM EFFECTS OF MEDICAL IMPLANTS"
  expect_equal(sep(rbind(pm_silver(PY = 1991), pm_silver(PY = 1992)),
                   rbind(oa_silver(PY = 1991, SO = so), oa_silver(PY = 1992, SO = so))), 2)
})

test_that("WoS e Scopus: si unisce anche Barre' 1994/1995", {
  skip_if_not_installed("bibliometrixData")
  data(isiCollection, package = "bibliometrixData")
  data(scopusCollection, package = "bibliometrixData")
  M <- suppressWarnings(mergeDbSources(isiCollection, scopusCollection, verbose = FALSE))
  expect_equal(nrow(M), 596)
  expect_equal(sum(grepl("^DO NOT LOOK FOR SCAPEGOATS", M$TI)), 1)
})
