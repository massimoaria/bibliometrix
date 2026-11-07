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

test_that("convert2df non spezza gli autori BibTeX andati a capo", {
  M <- load_wos_bibtex_fixture()
  expect_equal(nrow(M), 3)

  # Record 1: l'a capo cade dopo la virgola di "Selvanathan, Saroja".
  expect_equal(
    M$AF[1],
    "SELVANATHAN ELIYATHAMBY A;JAYASINGHE MANEKA;SELVANATHAN SAROJA"
  )
  expect_equal(M$AU[1], "SELVANATHAN EA;JAYASINGHE M;SELVANATHAN S")

  # Record 2: l'a capo cade subito dopo "and", senza spazi residui.
  expect_equal(M$AF[2], "MARASINGHE SUMUDU;SIMPSON GREG D;NEWSOME DAVID")
  expect_equal(M$AU[2], "MARASINGHE S;SIMPSON GD;NEWSOME D")

  # Record 3: campo su una riga sola, invariato.
  expect_equal(M$AF[3], "SHINDE KIRAN")
  expect_equal(M$AU[3], "SHINDE K")

  # Nessun autore ridotto al solo cognome o al solo nome di battesimo.
  authors <- trimws(unlist(strsplit(M$AF, ";")))
  expect_true(all(grepl(" ", authors)))
  expect_false(any(grepl("^\\s", unlist(strsplit(M$AF, ";")))))
})

test_that("convert2df non lascia spazi nell'autore corrispondente OpenAlex", {
  # issue #666. Nel formato "vecchio" C1 e C1_ID sono uniti da
  # extract_collapsed_affiliations() con "; ", mentre AU e AU_ID arrivano dalla
  # conversione '|' -> ';' senza spazio. replace_corresponding_info() divideva
  # su ";" senza tagliare: l'affiliazione dell'autore corrispondente usciva con
  # uno spazio davanti ogni volta che il corrispondente non era il primo autore,
  # e la stessa universita' finiva contata come due valori distinti.
  M <- load_openalex_oldformat_fixture()
  expect_equal(nrow(M), 3)

  # Nessun RP con spazi in testa o in coda.
  expect_equal(M$RP, trimws(M$RP))

  # W1 ha il corrispondente in seconda posizione, W3 in prima: stessa
  # affiliazione, quindi uno stesso valore e non due.
  expect_equal(M$RP[1], M$RP[3])
  expect_equal(M$RP[1], "UNIVERSITY OF CAMPANIA LUIGI VANVITELLI, CASERTA, ITALY")
  expect_equal(M$RP[2], "UNIVERSITY OF NAPLES FEDERICO II, NAPLES, ITALY")
  expect_length(unique(M$RP), 2)

  expect_equal(M$AU1_CO, rep("ITALY", 3))
})

test_that("replace_corresponding_info tollera il separatore con spazio", {
  # issue #666. Con AU_ID unito da "; " il confronto fra gli ID non trovava piu'
  # il corrispondente: nome e paese uscivano NA mentre l'affiliazione, il cui
  # split era gia' tagliato, si risolveva. Il difetto era quindi invisibile nel
  # solo caso in cui il corrispondente e' il primo autore.
  data <- data.frame(
    corresponding_author_ids = "A2",
    corresponding_institution_ids = "I2",
    AU = "SMITH J; DOE J",
    AU_ID = "A1; A2",
    C1 = "UNIV A; UNIV B",
    C1_ID = "I1; I2",
    AU_CO = "USA; CANADA",
    stringsAsFactors = FALSE
  )
  res <- as.data.frame(replace_corresponding_info(data))

  expect_equal(res$corresponding_author_name, "DOE J")
  expect_equal(res$RP, "UNIV B")
  expect_equal(res$AU1_CO, "CANADA")
})

# ----- Nomi OpenAlex: cognome prima ---------------------------------------------

test_that("oaParseNames mette il cognome prima", {
  casi <- c(
    "Frederick H. Silver" = "SILVER FH",
    "F. H. Silver" = "SILVER FH",
    "F.H. Silver" = "SILVER FH",
    "Jean-Pierre Dupont" = "DUPONT JP",
    "Jean‐Pierre Dupont" = "DUPONT JP",
    "Bas van der Meer" = "VAN DER MEER B",
    "Ana de la Cruz" = "DE LA CRUZ A",
    "Joao da Silva" = "DA SILVA J",
    "Mohamed El Sayed" = "EL SAYED M",
    "Conor O’Brien" = "O'BRIEN C",
    "John Smith Jr." = "SMITH J",
    "Silver, F. H." = "SILVER FH",
    "Da Li" = "LI D",
    "Madonna" = "MADONNA",
    "World Health Organization" = "WORLD HEALTH ORGANIZATION",
    "Juan Garcia Lopez" = "LOPEZ JG", # limite noto: cognome composto senza particella
    "Wei Yang (109917)" = "YANG W",
    "Silver, F. H. (A123) (B45)" = "SILVER FH",
    "(109917)" = "(109917)"
  )
  expect_equal(oaParseNames(names(casi))$AU, unname(casi))
  expect_equal(oaParseNames("Frederick H. Silver")$AF, "SILVER, FREDERICK H")
})

test_that("oaAuthorForm e' idempotente e conserva le posizioni", {
  au <- "FREDERICK H. SILVER;ALFRED J. TRIA"
  M <- data.frame(
    AU = c(au, "X"), AF = c(au, NA), AU_ID = c("A1;A2", "A3"),
    corresponding_author_name = c("ALFRED J. TRIA", NA), stringsAsFactors = FALSE
  )
  once <- oaAuthorForm(M)
  expect_equal(once$AU, c("SILVER FH;TRIA AJ", "X")) # AF mancante: AU non si tocca
  expect_equal(once$AF, c("SILVER, FREDERICK H;TRIA, ALFRED J", NA))
  expect_equal(once$corresponding_author_name, c("TRIA AJ", NA))
  expect_equal(oaAuthorForm(once), once)
})

test_that("convert2df scrive gli autori OpenAlex con il cognome prima", {
  M <- load_openalex_fixture()
  i <- grep("ARIA", M$AU)[1]
  expect_match(M$AU[i], "^ARIA M;CUCCURULLO C")
  expect_match(M$AF[i], "^ARIA, MASSIMO;CUCCURULLO, CORRADO")
  expect_match(M$SR[i], "^ARIA M, 2017")
})

test_that("convert2df usa l'anno della rivista per PubMed via API", {
  # La struttura che pmApiRequest() ottiene da XML::xmlToList()
  record <- list(
    MedlineCitation = list(
      PMID = list(text = "10171110", .attrs = c(Version = "1")),
      Article = list(
        Journal = list(
          JournalIssue = list(PubDate = list(Year = "1991")),
          Title = "Journal of long-term effects of medical implants"
        ),
        ArticleTitle = "Anterior cruciate ligament replacement: a review.",
        AuthorList = list(Author = list(
          LastName = "Silver", ForeName = "F H", Initials = "FH"
        )),
        PublicationTypeList = list(PublicationType = list(
          text = "Journal Article", .attrs = c(UI = "D016428")
        ))
      )
    ),
    PubmedData = list(History = list(PubMedPubDate = list(
      Year = "1990", Month = "12", Day = "10", .attrs = c(PubStatus = "entrez")
    )))
  )
  P <- list(data = list(PubmedArticle = record))
  M <- suppressWarnings(suppressMessages({
    utils::capture.output(res <- convert2df(P, dbsource = "pubmed", format = "api"))
    res
  }))
  expect_equal(M$PY, 1991)
})
