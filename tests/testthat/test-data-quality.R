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

test_that("metaTagExtraction estrae CR_SO da Scopus con CR mancante (#696)", {
  M <- load_scopus_fixture()
  M$CR[1] <- NA
  M2 <- metaTagExtraction(M, Field = "CR_SO", sep = ";")
  expect_true(is.na(M2$CR_SO[1]))
  expect_identical(
    M2$CR_SO[-1],
    metaTagExtraction(M[-1, ], Field = "CR_SO", sep = ";")$CR_SO
  )
  expect_identical(convert_scopus_new_to_classic(NA_character_), NA_character_)
  expect_identical(convert_scopus_new_to_classic(""), "")
})

test_that("metaTagExtraction non assegna autori a CR_SO per riferimenti Scopus senza fonte (#711)", {
  # Citazione di libro senza fonte editoriale non deve estrarre il primo autore (#711)
  ref_book <- "GARCIA O., WEI L., TRANSLANGUAGING: LANGUAGE, BILINGUALISM, AND EDUCATION, (2014)"
  test_book <- data.frame(DB = "SCOPUS", CR = ref_book, stringsAsFactors = FALSE)
  res_book <- metaTagExtraction(test_book, Field = "CR_SO", sep = ";")
  expect_true(is.na(res_book$CR_SO))

  # Citazione mista: il libro senza fonte viene omesso, i periodici sono mantenuti
  ref_mixed <- paste(
    "GARCIA O., WEI L., TRANSLANGUAGING: LANGUAGE, BILINGUALISM, AND EDUCATION, (2014)",
    "SMITH J., TITLE (2020) NATURE, 10, PP. 1-5",
    "LI T., YAN J., LI J., REVIEW, ANN MED, 57, (2025)",
    sep = ";"
  )
  test_mixed <- data.frame(DB = "SCOPUS", CR = ref_mixed, stringsAsFactors = FALSE)
  res_mixed <- metaTagExtraction(test_mixed, Field = "CR_SO", sep = ";")
  expect_equal(res_mixed$CR_SO, "NATURE;ANN MED")

  # Periodico con layout corto (volume senza pagine) non viene piu scartato
  ref_short <- "DOE J., A STUDY, ANN MED, 57, (2025)"
  test_short <- data.frame(DB = "SCOPUS", CR = ref_short, stringsAsFactors = FALSE)
  res_short <- metaTagExtraction(test_short, Field = "CR_SO", sep = ";")
  expect_equal(res_short$CR_SO, "ANN MED")
})

test_that("applyReferenceMatching ignora i documenti Scopus senza CR (#696)", {
  M <- load_scopus_fixture()
  M$CR[1] <- NA
  M$CR[2] <- ""
  invisible(capture.output(res <- applyReferenceMatching(M)))
  expect_false(any(c(M$SR[1], M$SR[2]) %in% res$CR_normalized$SR))
  expect_setequal(res$CR_normalized$SR, M$SR[-(1:2)])
})

# normalize_citations() restituisce una riga per occorrenza: ricollegarla sul
# testo della citazione moltiplicava un riferimento citato k volte in k x k
# righe, con il warning many-to-many di dplyr. Una riga per stringa deve dare
# lo stesso risultato, senza warning.
test_that("applyReferenceMatching unisce ogni citazione una sola volta", {
  M <- load_scopus_fixture()
  expect_no_warning(invisible(capture.output(res <- applyReferenceMatching(M))))
  expect_false(anyDuplicated(res$full_data) > 0)
  expect_setequal(unique(res$full_data$CR_canonical), res$summary$CR_canonical)
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

test_that("metaTagExtraction CR_SO leaves out Scopus references with no source (#711)", {
  M <- data.frame(
    DB = "SCOPUS",
    CR = paste(
      "GARCIA O., WEI L., TRANSLANGUAGING: LANGUAGE, BILINGUALISM, AND EDUCATION, (2014)",
      "SMITH J., BROWN A., A STUDY OF X, J INFORMETR, 11, 3, PP. 959-975, (2017)",
      "ROSSI M., ANOTHER STUDY (2015) SCIENTOMETRICS, 102, PP. 1-20",
      sep = ";"
    )
  )
  expect_identical(metaTagExtraction(M, Field = "CR_SO")$CR_SO, "J INFORMETR;SCIENTOMETRICS")

  M$CR <- "GARCIA O., WEI L., TRANSLANGUAGING: LANGUAGE, BILINGUALISM, AND EDUCATION, (2014)"
  expect_true(is.na(metaTagExtraction(M, Field = "CR_SO")$CR_SO))
})

# ----- Reference matching on titles (cases from a Scopus collection, 569 documents) ---

rm_clusters <- function(refs, ...) {
  invisible(capture.output(res <- normalize_citations(refs, ...)))
  res$cluster_id[match(refs, res$CR_original)]
}

test_that("i coautori scritti in modi diversi non separano lo stesso lavoro", {
  refs <- c(
    "GARCIA O., WEI L., TRANSLANGUAGING: LANGUAGE, BILINGUALISM AND EDUCATION (2014)",
    "GARCIA O., LI W., TRANSLANGUAGING: LANGUAGE, BILINGUALISM AND EDUCATION (2014)",
    "GARCIA O., WEI LI, TRANSLANGUAGING: LANGUAGE, BILINGUALISM AND EDUCATION (2014)",
    "GARCIA O., WEI L., TRANSLANGUAGING: LANGUAGE, BILINGUALISM, AND EDUCATION (2014)",
    "GARCIA O., WEI L., TRANSLANGUAGING: LANGUAGE, BILINGUALISM AND EDUCATION (PALGRAVE PIVOT) (2014)",
    "GARCIA O., JOHNSON S.I., SELTZER K., THE TRANSLANGUAGING CLASSROOM: LEVERAGING STUDENT BILINGUALISM FOR LEARNING (2017)",
    "GARCIA O., IBARRA JOHNSON S., SELTZER K., THE TRANSLANGUAGING CLASSROOM: LEVERAGING STUDENT BILINGUALISM FOR LEARNING (2017)",
    "GARCIA O., JOHNSON S. I., SELTZER K., THE TRANSLANGUAGING CLASSROOM. LEVERAGING STUDENT BILINGUALISM FOR LEARNING (2017)"
  )
  cl <- rm_clusters(refs)
  expect_length(unique(cl[1:5]), 1)
  expect_length(unique(cl[6:8]), 1)
  expect_false(cl[1] == cl[6])
})

test_that("lavori diversi dello stesso autore e anno restano separati", {
  refs <- c(
    "GENESEE F., GEVA E., DRESSLER C., KAMIL M., SYNTHESIS: CROSS-LINGUISTIC RELATIONSHIPS, DEVELOPING LITERACY IN SECOND-LANGUAGE LEARNERS (2006) PP. 153-174",
    "GENESEE F., BILINGUAL FIRST LANGUAGE ACQUISITION IN PERSPECTIVE, CHILDHOOD BILINGUALISM: RESEARCH ON INFANCY THROUGH SCHOOL AGE (2006) PP. 45-67",
    "GENESEE F., WHAT DO WE KNOW ABOUT BILINGUAL EDUCATION FOR MAJORITY-LANGUAGE STUDENTS?, THE HANDBOOK OF BILINGUALISM (2006) PP. 547-576"
  )
  expect_length(unique(rm_clusters(refs)), 3)

  refs <- c(
    "GARCIA O., WEI L., TRANSLANGUAGING AND EDUCATION, TRANSLANGUAGING: LANGUAGE, BILINGUALISM AND EDUCATION (2014) PP. 63-77",
    "GARCIA O., WEI L., TRANSLANGUAGING IN EDUCATION: PRINCIPLES, IMPLICATIONS AND CHALLENGES, TRANSLANGUAGING: LANGUAGE, BILINGUALISM AND EDUCATION (2014) PP. 119-135",
    "GARCIA O., KANO N., TRANSLANGUAGING AS PROCESS AND PEDAGOGY: DEVELOPING THE ENGLISH WRITING OF JAPANESE STUDENTS IN THE US, THE MULTILINGUAL TURN IN LANGUAGES EDUCATION (2014) PP. 258-277"
  )
  expect_length(unique(rm_clusters(refs)), 3)
})

test_that("un capitolo resta distinto dal libro che lo contiene", {
  refs <- c(
    "GARCIA O., WEI L., TRANSLANGUAGING: LANGUAGE, BILINGUALISM AND EDUCATION (2014)",
    "GARCIA O., WEI L., TRANSLANGUAGING AND EDUCATION, TRANSLANGUAGING: LANGUAGE, BILINGUALISM AND EDUCATION (2014) PP. 63-77",
    "CENOZ J., BASQUE EDUCATIONAL RESEARCH FROM AN INTERNATIONAL PERSPECTIVE (2009)",
    "CENOZ J., TOWARDS MULTILINGUAL EDUCATION, BASQUE EDUCATIONAL RESEARCH FROM AN INTERNATIONAL PERSPECTIVE (2009)"
  )
  cl <- rm_clusters(refs)
  expect_false(cl[1] == cl[2])
  expect_false(cl[3] == cl[4])
})

test_that("un capitolo citato con contenitori diversi resta uno, una ristampa no", {
  refs <- c(
    "GENESEE F., WHAT DO WE KNOW ABOUT BILINGUAL EDUCATION FOR MAJORITY-LANGUAGE STUDENTS?, THE HANDBOOK OF BILINGUALISM (2004) PP. 547-576",
    "GENESEE F., WHAT DO WE KNOW ABOUT BILINGUAL EDUCATION FOR MAJORITY LANGUAGE STUDENTS, HANDBOOK OF BILINGUALISM AND MULTICULTURALISM (2004) PP. 547-576",
    "GENESEE F., WHAT DO WE KNOW ABOUT BILINGUAL EDUCATION FOR MAJORITY LANGUAGE STUDENTS?, EDUCATION FOR ALL (2004) PP. 141-156"
  )
  cl <- rm_clusters(refs)
  expect_equal(cl[1], cl[2])
  expect_false(cl[1] == cl[3])
})

test_that("gli anni vicini si uniscono, le edizioni lontane no", {
  title <- "SOCIAL LINGUISTICS AND LITERACIES: IDEOLOGY IN DISCOURSES"
  refs <- paste0("GEE J.P., ", title, c(" (2015)", " (2015) ", " (2016)", " (1996)", " (2008)"))
  refs <- trimws(refs)
  refs <- unique(c(refs, paste0("GEE J. P., ", title, " (2015)")))
  cl <- rm_clusters(refs)
  expect_equal(cl[refs == paste0("GEE J.P., ", title, " (2016)")], cl[1])
  expect_false(cl[refs == paste0("GEE J.P., ", title, " (1996)")] == cl[1])
  expect_false(cl[refs == paste0("GEE J.P., ", title, " (2008)")] == cl[1])
})

test_that("merge_editions unisce le edizioni, ma non volumi o pagine diversi", {
  title <- "SOCIAL LINGUISTICS AND LITERACIES: IDEOLOGY IN DISCOURSES"
  refs <- paste0("GEE J.P., ", title, c(" (1996)", " (2008)", " (2015)"))
  expect_length(unique(rm_clusters(refs)), 3)
  expect_length(unique(rm_clusters(refs, merge_editions = TRUE)), 1)

  articles <- c(
    "SMITH J., LANGUAGE POLICY IN MULTILINGUAL SCHOOLS (2001) JOURNAL OF LANGUAGE, 10, PP. 1-20",
    "SMITH J., LANGUAGE POLICY IN MULTILINGUAL SCHOOLS (2010) JOURNAL OF LANGUAGE, 19, PP. 30-50"
  )
  expect_length(unique(rm_clusters(articles, merge_editions = TRUE)), 2)
})

test_that("l'anno viene dalle parentesi, non da un numero nel titolo", {
  refs <- c(
    "SMITH J., THE 2030 AGENDA FOR LANGUAGE EDUCATION POLICY (2019)",
    "SMITH J., JONES K., THE 2030 AGENDA FOR LANGUAGE EDUCATION POLICY (2019)"
  )
  invisible(capture.output(res <- normalize_citations(refs)))
  expect_setequal(res$year, "2019")
  expect_length(unique(res$cluster_id), 1)
})

test_that("use_title = FALSE riproduce il confronto senza titolo", {
  refs <- c(
    "GARCIA O., WEI L., TRANSLANGUAGING AND EDUCATION, TRANSLANGUAGING: LANGUAGE, BILINGUALISM AND EDUCATION (2014) PP. 63-77",
    "GARCIA O., WEI L., TRANSLANGUAGING IN EDUCATION: PRINCIPLES, IMPLICATIONS AND CHALLENGES, TRANSLANGUAGING: LANGUAGE, BILINGUALISM AND EDUCATION (2014) PP. 119-135"
  )
  expect_length(unique(rm_clusters(refs, use_title = FALSE)), 1)
})

test_that("la similarita segnala le fusioni deboli e splitReferenceVariant le annulla", {
  M <- data.frame(
    SR = c("A", "B", "C"),
    CR = c(
      "KRAMSCH C., THE MULTILINGUAL SUBJECT (2006) INTERNATIONAL JOURNAL OF APPLIED LINGUISTICS, 16, 1, PP. 97-110;DOE J., A DIFFERENT WORK ABOUT LANGUAGES (2006)",
      "KRAMSCH C., THE MULTILINGUAL SUBJECT (2006) INTERNATIONAL JOURNAL OF APPLIED LINGUISTICS, 16, 1, PP. 97-110",
      "KRAMSCH C., PREVIEW ARTICLE: THE MULTILINGUAL SUBJECT (2006) INTERNATIONAL JOURNAL OF APPLIED LINGUISTICS, 16, 1, PP. 97-110"
    ),
    stringsAsFactors = FALSE
  )
  invisible(capture.output(res <- applyReferenceMatching(M)))
  merged <- res$summary[grepl("KRAMSCH", res$summary$CR_canonical), ]
  expect_equal(nrow(merged), 1)
  expect_equal(merged$n_variants, 2)
  expect_lt(merged$min_similarity, 0.9)
  expect_equal(max(res$full_data$similarity[grepl("KRAMSCH", res$full_data$CR)]), 1)

  variant <- "KRAMSCH C., PREVIEW ARTICLE: THE MULTILINGUAL SUBJECT (2006) INTERNATIONAL JOURNAL OF APPLIED LINGUISTICS, 16, 1, PP. 97-110"
  split <- splitReferenceVariant(res, variant)
  expect_equal(sum(grepl("KRAMSCH", split$summary$CR_canonical)), 2)
  expect_equal(split$CR_normalized$CR[split$CR_normalized$SR == "C"], variant)
  expect_false(grepl("PREVIEW", split$CR_normalized$CR[split$CR_normalized$SR == "B"]))
  expect_error(splitReferenceVariant(res, "NOT A REFERENCE"), "Not among")

  # Separare la forma canonica lascia il ruolo alla variante rimasta
  canon <- merged$CR_canonical
  other <- splitReferenceVariant(res, canon)
  expect_equal(sum(grepl("KRAMSCH", other$summary$CR_canonical)), 2)
  expect_true(all(c(canon, variant) %in% other$summary$CR_canonical))
})
