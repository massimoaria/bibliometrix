# Test per il filtro Journal Ranking di Biblioshiny (issue #643).
# Le funzioni vivono in inst/biblioshiny/utils.R, che non fa parte del
# namespace del pacchetto: si estraggono dal file solo le definizioni servono,
# per non dover attaccare tutte le dipendenze di Shiny.

ranking_helpers <- function() {
  path <- system.file("biblioshiny", "utils.R", package = "bibliometrix")
  if (!nzchar(path) || !file.exists(path)) {
    path <- testthat::test_path("..", "..", "inst", "biblioshiny", "utils.R")
  }
  skip_if_not(file.exists(path), "inst/biblioshiny/utils.R non trovato")

  wanted <- c(
    "read_journal_ranking", "issnColumns", "extractISSN",
    "matchJournalRanking", "rankingMatchReport"
  )
  env <- new.env(parent = globalenv())
  for (e in as.list(parse(path))) {
    if (is.call(e) && identical(as.character(e[[1]]), "<-") &&
        as.character(e[[2]]) %in% wanted) {
      eval(e, envir = env)
    }
  }
  expect_setequal(ls(env), wanted)
  env
}

# Collezione minima: due riviste, ognuna con ISSN e eISSN.
ranking_collection <- function() {
  data.frame(
    SO = c("JOURNAL OF INFORMETRICS", "SCIENTOMETRICS", "SURVEY REVIEW"),
    SN = c("1751-1577", "0138-9130", NA),
    EI = c("1875-5879", NA, NA),
    stringsAsFactors = FALSE
  )
}

test_that("extractISSN normalizza e scarta i valori non validi", {
  e <- ranking_helpers()
  expect_equal(e$extractISSN("1751-1577")[[1]], "17511577")
  expect_equal(e$extractISSN("1697-011x")[[1]], "1697011X")
  expect_equal(e$extractISSN("0138-9130; 1588-2861")[[1]], c("01389130", "15882861"))
  expect_equal(e$extractISSN(NA_character_)[[1]], character(0))
  expect_equal(e$extractISSN("")[[1]], character(0))
  expect_equal(e$extractISSN("not an issn")[[1]], character(0))
})

test_that("un file a due colonne continua a essere letto come prima", {
  e <- ranking_helpers()
  f <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(Journal = c("Journal of Informetrics", "Scientometrics"),
               Quartile = c("Q1", "Q2")),
    f, row.names = FALSE
  )
  r <- e$read_journal_ranking(f)
  expect_equal(r$SO, c("JOURNAL OF INFORMETRICS", "SCIENTOMETRICS"))
  expect_equal(r$Ranking, c("Q1", "Q2"))
  expect_true(all(is.na(r$ISSN)))
})

test_that("le colonne ISSN sono riconosciute ovunque si trovino", {
  e <- ranking_helpers()
  f <- tempfile(fileext = ".csv")
  # ISSN in seconda posizione: nome e ranking restano le prime due colonne
  # non-ISSN, quindi Quartile non viene scambiato per il ranking sbagliato
  write.csv(
    data.frame(Journal = "Journal of Informetrics",
               ISSN = "1751-1577",
               eISSN = "1875-5879",
               Quartile = "Q1"),
    f, row.names = FALSE
  )
  r <- e$read_journal_ranking(f)
  expect_equal(r$SO, "JOURNAL OF INFORMETRICS")
  expect_equal(r$Ranking, "Q1")
  expect_equal(e$extractISSN(r$ISSN)[[1]], c("17511577", "18755879"))
})

test_that("un file senza colonna nome e ranking viene rifiutato", {
  e <- ranking_helpers()
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(Journal = "X", ISSN = "1751-1577"), f, row.names = FALSE)
  expect_error(e$read_journal_ranking(f), "ranking")
})

test_that("il match per ISSN recupera le riviste che il nome non trova", {
  e <- ranking_helpers()
  M <- ranking_collection()

  # nomi abbreviati: nessuno corrisponde
  byName <- data.frame(SO = c("J. INFORMETRICS", "SCIENTOMETRICS J."),
                       Ranking = c("Q1", "Q2"), ISSN = NA_character_,
                       stringsAsFactors = FALSE)
  hit <- e$matchJournalRanking(M, byName)
  expect_equal(sum(hit$keep), 0)

  # stessi nomi ma con ISSN: entrambe recuperate
  byISSN <- transform(byName, ISSN = c("1751-1577", "0138-9130"))
  hit <- e$matchJournalRanking(M, byISSN)
  expect_equal(sum(hit$keep), 2)
  expect_equal(sum(hit$byISSN), 2)
  expect_equal(sum(hit$byName), 0)
})

test_that("il match per nome resta attivo quando l'ISSN manca", {
  e <- ranking_helpers()
  M <- ranking_collection()
  # Survey Review non ha ISSN nella collezione: deve comunque essere trovata
  r <- data.frame(SO = "SURVEY REVIEW", Ranking = "Q1", ISSN = "0039-6265",
                  stringsAsFactors = FALSE)
  hit <- e$matchJournalRanking(M, r)
  expect_equal(sum(hit$keep), 1)
  expect_equal(sum(hit$byISSN), 0)
  expect_equal(sum(hit$byName), 1)
})

test_that("documenti e ranking senza ISSN non si accoppiano fra loro", {
  e <- ranking_helpers()
  M <- ranking_collection()
  # nomi che non esistono nella collezione e nessun ISSN da nessuna parte:
  # NA %in% NA e' TRUE, quindi senza il filtro sugli NA tutto matcherebbe
  r <- data.frame(SO = "RIVISTA INESISTENTE", Ranking = "Q1",
                  ISSN = NA_character_, stringsAsFactors = FALSE)
  hit <- e$matchJournalRanking(M, r)
  expect_equal(sum(hit$keep), 0)
})

test_that("il report dice su quale chiave e' avvenuto il match", {
  e <- ranking_helpers()
  M <- ranking_collection()

  noIssn <- data.frame(SO = "JOURNAL OF INFORMETRICS", Ranking = "Q1",
                       ISSN = NA_character_, stringsAsFactors = FALSE)
  msg <- e$rankingMatchReport(M, e$matchJournalRanking(M, noIssn), noIssn)
  expect_match(msg, "1 of 3 sources")
  expect_match(msg, "no ISSN column")

  withIssn <- data.frame(SO = "J. INFORMETRICS", Ranking = "Q1",
                         ISSN = "1751-1577", stringsAsFactors = FALSE)
  msg <- e$rankingMatchReport(M, e$matchJournalRanking(M, withIssn), withIssn)
  expect_match(msg, "Matched by ISSN: 1 documents")
})
