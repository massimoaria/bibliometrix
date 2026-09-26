# Test per biblioAnalysis, summary e plot

test_that("biblioAnalysis restituisce oggetto bibliometrix", {
  M <- load_wos_fixture()
  res <- biblioAnalysis(M)
  expect_s3_class(res, "bibliometrix")
  expect_true(res$Articles > 0)
  expect_true(res$nAuthors > 0)
  expect_true(is.numeric(res$Years))
})

test_that("biblioAnalysis contiene tutti i componenti attesi", {
  M <- load_wos_fixture()
  res <- biblioAnalysis(M)
  expected <- c("Articles", "Authors", "AuthorsFrac", "FirstAuthors",
                "nAUperPaper", "Appearances", "nAuthors", "MostCitedPapers",
                "Years", "Sources", "DE", "ID")
  expect_true(all(expected %in% names(res)))
})

test_that("biblioAnalysis funziona con dati Scopus", {
  M <- load_scopus_fixture()
  res <- biblioAnalysis(M)
  expect_s3_class(res, "bibliometrix")
  expect_equal(res$Articles, nrow(M))
})

test_that("biblioAnalysis funziona con dati OpenAlex", {
  M <- load_openalex_fixture()
  res <- biblioAnalysis(M)
  expect_s3_class(res, "bibliometrix")
  expect_equal(res$Articles, nrow(M))
})

test_that("summary.bibliometrix funziona senza errori", {
  M <- load_wos_fixture()
  res <- biblioAnalysis(M)
  expect_no_error(
    capture.output(summary(res, k = 5, pause = FALSE, verbose = FALSE))
  )
})

test_that("plot.bibliometrix funziona senza errori", {
  M <- load_wos_fixture()
  res <- biblioAnalysis(M)
  expect_no_error(plot(res, k = 5, pause = FALSE))
})

# ---------------------------------------------------------------------------
# Collezioni degeneri o prive di un campo obbligatorio (da PR #661).
# biblioAnalysis e i suoi metodi summary/plot leggevano valori assegnati solo
# dentro un if, e calcolavano intervalli che collassano quando la collezione non
# ha estensione. Un test per ciascun modo di rottura misurato.
# ---------------------------------------------------------------------------

# Esegue analisi, summary e plot su una collezione, senza output a schermo.
expect_pipeline_runs <- function(M) {
  res <- suppressWarnings(suppressMessages(biblioAnalysis(M)))
  expect_s3_class(res, "bibliometrix")
  expect_no_error(
    capture.output(suppressWarnings(suppressMessages(
      summary(res, k = 5, pause = FALSE, verbose = FALSE)
    )))
  )
  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(suppressWarnings(suppressMessages(plot(res, k = 5, pause = FALSE))))
  res
}

test_that("biblioAnalysis analizza una collezione senza autori", {
  M <- load_wos_fixture()
  M$AU <- NULL
  res <- expect_pipeline_runs(M)
  # NULL, come ogni altro campo che la collezione non sostiene: 0 direbbe
  # "documenti senza autori", che e' un'altra affermazione.
  expect_null(res$Authors)
  expect_null(res$nAUperPaper)
  expect_null(res$AuSingleAuthoredArt)
  expect_null(res$AuMultiAuthoredArt)
  expect_equal(res$Appearances, 0)
  expect_equal(res$Articles, nrow(M))
})

test_that("biblioAnalysis analizza una collezione senza anno di pubblicazione", {
  M <- load_wos_fixture()
  M$PY <- NULL
  res <- expect_pipeline_runs(M)
  expect_null(res$Years)
  # Le citazioni restano note anche quando la loro eta' non lo e'.
  expect_equal(length(res$TotalCitation), nrow(M))
  expect_equal(nrow(res$MostCitedPapers), nrow(M))
  expect_true(all(is.na(res$MostCitedPapers$TCperYear)))
})

test_that("biblioAnalysis analizza una collezione senza citazioni", {
  M <- load_wos_fixture()
  M$TC <- NULL
  res <- expect_pipeline_runs(M)
  expect_null(res$TotalCitation)
  expect_null(res$MostCitedPapers)
})

test_that("summary.bibliometrix funziona senza la rivista di pubblicazione", {
  M <- load_wos_fixture()
  M$SO <- NULL
  res <- expect_pipeline_runs(M)
  expect_null(res$Sources)
})

test_that("biblioAnalysis funziona su una collezione priva del campo DB", {
  # metaTagExtraction sceglie il ramo con if (M$DB[1] == ...): senza la colonna
  # la condizione ha lunghezza zero e if() non puo' valutarla.
  M <- load_wos_fixture()
  M$DB <- NULL
  expect_pipeline_runs(M)
})

test_that("biblioAnalysis rifiuta una collezione vuota con un messaggio chiaro", {
  M <- load_wos_fixture()
  expect_error(biblioAnalysis(M[0, ]), "at least one document")
})

test_that("plot.bibliometrix disegna una collezione di un solo documento", {
  # Con un solo documento ogni intervallo e' nullo: il riquadro del logo
  # collassa e grid si ferma su un raster di proporzioni indefinite.
  M <- load_wos_fixture()
  expect_pipeline_runs(M[1, ])
})

test_that("logoDelta non restituisce mai un'estensione nulla", {
  expect_equal(logoDelta(c(0, 100)), 15)
  expect_equal(logoDelta(c(7, 7)), 1)
  expect_equal(logoDelta(2020), 1)
  expect_equal(logoDelta(c(NA, NA)), 1)
})

# sourceGrowth() su una collezione di un solo anno. PYSO ha una riga sola, e
# PYSO[, colonne] la riduceva a un vettore che as.matrix() trasformava in una
# colonna: la tabella usciva trasposta e l'assegnazione dei nomi si fermava con
# "'names' attribute [4] must be the same length as the vector [2]".

test_that("sourceGrowth su un solo anno restituisce una riga, non una tabella trasposta", {
  skip_if_not_installed("bibliometrixData")
  data(management, package = "bibliometrixData")
  class(management) <- c("bibliometrixDB", "data.frame")
  SG <- sourceGrowth(management[management$PY == 2019, ], top = 3)
  expect_equal(nrow(SG), 1)
  expect_equal(names(SG)[1], "Year")
  expect_equal(SG$Year, 2019)
  expect_true(all(vapply(SG[, -1, drop = FALSE], is.numeric, logical(1))))
})

test_that("sourceGrowth su piu' anni non cambia", {
  skip_if_not_installed("bibliometrixData")
  data(management, package = "bibliometrixData")
  class(management) <- c("bibliometrixDB", "data.frame")
  SG <- sourceGrowth(management, top = 5)
  expect_equal(names(SG)[1], "Year")
  expect_equal(nrow(SG), length(min(management$PY):max(management$PY)))
  expect_true(ncol(SG) >= 6)
})

# dominance() escludeva i co-autori che non hanno mai pubblicato da primo
# autore in articoli a piu' autori (FAA = 0), e aggiungeva righe spurie di NA
# quando k superava il numero di autori disponibili (#694).

test_that("dominance include co-autori con zero primi autorati e delimita k", {
  M <- data.frame(
    SR = paste("Paper", 1:12),
    TI = paste("Paper", 1:12),
    AU = c(
      rep("ALICE; PROF_SILVA", 4),
      "BOB; PROF_SILVA",
      rep("OTHER; PROF_SILVA", 5),
      "BOB", "BOB"
    ),
    AU_CO = NA_character_,
    PY = 2024,
    SO = "TEST JOURNAL",
    TC = 1,
    stringsAsFactors = FALSE
  )
  class(M) <- c("bibliometrixDB", "data.frame")
  res <- biblioAnalysis(M)

  DF <- dominance(res, k = 5)
  expect_equal(nrow(DF), 4)
  expect_false(any(is.na(DF$Author)))
  expect_equal(row.names(DF), as.character(1:4))

  expect_true("PROF_SILVA" %in% DF$Author)
  ps <- DF[DF$Author == "PROF_SILVA", ]
  expect_equal(ps$"Dominance Factor", 0.0)
  expect_equal(ps$"Tot Articles", 10)
  expect_equal(ps$"Single-Authored", 0)
  expect_equal(ps$"Multi-Authored", 10)
  expect_equal(ps$"First-Authored", 0)
  expect_equal(ps$"Rank by Articles", 1)
  expect_equal(ps$"Rank by DF", 4)

  # Altri autori con FAA > 0 e k ridotto
  DF2 <- dominance(res, k = 2)
  expect_equal(nrow(DF2), 2)
  expect_equal(sort(DF2$Author), c("OTHER", "PROF_SILVA"))
  expect_equal(DF2$"Rank by Articles", c(2, 1))
  expect_equal(DF2$"Rank by DF", c(1, 2))
  expect_equal(row.names(DF2), as.character(1:2))
})

test_that("dominance gestisce casi limite di borda senza errori", {
  # Solo articoli a singolo autore
  M_solo <- data.frame(
    SR = c("P1", "P2"),
    TI = c("Paper 1", "Paper 2"),
    AU = c("SOLO_A", "SOLO_B"),
    AU_CO = NA_character_,
    PY = 2024,
    SO = "JOURNAL",
    TC = 1,
    stringsAsFactors = FALSE
  )
  class(M_solo) <- c("bibliometrixDB", "data.frame")
  res_solo <- biblioAnalysis(M_solo)
  DF_solo <- dominance(res_solo)
  expect_equal(nrow(DF_solo), 0)
  expect_equal(ncol(DF_solo), 8)

  # k = 0
  DF_zero <- dominance(res_solo, k = 0)
  expect_equal(nrow(DF_zero), 0)
  expect_equal(ncol(DF_zero), 8)

  # Oggetto vuoto o privo di autori
  res_empty <- structure(list(Authors = NULL), class = "bibliometrix")
  DF_empty <- dominance(res_empty)
  expect_equal(nrow(DF_empty), 0)
  expect_equal(ncol(DF_empty), 8)

  # Input che non e' un oggetto bibliometrix
  expect_true(is.na(dominance(data.frame())))
})

