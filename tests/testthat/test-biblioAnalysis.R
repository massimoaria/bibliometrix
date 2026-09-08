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
