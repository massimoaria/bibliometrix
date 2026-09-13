# Test per lotka e bradford

test_that("lotka funziona con dati bibliometrici", {
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  class(scientometrics) <- c("bibliometrixDB", "data.frame")
  L <- lotka(scientometrics)
  expect_type(L, "list")
  expect_true(all(c("AuthorProd", "Beta", "C", "R2", "p.value") %in% names(L)))
  expect_true(L$Beta > 0)
  expect_true(L$R2 >= 0 && L$R2 <= 1)
})

test_that("bradford funziona e restituisce zone", {
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  BR <- bradford(scientometrics)
  expect_type(BR, "list")
  expect_true("table" %in% names(BR))
  expect_true(is.data.frame(BR$table))
  expect_true("Zone" %in% names(BR$table))
})

# Collezioni che la legge di Lotka non puo' descrivere (#674). Prima di questi
# controlli lotka() moriva dentro str_split(), aggregate() o ks.test() con un
# messaggio che non nominava nessuno dei tre.

test_that("lotka rifiuta una collezione dove tutti gli autori hanno la stessa produttivita", {
  M <- data.frame(
    AU = c("SMITH J", "DOE J", "LEE K", "WANG X", "SILVA A"),
    TC = c(10, 5, 3, 2, 1),
    SO = "JOURNAL OF TESTING",
    PY = 2023L,
    stringsAsFactors = FALSE
  )
  class(M) <- c("bibliometrixDB", "data.frame")
  expect_error(lotka(M), "at least two different publication counts")
  expect_error(lotka(M), "has written 1 document,")
})

test_that("lotka accorda il messaggio al numero di documenti", {
  M <- data.frame(AU = c("A A;B B", "A A;B B"), stringsAsFactors = FALSE)
  class(M) <- c("bibliometrixDB", "data.frame")
  expect_error(lotka(M), "has written 2 documents,")
})

test_that("lotka rifiuta una collezione senza il campo AU", {
  M <- data.frame(TC = c(1, 2), PY = c(2020L, 2021L))
  class(M) <- c("bibliometrixDB", "data.frame")
  expect_error(lotka(M), "does not carry AU")
})

test_that("lotka rifiuta una collezione vuota", {
  M <- data.frame(AU = character(0), stringsAsFactors = FALSE)
  class(M) <- c("bibliometrixDB", "data.frame")
  expect_error(lotka(M), "at least one document")
})

test_that("lotka rifiuta una collezione senza nomi di autore", {
  M <- data.frame(AU = c(NA_character_, NA_character_), stringsAsFactors = FALSE)
  class(M) <- c("bibliometrixDB", "data.frame")
  expect_error(lotka(M), "found no author name")
})

test_that("lotka analizza la collezione piu' piccola che ha due livelli di produttivita", {
  # Il confine del controllo: un autore con due documenti e uno con uno solo
  # bastano a fare una retta, e il grafico si disegna.
  M <- data.frame(AU = c("A A", "B B", "B B"), stringsAsFactors = FALSE)
  class(M) <- c("bibliometrixDB", "data.frame")
  expect_no_error(L <- lotka(M))
  expect_equal(nrow(L$AuthorProd), 2)
  expect_true(is.finite(L$Beta))
  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(L$g))
})
