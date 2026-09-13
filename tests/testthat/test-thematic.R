# Test per thematicMap e thematicEvolution
# Queste funzioni richiedono dataset grandi per generare cluster significativi

test_that("thematicMap funziona con dataset bibliometrixData", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  scientometrics$PY <- as.numeric(scientometrics$PY)
  scientometrics$TC <- as.numeric(scientometrics$TC)
  res <- expect_no_error(
    suppressWarnings(suppressMessages(
      thematicMap(scientometrics, field = "ID", n = 50, minfreq = 5,
                  stemming = FALSE, size = 0.5, repel = TRUE)
    ))
  )
  expect_type(res, "list")
  expect_true(all(c("map", "clusters", "words", "nclust") %in% names(res)))
  expect_true(res$nclust > 0)
})

test_that("thematicEvolution funziona con cut temporali", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  scientometrics$PY <- as.numeric(scientometrics$PY)
  scientometrics$TC <- as.numeric(scientometrics$TC)
  years <- range(scientometrics$PY, na.rm = TRUE)
  mid_year <- floor(mean(years))
  nexus <- expect_no_error(
    suppressWarnings(suppressMessages(
      thematicEvolution(scientometrics, field = "ID", years = mid_year,
                        n = 50, minFreq = 2)
    ))
  )
  expect_type(nexus, "list")
  expect_true("Nodes" %in% names(nexus))
  expect_true("Edges" %in% names(nexus))
})

# adjust_positions_oblique() separa i cluster che si sovrappongono sulla mappa.
# Con un solo cluster non c'e' nulla da separare, ma 1:(nrow - 1) contava
# all'indietro - 1:0 e' c(1, 0) - e il ciclo interno leggeva una riga
# inesistente: dist diventava NA e la funzione si fermava su "missing value
# where TRUE/FALSE needed".

test_that("adjust_positions_oblique lascia intatto un solo punto", {
  one <- data.frame(rcentrality = 1, rdensity = 1)
  expect_no_error(out <- adjust_positions_oblique(one))
  expect_identical(out, one)
})

test_that("adjust_positions_oblique accetta un insieme vuoto", {
  none <- data.frame(rcentrality = numeric(0), rdensity = numeric(0))
  expect_no_error(out <- adjust_positions_oblique(none))
  expect_equal(nrow(out), 0)
})

test_that("adjust_positions_oblique separa ancora i punti sovrapposti", {
  set.seed(1)
  d <- data.frame(rcentrality = c(1, 1, 3), rdensity = c(2, 2, 7))
  out <- adjust_positions_oblique(d)
  expect_equal(nrow(out), 3)
  # i due coincidenti non lo sono piu' ...
  expect_gt(
    sqrt(
      (out$rcentrality[1] - out$rcentrality[2])^2 +
        (out$rdensity[1] - out$rdensity[2])^2
    ),
    0
  )
  # ... e il punto lontano non si e' mosso
  expect_equal(out$rcentrality[3], d$rcentrality[3])
  expect_equal(out$rdensity[3], d$rdensity[3])
})

test_that("thematicMap disegna una mappa con un solo cluster", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  class(scientometrics) <- c("bibliometrixDB", "data.frame")
  # repel = FALSE e' il ramo che passa per adjust_positions_oblique; l'altro
  # test di thematicMap usa repel = TRUE e non lo attraversa mai.
  set.seed(1)
  TM <- expect_no_error(suppressWarnings(suppressMessages(
    thematicMap(
      scientometrics,
      field = "ID", n = 3, minfreq = 1, size = 0.5, repel = FALSE
    )
  )))
  expect_equal(nrow(TM$clusters), 1)
  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(TM$map))
})

test_that("thematicEvolution nomina il periodo che non ha una rete", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  class(scientometrics) <- c("bibliometrixDB", "data.frame")
  # Senza punti di taglio i periodi sono cinque di uguale ampiezza, e il primo
  # raccoglie 2 documenti soli: la sua rete di co-occorrenze e' vuota,
  # thematicMap() restituisce NULL e prima il chiamante si fermava su "no
  # applicable method for 'filter' applied to an object of class NULL".
  expect_error(
    suppressMessages(thematicEvolution(
      scientometrics,
      field = "ID", years = NULL, n = 100, minFreq = 2
    )),
    "holds 2 documents and yields no co-occurrence network"
  )
})

test_that("i punti di taglio danno sempre almeno due periodi", {
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  # E' l'invariante per cui il ramo che rifiutava meno di due periodi in
  # thematicEvolution() era irraggiungibile, e per cui e' stato rimosso:
  # timeslice() costruisce i tagli come c(min(PY) - 1, years, max(PY)), quindi
  # un vettore di n tagli da n + 1 periodi, e nessun taglio ripiega su k = 5.
  for (yrs in list(2000, c(1995, 2005), c(1990, 2000, 2010))) {
    expect_gte(length(timeslice(scientometrics, breaks = yrs)), 2)
    expect_equal(length(timeslice(scientometrics, breaks = yrs)), length(yrs) + 1)
  }
  expect_gte(length(timeslice(scientometrics, breaks = NULL)), 2)
})

test_that("thematicEvolution nomina il periodo vuoto con il suo intervallo", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  scientometrics$PY <- as.numeric(scientometrics$PY)
  G <- scientometrics[scientometrics$PY <= 1995 | scientometrics$PY >= 2005, ]
  class(G) <- c("bibliometrixDB", "data.frame")
  # Un periodo senza documenti non ha anni da cui prendere un nome - min() di
  # niente e' Inf - quindi viene riportato con l'intervallo del taglio.
  expect_error(
    suppressMessages(thematicEvolution(G, field = "ID", years = c(1996, 2004), n = 100, minFreq = 2)),
    "the period \\(1996,2004\\] holds no document"
  )
})

# Una rete puo' esistere senza che esista una mappa. Se nessun termine raggiunge
# minfreq la tabella dei cluster e' vuota, i limiti dei quadranti venivano
# calcolati su vettori vuoti (Inf / -Inf) e il frame delle annotazioni si
# fermava con "arguments imply differing number of rows: 0, 4". Lo stesso se la
# matrice ha righe ma nessuna coppia di termini co-occorre: networkPlot() la
# rifiuta. In entrambi i casi thematicMap() restituisce NULL, come per una rete
# vuota, e thematicEvolution() nomina il periodo.

test_that("thematicMap restituisce NULL quando nessun termine raggiunge minfreq", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  data(management, package = "bibliometrixData")
  class(management) <- c("bibliometrixDB", "data.frame")
  out <- capture.output(TM <- suppressWarnings(
    thematicMap(management, field = "ID", n = 250, minfreq = 1000)
  ))
  expect_null(TM)
  expect_true(any(grepl("No term of the network occurs at least", out)))
})

test_that("thematicMap restituisce NULL quando nessuna coppia di termini co-occorre", {
  M <- data.frame(
    AU = paste0("A", 1:6, " X"), DE = rep(c("ALPHA", "BETA", "GAMMA"), 2),
    ID = "", PY = 2020, TC = 1, SO = "J", TI = paste("T", 1:6), DI = "",
    DT = "ARTICLE", DB = "SCOPUS", stringsAsFactors = FALSE
  )
  M$SR <- paste0("A", 1:6, ", 2020, J")
  class(M) <- c("bibliometrixDB", "data.frame")
  out <- capture.output(TM <- suppressWarnings(
    thematicMap(M, field = "DE", n = 50, minfreq = 1)
  ))
  expect_null(TM)
  expect_true(any(grepl("No two terms of the network are linked", out)))
})

test_that("thematicEvolution nomina il periodo i cui termini non raggiungono minFreq", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  data(management, package = "bibliometrixData")
  class(management) <- c("bibliometrixDB", "data.frame")
  expect_error(
    suppressWarnings(capture.output(thematicEvolution(
      management,
      field = "ID", years = 1995, n = 100, minFreq = 1000
    ))),
    "the period 1985-1995 holds 26 documents"
  )
})
