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
