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
