# Test per fieldByYear, authorProdOverTime, threeFieldsPlot, histNetwork, histPlot

test_that("fieldByYear genera output con grafico", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  res <- expect_no_error(
    suppressWarnings(suppressMessages(
      fieldByYear(scientometrics, field = "ID", timespan = NULL,
                  min.freq = 5, n.items = 5, graph = TRUE)
    ))
  )
  expect_type(res, "list")
  expect_true("graph" %in% names(res))
})

test_that("authorProdOverTime produce risultati corretti", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  res <- expect_no_error(
    suppressWarnings(suppressMessages(
      authorProdOverTime(scientometrics, k = 5, graph = FALSE)
    ))
  )
  expect_type(res, "list")
  expect_true(all(c("dfAU", "dfPapersAU", "graph") %in% names(res)))
  expect_true(is.data.frame(res$dfAU))
})

test_that("threeFieldsPlot genera sankey plot senza errori", {
  skip_on_cran()
  M <- load_wos_fixture()
  expect_no_error(
    suppressWarnings(suppressMessages(
      threeFieldsPlot(M, fields = c("AU", "DE", "SO"), n = c(5, 5, 5))
    ))
  )
})

test_that("histNetwork costruisce rete storica", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  histResults <- expect_no_error(
    suppressWarnings(suppressMessages(
      histNetwork(scientometrics, sep = ";", verbose = FALSE)
    ))
  )
  expect_type(histResults, "list")
})
