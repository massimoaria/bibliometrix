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
