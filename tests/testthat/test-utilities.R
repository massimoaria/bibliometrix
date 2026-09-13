# Test per funzioni utility: trim, trim.leading, trimES

test_that("trim rimuove spazi iniziali e finali", {
  expect_equal(trim("  hello  "), "hello")
  expect_equal(trim("hello"), "hello")
  expect_equal(trim("  "), "")
  expect_equal(trim(c("  a ", " b")), c("a", "b"))
})

test_that("trim.leading rimuove solo spazi iniziali", {
  expect_equal(trim.leading("  hello"), "hello")
  expect_equal(trim.leading("  hello  "), "hello  ")
  expect_equal(trim.leading("hello"), "hello")
  expect_equal(trim.leading(c("  a", " b")), c("a", "b"))
})

test_that("trimES rimuove spazi extra multipli", {
  expect_equal(trimES("hello  world"), "hello world")
  expect_equal(trimES("a   b   c"), "a b c")
  expect_equal(trimES("hello world"), "hello world")
  expect_equal(trimES(c("a  b", "c   d")), c("a b", "c d"))
})

# timeslice() documenta k come "used only in case breaks argument is not
# provided". NULL e' il modo naturale di dirlo per chi non ha punti di taglio,
# ma NULL[1] e' NULL, quindi is.na() dava logical(0) e il test si fermava con
# "argument is of length zero" invece di ripiegare su k.

test_that("timeslice tratta breaks = NULL come breaks non fornito", {
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  expect_no_error(sl <- timeslice(scientometrics, breaks = NULL))
  expect_identical(sl, timeslice(scientometrics))
  expect_length(sl, 5) # il default k = 5
})

test_that("timeslice con breaks = NULL rispetta k", {
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  expect_length(timeslice(scientometrics, breaks = NULL, k = 3), 3)
  expect_length(timeslice(scientometrics, breaks = numeric(0), k = 3), 3)
})

test_that("timeslice con punti di taglio espliciti non cambia", {
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  sl <- timeslice(scientometrics, breaks = c(1995, 2005))
  expect_length(sl, 3)
  expect_equal(sum(vapply(sl, nrow, integer(1))), nrow(scientometrics))
})
