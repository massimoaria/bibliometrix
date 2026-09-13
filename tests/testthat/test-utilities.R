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

# Un periodo che non contiene documenti e' un livello del fattore prodotto da
# cut() ma non un gruppo di split(): i nomi erano piu' lunghi della lista a cui
# venivano assegnati e timeslice() si fermava con "'names' attribute [3] must
# be the same length as the vector [2]". Basta un buco negli anni di
# pubblicazione, non serve un taglio fuori intervallo.

gap_collection <- function() {
  data(scientometrics, package = "bibliometrixData")
  scientometrics$PY <- as.numeric(scientometrics$PY)
  G <- scientometrics[scientometrics$PY <= 1995 | scientometrics$PY >= 2005, ]
  class(G) <- c("bibliometrixDB", "data.frame")
  G
}

test_that("timeslice tiene un periodo vuoto invece di fermarsi", {
  skip_if_not_installed("bibliometrixData")
  G <- gap_collection()
  expect_equal(sum(G$PY > 1995 & G$PY < 2005), 0) # il buco c'e' davvero
  expect_no_error(sl <- timeslice(G, breaks = c(1996, 2004)))
  expect_length(sl, 3)
  expect_equal(unname(vapply(sl, nrow, integer(1))[2]), 0)
  expect_equal(sum(vapply(sl, nrow, integer(1))), nrow(G)) # nessun documento perso
})

test_that("timeslice accetta tagli fuori dall'intervallo degli anni", {
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  expect_length(timeslice(scientometrics, breaks = 1800), 2)
  expect_length(timeslice(scientometrics, breaks = 2100), 2)
  # il taglio non e' nell'intervallo, quindi un periodo resta vuoto
  expect_equal(min(vapply(timeslice(scientometrics, breaks = 1800), nrow, integer(1))), 0)
})

test_that("timeslice nomina il taglio che ripete un estremo", {
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  yr <- as.numeric(scientometrics$PY)
  expect_error(
    timeslice(scientometrics, breaks = max(yr, na.rm = TRUE)),
    "is a repeated bound"
  )
  expect_error(
    timeslice(scientometrics, breaks = max(yr, na.rm = TRUE)),
    "fall inside 1985-2015"
  )
  expect_error(timeslice(scientometrics, breaks = c(2000, 2000)), "2000 is a repeated bound")
})

test_that("timeslice ordina da se' i tagli passati alla rinfusa", {
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  expect_identical(
    timeslice(scientometrics, breaks = c(2005, 1995)),
    timeslice(scientometrics, breaks = c(1995, 2005))
  )
})
