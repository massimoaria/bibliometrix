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

test_that("histNetwork restituisce una matrice quadrata con poche citazioni locali", {
  M <- metaTagExtraction(load_wos_fixture(), Field = "SR")
  h <- suppressWarnings(suppressMessages(
    histNetwork(M, min.citations = 0, sep = ";", verbose = FALSE)
  ))
  expect_true(is.matrix(h$NetMatrix))
  expect_equal(nrow(h$NetMatrix), ncol(h$NetMatrix))
  expect_identical(rownames(h$NetMatrix), colnames(h$NetMatrix))
  expect_true(sum(h$NetMatrix) > 0)
})

test_that("histNetwork non fallisce senza citazioni locali", {
  # Nessun riferimento corrisponde a un documento della collezione: la colonna
  # LCR resta vuota e cocMatrix() restituisce NA invece di una matrice
  M <- metaTagExtraction(load_wos_fixture(), Field = "SR")
  M$CR <- "SMITH J, 1900, NOWHERE J DOI 10.0000/NONE"
  expect_no_error(
    suppressWarnings(suppressMessages(
      capture.output(h <- histNetwork(M, min.citations = 0, sep = ";", verbose = FALSE))
    ))
  )
  expect_true(is.matrix(h$NetMatrix))
  expect_equal(dim(h$NetMatrix), c(nrow(M), nrow(M)))
  expect_identical(rownames(h$NetMatrix), colnames(h$NetMatrix))
  expect_equal(sum(h$NetMatrix), 0)
  expect_equal(sum(h$LCS), 0)
})

test_that("histNetwork trova le citazioni locali nei nuovi export WoS", {
  # Prima della normalizzazione dei riferimenti in isi2df() questa collezione
  # restituiva "Matrix is empty!!" e zero citazioni locali (#640)
  M <- suppressMessages(metaTagExtraction(load_wos_newformat_fixture(), Field = "SR"))
  h <- suppressWarnings(suppressMessages(
    histNetwork(M, min.citations = 0, sep = ";", verbose = FALSE)
  ))
  expect_true(is.matrix(h$NetMatrix))
  expect_equal(dim(h$NetMatrix), c(3L, 3L))
  # Aaker 1997 e' citato da Abratt e Balmer, Abratt 1999 da Balmer
  expect_equal(sum(h$LCS), 3)
  expect_equal(sum(h$NetMatrix > 0), 3)
})

# Il box in cui viene disegnato il logo. Quando tutti i valori di un asse sono
# uguali - un solo anno, una sola rivista, un solo cluster - diff(range())
# vale zero: il box collassa a un punto, il viewport di grid ha area nulla, il
# suo rapporto e' 0/0 e il grafico si ferma con "missing value where
# TRUE/FALSE needed" invece di disegnarsi. logoDelta() e' la difesa comune.

test_that("logoDelta non restituisce mai un lato nullo", {
  expect_equal(logoDelta(c(0, 10), frac = 0.125), 1.25)
  expect_equal(logoDelta(c(1990, 2020), frac = 0.10), 3)
  expect_equal(logoDelta(c(2020, 2020)), 1) # un solo anno
  expect_equal(logoDelta(numeric(0)), 1) # nessun valore
  expect_equal(logoDelta(c(NA_real_, NA_real_)), 1) # solo valori mancanti
  expect_equal(logoDelta(c(5, 5), fallback = 0.5), 0.5)
  expect_gt(logoDelta(c(3, 3)), 0)
})

test_that("il logo si disegna quando i valori di entrambi gli assi sono uguali", {
  skip_if_not_installed("ggplot2")
  # La forma esatta che fermava il grafico della produzione annuale su una
  # collezione filtrata su un solo anno.
  Y <- data.frame(Year = 2020L, Freq = 12L)
  x <- c(max(Y$Year) - 0.02 - logoDelta(Y$Year, frac = 0.125), max(Y$Year) - 0.02) + 1
  y <- c(min(Y$Freq), min(Y$Freq) + logoDelta(Y$Freq, frac = 0.125))
  expect_gt(diff(x), 0)
  expect_gt(diff(y), 0)

  data("logo", package = "bibliometrix", envir = environment())
  g <- ggplot2::ggplot(Y, ggplot2::aes(x = Year, y = Freq)) +
    ggplot2::geom_point() +
    ggplot2::annotation_custom(
      grid::rasterGrob(logo, interpolate = TRUE),
      xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]
    )
  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(g))
})

test_that("bradford disegna il logo su una collezione con una sola rivista", {
  skip_if_not_installed("bibliometrixData")
  data(scientometrics, package = "bibliometrixData")
  class(scientometrics) <- c("bibliometrixDB", "data.frame")
  B <- bradford(scientometrics)
  expect_equal(nrow(B$table), 1) # una sola rivista: log(Rank) e' sempre 0
  box <- Filter(
    function(l) inherits(l$geom, "GeomCustomAnn"),
    B$graph$layers
  )[[1]]
  expect_gt(box$geom_params$xmax - box$geom_params$xmin, 0)
  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(B$graph))
})

test_that("utils.R di Biblioshiny prende logoDelta dal pacchetto", {
  path <- system.file("biblioshiny", "utils.R", package = "bibliometrix")
  skip_if(path == "", "utils.R non disponibile")
  src <- readLines(path, warn = FALSE)
  expect_true(any(grepl("logoDelta <- bibliometrix:::logoDelta", src, fixed = TRUE)))
  expect_true(is.function(logoDelta))
})
