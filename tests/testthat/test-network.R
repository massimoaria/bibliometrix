# Test per biblioNetwork, cocMatrix, networkStat, networkPlot, normalizeSimilarity

test_that("biblioNetwork crea matrice co-citation per references", {
  M <- load_wos_fixture()
  NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
  expect_true(inherits(NetMatrix, "Matrix") || inherits(NetMatrix, "matrix"))
  expect_equal(nrow(NetMatrix), ncol(NetMatrix))
  expect_true(nrow(NetMatrix) > 0)
})

test_that("biblioNetwork crea matrice collaboration per autori", {
  M <- load_wos_fixture()
  NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "authors", sep = ";")
  expect_true(nrow(NetMatrix) > 0)
  expect_equal(nrow(NetMatrix), ncol(NetMatrix))
})

test_that("biblioNetwork crea matrice co-occurrences per keywords", {
  M <- load_wos_fixture()
  NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
  expect_true(nrow(NetMatrix) > 0)
  expect_equal(nrow(NetMatrix), ncol(NetMatrix))
})

test_that("biblioNetwork crea matrice coupling per references", {
  M <- load_wos_fixture()
  NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ";")
  expect_true(nrow(NetMatrix) > 0)
  expect_equal(nrow(NetMatrix), ncol(NetMatrix))
})

test_that("cocMatrix crea matrice bipartita sparsa", {
  M <- load_wos_fixture()
  WA <- cocMatrix(M, Field = "AU", type = "sparse", sep = ";")
  expect_true(inherits(WA, "dgCMatrix") || inherits(WA, "Matrix") || inherits(WA, "sparseMatrix"))
  expect_equal(nrow(WA), nrow(M))
})

test_that("networkStat calcola statistiche di rete", {
  M <- load_wos_fixture()
  NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
  ns <- networkStat(NetMatrix)
  expect_type(ns, "list")
  expect_true("network" %in% names(ns))
})

test_that("normalizeSimilarity calcola indici di similarita", {
  M <- load_wos_fixture()
  NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
  S <- normalizeSimilarity(NetMatrix, type = "association")
  expect_true(inherits(S, "Matrix") || is.matrix(S))
})

test_that("networkPlot genera output senza errori", {
  skip_on_cran()
  M <- load_wos_fixture()
  NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
  net <- expect_no_error(
    suppressWarnings(networkPlot(NetMatrix, n = 10, type = "auto", verbose = FALSE))
  )
  expect_true("graph" %in% names(net))
  expect_true(inherits(net$graph, "igraph"))
})
