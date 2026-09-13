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

test_that("cocMatrix mantiene due dimensioni con un solo termine distinto", {
  # Un solo termine distinto produceva un vettore invece di una matrice
  mk <- function(de) {
    M <- data.frame(
      AU = "A;B", SO = "S", PY = 2020, TC = 1, DE = de,
      SR = paste0("D", seq_along(de)), stringsAsFactors = FALSE
    )
    row.names(M) <- M$SR
    class(M) <- c("bibliometrixDB", "data.frame")
    M
  }

  WF <- cocMatrix(mk(c("X", "X", "X")), Field = "DE", type = "matrix", sep = ";")
  expect_equal(dim(WF), c(3L, 1L))

  WS <- cocMatrix(mk(c("X", "X", "X")), Field = "DE", type = "sparse", sep = ";")
  expect_equal(dim(WS), c(3L, 1L))

  # il caso multi-termine resta invariato
  expect_equal(dim(cocMatrix(mk(c("X", "Y", "X")), Field = "DE", type = "matrix", sep = ";")), c(3L, 2L))
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

# Una rete in cui nessuna coppia di elementi e' collegata resta senza nodi dopo
# la rimozione degli isolati. clusteringNetwork() confrontava allora una
# modularita' NaN e si fermava con "missing value where TRUE/FALSE needed";
# con walktrap, apply() su una lista di archi vuota dava "argument is of length
# zero". Basta una collezione filtrata su un solo anno (nessuna coppia di autori
# che scrive insieme) o su una sola rivista (nessuna coppia di keyword).

test_that("networkPlot rifiuta una rete in cui nessuna coppia e' collegata", {
  NM <- Matrix::Matrix(diag(c(3, 2, 1)), sparse = TRUE)
  dimnames(NM) <- list(c("A", "B", "C"), c("A", "B", "C"))
  expect_error(
    networkPlot(NM, n = 3, remove.isolates = TRUE, verbose = FALSE, cluster = "louvain", seed = 1),
    "no two items of this network are linked, so once"
  )
  # Un solo legame, ma sotto edges.min: stesso caso, e il messaggio lo dice.
  NM[1, 2] <- NM[2, 1] <- 1
  expect_error(
    networkPlot(NM, n = 3, remove.isolates = TRUE, edges.min = 2, verbose = FALSE, cluster = "louvain", seed = 1),
    "linked at least 2 times \\(edges.min\\)"
  )
})

test_that("clusteringNetwork accetta un grafo senza archi", {
  g <- igraph::make_empty_graph(3, directed = FALSE)
  igraph::V(g)$name <- c("a", "b", "c")
  for (cl in c("louvain", "leiden", "walktrap")) {
    expect_no_error(res <- suppressWarnings(clusteringNetwork(g, cl, seed = 1)))
    expect_equal(length(igraph::V(res$bsk.network)$community), 3)
  }
})

test_that("clusteringNetwork con seed resta invariato su un grafo con archi", {
  g <- igraph::make_graph(c(1, 2, 2, 3, 3, 1, 4, 5, 5, 6, 6, 4, 3, 4), directed = FALSE)
  a <- suppressWarnings(clusteringNetwork(g, "louvain", seed = 7))
  expect_equal(max(a$net_groups$membership), 2)
  expect_equal(igraph::modularity(g, a$net_groups$membership), 0.3571429, tolerance = 1e-6)
})
