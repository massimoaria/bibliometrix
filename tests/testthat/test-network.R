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

# Con OpenAlex CR contiene gli id dei work citati (W + cifre). Il filtro che
# scarta le stringhe di riferimento con 10 caratteri o meno eliminava gli id
# piu' corti (7-10 caratteri): l'1,7% dei riferimenti di una raccolta reale,
# in tutte le analisi basate su CR.

test_that("cocMatrix mantiene gli id OpenAlex corti in CR", {
  M <- data.frame(
    SR = c("D1", "D2", "D3"), DB = "OPENALEX",
    CR = c("W1234567; W2741809807; W99887766", "W1234567; W99887766", "W2741809807"),
    stringsAsFactors = FALSE
  )
  row.names(M) <- M$SR
  WR <- cocMatrix(M, Field = "CR", sep = ";")
  expect_setequal(colnames(WR), c("W1234567", "W2741809807", "W99887766"))
  expect_equal(as.numeric(Matrix::rowSums(WR)), c(3, 2, 1))
})

test_that("cocMatrix scarta ancora le stringhe CR troppo corte che non sono id", {
  M <- data.frame(
    SR = c("D1", "D2"), DB = "ISI",
    CR = c("ANONYMOUS;SMITH J, 2001, J INFORMETR, V1, P1", "SMITH J, 2001, J INFORMETR, V1, P1;NO TITLE"),
    stringsAsFactors = FALSE
  )
  row.names(M) <- M$SR
  WR <- cocMatrix(M, Field = "CR", sep = ";")
  expect_equal(ncol(WR), 1L)
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

# normalizeSimilarity() costruiva matrici dense n x n con outer(): su una
# co-citazione di 10.000 riferimenti erano quasi 5 GB. Ora lavora solo sulle
# celle non nulle; i valori e la classe restituita devono restare quelli di
# prima, cioe' della formula applicata alla matrice densa.

test_that("normalizeSimilarity coincide con la formula densa per ogni indice", {
  C <- Matrix::Matrix(c(
    4, 2, 0, 1, 0,
    2, 3, 1, 0, 0,
    0, 1, 2, 0, 0,
    1, 0, 0, 1, 0,
    0, 0, 0, 0, 0
  ), 5, sparse = TRUE)
  dimnames(C) <- list(letters[1:5], letters[1:5])
  A <- as.matrix(C)
  D <- diag(A)
  ref <- list(
    association = A / outer(D, D),
    inclusion = A / outer(D, D, pmin),
    jaccard = A / (outer(D, D, "+") - A),
    salton = A / sqrt(outer(D, D)),
    equivalence = (A / sqrt(outer(D, D)))^2
  )
  for (ty in names(ref)) {
    r <- ref[[ty]]
    r[is.nan(r)] <- 0
    S <- normalizeSimilarity(C, type = ty)
    expect_s4_class(S, "dsCMatrix")
    expect_equal(as.matrix(S), r, info = ty)
    # la stessa matrice passata come matrix di base
    expect_equal(as.matrix(normalizeSimilarity(A, type = ty)), r, info = ty)
  }
})

test_that("normalizeSimilarity resta sparsa e simmetrica su una rete reale", {
  M <- load_wos_fixture()
  NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
  S <- normalizeSimilarity(NetMatrix, type = "association")
  expect_s4_class(S, "dsCMatrix")
  expect_equal(Matrix::nnzero(S), Matrix::nnzero(NetMatrix))
  expect_equal(dimnames(S), dimnames(NetMatrix))
})

test_that("normalizeSimilarity rifiuta un tipo sconosciuto", {
  C <- Matrix::Matrix(diag(2), sparse = TRUE)
  expect_error(normalizeSimilarity(C, type = "cosine"), "type must be one of")
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

# La colorazione degli archi e i pesi di community.repulsion erano calcolati
# arco per arco (apply() e un ciclo con which() sui nomi): su una rete di
# 1000 riferimenti co-citati, 120.000 archi, erano i tre quarti del tempo di
# networkPlot(). Ora sono vettoriali; il risultato deve restare lo stesso.

test_that("clusteringNetwork colora gli archi interni e grigi quelli tra comunita'", {
  g <- igraph::make_graph(c(1, 2, 2, 3, 3, 1, 4, 5, 5, 6, 6, 4, 3, 4), directed = FALSE)
  igraph::V(g)$name <- letters[1:6]
  res <- suppressWarnings(clusteringNetwork(g, "louvain", seed = 7))
  comm <- igraph::V(res$bsk.network)$community
  el <- igraph::as_edgelist(res$bsk.network, names = FALSE)
  inside <- comm[el[, 1]] == comm[el[, 2]]
  expect_equal(igraph::E(res$bsk.network)$color[inside], colorlist()[comm[el[inside, 1]]])
  expect_equal(igraph::E(res$bsk.network)$color[!inside], "gray70")
  expect_equal(igraph::E(res$bsk.network)$lty, ifelse(inside, 1, 5))
})

test_that("switchLayout rafforza gli archi interni e indebolisce quelli tra comunita'", {
  g <- igraph::make_graph(c(1, 2, 2, 3, 3, 1, 4, 5, 5, 6, 6, 4, 3, 4), directed = FALSE)
  igraph::V(g)$name <- letters[1:6]
  igraph::V(g)$community <- c(1, 1, 1, 2, 2, 2)
  igraph::E(g)$weight <- 2
  w <- igraph::E(switchLayout(g, "circle", 0.5)$bsk.network)$weight
  inside <- c(rep(TRUE, 6), FALSE)
  expect_true(all(w[inside] > 2))
  expect_true(all(w[!inside] < 2))
  expect_equal(length(unique(w[inside])), 1L)
  # senza repulsione i pesi restano quelli di partenza
  expect_equal(igraph::E(switchLayout(g, "circle", 0)$bsk.network)$weight, rep(2, 7))
})
