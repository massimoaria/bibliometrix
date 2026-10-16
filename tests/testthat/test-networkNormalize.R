# networkPlot(): with a normalisation the communities, their modularity and the
# PageRank are those of the normalised network; without one, or with
# normalize.clusters = FALSE, they are those of the counts.

norm_fixture <- function() {
  data(management, package = "bibliometrixData", envir = environment())
  biblioNetwork(management, analysis = "co-occurrences", network = "keywords",
                n = 150, sep = ";")
}

plot_net <- function(NM, ...) {
  suppressWarnings(networkPlot(NM, n = 100, type = "auto", cluster = "louvain",
                               remove.isolates = TRUE, weighted = TRUE,
                               seed = 1234, verbose = FALSE, ...))
}

test_that("with a normalisation the communities are those of the normalised network", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  NM <- norm_fixture()
  for (type in c("association", "salton")) {
    net <- plot_net(NM, normalize = type)
    g <- net$graph
    el <- igraph::as_edgelist(g)
    S <- normalizeSimilarity(NM, type)
    rownames(S) <- colnames(S) <- tolower(rownames(S))
    gS <- g
    igraph::E(gS)$weight <- as.numeric(S[el])
    ref <- clusteringNetwork(gS, "louvain", seed = 1234)$net_groups
    expect_identical(net$cluster_obj$membership, ref$membership)
    expect_equal(net$modularity, igraph::modularity(gS, ref$membership))
    expect_equal(
      net$cluster_res$pagerank_centrality,
      unname(igraph::page_rank(gS)$vector[match(net$cluster_res$vertex, igraph::V(gS)$name)])
    )
    # the edges keep the widths of the counts
    expect_identical(igraph::E(g)$weight, as.numeric(as.matrix(NM)[toupper(el)]))
  }
})

test_that("normalize.clusters = FALSE keeps the communities of the counts", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  NM <- norm_fixture()
  a <- plot_net(NM, normalize = "association", normalize.clusters = FALSE)
  b <- plot_net(NM)
  expect_identical(a$cluster_obj$membership, b$cluster_obj$membership)
})

test_that("without a normalisation modularity and PageRank are those of the counts", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  NM <- norm_fixture()
  net <- plot_net(NM, community.repulsion = 0.5)
  g <- net$graph
  el <- igraph::as_edgelist(g)
  # the layout altered a copy of the weights, not the network
  expect_identical(igraph::E(g)$weight, as.numeric(as.matrix(NM)[toupper(el)]))
  expect_equal(net$modularity, igraph::modularity(g, net$cluster_obj$membership))
  expect_equal(
    net$cluster_res$pagerank_centrality,
    unname(igraph::page_rank(g)$vector[match(net$cluster_res$vertex, igraph::V(g)$name)])
  )
})
