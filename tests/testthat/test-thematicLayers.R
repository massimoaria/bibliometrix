# Test per plotThematicLayers

teLayersNexus <- local({
  cache <- list()
  function(assign = TRUE) {
    key <- as.character(assign)
    if (is.null(cache[[key]])) {
      data(management, package = "bibliometrixData", envir = environment())
      cache[[key]] <<- suppressMessages(thematicEvolution(
        management,
        field = "DE",
        years = c(2004, 2008, 2013),
        n = 250,
        minFreq = 2,
        assign.evolution.colors = list(assign = assign)
      ))
    }
    cache[[key]]
  }
})

test_that("plotThematicLayers disegna un ggplot senza avvisi", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  nexus <- teLayersNexus()
  p <- plotThematicLayers(nexus)
  expect_s3_class(p, "ggplot")
  expect_no_warning(ggplot2::ggplot_build(p))
})

test_that("ogni tema sta nel suo piano e i link uniscono periodi adiacenti", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  nexus <- teLayersNexus()
  g <- teLayerGeometry(nexus)
  n_clusters <- sum(vapply(nexus$TM, function(t) nrow(t$clusters), 1L))
  expect_equal(nrow(g$points), n_clusters)
  expect_true(all(abs(g$points$x) <= 1 & abs(g$points$y) <= 1))
  # the projection keeps each plane in its own band
  for (i in seq_along(g$periods)) {
    band <- range(g$planes$Y[g$planes$slice == i])
    y <- g$points$Y[g$points$slice == i]
    expect_true(all(y > band[1] & y < band[2]))
  }
  slice <- stats::setNames(g$points$slice, g$points$label)
  expect_true(all(slice[g$edges$lab_to] - slice[g$edges$lab_from] == 1))
  expect_true(all(g$points$rel_freq <= 1) && all(g$points$rel_freq > 0))
  expect_equal(
    tapply(g$points$rel_freq, g$points$slice, max),
    stats::setNames(rep(1, length(g$periods)), seq_along(g$periods)),
    ignore_attr = TRUE
  )
})

test_that("un tema prende il colore della sua linea, gli altri restano grigi", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  g <- teLayerGeometry(teLayersNexus())
  lin <- g$edges[g$edges$lineage, ]
  expect_gt(nrow(lin), 0)
  expect_true(any(!g$edges$lineage))
  ends <- unique(c(lin$lab_from, lin$lab_to))
  expect_setequal(g$points$label[g$points$lineage], ends)
  expect_true(all(g$points$colour[!g$points$lineage] == TE_GREY))
  for (k in seq_len(nrow(lin))) {
    expect_equal(g$points$colour[g$points$label == lin$lab_from[k]], lin$colour[k])
    expect_equal(g$points$colour[g$points$label == lin$lab_to[k]], lin$colour[k])
  }
})

test_that("min.flow toglie i link deboli", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  nexus <- teLayersNexus()
  all_links <- teLayerGeometry(nexus)$edges
  strong <- teLayerGeometry(nexus, min.flow = 0.4)$edges
  expect_lt(nrow(strong), nrow(all_links))
  expect_true(all(strong$weight >= 0.4))
})

test_that("senza i colori delle linee tutti i link sono grigi e i temi pure", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  g <- teLayerGeometry(teLayersNexus(assign = FALSE))
  expect_true(all(g$edges$lineage))
  expect_true(all(g$edges$colour == TE_NO_LINEAGE))
  expect_true(all(g$points$colour == TE_GREY))
  expect_s3_class(plotThematicLayers(teLayersNexus(assign = FALSE)), "ggplot")
})

test_that("la versione interattiva ha un trace per link e il click sui temi", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  nexus <- teLayersNexus()
  w <- plotThematicLayers(nexus, interactive = TRUE)
  expect_s3_class(w, "plotly")
  b <- plotly::plotly_build(w)$x$data
  names <- vapply(b, function(tr) if (is.null(tr$name)) "" else tr$name, "")
  expect_equal(sum(names == "tl_link"), nrow(teLayerGeometry(nexus)$edges))
  expect_equal(sum(names == "tl_themes"), 1)
  expect_length(w$jsHooks$render, 1)
  # only the themes answer hover and click: a per-point "skip" would not
  # take the links out, and a click on a theme went to its link
  hi <- lapply(b[names != "tl_themes"], function(tr) tr$hoverinfo)
  expect_true(all(vapply(hi, identical, TRUE, "skip")))
})

test_that("plotThematicLayers rifiuta un oggetto che non viene da thematicEvolution", {
  expect_error(plotThematicLayers(list(a = 1)), "thematicEvolution")
  expect_error(plotThematicLayers(list(TM = list(), Nodes = 1, Edges = 1, check = FALSE)), "thematicEvolution")
})

test_that("plotThematicLayers dice quali colonne mancano ai cluster", {
  skip_on_cran()
  skip_if_not_installed("bibliometrixData")
  nexus <- teLayersNexus()
  # Biblioshiny renames the columns of the clusters for its tables
  names(nexus$TM[[1]]$clusters)[names(nexus$TM[[1]]$clusters) == "rcentrality"] <- "RankCentrality"
  expect_error(plotThematicLayers(nexus), "rcentrality")
})
