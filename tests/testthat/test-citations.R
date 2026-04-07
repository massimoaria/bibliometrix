# Test per citations, localCitations, Hindex

test_that("citations calcola distribuzione citazioni per articolo", {
  M <- load_wos_fixture()
  CR <- citations(M, field = "article", sep = ";")
  expect_type(CR, "list")
  expect_true("Cited" %in% names(CR))
  expect_true(length(CR$Cited) > 0)
})

test_that("citations calcola distribuzione per autore", {
  M <- load_wos_fixture()
  CR <- citations(M, field = "author", sep = ";")
  expect_type(CR, "list")
  expect_true("Cited" %in% names(CR))
  expect_true(length(CR$Cited) > 0)
})

test_that("citations funziona con dati Scopus", {
  M <- load_scopus_fixture()
  CR <- citations(M, field = "article", sep = ";")
  expect_type(CR, "list")
  expect_true(length(CR$Cited) > 0)
})

test_that("localCitations calcola citazioni locali", {
  M <- load_wos_fixture()
  LC <- expect_no_error(
    suppressWarnings(suppressMessages(localCitations(M, fast.search = TRUE, verbose = FALSE)))
  )
  expect_type(LC, "list")
  expect_true(all(c("Authors", "Papers", "M") %in% names(LC)))
  expect_true(is.data.frame(LC$Authors))
  expect_true(is.data.frame(LC$Papers))
})

test_that("Hindex calcola indici per autore", {
  M <- load_wos_fixture()
  first_au <- trimws(strsplit(M$AU[1], ";")[[1]][1])
  H <- Hindex(M, field = "author", elements = first_au, sep = ";")
  expect_type(H, "list")
  expect_true("H" %in% names(H))
  expect_true("CitationList" %in% names(H))
  expect_true(is.data.frame(H$H))
  expect_true(all(c("h_index", "g_index", "m_index") %in% names(H$H)))
})

test_that("Hindex calcola indici per source", {
  M <- load_wos_fixture()
  first_so <- M$SO[1]
  H <- Hindex(M, field = "source", elements = first_so, sep = ";")
  expect_true(is.data.frame(H$H))
})
