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

test_that("Hindex include elementi con zero citazioni con h-index pari a zero", {
  M <- data.frame(
    AU = c("SMITH J", "DOE J", "DOE J"),
    TC = c(10, 0, 0),
    PY = c(2020L, 2021L, 2022L),
    SO = c("JOURNAL A", "JOURNAL B", "JOURNAL B"),
    stringsAsFactors = FALSE
  )
  H <- Hindex(M, field = "author")
  expect_equal(nrow(H$H), 2)
  expect_true("DOE J" %in% H$H$Element)
  doe <- H$H[H$H$Element == "DOE J", ]
  expect_equal(doe$h_index, 0L)
  expect_equal(doe$g_index, 0L)
  expect_equal(doe$m_index, 0)
  expect_equal(doe$TC, 0)
  expect_equal(doe$NP, 2)
  expect_equal(sort(names(H$CitationList)), sort(H$H$Element))
})

test_that("Hindex restituisce una riga quando elements seleziona un elemento a zero citazioni", {
  M <- data.frame(
    AU = "DOE J",
    TC = 0,
    PY = 2022L,
    SO = "JOURNAL B",
    stringsAsFactors = FALSE
  )
  H <- Hindex(M, field = "author", elements = "DOE J")
  expect_equal(nrow(H$H), 1)
  expect_equal(H$H$Element, "DOE J")
  expect_equal(H$H$h_index, 0L)
})

test_that("Hindex calcola indici per source con zero citazioni", {
  M <- data.frame(
    AU = c("SMITH J", "DOE J"),
    TC = c(10, 0),
    PY = c(2020L, 2022L),
    SO = c("NATURE", "NEW JOURNAL"),
    stringsAsFactors = FALSE
  )
  H <- Hindex(M, field = "source")
  expect_equal(nrow(H$H), 2)
  expect_true("NEW JOURNAL" %in% H$H$Element)
  so <- H$H[H$H$Element == "NEW JOURNAL", ]
  expect_equal(so$h_index, 0L)
  expect_equal(so$g_index, 0L)
})

