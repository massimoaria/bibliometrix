# Le opere il cui DOI authorCard() manda a OpenAlex per trovare l'autore.
# authorCardWorks() vive in inst/biblioshiny/utils.R, fuori dal namespace.
authorcard_helper <- function() {
  path <- system.file("biblioshiny", "utils.R", package = "bibliometrix")
  if (!nzchar(path) || !file.exists(path)) {
    path <- testthat::test_path("..", "..", "inst", "biblioshiny", "utils.R")
  }
  skip_if_not(file.exists(path), "inst/biblioshiny/utils.R non trovato")
  env <- new.env(parent = globalenv())
  for (e in as.list(parse(path))) {
    if (is.call(e) && identical(as.character(e[[1]]), "<-") &&
        identical(as.character(e[[2]]), "authorCardWorks")) {
      eval(e, envir = env)
    }
  }
  env$authorCardWorks
}

test_that("authorCardWorks mette prima gli articoli di rivista, poi l'ordine dei DOI", {
  authorCardWorks <- authorcard_helper()
  M <- data.frame(
    DI = c("10.1/A", "10.1/B", "10.1/C", "10.1/D", "10.1/E"),
    DT = c("BOOK CHAPTER", "ARTICLE", "CONFERENCE PAPER", "REVIEW", "ARTICLE; EARLY ACCESS"),
    stringsAsFactors = FALSE
  )
  works <- data.frame(doi = c("10.1/A", "10.1/B", "10.1/B", "10.1/C", "10.1/D", "10.1/E"),
                      author_position = c(1, 2, 2, 1, 3, 1), stringsAsFactors = FALSE)
  out <- authorCardWorks(works, M, n = 5)
  expect_equal(out$doi, c("10.1/B", "10.1/D", "10.1/E", "10.1/A", "10.1/C"))
  expect_equal(authorCardWorks(works, M, n = 2)$doi, c("10.1/B", "10.1/D"))
  # senza DT resta l'ordine dei DOI, senza duplicati
  expect_equal(authorCardWorks(works, M["DI"], n = 5)$doi, paste0("10.1/", LETTERS[1:5]))
})
