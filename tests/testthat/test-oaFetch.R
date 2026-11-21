# Test per oaFetch: i lavori di OpenAlex con un doi anche in primary_location

test_that("i campi di primary_location che ripetono quelli del lavoro vengono tolti", {
  work <- list(
    id = "https://openalex.org/W1",
    doi = "https://doi.org/10.1/x",
    title = "A work",
    primary_location = list(
      id = "doi:10.1/x",
      doi = "https://doi.org/10.1/x",
      is_oa = TRUE,
      landing_page_url = "https://doi.org/10.1/x",
      source = list(id = "https://openalex.org/S1", display_name = "A journal")
    )
  )
  fixed <- oaDropRepeatedLocationFields(work)
  expect_named(
    fixed$primary_location,
    c("id", "is_oa", "landing_page_url", "source")
  )
  expect_identical(fixed$primary_location$source, work$primary_location$source)
  expect_identical(fixed[names(fixed) != "primary_location"], work[names(work) != "primary_location"])
})

test_that("un lavoro senza primary_location resta com'e'", {
  work <- list(id = "https://openalex.org/W1", doi = NULL, primary_location = NULL)
  expect_identical(oaDropRepeatedLocationFields(work), work)
  work2 <- list(id = "https://openalex.org/W2", primary_location = list())
  expect_identical(oaDropRepeatedLocationFields(work2), work2)
})

test_that("oaFetch restituisce i lavori in una tibble con nomi unici", {
  skip_on_cran()
  skip_if_offline("api.openalex.org")
  w <- oaFetch(entity = "works", doi = "10.1016/j.joi.2017.08.007")
  expect_s3_class(w, "data.frame")
  expect_equal(nrow(w), 1)
  expect_false(any(duplicated(names(w))))
  expect_equal(w$doi, "https://doi.org/10.1016/j.joi.2017.08.007")
  expect_equal(w$source_display_name, "Journal of Informetrics")
})

# OpenAlex manda observed_orcids = list() per un autore senza ORCID: openalexR
# riciclava la riga dell'autore a lunghezza 0 e l'autore spariva da authorships
oa_test_author <- function(name, orcid) {
  list(
    author_position = "middle",
    author = list(
      id = paste0("https://openalex.org/A", name), display_name = name, orcid = orcid,
      observed_orcids = if (is.null(orcid)) list() else list(orcid)
    ),
    is_corresponding = FALSE, raw_affiliation_strings = list(), institutions = list()
  )
}

test_that("i campi vuoti di un autore diventano NULL, gli altri restano", {
  work <- list(id = "https://openalex.org/W1", authorships = list(
    oa_test_author("Anna", "https://orcid.org/0000-0000-0000-0001"),
    oa_test_author("Bruno", NULL)
  ))
  fixed <- oaKeepEmptyAuthorFields(work)
  expect_identical(fixed$authorships[[1]], work$authorships[[1]])
  expect_named(fixed$authorships[[2]]$author, c("id", "display_name", "orcid", "observed_orcids"))
  expect_null(fixed$authorships[[2]]$author$observed_orcids)
  expect_identical(fixed$authorships[[2]]$author$display_name, "Bruno")
  no_auth <- list(id = "https://openalex.org/W2", authorships = list())
  expect_identical(oaKeepEmptyAuthorFields(no_auth), no_auth)
})

test_that("un autore senza ORCID resta in authorships dopo oa2df", {
  skip_if_not_installed("openalexR")
  work <- list(id = "https://openalex.org/W1", display_name = "A work", authorships = list(
    oa_test_author("Anna", "https://orcid.org/0000-0000-0000-0001"),
    oa_test_author("Bruno", NULL)
  ))
  a <- openalexR::oa2df(list(oaKeepEmptyAuthorFields(work)), entity = "works")$authorships[[1]]
  expect_equal(a$display_name, c("Anna", "Bruno"))
  expect_equal(a$orcid, c("https://orcid.org/0000-0000-0000-0001", NA))
})

test_that("oaFetch e authorBio trovano l'ultimo autore senza ORCID", {
  skip_on_cran()
  skip_if_offline("api.openalex.org")
  w <- oaFetch(entity = "works", doi = "10.1177/1094428108319845")
  expect_equal(w$authorships[[1]]$display_name,
               c("Arthur G. Bedeian", "David D. Van Fleet", "Hugh H. Hyman"))
  g <- authorBio(doi = "10.1177/1094428108319845", author_name = "HYMAN, HUGH H., III")
  expect_equal(g$display_name, "Hugh H. Hyman")
})
