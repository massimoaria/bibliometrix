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
