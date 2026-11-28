# geminiRequest() lives in the Biblioshiny sources, not in the package
# namespace: evaluate it from biblioAI.R.
gemini_request_helper <- function() {
  path <- system.file("biblioshiny", "biblioAI.R", package = "bibliometrix")
  env <- new.env(parent = globalenv())
  for (expr in as.list(parse(path, encoding = "UTF-8"))) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        identical(expr[[2]], as.name("geminiRequest"))) {
      eval(expr, envir = env)
    }
  }
  env$geminiRequest
}

test_that("the Gemini key goes in the x-goog-api-key header, not in the URL", {
  skip_if_not_installed("httr2", "1.1.0")
  gemini_request <- gemini_request_helper()
  url <- "https://generativelanguage.googleapis.com/v1beta/models"

  for (key in c("AQ.Ab8RN6example", "AIzaSyExample")) {
    req <- gemini_request(url, key)
    expect_identical(req$url, url)
    expect_false(grepl(key, req$url, fixed = TRUE))
    expect_identical(
      httr2::req_get_headers(req, "reveal")[["x-goog-api-key"]],
      key
    )
    expect_false(any(grepl(key, utils::capture.output(print(req)),
                           fixed = TRUE)))
  }
})

test_that("no Gemini request puts the key in the URL", {
  path <- system.file("biblioshiny", "biblioAI.R", package = "bibliometrix")
  src <- readLines(path, warn = FALSE, encoding = "UTF-8")
  expect_false(any(grepl("req_url_query\\([^)]*key\\s*=", src)))
})

test_that("a saved model removed from the menu is replaced by its successor", {
  path <- system.file("biblioshiny", "biblioAI.R", package = "bibliometrix")
  env <- new.env(parent = globalenv())
  for (expr in as.list(parse(path, encoding = "UTF-8"))) {
    if (is.call(expr) && as.character(expr[[1]]) %in% c("<-", "=") &&
        identical(expr[[2]], as.name("loadGeminiModel"))) {
      eval(expr, envir = env)
    }
  }
  file <- tempfile()
  on.exit(unlink(file))
  saved <- c("2.5-flash" = "3.5-flash-lite", "3-flash-preview" = "3.6-flash",
             "3.8-flash" = "3.8-flash", "2.5-flash-lite" = "2.5-flash-lite")
  for (m in names(saved)) {
    writeLines(c(m, "large"), file)
    expect_identical(env$loadGeminiModel(file), c(saved[[m]], "large"))
  }
  expect_identical(env$loadGeminiModel(tempfile()), c("3.5-flash-lite", "medium"))
})

# Biblio AI could close its answer with "How would you like to proceed?" or
# with offers of further analyses, and the panel gives no way to reply: every
# prompt now ends with the response rules.

test_that("every Biblio AI prompt ends with the response rules", {
  path <- system.file("biblioshiny", "biblioAI.R", package = "bibliometrix")
  env <- new.env(parent = globalenv())
  wanted <- c("GEMINI_ENDPOINT_HOST", "BIBLIO_AI_CITATION_RULES",
              "BIBLIO_AI_RESPONSE_RULES", "geminiPrepareImage")
  for (expr in as.list(parse(path, encoding = "UTF-8"))) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        as.character(expr[[2]]) %in% wanted) {
      eval(expr, envir = env)
    }
  }
  expect_true(all(wanted %in% ls(env)))
  env$is_Online <- function(...) TRUE
  env$getWD <- function() tempdir()

  out <- env$geminiPrepareImage(NULL, type = "text", prompt = "Interpret this table.",
    key = TRUE, desc = " Description of the analysis.",
    values = list(Chrome_url = "chrome"))
  expect_null(out$error_msg)
  expect_true(startsWith(out$prompt, "Interpret this table. Description"))
  expect_true(endsWith(out$prompt, env$BIBLIO_AI_RESPONSE_RULES))

  src <- readLines(path, warn = FALSE, encoding = "UTF-8")
  expect_false(any(grepl("creted", src, fixed = TRUE)))
  expect_false(any(grepl("guidance for data-driven interpretation", src, fixed = TRUE)))
})

# Evaluate the named top-level assignments of biblioAI.R in a fresh env.
biblio_ai_defs <- function(names) {
  env <- new.env(parent = globalenv())
  for (expr in as.list(parse(system.file("biblioshiny", "biblioAI.R", package = "bibliometrix"), encoding = "UTF-8"))) {
    if (is.call(expr) && as.character(expr[[1]]) %in% c("<-", "=") &&
        as.character(expr[[2]]) %in% names) {
      eval(expr, envir = env)
    }
  }
  env
}

test_that("doc2clust() sends title, year and citations with the SR", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
  })
  env <- biblio_ai_defs("doc2clust")
  res <- list(doc2clust = data.frame(
    Assigned_cluster = c("a", "a", "a", "a", "b", NA),
    SR = c("X, 2020, J", "Y, 2019, K", "V, 2018, M", "U, 2017, N",
           "Z, 2021, L", "W, 2000, Q"),
    TI = c("A title; with\nbreak", NA, "t3", "t4", "Third", "x"),
    PY = c(2020, 2019, 2018, 2017, NA, 2000),
    TC = c(5, 0, 1, 1, 3, 1),
    pagerank = c(0.1, 0.2, 0.05, 0.01, 0.3, 0.4)
  ))
  out <- as.data.frame(env$doc2clust(res))
  expect_identical(names(out), c("Assigned_cluster", "cluster"))
  expect_identical(nrow(out), 4L) # top 3 of "a" + "b"; NA cluster dropped
  expect_true(
    'X, 2020, J, title "A title, with break", year 2020, total citations 5, pagerank 0.1' %in%
      out$cluster
  )
  # a missing value is left out, not sent as NA
  expect_true("Y, 2019, K, year 2019, total citations 0, pagerank 0.2" %in% out$cluster)
  expect_true('Z, 2021, L, title "Third", total citations 3, pagerank 0.3' %in% out$cluster)
  expect_false(any(grepl("NA|;", out$cluster)))

  # a collection without TI / TC: the SR alone, as before
  res$doc2clust$TI <- NULL
  res$doc2clust$TC <- NULL
  out <- as.data.frame(env$doc2clust(res))
  expect_true("Z, 2021, L, pagerank 0.3" %in% out$cluster)
})

test_that("the response rules end with the citation rules", {
  env <- biblio_ai_defs(c("BIBLIO_AI_CITATION_RULES", "BIBLIO_AI_RESPONSE_RULES"))
  rules <- env$BIBLIO_AI_RESPONSE_RULES
  expect_true(endsWith(rules, env$BIBLIO_AI_CITATION_RULES))
  expect_match(env$BIBLIO_AI_CITATION_RULES, "use only those that appear in the data above")
  expect_match(env$BIBLIO_AI_CITATION_RULES, "methodological references")
})
