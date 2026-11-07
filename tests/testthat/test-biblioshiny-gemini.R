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
