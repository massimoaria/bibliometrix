upload_settings_helper <- function() {
  path <- system.file("biblioshiny", "utils.R", package = "bibliometrix")
  env <- new.env(parent = globalenv())
  for (expr in as.list(parse(path))) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        identical(expr[[2]], as.name("resolveUploadSettings"))) {
      eval(expr, envir = env)
    }
  }
  env$resolveUploadSettings
}

mock_upload <- function(name, lines = "") {
  # Reproduce uploads whose temporary storage name has lost the suffix.
  path <- tempfile()
  writeLines(lines, path, useBytes = TRUE)
  data.frame(name = name, datapath = path, stringsAsFactors = FALSE)
}

scopus_bib_header <- c("Scopus", "EXPORT DATE: 06 November 2026", "",
                      "@ARTICLE{example,", "  title = {Example title},")

test_that("Scopus BibTeX recovers from saved-collection mode and default WoS", {
  resolve <- upload_settings_helper()
  upload <- mock_upload("export.bib", scopus_bib_header)
  on.exit(unlink(upload$datapath))

  result <- resolve(upload, "load", "isi")
  expect_identical(result$mode, "import")
  expect_identical(result$dbsource, "scopus")
  expect_match(result$notice, "Detected a Scopus BibTeX")
})

test_that("correct Scopus import settings are retained without a notice", {
  resolve <- upload_settings_helper()
  upload <- mock_upload("export.bib", scopus_bib_header)
  on.exit(unlink(upload$datapath))

  result <- resolve(upload, "import", "scopus")
  expect_identical(result$mode, "import")
  expect_identical(result$dbsource, "scopus")
  expect_length(result$notice, 0)
})

test_that("recovered settings import an actual Scopus BibTeX collection", {
  resolve <- upload_settings_helper()
  upload <- mock_upload(
    "EXPORT.BIB",
    readLines(fixture_path("scopus_bibtex_sample.bib"), warn = FALSE)
  )
  on.exit(unlink(upload$datapath))

  settings <- resolve(upload, "load", "isi")
  expect_identical(settings$mode, "import")
  expect_identical(settings$extension, "bib")
  M <- convert2df(upload$datapath, dbsource = settings$dbsource,
                  format = "bibtex")
  expect_s3_class(M, "bibliometrixDB")
  expect_equal(nrow(M), 2)
  expect_identical(unique(M$DB), "SCOPUS")
  expect_setequal(M$DI, c("10.0000/biblioshiny-upload-1",
                         "10.0000/biblioshiny-upload-2"))
})

test_that("uppercase filenames and a UTF-8 BOM still identify Scopus exports", {
  resolve <- upload_settings_helper()
  upload <- mock_upload("EXPORT.BIB", c("\ufeffScopus", scopus_bib_header[-1]))
  on.exit(unlink(upload$datapath))

  result <- resolve(upload, "load", "isi")
  expect_identical(result$extension, "bib")
  expect_identical(result$dbsource, "scopus")
  expect_identical(result$mode, "import")
})

test_that("a generic BibTeX suffix is not enough to guess a source", {
  resolve <- upload_settings_helper()
  upload <- mock_upload("export.bib", "@article{example, title={Scopus}}")
  on.exit(unlink(upload$datapath))

  expect_error(resolve(upload, "load", "isi"), "Import raw file", fixed = TRUE)
  result <- resolve(upload, "import", "isi")
  expect_identical(result$dbsource, "isi")
  expect_length(result$notice, 0)
})

test_that("unsupported Scopus formats fail with actionable text", {
  resolve <- upload_settings_helper()
  upload <- mock_upload("export.txt", "Scopus")
  on.exit(unlink(upload$datapath))

  expect_error(resolve(upload, "import", "scopus"), "Scopus import accepts")
  expect_error(resolve(upload, "load", "scopus"), "saved .xlsx", fixed = TRUE)
  expect_error(resolve(upload, "null", "scopus"), "Choose 'Import raw file")
})

test_that("saved collections and Scopus CSV or ZIP keep their existing routes", {
  resolve <- upload_settings_helper()
  for (ext in c("xlsx", "rdata", "rda", "rds")) {
    upload <- mock_upload(paste0("collection.", toupper(ext)))
    result <- resolve(upload, "load", "isi")
    expect_identical(result$mode, "load")
    expect_identical(result$extension, ext)
    unlink(upload$datapath)
  }
  for (ext in c("csv", "zip")) {
    upload <- mock_upload(paste0("export.", toupper(ext)))
    result <- resolve(upload, "import", "scopus")
    expect_identical(result$mode, "import")
    expect_identical(result$extension, ext)
    unlink(upload$datapath)
  }
})
