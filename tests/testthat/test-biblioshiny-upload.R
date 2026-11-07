# checkUploadFormat() lives in the Biblioshiny sources, not in the package
# namespace: evaluate it, with the objects it uses, from utils.R.
upload_check_helper <- function() {
  path <- system.file("biblioshiny", "utils.R", package = "bibliometrix")
  wanted <- c("uploadDbLabels", "detectExportSource", "checkUploadFormat")
  env <- new.env(parent = globalenv())
  for (expr in as.list(parse(path))) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        as.character(expr[[2]]) %in% wanted) {
      eval(expr, envir = env)
    }
  }
  env$checkUploadFormat
}

mock_upload <- function(name, lines = "") {
  path <- tempfile()
  writeLines(lines, path, useBytes = TRUE)
  data.frame(name = name, datapath = path, stringsAsFactors = FALSE)
}

scopus_bib_header <- c("Scopus", "EXPORT DATE: 06 November 2026", "",
                       "@ARTICLE{example,", "  title = {Example title},")

test_that("a Scopus BibTeX in 'Load bibliometrix file(s)' is a wrong format", {
  check <- upload_check_helper()
  upload <- mock_upload("export.bib", scopus_bib_header)
  on.exit(unlink(upload$datapath))

  expect_error(check(upload, "load", "isi"), "is not correct for 'Load",
               fixed = TRUE)
  expect_error(check(upload, "load", "isi"),
               "select 'Scopus' as the database", fixed = TRUE)
})

test_that("a Scopus BibTeX with another database is stopped, not switched", {
  check <- upload_check_helper()
  upload <- mock_upload("EXPORT.BIB", c("\ufeffScopus", scopus_bib_header[-1]))
  on.exit(unlink(upload$datapath))

  expect_error(check(upload, "import", "isi"),
               "looks like a Scopus export, but the selected database is 'Web of Science",
               fixed = TRUE)
  expect_identical(check(upload, "import", "scopus"), "bib")
})

test_that("the Scopus BibTeX fixture is imported with the right settings", {
  check <- upload_check_helper()
  upload <- mock_upload(
    "EXPORT.BIB",
    readLines(fixture_path("scopus_bibtex_sample.bib"), warn = FALSE)
  )
  on.exit(unlink(upload$datapath))

  expect_identical(check(upload, "import", "scopus"), "bib")
  M <- convert2df(upload$datapath, dbsource = "scopus", format = "bibtex")
  expect_s3_class(M, "bibliometrixDB")
  expect_equal(nrow(M), 2)
  expect_identical(unique(M$DB), "SCOPUS")
})

test_that("WoS exports are recognized and suggested", {
  check <- upload_check_helper()
  txt <- mock_upload("savedrecs.txt",
                     readLines(fixture_path("wos_sample.txt"), n = 5))
  bib <- mock_upload("savedrecs.bib",
                     readLines(fixture_path("wos_bibtex_sample.bib"), n = 5))
  on.exit(unlink(c(txt$datapath, bib$datapath)))

  expect_identical(check(txt, "import", "isi"), "txt")
  expect_identical(check(bib, "import", "isi"), "bib")
  expect_error(check(txt, "import", "scopus"), "looks like a Web of Science")
  expect_error(check(bib, "load", "isi"), "select 'Web of Science",
               fixed = TRUE)
})

test_that("an unrecognized export keeps the chosen database", {
  check <- upload_check_helper()
  upload <- mock_upload("export.bib", "@article{example, title={Scopus}}")
  on.exit(unlink(upload$datapath))

  expect_error(check(upload, "load", "isi"), "choose 'Import raw file(s)'",
               fixed = TRUE)
  expect_identical(check(upload, "import", "isi"), "bib")
})

test_that("unsupported Scopus formats and no mode give a message", {
  check <- upload_check_helper()
  upload <- mock_upload("export.txt", "Scopus")
  on.exit(unlink(upload$datapath))

  expect_error(check(upload, "import", "scopus"), "not correct for Scopus")
  expect_error(check(upload, "load", "scopus"), "(.txt) is not correct",
               fixed = TRUE)
  expect_error(check(upload, "null", "scopus"), "Choose 'Import raw file")
})

test_that("saved collections and Scopus CSV or ZIP are accepted", {
  check <- upload_check_helper()
  for (ext in c("xlsx", "rdata", "rda", "rds")) {
    upload <- mock_upload(paste0("collection.", toupper(ext)))
    expect_identical(check(upload, "load", "isi"), ext)
    unlink(upload$datapath)
  }
  for (ext in c("csv", "zip")) {
    upload <- mock_upload(paste0("export.", toupper(ext)))
    expect_identical(check(upload, "import", "scopus"), ext)
    unlink(upload$datapath)
  }
})
