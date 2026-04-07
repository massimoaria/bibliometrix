# helper-load-fixtures.R
# Caricato automaticamente da testthat prima dell'esecuzione dei test.
# Fornisce funzioni helper per caricare i dataset fixture pre-convertiti.

fixture_path <- function(filename) {
  testthat::test_path("fixtures", filename)
}

# Carica e converte i fixture nei diversi formati.
# Ogni funzione restituisce un data.frame di classe "bibliometrixDB".

load_wos_fixture <- function() {
  suppressWarnings(suppressMessages(
    convert2df(fixture_path("wos_sample.txt"), dbsource = "wos", format = "plaintext")
  ))
}

load_scopus_fixture <- function() {
  suppressWarnings(suppressMessages(
    convert2df(fixture_path("scopus_sample.csv"), dbsource = "scopus", format = "csv")
  ))
}

load_openalex_fixture <- function() {
  suppressWarnings(suppressMessages(
    convert2df(fixture_path("openalex_sample.csv"), dbsource = "openalex", format = "csv")
  ))
}

load_lens_fixture <- function() {
  suppressWarnings(suppressMessages(
    convert2df(fixture_path("lens_sample.csv"), dbsource = "lens", format = "csv")
  ))
}
