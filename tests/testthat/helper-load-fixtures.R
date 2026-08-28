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

# Export WoS in cui i riferimenti citati usano la forma "Cognome, Iniziali,"
# invece della classica "Cognome Iniziali," (issue #640).
load_wos_newformat_fixture <- function() {
  suppressWarnings(suppressMessages(
    convert2df(fixture_path("wos_newformat_sample.txt"), dbsource = "wos", format = "plaintext")
  ))
}

# Export WoS in formato BibTeX in cui il campo Author va a capo su piu' righe
# (issue #590).
load_wos_bibtex_fixture <- function() {
  suppressWarnings(suppressMessages(
    convert2df(fixture_path("wos_bibtex_sample.bib"), dbsource = "isi", format = "bibtex")
  ))
}
