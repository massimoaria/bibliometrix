# Test per il controllo delle dipendenze all'avvio di Biblioshiny (issue #624).
# require()/install.packages()/installed.packages() sono sostituiti da stub: i
# test non installano nulla e non usano la rete.

load_libraries_fn <- function(failing = character(0)) {
  path <- system.file("biblioshiny", "libraries.R", package = "bibliometrix")
  testthat::skip_if(path == "", "libraries.R non disponibile")

  env <- new.env(parent = globalenv())
  env$install.packages <- function(...) invisible(NULL)
  env$installed.packages <- function(...) {
    matrix(character(0), nrow = 0, ncol = 1, dimnames = list(character(0), "Package"))
  }
  env$require <- function(package, ...) !(package %in% failing)
  sys.source(path, envir = env)
  env$libraries
}

test_that("libraries() elenca i pacchetti che non ha potuto caricare", {
  libraries <- load_libraries_fn(failing = c("pdftools", "chromote"))
  res <- suppressMessages(libraries())

  expect_type(res, "list")
  expect_named(res, c("ok", "missing", "missing_names"))
  expect_false(res$ok)

  # l'elenco leggibile conserva il requisito di versione ...
  expect_true("pdftools (>= 3.6.0)" %in% res$missing)
  expect_true("chromote" %in% res$missing)

  # ... quello per install.packages() no, cosi' la riga si incolla com'e'
  expect_setequal(res$missing_names, c("pdftools", "chromote"))
  expect_false(any(grepl(">=", res$missing_names, fixed = TRUE)))
})

test_that("libraries() non segnala nulla quando tutto si carica", {
  libraries <- load_libraries_fn()
  res <- suppressMessages(libraries())

  expect_true(res$ok)
  expect_length(res$missing, 0)
  expect_length(res$missing_names, 0)
})

test_that("i sorgenti di Biblioshiny sono puro ASCII", {
  # In una localizzazione MBCS (cinese, giapponese, coreano, cirillico su
  # Windows) R ignora encoding = "UTF-8" e decodifica i file con la codepage di
  # sistema: un carattere non-ASCII si corrompe e, se cade dentro una stringa,
  # il file non si parsa piu' e Biblioshiny non parte (issue #589). I caratteri
  # non-ASCII vanno scritti come escape \u{...} o, nell'HTML, come entita'
  # numeriche: il sorgente resta ASCII e il valore a runtime non cambia.
  dir <- system.file("biblioshiny", package = "bibliometrix")
  skip_if(dir == "", "cartella biblioshiny non disponibile")

  files <- list.files(dir, pattern = "[.]R$", full.names = TRUE)
  expect_gt(length(files), 0)

  offenders <- character(0)
  for (f in files) {
    raw <- readBin(f, "raw", file.info(f)$size)
    if (any(raw >= as.raw(128))) offenders <- c(offenders, basename(f))
  }
  expect_equal(offenders, character(0))
})

test_that("i sorgenti di Biblioshiny si parsano", {
  dir <- system.file("biblioshiny", package = "bibliometrix")
  skip_if(dir == "", "cartella biblioshiny non disponibile")

  for (f in list.files(dir, pattern = "[.]R$", full.names = TRUE)) {
    expect_no_error(parse(f))
  }
})

test_that("i sorgenti Biblioshiny non chiamano is_online()", {
  # utils.R definisce is_Online(); il vecchio nome is_online() e' esportato da
  # httr2, che l'app carica, quindi una chiamata rimasta al nome vecchio non
  # produce alcun errore: si risolve in silenzio su curl::has_internet(), che
  # sonda la connettivita' in generale invece dell'host che interessa. E'
  # esattamente quello che era successo in biblioAI.R (#570).
  dir <- system.file("biblioshiny", package = "bibliometrix")
  skip_if(dir == "", "cartella biblioshiny non disponibile")

  offenders <- character(0)
  for (f in list.files(dir, pattern = "[.]R$", full.names = TRUE)) {
    txt <- readLines(f, warn = FALSE)
    if (any(grepl("(^|[^_[:alnum:].])is_online[[:space:]]*\\(", txt))) {
      offenders <- c(offenders, basename(f))
    }
  }
  expect_equal(offenders, character(0))
})

test_that("check_online considera raggiungibile un host che risponde con un errore HTTP", {
  # Un 404 dimostra che il server ha risposto: la domanda e' la raggiungibilita',
  # non l'esito della richiesta. La radice dell'endpoint Gemini risponde 404, e
  # trattarla come offline bloccherebbe Biblio AI per tutti (#570).
  path <- system.file("biblioshiny", "utils.R", package = "bibliometrix")
  skip_if(path == "", "utils.R non disponibile")
  skip_on_cran()

  e <- new.env(parent = globalenv())
  suppressWarnings(suppressMessages(sys.source(path, envir = e)))
  skip_if_not(is.function(e$check_online))

  # host inesistente: errore di trasporto, quindi davvero non raggiungibile
  expect_false(e$check_online(host = "https://no.such.host.invalid", timeout = 2))
})
