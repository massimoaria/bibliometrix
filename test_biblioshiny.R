#!/usr/bin/env Rscript
# ===========================================================================
# test_biblioshiny.R
# Script standalone per testare biblioshiny prima della sottomissione a CRAN
#
# Eseguire con: Rscript test_biblioshiny.R
# oppure: source("test_biblioshiny.R") dalla console R
#
# PREREQUISITI:
#   - bibliometrix installato (devtools::install())
#   - shinytest2 installato (install.packages("shinytest2"))
#   - Chrome o Chromium installato (per i test automatici browser)
#   - File di test in ~/Desktop/bibliometrix_test_files/
#
# Lo script ha 3 sezioni:
#   A) Test automatici delle funzioni core usate da biblioshiny
#   B) Test automatici con shinytest2 (avvio app e navigazione)
#   C) Checklist interattiva per test manuali
# ===========================================================================

cat("\n")
cat("============================================================\n")
cat("  TEST BIBLIOSHINY - Pre-sottomissione CRAN\n")
cat("============================================================\n\n")

library(bibliometrix)

# --- Configurazione ---
test_data_dir <- path.expand("~/Desktop/bibliometrix_test_files")
results <- list()
pass_count <- 0
fail_count <- 0
skip_count <- 0

record_result <- function(section, test_name, passed, message = "", skipped = FALSE) {
  status <- if (skipped) "SKIP" else if (passed) "PASS" else "FAIL"
  cat(sprintf("  [%s] %s", status, test_name))
  if (nchar(message) > 0) cat(sprintf(" -- %s", message))
  cat("\n")
  key <- paste0(section, "::", test_name)
  results[[key]] <<- list(passed = passed, message = message, skipped = skipped)
  if (skipped) {
    skip_count <<- skip_count + 1
  } else if (passed) {
    pass_count <<- pass_count + 1
  } else {
    fail_count <<- fail_count + 1
  }
}

# ===========================================================================
# SEZIONE A: Test automatici delle funzioni core usate da biblioshiny
# ===========================================================================

cat("--- SEZIONE A: Test funzioni core per biblioshiny ---\n\n")

# A1: Import da ogni formato supportato
import_tests <- list(
  list(name = "Import Scopus CSV",
       file = file.path(test_data_dir, "Scopus", "scopus.csv"),
       db = "scopus", fmt = "csv", expected_db = "SCOPUS"),
  list(name = "Import OpenAlex CSV",
       file = file.path(test_data_dir, "openalex", "openalex.csv"),
       db = "openalex", fmt = "csv", expected_db = "OPENALEX"),
  list(name = "Import Lens CSV",
       file = file.path(test_data_dir, "lens", "lens-export.csv"),
       db = "lens", fmt = "csv", expected_db = "LENS"),
  list(name = "Import WoS plaintext",
       file = file.path(test_data_dir, "WoS", "wos.zip"),
       db = "wos", fmt = "plaintext", expected_db = "ISI")
)

datasets <- list()

for (test in import_tests) {
  tryCatch({
    if (!file.exists(test$file)) {
      record_result("A", test$name, FALSE, paste("File non trovato:", test$file))
      next
    }
    M <- suppressWarnings(suppressMessages(
      convert2df(test$file, dbsource = test$db, format = test$fmt)
    ))
    ok <- inherits(M, "bibliometrixDB") &&
      nrow(M) > 0 &&
      all(c("AU", "TI", "SO", "PY", "TC", "SR") %in% names(M)) &&
      unique(M$DB) == test$expected_db
    record_result("A", test$name, ok,
                  paste(nrow(M), "record,", ncol(M), "colonne"))
    if (ok) datasets[[test$db]] <- M
  }, error = function(e) {
    record_result("A", test$name, FALSE, conditionMessage(e))
  })
}

# A2: biblioAnalysis su ogni dataset importato
for (db_name in names(datasets)) {
  test_name <- paste("biblioAnalysis su", db_name)
  tryCatch({
    res <- biblioAnalysis(datasets[[db_name]])
    ok <- inherits(res, "bibliometrix") && res$Articles > 0 && res$nAuthors > 0
    record_result("A", test_name, ok,
                  paste(res$Articles, "articoli,", res$nAuthors, "autori"))
  }, error = function(e) {
    record_result("A", test_name, FALSE, conditionMessage(e))
  })
}

# A3: Summary e Plot
if (length(datasets) > 0) {
  M_test <- datasets[[1]]
  res_test <- biblioAnalysis(M_test)

  tryCatch({
    capture.output(summary(res_test, k = 5, pause = FALSE, verbose = FALSE))
    record_result("A", "summary.bibliometrix", TRUE)
  }, error = function(e) {
    record_result("A", "summary.bibliometrix", FALSE, conditionMessage(e))
  })

  tryCatch({
    pdf(tempfile(fileext = ".pdf"))
    plot(res_test, k = 5, pause = FALSE)
    dev.off()
    record_result("A", "plot.bibliometrix", TRUE)
  }, error = function(e) {
    try(dev.off(), silent = TRUE)
    record_result("A", "plot.bibliometrix", FALSE, conditionMessage(e))
  })
}

# A4: missingData
if (length(datasets) > 0) {
  for (db_name in names(datasets)) {
    test_name <- paste("missingData su", db_name)
    tryCatch({
      res <- missingData(datasets[[db_name]])
      ok <- is.list(res) &&
        all(c("allTags", "mandatoryTags") %in% names(res)) &&
        is.data.frame(res$mandatoryTags)
      record_result("A", test_name, ok)
    }, error = function(e) {
      record_result("A", test_name, FALSE, conditionMessage(e))
    })
  }
}

# A5: Reti bibliometriche
if (length(datasets) > 0) {
  M_test <- datasets[[1]]

  network_tests <- list(
    list(name = "Rete co-citation references", analysis = "co-citation", network = "references"),
    list(name = "Rete collaboration autori", analysis = "collaboration", network = "authors"),
    list(name = "Rete co-occurrences keywords", analysis = "co-occurrences", network = "keywords"),
    list(name = "Rete coupling references", analysis = "coupling", network = "references")
  )

  for (nt in network_tests) {
    tryCatch({
      NetMatrix <- biblioNetwork(M_test, analysis = nt$analysis,
                                  network = nt$network, sep = ";")
      ok <- (inherits(NetMatrix, "Matrix") || is.matrix(NetMatrix)) &&
        nrow(NetMatrix) > 0 && nrow(NetMatrix) == ncol(NetMatrix)
      record_result("A", nt$name, ok,
                    paste(nrow(NetMatrix), "x", ncol(NetMatrix)))
    }, error = function(e) {
      record_result("A", nt$name, FALSE, conditionMessage(e))
    })
  }

  # networkPlot
  tryCatch({
    NetMatrix <- biblioNetwork(M_test, analysis = "co-citation",
                                network = "references", sep = ";")
    pdf(tempfile(fileext = ".pdf"))
    net <- suppressWarnings(networkPlot(NetMatrix, n = 20, type = "auto", verbose = FALSE))
    dev.off()
    ok <- "graph" %in% names(net) && inherits(net$graph, "igraph")
    record_result("A", "networkPlot", ok)
  }, error = function(e) {
    try(dev.off(), silent = TRUE)
    record_result("A", "networkPlot", FALSE, conditionMessage(e))
  })
}

# A6: Citations e H-index
if (length(datasets) > 0) {
  M_test <- datasets[[1]]

  tryCatch({
    CR <- citations(M_test, field = "article", sep = ";")
    ok <- is.list(CR) && "Cited" %in% names(CR) && length(CR$Cited) > 0
    record_result("A", "citations (article)", ok)
  }, error = function(e) {
    record_result("A", "citations (article)", FALSE, conditionMessage(e))
  })

  tryCatch({
    CR <- citations(M_test, field = "author", sep = ";")
    ok <- is.list(CR) && "Cited" %in% names(CR)
    record_result("A", "citations (author)", ok)
  }, error = function(e) {
    record_result("A", "citations (author)", FALSE, conditionMessage(e))
  })

  tryCatch({
    first_au <- trimws(strsplit(M_test$AU[1], ";")[[1]][1])
    H <- Hindex(M_test, field = "author", elements = first_au, sep = ";")
    ok <- is.list(H) && "H" %in% names(H) && is.data.frame(H$H)
    record_result("A", "Hindex", ok)
  }, error = function(e) {
    record_result("A", "Hindex", FALSE, conditionMessage(e))
  })
}

# A7: Keyword e text extraction
if (length(datasets) > 0) {
  M_test <- datasets[[1]]

  tryCatch({
    tab <- tableTag(M_test, Tag = "AU")
    ok <- (is.data.frame(tab) || inherits(tab, "table")) && length(tab) > 0
    record_result("A", "tableTag (AU)", ok)
  }, error = function(e) {
    record_result("A", "tableTag (AU)", FALSE, conditionMessage(e))
  })

  tryCatch({
    tab <- tableTag(M_test, Tag = "DE")
    ok <- (is.data.frame(tab) || inherits(tab, "table")) && length(tab) > 0
    record_result("A", "tableTag (DE)", ok)
  }, error = function(e) {
    record_result("A", "tableTag (DE)", FALSE, conditionMessage(e))
  })
}

# A8: metaTagExtraction
if (length(datasets) > 0) {
  M_test <- datasets[[1]]

  for (field in c("CR_AU", "CR_SO", "AU_CO", "AU1_CO")) {
    test_name <- paste("metaTagExtraction", field)
    tryCatch({
      M2 <- metaTagExtraction(M_test, Field = field, sep = ";")
      ok <- field %in% names(M2)
      record_result("A", test_name, ok)
    }, error = function(e) {
      record_result("A", test_name, FALSE, conditionMessage(e))
    })
  }
}

# A9: Merge di dataset da sorgenti diverse
if (length(datasets) >= 2) {
  tryCatch({
    M_merged <- suppressWarnings(suppressMessages(
      do.call(mergeDbSources, c(datasets, list(remove.duplicated = TRUE)))
    ))
    ok <- is.data.frame(M_merged) && nrow(M_merged) > 0
    record_result("A", "mergeDbSources (multi-database)", ok,
                  paste(nrow(M_merged), "record dopo merge"))
  }, error = function(e) {
    record_result("A", "mergeDbSources (multi-database)", FALSE, conditionMessage(e))
  })
}

# A10: Thematic analysis (usa bibliometrixData per dataset piu grande)
tryCatch({
  if (requireNamespace("bibliometrixData", quietly = TRUE)) {
    data(scientometrics, package = "bibliometrixData")
    scientometrics$PY <- as.numeric(scientometrics$PY)
    scientometrics$TC <- as.numeric(scientometrics$TC)
    res <- suppressWarnings(suppressMessages(
      thematicMap(scientometrics, field = "ID", n = 50, minfreq = 5,
                  stemming = FALSE, size = 0.5, repel = TRUE)
    ))
    ok <- is.list(res) && res$nclust > 0
    record_result("A", "thematicMap", ok, paste(res$nclust, "cluster"))
  } else {
    record_result("A", "thematicMap", FALSE, "bibliometrixData non installato", skipped = TRUE)
  }
}, error = function(e) {
  record_result("A", "thematicMap", FALSE, conditionMessage(e))
})

# A11: fieldByYear
tryCatch({
  if (requireNamespace("bibliometrixData", quietly = TRUE)) {
    data(scientometrics, package = "bibliometrixData")
    res <- suppressWarnings(suppressMessages(
      fieldByYear(scientometrics, field = "ID", timespan = NULL,
                  min.freq = 5, n.items = 5, graph = TRUE)
    ))
    ok <- is.list(res) && "graph" %in% names(res)
    record_result("A", "fieldByYear", ok)
  } else {
    record_result("A", "fieldByYear", FALSE, "bibliometrixData non installato", skipped = TRUE)
  }
}, error = function(e) {
  record_result("A", "fieldByYear", FALSE, conditionMessage(e))
})

# A12: authorProdOverTime
tryCatch({
  if (requireNamespace("bibliometrixData", quietly = TRUE)) {
    data(scientometrics, package = "bibliometrixData")
    res <- suppressWarnings(suppressMessages(
      authorProdOverTime(scientometrics, k = 10, graph = FALSE)
    ))
    ok <- is.list(res) && is.data.frame(res$dfAU)
    record_result("A", "authorProdOverTime", ok)
  } else {
    record_result("A", "authorProdOverTime", FALSE, "bibliometrixData non installato", skipped = TRUE)
  }
}, error = function(e) {
  record_result("A", "authorProdOverTime", FALSE, conditionMessage(e))
})

cat("\n")

# ===========================================================================
# SEZIONE B: Test automatici con shinytest2
# ===========================================================================

cat("--- SEZIONE B: Test automatici biblioshiny con shinytest2 ---\n\n")

run_shinytest2 <- FALSE
if (requireNamespace("shinytest2", quietly = TRUE)) {
  run_shinytest2 <- TRUE
} else {
  cat("  [SKIP] shinytest2 non installato. Installa con: install.packages('shinytest2')\n")
  record_result("B", "shinytest2 disponibile", FALSE, "Non installato", skipped = TRUE)
}

if (run_shinytest2) {
  # Assicurarsi che NOT_CRAN sia settato per evitare skip automatici di shinytest2
  Sys.setenv(NOT_CRAN = "true")
  shinytest2_ok <- tryCatch({
    library(shinytest2)
    TRUE
  }, error = function(e) {
    cat("  [SKIP] shinytest2 non caricabile:", conditionMessage(e), "\n")
    record_result("B", "Caricamento shinytest2", FALSE, conditionMessage(e), skipped = TRUE)
    FALSE
  })

  if (shinytest2_ok) {
  app_dir <- system.file("biblioshiny", package = "bibliometrix")

  # B1: L'app si avvia correttamente
  app <- NULL
  tryCatch({
    app <- AppDriver$new(
      app_dir = app_dir,
      name = "biblioshiny-test",
      height = 900,
      width = 1400,
      load_timeout = 60000,
      timeout = 30000
    )
    record_result("B", "App si avvia correttamente", TRUE)
  }, error = function(e) {
    record_result("B", "App si avvia correttamente", FALSE, conditionMessage(e))
  })

  if (!is.null(app)) {

    # B2: Verifica che la UI principale sia caricata
    tryCatch({
      html <- app$get_html("body")
      has_content <- nchar(html) > 100
      has_sidebar <- grepl("sidebar|menu|treeview", html, ignore.case = TRUE)
      record_result("B", "UI principale caricata", has_content && has_sidebar)
    }, error = function(e) {
      record_result("B", "UI principale caricata", FALSE, conditionMessage(e))
    })

    # B3: Verifica i valori degli input iniziali
    tryCatch({
      load_val <- app$get_value(input = "load")
      record_result("B", "Input 'load' inizializzato", !is.null(load_val),
                    paste("valore:", load_val))
    }, error = function(e) {
      record_result("B", "Input 'load' inizializzato", FALSE, conditionMessage(e))
    })

    # B4: Selezione modalita import e database source
    tryCatch({
      app$set_inputs(load = "import", wait_ = FALSE)
      Sys.sleep(2)
      app$set_inputs(dbsource = "scopus", wait_ = FALSE)
      Sys.sleep(1)
      db_val <- app$get_value(input = "dbsource")
      record_result("B", "Selezione dbsource funziona", db_val == "scopus")
    }, error = function(e) {
      record_result("B", "Selezione dbsource funziona", FALSE, conditionMessage(e))
    })

    # B5: Upload file e import dati
    scopus_file <- file.path(test_data_dir, "Scopus", "scopus.csv")
    if (file.exists(scopus_file)) {
      tryCatch({
        app$set_inputs(load = "import", wait_ = FALSE)
        Sys.sleep(2)
        app$set_inputs(dbsource = "scopus", wait_ = FALSE)
        Sys.sleep(1)
        app$upload_file(file1 = scopus_file)
        Sys.sleep(3)
        app$click("applyLoad")
        Sys.sleep(15)  # attendere la conversione

        # Verificare che non ci siano errori visibili
        html <- app$get_html("body")
        has_error <- grepl("Error:", html, ignore.case = FALSE) &&
          !grepl("shiny-output-error-validation", html)
        record_result("B", "Import Scopus CSV via UI", !has_error)
      }, error = function(e) {
        record_result("B", "Import Scopus CSV via UI", FALSE, conditionMessage(e))
      })

      # B6: Navigazione tra i tab principali dopo import
      # Usa JavaScript per navigare i tab di shinydashboard (sidebarMenu)
      tabs_to_test <- c(
        "overview"        = "Overview",
        "sources"         = "Sources",
        "authors"         = "Authors",
        "documents"       = "Documents",
        "concepStructure" = "Conceptual Structure",
        "intStruct"       = "Intellectual Structure",
        "socialStruct"    = "Social Structure"
      )

      for (tab_id in names(tabs_to_test)) {
        tab_label <- tabs_to_test[[tab_id]]
        tryCatch({
          # Navigazione via JavaScript per shinydashboard sidebarMenu
          js_cmd <- sprintf(
            "Shiny.setInputValue('tabs', '%s', {priority: 'event'});",
            tab_id
          )
          app$run_js(js_cmd)
          Sys.sleep(5)
          # Verificare che la pagina non mostri errori fatali
          html <- app$get_html("body")
          has_fatal_error <- grepl("Error:", html, ignore.case = FALSE) &&
            !grepl("shiny-output-error-validation|error-message", html, ignore.case = TRUE)
          record_result("B", paste("Tab", tab_label, "senza errori"), !has_fatal_error)
        }, error = function(e) {
          record_result("B", paste("Tab", tab_label, "senza errori"), FALSE, conditionMessage(e))
        })
      }
    } else {
      record_result("B", "Import Scopus CSV via UI", FALSE,
                    "File Scopus non trovato", skipped = TRUE)
    }

    # B7: Caricamento dataset demo
    tryCatch({
      app$set_inputs(load = "demo", wait_ = FALSE)
      Sys.sleep(2)
      app$set_inputs(demoDataset = "bibliometrix_sample", wait_ = FALSE)
      Sys.sleep(1)
      app$click("applyLoad")
      Sys.sleep(15)
      html <- app$get_html("body")
      has_error <- grepl("Error:", html, ignore.case = FALSE) &&
        !grepl("shiny-output-error-validation", html)
      record_result("B", "Caricamento dataset demo", !has_error)
    }, error = function(e) {
      record_result("B", "Caricamento dataset demo", FALSE, conditionMessage(e))
    })

    # Chiudi app
    tryCatch({
      app$stop()
      record_result("B", "App chiusa correttamente", TRUE)
    }, error = function(e) {
      record_result("B", "App chiusa correttamente", FALSE, conditionMessage(e))
    })
  }
  } # fine if (shinytest2_ok)
}

cat("\n")

# ===========================================================================
# SEZIONE C: Checklist interattiva per test manuali
# ===========================================================================

cat("--- SEZIONE C: Checklist interattiva (test manuali) ---\n\n")

interactive_mode <- interactive()

if (interactive_mode) {
  cat("Questa sezione richiede di testare manualmente biblioshiny.\n")
  cat("L'app verra avviata e dovrai verificare ogni punto della checklist.\n\n")

  proceed <- readline("Vuoi procedere con i test manuali? (s/n): ")

  if (tolower(proceed) == "s") {
    checklist <- c(
      "L'app si avvia e mostra la dashboard iniziale",
      "Import file Scopus CSV: i dati vengono caricati senza errori",
      "Import file WoS plaintext (zip): i dati vengono caricati senza errori",
      "Import file OpenAlex CSV: i dati vengono caricati senza errori",
      "Import file Lens CSV: i dati vengono caricati senza errori",
      "Tab Overview > Main Information: la tabella riassuntiva e visibile",
      "Tab Overview > Annual Scientific Production: il grafico e visibile",
      "Tab Overview > Three Fields Plot: il grafico sankey e visibile",
      "Tab Sources > Most Relevant Sources: la tabella e il grafico sono visibili",
      "Tab Sources > Bradford's Law: il grafico e visibile",
      "Tab Authors > Most Relevant Authors: tabella e grafico visibili",
      "Tab Authors > Author Production Over Time: il grafico e visibile",
      "Tab Documents > Most Cited Documents: la tabella e visibile",
      "Tab Conceptual Structure > Co-word Analysis: la rete si genera",
      "Tab Conceptual Structure > Thematic Map: la mappa e visibile",
      "Tab Intellectual Structure > Co-citation Network: la rete si genera",
      "Tab Social Structure > Collaboration Network: la rete si genera",
      "Export dati in formato xlsx funziona",
      "L'app si chiude senza errori tramite il bottone Stop"
    )

    cat("\nAvvio biblioshiny... Testa ogni punto e rispondi 's' (pass) o 'n' (fail).\n")
    cat("Premi Ctrl+C nell'app per tornare alla console dopo ogni test.\n\n")

    # Avvia biblioshiny in background
    cat(">>> Avvia biblioshiny con: biblioshiny()\n")
    cat(">>> Dopo aver testato, torna qui e rispondi alle domande.\n\n")

    readline("Premi INVIO quando sei pronto per iniziare la checklist...")

    for (i in seq_along(checklist)) {
      response <- readline(sprintf("[%d/%d] %s (s/n/skip): ",
                                   i, length(checklist), checklist[i]))
      response <- tolower(trimws(response))
      if (response == "s") {
        record_result("C", checklist[i], TRUE)
      } else if (response == "skip") {
        record_result("C", checklist[i], FALSE, "Saltato", skipped = TRUE)
      } else {
        note <- readline("  Nota (opzionale): ")
        record_result("C", checklist[i], FALSE, note)
      }
    }
  } else {
    cat("  Test manuali saltati.\n")
  }
} else {
  cat("  [SKIP] Sezione interattiva non disponibile in modalita non-interattiva.\n")
  cat("  Eseguire con source() in una sessione R interattiva per i test manuali.\n")
}

# ===========================================================================
# REPORT FINALE
# ===========================================================================

cat("\n")
cat("============================================================\n")
cat("  REPORT FINALE\n")
cat("============================================================\n\n")

total <- pass_count + fail_count + skip_count
cat(sprintf("  Totale test:   %d\n", total))
cat(sprintf("  PASS:          %d\n", pass_count))
cat(sprintf("  FAIL:          %d\n", fail_count))
cat(sprintf("  SKIP:          %d\n", skip_count))
cat("\n")

if (fail_count == 0) {
  cat("  >>> RISULTATO: TUTTI I TEST PASSATI <<<\n")
  cat("  Il pacchetto e pronto per la sottomissione a CRAN.\n")
} else {
  cat("  >>> RISULTATO: ALCUNI TEST FALLITI <<<\n")
  cat("  Correggere i problemi prima della sottomissione.\n\n")
  cat("  Test falliti:\n")
  for (name in names(results)) {
    r <- results[[name]]
    if (!r$passed && !r$skipped) {
      cat(sprintf("    - %s", name))
      if (nchar(r$message) > 0) cat(sprintf(": %s", r$message))
      cat("\n")
    }
  }
}

cat("\n============================================================\n")

# Restituisci il risultato per uso programmatico
invisible(list(
  pass = pass_count,
  fail = fail_count,
  skip = skip_count,
  results = results
))
