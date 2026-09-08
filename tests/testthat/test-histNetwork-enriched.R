# Tests for histNetwork() on collections whose cited references were ADDED by
# metadata enrichment (completeMetadata): the references are WoS-format strings
# carrying a DOI, not the native record identifiers the OpenAlex/Lens matchers
# resolve local citations by. All tests run offline on a synthetic collection.

# A four-document collection where doc 4 cites docs 1-3 and doc 3 cites doc 1.
enriched_corpus <- function(db = "OPENALEX") {
  M <- data.frame(
    AU = c("SMITH J", "JONES A", "BROWN K", "WHITE L"),
    TI = c("ALPHA", "BETA", "GAMMA", "DELTA"),
    SO = c("J ONE", "J TWO", "J ONE", "J THREE"),
    PY = c(2010L, 2012L, 2014L, 2016L),
    TC = c(30L, 20L, 10L, 5L),
    DI = c("10.1/a", "10.1/b", "10.1/c", "10.1/d"),
    DE = NA_character_, ID = NA_character_, AB = NA_character_,
    id_oa = c("W1", "W2", "W3", "W4"),
    DB = db,
    SR_FULL = c("SMITH J, 2010, J ONE", "JONES A, 2012, J TWO",
                "BROWN K, 2014, J ONE", "WHITE L, 2016, J THREE"),
    stringsAsFactors = FALSE
  )
  M$SR <- M$SR_FULL
  # completeMetadata() rebuilds each reference from the source that provided it: the
  # author initials and the abbreviated journal do NOT reproduce SR_FULL, only the DOI
  # ties the reference back to the document.
  M$CR <- c(NA, NA,
    "SMITH JB, 2010, J ONE ABBREV, V1, P1, DOI 10.1/A",
    paste("SMITH JB, 2010, J ONE ABBREV, V1, P1, DOI 10.1/A",
          "JONES AC, 2012, J TWO ABBREV, V2, P9, DOI 10.1/B",
          "BROWN KP, 2014, J ONE ABBREV, V3, P4, DOI 10.1/C", sep = ";"))
  M
}

lcs_total <- function(M) {
  h <- suppressWarnings(suppressMessages(
    histNetwork(M, min.citations = 0, sep = ";", network = FALSE, verbose = FALSE)))
  sum(h$histData$LCS)
}

test_that("an enriched OpenAlex collection is matched by DOI", {
  # The identifier matcher has nothing to match: the references are no longer Work IDs.
  expect_equal(lcs_total(enriched_corpus()), 4)
})

test_that("a native OpenAlex collection keeps the identifier matcher", {
  M <- enriched_corpus()
  M$CR <- c(NA, NA, "W1", "W1;W2;W3")
  expect_equal(lcs_total(M), 4)
  matchable <- get("refIdsMatchable", envir = asNamespace("bibliometrix"))
  expect_true(matchable(M, "OPENALEX", ";"))
  expect_false(matchable(enriched_corpus(), "OPENALEX", ";"))
  # References resolved by Biblioshiny keep the Work IDs in CRids: still an identifier match.
  M2 <- enriched_corpus(); M2$CRids <- c(NA, NA, "W1", "W1;W2;W3")
  expect_true(matchable(M2, "OPENALEX", ";"))
})

test_that("any other database with enriched references is analysed, not refused", {
  # It used to print "Database not compatible with direct citation analysis" and then
  # fail with "object 'results' not found".
  expect_equal(lcs_total(enriched_corpus(db = "PUBMED")), 4)
})

test_that("WoS and Scopus collections are unaffected", {
  wos_res <- suppressWarnings(suppressMessages(
    histNetwork(load_wos_fixture(), min.citations = 0, sep = ";", network = FALSE, verbose = FALSE)))
  expect_true(is.list(wos_res) && nrow(wos_res$histData) > 0)
  scopus_res <- suppressWarnings(suppressMessages(
    histNetwork(load_scopus_fixture(), min.citations = 0, sep = ";", network = FALSE, verbose = FALSE)))
  expect_true(is.list(scopus_res) && nrow(scopus_res$histData) > 0)
})
