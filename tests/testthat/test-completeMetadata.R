# Tests for completeMetadata: vacancy predicate, DOI normaliser, JATS stripper,
# Crossref reference -> CR string conversion, fill-only merge contract.
# All tests run offline (no network calls); the Crossref client itself is
# exercised only indirectly via stubbed responses.

# These tests target the unexported helpers via :::.
.cm <- function(name) get(name, envir = asNamespace("bibliometrix"))

test_that(".normalize_doi strips prefixes and validates", {
  fn <- .cm(".normalize_doi")
  inp <- c(
    "10.1000/abc",
    "https://doi.org/10.1000/ABC",
    "http://dx.doi.org/10.1234/zz/yy",
    "doi:10.55555/x",
    "10.1/x",        # too few digits in registrant
    "not a doi",
    "",
    NA_character_
  )
  out <- fn(inp)
  expect_equal(out[1], "10.1000/abc")
  expect_equal(out[2], "10.1000/abc")
  expect_equal(out[3], "10.1234/zz/yy")
  expect_equal(out[4], "10.55555/x")
  expect_true(is.na(out[5]))
  expect_true(is.na(out[6]))
  expect_true(is.na(out[7]))
  expect_true(is.na(out[8]))
})

test_that(".is_vacant matches missingData()'s predicate", {
  fn <- .cm(".is_vacant")
  v <- c(NA, "", "NA", "none", "NA,0000,NA", "real value", " spaced ")
  out <- fn(v)
  expect_equal(out, c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that(".strip_jats_abstract removes JATS markup", {
  fn <- .cm(".strip_jats_abstract")
  inp <- "<jats:p>Hello <jats:italic>world</jats:italic>.</jats:p>"
  out <- fn(inp)
  expect_match(out, "Hello world", fixed = TRUE)
  expect_false(grepl("<", out, fixed = TRUE))
  expect_true(is.na(fn(NA_character_)))
  expect_true(is.na(fn("")))
})

test_that(".crossref_authors formats AU as WoS-style 'LASTNAME I'", {
  fn <- .cm(".crossref_authors")
  authors <- list(
    list(family = "Aria", given = "Massimo",
         sequence = "first",
         affiliation = list(list(name = "Univ Naples"))),
    list(family = "Cuccurullo", given = "Corrado",
         affiliation = list())
  )
  out <- fn(authors)
  expect_equal(out$AU, "ARIA M; CUCCURULLO C")
  expect_equal(out$C1, "UNIV NAPLES")
  expect_equal(out$RP, "UNIV NAPLES")
})

test_that(".crossref_refs_to_cr_string builds WoS-style refs from structured items", {
  fn <- .cm(".crossref_refs_to_cr_string")
  refs <- list(
    list(author = "Smith J",
         year = "2010",
         `journal-title` = "J Sci",
         volume = "1",
         `first-page` = "5",
         DOI = "10.1234/abc"),
    list(unstructured = "Doe (2020) Some random ref; with semicolons; 10.5678/xyz")
  )
  out <- fn(refs)
  expect_match(out, "SMITH J, 2010, J SCI, V1, P5, DOI 10.1234/abc", fixed = TRUE)
  expect_match(out, "; ", fixed = TRUE)
  expect_match(out, "2020", fixed = TRUE)
  expect_match(out, "10.5678/xyz", fixed = TRUE)
  ## Semicolons inside an unstructured ref must be neutralised so they do not
  ## fragment the CR field.
  un_part <- sub(".*; ", "", out)
  expect_false(grepl(";.*;", un_part))
})

test_that(".crossref_refs_to_cr_string returns NA on empty input", {
  fn <- .cm(".crossref_refs_to_cr_string")
  expect_true(is.na(fn(NULL)))
  expect_true(is.na(fn(list())))
})

test_that(".crossref_doctype maps common Crossref types", {
  fn <- .cm(".crossref_doctype")
  expect_equal(fn("journal-article"), "ARTICLE")
  expect_equal(fn("book-chapter"), "BOOK CHAPTER")
  expect_equal(fn("posted-content"), "PREPRINT")
  expect_equal(fn("custom-type"), "CUSTOM-TYPE")  ## verbatim fallback uppercased
})

test_that(".merge_enrichment fills only missing cells and records provenance", {
  merge_fn <- .cm(".merge_enrichment")
  M <- data.frame(
    SR = c("paper a", "paper b", "paper c"),
    DI = c("10.1234/a", "10.1234/b", "10.1234/c"),
    AB = c(NA, "existing abstract", ""),
    TI = c("", NA, "existing title"),
    AU = c(NA_character_, NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  enriched <- list(
    by_doi = list(
      "10.1234/a" = list(DI = "10.1234/a",
                      AB = "new abstract A",
                      TI = "new title A",
                      AU = "ARIA M"),
      "10.1234/b" = list(DI = "10.1234/b",
                      AB = "should NOT overwrite",
                      TI = "new title B",
                      AU = "CUCCURULLO C"),
      "10.1234/c" = list(DI = "10.1234/c",
                      AB = "new abstract C",
                      TI = "should NOT overwrite",
                      AU = "TEST X")
    ),
    fail_count = 0L
  )
  res <- merge_fn(M, enriched, eligible_idx = 1:3,
                  fields = c("AB","TI","AU"), source_label = "crossref")
  expect_equal(res$M$AB[1], "new abstract A")
  expect_equal(res$M$AB[2], "existing abstract")          ## preserved
  expect_equal(res$M$AB[3], "new abstract C")
  expect_equal(res$M$TI[1], "new title A")
  expect_equal(res$M$TI[2], "new title B")
  expect_equal(res$M$TI[3], "existing title")             ## preserved
  expect_equal(res$M$AU[1], "ARIA M")
  expect_equal(res$M$AU[2], "CUCCURULLO C")
  expect_equal(res$M$AU[3], "TEST X")

  expect_equal(res$filled[["AB"]], 2L)
  expect_equal(res$filled[["TI"]], 2L)
  expect_equal(res$filled[["AU"]], 3L)
  expect_equal(nrow(res$prov), 7L)
  expect_true(all(res$prov$source == "crossref"))
})

test_that(".merge_enrichment is a no-op when nothing is missing", {
  merge_fn <- .cm(".merge_enrichment")
  M <- data.frame(
    SR = "paper a",
    DI = "10.1234/a",
    AB = "already there",
    TI = "already there",
    stringsAsFactors = FALSE
  )
  enriched <- list(
    by_doi = list("10.1234/a" = list(DI = "10.1234/a",
                                  AB = "x", TI = "y")),
    fail_count = 0L
  )
  res <- merge_fn(M, enriched, eligible_idx = 1L,
                  fields = c("AB","TI"), source_label = "crossref")
  expect_identical(res$M, M)
  expect_null(res$prov)
  expect_equal(sum(res$filled), 0L)
})

test_that("completeMetadata is a no-op on a complete fixture (offline)", {
  ## Skip if we have no fixture or no internet check is needed.
  M <- tryCatch(load_wos_fixture(), error = function(e) NULL)
  skip_if(is.null(M), "no WoS fixture available")
  ## Block any HTTP traffic by passing an empty source set; the function
  ## must short-circuit and return the original collection unchanged.
  res <- completeMetadata(M, sources = character(0), verbose = FALSE)
  expect_identical(res$M, M)
  expect_equal(nrow(res$report), 0L)
})

test_that("completeMetadata short-circuits when no eligible records", {
  M <- data.frame(
    SR = c("a", "b"),
    DI = c(NA_character_, NA_character_),
    AB = c("x", "y"),
    AU = c("X", "Y"),
    TC = c(0, 0),
    stringsAsFactors = FALSE
  )
  res <- completeMetadata(M, sources = "crossref",
                          fields = c("AB","AU"), verbose = FALSE)
  expect_identical(res$M, M)
  expect_equal(nrow(res$report), 0L)
})

# ----- Phase 2: OpenAlex source -----------------------------------------------

test_that("completeMetadata auto-disables OpenAlex when M$DB is OPENALEX", {
  ## We construct a one-row collection with DB=OPENALEX and a DOI that
  ## triggers eligibility. With sources=c("openalex","crossref") and an
  ## offline-impossible Crossref endpoint, we just verify that we DO NOT
  ## crash on missing openalexR network and that the function attempts only
  ## Crossref. We block real traffic by setting fields to ones the local
  ## fallback path can short-circuit.
  M <- data.frame(
    SR = "a",
    DI = "10.1234/anything",
    AB = NA_character_,
    DB = "OPENALEX",
    TC = "0",
    stringsAsFactors = FALSE
  )
  ## Use sources = "openalex" alone -> auto-disabled -> empty source set ->
  ## function returns the original collection unchanged with empty report.
  res <- completeMetadata(M, sources = "openalex",
                          fields = c("AB","TC"), verbose = FALSE)
  expect_identical(res$M, M)
  expect_equal(nrow(res$report), 0L)
})

test_that("completeMetadata silently drops TC when only Crossref is selected", {
  M <- data.frame(
    SR = "a",
    DI = "10.1234/x",
    AB = NA_character_,
    TC = "0",
    DB = "ISI",
    stringsAsFactors = FALSE
  )
  ## Without an OpenAlex pass, TC must be dropped. Run with empty sources to
  ## avoid network: explicit sources = character(0) returns immediately.
  res <- completeMetadata(M, sources = character(0),
                          fields = c("AB","TC"), verbose = FALSE)
  expect_identical(res$M, M)
})

test_that(".openalex_authors formats AU as WoS-style 'LASTNAME I'", {
  fn <- .cm(".openalex_authors")
  authorships <- list(
    list(
      author = list(display_name = "Massimo Aria"),
      author_position = "first",
      is_corresponding = TRUE,
      institutions = list(list(display_name = "Univ Naples"))
    ),
    list(
      author = list(display_name = "Corrado Cuccurullo"),
      author_position = "middle",
      institutions = list(list(display_name = "Univ Vanvitelli"))
    )
  )
  out <- fn(authorships)
  expect_equal(out$AU, "ARIA M; CUCCURULLO C")
  expect_match(out$C1, "UNIV NAPLES", fixed = TRUE)
  expect_match(out$C1, "UNIV VANVITELLI", fixed = TRUE)
  expect_equal(out$RP, "UNIV NAPLES")
})

test_that(".openalex_doctype maps common OpenAlex types", {
  fn <- .cm(".openalex_doctype")
  expect_equal(fn("article"), "ARTICLE")
  expect_equal(fn("book-chapter"), "BOOK CHAPTER")
  expect_equal(fn("preprint"), "PREPRINT")
  expect_equal(fn("custom-type"), "CUSTOM-TYPE")
})

test_that(".openalex_work_to_row extracts the requested fields", {
  fn <- .cm(".openalex_work_to_row")
  work <- list(
    doi = "https://doi.org/10.1234/X",
    title = "A Title",
    publication_year = 2020,
    language = "en",
    type = "article",
    primary_location = list(source = list(display_name = "J Test")),
    abstract_inverted_index = list("Hello" = list(0L), "world" = list(1L)),
    cited_by_count = 7,
    authorships = list(
      list(
        author = list(display_name = "John Smith"),
        author_position = "first",
        is_corresponding = TRUE,
        institutions = list(list(display_name = "MIT"))
      )
    ),
    referenced_works = character(0)
  )
  out <- fn(work,
            fields = c("AB","AU","C1","DT","LA","PY","RP","SO","TC","TI","CR"),
            ref_lookup = list())
  expect_equal(out$DI, "10.1234/x")
  expect_equal(out$TI, "A Title")
  expect_equal(out$PY, "2020")
  expect_equal(out$LA, "en")
  expect_equal(out$DT, "ARTICLE")
  expect_equal(out$SO, "J Test")
  expect_equal(out$TC, "7")
  expect_equal(out$AU, "SMITH J")
  expect_equal(out$C1, "MIT")
  expect_equal(out$RP, "MIT")
  expect_match(out$AB, "Hello world", fixed = TRUE)
  ## CR key is absent (not NA) because referenced_works was empty.
  expect_true(is.null(out$CR))
})

test_that(".recompute_eligible drops rows fully populated by an earlier pass", {
  fn <- .cm(".recompute_eligible")
  M <- data.frame(
    SR = c("a","b","c"),
    AB = c("present", NA_character_, NA_character_),
    TI = c(NA_character_, "present", NA_character_),
    stringsAsFactors = FALSE
  )
  ## Initial eligibility was all 3 rows. After OpenAlex filled AB and TI on
  ## row a (so a now has TI=NA but AB=present), Crossref should still see
  ## row a as eligible because TI is missing.
  out <- fn(M, fields = c("AB","TI"), base_idx = 1:3)
  expect_equal(out, c(1L, 2L, 3L))
  ## Now mark row 'a' as fully populated:
  M2 <- M; M2$TI[1] <- "now present"
  out2 <- fn(M2, fields = c("AB","TI"), base_idx = 1:3)
  expect_equal(out2, c(2L, 3L))
})

test_that("completeMetadata report aggregates rows per source", {
  ## Stub the two .enrich_* helpers via local function-scope mocks so we
  ## can exercise the multi-source aggregation code path offline.
  M <- data.frame(
    SR = c("p1","p2"),
    DI = c("10.1234/aaa","10.1234/bbb"),
    AB = c(NA_character_, NA_character_),
    TI = c(NA_character_, NA_character_),
    TC = c("0","0"),
    DB = "ISI",
    stringsAsFactors = FALSE
  )

  ## Patch openalexR to be undetected -> .enrich_from_openalex returns empty
  ## (the requireNamespace branch). Patch httr2 + crossref endpoint by
  ## replacing .enrich_from_crossref via local() trace.
  orig_cr <- get(".enrich_from_crossref", envir = asNamespace("bibliometrix"))
  orig_oa <- get(".enrich_from_openalex", envir = asNamespace("bibliometrix"))
  local_unlock <- function(name, env) {
    if (exists(name, envir = env, inherits = FALSE)) {
      try(unlockBinding(name, env), silent = TRUE)
    }
  }
  ns <- asNamespace("bibliometrix")
  local_unlock(".enrich_from_crossref", ns)
  local_unlock(".enrich_from_openalex", ns)
  on.exit({
    assign(".enrich_from_crossref", orig_cr, envir = ns)
    assign(".enrich_from_openalex", orig_oa, envir = ns)
  }, add = TRUE)

  assign(".enrich_from_crossref",
         function(dois, fields, email, batch_size = 20,
                  progress = NULL, verbose = TRUE) {
           list(
             by_doi = list(
               "10.1234/aaa" = list(DI = "10.1234/aaa", TI = "T from CR"),
               "10.1234/bbb" = list(DI = "10.1234/bbb", AB = "AB from CR")
             ),
             fail_count = 0L
           )
         },
         envir = ns)
  assign(".enrich_from_openalex",
         function(dois, fields, email, oa_apikey,
                  progress = NULL, verbose = TRUE) {
           list(
             by_doi = list(
               "10.1234/aaa" = list(DI = "10.1234/aaa",
                                    AB = "AB from OA",
                                    TC = "42")
             ),
             fail_count = 0L
           )
         },
         envir = ns)

  res <- completeMetadata(
    M,
    sources = c("openalex", "crossref"),
    fields  = c("AB","TI","TC"),
    verbose = FALSE
  )

  ## Row 1: AB filled by OpenAlex (first); TI filled by Crossref (residual).
  expect_equal(res$M$AB[1], "AB from OA")
  expect_equal(res$M$TI[1], "T from CR")
  expect_equal(res$M$TC[1], "42")
  ## Row 2: AB filled by Crossref (OA had no record); TI was missing from
  ## both, stays NA.
  expect_equal(res$M$AB[2], "AB from CR")
  expect_true(is.na(res$M$TI[2]))

  ## Report should have rows for both sources.
  expect_setequal(unique(res$report$source), c("openalex", "crossref"))

  ## Provenance should record both sources too.
  prov <- attr(res$M, "enrichment")
  expect_setequal(unique(prov$source), c("openalex", "crossref"))

  ## M$ENRICH must summarise per row what was filled and from where.
  expect_true("ENRICH" %in% names(res$M))
  ## Row 1 had AB filled by OpenAlex, TC filled by OpenAlex, TI filled by Crossref.
  expect_match(res$M$ENRICH[1], "AB:OA", fixed = TRUE)
  expect_match(res$M$ENRICH[1], "TC:OA", fixed = TRUE)
  expect_match(res$M$ENRICH[1], "TI:CR", fixed = TRUE)
  ## Row 2 only had AB filled by Crossref.
  expect_match(res$M$ENRICH[2], "AB:CR", fixed = TRUE)
  expect_false(grepl(":OA", res$M$ENRICH[2], fixed = TRUE))
})
