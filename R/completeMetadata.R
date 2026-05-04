#' Complete missing metadata via DOI lookup against Crossref and OpenAlex
#'
#' Given a bibliometrix collection produced by \code{\link{convert2df}}, this
#' function takes the subset of records that have a DOI but are missing one
#' or more of the analysis-relevant fields, queries the Crossref REST API
#' (\url{https://api.crossref.org/works}) and/or OpenAlex (via
#' \pkg{openalexR}) using the DOI as the lookup key, and fills the gaps
#' with the values returned by those sources. Existing non-empty values
#' are never overwritten.
#'
#' When both sources are enabled, OpenAlex runs first (broader coverage of
#' AB/CR/C1/TC) and Crossref then fills the residual gaps. If the input
#' collection was originally imported from OpenAlex (\code{M$DB[1] ==
#' "OPENALEX"}), the OpenAlex pass is automatically skipped because re-querying
#' it would not add information.
#'
#' The vacancy predicate matches the one used by \code{\link{missingData}}:
#' a cell is considered missing when it is \code{NA} or one of
#' \code{c("", "NA", "none", "NA,0000,NA")}.
#'
#' Crossref cannot supply author keywords (DE), Keywords Plus (ID), Web of
#' Science categories (WC), or citation counts (TC). OpenAlex covers TC,
#' AB, AU, C1, CR, DT, LA, PY, RP, SO, TI well; OpenAlex \code{keywords}
#' are AI-derived topic labels and not author keywords, so DE is off by
#' default. ID and WC are always skipped.
#'
#' @param M Bibliometrix data frame produced by \code{\link{convert2df}}.
#' @param sources Character vector of enrichment sources.
#'   Default \code{c("openalex", "crossref")}. Order is irrelevant; OpenAlex
#'   always runs before Crossref. \code{"openalex"} is skipped if
#'   \code{M$DB[1] == "OPENALEX"}.
#' @param fields Character vector of WoS-codified fields to attempt to fill.
#'   Default \code{c("AB","AU","C1","CR","DT","LA","PY","RP","SO","TC","TI")}.
#'   \code{TC} is filled only by OpenAlex.
#' @param email Optional contact email used as the Crossref polite-pool
#'   identifier and OpenAlex \code{mailto}. If \code{NULL}, the function
#'   falls back to the env var \code{BIBLIOMETRIX_EMAIL} or the persisted
#'   file \code{~/.biblio_openalex_email.txt}.
#' @param oa_apikey Optional OpenAlex API key. If \code{NULL}, the function
#'   reads \code{Sys.getenv("openalexR.apikey")} and falls back to
#'   \code{~/.biblio_openalex_apikey.txt}. The OpenAlex pass works without a
#'   key (lower rate limit).
#' @param batch_size Number of DOIs per Crossref batch request (default 20).
#'   OpenAlex uses a fixed batch size of 50 (the maximum that keeps URLs
#'   under length limits).
#' @param max_records Optional cap on the number of records to enrich
#'   (useful for previewing). Default \code{Inf}.
#' @param progress Optional callback \code{function(done, total, label)}
#'   invoked after each batch. Used by biblioshiny to drive a progress bar.
#' @param verbose Logical. Print progress messages to the console.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{M}}{The enriched collection (same class as the input).}
#'   \item{\code{report}}{Long-format \code{data.frame} with one row per
#'     (field, source) summarising attempts, fills, and failures.}
#'   \item{\code{before}}{The \code{mandatoryTags} table from
#'     \code{missingData(M)} before enrichment.}
#'   \item{\code{after}}{The \code{mandatoryTags} table from
#'     \code{missingData(M)} after enrichment.}
#' }
#' Provenance is attached to the returned collection as
#' \code{attr(M, "enrichment")}, a long-format \code{data.frame} with columns
#' \code{SR}, \code{field}, \code{source}, \code{timestamp}.
#'
#' @examples
#' \dontrun{
#' data(scientometrics, package = "bibliometrixData")
#' res <- completeMetadata(scientometrics, email = "you@example.com")
#' res$report
#' res$after
#' }
#'
#' @export
completeMetadata <- function(M,
                             sources = c("openalex", "crossref"),
                             fields  = c("AB","AU","C1","CR","DT","LA",
                                         "PY","RP","SO","TC","TI"),
                             email   = NULL,
                             oa_apikey = NULL,
                             batch_size = 20,
                             max_records = Inf,
                             progress = NULL,
                             verbose = TRUE) {

  stopifnot(is.data.frame(M))
  ## Source filtering
  sources <- tolower(unique(sources))
  unknown <- setdiff(sources, c("crossref", "openalex"))
  if (length(unknown) > 0) {
    stop("Unsupported sources: ", paste(unknown, collapse = ", "))
  }
  ## If the collection itself comes from OpenAlex, re-querying it makes no
  ## sense -- transparently drop the OpenAlex pass.
  if ("openalex" %in% sources && "DB" %in% names(M) &&
      length(M$DB) > 0 && !is.na(M$DB[1]) && toupper(M$DB[1]) == "OPENALEX") {
    if (isTRUE(verbose)) {
      message("[completeMetadata] Source DB is OPENALEX; skipping OpenAlex pass.")
    }
    sources <- setdiff(sources, "openalex")
  }

  ## Field filtering -- always-skip set
  always_skip <- c("ID", "WC")
  skipped <- intersect(fields, always_skip)
  if (length(skipped) > 0) {
    if (isTRUE(verbose)) {
      message("[completeMetadata] No source provides: ",
              paste(skipped, collapse = ", "), ". Skipping.")
    }
    fields <- setdiff(fields, skipped)
  }
  ## TC requires OpenAlex; if OpenAlex is disabled, drop TC silently.
  if (!"openalex" %in% sources) {
    if ("TC" %in% fields && isTRUE(verbose)) {
      message("[completeMetadata] TC requires OpenAlex; skipping.")
    }
    fields <- setdiff(fields, "TC")
  }
  if (length(sources) == 0 || length(fields) == 0) {
    return(list(M = M,
                report = .empty_enrichment_report(),
                before = missingData(M)$mandatoryTags,
                after  = missingData(M)$mandatoryTags))
  }

  before <- missingData(M)$mandatoryTags

  ## TC normalization to match missingData()'s collection-level semantics.
  ## missingData() reports TC as 100% missing only when sum(M$TC) == 0;
  ## otherwise per-cell zeros are treated as legitimate (a real 0 citation
  ## count). To stay consistent, we treat TC = 0 as vacant for enrichment
  ## ONLY when the entire column sums to zero, by mapping those zeros to NA
  ## here so the standard vacancy predicate flags them. If even one row has
  ## TC > 0, all the zeros are left untouched (genuine zeros).
  if ("TC" %in% fields && "TC" %in% names(M)) {
    tc_sum <- suppressWarnings(sum(as.numeric(M$TC), na.rm = TRUE))
    if (!is.na(tc_sum) && tc_sum == 0) {
      tc_zeros <- !is.na(M$TC) & trimws(as.character(M$TC)) %in% c("0", "0.0")
      if (any(tc_zeros)) M$TC[tc_zeros] <- NA_character_
    }
  }

  ## Snapshot eligible rows: DOI present and at least one target field empty.
  doi_norm <- .normalize_doi(M$DI)
  target_present <- intersect(fields, names(M))
  if (length(target_present) == 0) {
    return(list(M = M, report = .empty_enrichment_report(),
                before = before, after = before))
  }
  any_missing <- rep(FALSE, nrow(M))
  for (f in target_present) {
    any_missing <- any_missing | .is_vacant(M[[f]])
  }
  eligible_idx <- which(!is.na(doi_norm) & any_missing)
  if (length(eligible_idx) == 0) {
    if (isTRUE(verbose)) message("[completeMetadata] No eligible records.")
    return(list(M = M, report = .empty_enrichment_report(),
                before = before, after = before))
  }
  if (length(eligible_idx) > max_records) {
    eligible_idx <- eligible_idx[seq_len(max_records)]
  }

  ## Resolve email and OpenAlex API key.
  email <- .resolve_email(email)
  oa_apikey <- .resolve_oa_apikey(oa_apikey)

  M_out <- M
  prov_all <- list()
  report_rows <- list()
  filled_total <- stats::setNames(integer(length(fields)), fields)

  ## ---- Pass 1: OpenAlex (preferred when available) ------------------------
  if ("openalex" %in% sources) {
    eligible_idx_oa <- .recompute_eligible(M_out, fields, eligible_idx)
    if (length(eligible_idx_oa) > 0) {
      oa_dois <- .normalize_doi(M_out$DI)[eligible_idx_oa]
      enriched_oa <- .enrich_from_openalex(oa_dois, fields, email, oa_apikey,
                                           progress = progress,
                                           verbose = verbose)
      merged_oa <- .merge_enrichment(M_out, enriched_oa, eligible_idx_oa,
                                     fields, source_label = "openalex")
      M_out <- merged_oa$M
      if (!is.null(merged_oa$prov)) prov_all[[length(prov_all) + 1]] <- merged_oa$prov
      report_rows[[length(report_rows) + 1]] <- .build_enrichment_report(
        merged_oa$filled,
        attempted = length(eligible_idx_oa),
        source_label = "openalex",
        fail_count = enriched_oa$fail_count
      )
      filled_total <- filled_total + merged_oa$filled[names(filled_total)]
    }
  }

  ## ---- Pass 2: Crossref (fills residual gaps) -----------------------------
  if ("crossref" %in% sources) {
    cr_fields <- setdiff(fields, "TC")  # Crossref cannot provide TC
    eligible_idx_cr <- .recompute_eligible(M_out, cr_fields, eligible_idx)
    if (length(eligible_idx_cr) > 0 && length(cr_fields) > 0) {
      cr_dois <- .normalize_doi(M_out$DI)[eligible_idx_cr]
      enriched_cr <- .enrich_from_crossref(cr_dois, cr_fields, email,
                                           batch_size = batch_size,
                                           progress = progress,
                                           verbose = verbose)
      merged_cr <- .merge_enrichment(M_out, enriched_cr, eligible_idx_cr,
                                     cr_fields, source_label = "crossref")
      M_out <- merged_cr$M
      if (!is.null(merged_cr$prov)) prov_all[[length(prov_all) + 1]] <- merged_cr$prov
      report_rows[[length(report_rows) + 1]] <- .build_enrichment_report(
        merged_cr$filled,
        attempted = length(eligible_idx_cr),
        source_label = "crossref",
        fail_count = enriched_cr$fail_count
      )
      filled_total[names(merged_cr$filled)] <-
        filled_total[names(merged_cr$filled)] + merged_cr$filled
    }
  }

  ## Drop derived columns whose source field has been touched, so the user
  ## can rerun metaTagExtraction() to refresh them. Classic bibliometrix
  ## works on flat data frames, so no relational rebuild is needed.
  touched <- names(filled_total)[filled_total > 0]
  if ("C1" %in% touched || "AU" %in% touched) {
    for (col in c("AU_UN", "AU_CO", "AU1_UN", "AU1_CO")) {
      if (col %in% names(M_out)) M_out[[col]] <- NULL
    }
  }

  ## Attach concatenated provenance and project it onto a
  ## per-row M$ENRICH column so downstream consumers (e.g. biblioshiny's
  ## contents table) can see what was filled and from where without digging
  ## into the attribute.
  if (length(prov_all) > 0) {
    prov_df <- do.call(rbind, prov_all)
    attr(M_out, "enrichment") <- prov_df

    src_short <- c(openalex = "OA", crossref = "CR")
    tag_per_row <- paste0(prov_df$field, ":",
                          ifelse(prov_df$source %in% names(src_short),
                                 src_short[prov_df$source],
                                 toupper(prov_df$source)))
    enrich_per_sr <- tapply(tag_per_row, prov_df$SR, function(v) {
      paste(unique(v), collapse = "; ")
    })
    if ("SR" %in% names(M_out)) {
      M_out$ENRICH <- unname(enrich_per_sr[as.character(M_out$SR)])
    }
  }

  after <- missingData(M_out)$mandatoryTags
  report <- if (length(report_rows) > 0) {
    do.call(rbind, report_rows)
  } else {
    .empty_enrichment_report()
  }

  list(M = M_out, report = report, before = before, after = after)
}

#' @noRd
.recompute_eligible <- function(M, fields, base_idx) {
  ## Among the rows we initially flagged eligible, keep only those that still
  ## have at least one missing target field. This makes Pass 2 (Crossref)
  ## skip rows that Pass 1 (OpenAlex) already fully populated.
  if (length(base_idx) == 0) return(base_idx)
  fields_present <- intersect(fields, names(M))
  if (length(fields_present) == 0) return(integer(0))
  any_missing <- rep(FALSE, length(base_idx))
  for (f in fields_present) {
    any_missing <- any_missing | .is_vacant(M[[f]][base_idx])
  }
  base_idx[any_missing]
}

# -- DOI helpers ---------------------------------------------------------------

#' @noRd
.normalize_doi <- function(x) {
  if (is.null(x)) return(character(0))
  s <- as.character(x)
  s <- trimws(s)
  s[s == "" | s == "NA" | s == "none"] <- NA_character_
  s <- sub("^https?://(dx\\.)?doi\\.org/", "", s, ignore.case = TRUE)
  s <- sub("^doi:\\s*", "", s, ignore.case = TRUE)
  ok <- grepl("^10\\.[0-9]{4,9}/", s)
  s[!ok] <- NA_character_
  tolower(s)
}

#' @noRd
.is_vacant <- function(x) {
  is.na(x) | x %in% c("NA,0000,NA", "NA", "", "none")
}

#' @noRd
.empty_enrichment_report <- function() {
  data.frame(field = character(0), source = character(0),
             n_attempted = integer(0), n_filled = integer(0),
             n_failed = integer(0), stringsAsFactors = FALSE)
}

#' @noRd
.build_enrichment_report <- function(filled_per_field, attempted,
                                     source_label, fail_count) {
  fields <- names(filled_per_field)
  if (length(fields) == 0) return(.empty_enrichment_report())
  data.frame(
    field       = fields,
    source      = source_label,
    n_attempted = attempted,
    n_filled    = as.integer(filled_per_field),
    n_failed    = fail_count,
    stringsAsFactors = FALSE
  )
}

#' @noRd
.resolve_email <- function(email) {
  if (!is.null(email) && nzchar(email)) return(email)
  env <- Sys.getenv("BIBLIOMETRIX_EMAIL", unset = "")
  if (nzchar(env)) return(env)
  env2 <- Sys.getenv("openalexR.mailto", unset = "")
  if (nzchar(env2)) return(env2)
  pf <- path.expand("~/.biblio_openalex_email.txt")
  if (file.exists(pf)) {
    e <- trimws(readLines(pf, warn = FALSE)[1])
    if (length(e) == 1 && nzchar(e)) return(e)
  }
  NULL
}

#' @noRd
.resolve_oa_apikey <- function(key) {
  if (!is.null(key) && nzchar(key)) return(key)
  env <- Sys.getenv("openalexR.apikey", unset = "")
  if (nzchar(env)) return(env)
  pf <- path.expand("~/.biblio_openalex_apikey.txt")
  if (file.exists(pf)) {
    k <- trimws(readLines(pf, warn = FALSE)[1])
    if (length(k) == 1 && nzchar(k) && nchar(k) >= 10) return(k)
  }
  NULL
}

# -- Crossref client -----------------------------------------------------------

#' @noRd
.enrich_from_crossref <- function(dois, fields, email,
                                  batch_size = 20,
                                  progress = NULL,
                                  verbose = TRUE) {

  if (!requireNamespace("httr2", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("completeMetadata() requires the 'httr2' and 'jsonlite' packages.")
  }

  ## We deduplicate DOIs because two rows may share a DOI (rare but possible)
  uniq <- unique(dois)
  chunks <- split(uniq, ceiling(seq_along(uniq) / batch_size))

  ua <- .crossref_user_agent(email)
  base <- "https://api.crossref.org/works"
  results <- list()
  fail_count <- 0L

  for (i in seq_along(chunks)) {
    chunk <- chunks[[i]]
    items <- tryCatch(
      .crossref_fetch_batch(chunk, base, ua),
      error = function(e) {
        if (isTRUE(verbose)) {
          message(sprintf("[completeMetadata] Batch %d failed: %s",
                          i, conditionMessage(e)))
        }
        NULL
      }
    )
    if (is.null(items)) {
      fail_count <- fail_count + length(chunk)
    } else {
      results <- c(results, items)
    }
    if (is.function(progress)) {
      progress(i * batch_size, length(uniq),
               sprintf("Crossref batch %d / %d", i, length(chunks)))
    }
    if (isTRUE(verbose)) {
      message(sprintf("[completeMetadata] Crossref %d/%d batches done.",
                      i, length(chunks)))
    }
  }

  ## items is a named list keyed by lowercase DOI; build a per-DOI parsed frame
  parsed <- lapply(results, function(it) .crossref_item_to_row(it, fields))
  parsed <- parsed[!vapply(parsed, is.null, logical(1))]
  if (length(parsed) == 0) {
    return(list(by_doi = list(), fail_count = fail_count))
  }
  list(by_doi = stats::setNames(parsed,
                                vapply(parsed, function(r) r$DI, character(1))),
       fail_count = fail_count)
}

#' @noRd
.crossref_user_agent <- function(email) {
  ver <- tryCatch(as.character(utils::packageVersion("bibliometrix")),
                  error = function(e) "dev")
  if (!is.null(email) && nzchar(email)) {
    sprintf("bibliometrix/%s (https://www.bibliometrix.org; mailto:%s)",
            ver, email)
  } else {
    sprintf("bibliometrix/%s (https://www.bibliometrix.org)", ver)
  }
}

#' @noRd
.crossref_fetch_batch <- function(dois, base, ua) {
  ## Build filter=doi:DOI1,doi:DOI2,... ; ask only for the per-item fields we need.
  filter_val <- paste0("doi:", dois, collapse = ",")
  req <- httr2::request(base) |>
    httr2::req_url_query(
      filter = filter_val,
      rows   = as.character(length(dois))
    ) |>
    httr2::req_user_agent(ua) |>
    httr2::req_retry(max_tries = 4, backoff = function(i) 2 ^ i) |>
    httr2::req_timeout(60)

  resp <- httr2::req_perform(req)
  raw <- httr2::resp_body_string(resp)
  parsed <- jsonlite::fromJSON(raw, simplifyVector = FALSE)
  if (is.null(parsed$message$items)) return(list())
  parsed$message$items
}

# -- Crossref item parser ------------------------------------------------------

#' @noRd
.crossref_item_to_row <- function(it, fields) {
  doi <- tolower(.coalesce_chr(it$DOI))
  if (is.na(doi) || !nzchar(doi)) return(NULL)

  out <- list(DI = doi)

  if ("TI" %in% fields) {
    out$TI <- .coalesce_chr(.first(it$title))
  }
  if ("SO" %in% fields) {
    so <- .first(it$`container-title`)
    if (!nzchar(so %||% "")) so <- .first(it$`short-container-title`)
    out$SO <- .coalesce_chr(so)
  }
  if ("PY" %in% fields) {
    out$PY <- .crossref_year(it)
  }
  if ("LA" %in% fields) {
    out$LA <- .coalesce_chr(it$language)
  }
  if ("DT" %in% fields) {
    out$DT <- .crossref_doctype(.coalesce_chr(it$type))
  }
  if ("AB" %in% fields) {
    out$AB <- .strip_jats_abstract(.coalesce_chr(it$abstract))
  }
  if ("AU" %in% fields || "C1" %in% fields || "RP" %in% fields) {
    au_info <- .crossref_authors(it$author)
    if ("AU" %in% fields) out$AU <- au_info$AU
    if ("C1" %in% fields) out$C1 <- au_info$C1
    if ("RP" %in% fields) out$RP <- au_info$RP
  }
  if ("CR" %in% fields) {
    out$CR <- .crossref_refs_to_cr_string(it$reference)
  }

  ## Replace empty strings with NA so the merge step ignores them.
  out <- lapply(out, function(v) {
    if (is.character(v) && length(v) == 1 && (is.na(v) || !nzchar(v))) NA_character_
    else v
  })
  out
}

#' @noRd
.first <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  if (is.list(x)) return(as.character(x[[1]]))
  as.character(x[[1]])
}

#' @noRd
.coalesce_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  v <- as.character(x[[1]])
  if (is.na(v) || !nzchar(v)) return(NA_character_)
  v
}

#' @noRd
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

#' @noRd
.crossref_year <- function(it) {
  ## Try issued -> published-print -> published-online -> created
  for (k in c("issued", "published-print", "published-online", "created")) {
    parts <- it[[k]]$`date-parts`
    if (!is.null(parts) && length(parts) > 0) {
      y <- suppressWarnings(as.integer(parts[[1]][[1]]))
      if (!is.na(y) && y >= 1500 && y <= 2200) return(as.character(y))
    }
  }
  NA_character_
}

#' @noRd
.crossref_doctype <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_character_)
  ## Map Crossref types onto the WoS-codified DT vocabulary used elsewhere
  ## in bibliometrix. Anything we cannot map confidently is uppercased verbatim
  ## so downstream filtering still works.
  x <- tolower(x)
  toupper(switch(
    x,
    "journal-article"        = "ARTICLE",
    "proceedings-article"    = "PROCEEDINGS PAPER",
    "book-chapter"           = "BOOK CHAPTER",
    "book"                   = "BOOK",
    "monograph"              = "BOOK",
    "edited-book"            = "EDITED BOOK",
    "reference-book"         = "BOOK",
    "report"                 = "REPORT",
    "posted-content"         = "PREPRINT",
    "dissertation"           = "DISSERTATION",
    "dataset"                = "DATA",
    "review"                 = "REVIEW",
    "journal-issue"          = "EDITORIAL",
    x
  ))
}

#' @noRd
.crossref_authors <- function(authors) {
  if (is.null(authors) || length(authors) == 0) {
    return(list(AU = NA_character_, C1 = NA_character_, RP = NA_character_))
  }
  fmt <- vapply(authors, function(a) {
    fam <- toupper(a$family %||% "")
    giv <- a$given %||% ""
    inits <- ""
    if (nzchar(giv)) {
      tokens <- strsplit(giv, "[ \\.\\-]+")[[1]]
      tokens <- tokens[nzchar(tokens)]
      inits <- paste(toupper(substr(tokens, 1, 1)), collapse = "")
    }
    if (!nzchar(fam)) return(NA_character_)
    trimws(paste(fam, inits))
  }, character(1))
  fmt <- fmt[!is.na(fmt) & nzchar(fmt)]
  AU <- if (length(fmt) > 0) paste(fmt, collapse = "; ") else NA_character_

  affs <- lapply(authors, function(a) {
    aa <- a$affiliation
    if (is.null(aa) || length(aa) == 0) return(character(0))
    vapply(aa, function(z) {
      if (is.list(z) && !is.null(z$name)) as.character(z$name) else NA_character_
    }, character(1))
  })
  affs <- unlist(affs, use.names = FALSE)
  affs <- affs[!is.na(affs) & nzchar(affs)]
  affs <- unique(affs)
  C1 <- if (length(affs) > 0) toupper(paste(affs, collapse = "; ")) else NA_character_

  ## Corresponding author guess: first author flagged sequence == "first",
  ## otherwise the first author. Crossref's corresponding flag is unreliable.
  rp_idx <- which(vapply(authors, function(a) isTRUE(a$sequence == "first"),
                         logical(1)))
  if (length(rp_idx) == 0) rp_idx <- 1L
  rp_aff <- authors[[rp_idx[1]]]$affiliation
  RP <- NA_character_
  if (!is.null(rp_aff) && length(rp_aff) > 0) {
    nm <- vapply(rp_aff, function(z) {
      if (is.list(z) && !is.null(z$name)) as.character(z$name) else NA_character_
    }, character(1))
    nm <- nm[!is.na(nm) & nzchar(nm)]
    if (length(nm) > 0) RP <- toupper(paste(nm, collapse = "; "))
  }

  list(AU = AU, C1 = C1, RP = RP)
}

#' @noRd
.strip_jats_abstract <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) return(NA_character_)
  out <- NA_character_
  if (requireNamespace("xml2", quietly = TRUE)) {
    out <- tryCatch({
      ## Wrap to ensure a single root and tolerate JATS namespaces.
      doc <- xml2::read_xml(paste0(
        "<root xmlns:jats=\"http://jats.nlm.nih.gov\">", x, "</root>"
      ))
      txt <- xml2::xml_text(doc)
      trimws(gsub("\\s+", " ", txt))
    }, error = function(e) NA_character_)
  }
  if (is.na(out) || !nzchar(out)) {
    ## Fallback: strip tags and decode common entities.
    out <- gsub("<[^>]+>", " ", x)
    out <- gsub("&amp;", "&", out, fixed = TRUE)
    out <- gsub("&lt;",  "<", out, fixed = TRUE)
    out <- gsub("&gt;",  ">", out, fixed = TRUE)
    out <- gsub("&quot;","\"", out, fixed = TRUE)
    out <- gsub("&#x2013;","-", out, fixed = TRUE)
    out <- trimws(gsub("\\s+", " ", out))
  }
  if (!nzchar(out)) NA_character_ else out
}

#' @noRd
.crossref_refs_to_cr_string <- function(refs) {
  if (is.null(refs) || length(refs) == 0) return(NA_character_)
  parts <- vapply(refs, .one_crossref_ref, character(1))
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (length(parts) == 0) return(NA_character_)
  paste(parts, collapse = "; ")
}

#' @noRd
.one_crossref_ref <- function(r) {
  if (is.null(r) || length(r) == 0) return(NA_character_)
  ## Prefer structured representation when available.
  au   <- r$author %||% ""
  yr   <- r$year %||% ""
  jrn  <- r$`journal-title` %||% (r$`series-title` %||% "")
  vol  <- r$volume %||% ""
  page <- r$`first-page` %||% ""
  doi  <- r$DOI %||% ""
  art  <- r$`article-title` %||% ""

  has_struct <- any(nzchar(c(au, yr, jrn, vol, page, doi)))
  if (has_struct) {
    tokens <- c(
      if (nzchar(au))   toupper(.first_token(au))                else NULL,
      if (nzchar(yr))   yr                                       else NULL,
      if (nzchar(jrn))  toupper(jrn)                             else NULL,
      if (nzchar(vol))  paste0("V", vol)                         else NULL,
      if (nzchar(page)) paste0("P", page)                        else NULL,
      if (nzchar(doi))  paste0("DOI ", tolower(doi))             else NULL
    )
    out <- paste(tokens, collapse = ", ")
    return(out)
  }

  ## Unstructured fallback: lift year and DOI via regex; keep the rest verbatim.
  un <- r$unstructured %||% ""
  if (!nzchar(un) && nzchar(art)) un <- art
  if (!nzchar(un)) return(NA_character_)
  yr_m  <- regmatches(un, regexpr("\\b(19|20)[0-9]{2}\\b", un))
  doi_m <- regmatches(un, regexpr("10\\.[0-9]{4,9}/[^\\s,;]+", un, perl = TRUE))
  un_clean <- gsub(";", ",", un, fixed = TRUE)  ## protect CR separator
  tokens <- c(toupper(un_clean),
              if (length(yr_m)) yr_m else NULL,
              if (length(doi_m)) paste0("DOI ", tolower(doi_m)) else NULL)
  paste(tokens, collapse = ", ")
}

#' @noRd
.first_token <- function(x) {
  s <- as.character(x[[1]])
  s <- trimws(s)
  s <- sub("[,;].*$", "", s)
  s
}

# -- Fill-only merge -----------------------------------------------------------

#' @noRd
.merge_enrichment <- function(M, enriched, eligible_idx, fields,
                              source_label = "crossref") {
  filled <- stats::setNames(integer(length(fields)), fields)
  prov_rows <- list()

  if (length(enriched$by_doi) == 0) {
    return(list(M = M, prov = NULL, filled = filled))
  }

  doi_norm <- .normalize_doi(M$DI)
  ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  ## Make sure SR exists for provenance
  has_sr <- "SR" %in% names(M)

  for (i in eligible_idx) {
    di <- doi_norm[i]
    if (is.na(di)) next
    rec <- enriched$by_doi[[di]]
    if (is.null(rec)) next
    for (f in fields) {
      if (!(f %in% names(M))) next
      cur <- M[[f]][i]
      if (!.is_vacant(cur)) next
      val <- rec[[f]]
      if (is.null(val) || length(val) == 0) next
      v <- as.character(val[[1]])
      if (is.na(v) || !nzchar(v)) next
      ## Coerce target column to character if needed (e.g. all-NA logical).
      if (!is.character(M[[f]])) M[[f]] <- as.character(M[[f]])
      M[[f]][i] <- v
      filled[f] <- filled[f] + 1L
      prov_rows[[length(prov_rows) + 1]] <- data.frame(
        SR        = if (has_sr) M$SR[i] else NA_character_,
        field     = f,
        source    = source_label,
        timestamp = ts,
        stringsAsFactors = FALSE
      )
    }
  }

  prov <- if (length(prov_rows) > 0) do.call(rbind, prov_rows) else NULL
  list(M = M, prov = prov, filled = filled)
}

# -- OpenAlex client -----------------------------------------------------------
# Uses openalexR::oa_fetch with output = "list" so we can reuse the helpers
# in R/resolveOAReferences.R (same code path biblioshiny uses) without going
# through openalexR's tibble post-processing. Returns a per-DOI list aligned
# with the Crossref output so .merge_enrichment can treat them uniformly.

#' @noRd
.enrich_from_openalex <- function(dois, fields, email, oa_apikey,
                                  progress = NULL, verbose = TRUE) {

  if (!requireNamespace("openalexR", quietly = TRUE)) {
    if (isTRUE(verbose)) {
      message("[completeMetadata] openalexR is not installed; skipping OpenAlex pass.")
    }
    return(list(by_doi = list(), fail_count = length(unique(dois))))
  }

  ## OpenAlex polite pool / API key plumbing -- set as env vars + options for
  ## the duration of the call (openalexR reads both).
  if (!is.null(email) && nzchar(email)) {
    op_old <- options(openalexR.mailto = email)
    on.exit(options(op_old), add = TRUE)
    Sys.setenv(openalexR.mailto = email)
  }
  if (!is.null(oa_apikey) && nzchar(oa_apikey)) {
    op_old2 <- options(openalexR.apikey = oa_apikey)
    on.exit(options(op_old2), add = TRUE)
    Sys.setenv(openalexR.apikey = oa_apikey)
  }

  uniq <- unique(dois)
  ## OpenAlex accepts up to 50 IDs per filter request.
  oa_batch <- 50L
  chunks <- split(uniq, ceiling(seq_along(uniq) / oa_batch))

  works <- list()
  fail_count <- 0L

  for (i in seq_along(chunks)) {
    chunk <- chunks[[i]]
    fetched <- tryCatch(
      openalexR::oa_fetch(
        entity  = "works",
        doi     = chunk,
        output  = "list",
        verbose = FALSE
      ),
      error = function(e) {
        if (isTRUE(verbose)) {
          message(sprintf("[completeMetadata] OpenAlex batch %d failed: %s",
                          i, conditionMessage(e)))
        }
        NULL
      }
    )
    if (is.null(fetched)) {
      fail_count <- fail_count + length(chunk)
    } else if (is.list(fetched)) {
      works <- c(works, fetched)
    }
    if (is.function(progress)) {
      progress(i * oa_batch, length(uniq),
               sprintf("OpenAlex batch %d / %d", i, length(chunks)))
    }
    if (isTRUE(verbose)) {
      message(sprintf("[completeMetadata] OpenAlex %d/%d batches done.",
                      i, length(chunks)))
    }
  }

  if (length(works) == 0) {
    return(list(by_doi = list(), fail_count = fail_count))
  }

  ## CR field requires resolving referenced_works (a second batch of fetches).
  ## We build a lookup ID -> WoS-style CR string and then assemble per-paper
  ## CR strings in the per-DOI parser.
  ref_lookup <- if ("CR" %in% fields) {
    .openalex_resolve_refs(works, email, oa_apikey,
                           progress = progress, verbose = verbose)
  } else {
    list()
  }

  parsed <- lapply(works, function(w) .openalex_work_to_row(w, fields,
                                                            ref_lookup))
  parsed <- parsed[!vapply(parsed, is.null, logical(1))]
  if (length(parsed) == 0) {
    return(list(by_doi = list(), fail_count = fail_count))
  }

  list(by_doi = stats::setNames(parsed,
                                vapply(parsed, function(r) r$DI, character(1))),
       fail_count = fail_count)
}

#' @noRd
.openalex_resolve_refs <- function(works, email, oa_apikey,
                                   progress = NULL, verbose = TRUE) {
  all_refs <- unlist(lapply(works, function(w) w$referenced_works),
                     use.names = FALSE)
  all_refs <- all_refs[!is.na(all_refs) & nzchar(all_refs)]
  if (length(all_refs) == 0) return(list())
  uniq_refs <- unique(all_refs)

  batch <- 50L
  n_chunks <- ceiling(length(uniq_refs) / batch)
  out <- list()
  for (i in seq_len(n_chunks)) {
    rng <- ((i - 1) * batch + 1):min(i * batch, length(uniq_refs))
    short_ids <- gsub("^https://openalex\\.org/", "", uniq_refs[rng])
    fetched <- tryCatch(
      openalexR::oa_fetch(
        entity      = "works",
        openalex_id = paste(short_ids, collapse = "|"),
        output      = "list",
        verbose     = FALSE
      ),
      error = function(e) NULL
    )
    if (is.list(fetched)) {
      for (w in fetched) {
        if (!is.null(w$id)) {
          cr <- build_single_cr(w)
          if (!is.na(cr)) out[[w$id]] <- cr
        }
      }
    }
    if (is.function(progress)) {
      progress(i * batch, length(uniq_refs),
               sprintf("OpenAlex refs %d / %d", i, n_chunks))
    }
  }
  out
}

#' @noRd
.openalex_work_to_row <- function(w, fields, ref_lookup) {
  doi <- .coalesce_chr(w$doi)
  if (is.na(doi)) return(NULL)
  doi <- tolower(sub("^https?://(dx\\.)?doi\\.org/", "", doi,
                     ignore.case = TRUE))
  if (!nzchar(doi)) return(NULL)

  out <- list(DI = doi)

  if ("TI" %in% fields) {
    out$TI <- .coalesce_chr(w$title %||% w$display_name)
  }
  if ("PY" %in% fields) {
    py <- w$publication_year
    out$PY <- if (!is.null(py) && !is.na(py)) as.character(py) else NA_character_
  }
  if ("LA" %in% fields) {
    out$LA <- .coalesce_chr(w$language)
  }
  if ("DT" %in% fields) {
    out$DT <- .openalex_doctype(.coalesce_chr(w$type))
  }
  if ("SO" %in% fields) {
    out$SO <- .coalesce_chr(
      tryCatch(w$primary_location$source$display_name, error = function(e) NA)
    )
  }
  if ("AB" %in% fields && !is.null(w$abstract_inverted_index)) {
    out$AB <- abstract_build(w$abstract_inverted_index)
  }
  if ("TC" %in% fields) {
    tc <- w$cited_by_count
    out$TC <- if (!is.null(tc) && !is.na(tc)) as.character(tc) else NA_character_
  }
  if (any(c("AU", "C1", "RP") %in% fields)) {
    info <- .openalex_authors(w$authorships)
    if ("AU" %in% fields) out$AU <- info$AU
    if ("C1" %in% fields) out$C1 <- info$C1
    if ("RP" %in% fields) out$RP <- info$RP
  }
  if ("CR" %in% fields) {
    refs <- w$referenced_works
    if (!is.null(refs) && length(refs) > 0 && length(ref_lookup) > 0) {
      strs <- unlist(lapply(refs, function(rid) ref_lookup[[rid]]),
                     use.names = FALSE)
      strs <- strs[!is.na(strs) & nzchar(strs)]
      if (length(strs) > 0) out$CR <- paste(strs, collapse = "; ")
    }
  }

  ## Replace empty-strings with NA so the merge step ignores them.
  out <- lapply(out, function(v) {
    if (is.character(v) && length(v) == 1 && (is.na(v) || !nzchar(v))) NA_character_
    else v
  })
  out
}

#' @noRd
.openalex_doctype <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_character_)
  x <- tolower(x)
  toupper(switch(
    x,
    "article"             = "ARTICLE",
    "review"              = "REVIEW",
    "book"                = "BOOK",
    "book-chapter"        = "BOOK CHAPTER",
    "report"              = "REPORT",
    "preprint"            = "PREPRINT",
    "dissertation"        = "DISSERTATION",
    "dataset"             = "DATA",
    "editorial"           = "EDITORIAL",
    "letter"              = "LETTER",
    "paratext"            = "PARATEXT",
    "reference-entry"     = "REFERENCE",
    "standard"            = "STANDARD",
    x
  ))
}

#' @noRd
.openalex_authors <- function(authorships) {
  if (is.null(authorships) || length(authorships) == 0) {
    return(list(AU = NA_character_, C1 = NA_character_, RP = NA_character_))
  }
  ## AU: WoS-style "LASTNAME I" -- match the convention used by csvScopus2df.
  fmt <- vapply(authorships, function(a) {
    nm <- a$author$display_name %||% ""
    if (!nzchar(nm)) return(NA_character_)
    parts <- unlist(strsplit(trimws(nm), "\\s+"))
    if (length(parts) == 0) return(NA_character_)
    if (length(parts) == 1) return(toupper(parts[1]))
    surname <- toupper(parts[length(parts)])
    given <- parts[-length(parts)]
    inits <- paste(toupper(substr(given, 1, 1)), collapse = "")
    trimws(paste(surname, inits))
  }, character(1))
  fmt <- fmt[!is.na(fmt) & nzchar(fmt)]
  AU <- if (length(fmt) > 0) paste(fmt, collapse = "; ") else NA_character_

  ## C1: union of institution display names across all authors.
  inst_names <- unlist(lapply(authorships, function(a) {
    if (is.null(a$institutions) || length(a$institutions) == 0) {
      return(character(0))
    }
    vapply(a$institutions, function(inst) {
      if (!is.null(inst$display_name)) inst$display_name else NA_character_
    }, character(1))
  }), use.names = FALSE)
  inst_names <- inst_names[!is.na(inst_names) & nzchar(inst_names)]
  inst_names <- unique(inst_names)
  C1 <- if (length(inst_names) > 0) {
    toupper(paste(inst_names, collapse = "; "))
  } else NA_character_

  ## RP: corresponding author affiliation. Prefer is_corresponding flag,
  ## fall back to first authorship.
  corr_idx <- which(vapply(authorships,
                           function(a) isTRUE(a$is_corresponding),
                           logical(1)))
  if (length(corr_idx) == 0) {
    first_idx <- which(vapply(authorships,
                              function(a) isTRUE(a$author_position == "first"),
                              logical(1)))
    corr_idx <- if (length(first_idx) > 0) first_idx[1] else 1L
  } else {
    corr_idx <- corr_idx[1]
  }
  rp_inst <- authorships[[corr_idx]]$institutions
  RP <- NA_character_
  if (!is.null(rp_inst) && length(rp_inst) > 0) {
    nm <- vapply(rp_inst, function(z) {
      if (!is.null(z$display_name)) z$display_name else NA_character_
    }, character(1))
    nm <- nm[!is.na(nm) & nzchar(nm)]
    if (length(nm) > 0) RP <- toupper(paste(nm, collapse = "; "))
  }
  if (is.na(RP)) {
    raw_aff <- authorships[[corr_idx]]$affiliations
    if (!is.null(raw_aff) && length(raw_aff) > 0) {
      raw <- raw_aff[[1]]$raw_affiliation_string
      if (!is.null(raw) && nzchar(raw)) RP <- toupper(raw)
    }
  }

  list(AU = AU, C1 = C1, RP = RP)
}
