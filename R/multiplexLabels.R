# Names of the roots of a multiplex coupling.
#
# A root is a group of documents that cite the same works: an intellectual
# base. Its name therefore comes from the content of its strong references (the
# references frequent among its documents and concentrated in it), not from the
# keywords of its documents, which name the themes. The content used is the
# title of each strong reference, taken from
#   1. the collection itself, when the reference is a document of it (by DOI);
#   2. the reference string, for Scopus, which writes the cited title;
#   3. OpenAlex, by DOI or OpenAlex id, only when the user is online and has
#      configured both an OpenAlex API key and an email; the titles are kept in
#      a cache for the R session, so that a repeated analysis downloads nothing.
# A root with fewer than five reference titles is named after its
# cited sources and its most representative reference.

# session cache of the OpenAlex titles of cited references, keyed by
# "doi:<doi>" or "id:<OpenAlex id>"; NA marks a work OpenAlex does not return
.mpCache <- new.env(parent = emptyenv())

mpCacheGet <- function(keys) {
  vapply(keys, function(k) {
    if (exists(k, envir = .mpCache, inherits = FALSE)) get(k, envir = .mpCache) else NA_character_
  }, character(1), USE.NAMES = FALSE)
}

mpCacheHas <- function(keys) {
  vapply(keys, exists, logical(1), envir = .mpCache, inherits = FALSE, USE.NAMES = FALSE)
}

# Index of the cited references of the analysed documents: one row per
# normalized reference (the unit of the references layer), with its DOI, its
# OpenAlex id, the title of the collection document with the same DOI, and the
# title written in the reference string (Scopus).
mpReferenceIndex <- function(M, M_all, keys) {
  CR <- gsub("DOI;", "DOI ", as.character(M$CR))
  raw <- trimws(unlist(strsplit(CR, ";")))
  raw <- raw[!is.na(raw) & nchar(raw) > 0]
  key <- .normalize_cr(raw)
  keep <- key %in% keys
  raw <- raw[keep]
  key <- key[keep]
  doi <- tolower(sub("^.*?\\bDOI\\s+(10\\.[^ ,;]+).*$", "\\1", raw, perl = TRUE))
  doi[!grepl("^10\\.", doi)] <- NA_character_
  doi <- sub("[.]$", "", doi)
  oaid <- ifelse(grepl("^W[0-9]+$", key), key, NA_character_)
  scopus <- isTRUE(any(toupper(M$DB) == "SCOPUS", na.rm = TRUE))
  stitle <- if (scopus) mpScopusTitle(raw) else rep(NA_character_, length(raw))
  first <- function(v) {
    s <- split(v, factor(key, levels = keys))
    vapply(s, function(x) {
      x <- x[!is.na(x)]
      if (length(x)) x[1] else NA_character_
    }, character(1), USE.NAMES = FALSE)
  }
  idx <- data.frame(ref = keys, doi = first(doi), oaid = first(oaid), title_scopus = first(stitle),
                    stringsAsFactors = FALSE)
  # references that are documents of the collection carry its title
  ti_by_doi <- if (all(c("DI", "TI") %in% names(M_all))) {
    d <- tolower(trimws(as.character(M_all$DI)))
    ok <- !is.na(d) & nchar(d) > 0 & !is.na(M_all$TI)
    stats::setNames(as.character(M_all$TI[ok]), d[ok])
  } else {
    character(0)
  }
  idx$title_local <- unname(ti_by_doi[idx$doi])
  idx
}

# title written in a Scopus reference: the text between the authors and the year
mpScopusTitle <- function(x) {
  t <- sub("^(.*?)\\s*\\(\\d{4}\\).*$", "\\1", x, perl = TRUE)
  t[t == x] <- NA_character_
  # the authors end at the last "Surname, I." group before the title
  t <- sub("^.*?(?:[A-Z][^,]*,\\s+(?:[A-Z]\\.\\s*-?)+,\\s+)+", "", t, perl = TRUE)
  t <- trimws(t)
  t[!is.na(t) & nchar(t) < 8] <- NA_character_
  t
}

# OpenAlex can be used when the user has configured both an API key and an
# email and api.openalex.org answers
mpOpenAlexReady <- function(email = NULL, api.key = NULL) {
  email <- .resolve_email(email)
  key <- .resolve_oa_apikey(api.key)
  if (is.null(email) || is.null(key)) {
    return(list(ok = FALSE, reason = "no OpenAlex API key and email configured"))
  }
  if (!requireNamespace("openalexR", quietly = TRUE)) {
    return(list(ok = FALSE, reason = "openalexR is not installed"))
  }
  online <- tryCatch({
    req <- httr2::req_timeout(httr2::request("https://api.openalex.org"), 5)
    resp <- httr2::req_perform(httr2::req_error(req, is_error = function(r) FALSE))
    httr2::resp_status(resp) < 500
  }, error = function(e) FALSE)
  if (!online) return(list(ok = FALSE, reason = "OpenAlex is not reachable"))
  list(ok = TRUE, email = email, api.key = key, reason = "")
}

# titles of works from OpenAlex, by DOI and by OpenAlex id, through the cache
mpFetchTitles <- function(doi, oaid, email, api.key, verbose = TRUE) {
  doi <- unique(doi[!is.na(doi)])
  oaid <- unique(oaid[!is.na(oaid)])
  # paste0() of a zero-length vector gives one string ("id:"), not none
  ck <- c(if (length(doi)) paste0("doi:", doi), if (length(oaid)) paste0("id:", oaid))
  todo_doi <- if (length(doi)) doi[!mpCacheHas(paste0("doi:", doi))] else character(0)
  todo_id <- if (length(oaid)) oaid[!mpCacheHas(paste0("id:", oaid))] else character(0)
  if (length(todo_doi) + length(todo_id)) {
    op <- options(openalexR.mailto = email, openalexR.apikey = api.key)
    on.exit(options(op), add = TRUE)
    old_env <- Sys.getenv(c("openalexR.mailto", "openalexR.apikey"), unset = NA)
    Sys.setenv(openalexR.mailto = email, openalexR.apikey = api.key)
    on.exit({
      for (v in names(old_env)) {
        if (is.na(old_env[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, stats::setNames(list(old_env[[v]]), v))
      }
    }, add = TRUE)
    if (isTRUE(verbose)) {
      message("Downloading from OpenAlex the titles of ", length(todo_doi) + length(todo_id), " references")
    }
    fetch <- function(keys, type) {
      for (chunk in split(keys, ceiling(seq_along(keys) / 50))) {
        works <- tryCatch(
          if (type == "doi") {
            openalexR::oa_fetch(entity = "works", doi = chunk, output = "list", verbose = FALSE)
          } else {
            openalexR::oa_fetch(entity = "works", openalex_id = paste(chunk, collapse = "|"),
                                output = "list", verbose = FALSE)
          },
          error = function(e) NULL
        )
        if (is.null(works) && length(chunk) > 1) {
          # a batch that fails as a whole (a malformed key): one key at a time
          for (one in chunk) fetch(one, type)
          next
        }
        if (is.null(works)) {
          # a single key OpenAlex cannot look up: cached as not found
          assign(paste0(type, ":", chunk), NA_character_, envir = .mpCache)
          next
        }
        if (!is.null(works$id)) works <- list(works) # a single work
        found <- stats::setNames(rep(NA_character_, length(chunk)), chunk)
        for (w in works) {
          title <- w$display_name %||% w$title
          if (is.null(title) || !nzchar(title)) next
          k <- if (type == "doi") tolower(sub("^https?://doi.org/", "", w$doi %||% "")) else sub("^.*/", "", w$id %||% "")
          if (k %in% names(found)) found[[k]] <- title
        }
        for (k in names(found)) assign(paste0(type, ":", k), found[[k]], envir = .mpCache)
      }
    }
    if (length(todo_doi)) fetch(unique(todo_doi), "doi")
    if (length(todo_id)) fetch(unique(todo_id), "id")
  }
  if (!length(ck)) return(stats::setNames(character(0), character(0)))
  stats::setNames(mpCacheGet(ck), ck)
}

# unigrams and bigrams of a title, without stopwords and generic title words.
# A bigram joins two words adjacent in the title, within the same segment
# (punctuation separates segments), both of them kept: "Journal of Business"
# gives no "journal business", "entrepreneurship: past research" no
# "entrepreneurship past".
mpTitleTerms <- function(title) {
  stop <- unique(c(tidytext::stop_words$word, "study", "studies", "approach", "approaches", "evidence",
                   "effect", "effects", "role", "new", "based", "using", "perspective", "paper", "case",
                   "analysis", "research", "review", "toward", "towards", "insights", "implications",
                   "past", "present", "future", "years", "decade", "decades", "journal", "journals",
                   "field", "fields", "agenda", "directions", "issues", "introduction", "special",
                   "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety",
                   "hundred", "anniversary"))
  keep <- function(w) nchar(w) > 2 & !grepl("^[0-9]+$", w) & !(w %in% stop)
  # "&" is a conjunction: "Business & Industrial" gives no "business industrial"
  segs <- strsplit(gsub("&", " and ", tolower(title), fixed = TRUE), "[:;,.?!()\\[\\]\"]+")[[1]]
  out <- character(0)
  for (sg in segs) {
    w <- strsplit(gsub("[^a-z0-9 -]", " ", sg), "\\s+")[[1]]
    w <- gsub("^-+|-+$", "", w)
    w <- w[nchar(w) > 0]
    if (!length(w)) next
    k <- keep(w)
    if (length(w) > 1) {
      ok <- k[-length(w)] & k[-1]
      out <- c(out, paste(w[-length(w)], w[-1])[ok])
    }
    out <- c(out, w[k])
  }
  unique(out)
}

# Label of every root from the titles of its strong references.
mpRootsLabels <- function(mc, memb, n.labels = 3, n.refs = 30, email = NULL, api.key = NULL,
                          verbose = TRUE) {
  X <- mc$X_R > 0
  Z <- mpMembershipMatrix(memb)
  K <- nrow(Z)
  inC <- as.matrix(Z %*% X)
  tot <- Matrix::colSums(X)
  refs <- colnames(X)
  strong <- lapply(seq_len(K), function(k) {
    sc <- inC[k, ] * inC[k, ] / pmax(tot, 1)
    top <- order(-sc)[seq_len(min(n.refs, sum(sc > 0)))]
    data.frame(ref = refs[top], w = sc[top] / sum(sc[top]), stringsAsFactors = FALSE)
  })
  idx <- mc$refs
  needed <- unique(unlist(lapply(strong, `[[`, "ref")))
  ix <- idx[match(needed, idx$ref), ]
  title <- ifelse(!is.na(ix$title_local), ix$title_local, ix$title_scopus)
  origin <- ifelse(!is.na(ix$title_local), "collection", ifelse(!is.na(ix$title_scopus), "reference string", NA))
  # the remaining titles from OpenAlex, only when the user is online and has
  # configured an API key and an email
  miss <- is.na(title) & (!is.na(ix$doi) | !is.na(ix$oaid))
  oa <- list(ok = FALSE, reason = "not needed")
  if (any(miss)) {
    oa <- mpOpenAlexReady(email, api.key)
    if (oa$ok) {
      d <- ix$doi[miss & !is.na(ix$doi)]
      o <- ix$oaid[miss & is.na(ix$doi) & !is.na(ix$oaid)]
      got <- mpFetchTitles(d, o, oa$email, oa$api.key, verbose)
      ft <- ifelse(!is.na(ix$doi), got[paste0("doi:", ix$doi)], got[paste0("id:", ix$oaid)])
      fill <- miss & !is.na(ft)
      title[fill] <- ft[fill]
      origin[fill] <- "OpenAlex"
    } else if (isTRUE(verbose)) {
      message("Root names without OpenAlex (", oa$reason, "): from the titles in the collection",
              " and in the references, otherwise from the cited sources")
    }
  }
  title_of <- stats::setNames(title, needed)
  origin_of <- stats::setNames(origin, needed)

  # terms of every root: weight of the strong references whose title has the
  # term, then idf across the roots
  terms_by_root <- lapply(strong, function(s) {
    t <- title_of[s$ref]
    has <- !is.na(t)
    if (!any(has)) return(stats::setNames(numeric(0), character(0)))
    tt <- lapply(t[has], mpTitleTerms)
    w <- rep(s$w[has], lengths(tt))
    tapply(w, unlist(tt), sum)
  })
  vocab <- unique(unlist(lapply(terms_by_root, names)))
  df <- table(factor(unlist(lapply(terms_by_root, names)), levels = vocab))
  idf <- log(1 + K / as.numeric(df[vocab]))
  names(idf) <- vocab

  out <- data.frame(terms = character(K), label_source = character(K), stringsAsFactors = FALSE)
  for (k in seq_len(K)) {
    s <- strong[[k]]
    n_titled <- sum(!is.na(title_of[s$ref]))
    sc <- terms_by_root[[k]]
    if (n_titled >= 5 && length(sc)) {
      sc <- sc * idf[names(sc)] * ifelse(grepl(" ", names(sc)), 1.5, 1)
      sc <- sort(sc, decreasing = TRUE)
      chosen <- character(0)
      for (t in names(sc)) {
        overlap <- any(vapply(chosen, function(c) grepl(t, c, fixed = TRUE) || grepl(c, t, fixed = TRUE), logical(1)))
        if (!overlap) chosen <- c(chosen, t)
        if (length(chosen) == n.labels) break
      }
      src <- table(origin_of[s$ref][!is.na(title_of[s$ref])])
      out$terms[k] <- paste(chosen, collapse = "; ")
      out$label_source[k] <- sprintf("titles of %d of %d strong references (%s)", n_titled, nrow(s),
                                     paste(names(src), collapse = ", "))
    } else {
      # too few titles: the cited sources and the most representative reference
      so <- mpRefSource(s$ref)
      ws <- tapply(s$w, so, sum)
      ws <- ws[names(ws) != ""]
      top_so <- names(sort(ws, decreasing = TRUE))[seq_len(min(2, length(ws)))]
      out$terms[k] <- tolower(paste(c(top_so, mpShortRef(s$ref[1])), collapse = "; "))
      out$label_source[k] <- "cited sources (too few reference titles)"
    }
  }
  attr(out, "openalex") <- oa$reason
  out
}

# source of a normalized reference: what follows the year ("COHEN WM 1990 ADMIN SCI QUART")
mpRefSource <- function(x) {
  s <- ifelse(grepl("\\b[12][0-9]{3}\\b", x), sub("^.*?\\b[12][0-9]{3}\\b\\s*", "", x, perl = TRUE), "")
  trimws(s)
}
