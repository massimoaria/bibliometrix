utils::globalVariables(c("num"))
#' Merge bibliographic data frames from supported bibliogtraphic DBs
#'
#' Merge bibliographic data frames from different databases (WoS,SCOPUS, Lens, Openalex, etc-) into a single one.
#'
#' bibliographic data frames are obtained by the converting function \code{\link{convert2df}}.
#' The function merges data frames identifying common tag fields and duplicated records.
#'
#' @details
#' When the collections to merge come from \strong{more than one database}, the cited
#' references field \code{CR} is renamed \code{CR_raw} and \code{CR} is emptied
#' (set to the string \code{"NA"} for every document).
#'
#' This is intentional, not a loss of data. Each database writes its references in its
#' own format -- WoS uses \code{AUTHOR, YEAR, JOURNAL, DOI ...} strings, Scopus writes
#' full free-text citations, OpenAlex and Lens use identifiers -- so the same cited work
#' appears under different strings depending on the source. Keeping them in a single
#' \code{CR} column would make every reference-based measure count the same work more
#' than once. The original strings are preserved verbatim in \code{CR_raw}.
#'
#' The practical consequence is that reference-based analyses --
#' \code{\link{biblioNetwork}} with \code{analysis = "co-citation"} or
#' \code{"coupling"}, \code{\link{histNetwork}}, and local citation counts --
#' are \strong{not available} on a multi-database merged collection. Everything that
#' does not rely on \code{CR} (authors, sources, keywords, affiliations, countries,
#' collaboration and co-word networks) is unaffected. To run a citation analysis, work
#' on the single-database collections before merging them.
#'
#' Merging collections that all come from the \emph{same} database leaves \code{CR}
#' untouched.
#'
#' Author names are left as the single collections carry them: the merge only
#' harmonises how the databases punctuate them (Scopus writes the full names as
#' \code{REID, MONIQUE}, WoS as \code{REID MONIQUE}). Import the collections you
#' intend to merge with the \strong{same author name format} -- full names for all
#' of them, or surname and initials for all of them -- so that their \code{AU}
#' fields are comparable. OpenAlex records saved before names were put surname
#' first are rewritten on merging.
#'
#' When the collections come from more than one database, the record kept among
#' duplicates is the one with the most metadata, and the order of the databases
#' only decides ties. An OpenAlex record whose source is PubMed or PubMed Central
#' (OpenAlex gives the repository, not the journal) is kept only if every copy is
#' such a record. The kept record takes the highest citation count of its copies.
#'
#' @param ... are the bibliographic data frames to merge.
#' @param remove.duplicated is logical. If TRUE duplicated documents will be deleted from the bibliographic collection.
#' @param verbose is logical.  If TRUE, information on duplicate documents is printed on the screen.
#' @return the value returned from \code{mergeDbSources} is a bibliographic data frame.
#'   When the sources belong to different databases, the original cited references are
#'   returned in the \code{CR_raw} column and \code{CR} is empty (see \emph{Details}).
#'
#'
#' @examples
#'
#' data(isiCollection, package = "bibliometrixData")
#'
#' data(scopusCollection, package = "bibliometrixData")
#'
#' M <- mergeDbSources(isiCollection, scopusCollection, remove.duplicated = TRUE)
#'
#' dim(M)
#'
#' @seealso \code{\link{convert2df}} to import and convert an ISI or SCOPUS Export file in a bibliographic data frame.
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis.
#' @seealso \code{\link{summary}} to obtain a summary of the results.
#' @seealso \code{\link{plot}} to draw some useful plots of the results.
#'
#' @export


mergeDbSources <- function(..., remove.duplicated = TRUE, verbose = TRUE) {
  index <- NULL

  mc <- match.call(expand.dots = TRUE)

  if (length(mc) > 3) {
    M <- dplyr::bind_rows(list(...))
  } else {
    M <- dplyr::bind_rows(...)
  }
  # create KW_Merged field 
  M <- M %>% mergeKeywords(force=TRUE)

  # OpenAlex files saved by earlier versions still carry names in natural order
  M <- oaAuthorForm(M, which(M$DB == "OPENALEX"))

  dbLabels <- data.frame(
    DB = toupper(c("isi", "scopus", "openalex", "lens", "dimensions", "pubmed", "cochrane")),
    num = c(1, 2, 3, 4, 5, 6, 7)
  )
  DB <- unique(M$DB)
  
  if (length(DB) >1) {
    # order by db
    M <- M %>%
      left_join(dbLabels, by = "DB") %>%
      arrange(num) %>%
      select(-num) %>%
      rename("CR_raw" = "CR") %>%
      mutate(CR = "NA")

    # The reference formats of the different databases are not comparable, so the
    # merged CR cannot be used as it is. The original strings are kept in CR_raw.
    if (isTRUE(verbose)) {
      cat(paste0(
        "\nCollections from ", length(DB), " different databases (",
        paste(DB, collapse = ", "), ") have been merged.\n",
        "The cited references field CR has been emptied: each database writes its\n",
        "references in its own format, so the same cited work would be counted more\n",
        "than once. The original reference strings are preserved in the CR_raw field.\n",
        "Reference-based analyses (co-citation, bibliographic coupling, historiograph\n",
        "and local citations) are therefore not available on the merged collection.\n"
      ))
    }
  }
  

  if (isTRUE(remove.duplicated)) {
    # Which copy of a duplicated document to keep. Within a single database the
    # first one; across databases the one with the most metadata.
    rank <- if (length(DB) > 1) mergeRecordRank(M) else seq_len(nrow(M))

    # remove by DOI
    if ("DI" %in% names(M)) {
      M$DI[M$DI == ""] <- NA
      d <- mergeDuplicates(M$DI, rank)
      index <- which(d)
      if (length(index) > 0) {
        if (length(DB) > 1) M <- mergeKeepCitations(M, M$DI)
        M <- M[-index, ]
        rank <- rank[-index]
      }
    }

    # remove by title
    if ("TI" %in% names(M)) {
      TI <- gsub("[^[:alnum:] ]", "", M$TI)
      TI <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", TI, perl = TRUE)
      key <- paste(TI, " ", M$PY)
      d <- mergeDuplicates(key, rank)
      if (isTRUE(verbose)) cat("\n", sum(d) + length(index), "duplicated documents have been removed\n")
      if (any(d) && length(DB) > 1) M <- mergeKeepCitations(M, key)
      M <- M[!d, ]
    }
  }

  if (length(unique(M$DB)) > 1) {
    M$DB_Original <- M$DB
    M$DB <- "ISI"

    ## author data cleaning
    if ("AU" %in% names(M)) {
      # Every converter already emits author names in the WoS form -- surname,
      # a space, then the initials -- so the only thing that differs between
      # databases is how they punctuate the name: Scopus writes the full names
      # as "REID, MONIQUE", WoS as "REID MONIQUE". Harmonise the punctuation
      # and the separator, and nothing else.
      #
      # This used to rebuild every name as the surname plus the first letter of
      # a single token, which silently discarded information: "SELVANATHAN EA"
      # became "SELVANATHAN E" and "LAW CCH" became "LAW C", so authors sharing
      # a first initial were merged into one, and a collection imported with
      # Author Name Format = Fullname had its full names pushed back to
      # initials, undoing the choice made at import.
      M$AU <- gsub("\\.", "", M$AU)
      M$AU <- trimES(gsub(",", " ", M$AU))
      M$AU <- trimws(gsub("\\s*;\\s*", ";", M$AU))
    }
  }

  ## AU_CO and AU1_CO are derived from the addresses, and in a merged collection
  ## they are stale: a source where they had already been extracted brings its
  ## own values, while the documents coming from the other sources keep an empty
  ## one, so every country measure reads those as "no country" -- the merge of a
  ## collection carrying AU_CO with one that does not silently under-counts
  ## international collaboration. Drop them and let the analyses recompute from
  ## C1/RP, which is what a freshly converted collection looks like anyway.
  derived <- intersect(c("AU_CO", "AU1_CO"), names(M))
  if (length(derived) > 0) {
    M <- M[, setdiff(names(M), derived), drop = FALSE]
  }

  M <- metaTagExtraction(M, "SR")
  row.names(M) <- M$SR

  class(M) <- c("bibliometrixDB", "data.frame")
  return(M)
}

# The position of each record in the preference among copies of a document
# (1 = kept first): not an OpenAlex record with a repository as its source, then
# the most metadata, then the order of the rows, which is the order of the
# databases.
mergeRecordRank <- function(M) {
  fields <- intersect(
    c(
      "AU", "TI", "SO", "DI", "AB", "DE", "ID", "C1", "RP", "VL", "IS", "PP",
      "BP", "SN", "LA", "CR_raw"
    ),
    names(M)
  )
  filled <- function(x) {
    x <- as.character(x)
    !is.na(x) & nzchar(trimws(x)) & !toupper(trimws(x)) %in% c("NA", "NONE")
  }
  score <- Reduce(`+`, lapply(M[fields], filled), init = 0L)
  if ("TC" %in% names(M)) {
    score <- score + (!is.na(M$TC) & M$TC > 0)
  }
  repository <- M$DB == "OPENALEX" &
    grepl("^(PUBMED|PUBMED CENTRAL|EUROPE PMC.*)$", toupper(trimws(M$SO)))
  repository[is.na(repository)] <- FALSE

  rank <- integer(nrow(M))
  rank[order(repository, -score, seq_len(nrow(M)))] <- seq_len(nrow(M))
  rank
}

# TRUE for every copy of a document but the best ranked. A missing key is never
# a duplicate.
mergeDuplicates <- function(key, rank) {
  d <- rep(FALSE, length(key))
  ord <- order(rank)
  ord <- ord[!is.na(key[ord])]
  d[ord] <- duplicated(key[ord])
  d
}

# All the copies of a document take its highest citation count: a database that
# does not count citations (PubMed) would otherwise lose them.
mergeKeepCitations <- function(M, key) {
  if (!"TC" %in% names(M)) {
    return(M)
  }
  has <- !is.na(key)
  M$TC[has] <- ave(M$TC[has], key[has], FUN = function(x) {
    if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  })
  M
}
