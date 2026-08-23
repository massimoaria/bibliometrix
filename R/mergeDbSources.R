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
    # remove by DOI
    if ("DI" %in% names(M)) {
      M$DI[M$DI == ""] <- NA
      index <- which(duplicated(M$DI) & !is.na(M$DI))
      if (length(index) > 0) M <- M[-index, ]
    }

    # remove by title
    if ("TI" %in% names(M)) {
      TI <- gsub("[^[:alnum:] ]", "", M$TI)
      TI <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", TI, perl = TRUE)
      d <- duplicated(paste(TI, " ", M$PY))
      if (isTRUE(verbose)) cat("\n", sum(d) + length(index), "duplicated documents have been removed\n")
      M <- M[!d, ]
    }
  }

  if (length(unique(M$DB)) > 1) {
    M$DB_Original <- M$DB
    M$DB <- "ISI"

    ## author data cleaning
    if ("AU" %in% names(M)) {
      M$AU <- gsub(",", " ", M$AU)
      AUlist <- strsplit(M$AU, ";")
      AU <- lapply(AUlist, function(l) {
        l <- trim(l)
        name <- strsplit(l, " ")
        lastname <- unlist(lapply(name, function(ln) {
          ln[1]
        }))
        firstname <- lapply(name, function(ln) {
          f <- paste(substr(ln[-1], 1, 1), collapse = " ")
        })
        AU <- paste(lastname, unlist(firstname), sep = " ", collapse = ";")
        return(AU)
      })
      M$AU <- unlist(AU)
    }
  }

  M <- metaTagExtraction(M, "SR")
  row.names(M) <- M$SR

  class(M) <- c("bibliometrixDB", "data.frame")
  return(M)
}
