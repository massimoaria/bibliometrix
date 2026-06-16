utils::globalVariables(c(
  "nLABEL", "LABEL", "GCS", "CITING", "CIT_PY", "paper", "TC",
  "AU", "Page.start", "Page.end", "PP", "SR", "Included",
  "PP.y", "PP.x", "toRemove", "SR_cited", "LCS", "SR_FULL",
  "TI", "DE", "ID", "DI", "Year", "SR_citing", "ref", "n",
  "id_oa", "UT", "PY","TI_clean","CR_clean","is_match", "cited_SR", "citing_SR"
))
#' Historical co-citation network
#'
#' \code{histNetwork} creates a historical citation network from a bibliographic
#' data frame.
#'
#' @param M is a bibliographic data frame obtained by the converting function
#'   \code{\link{convert2df}}. It is a data matrix with cases corresponding to
#'   manuscripts and variables to Field Tag in the original SCOPUS, OpenAlex, Lens.org and Clarivate
#'   Analytics Web of Science file.
#' @param min.citations DEPRECATED. New algorithm does not use this parameters. It will be remove in the next version of bibliometrix.
#' @param sep is the field separator character. This character separates strings
#'   in CR column of the data frame. The default is \code{sep = ";"}.
#' @param network is logical. If TRUE, function calculates and returns also the direct citation network. If FALSE,
#' the function returns only the local citation table.
#' @param verbose is logical. If TRUE, results are printed on screen.
#' @return \code{histNetwork} returns an object of \code{class} "list"
#'   containing the following components:
#'
#'   \tabular{lll}{ NetMatrix \tab  \tab the historical co-citation network
#'   matrix\cr histData \tab      \tab the set of n most cited references\cr M
#'   \tab      \tab the bibliographic data frame}
#'
#'
#' @examples
#' \dontrun{
#' data(management, package = "bibliometrixData")
#'
#' histResults <- histNetwork(management, sep = ";")
#' }
#'
#' @seealso \code{\link{convert2df}} to import and convert a supported
#'   export file in a bibliographic data frame.
#' @seealso \code{\link{summary}} to obtain a summary of the results.
#' @seealso \code{\link{plot}} to draw some useful plots of the results.
#' @seealso \code{\link{biblioNetwork}} to compute a bibliographic network.
#'
#' @export

histNetwork <- function(M, min.citations, sep = ";", network = TRUE, verbose = TRUE) {
  min.citations <- 0
  db <- M$DB[1]
  if (!("DI" %in% names(M))) {
    M$DI <- ""
  } else {
    M$DI[is.na(M$DI)] <- ""
  }
  has_cr <- "CR" %in% names(M) && any(!is.na(M$CR) & M$CR != "")
  has_crids <- "CRids" %in% names(M) && any(!is.na(M$CRids) & M$CRids != "")
  if (!has_cr && !has_crids) {
    cat("\nYour collection does not contain Cited References metadata (Field CR is missing)\n")
    return(NA)
  }

  M$TC[is.na(M$TC)] <- 0

  if (db == "ISI") db <- "WOS"
  switch(db,
    WOS = {
      results <- wos(M = M, min.citations = min.citations, sep = sep, network = network, verbose = verbose)
    },
    SCOPUS = {
      results <- scopus(M = M, min.citations = min.citations, sep = sep, network = network, verbose = verbose)
    },
    OPENALEX = {
      results <- openalex(M = M, min.citations = min.citations, sep = sep, network = network, verbose = verbose)
    },
    LENS = {
      results <- lens(M = M, min.citations = min.citations, sep = sep, network = network, verbose = verbose)
    },
    {
      cat("\nDatabase not compatible with direct citation analysis\n")
    }
  )

  return(results)
}


wos <- function(M, min.citations, sep, network, verbose) {
  if (isTRUE(verbose)) {
    cat("\nWOS DB:\nSearching local citations (LCS) by reference items (SR) and DOIs...\n")
  }
  if (!("SR_FULL" %in% names(M))) {
    M <- metaTagExtraction(M, Field = "SR")
  }

  M <- M[order(M$PY), ]
  M$Paper <- 1:nrow(M)
  M_orig <- M
  M$nLABEL <- 1:nrow(M)
  # papers <- M$nLABEL[M$TC >= min.citations]

  # Reference list and citing papers
  CR <- strsplit(M$CR, sep)

  CR <- lapply(seq_along(CR), function(i) {
    l <- data.frame(
      ref = CR[[i]],
      paper = i
    )
  })
  CR <- (do.call(rbind, CR))
  CR$DI <-
    trimws(unlist(lapply(
      strsplit(CR$ref, "DOI", fixed = TRUE), "[", 2
    )))
  CR$DI[is.na(CR$DI) | CR$DI == "NA"] <- ""
  CR$AU <-
    trimws(gsub("[ ]{2,}", "", (gsub(
      "\\.", " ", unlist(lapply(strsplit(CR$ref, ",", fixed = TRUE), "[", 1))
    ))))
  CR$PY <-
    trimws(unlist(lapply(strsplit(CR$ref, ",", fixed = TRUE), "[", 2)))
  CR$SO <-
    trimws(unlist(lapply(strsplit(CR$ref, ",", fixed = TRUE), "[", 3)))
  CR$SR <- paste(CR$AU, ", ", CR$PY, ", ", CR$SO, sep = "")

  if (isTRUE(verbose)) {
    cat("\nAnalyzing", nrow(CR), "reference items...\n")
  }
  # Local cited documents by DOI and reference item
  # M=M[papers,]
  M$LABEL <- paste(M$SR_FULL, "DOI", toupper(M$DI))

  CR$LABEL <- paste(CR$SR, "DOI", CR$DI)

  # By reference
  L <- left_join(M, CR, by = c("LABEL"))
  L <- L[!is.na(L$paper), ]
  L$CITING <- M$LABEL[L$paper]
  L$nCITING <- M$nLABEL[L$paper]
  L$CIT_PY <- M$PY[L$paper]

  LCS <- L %>%
    group_by(nLABEL) %>%
    summarize(
      LABEL = LABEL[1],
      n = length(nLABEL)
    ) %>%
    as.data.frame()

  M$LCS <- 0
  M[LCS$nLABEL, "LCS"] <- LCS$n
  M_orig$LCS <- M$LCS

  histData <- M[c("LABEL", "TI", "DE", "ID", "DI", "PY", "LCS", "TC")]
  names(histData) <- c("Paper", "Title", "Author_Keywords", "KeywordsPlus", "DOI", "Year", "LCS", "GCS")
  histData <- histData %>%
    dplyr::filter(GCS >= min.citations)

  if (isTRUE(network)) {
    # Citing data frame
    CITING <- L %>%
      group_by(CITING) %>%
      summarize(
        LCR = paste(LABEL, collapse = ";"),
        PY = CIT_PY[1],
        Paper = paper[1]
      ) %>%
      ungroup() %>%
      arrange(PY) %>%
      as.data.frame()

    M_orig$LCR <- NA
    M_orig$LCR[CITING$Paper] <- CITING$LCR
    M_orig$LABEL <- M$LABEL
    M <- M_orig %>%
      dplyr::filter(TC >= min.citations)


    ## assign an unique name to each document
    st <- i <- 0
    while (st == 0) {
      ind <- which(duplicated(M$LABEL))
      if (length(ind) > 0) {
        i <- i + 1
        M$LABEL[ind] <- paste0(M$LABEL[ind], "-", letters[i], sep = "")
      } else {
        st <- 1
      }
    }
    row.names(M) <- M$LABEL

    # NetMatrix
    WLCR <- cocMatrix(M, "LCR", sep = ";")
    missingLABEL <- setdiff((M$LABEL), colnames(WLCR))
    colLab <- c(colnames(WLCR), missingLABEL)
    WLCR <- cbind(WLCR, matrix(0, nrow(WLCR), length(missingLABEL)))
    WLCR <- as.data.frame(as.matrix(WLCR))
    colnames(WLCR) <- colLab
    LABEL <- (row.names(WLCR))
    WLCR <- as.matrix(WLCR[LABEL])
    # row.names(WLCR) <- LABEL
  } else {
    WLCR <- NULL
  }

  if (isTRUE(verbose)) {
    cat(
      "\nFound",
      length(M$LCS[M$LCS > 0]),
      "documents with no empty Local Citations (LCS)\n"
    )
  }
  results <-
    list(
      NetMatrix = WLCR,
      histData = histData,
      M = M_orig,
      LCS = M$LCS
    )

  return(results)
}

# New algorithm for Scopus
scopus <- function(M, min.citations, sep, network, verbose) {
  if (!("SR_FULL" %in% names(M))) {
    M <- metaTagExtraction(M, Field = "SR")
  }
  
  CR <- match_citations_fast(M = M, sep = sep)

  LCS <- CR %>% 
    group_by(SR_cited) %>%
    count(name = "LCS")

  M <- M %>%
    left_join(LCS, by = c("SR" = "SR_cited")) %>%
    mutate(LCS = ifelse(is.na(LCS), 0, LCS))

  histData <- M %>%
    select(SR_FULL, TI, DE, ID, DI, PY, LCS, TC) %>%
    rename(
      Paper = SR_FULL,
      Title = TI,
      Author_Keywords = DE,
      KeywordsPlus = ID,
      DOI = DI,
      Year = PY,
      GCS = TC
    ) %>%
    arrange(Year) %>%
    as.data.frame()

  names(histData) <- c("Paper", "Title", "Author_Keywords", "KeywordsPlus", "DOI", "Year", "LCS", "GCS")

  if (isTRUE(network)) {
    CRadd <- data.frame(SR_citing = unique(M$SR), SR_cited = unique(M$SR), value = 1)

    WLCR <- CR %>%
      select(SR_citing, SR_cited) %>%
      mutate(value = 1) %>%
      bind_rows(CRadd) %>%
      distinct() %>%
      pivot_wider(names_from = "SR_cited", values_from = "value", values_fill = 0) %>%
      dplyr::filter(SR_citing %in% CRadd$SR_cited)

    SRrow <- WLCR$SR_citing
    SRcol <- colnames(WLCR)[-1]

    WLCR <- as.matrix(WLCR %>% select(-1))
    row.names(WLCR) <- SRrow
    colnames(WLCR) <- SRcol
  } else {
    WLCR <- NULL
  }

  if (isTRUE(verbose)) {
    cat(
      "\nFound",
      length(M$LCS[M$LCS > 0]),
      "documents with no empty Local Citations (LCS)\n"
    )
  }

  results <-
    list(
      NetMatrix = WLCR,
      histData = histData,
      M = M,
      LCS = M$LCS
    )
}

openalex <- function(M, min.citations = min.citations, sep = sep, network = network, verbose = verbose) {
  ids <- M$id_oa

  # Use CRids (OpenAlex IDs) if available, otherwise fall back to CR
  if ("CRids" %in% names(M) && any(!is.na(M$CRids) & M$CRids != "")) {
    cr_col <- M$CRids
  } else {
    cr_col <- M$CR
  }
  cr_col[is.na(cr_col) | cr_col == ""] <- "none"

  CR <- strsplit(cr_col, ";")
  CR <- data.frame(id_oa = rep(M$id_oa, lengths(CR)), ref = trimws(unlist(CR))) %>%
    dplyr::filter(ref %in% ids)

  LCS <- CR %>%
    count(id_oa = ref) %>%
    rename(LCS = n)

  histData <- M %>%
    left_join(LCS, by = c("id_oa")) %>%
    mutate(
      LCS = ifelse(is.na(LCS), 0, LCS),
      DE = ID
    ) %>%
    rename(
      LABEL = SR,
      GCS = TC
    ) %>%
    select(c("LABEL", "TI", "DE", "ID", "DI", "PY", "LCS", "GCS")) %>%
    dplyr::filter(GCS >= min.citations)

  names(histData) <- c("Paper", "Title", "Author_Keywords", "KeywordsPlus", "DOI", "Year", "LCS", "GCS")

  if (isTRUE(network)) {
    CRadd <- data.frame(id_oa = unique(M$id_oa), ref = unique(M$id_oa), value = 1)
    WLCR <- CR %>%
      mutate(value = 1) %>%
      bind_rows(CRadd) %>%
      distinct() %>%
      pivot_wider(names_from = "ref", values_from = "value", values_fill = 0) %>%
      dplyr::filter(id_oa %in% CRadd$ref)

    SRrow <- WLCR %>%
      select(id_oa) %>%
      left_join(
        M %>%
          select(id_oa, SR),
        by = "id_oa"
      )

    SR_col <- data.frame(id_oa = colnames(WLCR)[-1]) %>%
      left_join(
        M %>%
          select(id_oa, SR),
        by = "id_oa"
      )

    WLCR <- as.matrix(WLCR %>% select(-1))
    row.names(WLCR) <- SRrow$SR
    colnames(WLCR) <- SR_col$SR
    WLCR <- WLCR[colnames(WLCR), colnames(WLCR)]
  } else {
    WLCR <- NULL
  }

  results <-
    list(
      NetMatrix = WLCR,
      histData = histData,
      M = M %>%
        left_join(LCS, by = "id_oa") %>%
        replace_na(list(LCS = 0)),
      LCS = M$LCS
    )
}

lens <- function(M, min.citations = min.citations, sep = sep, network = network, verbose = verbose) {
  M$CR[is.na(M$CR)] <- "none"
  ids <- M$UT
  CR <- lapply(strsplit(M$CR, ";"), trimws)
  CR <- data.frame(UT = rep(M$UT, lengths(CR)), ref = unlist(CR)) %>%
    dplyr::filter(ref %in% ids)

  LCS <- CR %>%
    count(UT = ref) %>%
    rename(LCS = n)

  histData <- M %>%
    left_join(LCS, by = c("UT")) %>%
    mutate(
      LCS = ifelse(is.na(LCS), 0, LCS),
      DE = ID
    ) %>%
    rename(
      LABEL = SR,
      GCS = TC
    ) %>%
    select(c("LABEL", "TI", "DE", "ID", "DI", "PY", "LCS", "GCS")) %>%
    dplyr::filter(GCS >= min.citations)

  names(histData) <- c("Paper", "Title", "Author_Keywords", "KeywordsPlus", "DOI", "Year", "LCS", "GCS")

  if (isTRUE(network)) {
    CRadd <- data.frame(UT = unique(M$UT), ref = unique(M$UT), value = 1)
    WLCR <- CR %>%
      mutate(value = 1) %>%
      bind_rows(CRadd) %>%
      distinct() %>%
      pivot_wider(names_from = "ref", values_from = "value", values_fill = 0) %>%
      dplyr::filter(UT %in% CRadd$ref)

    SRrow <- WLCR %>%
      select(UT) %>%
      left_join(
        M %>%
          select(UT, SR),
        by = "UT"
      )

    SR_col <- data.frame(UT = colnames(WLCR)[-1]) %>%
      left_join(
        M %>%
          select(UT, SR),
        by = "UT"
      )

    WLCR <- as.matrix(WLCR %>% select(-1))
    row.names(WLCR) <- SRrow$SR
    colnames(WLCR) <- SR_col$SR
    WLCR <- WLCR[colnames(WLCR), colnames(WLCR)]
  } else {
    WLCR <- NULL
  }

  results <-
    list(
      NetMatrix = WLCR,
      histData = histData,
      M = M %>%
        left_join(LCS, by = "UT") %>%
        replace_na(list(LCS = 0)),
      LCS = M$LCS
    )
}

# Funzione alternativa più veloce per dataset molto grandi
# match_citations_fast <- function(titles_df, references_df) {
#   
#   # Normalizza
#   titles_norm <- titles_df %>%
#     mutate(TI_clean = normalize_text(TI))
#   
#   refs_norm <- references_df %>%
#     mutate(CR_clean = normalize_text(CR))
#   
#   # Crea una matrice di matching usando stringdist
#   results <- expand_grid(
#     SR_cited = titles_norm$SR,
#     SR_citing = refs_norm$SR
#   ) %>%
#     left_join(titles_norm %>% select(SR, TI_clean), by = c("SR_cited" = "SR")) %>%
#     left_join(refs_norm %>% select(SR, CR_clean), by = c("SR_citing" = "SR")) %>%
#     mutate(
#       is_match = str_detect(CR_clean, fixed(TI_clean))
#     ) %>%
#     filter(is_match) %>%
#     select(SR_cited, SR_citing)
#   
#   return(results)
# }
# 
# # Funzione to normalize text
# normalize_text <- function(text) {
#   text %>%
#     str_to_upper() %>%
#     str_replace_all("[^A-Z0-9\\s]", " ") %>%
#     str_squish()
# }
#' Match local citations for Scopus collections (flat data frame).
#'
#' A citing paper cites a target paper when ONE of its INDIVIDUAL references
#' carries the target's first-author surname AND publication year AND (a substring
#' of) its title. Earlier this matched the target title as a substring of the whole
#' concatenated CR list, with no author/year guard — which over-counts badly for
#' short/generic titles: a book chapter titled "INTRODUCTION" matched every
#' reference list that merely contained the word "introduction" (LCS far above the
#' true count, even LCS > GCS). Scopus also ships two reference layouts (classic
#' "...(YEAR) JOURNAL" and the newer "...JOURNAL, ..., (YEAR)"); keying on the
#' per-reference first author + parenthesised year is robust to both.
#'
#' @param M bibliographic data frame (needs SR, AU, PY, TI, CR).
#' @param sep field separator splitting individual references in CR.
#' @return data.frame(SR_cited, SR_citing) of confirmed local-citation links.
#' @keywords internal
match_citations_fast <- function(M, sep = ";") {
  norm <- function(x) stringr::str_squish(stringr::str_replace_all(
    stringr::str_to_upper(x), "[^A-Z0-9\\s]", " "))
  # First space-delimited token, punctuation stripped -> surname key. Stable across
  # "FARZANEGAN MR" / "FARZANEGAN M.R." and hyphenated names ("RAMOS-RODRIGUEZ").
  surname <- function(au) toupper(gsub("[^A-Za-z0-9]", "", sub("[ ].*", "", trimws(au))))

  # Cited-document keys (drop rows with no title/author/year to anchor on).
  cited <- data.frame(
    SR_cited = M$SR,
    AU_sur   = surname(sub(";.*", "", M$AU)),
    PY       = as.character(M$PY),
    TI_clean = norm(M$TI),
    stringsAsFactors = FALSE
  )
  cited <- cited[nzchar(cited$TI_clean) & nzchar(cited$AU_sur) & !is.na(cited$PY) & cited$PY != "NA", ]

  # Citing-side INDIVIDUAL references (flat: split the concatenated CR list).
  sp  <- strsplit(as.character(M$CR), sep)
  refs <- data.frame(
    SR_citing = rep(M$SR, lengths(sp)),
    ref       = trimws(unlist(sp)),
    stringsAsFactors = FALSE
  )
  el <- strsplit(refs$ref, ",", fixed = TRUE)
  refs$AU_sur    <- surname(vapply(el, function(l) if (length(l)) l[[1]] else "", character(1)))
  refs$PY        <- sub(".*\\((\\d{4})\\).*", "\\1", refs$ref)
  refs$PY[!grepl("^\\d{4}$", refs$PY)] <- NA_character_
  refs$ref_clean <- norm(refs$ref)
  refs <- refs[nzchar(refs$AU_sur) & !is.na(refs$PY), ]

  if (nrow(cited) == 0 || nrow(refs) == 0) {
    return(data.frame(SR_cited = character(0), SR_citing = character(0)))
  }

  # Candidate pairs share first-author surname AND year; then require the target
  # title to appear within that single reference. The author+year join is selective,
  # so this stays cheap despite the per-reference granularity.
  cand <- dplyr::inner_join(cited, refs, by = c("AU_sur", "PY"),
                            relationship = "many-to-many")
  if (nrow(cand) == 0) {
    return(data.frame(SR_cited = character(0), SR_citing = character(0)))
  }
  cand <- cand[stringr::str_detect(cand$ref_clean, stringr::fixed(cand$TI_clean)), ]
  matches <- unique(cand[cand$SR_cited != cand$SR_citing, c("SR_cited", "SR_citing"), drop = FALSE])
  rownames(matches) <- NULL
  matches
}