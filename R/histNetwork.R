#' Historical co-citation network
#'
#' \code{histNetwork} creates a historical citation network from a bibliographic
#' data frame.
#'
#' @param M is a bibliographic data frame obtained by the converting function
#'   \code{\link{convert2df}}. It is a data matrix with cases corresponding to
#'   manuscripts and variables to Field Tag in the original SCOPUS, OpenAlex, Lens.org and Clarivate
#'   Analitics Web of Science file.
#' @param min.citations DEPRECATED. New algorithm does not use this parameters. It will be remove in the next version of bibliometrix. 
#' @param sep is the field separator character. This character separates strings
#'   in CR column of the data frame. The default is \code{sep = ";"}.
#' @param network is logical. If TRUE, fuction calculates and returns also the direct citation network. If FALSE,
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
#' 
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

histNetwork<-function(M, min.citations, sep = ";", network = TRUE, verbose = TRUE){
  
  min.citations = 0
  db <- M$DB[1]
  if (!("DI" %in% names(M))){M$DI <- ""}else{M$DI[is.na(M$DI)] <- ""}
  if (!("CR" %in% names(M))){
    cat("\nYour collection does not contain Cited References metadata (Field CR is missing)\n")
    return(NA)
  }
  
  M$TC[is.na(M$TC)] <- 0
  
  if (db == "ISI") db <- "WOS"
  switch(db,
         WOS={
           results <- wos(M=M, min.citations=min.citations, sep=sep, network=network, verbose=verbose)
         },
         SCOPUS={
           results <- scopus(M=M, min.citations=min.citations, sep=sep, network=network, verbose=verbose)
         },
         OPENALEX={
           results <- openalex(M=M, min.citations=min.citations, sep=sep, network=network, verbose=verbose)
         },
         LENS={
           results <- lens(M=M, min.citations=min.citations, sep=sep, network=network, verbose=verbose)
         },
         {cat("\nDatabase not compatible with direct citation analysis\n")})
  
  return(results)
}


wos <- function(M, min.citations, sep, network, verbose){
  if (isTRUE(verbose)) {
    cat("\nWOS DB:\nSearching local citations (LCS) by reference items (SR) and DOIs...\n")
  }
  if (!("SR_FULL" %in% names(M))) {
    M = metaTagExtraction(M, Field = "SR")
  }
  
  M = M[order(M$PY), ]
  M$Paper <- 1:nrow(M)
  M_orig <- M
  M$nLABEL <- 1:nrow(M)
  #papers <- M$nLABEL[M$TC >= min.citations]
  
  # Reference list and citing papers
  CR <- strsplit(M$CR, sep)
  
  CR <- lapply(seq_along(CR), function(i) {
    l <- data.frame(ref = CR[[i]],
                    paper = i)
  })
  CR <- (do.call(rbind, CR))
  CR$DI <-
    trimws(unlist(lapply(
      strsplit(CR$ref, 'DOI', fixed = TRUE), '[', 2
    )))
  CR$DI[is.na(CR$DI) | CR$DI=="NA"] <- ""
  CR$AU <-
    trimws(gsub("[ ]{2,}", "", (gsub(
      "\\.", " ", unlist(lapply(strsplit(CR$ref, ',', fixed = TRUE), '[', 1))
    ))))
  CR$PY <-
    trimws(unlist(lapply(strsplit(CR$ref, ',', fixed = TRUE), '[', 2)))
  CR$SO <-
    trimws(unlist(lapply(strsplit(CR$ref, ',', fixed = TRUE), '[', 3)))
  CR$SR <- paste(CR$AU, ", ", CR$PY, ", ", CR$SO, sep = "")
  
  if (isTRUE(verbose)) {
    cat("\nAnalyzing", nrow(CR),"reference items...\n")
  }
  # Local cited documents by DOI and reference item
  #M=M[papers,]
  M$LABEL <- paste(M$SR_FULL, "DOI", toupper(M$DI))
  
  CR$LABEL <- paste(CR$SR, "DOI",CR$DI)
  
  # By reference
  L <- left_join(M,CR,by=c("LABEL"))
  L <- L[!is.na(L$paper),]
  L$CITING <- M$LABEL[L$paper]
  L$nCITING <- M$nLABEL[L$paper]
  L$CIT_PY <- M$PY[L$paper]
  
  LCS <- L %>% group_by(.data$nLABEL) %>%
    summarize(LABEL = .data$LABEL[1],
              n = length(.data$nLABEL)) %>%
    as.data.frame()
  
  M$LCS <- 0
  M[LCS$nLABEL, "LCS"] <- LCS$n
  M_orig$LCS <- M$LCS
  
  histData <- M[c("LABEL","TI","DE","ID","DI","PY","LCS","TC")]
  names(histData) <- c("Paper","Title","Author_Keywords","KeywordsPlus", "DOI","Year","LCS","GCS")
  histData <- histData %>% 
    dplyr::filter(.data$GCS>=min.citations)
  
  if (isTRUE(network)){
    # Citing data frame
    CITING <- L %>% 
      group_by(.data$CITING) %>%
      summarize(
        LCR = paste(.data$LABEL, collapse = ";"),
        PY = .data$CIT_PY[1],
        Paper = .data$paper[1]
      ) %>%
      ungroup() %>%
      arrange(.data$PY) %>% 
      as.data.frame()
    
    M_orig$LCR <- NA
    M_orig$LCR[CITING$Paper] <- CITING$LCR
    M_orig$LABEL <- M$LABEL
    M <- M_orig %>% 
      dplyr::filter(.data$TC >= min.citations)
    
  
  ## assign an unique name to each document
    st<-i<-0
    while(st==0){
      ind <- which(duplicated(M$LABEL))
      if (length(ind)>0){
        i <- i+1
        M$LABEL[ind]=paste0(M$LABEL[ind],"-",letters[i],sep="")}else{st <- 1}}
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
    #row.names(WLCR) <- LABEL
  } else{
    WLCR = NULL
  }
  
  if (isTRUE(verbose)) {
    cat("\nFound",
        length(M$LCS[M$LCS > 0]),
        "documents with no empty Local Citations (LCS)\n")
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

# scopus <- function(M, min.citations, sep, network, verbose){
#   
#   if (isTRUE(verbose)) {
#     cat("\nSCOPUS DB: Searching local citations (LCS) by document titles (TI) and DOIs...\n")
#   }
#   
#   if (!("SR_FULL" %in% names(M))) {
#     M = metaTagExtraction(M, Field = "SR")
#   }
#   
#   M$nCITING <- 1:nrow(M)
#   papers <- M$nCITING[M$TC >= min.citations]
#   
#   TIpost <-
#     paste(gsub("[[:punct:]]", "", M$TI[papers]), " ", M$PY[papers], " ", sep = "")
#   
#   CR <- gsub("[[:punct:]]", "", M$CR)
#   n <- nchar(CR)
#   n[is.na(n)] <- 2
#   n <- n + 1
#   nCum <- c(1, cumsum(n[-length(n)]))
#   CR <- paste(CR, collapse = " ")
#   
#   #L <- str_locate_all(CR, TIpost)
#   L <- stringi::stri_locate_all_regex(CR,TIpost, omit_no_match = TRUE)
#   
#   LCS <- lengths(L) / 2
#   
#   M$LCS <- 0
#   M$LCS[papers] <- LCS
#   
# 
#   ### HistData
#   histData <- M %>%
#     select(.data$SR_FULL, .data$TI,.data$DE,.data$ID,.data$DI, .data$PY, .data$LCS, .data$TC) %>%
#     rename(
#       Paper = .data$SR_FULL,
#       Title = .data$TI,
#       Author_Keywords = .data$DE,
#       KeywordsPlus = .data$ID,
#       DOI = .data$DI,
#       Year = .data$PY,
#       GCS = .data$TC
#     ) %>%
#     arrange(.data$Year) %>%
#     dplyr::filter(.data$GCS>=min.citations) %>% 
#     as.data.frame()
#   
#   
#   if (isTRUE(network)) {
#     ## Network matrix
#     df <- lapply(seq_along(L), function(i) {
#       l <-
#         data.frame(
#           ref = L[[i]],
#           paper = rep(papers[i], length(L[[i]][, 1]))
#         )
#     })
#     df <- (do.call(rbind, df))
#     
#     A <- outer(df$ref.start, nCum, "-")
#     A[A < 0] <- NA
#     df$CITINGn <- unlist(apply(A, 1, which.min))
#     df$CITING <- M$SR[df$CITINGn]
#     df$CITED <- M$SR[df$paper]
#     df <- df %>% 
#       dplyr::filter(.data$CITING %in% histData$Paper)
#     
#     NetMatrix <-
#       (as_adjacency_matrix(graph_from_data_frame(df[, c(6, 5)], directed = T)))
#   } else{
#     NetMatrix = NULL
#   }
#   
#   if (isTRUE(verbose)) {
#     cat("\nFound",
#         length(M$LCS[M$LCS > 0]),
#         "documents with no empty Local Citations (LCS)\n")
#   }
#   
#   results <-
#     list(
#       NetMatrix = NetMatrix,
#       histData = histData,
#       M = M,
#       LCS = M$LCS
#     )
# }

# New algorithm for Scopus
# Local citation matching is based on First Author, Year and PP
scopus <- function(M, min.citations, sep, network, verbose){
  
  if (!("SR_FULL" %in% names(M))) {
    M = metaTagExtraction(M, Field = "SR")
  }
  
  CR <- strsplit(M$CR,";")
  
  CR <- data.frame(SR_citing=rep(M$SR,lengths(CR)), ref=trimws(unlist(CR)))
  
  CR$PY <- as.numeric(gsub(".*\\((\\d{4})\\).*", "\\1", CR$ref))
  
  CR$AU <- trimws(gsub("\\.", "", gsub("\\. ", "", gsub("^(.*?),.*$", "\\1", CR$ref))))
  
  CR$PP <- gsub(".*PP\\. ([0-9-]+).*", "\\1", CR$ref)
  
  CR <- CR %>% 
    dplyr::filter(!is.na(.data$PY), (substr(CR$PP,1,1) %in% 0:9))
  
  M_merge <- M %>% 
    select(.data$AU,.data$PY,.data$Page.start, .data$Page.end, .data$PP, .data$SR) %>% 
    mutate(AU = gsub(";.*$", "", .data$AU),
           Page.start = as.numeric(.data$Page.start),
           Page.end = as.numeric(.data$Page.end),
           PP = ifelse(!is.na(.data$Page.start), paste0(.data$Page.start,"-",.data$Page.end), NA),
           Included = TRUE
    ) %>% 
    rename(SR_cited = .data$SR)
  
  CR <- CR %>% 
    left_join(M_merge, join_by("PY", "AU"), relationship = "many-to-many") %>% 
    dplyr::filter(!is.na(.data$Included)) %>% 
    group_by(.data$PY,.data$AU) %>% 
    mutate(toRemove = ifelse(!is.na(.data$PP.y) & .data$PP.x!=.data$PP.y, TRUE,FALSE)) %>% # to remove FALSE POSITIVE
    ungroup() %>% 
    dplyr::filter(.data$toRemove != TRUE)
  
  LCS <- CR %>% 
    group_by(.data$SR_cited) %>% 
    count(name="LCS")
  
  
  M <- M %>% 
    left_join(LCS, by=c("SR" = "SR_cited")) %>% 
    mutate(LCS = ifelse(is.na(.data$LCS),0,.data$LCS))
  
  histData <- M %>%
    select(.data$SR_FULL, .data$TI,.data$DE,.data$ID,.data$DI, .data$PY, .data$LCS, .data$TC) %>%
    rename(
      Paper = .data$SR_FULL,
      Title = .data$TI,
      Author_Keywords = .data$DE,
      KeywordsPlus = .data$ID,
      DOI = .data$DI,
      Year = .data$PY,
      GCS = .data$TC
    ) %>%
    arrange(.data$Year) %>%
    as.data.frame()
  
  names(histData) <- c("Paper","Title","Author_Keywords","KeywordsPlus", "DOI","Year","LCS","GCS")
  
  if (isTRUE(network)){
    
    CRadd <- data.frame(SR_citing=unique(M$SR), SR_cited=unique(M$SR), value=1)
    
    WLCR <- CR %>%
      select(.data$SR_citing, .data$SR_cited) %>% 
      mutate(value = 1) %>% 
      bind_rows(CRadd) %>% 
      distinct() %>% 
      pivot_wider(names_from = "SR_cited", values_from = "value", values_fill = 0) %>% 
      dplyr::filter(.data$SR_citing %in% CRadd$SR_cited)
    
    SRrow <- WLCR$SR_citing
    SRcol <- colnames(WLCR)[-1]
    
    WLCR <- as.matrix(WLCR %>% select(-1))
    row.names(WLCR) <- SRrow
    colnames(WLCR) <- SRcol
  } else {
    WLCR = NULL
  }
  
  if (isTRUE(verbose)) {
    cat("\nFound",
        length(M$LCS[M$LCS > 0]),
        "documents with no empty Local Citations (LCS)\n")
  }
  
  results <-
    list(
      NetMatrix = WLCR,
      histData = histData,
      M = M,
      LCS = M$LCS
    )
}

openalex <- function(M, min.citations=min.citations, sep=sep, network=network, verbose=verbose){
  
  M$CR[is.na(M$CR)] <- "none"
  ids <- M$id_oa
  CR <- strsplit(M$CR, ";")
  CR <- data.frame(id_oa = rep(M$id_oa,lengths(CR)), ref = unlist(CR)) %>% 
    dplyr::filter(.data$ref %in% ids)
  
  LCS <- CR %>% 
    count(id_oa = .data$ref) %>% 
    rename(LCS = .data$n)
  
  histData <- M %>% 
    left_join(LCS, by = c("id_oa")) %>% 
    mutate(LCS = ifelse(is.na(.data$LCS),0,.data$LCS),
           DE = .data$ID) %>% 
    rename(LABEL = .data$SR,
           GCS = .data$TC) %>% 
    select(c("LABEL","TI","DE","ID","DI","PY","LCS","GCS")) %>% 
    dplyr::filter(.data$GCS>=min.citations)
  
  names(histData) <- c("Paper","Title","Author_Keywords","KeywordsPlus", "DOI","Year","LCS","GCS")
  
  if (isTRUE(network)){
    CRadd <- data.frame(id_oa=unique(M$id_oa), ref=unique(M$id_oa), value=1)
    WLCR <- CR %>%
      mutate(value = 1) %>% 
      bind_rows(CRadd) %>%
      distinct() %>% 
      pivot_wider(names_from = "ref", values_from = "value", values_fill = 0) %>% 
      dplyr::filter(.data$id_oa %in% CRadd$ref)
    
    SRrow <- WLCR %>% select(.data$id_oa) %>% 
      left_join(M %>% 
                  select(.data$id_oa, .data$SR), 
                by="id_oa")
    
    SR_col <- data.frame(id_oa = colnames(WLCR)[-1]) %>% 
      left_join(M %>% 
                  select(.data$id_oa, .data$SR), 
                by="id_oa")
    
    WLCR <- as.matrix(WLCR %>% select(-1))
    row.names(WLCR) <- SRrow$SR
    colnames(WLCR) <- SR_col$SR
    WLCR <- WLCR[colnames(WLCR),colnames(WLCR)]
  } else {
    WLCR = NULL
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

lens <- function(M, min.citations=min.citations, sep=sep, network=network, verbose=verbose){
  
  M$CR[is.na(M$CR)] <- "none"
  ids <- M$UT
  CR <- lapply(strsplit(M$CR, ";"), trimws)
  CR <- data.frame(UT = rep(M$UT,lengths(CR)), ref = unlist(CR)) %>% 
    dplyr::filter(.data$ref %in% ids)
  
  LCS <- CR %>% 
    count(UT = .data$ref) %>% 
    rename(LCS = .data$n)
  
  histData <- M %>% 
    left_join(LCS, by = c("UT")) %>% 
    mutate(LCS = ifelse(is.na(.data$LCS),0,.data$LCS),
           DE = .data$ID) %>% 
    rename(LABEL = .data$SR,
           GCS = .data$TC) %>% 
    select(c("LABEL","TI","DE","ID","DI","PY","LCS","GCS")) %>% 
    dplyr::filter(.data$GCS>=min.citations)
  
  names(histData) <- c("Paper","Title","Author_Keywords","KeywordsPlus", "DOI","Year","LCS","GCS")
  
  if (isTRUE(network)){
    
    CRadd <- data.frame(UT=unique(M$UT), ref=unique(M$UT), value=1)
    WLCR <- CR %>%
      mutate(value = 1) %>% 
      bind_rows(CRadd) %>%
      distinct() %>% 
      pivot_wider(names_from = "ref", values_from = "value", values_fill = 0) %>% 
      dplyr::filter(.data$UT %in% CRadd$ref)
    
    SRrow <- WLCR %>% select(.data$UT) %>% 
      left_join(M %>% 
                  select(.data$UT, .data$SR), 
                by="UT")
    
    SR_col <- data.frame(UT = colnames(WLCR)[-1]) %>% 
      left_join(M %>% 
                  select(.data$UT, .data$SR), 
                by="UT")
    
    WLCR <- as.matrix(WLCR %>% select(-1))
    row.names(WLCR) <- SRrow$SR
    colnames(WLCR) <- SR_col$SR
    WLCR <- WLCR[colnames(WLCR),colnames(WLCR)]

  } else {
    WLCR = NULL
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