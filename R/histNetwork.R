#' Historical co-citation network
#'
#' \code{histNetwork} creates a historical citation network from a bibliographic
#' data frame.
#'
#' @param M is a bibliographic data frame obtained by the converting function
#'   \code{\link{convert2df}}. It is a data matrix with cases corresponding to
#'   manuscripts and variables to Field Tag in the original SCOPUS and Clarivate
#'   Analitics Web of Science file.
#' @param min.citations is a positive integer. It sets the minimum number of citations 
#'   for the documents included in the analysis. It can be greater than or equal to 1. The default is \code{min.citations = 1}.
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
#' data(management)
#'
#' histResults <- histNetwork(management, min.citations = 0, sep = ";")
#'
#'
#' @seealso \code{\link{convert2df}} to import and convert an ISI or SCOPUS
#'   Export file in a bibliographic data frame.
#' @seealso \code{\link{summary}} to obtain a summary of the results.
#' @seealso \code{\link{plot}} to draw some useful plots of the results.
#' @seealso \code{\link{biblioNetwork}} to compute a bibliographic network.
#'
#' @export

histNetwork<-function(M, min.citations = 1, sep = ";", network = TRUE, verbose = TRUE){
  
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
                    paper = i,
                    stringsAsFactors = FALSE)
  })
  CR <- (do.call(rbind, CR))
  CR$DI <-
    trimws(unlist(lapply(
      strsplit(CR$ref, 'DOI', fixed = TRUE), '[', 2
    )))
  CR$DI[is.na(CR$DI)] <- ""
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
  
  histData <- M[c("LABEL","TI","DI","PY","LCS","TC")]
  names(histData) <- c("Paper","Title", "DOI","Year","LCS","GCS")
  
  if (isTRUE(network)){
    # Citing data frame
    CITING <- L %>% group_by(.data$CITING) %>%
      summarize(
        LCR = paste(.data$LABEL, collapse = ";"),
        PY = .data$CIT_PY[1],
        Paper = .data$paper[1]
      ) %>%
      ungroup() %>%
      arrange(.data$PY) %>% as.data.frame()
    
    M_orig$LCR <- NA
    M_orig$LCR[CITING$Paper] <- CITING$LCR
    M_orig$LABEL <- M$LABEL
    M <- M_orig
  
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
    WLCR <- as.data.frame(as.matrix(WLCR), stringsAsFactors = FALSE)
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

scopus <- function(M, min.citations, sep, network, verbose){
  
  if (isTRUE(verbose)) {
    cat("\nSCOPUS DB: Searching local citations (LCS) by document titles (TI) and DOIs...\n")
  }
  
  M$nCITING <- 1:nrow(M)
  papers <- M$nCITING[M$TC >= min.citations]
  
  TIpost <-
    paste(gsub("[[:punct:]]", "", M$TI[papers]), " ", M$PY[papers], " ", sep = "")
  
  CR <- gsub("[[:punct:]]", "", M$CR)
  n <- nchar(CR)
  n[is.na(n)] <- 2
  n <- n + 1
  nCum <- c(1, cumsum(n[-length(n)]))
  CR <- paste(CR, collapse = " ")
  
  L <- str_locate_all(CR, TIpost)
  
  
  LCS <- lengths(L) / 2
  
  M$LCS <- 0
  M$LCS[papers] <- LCS
  
  ### HistData
  histData <- M %>%
    select(.data$SR_FULL, .data$TI,.data$DI, .data$PY, .data$LCS, .data$TC) %>%
    rename(
      Paper = .data$SR_FULL,
      Title = .data$TI,
      DOI = .data$DI,
      Year = .data$PY,
      GCS = .data$TC
    ) %>%
    arrange(.data$Year) %>%
    as.data.frame()
  
  if (isTRUE(network)) {
    ## Network matrix
    df <- lapply(seq_along(L), function(i) {
      l <-
        data.frame(
          ref = L[[i]],
          paper = rep(papers[i], length(L[[i]][, 1])),
          stringsAsFactors = FALSE
        )
    })
    df <- (do.call(rbind, df))
    
    A <- outer(df$ref.start, nCum, "-")
    A[A < 0] <- NA
    df$CITINGn <- unlist(apply(A, 1, which.min))
    df$CITING <- M$SR[df$CITINGn]
    df$CITED <- M$SR[df$paper]
    
    NetMatrix <-
      (as_adjacency_matrix(graph_from_data_frame(df[, c(6, 5)], directed = T)))
  } else{
    NetMatrix = NULL
  }
  
  if (isTRUE(verbose)) {
    cat("\nFound",
        length(M$LCS[M$LCS > 0]),
        "documents with no empty Local Citations (LCS)\n")
  }
  
  results <-
    list(
      NetMatrix = NetMatrix,
      histData = histData,
      M = M,
      LCS = M$LCS
    )
}
