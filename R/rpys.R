utils::globalVariables(c(
  "Year", "diffMedian", "Citations", "citedYears",
  "Reference", "citingYears", "benchmark", "status", "citations",
  "Freq", "diffMedian5", "CP", "CR", "Class", "HP", "LC", "RPY", 
  "SB", "Z", "dev_median_t2_t2", "dev_median_t4_t",
  "diffMedian2", "freqO", "freqYC", "is_peak", "lead", "observed",
  "seq1", "seq2", "seq3", "z_class", "everything"
))
#' Reference Publication Year Spectroscopy
#'
#' \code{rpys} computes a Reference Publication Year Spectroscopy for detecting
#' the Historical Roots of Research Fields.
#' The method was introduced by Marx et al., 2014.\cr\cr
#'
#' References:\cr\cr
#' Marx, W., Bornmann, L., Barth, A., & Leydesdorff, L. (2014).
#' Detecting the historical roots of research fields by reference publication
#' year spectroscopy (RPYS). Journal of the Association for Information Science and Technology,
#' 65(4), 751-764.\cr\cr
#' Thor A., Bornmann L., Mark W. & Mutz R.(2018). 
#' Identifying single influential publications in a research field: new analysis opportunities of the CRExplorer. 
#' Scientometrics, 116:591–608 https://doi.org/10.1007/s11192-018-2733-7\cr\cr
#'
#' @param M is a data frame obtained by the converting function
#'   \code{\link{convert2df}}. It is a data matrix with cases corresponding to
#'   articles and variables to Field Tag in the original ISI or SCOPUS file.
#' @param sep is the cited-references separator character. This character separates cited-references in the CR
#' column of the data frame. The default is \code{sep = ";"}.
#' @param timespan is a numeric vector c(min year,max year). The default value is NULL (the entire timespan is considered).
#' @param median.window is a character string that can be "centered" or "backward". It indicates the type of median to be used. 
#' "centered" is the default value and it uses the centered 5-year median (t-2 to t+2) as proposed by Marx et al. (2014). "backward" uses the backward 5-year median (t-4 to t) as proposed by Aria and Cuccurullo (2017).
#' @param graph is a logical. If TRUE the function plot the spectroscopy otherwise the plot is created but not drawn down.
#' @return a list containing the spectroscopy (class ggplot2) and three dataframes with the number of citations
#' per year, the list of the cited references for each year, and the reference list with citations recorded year by year, respectively.
#'
#'
#' @examples
#'
#' \dontrun{
#' data(management, package = "bibliometrixData")
#' res <- rpys(management, sep = ";", graph = TRUE)
#' }
#'
#' @seealso \code{\link{convert2df}} to import and convert an ISI or SCOPUS
#'   Export file in a data frame.
#' @seealso \code{\link{biblioAnalysis}} to perform a bibliometric analysis.
#' @seealso \code{\link{biblioNetwork}} to compute a bibliographic network.
#' @export

rpys <- function(M, sep = ";", timespan = NULL, median.window = "centered", graph = T) {
  options(dplyr.summarise.inform = FALSE)
  
  if (is.null(timespan)) {
    timespan <- c(max(M$PY, na.rm = TRUE)-100, max(M$PY, na.rm = TRUE)-3)
  }

  M$CR <- gsub("DOI;", "DOI ", as.character(M$CR))

  Fi <- strsplit(M[, "CR"], sep)
  Fi <- lapply(Fi, trim.leading)
  Fi <- lapply(Fi, function(l) l <- l[nchar(l) > 10])
  citingYears <- rep(M$PY, lengths(Fi))
  Fi <- (unlist(Fi))

  df <- data.frame(Reference = Fi, citingYears = citingYears) %>%
    mutate(Reference = refCleaning(Reference, db = M$DB[1]))
  df$citedYears <- as.numeric(yearExtract(df$Reference, db = M$DB[1]))

  df <- df %>%
    dplyr::filter(!is.na(Reference) & citedYears > 1700 & citedYears <= as.numeric(substr(Sys.Date(), 1, 4))) %>%
    group_by(citedYears, citingYears, Reference) %>%
    summarize(citations = n()) %>%
    # group_by(citedYears, citingYears) %>%
    # mutate(
    #   benchmark = mean(citations, na.rm = T),
    #   status = sign(citations - benchmark)
    # ) %>%
    ungroup() %>%
    arrange(citedYears, Reference, citingYears)

  CR <- df %>%
    group_by(citedYears, Reference) %>%
    select(-citingYears) %>%
    summarize(Freq = sum(citations))

  RPYS <- CR %>%
    select(-Reference) %>%
    group_by(citedYears) %>%
    summarize(n = sum(Freq, na.rm = TRUE)) %>% 
    arrange(citedYears)
  
  # Create a complete sequence of years and fill missing frequencies with 0
  full_years <- tibble(citedYears = seq(min(RPYS$citedYears), max(RPYS$citedYears)))
  RPYS_full <- full_years %>%
    left_join(RPYS, by = "citedYears") %>%
    mutate(n = if_else(is.na(n), 0L, n))
  
  # Extract vectors for computation
  years <- RPYS_full$citedYears
  counts <- RPYS_full$n
  
  # Compute backward-looking 5-year median (t-4 to t)
  median_t4_t <- sapply(seq_along(years), function(i) {
    start <- pmax(1, i - 4)
    median(counts[start:i])
  })
  
  # Compute centered 5-year median (t-2 to t+2)
  median_t2_t2 <- sapply(seq_along(years), function(i) {
    start <- pmax(1, i - 2)
    end <- pmin(length(counts), i + 2)
    median(counts[start:end])
  })
  
  # Add the results to the data frame
  RPYS <- RPYS_full %>%
    mutate(
      dev_median_t4_t = n - median_t4_t,
      dev_median_t2_t2 = n - median_t2_t2
    ) %>%
    select(citedYears, n, dev_median_t4_t, dev_median_t2_t2)


  if (length(timespan) == 2) {
    RPYS <- RPYS %>%
      dplyr::filter(citedYears >= min(timespan) &
        citedYears <= max(timespan))
  }
  names(RPYS) <- c("Year", "Citations", "diffMedian5", "diffMedian2")

  # RPYS$diffMedian <- RPYS$diffMedian5
  
  # set a limit to negative numbers
  minLimit <- round(max(RPYS$Citations)*(-0.05))
 
  # choose the median windows
  if (median.window == "centered") {
    RPYS <- RPYS %>%
      mutate(diffMedian = diffMedian2)
  } else if (median.window == "backward") {
    RPYS <- RPYS %>%
      mutate(diffMedian = diffMedian5)
  } 
  
  RPYS <- RPYS %>%
    mutate(diffMedian = if_else(diffMedian > minLimit, diffMedian, minLimit))

  data("logo", envir = environment())
  logo <- grid::rasterGrob(logo, interpolate = TRUE)

  x <- c(min(RPYS$Year), min(RPYS$Year) + diff(range(RPYS$Year)) * 0.125) + 1
  y <- c(min(c(RPYS$Citations, RPYS$diffMedian)), min(c(RPYS$Citations, RPYS$diffMedian)) + diff(range(c(RPYS$Citations, RPYS$diffMedian))) * 0.125) * 1.05

  RPYS <- RPYS %>%
    left_join(
      CR %>%
        group_by(citedYears) %>%
        slice_max(order_by = Freq, n = 3, with_ties = FALSE) %>%
        summarize(References = paste(firstup(Reference), collapse = "\n")),
      by = c("Year" = "citedYears")
    )


  g <- ggplot(RPYS, aes(x = Year, y = Citations, text = paste("Year: ", Year, " - Total Citations: ", Citations, "\nTop 3 References:\n", References))) +
    geom_line(aes(group = "NA")) +
    geom_line(aes(x = Year, y = diffMedian, color = "firebrick", group = "NA")) +
    labs(
      x = "Year",
      y = "Cited References",
      title = "Reference Publication Year Spectroscopy",
      caption = "Number of Cited References (black line) - Deviation from the 5-Year Median (red line)"
    ) +
    scale_x_continuous(breaks = (RPYS$Year[seq(1, length(RPYS$Year), by = round(length(RPYS$Year) / 30))])) +
    theme(
      text = element_text(color = "#444444"), legend.position = "none",
      plot.caption = element_text(
        size = 9, hjust = 0.5,
        color = "black", face = "bold"
      ),
      panel.background = element_rect(fill = "#FFFFFF")
      # ,panel.grid.minor = element_line(color = '#FFFFFF')
      , panel.grid.major = element_line(color = "#EFEFEF"),
      plot.title = element_text(size = 24),
      axis.title = element_text(size = 14, color = "#555555"),
      axis.title.y = element_text(vjust = 1, angle = 90),
      axis.title.x = element_text(hjust = 0.95, angle = 0),
      axis.text.x = element_text(size = 8, angle = 90),
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      axis.line.y = element_line(color = "black", linewidth = 0.5)
    ) +
    annotation_custom(logo, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2])

  if (isTRUE(graph)) {
    plot(g)
  }
  CR$Reference <- reduceRefs(CR$Reference)
  CR <- CR %>%
    rename(Year = citedYears) %>%
    ungroup()
  
  ## identify types of sequences 
  Sequences <- sequenceTypes(M, timespan = timespan) 
  
  RPYS <- RPYS %>% select(-References)
  CR <- CR %>% mutate(Year = as.character(Year))
 
  result <- list(
    spectroscopy = g,
    rpysTable = RPYS,
    CR = CR,
    df = df,
    Sequences = Sequences %>% arrange(RPY, desc(Freq), CR)
  )
  return(result)
}

yearExtract <- function(string, db) {
  string <- paste(" ",string," ", sep = "")
  if (db == "ISI") {
    ind <- regexpr(" [[:digit:]]{4} ", string)
    ind[is.na(ind)] <- -1
    string[ind == -1] <- " 0000 "
    ind[ind == -1] <- 1
    attr(ind[ind == -1], "match.length") <- 6
    y <- trim(unlist(regmatches(string, ind)))
  } else {
    ind <- regexpr("\\([[:digit:]]{4}\\)", string)
    ind[is.na(ind)] <- -1
    string[ind == -1] <- "(0000)"
    ind[ind == -1] <- 1
    attr(ind[ind == -1], "match.length") <- 6
    y <- unlist(regmatches(string, ind))
    y <- substr(y, 2, 5)
  }
  return(y)
}

reduceRefs <- function(A) {
  ind <- unlist(regexec("*V[0-9]", A))
  A[ind > -1] <- substr(A[ind > -1], 1, (ind[ind > -1] - 1))
  ind <- unlist(regexec("*DOI ", A))
  A[ind > -1] <- substr(A[ind > -1], 1, (ind[ind > -1] - 1))
  return(A)
}

refCleaning <- function(l, db) {
  if (db == "ISI") {
    # ref<-unlist(lapply(Fi, function(l){
    l <- gsub("\\).*", ")", l)
    l <- gsub(",", " ", l)
    l <- gsub(";", " ", l)
    l <- gsub("\\.", " ", l)
    l <- trimws(trimES(l))
    l <- l[nchar(l) > 0]
    # return(l)
    # }))
  } else {
    # ref<-unlist(lapply(Fi, function(l){
    l <- gsub(",", " ", l)
    l <- gsub(";", " ", l)
    l <- gsub("\\.", " ", l)
    l <- trimws(trimES(l))
    l <- l[nchar(l) > 0]
    return(l)
    # }))
  }
  return(l)
}

sequenceTypes <- function(M, timespan = NULL){
  ## calculate Attribute matrices for CR and PY
  ACR <- cocMatrix(
    M,
    Field = "CR",
    sep = ";"
  )
  
  APY <- cocMatrix(
    M,
    Field = "PY",
    sep = ";"
  )
  APY <- APY[, order(colnames(APY))]
  
  B <- crossprod(ACR,APY)
  
  Y <- yearExtract(row.names(B), db=M$DB[1])
  
  
  rowS <- Matrix::rowSums(B)
  colS <- Matrix::colSums(B)
  indR <- which(rowS > 0)
  B <- B[indR,]
  Y <- Y[indR]
  rowS <- rowS[indR]
  dfObs <- as.matrix(B) %>% as.data.frame() %>% tibble::rownames_to_column("CR") %>% mutate(RPY = Y) %>% 
    select(CR,RPY, everything())
  
  # calculate the expected frequency of df by RPY
  dfExp <- df <- dfObs
  for (y in sort(unique(Y))) {
    ind <- which(dfObs$RPY == y)
    if (length(ind) > 1) {
      dfExp[ind,-c(1,2)] <- 
        outer(
          rowSums(dfObs[ind,-c(1,2)]),
          colSums(dfObs[ind,-c(1,2)]),
          FUN = function(r, c) r * c / sum(rowSums(dfObs[ind,-c(1,2)]))
        )
      df[ind,-c(1,2)] <- (dfObs[ind,-c(1,2)] - dfExp[ind,-c(1,2)]) / 
        sqrt(dfExp[ind,-c(1,2)])
    }
  }
  
  # create a data  frame with the CR, RPY and PY
  df <-df %>% 
    #mutate(RPY = Y) %>% 
    dplyr::filter(RPY !="", as.numeric(RPY)>=timespan[1]) %>% 
    pivot_longer(
      cols = -c(CR, RPY),
      names_to = "PY",
      values_to = "Z"
    ) %>% 
    mutate(Z = ifelse(PY<RPY,NA,Z))
  
  df <- df %>% left_join(
    dfExp %>%
      select(-RPY) %>% 
      pivot_longer(
        cols = -CR,
        names_to = "PY",
        values_to = "expected"
      ),
    by = c("CR", "PY")
  ) %>%
    left_join(
      dfObs %>%
        select(-RPY) %>% 
        pivot_longer(
          cols = -CR,
          names_to = "PY",
          values_to = "observed"
        ),
      by = c("CR", "PY")
    )
  
  df <- df %>% 
    mutate(z_class = ifelse(!is.na(Z), case_when(Z >= 1 ~ "+",
                                                 Z <= -1 ~ "-",
                                                 TRUE ~ "o"),NA)) %>% 
    arrange(RPY,PY) %>% dplyr::filter(!is.na(Z)) %>% 
    group_by(CR,RPY) %>% 
    reframe(sequence = paste(z_class, collapse=""),
            freqYC = sum(observed>0)/n(),
            freqO = sum(Z>-1)/n()) %>% 
    left_join(data.frame(CR = names(rowS), Freq = rowS), by = "CR") %>% 
    select(CR, RPY, Freq, freqYC, freqO, sequence)
  
  df$seq1 <- substr(df$sequence,1,4)
  df$seq2 <- ifelse(nchar(df$sequence)>4,substr(df$sequence,5,nchar(df$sequence)),"")
  df$seq3 <- ifelse(nchar(df$sequence)>6, substr(df$sequence,nchar(df$sequence)-2,nchar(df$sequence)),"")
  df$SB <- regexpr("\\-{2,}",substr(df$sequence,1,3))>-1 & regexpr("\\+",substr(df$sequence,4,nchar(df$sequence)))>-1
  df$CP <- df$freqO>=0.8 & df$freqYC>=0.8 & nchar(df$sequence)>2
  df$HP <- regexpr("\\+{2,}",substr(df$sequence,1,3))>-1
  df$LC <- !regexpr("\\+{3,}",df$seq1)>-1 & regexpr("\\+{2,}",df$seq2)>-1 & regexpr("\\+{0,}",df$seq3)>-1
  
  df <- df %>% select(-seq1,-seq2,-seq3) %>% 
    mutate(SB = ifelse(SB, "Sleeping Beauty", NA),
           CP = ifelse(CP, "Constant Performer", NA),
           HP = ifelse(HP, "Hot Paper", NA),
           LC = ifelse(LC, "Life Cycle", NA)) %>%
    unite("Class", c("SB","CP","HP","LC"), sep="+", na.rm=T) %>% 
    select(CR, RPY, Freq, sequence, Class)
}

firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
