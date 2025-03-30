utils::globalVariables(c("TC", "AUs", "PY", "h_index", "PY_start", "Element"))
#' h-index calculation
#'
#' It calculates the authors' h-index and its variants.
#'
#' @param M is a bibliographic data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to manuscripts and variables to Field Tag in the original SCOPUS and Clarivate Analytics WoS file.
#' @param field is character. It can be equal to c("author", "source"). field indicates if H-index have to be calculated for a list of authors or for a list of sources. Default
#' value is \code{field = "author"}.
#' @param elements is a character vector. It contains the authors' names list or the source list for which you want to calculate the H-index. When the field is
#' "author", the argument has the form C("SURNAME1 N","SURNAME2 N",...), in other words, for each author: surname and initials separated by one blank space. If elements=NULL, the function calculates impact indices for all elements contained in the data frame.
#' i.e for the authors SEMPRONIO TIZIO CAIO and ARIA MASSIMO \code{elements} argument is \code{elements = c("SEMPRONIO TC", "ARIA M")}.
#' @param sep is the field separator character. This character separates authors in each string of AU column of the bibliographic data frame. The default is \code{sep = ";"}.
#' @param years is a integer. It indicates the number of years to consider for Hindex calculation. Default is Inf.
#' @return an object of \code{class} "list". It contains two elements: H is a data frame with h-index, g-index and m-index for each author; CitationList is a list with the bibliographic collection for each author.
#'
#'
#' @examples
#'
#' ### EXAMPLE 1: ###
#'
#' data(scientometrics, package = "bibliometrixData")
#'
#' authors <- c("SMALL H", "CHEN DZ")
#'
#' Hindex(scientometrics, field = "author", elements = authors, sep = ";")$H
#'
#' Hindex(scientometrics, field = "source", elements = "SCIENTOMETRICS", sep = ";")$H
#'
#' ### EXAMPLE 2: Garfield h-index###
#'
#' data(garfield, package = "bibliometrixData")
#'
#' indices <- Hindex(garfield, field = "author", elements = "GARFIELD E", years = Inf, sep = ";")
#'
#' # h-index, g-index and m-index of Eugene Garfield
#' indices$H
#'
#' # Papers and total citations
#' head(indices$CitationList[[1]])
#'
#' @seealso \code{\link{convert2df}} to import and convert an WoS or SCOPUS Export file in a bibliographic data frame.
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis.
#' @seealso \code{\link{summary}} to obtain a summary of the results.
#' @seealso \code{\link{plot}} to draw some useful plots of the results.
#'
#' @export

Hindex <- function(M, field = "author", elements = NULL, sep = ";", years = Inf) {
  # elements=paste("\\\b",elements,"\\\b",sep="")
  M$TC <- as.numeric(M$TC)
  M$PY <- as.numeric(M$PY)
  M <- M %>% dplyr::filter(!is.na(TC) | !is.na(TC))
  # M <- M[M$TC>0,]

  Today <- as.numeric(substr(Sys.time(), 1, 4))
  past <- Today - years
  if (min(M$PY, na.rm = TRUE) < past) {
    M <- M[M$PY >= past, ]
  }

  switch(field,
    author = {
      AU <- M$AU
      AU <- trimES(gsub(",", " ", AU))
      listAU <- strsplit(AU, split = sep)
      l <- lengths(listAU)
      index <- rep(row.names(M), l)
      df <- M[index, ]
      df$AUs <- trimws(unlist(listAU))
    },
    source = {
      df <- M
      df$AUs <- M$SO
    }
  )
  rm(M)

  h_calc <- function(x) {
    h <- tail(which(1:length(x) <= sort(x, decreasing = T)), 1) # [1]-1
    return(h)
  }

  g_calc <- function(x) {
    g <- tail(which(1:length(x) <= cummean(sort(x, decreasing = T))), 1)
    return(g)
  }

  H <- df %>%
    group_by(AUs) %>%
    reframe(
      h_index = h_calc(TC),
      g_index = g_calc(TC),
      PY_start = min(PY),
      TC = sum(TC),
      NP = length(AUs)
    ) %>%
    mutate(
      m_index = h_index / (Today - PY_start + 1)
    ) %>%
    rename(Element = AUs) %>%
    as.data.frame()

  if (!is.null(elements)) {
    H <- H %>%
      dplyr::filter(Element %in% elements) %>%
      dplyr::select("Element", "h_index", "g_index", "m_index", "TC", "NP", "PY_start")

    df <- df %>% dplyr::filter(AUs %in% elements)
  } else {
    H <- H %>%
      dplyr::select("Element", "h_index", "g_index", "m_index", "TC", "NP", "PY_start")
  }


  # H <- H[c("Element","h_index","g_index","m_index","TC","NP","PY_start")]

  vars <- intersect(c("AU", "SO", "PY", "TC", "DI"), names(df))


  CitationList <- split(df[vars], f = df$AUs)

  results <- list(H = H, CitationList = CitationList)

  return(results)
}
