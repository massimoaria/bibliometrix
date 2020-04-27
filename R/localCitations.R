#' Author local citations 
#'
#' It calculates local citations (LCS) of authors and documents of a bibliographic collection.
#' 
#' Local citations measure how many times an author (or a document) included in this collection have been cited by the documents also included in the collection.
#'
#' @param M is a bibliographic data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to manuscripts and variables to Field Tag in the original SCOPUS and Clarivate Analytics WoS file.
#' @param sep is the field separator character. This character separates citations in each string of CR column of the bibliographic data frame. The default is \code{sep = ";"}.
#' @param fast.search is logical. if true, the function calculates local citations only for 25 percent top cited documents.
#' @return an object of \code{class} "list" containing author local citations and document local citations.
#'
#' 
#' @examples
#'  
#' data(scientometrics)
#' 
#' CR <- localCitations(scientometrics, sep = ";")
#'
#' CR$Authors[1:10,]
#' CR$Papers[1:10,]
#'
#' @seealso \code{\link{citations}} function for citation frequency distribution.
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis.
#' @seealso \code{\link{summary}} to obtain a summary of the results.
#' @seealso \code{\link{plot}} to draw some useful plots of the results.
#' 
#' @export

localCitations <- function(M, fast.search=FALSE, sep = ";"){
  
  M$TC[is.na(M$TC)] <- 0
  if (isTRUE(fast.search)){
    loccit=quantile(as.numeric(M$TC),0.75, na.rm = TRUE)
  } else {loccit=1}
  
  H=histNetwork(M,min.citations = loccit, sep=sep, network=FALSE)
  LCS=H$histData
  M=H$M
  rm(H)
  AU=strsplit(M$AU,split=";")
  n=lengths(AU)
  
  df=data.frame(AU=unlist(AU),LCS=rep(LCS$LCS,n))
  AU=aggregate(df$LCS,by=list(df$AU),FUN="sum")
  names(AU)=c("Author", "LocalCitations" )
  AU=AU[order(-AU$LocalCitations),]
  
  if ("SR" %in% names(M)){
    LCS=data.frame(Paper=M$SR,DOI=M$DI,Year=M$PY,LCS=M$LCS,GCS=M$TC)
    LCS=LCS[order(-LCS$LCS),]
  }
  CR=list(Authors=AU,Papers=LCS, M=M)
  return(CR)
  
}