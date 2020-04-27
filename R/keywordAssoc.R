#' ID and DE keyword associations
#'
#' It associates authors' keywords to keywords plus.
#'
#' @param M is a bibliographic data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to manuscripts and variables to Field Tag in the original SCOPUS and Clarivate Analytics WoS file.
#' @param sep is the field separator character. This character separates keywords in each string of ID and DE columns of the bibliographic data frame. The default is \code{sep = ";"}.
#' @param excludeKW is character vector. It contains authors' keywords to exclude from the analysis.
#' @param n is a integer. It indicates the number of authors' keywords to associate to each keyword plus. The default is \code{n = 10}.
#' @return an object of \code{class} "list".
#'
#' 
#'
#' @examples
#' 
#' data(scientometrics)
#'
#' KWlist <- keywordAssoc(scientometrics, sep = ";",n = 10, excludeKW = NA)
#'
#' # list of first 10 Keywords plus
#' names(KWlist)
#' 
#' # list of first 10 authors' keywords associated to the first Keyword plus
#' KWlist[[1]][1:10]
#' 
#' @seealso \code{\link{convert2df}} to import and convert a WoS or SCOPUS Export file in a bibliographic data frame.
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis.
#' @seealso \code{\link{summary}} to obtain a summary of the results.
#' @seealso \code{\link{plot}} to draw some useful plots of the results.
#'
#' @export
keywordAssoc <- function(M, sep = ";",n = 10, excludeKW = NA){

excludeKW=toupper(excludeKW)

WDE <- cocMatrix(M, Field = "DE", type = "sparse", sep = sep)
WID <- cocMatrix(M, Field = "ID", type = "sparse", sep = sep)

NetMatrix=Matrix::crossprod(WID,WDE)
if (!is.na(excludeKW)){
  NetMatrix=NetMatrix[!(row.names(NetMatrix) %in% excludeKW), !(colnames(NetMatrix) %in% excludeKW)]
}
NetMatrix=NetMatrix[!is.na(row.names(NetMatrix)),!is.na(colnames(NetMatrix))]
NetMatrix=NetMatrix[nchar(row.names(NetMatrix))>0,nchar(colnames(NetMatrix))>0]
rS=Matrix::rowSums(NetMatrix)

NetDegree <- sort(rS,decreasing=TRUE)[n]
NET=NetMatrix[rS>=NetDegree,Matrix::colSums(NetMatrix)>1]
KW=apply(NET,1,function(x){
  i=sort(x,decreasing=TRUE)[n]
  x=sort(x[x>=i],decreasing=TRUE)
  return(x)
})

return(KW)
}

