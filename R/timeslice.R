#' Bibliographic data frame time slice 
#'
#' Divide a bibliographic data frame into time slice
#' 
#' 
#'
#' @param M is a bibliographic data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to manuscripts and variables to Field Tag in the original SCOPUS and Clarivate Analytics WoS file.
#' @param breaks is a numeric vector of two or more unique cut points. 
#' @param k is an integer value giving the number of intervals into which the data frame is to be cut. \code{k} is used only in case \code{breaks} argument is not provided. The default is \code{k = 5}.
#' @return the value returned from \code{split} is a list containing the data frames for each sub-period.
#'
#' 
#'
#' @examples
#'  
#' data(scientometrics)
#' 
#' list_df <- timeslice(scientometrics, breaks = c(1995, 2005))
#'
#' names(list_df)
#'
#' @seealso \code{\link{convert2df}} to import and convert an ISI or SCOPUS Export file in a bibliographic data frame.
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis.
#' @seealso \code{\link{summary}} to obtain a summary of the results.
#' @seealso \code{\link{plot}} to draw some useful plots of the results.
#' 
#' @export
timeslice <- function(M,breaks=NA,k=5){

  M$PY=as.numeric(M$PY)
  period=list()
  if (is.na(breaks[1]) & is.numeric(k)){
    breaks=(floor(seq(min(M$PY,na.rm=TRUE)-1,max(M$PY,na.rm=TRUE),length.out=k+1)))}
  
  else {breaks=c(min(M$PY,na.rm=TRUE)-1,breaks,max(M$PY,na.rm=TRUE))}
    df=cut(M$PY,breaks)
    N=levels(df)
    ind=as.numeric(df)
    df=split(M,ind)
    names(df)=N
    return(df)
}