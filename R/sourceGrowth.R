#' Number of documents published annually per Top Sources
#'
#' It calculates yearly published documents of the top sources.
#'
#' @param M is a data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to articles and variables to Field Tag in the original ISI or SCOPUS file.
#' @param top is a numeric. It indicates the number of top sources to analyze. The default value is 5.
#' @param cdf is a logical. If TRUE, the function calculates the cumulative occurrences distribution. 
#' @return an object of class \code{data.frame}
#' @examples
#'
#' data(scientometrics)
#' topSO=sourceGrowth(scientometrics, top=1, cdf=TRUE)
#' topSO
#'
#' # Plotting results
#' \dontrun{
#' install.packages("reshape2")
#' library(reshape2)
#' library(ggplot2)
#' DF=melt(topSO, id='Year')
#' ggplot(DF,aes(Year,value, group=variable, color=variable))+geom_line()
#' }
#'
#' @export
#' 
sourceGrowth<-function(M, top=5, cdf=TRUE){
  PY=min(M$PY,na.rm = T):max(M$PY,na.rm = T)
  WSO=cocMatrix(M,Field="SO")
  if(is.null(dim(WSO))){
    WSO=cbind(WSO)
    colnames(WSO)=M$SO[1]
  }
  if (top>dim(WSO)[2]){top=dim(WSO)[2]}
  
  M$PY=as.character(M$PY)
  WPY=cocMatrix(M,Field="PY")
  i=setdiff(PY,colnames(WPY))
  if (length(i)>0){
    WPY=cbind(WPY,matrix(0,dim(WPY)[1],length(i)))
    colnames(WPY)[(dim(WPY)[2]-length(i)+1):dim(WPY)[2]]=as.character(i)
  }
  PYSO=Matrix::crossprod(WPY,WSO)
  ind=Matrix::colSums(PYSO)
  deg=sort(ind, decreasing=T)[top]
  sonames=colnames(PYSO)[ind>=deg]
  PYSO=as.data.frame(as.matrix(PYSO[,ind>=deg]))
  
  PYSO=cbind(as.numeric(colnames(WPY)),PYSO)
  
  PYSO=PYSO[order(PYSO[,1]),]
  if (isTRUE(cdf)){
    PYSO[,-1]=apply(as.data.frame(PYSO[,-1]),2,cumsum)
  }
  names(PYSO)=c("Year",sonames)
  return(PYSO)
}