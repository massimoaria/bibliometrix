#' Yearly occurrences of top keywords/terms 
#'
#' It calculates yearly occurrences of top keywords/terms.
#'
#' @param M is a data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to articles and variables to Field Tag in the original WoS or SCOPUS file.
#' @param Tag is a character object. It indicates one of the keyword field tags of the
#'   standard ISI WoS Field Tag codify (ID or DE) or a field tag created by \code{\link{termExtraction}} function (TI_TM, AB_TM, etc.).
#' @param sep is the field separator character. This character separates strings in each keyword column of the data frame. The default is \code{sep = ";"}.
#' @param top is a numeric. It indicates the number of top keywords to analyze. The default value is 10.
#' @param cdf is a logical. If TRUE, the function calculates the cumulative occurrences distribution. 
#' @return an object of class \code{data.frame}
#' @examples
#'
#' data(scientometrics)
#' topKW=KeywordGrowth(scientometrics, Tag = "ID", sep = ";", top=5, cdf=TRUE)
#' topKW
#' 
#' # Plotting results
#' \dontrun{
#' install.packages("reshape2")
#' library(reshape2)
#' library(ggplot2)
#' DF=melt(topKW, id='Year')
#' ggplot(DF,aes(Year,value, group=variable, color=variable))+geom_line
#' }
#'
#' @export
KeywordGrowth <- function(M, Tag = "ID", sep = ";", top=10, cdf=TRUE){
  i<-which(names(M)==Tag)
  PY=as.numeric(M$PY)
  Tab<-(strsplit(as.character(M[,i]),sep))
  Y=rep(PY,lengths(Tab))
  A=data.frame(Tab=unlist(Tab),Y=Y)
  A$Tab=trim.leading(A$Tab)
  A=A[A$Tab!="",]
  A=A[!is.na(A$Y),]
  Ymin=min(A$Y)
  Ymax=max(A$Y)
  Year=Ymin:Ymax
  Tab<-names(sort(table(A$Tab),decreasing=TRUE))[1:top]

  words=matrix(0,length(Year),top+1)
  words=data.frame(words)
  names(words)=c("Year",Tab)
  words[,1]=Year
  for (j in 1:length(Tab)){
    word=(table(A[A$Tab %in% Tab[j],2]))
    words[,j+1]=trim.years(word,Year,cdf)
    
  }
  return(words)
}

trim.years<-function(w,Year,cdf){

  Y=as.numeric(names(w))
  W=matrix(0,length(Year),1)
  
  for (i in 1:length(Year)){
    if (Y[1]==Year[i] & length(Y)>0){W[i,1]=w[1]
    Y=Y[-1]
    w=w[-1]}
  }
  if (isTRUE(cdf)) W=cumsum(W)
  names(W)=Year
  W=data.frame(W)
  return(W)}
