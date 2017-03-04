#' h-index calculation
#'
#' It calculates the authors' h-index and its variants.
#'
#' @param M is a bibliographic data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to manuscripts and variables to Field Tag in the original SCOPUS and Thomson Reuters' ISI Web of Knowledge file.
#' @param authors is a character vector. It contains the the authors' names list for which you want to calculate the H-index. The aurgument has the form C("SURNAME1 N","SURNAME2 N",...), in other words, for each author: surname and initials separated by one blank space. 
#' i.e for the auhtors SEMPRONIO TIZIO CAIO and ARIA MASSIMO \code{authors} argument is \code{authors = c("SEMPRONIO TC", "ARIA M")}.
#' @param sep is the field separator character. This character separates auhtors in each string of AU column of the bibiographic data frame. The default is \code{sep = ";"}.
#' @param years is a integer. It indicates the number of years to consider for Hindex calculation. Default is 10.
#' @return an object of \code{class} "list". It contains two elements: H is a data frame with h-index, g-index and m-index for each author; CitationList is a list with the bibliographic collection for each author.
#'
#' 
#' @examples
#' 
#' ### EXAMPLE 1: ###
#'  
#' data(scientometrics)
#'
#' authors <- c("SMALL H", "CHEN DZ")
#'
#' Hindex(scientometrics, authors, sep = ";")$H
#' 
#' ### EXAMPLE 2: Garfield h-index###
#'  
#' data(garfield)
#'
#' indices=Hindex(garfield, authors="GARFIELD E", sep = ";")
#'
#' # h-index, g-index and m-index of Eugene Garfield
#' indices$H
#' 
#' # Papers and total citations
#' indices$CitationList[[1]]
#'
#' @seealso \code{\link{convert2df}} to import and convert an ISI or SCOPUS Export file in a bibliographic data frame. 
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis.
#' @seealso \code{\link{summary}} to obtain a summary of the results.
#' @seealso \code{\link{plot}} to draw some useful plots of the results.
#' 
#' @export

Hindex <- function(M, authors, sep = ";",years=10){
  
  Today=as.numeric(substr(Sys.time(),1,4))
  past=Today-years
  if (min(M$PY)<past){M=M[M$PY>=past,]}
  
  TC2=NULL
   ## identify manuscripts of the author
  AU=M$AU
  AU=gsub(","," ",AU)
  AU=gsub("  "," ",AU)
 
  H=data.frame(Author=authors,h_index=0,g_index=0,m_index=0,TC=0,NP=0)
  TotalCitations=list()
  for (j in 1:length(authors)){
    author=authors[j]
    ind=which(regexpr(author,AU)!=-1)
    
    if (length(ind)>0){
    range=as.numeric(format(Sys.Date(),'%Y'))-min(as.numeric(M$PY[ind]))+1
    H[j,6]=length(ind)
    H[j,5]=sum(M$TC[ind])
    TC=sort(as.numeric(M$TC[ind]))
    TCC=sort(as.numeric(M$TC[ind]),decreasing = TRUE)
      for (i in 1:length(TC)){
        TC2[i]=mean(TCC[1:i])
        if (TC2[i]>=i){H[j,3]=i}
        if (length(TC[TC>=i])>=i){
        H[j,2]=i
        H[j,4]=i/range}
      }
    }
    #TotalCitations[[j]]=data.frame(Year=as.numeric(M$PY[ind]),TC,Year2=sort(as.numeric(M$PY[ind])),TC2)
    if (length(ind)>0){
    df=data.frame(Authors=substr(M$AU[ind], 1, 30),Journal=substr(M$SO[ind], 1, 30),Year=as.numeric(M$PY[ind]),TotalCitation=M$TC[ind])
    TotalCitations[[j]]=df[order(df$TotalCitation),]
    }
    }
  results=list(H=H,CitationList=TotalCitations)
  return(results)
  }