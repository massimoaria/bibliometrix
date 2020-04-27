#' h-index calculation
#'
#' It calculates the authors' h-index and its variants.
#'
#' @param M is a bibliographic data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to manuscripts and variables to Field Tag in the original SCOPUS and Clarivate Analytics WoS file.
#' @param field is character. It can be equal to c("author", "source"). field indicates if H-index have to be calculated for a list of authors or for a list of sources. Default
#' value is \code{field = "author"}.
#' @param elements is a character vector. It contains the authors' names list or the source list for which you want to calculate the H-index. When the field is
#' "author", the argument has the form C("SURNAME1 N","SURNAME2 N",...), in other words, for each author: surname and initials separated by one blank space. 
#' i.e for the authors SEMPRONIO TIZIO CAIO and ARIA MASSIMO \code{elements} argument is \code{elements = c("SEMPRONIO TC", "ARIA M")}.
#' @param sep is the field separator character. This character separates authors in each string of AU column of the bibliographic data frame. The default is \code{sep = ";"}.
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
#' Hindex(scientometrics, field = "author", elements = authors, sep = ";")$H
#' 
#' Hindex(scientometrics, field = "source", elements = "SCIENTOMETRICS", sep = ";")$H
#' 
#' ### EXAMPLE 2: Garfield h-index###
#'  
#' data(garfield)
#'
#' indices=Hindex(garfield, field = "author", elements = "GARFIELD E", , sep = ";")
#'
#' # h-index, g-index and m-index of Eugene Garfield
#' indices$H
#' 
#' # Papers and total citations
#' indices$CitationList[[1]]
#'
#' @seealso \code{\link{convert2df}} to import and convert an WoS or SCOPUS Export file in a bibliographic data frame. 
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis.
#' @seealso \code{\link{summary}} to obtain a summary of the results.
#' @seealso \code{\link{plot}} to draw some useful plots of the results.
#' 
#' @export

Hindex <- function(M, field="author", elements, sep = ";",years=10){
  
  #elements=paste("\\\b",elements,"\\\b",sep="")
  M$TC=as.numeric(M$TC)
  M$PY=as.numeric(M$PY)
  Today=as.numeric(substr(Sys.time(),1,4))
  past=Today-years
  if (min(M$PY, na.rm = TRUE)<past){M=M[M$PY>=past,]}
  
  TC2=NULL
  
  switch(field,
         author={
           AU=M$AU
           AU=trimES(gsub(","," ",AU))
           i=which(regexpr("ANONYMOUS",elements)>-1)
           if(length(i)>0){elements=elements[-i]}
           Name="Author"
           AU=paste(";",AU,";",sep="")
           },
         source={
           SO=M$SO
           Name="Source"
         })
   
  ## identify manuscripts of the author or of the sources
  
 
  H=data.frame(Element=elements,h_index=0,g_index=0,m_index=0,TC=0,NP=0, PY_start=NA,stringsAsFactors = FALSE)
  TotalCitations=list()
  for (j in 1:length(elements)){
    author=elements[j]
    
    if (!is.null(shiny::getDefaultReactiveDomain())){shiny::incProgress(1/length(elements), detail=paste0("\nAuthor: ",author))}
    
    if (field=="source"){ind=which(SO==author)} else{
      ind=which(regexpr(paste(";",author,";",sep=""),AU)!=-1)
    }
    
    if (length(ind)>0){
    range=as.numeric(format(Sys.Date(),'%Y'))-min(as.numeric(M$PY[ind]))+1
    H[j,6]=length(ind)
    H[j,5]=sum(M$TC[ind])
    H[j,7]=min(as.numeric(M$PY[ind]), na.rm = TRUE)
    TC=sort(as.numeric(M$TC[ind]))
    TCC=sort(as.numeric(M$TC[ind]),decreasing = TRUE)
      for (i in 1:length(TC)){
        TC2[i]=mean(TCC[1:i], na.rm=TRUE)
        if (TC2[i]>=i){H[j,3]=i}
        if (length(TC[TC>=i])>=i){
        H[j,2]=i
        H[j,4]=i/range}
      }
    }
    #TotalCitations[[j]]=data.frame(Year=as.numeric(M$PY[ind]),TC,Year2=sort(as.numeric(M$PY[ind])),TC2)
    if (length(ind)>0){
    df=data.frame(Authors=substr(M$AU[ind], 1, 30),Journal=substr(M$SO[ind], 1, 30),Year=as.numeric(M$PY[ind]),TotalCitation=M$TC[ind], stringsAsFactors = FALSE)
    TotalCitations[[j]]=df[order(df$TotalCitation),]
    }
  }
  names(H)[1]=Name
  results=list(H=H,CitationList=TotalCitations)
  return(results)
}

