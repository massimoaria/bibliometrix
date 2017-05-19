#' Author local citations 
#'
#' It calculates frequency distribution of cited local authors.
#' 
#' Local citations measure how many times an author included in this collection have been cited by other authors also in the collection.
#'
#' @param M is a bibliographic data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to manuscripts and variables to Field Tag in the original SCOPUS and Thomson Reuters' ISI Web of Knowledge file.
#' @param results is an object of \code{class} "bibliometrix". The default is \code{field = "article"}.
#' @param sep is the field separator character. This character separates citations in each string of CR column of the bibiographic data frame. The default is \code{sep = ";"}.
#' @return an object of \code{class} "table".
#'
#' 
#'
#' @examples
#'  
#' data(scientometrics)
#' 
#' results <- biblioAnalysis(scientometrics)
#'
#' CR <- localCitations(scientometrics, results, sep = ";")
#'
#' CR[1:10]
#'
#' @seealso \code{\link{citations}} function for citation frequency distribution.
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis.
#' @seealso \code{\link{summary}} to obtain a summary of the results.
#' @seealso \code{\link{plot}} to draw some useful plots of the results.
#' 
#' @export

localCitations <- function(M, results, sep = ";"){
  CR=NULL
  
    listCR=strsplit(M$CR,sep)
    if (M$DB[1]=="ISI"){ 
      listCR=lapply(listCR, function(l){
        ListL=lapply(strsplit(unlist(l),","),function(x) x[1])
        l=trim(unlist(ListL))
        
      })}
    if (M$DB[1]=="SCOPUS"){ 
      listCR=lapply(listCR, function(l){
        ListL=lapply(l,function(x) {
          a=strsplit(x,"\\., ")
          ind=which(grepl("[[:digit:]]", a[[1]]))
          if (length(ind)==0) ind=1
          x=unlist(a[[1]][1:(ind[1]-1)])
          })
        l=unlist(ListL)
      })}
  
    listCR=lapply(listCR,function(l){
      L=lapply(strsplit(l," "),function(x){
        x=paste(x[1],substr(x[2],1,1))
      })
    })
  CR=unlist(listCR)
  CR=gsub("\\.","",CR)
  CR=CR[nchar(CR)>=3]
  if (M$DB[1]=="ISI"){
    CR=trim.leading(CR)
    CR=gsub(" ","-",CR)
    #AU=trim.leading(results$Authors)
    AU=gsub(",","-",names(results$Authors))}
  if (M$DB[1]=="SCOPUS"){
    CR=gsub(" ","",CR)
    AU=gsub(" ","",names(results$Authors))}
  CR=CR[CR %in% AU]
  CR=gsub("-"," ",CR)
  CR=sort(table(CR),decreasing=TRUE)
  return(CR)
}