#' Merge bibliographic data frames from SCOPUS and WoS  
#'
#' Merge bibliographic data frames from different databases (WoS and SCOPUS) into a single one.
#' 
#' bibliographic data frames are obtained by the converting function \code{\link{convert2df}}. 
#' The function merges data frames identifying common tag fields and duplicated records.
#'
#' @param ... are the bibliographic data frames to merge.
#' @param remove.duplicated is logical. If TRUE duplicated documents will be deleted from the bibliographic collection.
#' @return the value returned from \code{mergeDbSources} is a bibliographic data frame.
#'
#'
#' @examples
#'  
#' data(isiCollection)
#' 
#' data(scopusCollection)
#' 
#' M <- mergeDbSources(isiCollection, scopusCollection, remove.duplicated=TRUE)
#' 
#' dim(M)
#'
#'
#' @seealso \code{\link{convert2df}} to import and convert an ISI or SCOPUS Export file in a bibliographic data frame.
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis.
#' @seealso \code{\link{summary}} to obtain a summary of the results.
#' @seealso \code{\link{plot}} to draw some useful plots of the results.
#'
#' @export


mergeDbSources <- function(...,remove.duplicated=TRUE){

###
  L <- list(...)
  
  n=length(L)
  
  Tags=names(L[[1]])

  ## identification of common tags
  for (j in 2:n){
    Tags=intersect(Tags,names(L[[j]]))
  }
  #####
  M=data.frame(matrix(NA,1,length(Tags)))
  names(M)=Tags
  for (j in 1:n){
    L[[j]]=L[[j]][,Tags]
    
    M=rbind(M,L[[j]])
  }
  
  ## author data cleaning
  if ("AU" %in% Tags){
    M$AU=gsub(","," ",M$AU)
    AUlist=strsplit(M$AU,";")
    AU=lapply(AUlist,function(l){
      l=trim(l)
      name=strsplit(l," ")
      lastname=unlist(lapply(name,function(ln){ln[1]}))
      firstname=lapply(name,function(ln){
        f=paste(substr(ln[-1],1,1),collapse=" ")
        })
      AU=paste(lastname,unlist(firstname),sep=" ",collapse=";")
      return(AU)
    })
    M$AU=unlist(AU)
    
  }
  M=M[-1,]
  M$DB="ISI"
  
  if (isTRUE(remove.duplicated)){
    M$TI=gsub("[^[:alnum:] ]","",M$TI)
    M$TI=gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", M$TI, perl=TRUE)
    d=duplicated(M$TI)
    cat("\n",sum(d),"duplicated documents have been removed\n")
    M=M[!d,]
  }
  return(M)
}
