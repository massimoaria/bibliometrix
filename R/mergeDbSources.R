utils::globalVariables(c("num"))
#' Merge bibliographic data frames from supported bibliogtraphic DBs  
#'
#' Merge bibliographic data frames from different databases (WoS,SCOPUS, Lens, Openalex, etc-) into a single one.
#' 
#' bibliographic data frames are obtained by the converting function \code{\link{convert2df}}. 
#' The function merges data frames identifying common tag fields and duplicated records.
#'
#' @param ... are the bibliographic data frames to merge.
#' @param remove.duplicated is logical. If TRUE duplicated documents will be deleted from the bibliographic collection.
#' @param verbose is logical.  If TRUE, information on duplicate documents is printed on the screen.
#' @return the value returned from \code{mergeDbSources} is a bibliographic data frame.
#'
#'
#' @examples
#'
#' data(isiCollection, package = "bibliometrixData")
#' 
#' data(scopusCollection, package = "bibliometrixData")
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


mergeDbSources <- function(...,remove.duplicated=TRUE, verbose=TRUE){

  index <- NULL

  mc <- match.call(expand.dots = TRUE)
  
  if (length(mc)>3){
      M <- dplyr::bind_rows(list(...))
    }else{
      M <- dplyr::bind_rows(...)
    }
  
   dbLabels <- data.frame(DB = toupper(c("isi","scopus","openalex","lens","dimensions","pubmed","cochrane")),
                          num = c(1,2,3,4,5,6,7))
   # order by db
   M <- M %>% 
     left_join(dbLabels, by = "DB") %>% 
     arrange(num) %>% 
     select(-num) %>% 
     rename("CR_raw" = "CR") %>% 
     mutate(CR = "NA")
  
  
  if (isTRUE(remove.duplicated)){
    # remove by DOI
    if ("DI" %in% names(M)){ 
      M$DI[M$DI == ""] <- NA
      index <- which(duplicated(M$DI) & !is.na(M$DI))
      if (length(index)>0) M <- M[-index,]
    }
    
    # remove by title
    if ("TI" %in% names(M)){
      TI <- gsub("[^[:alnum:] ]","",M$TI)
      TI <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", TI, perl=TRUE)
      d <- duplicated(paste(TI," ",M$PY))
      if (isTRUE(verbose)) cat("\n",sum(d)+length(index),"duplicated documents have been removed\n")
      M <- M[!d,]
    }
  }
  
  if (length(unique(M$DB))>1){
    M$DB_Original <- M$DB
    M$DB <- "ISI"
    
    ## author data cleaning
    if ("AU" %in% names(M)){
      M$AU <- gsub(","," ",M$AU)
      AUlist <- strsplit(M$AU,";")
      AU <- lapply(AUlist,function(l){
        l <- trim(l)
        name <- strsplit(l," ")
        lastname <- unlist(lapply(name,function(ln){ln[1]}))
        firstname <- lapply(name,function(ln){
          f <- paste(substr(ln[-1],1,1),collapse=" ")
        })
        AU <- paste(lastname,unlist(firstname),sep=" ",collapse=";")
        return(AU)
      })
      M$AU <- unlist(AU)
      
    }
  }
  
  M <- metaTagExtraction(M, "SR")
  row.names(M) <- M$SR
  
  class(M) <- c("bibliometrixDB", "data.frame")
  return(M)
}
