#' Tabulate elements from a Tag Field column
#'
#' It tabulates elements from a Tag Field column of a bibliographic data frame.
#'
#' \code{tableTag} is an internal routine of main function \code{\link{biblioAnalysis}}.
#'
#' @param M is a data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to articles and variables to Field Tag in the original WoS or SCOPUS file.
#' @param Tag is a character object. It indicates one of the field tags of the
#'   standard ISI WoS Field Tag codify.
#' @param sep is the field separator character. This character separates strings in each column of the data frame. The default is \code{sep = ";"}.
#' @param ngrams is an integer between 1 and 3. It indicates the type of n-gram to extract from titles or abstracts. 
#' @param remove.terms is a character vector. It contains a list of additional terms to delete from the documents before term extraction. The default is \code{remove.terms = NULL}.
#' @param synonyms is a character vector. Each element contains a list of synonyms, separated by ";",  that will be merged into a single term (the first word contained in the vector element). The default is \code{synonyms = NULL}.
#' @return an object of class \code{table}
#' @examples
#'
#' data(scientometrics, package = "bibliometrixData")
#' Tab <- tableTag(scientometrics, Tag = "CR", sep = ";")
#' Tab[1:10]
#'
#' @export
tableTag <- function(M, Tag = "CR", sep = ";", ngrams=1, remove.terms=NULL, synonyms=NULL){
  
  if (Tag %in% c("AB","TI")){
    M=termExtraction(M,Field=Tag,stemming=F,verbose=FALSE, ngrams = ngrams, remove.terms=remove.terms, synonyms=synonyms)
    i=which(names(M)==paste(Tag,"_TM",sep=""))
  }else{i<-which(names(M)==Tag)}
  
  if (Tag=="C1"){
    M$C1=gsub("\\[.+?]","",M$C1) 
  }
  
  Tab<-unlist(strsplit(as.character(M %>% dplyr::pull(i)),sep))
  
  ### inserted to remove punct and extra spaces ####
  #Tab<-trimws(trimES(gsub("[[:punct:]]"," ",Tab)))
  Tab<-trimws(trimES(gsub("\\.|\\,"," ",Tab)))
  ####
  Tab<-Tab[Tab!=""]
  
  # Merge synonyms in the vector synonyms
  if (length(synonyms)>0 & class(synonyms)=="character"){
    s <- strsplit(toupper(synonyms),";")
    snew <- trimws(unlist(lapply(s,function(l) l[1])))
    sold <- (lapply(s,function(l) trimws(l[-1])))
    for (i in 1:length(s)){
      Tab <-  str_replace_all(Tab, paste(sold[[i]], collapse="|",sep=""),snew[i])
    }
  }
  
  Tab<-sort(table(Tab),decreasing=TRUE)
  # remove terms from ID and DE
  if ((Tag %in% c("DE","ID")) & (!is.null(remove.terms))){
    term <- setdiff(names(Tab),toupper(remove.terms))
    Tab <- Tab[term]
  }
  
  return(Tab)
}
