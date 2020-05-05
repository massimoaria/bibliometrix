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
#' @return an object of class \code{table}
#' @examples
#'
#' data(scientometrics)
#' Tab <- tableTag(scientometrics, Tag = "CR", sep = ";")
#' Tab[1:10]
#'
#' @export
tableTag <- function(M, Tag = "CR", sep = ";"){
  
  if (Tag %in% c("AB","TI")){
    M=termExtraction(M,Field=Tag,stemming=F,verbose=FALSE)
    i=dim(M)[2]
  }else{i<-which(names(M)==Tag)}
  
  if (Tag=="C1"){
    M$C1=gsub("\\[.+?]","",M$C1) 
  }
  
  Tab<-unlist(strsplit(as.character(M[,i]),sep))
  
  ### inserted to remove punct and extra spaces ####
  #Tab<-trimws(trimES(gsub("[[:punct:]]"," ",Tab)))
  Tab<-trimws(trimES(gsub("\\.|\\,"," ",Tab)))
  ####
  Tab<-Tab[Tab!=""]
  Tab<-sort(table(Tab),decreasing=TRUE)
  return(Tab)
}
