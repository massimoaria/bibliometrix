#' Term extraction tool from textual fields of a manuscript
#'
#' It extracts terms from a text field (abstract, title, author's keywords, etc.) of a bibliographic data frame.
#' @param M is a data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to articles and variables to Field Tag in the original WoS or SCOPUS file.
#' @param Field is a character object. It indicates the field tag of textual data :
#' \tabular{lll}{
#' \code{"TI"}\tab   \tab Manuscript title\cr
#' \code{"AB"}\tab   \tab Manuscript abstract\cr
#' \code{"ID"}\tab   \tab Manuscript keywords plus\cr
#' \code{"DE"}\tab   \tab Manuscript author's keywords}
#' The default is \code{Field = "TI"}.
#'
#' @param stemming is logical. If TRUE the Porter Stemming algorithm is applied to all extracted terms. The default is \code{stemming = FALSE}.
#' @param language is a character. It is the language of textual contents ("english", "german","italian","french","spanish"). The default is \code{language="english"}.
#' @param remove.numbers is logical. If TRUE all numbers are deleted from the documents before term extraction. The default is \code{remove.numbers = TRUE}.
#' @param remove.terms is a character vector. It contains a list of additional terms to delete from the documents before term extraction. The default is \code{remove.terms = NULL}.
#' @param keep.terms is a character vector. It contains a list of compound words "formed by two or more terms" to keep in their original form in the term extraction process. The default is \code{keep.terms = NULL}.
#' @param synonyms is a character vector. Each element contains a list of synonyms, separated by ";",  that will be merged into a single term (the first word contained in the vector element). The default is \code{synonyms = NULL}.
#' @param verbose is logical. If TRUE the function prints the most frequent terms extracted from documents. The default is \code{verbose=TRUE}.
#' @return the bibliometric data frame with a new column containing terms about the field tag indicated in the argument \code{Field}.
#'
#'
#' @examples
#' # Example 1: Term extraction from titles
#'
#' data(scientometrics)
#' 
#' # vector of compound words
#' keep.terms <- c("co-citation analysis","bibliographic coupling")
#' 
#' # term extraction
#' scientometrics <- termExtraction(scientometrics, Field = "TI",
#' remove.numbers=TRUE, remove.terms=NULL, keep.terms=keep.terms, verbose=TRUE)
#' 
#' # terms extracted from the first 10 titles
#' scientometrics$TI_TM[1:10]
#'
#'
#' #Example 2: Term extraction from abstracts
#'
#' data(scientometrics)
#' 
#' # vector of terms to remove
#' remove.terms=c("analysis","bibliographic")
#' 
#' # term extraction
#' scientometrics <- termExtraction(scientometrics, Field = "AB", stemming=TRUE,language="english",
#'  remove.numbers=TRUE, remove.terms=remove.terms, keep.terms=NULL, verbose=TRUE)
#' 
#' # terms extracted from the first abstract
#' scientometrics$AB_TM[1]
#' 
#' # Example 3: Term extraction from keywords with synonyms
#'
#' data(scientometrics)
#' 
#' # vector of synonyms 
#' synonyms <- c("citation; citation analysis", "h-index; index; impact factor")
#' 
#' # term extraction
#' scientometrics <- termExtraction(scientometrics, Field = "ID",
#' synonyms=synonyms, verbose=TRUE)
#'
#'
#' @seealso \code{\link{convert2df}} to import and convert an WoS or SCOPUS Export file in a bibliographic data frame.
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis
#' 
#' @export

termExtraction <- function(M, Field="TI", stemming=FALSE,language="english",remove.numbers=TRUE, remove.terms=NULL, keep.terms=NULL, synonyms=NULL, verbose=TRUE){
  
  # load stopwords
  data("stopwords",envir=environment())
  switch(language,
    english={stopwords=stopwords$en},
    italian={stopwords=stopwords$it},
    german={stopwords=stopwords$de},
    french={stopwords=stopwords$fr},
    spanish={stopwords=stopwords$es}
    )
  #stopwords=stopwords

  # remove all special characters (except "-")
  TERMS=toupper(M[,Field])
  TERMS=gsub("ELSEVIER B.V. ALL RIGHTS RESERVED","",TERMS)
  TERMS=gsub("RIGHTS RESERVED","",TERMS)
  TERMS=gsub("ELSEVIER","",TERMS)
  TERMS=gsub("[^[:alnum:][:blank:]\\-]", "", TERMS)
  
  if (remove.numbers==TRUE){TERMS=gsub("[[:digit:]]","",TERMS)}
  
  TERMS=gsub("\\."," ",TERMS)
  TERMS=gsub(" - "," ",TERMS)
  
  # keep terms in the vector keep.terms
  if (length(keep.terms)>0 & class(keep.terms)=="character"){
    keep.terms=toupper(keep.terms)
    kt=gsub(" ","-",keep.terms)
    for (i in 1:length(keep.terms)){
      TERMS=gsub(keep.terms[i],kt[i],TERMS)
    }
  }
  
  # create a list of terms for each document
  listTERMS=strsplit(TERMS,split=" ")

  # merge synonyms 
  if (length(synonyms)>0 & class(synonyms)=="character"){
    synonyms=toupper(synonyms)
    listTERMS=lapply(listTERMS,function(l){
      s=(strsplit(synonyms,split=";"))
      
      for (i in 1:length(synonyms)){
        
        ind=which(l %in% trim(s[[i]]))
        if (length(ind)>0){l[ind]=trim(s[[i]][1])}
      }
      return(l)
    })
  }
  
  # remove stopwords from each list of terms
  listTERMS=lapply(listTERMS,function(l){
    l=l[!(l %in% stopwords)]
    l=l[nchar(l)>1]
  })
  
  # remove user-defined terms from each list of terms
  if (length(remove.terms)>0 & class(remove.terms)=="character"){
    remove.terms=toupper(remove.terms)
    listTERMS=lapply(listTERMS,function(l){
      l=l[!(l %in% remove.terms)]
      l=l[nchar(l)>1]
    })
  }
  
  # word stemming algorithm
  if (stemming==TRUE){
    listTERMS=lapply(listTERMS,function(l){
      l=tolower(l)
      l=toupper(SnowballC::wordStem(l,language=language))
    })
  }
  
  
  # create a vector of extracted terms
  TM=unlist(lapply(listTERMS,function(l){
    l=paste0(l,collapse=";")
  }))
  
  # assign the vector to the bibliographic data frame
  switch(Field,
         TI={M$TI_TM=TM},
         AB={M$AB_TM=TM},
         ID={M$ID_TM=TM},
         DE={M$DE_TM=TM})

  
  # display results
  if (verbose==TRUE){
    s=sort(table(unlist(strsplit(TM,split=";"))),decreasing = TRUE)
    
    if (length(s>25)){print(s[1:25])}else{print(s)}
    }
  
  return(M)
  
}