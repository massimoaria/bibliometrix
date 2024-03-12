utils::globalVariables(c("SR", "text", "ngram"))
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
#' @param ngrams is an integer between 1 and 3. It indicates the type of n-gram to extract from texts. 
#' An n-gram is a contiguous sequence of n terms. The function can extract n-grams composed by 1, 2, 3 or 4 terms. Default value is \code{ngrams=1}.
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
#' data(scientometrics, package = "bibliometrixData")
#' 
#' # vector of compound words
#' keep.terms <- c("co-citation analysis","bibliographic coupling")
#' 
#' # term extraction
#' scientometrics <- termExtraction(scientometrics, Field = "TI", ngrams = 1,
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
#' # term extraction
#' scientometrics <- termExtraction(scientometrics, Field = "AB", ngrams = 2, 
#'  stemming=TRUE,language="english",
#'  remove.numbers=TRUE, remove.terms=NULL, keep.terms=NULL, verbose=TRUE)
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
#' scientometrics <- termExtraction(scientometrics, Field = "ID", ngrams = 1,
#' synonyms=synonyms, verbose=TRUE)
#'
#'
#' @seealso \code{\link{convert2df}} to import and convert an WoS or SCOPUS Export file in a bibliographic data frame.
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis
#' 
#' @export



termExtraction <- function(M, Field="TI", ngrams = 1, stemming=FALSE, language="english",remove.numbers=TRUE, remove.terms=NULL, keep.terms=NULL, synonyms=NULL, verbose=TRUE){
  
  # ngrams imposed = 1 for keywords
  if (Field %in% c("ID","DE")){ngrams <- 1}
  
  # load stopwords
  data("stopwords",envir=environment())
  data("stop_words", envir=environment(), package = "tidytext")
  stop_words <- stop_words %>% as.data.frame()
  
  if (ngrams == 2){remove.terms <- c(remove.terms,stopwords$bigrams)}
  
  switch(language,
    english={stopwords=(stop_words$word)},
    italian={stopwords=stopwords$it},
    german={stopwords=stopwords$de},
    french={stopwords=stopwords$fr},
    spanish={stopwords=stopwords$es}
    )
  stopwords <- tolower(stopwords)

  # remove all special characters (except "-" becoming "_")
  TERMS <- M %>% 
    select(SR,!!Field)
  
  names(TERMS) <- c("SR","text")
  
  TERMS$text <- gsub(" - "," ",TERMS$text)
  
  # save original multi-words keywords
  if (Field %in% c("ID","DE")){
    listTerms <- strsplit(TERMS$text,";")
    TERMS$text <- unlist(lapply(listTerms, function(l){
      l <- gsub("-","__",trimES(trimws(l)))
      l <- tolower(paste(gsub(" ","_",l), sep="", collapse=";"))  
    }))
  } else {
  
  TERMS <- TERMS %>%
    mutate(text = tolower(gsub("[^[:alnum:][:blank:]\\-]", "", text)),
           text = gsub("-", "__",text))
  }

  
  # remove numbers
  if (remove.numbers==TRUE){
    TERMS <- TERMS %>%
      mutate(text = gsub("[[:digit:]]","",text))
    }
  
  # keep terms in the vector keep.terms
  if (length(keep.terms)>0 & is.character(keep.terms)){
    keep.terms <- tolower(keep.terms)
    if (Field %in% c("DE","ID")){
      kt <- gsub(" ","_",keep.terms)
      kt <- gsub("-","__",keep.terms)
    } else {
      kt <- gsub("-","__",keep.terms)
    }
    for (i in 1:length(keep.terms)){
      TERMS <- TERMS %>%
        mutate(text = gsub(keep.terms[i],kt[i],text))
    }
  }

  if (is.null(remove.terms)) remove.terms <- ""
  
  TERMS <- extractNgrams(text=TERMS, Var="text", nword=ngrams, 
                             stopwords=stopwords, custom_stopwords=tolower(remove.terms),
                             stemming=stemming, language=language, synonyms = synonyms, Field = Field)
  
  TERMS <- TERMS %>%
    dplyr::filter(!(ngram %in% paste(rep("NA",ngrams),sep="",collapse=" "))) %>% 
    group_by(SR) %>%
    summarize(text = paste(ngram, collapse=";"))
  
  # assign the vector to the bibliographic data frame
  col_name <- paste(Field,"_TM",sep="")
  M <- M[!names(M) %in% col_name]
  
  M <- TERMS %>%
    right_join(M, by = "SR") 
  names(M)[which(names(M) %in% "text")] <- col_name
  
  
  # display results
  if (verbose==TRUE){
    s <- tableTag(M,col_name)
    
    if (length(s>25)){print(s[1:25])}else{print(s)}
    }
  
  class(M) <- c("bibliometrixDB", "data.frame")
  row.names(M) <- M$SR
  return(M)
  
}

extractNgrams <- function(text, Var, nword, stopwords, custom_stopwords, stemming, language, synonyms, Field){
  # text is data frame containing the corpus data text = M %>% select(.data$SR,.data$AB)
  # Var is a string indicating the column name. I.e. Var = "AB"
  # nword is a integer vector indicating the ngrams to extract. I.e. nword = c(2,3)
  
  stopwords <- c(stopwords,"elsevier", "springer", "wiley", "mdpi", "emerald", "originalityvalue", "designmethodologyapproach", 
                 "-", " -", "-present", "-based", "-literature", "-matter")
  custom_stopngrams <- c(custom_stopwords,"rights reserved", "john wiley", "john wiley sons", "science bv", "mdpi basel", 
                         "mdpi licensee", "emerald publishing", "taylor francis", "paper proposes", 
                         "we proposes", "paper aims", "articles published", "study aims", "research limitationsimplications")
  ngram <- NULL
  
  # ngrams <- text %>%
  #   drop_na(any_of(Var)) %>%
  #   unnest_tokens(ngram, !!Var, token = "ngrams", n = nword) %>%
  #   separate(.data$ngram, paste("word",1:nword,sep=""), sep = " ")
  ngrams <- text %>%
    drop_na(any_of(Var)) %>%
    unnest_tokens(ngram, !!Var, token = "ngrams", n = nword) 
  ind <- which(substr(ngrams$ngram,1,2) %in% "__")
  ngrams$ngram[ind] <- trimws(substr(ngrams$ngram[ind],3,nchar(ngrams$ngram[ind])))
  
    ngrams <- ngrams %>%  
    separate(ngram, paste("word",1:nword,sep=""), sep = " ")
  
  
  
  ## come back to the original multiword format
    ngrams <- ngrams %>%
    mutate_at(paste("word",seq(1,nword),sep=""), ~gsub("__", "-",.)) %>% 
    mutate_at(paste("word",seq(1,nword),sep=""), ~gsub("_", " ",.))
  ##
   
    ngrams <- ngrams %>% dplyr::filter(if_all(starts_with("word"), ~ !.x %in% stopwords))
  
  if (isTRUE(stemming)){
    ngrams <- ngrams %>% 
      mutate(across(paste("word",1:nword,sep=""), ~SnowballC::wordStem(.x,language=language)))
    }
    #filter(if_all(starts_with("word"), ~ !str_detect(.x, "\\d"))) %>%
    ngrams <- ngrams %>% 
      unite(ngram, paste("word",1:nword,sep=""), sep = " ") %>%
      dplyr::filter(!ngram %in% custom_stopngrams) %>%
      mutate(ngram = toupper(ngram))

    # Merge synonyms in the vector synonyms
    if (length(synonyms)>0 & is.character(synonyms)){
      s <- strsplit(toupper(synonyms),";")
      snew <- trimws(unlist(lapply(s,function(l) l[1])))
      sold <- (lapply(s,function(l){
        l <- trimws(l[-1])
        #l <- paste("(?<![[^[:alnum:]]|[[:alnum:]]])",l,sep="")  ### string to make an exact matching
      }))

      for (i in 1:length(s)){
        ngrams$ngram[ngrams$ngram %in% unlist(sold[[i]])] <- snew[i]
      }
    }
    
  return(ngrams)
}

