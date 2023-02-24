#' Bibliographic bipartite network matrices
#'
#' \code{cocMatrix} computes occurrences between elements of a Tag Field from a bibliographic data frame. Manuscript is the unit of analysis.
#'
#' This occurrence matrix represents a bipartite network which can be transformed into a collection of bibliographic
#' networks such as coupling, co-citation, etc.. 
#' 
#' The function follows the approach proposed by Batagelj & Cerinsek (2013) and Aria & cuccurullo (2017).\cr\cr
#' 
#' References:\cr
#' Batagelj, V., & Cerinsek, M. (2013). On bibliographic networks. Scientometrics, 96(3), 845-864.\cr
#' Aria, M., & Cuccurullo, C. (2017). bibliometrix: An R-tool for comprehensive science mapping analysis. Journal of Informetrics, 11(4), 959-975.\cr
#' 
#' @param M is a data frame obtained by the converting function
#'   \code{\link{convert2df}}. It is a data matrix with cases corresponding to
#'   articles and variables to Field Tag in the original WoS or SCOPUS file.
#' @param Field is a character object. It indicates one of the field tags of the
#'   standard ISI WoS Field Tag codify. Field can be equal to one of these tags:
#'   \tabular{lll}{ \code{AU}\tab   \tab Authors\cr \code{SO}\tab   \tab
#'   Publication Name (or Source)\cr \code{JI}\tab   \tab ISO Source
#'   Abbreviation\cr \code{DE}\tab   \tab Author Keywords\cr \code{ID}\tab
#'   \tab Keywords associated by WoS or SCOPUS database \cr \code{CR}\tab   \tab
#'   Cited References}
#'
#'   for a complete list of filed tags see:
#'   \href{https://www.bibliometrix.org/documents/Field_Tags_bibliometrix.pdf}{Field Tags used in bibliometrix}\cr\cr
#'   
#' @param type indicates the output format of co-occurrences: \tabular{lll}{
#'   \code{type = "matrix"} \tab   \tab produces an object of class
#'   \code{matrix}\cr \code{type = "sparse"} \tab   \tab produces an object of
#'   class \code{dgMatrix} of the package \code{\link{Matrix}}. "sparse"
#'   argument generates a compact representation of the matrix.}
#' @param n is an integer. It indicates the number of items to select. If \code{N = NULL}, all items are selected.
#' @param sep is the field separator character. This character separates strings in each 
#' column of the data frame. The default is \code{sep = ";"}.
#' @param binary is a logical. If TRUE each cell contains a 0/1. if FALSE each cell contains the frequency. 
#' @param short is a logical. If TRUE all items with frequency<2 are deleted to reduce the matrix size.
#' @param remove.terms is a character vector. It contains a list of additional terms to delete from the documents before term extraction. The default is \code{remove.terms = NULL}.
#' @param synonyms is a character vector. Each element contains a list of synonyms, separated by ";",  that will be merged into a single term (the first word contained in the vector element). The default is \code{synonyms = NULL}.
#' @return a bipartite network matrix with cases corresponding to manuscripts and variables to the
#'   objects extracted from the Tag \code{Field}.
#'
#' @examples
#' # EXAMPLE 1: Articles x Authors occurrence matrix
#'
#' data(scientometrics, package = "bibliometrixData")
#' WA <- cocMatrix(scientometrics, Field = "AU", type = "sparse", sep = ";")
#'
#' # EXAMPLE 2: Articles x Cited References occurrence matrix
#'
#' # data(scientometrics, package = "bibliometrixData")
#'
#' # WCR <- cocMatrix(scientometrics, Field = "CR", type = "sparse", sep = ";")
#'
#' # EXAMPLE 3: Articles x Cited First Authors occurrence matrix
#'
#' # data(scientometrics, package = "bibliometrixData")
#' # scientometrics <- metaTagExtraction(scientometrics, Field = "CR_AU", sep = ";")
#' # WCR <- cocMatrix(scientometrics, Field = "CR_AU", type = "sparse", sep = ";")
#' 
#' @seealso \code{\link{convert2df}} to import and convert an ISI or SCOPUS
#'   Export file in a data frame.
#' @seealso \code{\link{biblioAnalysis}} to perform a bibliometric analysis.
#' @seealso \code{\link{biblioNetwork}} to compute a bibliographic network.
#' @export

cocMatrix<-function(M, Field = "AU", type = "sparse", n=NULL, sep = ";",binary=TRUE, 
                    short = FALSE, remove.terms = NULL, synonyms = NULL){
  #
  # The function creates occurrences data between Works and Field
  #
  # type indicates the output format of occurrences:
  #   "matrix" argument generates a W x Field sparse matrix
  #   "sparse" argument generates a compact representation of the matrix (using the package Matrix)
  #    it represents a compact representation of a occurrences matrix.
  # Field indicates the ISI Tag
  # if Field is AU -> WA (Works x Authors)
  # if Field is CR -> WR (Works x References)
  # if Field is DE -> WK (Works x Keywords)
  # etc.
  #crossprod <- Matrix::crossprod
  size<-dim(M)
  if (!"LABEL" %in% names(M)) row.names(M) <- M$SR
  RowNames <- row.names(M)
  
  ### REMOVE TERMS AND MERGE SYNONYMS
  if (Field %in% c("ID", "DE", "TI", "TI_TM", "AB", "AB_TM")){
    # Crete df with all terms
    
    Fi <- strsplit(M[,Field], sep)
    TERMS <- data.frame(item = trimws(unlist(Fi)), SR = rep(M$SR,lengths(Fi)))

    # Merge synonyms in the vector synonyms
    if (length(synonyms)>0 & is.character(synonyms)){
      s <- strsplit(toupper(synonyms),";")
      snew <- trimws(unlist(lapply(s,function(l) l[1])))
      sold <- (lapply(s,function(l){
        l <- trimws(l[-1])
      }))
      
      for (i in 1:length(s)){
        TERMS$item[TERMS$item %in% unlist(sold[[i]])] <- snew[i]
      }
    }
    
    TERMS <- TERMS %>% 
      anti_join(data.frame(item=trimws(toupper(remove.terms))), by="item") %>% 
      mutate(item = trimws(.data$item))
    
      TERMS <- TERMS %>% 
        group_by(.data$SR) %>%
        summarize(item = paste(.data$item, collapse=";"))
      
      M <- M %>% 
        left_join(TERMS, by="SR")
      M[,Field] <- M$item
    
  }
  row.names(M) <- RowNames
  if (Field=="CR"){M$CR<-gsub("DOI;","DOI ",as.character(M$CR))}
  
  if (Field %in% names(M)){
    Fi<-strsplit(M[,Field],sep)} else{return(print(paste("Field",Field,"is not a column name of input data frame")))}
  Fi<-lapply(Fi,trim.leading)
  if (Field=="CR"){Fi<-lapply(Fi,function(l) l<-l[nchar(l)>10])}  ## delete not congruent references
  
  ## Scelta dell'informazione contenuta in CR da utilizzare (Reference, Autore, Affiliation, ecc.)
  
  # vector of unique units
  allField <- unlist(Fi)
  allField <- allField[!is.na(allField)]

  if (Field=="CR"){
    ind <- which(substr(allField,1,1)!="(")
    S <- allField
    S[ind] <- gsub("\\).*", ")", allField[ind])
    S[-ind] <- substr(S[-ind],1,100)
    S<-gsub(","," ",S)
    S<-gsub(";"," ",S)
    S<-reduceRefs(S)
    allField <- trimES(S)
    Fi<-lapply(Fi, function(l){
      l<-gsub("\\).*",")",l)
      l<-gsub(","," ",l)
      l<-gsub(";"," ",l)
      l<-l[nchar(l)>0]
      l<-reduceRefs(l)
      l<-trimES(l)
      return(l)
    })
  } else {
    # normalize reference names
    S<-gsub("\\,", ";", allField)
    S<-sub("\\;",",",S)
    S<-sub("\\;",",",S)
    S<-gsub("\\;.*","",S)
    allField <- trimws(S)
    Fi<-lapply(Fi, function(l){
      l<-gsub("\\,",";",l)
      l<-sub("\\;",",",l)
      l<-sub("\\;",",",l)
      l<-gsub("\\;.*","",l)
      l<-l[nchar(l)>0]
      l<-trimws(l)
      return(l)
    })
  }
  tabField <- sort(table(allField), decreasing = TRUE)
  uniqueField <- names(tabField)			     
  # select n items
  if (!is.null(n)) {
    uniqueField <- uniqueField[1:n]
  } else if (isTRUE(short)){
    uniqueField <- names(tabField[tabField>1])  # remove items with frequency<2
  }
  
  if (length(uniqueField)<1){
    print("Matrix is empty!!")
    return(NA)
  }
  
  if (type=="matrix" | !isTRUE(binary)){
    # Initialization of WA matrix
    WF<-matrix(0,size[1],length(uniqueField))
    } else if (type=="sparse"){
      WF<-Matrix(0,size[1],length(uniqueField))
    } else {
        print("error in type argument")
      return()
      }
  colnames(WF)<-uniqueField
  rownames(WF)<-rownames(M)
  # Population of WA matrix
  for (i in 1:size[1]){
    if (length(Fi[[i]])>0 & !is.na(Fi[[i]][1])) {
      #print(i)
      #if (Field=="CR"){Fi[[i]]=reduceRefs(Fi[[i]])}
      if (isTRUE(binary)){
        ## binary counting
        ind <- uniqueField %in% Fi[[i]]
        if (sum(ind) > 0){
          WF[i, ind] <- 1
        }
      }
      else{
        ## full counting
        tab=table(Fi[[i]])
        name <- names(tab)[names(tab) %in% uniqueField]
        name <- name[nchar(name)>0]
        if (length(name)>0){
          WF[i,name] <- tab[name]
        }
      }
    }
  }
  
  if (type=="sparse" & !isTRUE(binary)){
    WF <- Matrix(WF)
  }
  
  
  WF <- WF[,!is.na(uniqueField)]
  ind <- which(colnames(WF)=="NA")
  if (length(ind)>0) {WF <- WF[,-ind]}
  
  return(WF)
}

reduceRefs<- function(A){
  
  ind=unlist(regexec("*V[0-9]", A))
  A[ind>-1]=substr(A[ind>-1],1,(ind[ind>-1]-1))
  ind=unlist(regexec("*DOI ", A))
  A[ind>-1]=substr(A[ind>-1],1,(ind[ind>-1]-1))
  return(A)
}
