#' Creating Bibliographic networks
#'
#' \code{biblioNetwork} creates different bibliographic networks from a bibliographic data frame.
#'
#' The function \code{\link{biblioNetwork}} can create a collection of bibliographic networks 
#' following the approach proposed by Batagelj & Cerinsek (2013) and Aria & cuccurullo (2017).\cr\cr
#' Typical networks output of \code{biblioNetwork} are:\cr\cr
#' #### Collaboration Networks ############\cr
#' -- Authors collaboration (analysis = "collaboration", network = "authors")\cr
#' -- University collaboration (analysis = "collaboration", network = universities")\cr
#' -- Country collaboration (analysis = "collaboration", network = "countries")\cr\cr
#' #### Co-citation Networks ##############\cr
#' -- Authors co-citation (analysis = "co-citation", network = "authors")\cr
#' -- Reference co-citation (analysis = "co-citation", network = "references")\cr
#' -- Source co-citation (analysis = "co-citation", network = "sources")\cr\cr
#' #### Coupling Networks ################\cr
#' -- Manuscript coupling (analysis = "coupling", network = "references")\cr
#' -- Authors coupling (analysis = "coupling", network = "authors")\cr
#' -- Source coupling (analysis = "coupling", network = "sources")\cr
#' -- Country coupling (analysis = "coupling", network = "countries")\cr\cr
#' #### Co-occurrences Networks ################\cr
#' -- Authors co-occurrences (analysis = "co-occurrences", network = "authors")\cr
#' -- Source co-occurrences (analysis = "co-occurrences", network = "sources")\cr
#' -- Keyword co-occurrences (analysis = "co-occurrences", network = "keywords")\cr
#' -- Author-Keyword co-occurrences (analysis = "co-occurrences", network = "author_keywords")\cr
#' -- Title content co-occurrences (analysis = "co-occurrences", network = "titles")\cr
#' -- Abstract content co-occurrences (analysis = "co-occurrences", network = "abstracts")\cr\cr
#'
#' References:\cr
#' Batagelj, V., & Cerinsek, M. (2013). On bibliographic networks. Scientometrics, 96(3), 845-864.\cr
#' Aria, M., & Cuccurullo, C. (2017). bibliometrix: An R-tool for comprehensive science mapping analysis. Journal of Informetrics, 11(4), 959-975.\cr
#'
#' @param M is a bibliographic data frame obtained by the converting function
#'   \code{\link{convert2df}}. It is a data matrix with cases corresponding to
#'   manuscripts and variables to Field Tag in the original SCOPUS and Clarivate Analytics WoS file.
#' @param analysis is a character object. It indicates the type of analysis can be performed.
#'   \code{analysis} argument can be \code{"collaboration"}, \code{"coupling"}, \code{"co-occurrences"}  or \code{"co-citation"}.
#'   Default is \code{analysis = "coupling"}.
#' @param network is a character object. It indicates the network typology. The \code{network} argument can be
#' \code{"authors"}, \code{"references"}, \code{"sources"}, \code{"countries"},\code{"keywords"}, \code{"author_keywords"}, \code{"titles"}, or \code{"abstracts"}.
#' Default is \code{network = "authors"}.
#' @param n is an integer. It indicates the number of items to select. If \code{N = NULL}, all items are selected.
#' @param sep is the field separator character. This character separates strings in each column of the data frame. The default is \code{sep = ";"}.
#' @param shortlabel is logical. IF TRUE, reference labels are stored in a short format. Default is \code{shortlabel=TRUE}.
#' @return It is a squared network matrix. It is an object of class \code{dgMatrix} of the package \code{\link{Matrix}}.
#' @examples
#' # EXAMPLE 1: Authors collaboration network
#'
#' # data(scientometrics)
#'
#' # NetMatrix <- biblioNetwork(scientometrics, analysis = "collaboration", 
#' # network = "authors", sep = ";")
#' 
#' # net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Collaboration",labelsize=0.5) 
#'
#'
#' # EXAMPLE 2: Co-citation network
#'
#' data(scientometrics)
#'
#' NetMatrix <- biblioNetwork(scientometrics, analysis = "co-citation", 
#' network = "references", sep = ";")
#' 
#' net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Co-Citation",labelsize=0.5) 
#'
#' @seealso \code{\link{convert2df}} to import and convert a SCOPUS and Thomson 
#'   Reuters' ISI Web of Knowledge export file in a data frame.
#' @seealso \code{\link{cocMatrix}} to compute a co-occurrence matrix.
#' @seealso \code{\link{biblioAnalysis}} to perform a bibliometric analysis.
#' 
#' @export

biblioNetwork <-
  function(M,
           analysis = "coupling",
           network = "authors",
           n = NULL,
           sep = ";",
           shortlabel = TRUE) {
    crossprod <- Matrix::crossprod
    NetMatrix <-  NA
    
    if (analysis == "coupling") {
      switch(
        network,
        authors = {
          WA <- cocMatrix(M, Field = "AU", type = "sparse", n,sep)
          WCR <- cocMatrix(M, Field = "CR", type = "sparse", n,sep)
          CRA <- crossprod(WCR, WA)
          NetMatrix <- crossprod(CRA, CRA)
        },
        references = {
          WCR <- Matrix::t(cocMatrix(M, Field = "CR", type = "sparse", n,sep))
          NetMatrix <- crossprod(WCR, WCR)
        },
        sources = {
          WSO <- cocMatrix(M, Field = "SO", type = "sparse", n, sep)
          WCR <- cocMatrix(M, Field = "CR", type = "sparse", n, sep)
          CRSO <- crossprod(WCR, WSO)
          NetMatrix <- crossprod(CRSO, CRSO)
        },
        countries = {
          WCO <- cocMatrix(M, Field = "AU_CO", type = "sparse", n, sep)
          WCR <- cocMatrix(M, Field = "CR", type = "sparse", n, sep)
          CRCO <- crossprod(WCR, WCO)
          NetMatrix <- crossprod(CRCO, CRCO)
        }
      )
    }
    
    if (analysis == "co-occurrences") {
      switch(
        network,
        authors = {
          WA <- cocMatrix(M, Field = "AU", type = "sparse", n, sep)
        },
        keywords = {
          WA <- cocMatrix(M, Field = "ID", type = "sparse", n, sep)
        },
        author_keywords = {
          WA <- cocMatrix(M, Field = "DE", type = "sparse", n, sep)
        },
        titles = {
          WA <- cocMatrix(M, Field = "TI_TM", type = "sparse", n, sep)
        },
        abstracts = {
          WA <- cocMatrix(M, Field = "AB_TM", type = "sparse", n, sep)
        },
        sources = {
          WA <- cocMatrix(M, Field = "SO", type = "sparse", n, sep)
        }
      )
      NetMatrix <- crossprod(WA, WA)
      
    }
    
    
    
    if (analysis == "co-citation") {
      switch(
        network,
        authors = {
          WA <- cocMatrix(M, Field = "CR_AU", type = "sparse", n, sep)
        },
        references = {
          WA <- cocMatrix(M, Field = "CR", type = "sparse", n, sep)
        },
        sources = {
          WA <- cocMatrix(M, Field = "CR_SO", type = "sparse", n, sep)
        }
      )
      NetMatrix <- crossprod(WA, WA)
      
    }
    
    if (analysis == "collaboration") {
      switch(
        network,
        authors = {
          WA <- cocMatrix(M, Field = "AU", type = "sparse", n, sep)
          
        },
        universities = {
          WA <- cocMatrix(M, Field = "AU_UN", type = "sparse", n, sep)
          
        },
        countries = {
          WA <- cocMatrix(M, Field = "AU_CO", type = "sparse", n, sep)
        }
      )
      NetMatrix <- crossprod(WA, WA)
      
    }
    
    
    # delete empty vertices
    NetMatrix <- NetMatrix[nchar(colnames(NetMatrix)) != 0, nchar(colnames(NetMatrix)) != 0]
    
    # short label for scopus references
    if (network == "references") {
      ind <- which(regexpr("[A-Za-z]", substr(colnames(NetMatrix), 1, 1)) == 1)
      NetMatrix <- NetMatrix[ind, ind]
      if (isTRUE(shortlabel)) {
        LABEL <- labelShort(NetMatrix, db = tolower(M$DB[1]))
        LABEL <- removeDuplicatedlabels(LABEL)
        colnames(NetMatrix) <- rownames(NetMatrix) <-  LABEL
      }
      
    }
    # if (analysis != "coupling") {
    #   attr(NetMatrix, "PY") <- attr(WA, "PY")
    # }
    return(NetMatrix)
  }

### shortlabel
labelShort <- function(NET,db="isi"){
  LABEL <- colnames(NET)
  YEAR <- suppressWarnings(as.numeric(sub('.*(\\d{4}).*', '\\1', LABEL)))
  YEAR[is.na(YEAR)] <- ""
  switch(db,
         isi={
           AU <- strsplit(LABEL," ")
           AU <- unlist(lapply(AU, function(l){paste(l[1]," ",l[2],sep="")}))
           LABEL <- paste0(AU, " ", YEAR, sep="")
         },
         scopus={
           AU <- strsplit(LABEL,"\\. ")
           AU <- unlist(lapply(AU, function(l){l[1]}))
           LABEL <- paste0(AU, ". ", YEAR, sep="")
         })

  return(LABEL)
}

removeDuplicatedlabels <- function(LABEL){
  ## assign an unique name to each label
  tab <- sort(table(LABEL),decreasing=T)
  dup <- names(tab[tab>1])
  for (i in 1:length(dup)){
    ind <- which(LABEL %in% dup[i])
    if (length(ind)>0){
      LABEL[ind] <- paste0(LABEL[ind],"-",as.character(1:length(ind)),sep="")
    }
  }
  return(LABEL)
}
  
