#' Creating Bibliographic networks
#'
#' \code{biblioNetwork} creates different bibliographic networks from a bibliographic data frame.
#'
#' The function \code{\link{biblioNetwork}} can create a collection of bibliographic networks following the approach proposed by Batagely and Cerinsek (2013).\cr\cr
#' Typical networks output of \code{biblioNetwork} are:\cr\cr
#' #### Collaboration Networks ############\cr
#' -- Authors collaboration (analysis = "collaboration", network = "authors")\cr
#' -- University collaboration (analysis = "collaboration", network = universities")\cr
#' -- Country collabortion (analysis = "collaboration", network = "countries")\cr\cr
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
#' @param M is a bibliographic data frame obtained by the converting function
#'   \code{\link{convert2df}}. It is a data matrix with cases corresponding to
#'   manuscripts and variables to Field Tag in the original SCOPUS and Thomson Reuters' ISI Web of Knowledge file.
#' @param analysis is a character object. It indicates the type of analysis have to be performed.
#'   \code{analysis} argument can be \code{"collaboration"}, \code{"coupling"}, \code{"co-occurrences"}  or \code{"co-citation"}.
#'   Default is \code{analysis = "coupling"}.
#' @param network is a character object. It indicates the network typology. The \code{network} aurgument can be
#' \code{"authors"}, \code{"references"}, \code{"sources"}, \code{"countries"},\code{"keywords"}, \code{"author_keywords"}, \code{"titles"}, or \code{"abstracts"}.
#' Default is \code{network = "authors"}.
#' @param sep is the field separator character. This character separates strings in each column of the data frame. The default is \code{sep = ";"}.
#' @return It is a squared network matrix. It is an object of class \code{dgMatrix} of the package \code{\link{Matrix}}.
#' @examples
#' # EXAMPLE 1: Authors collaboration network
#'
#' data(scientometrics)
#'
#' NetMatrix <- biblioNetwork(scientometrics, analysis = "collaboration", 
#' network = "authors", sep = ";")
#' 
#' net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Collaboration",labelsize=0.5) 
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

biblioNetwork <- function(M, analysis = "coupling", network = "authors", sep = ";"){
  
  crossprod <- Matrix::crossprod
  NetMatrix=NA
  if (analysis=="coupling"){
  switch(network,
      authors={
      WA=cocMatrix(M, Field="AU", type = "sparse", sep)
      WCR=cocMatrix(M, Field="CR", type = "sparse", sep)
      CRA = crossprod(WCR, WA)
      NetMatrix = crossprod(CRA, CRA)
      },
      references={
        WCR=Matrix::t(cocMatrix(M, Field="CR", type = "sparse", sep))
        NetMatrix = crossprod(WCR, WCR)
      },
    sources={
      WSO=cocMatrix(M, Field="SO", type = "sparse", sep)
      WCR=cocMatrix(M, Field="CR", type = "sparse", sep)
      CRSO = crossprod(WCR, WSO)
      NetMatrix = crossprod(CRSO, CRSO)
    },
    countries={
      WCO=cocMatrix(M, Field="AU_CO", type = "sparse", sep)
      WCR=cocMatrix(M, Field="CR", type = "sparse", sep)
      CRCO = crossprod(WCR, WCO)
      NetMatrix = crossprod(CRCO, CRCO)
    }
  )}
  
  if (analysis=="co-occurrences"){
    switch(network,
           authors={
             WA=cocMatrix(M, Field="AU", type = "sparse", sep)
             NetMatrix = crossprod(WA, WA)
           },
           keywords={
             WK=cocMatrix(M, Field="ID", type = "sparse", sep)
             NetMatrix = crossprod(WK,WK)
           },
           author_keywords={
             WK=cocMatrix(M, Field="DE", type = "sparse", sep)
             NetMatrix = crossprod(WK,WK)
           },
           titles={
             WK=cocMatrix(M, Field="TI_TM", type = "sparse", sep)
             NetMatrix = crossprod(WK,WK)
           },
           abstracts={
             WK=cocMatrix(M, Field="AB_TM", type = "sparse", sep)
             NetMatrix = crossprod(WK,WK)
           },
           sources={
             WSO=cocMatrix(M, Field="SO", type = "sparse", sep)
             NetMatrix = crossprod(WSO, WSO)
           }
    )}
  
  if (analysis=="co-citation"){
    switch(network,
           authors={
             WA=cocMatrix(M, Field="CR_AU", type = "sparse", sep)
             NetMatrix = crossprod(WA, WA)
             },
           references={
             WCR=cocMatrix(M, Field="CR", type = "sparse", sep)
             NetMatrix = crossprod(WCR, WCR)
             ### reduce name length
             A=row.names(NetMatrix)
             ind=unlist(regexec("*V[0-9]", A))
             A[ind>-1]=substr(A[ind>-1],1,(ind[ind>-1]-1))
             ind=unlist(regexec("*DOI ", A))
             A[ind>-1]=substr(A[ind>-1],1,(ind[ind>-1]-1))
             row.names(NetMatrix)=A
             colnames(NetMatrix)=A
             ###
           },
           sources={
             WSO=cocMatrix(M, Field="CR_SO", type = "sparse", sep)
             NetMatrix = crossprod(WSO, WSO)
           }
    )}
  if (analysis=="collaboration"){
    switch(network,
           authors={
             WA=cocMatrix(M, Field="AU", type = "sparse", sep)
             NetMatrix = crossprod(WA, WA)
             },
           universities={
             WUN=cocMatrix(M, Field="AU_UN", type = "sparse", sep)
             NetMatrix = crossprod(WUN, WUN)
             },
           countries={
             WCO=cocMatrix(M, Field="AU_CO", type = "sparse", sep)
             NetMatrix = crossprod(WCO, WCO)
           })
  }
  # delete empty vertices
  NetMatrix=NetMatrix[nchar(colnames(NetMatrix))!=0,nchar(colnames(NetMatrix))!=0]
  
  return(NetMatrix)
}
