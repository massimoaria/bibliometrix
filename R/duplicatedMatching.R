#' Searching of duplicated records in a bibliographic database  
#'
#' Search duplicated records in a dataframe.
#' 
#' A bibliographic data frame is obtained by the converting function \code{\link{convert2df}}. 
#' It is a data matrix with cases corresponding to manuscripts and variables to Field Tag in the original SCOPUS and Clarivate Analytics WoS file.
#' The function identifies duplicated records in a bibliographic data frame and deletes them. 
#' Duplicate entries are identified through the restricted Damerau-Levenshtein distance. 
#' Two manuscripts that have a relative similarity measure greater than \code{tol} argument are stored in the output data frame only once. 
#'
#' @param M is the bibliographic data frame.
#' @param Field is a character object. It indicates one of the field tags used to identify duplicated records. Field can be equal to one of these tags: TI (title), AB (abstract), UT (manuscript ID).
#' @param exact is logical. If exact = TRUE the function searches duplicates using exact matching. If exact=FALSE, 
#' the function uses the restricted Damerau-Levenshtein distance to find duplicated documents.
#' @param tol is a numeric value giving the minimum relative similarity to match two manuscripts. Default value is \code{tol = 0.95}. 
#' To use the restricted Damerau-Levenshtein distance, exact argument has to be set as FALSE.
#' @return the value returned from \code{duplicatedMatching} is a data frame without duplicated records.
#'
#'
#' @examples
#'  
#' data(scientometrics)
#' 
#' M=rbind(scientometrics[1:20,],scientometrics[10:30,])
#' 
#' newM <- duplicatedMatching(M, Field = "TI", exact=FALSE, tol = 0.95)
#'
#' dim(newM)
#'
#'
#' @seealso \code{\link{convert2df}} to import and convert an WoS or SCOPUS Export file in a bibliographic data frame.
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis.
#' @seealso \code{\link{summary}} to obtain a summary of the results.
#' @seealso \code{\link{plot}} to draw some useful plots of the results.
#'
#' @export
duplicatedMatching <- function(M, Field="TI", exact=FALSE, tol=0.95){
  
  if (!(Field %in% names(M))){
    cat("\nField",Field,"is not a valid column name of your bibliographic data frame\n ")
    return(M)
  }
  if (isTRUE(exact)){
    exacgt="true"
  }else{exact="false"}
  switch(exact,
         true={
           M <- M[!duplicated(M[Field]),]
         },
         false={
           a=b=M[[Field]]
           an=nchar(a)
           A=matrix(an,length(an),length(an))
           A[is.na(A)]=0
           B=t(A)
           C=A
           C[B>A]=B[B>A]
           D=as.matrix(stringdistmatrix(a))
           Dn=1-(D/C)
           Dn[Dn>tol]=2
           M=M[!duplicated(Dn),]
         })
  
  return(M)
  
}