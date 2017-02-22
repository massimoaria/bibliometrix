#' Merging of Bibliographic data frames  
#'
#' Merge two bibliographic data frames.
#' 
#' A bibliographic data frame is obtained by the converting function \code{\link{convert2df}}. 
#' It is a data matrix with cases corresponding to manuscripts and variables to Field Tag in the original SCOPUS and Thomson Reuters' ISI Web of Knowledge file.
#' The function merges two bibliographic data frames deleting duplicate manuscripts. 
#' Duplicate entries are identified through the generalized Levenshtein (edit) distance. 
#' Two manuscripts that have a similarity greater than \code{tol} argument are stored in the merged data frame only once. 
#'
#' @param M1 is the first bibliographic data frame.
#' @param M2 is the second bibliographic data frame.
#' @param Field is a character object. It indicates one of the field tags used to match the two data frames. Field can be equal to one of this tags: TI (title), AB (abstract), UT (manuscript ID).
#' @param tol is a numeric value giving the minimum relative similarity to marge two manuscripts. Default value is \code{tol = 0.90}. 
#' @return the value returned from \code{dfMerge} is a data frame containing the merged data frames.
#'
#'
#' @examples
#'  
#' data(scientometrics)
#' 
#' M1=scientometrics[1:20,]
#' 
#' M2=scientometrics[10:30,]
#' 
#' mergedM <- dfMerge(M1, M2, Field = "TI", tol = 0.95)
#'
#' dim(mergedM)
#'
#'
#' @seealso \code{\link{convert2df}} to import and convert an ISI or SCOPUS Export file in a bibliographic data frame.
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis.
#' @seealso \code{\link{summary}} to obtain a summary of the results.
#' @seealso \code{\link{plot}} to draw some useful plots of the results.
#'
#' @export
dfMerge <- function(M1,M2,Field="TI",tol=0.90){
  
  a=M1[[Field]]
  b=M2[[Field]]
  an=nchar(a)
  bn=nchar(b)
  A=matrix(an,length(an),length(bn))
  B=matrix(bn,length(an),length(bn),byrow=TRUE)
  C=A
  C[B>A]=B[B>A]
  D=adist(a,b)
  Dn=1-(D/C)
  ind=which(Dn>=tol,arr.ind = TRUE)
  M=rbind(M1,M2[-ind[,2],])
  return(M)
  
}