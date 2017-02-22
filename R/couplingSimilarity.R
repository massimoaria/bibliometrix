#' Coupling similarity index
#'
#' It calculates a relative measure of bibliographic coupling.
#'
#' \code{couplingSimilarity} calculates Jaccard or Salton similarity from a coupling bibliographic matrix.
#' 
#' The Jaccard index (or Jaccard similarity coefficient) gives us a relative measure of the overlap of two sets. 
#' It is calcultated as the ratio between the intersection and the union of the reference lists (of two manuscripts).
#' The Salton index, instead, relates the intersection of the two lists to the geometric mean of the size of both sets.
#' 
#' Both indices are equal to zero if the intersection of the reference lists is empty; 
#' its reach a maximum of one if both lists are identical. 
#' 
#' @param NetMatrix is a coupling matrix obtained by the network functions \code{\link{biblioNetwork}} or \code{\link{cocMatrix}}.
#' @param type is a character. It can be "jaccard" or "salton" to obtain Jaccard or Salton similarity index respectively. The default is \code{type = "jaccard"}.
#' @return a similarity matrix.
#'
#' 
#'
#' @examples
#' 
#' data(scientometrics)
#' NetMatrix <- biblioNetwork(scientometrics, analysis = "coupling", network = "references", sep = ";")
#' S=couplingSimilarity(NetMatrix, type = "jaccard")
#'
#' @seealso \code{\link{biblioNetwork}} function to compute a bibliographic network.
#' @seealso \code{\link{cocMatrix}} to compute a bibliographic bipartite network.
#'
#' @export

couplingSimilarity <- function(NetMatrix, type = "jaccard"){
  
  diag <- Matrix::diag
  D=diag(NetMatrix)
  
  if (type=="jaccard"){denom=outer(D,D,"+")-NetMatrix}
  else if (type=="salton"){denom=sqrt(outer(D,D,"*"))}
  else {cat("\ntype argument is incorrect!\n"); cat("\nIt can be 'jaccard' or 'salton'");return()}
  S=NetMatrix/denom
  S=as.matrix(S)
  S[is.nan(S)]=0
  S=Matrix(S)
    
  return(S)
  
}