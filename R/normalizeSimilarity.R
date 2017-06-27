#' Calculate similarity indices
#'
#' It calculates a relative measure of bibliographic co-occurrences.
#'
#' \code{couplingSimilarity} calculates Association strength, Inclusion, Jaccard or Salton similarity from a co-occurrence bibliographic matrix.
#' 
#' The association strength is used by Van Eck and Waltman (2007) and Van Eck et al. (2006). Several works refer to the measure as the proximity index, 
#' while Leydesdorff (2008)and Zitt et al. (2000) refer to it as the probabilistic affinity (or activity) index. 
#'  
#' The inclusion index, also called Simpson coefficient, is an overlap measure used in information retrieval.
#' 
#' The Jaccard index (or Jaccard similarity coefficient) gives us a relative measure of the overlap of two sets. 
#' It is calcultated as the ratio between the intersection and the union of the reference lists (of two manuscripts).
#' 
#' The Salton index, instead, relates the intersection of the two lists to the geometric mean of the size of both sets.
#' The square of Salton index is also called Equivalence index.
#' 
#' The indices are equal to zero if the intersection of the reference lists is empty.
#' 
#' @param NetMatrix is a coupling matrix obtained by the network functions \code{\link{biblioNetwork}} or \code{\link{cocMatrix}}.
#' @param type is a character. It can be "association", "jaccard", "inclusion","salton" or "equivalence" to obtain Association Strength, Jaccard, 
#' Inclusion, Salton or Equivalence similarity index respectively. The default is \code{type = "association"}.
#' @return a similarity matrix.
#'
#' 
#'
#' @examples
#' 
#' data(scientometrics)
#' NetMatrix <- biblioNetwork(scientometrics, analysis = "co-occurrences", 
#'               network = "keywords", sep = ";")
#' S=normalizeSimilarity(NetMatrix, type = "association")
#'
#' @seealso \code{\link{biblioNetwork}} function to compute a bibliographic network.
#' @seealso \code{\link{cocMatrix}} to compute a bibliographic bipartite network.
#'
#' @export

normalizeSimilarity <- function(NetMatrix, type = "association"){
  
  diag <- Matrix::diag
  D=diag(NetMatrix)
  
  switch(type,
         association={S=NetMatrix/(outer(D,D,"*"))},
         inclusion={S=NetMatrix/outer(D,D, function(a,b){mapply(min,a,b)})},
         jaccard={S=NetMatrix/(outer(D,D,"+")-NetMatrix)},
         salton={S=NetMatrix/sqrt(outer(D,D,"*"))},
         equivalence={S=(NetMatrix/sqrt(outer(D,D,"*")))^2})
  
  S=as.matrix(S)
  S[is.nan(S)]=0
  S=Matrix(S)
    
  return(S)
  
}
