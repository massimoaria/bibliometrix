#' Lotka's law coefficient estimation
#'
#' It estimates Lotka's law coefficients for scientific productivity (\cite{Lotka A.J., 1926}).\cr\cr
#' 
#' Reference:
#' Lotka, A. J. (1926). The frequency distribution of scientific productivity. Journal of the Washington academy of sciences, 16(12), 317-323.\cr
#' 
#' @param results is an object of the class '\code{bibliometrix}' for which the analysis of the authors' dominance ranking is desired.
#' @return The function \code{lotka} returns a list of summary statistics of the Lotka's law estimation of an object of class \code{bibliometrix}.
#'
#' the list contains the following objects:
#' \tabular{lll}{
#' \code{Beta}  \tab   \tab Beta coefficient\cr
#' \code{C}   \tab   \tab Constant coefficient\cr
#' \code{R2} \tab   \tab Goodness of Fit\cr
#' \code{fitted} \tab     \tab Fitted Values\cr
#' \code{p.value} \tab     \tab Pvalue of two-sample Kolmogorov-Smirnov test between the empirical and the theorical Lotka's Law distribution (with Beta=2)\cr
#' \code{AuthorProd}    \tab   \tab Authors' Productivity frequency table}
#'
#' @examples
#' data(scientometrics)
#' results <- biblioAnalysis(scientometrics)
#' L=lotka(results)
#' L
#'
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis
#' @seealso \code{\link{summary}} method for class '\code{bibliometrix}'
#' 
#' @export


lotka<-function(results){

  if (class(results)!="bibliometrix"){cat('\n argument "results" have to be an object of class "bibliometrix"\n');return(NA)}
  
  # Author Productivity (LOTKA's LAW)
  Authors=results$Authors
  AUdf=data.frame(Authors)
  AuthorProd=aggregate(AUdf,by=list(AUdf$Freq),"length")
  AuthorProd[,2]=as.numeric(AuthorProd[,2])
  AuthorProd[,3]=AuthorProd[,2]/sum(AuthorProd[,2])
  names(AuthorProd)=c("N.Articles","N.Authors","Freq")
  LOTKA=(lm(log10(Freq)~log10(N.Articles),data=AuthorProd))
  Yt=LOTKA$coeff[1]-2*log10(AuthorProd[,1])
  p=suppressWarnings(ks.test(AuthorProd[,3],10^Yt,exact=FALSE)$p.value)
  L=list(Beta=abs(as.numeric(LOTKA$coeff[2])),C=10^(as.numeric(LOTKA$coeff[1])), R2=summary(LOTKA)$r.squared,fitted=10^LOTKA$fitted.values,p.value=p,AuthorProd=AuthorProd)
  return(L)
}
