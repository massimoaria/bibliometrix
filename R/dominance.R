#' Authors' dominance ranking
#'
#' It calculates the authors' dominance ranking from an object of the class '\code{bibliometrix}' as proposed by Kumar & Kumar, 2008.
#' @param results is an object of the class '\code{bibliometrix}' for which the analysis of the authors' dominance ranking is desired.
#' @param k is an integer, used for table formatting (number of authors). Default value is 10.
#' @return The function \code{dominance} returns a data frame with cases corresponding to the first \code{k} most productive authors and variables to typical field of a dominance analysis.
#'
#' the data frame variables are:
#' \tabular{lll}{
#' \code{Author} \tab   \tab Author's name\cr
#' \code{Dominance Factor}  \tab   \tab Dominance Factor (DF = FAA / MAA)\cr
#' \code{Tot Articles}   \tab   \tab N. of Authored Articles (TAA)\cr
#' \code{Single Authored}   \tab   \tab N. of Single-Authored Articles (SAA)\cr
#' \code{Multi Authored}   \tab   \tab N. of Multi-Authored Articles (MAA=TAA-SAA)\cr
#' \code{First Authored} \tab   \tab N. of First Authored Articles (FAA)\cr
#' \code{Rank by Articles}    \tab   \tab Author Ranking by N. of Articles\cr
#' \code{Rank by DF}    \tab   \tab Author Ranking by Dominance Factor}
#'
#'
#'
#' @examples
#' data(scientometrics)
#' results <- biblioAnalysis(scientometrics)
#' DF=dominance(results)
#' DF
#'
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis
#' @seealso \code{\link{summary}} method for class '\code{bibliometrix}'
#'
#' @export

dominance<-function(results, k = 10){
  
  # Author Rank by Dominance Rank  (Kumar & Kumar, 2008)
  
  #options(warn=-1)
  
  if (class(results)!="bibliometrix"){cat('\n argument "results" have to be an object of class "bibliometrix"\n');return(NA)}
  
  Nmf=table(results$FirstAuthors[results$nAUperPaper>1])
  FA=names(Nmf)
  #FA=gsub(" ", "", FA, fixed = TRUE)  # delete spaces
  
  AU=names(results$Authors)
  
  
  Tot=Single=rep(NA,length(FA))
  for (i in 1:length(FA)){
    Single[i]=sum(results$FirstAuthors[results$nAUperPaper==1]==FA[i])
    Tot[i]=results$Authors[FA[i] == AU]
    
  }
  Dominance=Nmf/(Tot-Single)
  
  D=data.frame("Author"=FA,"Dominance Factor"=as.numeric(Dominance),"Articles"=Tot,"Single-Authored"=Single,"Multi-Authored"=Tot-Single,"First-Author"=as.numeric(Nmf))
  D=D[order(-D[,3]),]
  D=D[1:k,]
  D$RankbyArticles=rank(-D$Articles,ties.method="min")
  D=D[order(-D$Dominance.Factor),]
  D$RankDF=rank(-D$Dominance.Factor,ties.method="min")
  names(D)=c("Author","Dominance Factor","Tot Articles","Single-Authored","Multi-Authored","First-Authored","Rank by Articles","Rank by DF")
  row.names(D)=1:k
  return(D)
}


