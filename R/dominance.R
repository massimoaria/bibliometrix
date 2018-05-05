#' Authors' dominance ranking
#'
#' It calculates the authors' dominance ranking from an object of the class '\code{bibliometrix}' as proposed by Kumar & Kumar, 2008.
#' @param results is an object of the class '\code{bibliometrix}' for which the analysis of the authors' dominance ranking is desired.
#' @param k is an integer, used for table formatting (number of authors). Default value is 10.
#' @return The function \code{dominance} returns a data frame with cases corresponding to the first \code{k} most productive authors and variables to typical field of a dominance analysis.
#'
#' the data frame variables are:
#' \tabular{lll}{
#' \code{Dominance Factor}  \tab   \tab Dominance Factor (DF = FAA / MAA)\cr
#' \code{Multi Authored}   \tab   \tab N. of Multi-Authored Articles (MAA)\cr
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
  
  
  Mnt=rep(NA,k)
  for (i in 1:length(FA)){
    Mnt[i]=results$Authors[FA[i] == AU]
    
  }
  Dominance=Nmf/Mnt
  
  
  t=0
  cont=0
  D=data.frame(matrix(NA,k,3))
  
  for (i in 1:length(FA)){
    if (sum(AU[i]==FA)>0){
      
      cont=cont+1
      D[cont,1]=Dominance[AU[i]==FA]
      D[cont,2]=results$Authors[i]
      D[cont,3]=Nmf[AU[i]==FA]
      
      row.names(D)[cont]=AU[i]
    }
    if (cont==k) break
  }
  
  
  D$RankbyArticles=1:dim(D)[1]
  D=D[order(-D[,1]),]
  D$RankDF=1:dim(D)[1]
  names(D)=c("Dominance Factor","Multi Authored","First Authored","Rank by Articles","Rank by DF")
  return(D)
}


