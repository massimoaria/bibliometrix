#' Plotting bibliometric analysis results
#'
#' \code{plot} method for class '\code{bibliometrix}'
#' @param x is the object for which plots are desired.
#' @param ... can accept two arguments:\cr 
#' \code{k} is an integer, used for plot formatting (number of objects). Default value is 10.\cr
#' \code{pause} is a logical, used to allow pause in screen scrolling of results. Default value is \code{pause = FALSE}.
#' @return none. The function \code{plot} returns a set of plots of the object of class \code{bibliometrix}.
#'
#' @examples
#' data(scientometrics)
#'
#' results <- biblioAnalysis(scientometrics)
#'
#' plot(results, k = 10, pause = FALSE)
#'
#' @seealso The bibliometric analysis function \code{\link{biblioAnalysis}}.
#' @seealso \code{\link{summary}} to compute a list of summary statistics of the object of class \code{bibliometrix}.
#' 
#' @method plot bibliometrix
#' @export


plot.bibliometrix<-function(x, ...){

  if (class(x)!="bibliometrix"){cat('\n argument "x" have to be an object of class "bibliometrix"\n');return(NA)}
  
  arguments <- list(...)
  if (sum(names(arguments)=="k")==0){k=10} else {k=arguments$k}
  if (sum(names(arguments)=="pause")==0){pause=FALSE} else {pause=arguments$pause}
  
  if (pause == TRUE){
    cat("Hit <Return> to see next plot: ")
    line <- readline()}

  # Authors
  barplot(x$Authors[1:k],horiz=TRUE,las=2,cex.names=0.5,main="Most Productive Authors",xlab="Articles")

  if (pause == TRUE){
    cat("Hit <Return> to see next plot: ")
    line <- readline()}

  # Countries
  barplot(sort(x$Countries,decreasing=TRUE)[1:k],horiz=TRUE,las=2,cex.names=0.6,main="Most Productive Countries",xlab="Articles")

  if (pause == TRUE){
    cat("Hit <Return> to see next plot: ")
    line <- readline()}

    # Articles per Year
  Y=table(x$Years)
  Ym=table(x$Years[x$nAUperPaper>1])
  plot(as.numeric(row.names(Y)),Y,type="l",main="Articles per Year",xlab="Year",ylab="N.Articles",col="blue")
  lines(as.numeric(row.names(Ym)),Ym,col="red")
  legend(x="topleft",c("Total Articles","Multi Auth. Articles"),col=c("blue","red"),lty = c(1, 1),cex=0.6,bty="n")

  if (pause == TRUE){
    cat("Hit <Return> to see next plot: ")
    line <- readline()}

  # Total Citation Plot
  TCY=aggregate(x$TotalCitation,list(x$Years),"sum")
  plot(as.numeric(row.names(Y)),TCY[,2]/Y,type="l",main="Average Citations per Article",xlab="Year",ylab="Average Citations",col="blue")
  lines(as.numeric(row.names(Y)),Y,col="red")
  legend(x="topright",c("Average Citations","N. of Articles"),col=c("blue","red"),lty = c(1, 1),cex=0.6,bty="n")

}
