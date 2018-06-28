#' Summarizing bibliometric analysis results
#'
#' \code{summary} method for class '\code{bibliometrix}'
#' @param object is the object for which a summary is desired.
#' @param ... can accept two arguments:\cr
#' \code{k} integer, used for table formatting (number of rows). Default value is 10.\cr
#' \code{pause} locical, used to allow pause in screen scrolling of results. Default value is \code{pause = FALSE}.\cr
#' \code{width} integer, used to define screen output width. Default value is \code{width = 120}.
#' @return The function \code{summary} computes and returns a list of summary statistics of the object of class \code{bibliometrics}.
#'
#' the list contains the following objects:
#' \tabular{lll}{
#' \code{MainInformation}   \tab   \tab Main Information about Data\cr
#' \code{AnnualProduction}  \tab   \tab Annual Scientific Production\cr
#' \code{AnnualGrowthRate}  \tab   \tab Annual Percentage Growth Rate\cr
#' \code{MostProdAuthors}   \tab   \tab Most Productive Authors\cr
#' \code{MostCitedPapers}   \tab   \tab Top manuscripts per number of citations\cr
#' \code{MostProdCountries} \tab   \tab Most Productive Countries\cr
#' \code{TCperCountries}    \tab   \tab Total Citation per Countries\cr
#' \code{MostRelSources}    \tab   \tab Most Relevant Sources\cr
#' \code{MostRelKeywords}   \tab   \tab Most Relevant Keywords}
#'
#'
#' @examples
#' data(scientometrics)
#' 
#' results <- biblioAnalysis(scientometrics)
#'
#' summary(results)
#'
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis
#' @seealso \code{\link{plot}} to draw some useful plots of the results.
#' 
#' @method summary bibliometrix
#' @export

summary.bibliometrix<-function(object, ...){

  if (class(object)!="bibliometrix"){cat('\n argument "object" have to be an object of class "bibliometrix"\n');return(NA)}
  
  options(width=130)
  
  arguments <- list(...)
  if (sum(names(arguments)=="k")==0){k=10} else {k=arguments$k}
  if (sum(names(arguments)=="pause")==0){pause=FALSE} else {pause=arguments$pause}
  if (sum(names(arguments)=="width")==0){options(width=120)} else {options(width=arguments$width)}
  
  Co=NULL
  AC=NULL
  
  #Main Information about data
  MainInfo=("\n\nMain Information about data\n\n")
  MainInfo[2]=paste("Documents                            ",object$Articles,"\n")
  MainInfo[3]=paste("Sources (Journals, Books, etc.)      ",length(object$Sources),"\n")
  MainInfo[4]=paste("Keywords Plus (ID)                   ",length(object$ID),"\n")
  MainInfo[5]=paste("Author's Keywords (DE)               ",length(object$DE),"\n")
  MainInfo[6]=paste("Period                               ",min(object$Years,na.rm=T),"-",max(object$Years,na.rm=T),"\n")
  TCm=format(mean(as.numeric(object$TotalCitation), na.rm=TRUE),digits=4)
  MainInfo[7]=paste("Average citations per documents      ",TCm,"\n\n")
  MainInfo[8]=paste("Authors                              ",object$nAuthors,"\n")
  MainInfo[9]=paste("Author Appearances                   ",object$Appearances,"\n")
  MainInfo[10]=paste("Authors of single authored documents ",object$nAuthors-object$AuMultiAuthoredArt,"\n")
  MainInfo[11]=paste("Authors of multi authored documents  ",object$AuMultiAuthoredArt,"\n\n")
  MainInfo[12]=paste("Documents per Author                 ",format(object$Articles/object$nAuthors,digits=3),"\n")
  MainInfo[13]=paste("Authors per Document                 ",format(object$nAuthors/object$Articles,digits=3),"\n")
  MainInfo[14]=paste("Co-Authors per Documents             ",format(mean(object$nAUperPaper),digits=3),"\n")
  CollIndex=format(object$AuMultiAuthoredArt/sum(object$nAUperPaper>1),digits=3)  # Collaboration Index
  MainInfo[15]=paste("Collaboration Index                  ",CollIndex,"\n")
  MainInfo[16]=paste("\n")
  if (!is.na(object$Documents[1])){
  MainInfo[17]=paste("Document types                    ","\n")
  for (i in 1:length(object$Documents)){
  MainInfo[length(MainInfo)+1]=paste(names(object$Documents)[i], "                                   ",as.numeric(object$Documents)[i],"\n")  
  }
  MainInfo[length(MainInfo)+1]=paste("\n")}
  cat(MainInfo)

  if (pause==TRUE){
  cat("Hit <Return> to see next table: ")
  line <- readline()}

  cat("\nAnnual Scientific Production\n\n")
  Y=data.frame(table(object$Years))
  names(Y)=c("Year   ", "Articles")
  print(Y,row.names=FALSE);cat("\n")
  ny=dim(Y)[1]
  GR=((Y[ny,2]/Y[1,2])^(1/(ny-1))-1)*100
  cat("Annual Percentage Growth Rate",GR,"\n\n")


  if (pause==TRUE){
    cat("Hit <Return> to see next table: ")
    line <- readline()}

  # Most Productive Authors
  cat("\nMost Productive Authors\n\n")
  A=data.frame(cbind(object$Authors[1:k]))
  A$MPA=row.names(A);A=A[,c(2,1)]
  A[,3:4]=object$AuthorsFrac[1:k,]
  names(A)=c("Authors       ", "Articles", "Authors       ","Articles Fractionalized")
  A=format(A,justify="left",digits=3)
  row.names(A)=1:k
  print(A,row.names=TRUE);cat("\n")
  
  if (pause==TRUE){
    cat("Hit <Return> to see next table: ")
    line <- readline()}

  # Most Cited Manuscipts
  cat("\nTop manuscripts per citations\n\n")
  MostCitedPapers=object$MostCitedPapers[1:k,]
  MostCitedPapers=format(MostCitedPapers,justify="left",digits=3)
  row.names(MostCitedPapers)=1:k
  print(MostCitedPapers,row.names=TRUE);cat("\n")

  if (pause==TRUE){
    cat("Hit <Return> to see next table: ")
    line <- readline()}

  kk=k
  if (!is.null(object$Countries)){
  # Most Productive Countries
  cat("\nMost Productive Countries (of corresponding authors)\n\n")

  if (length(object$Countries)<k) {kk=length(object$Countries)}

  object$Countries=as.array(object$Countries)
  Co=data.frame(object$Countries[1:kk])
  Co$Country=row.names(Co)
  names(Co)=c("Country  ","Articles","Freq")
  Co$Freq=as.numeric(Co[,2])/sum(object$Countries)
  Co=cbind(Co,object$CountryCollaboration[1:kk,2:3])
  Co$MCP_Ratio=Co$MCP/Co$Articles
  #names(Co)=c("Country  ","Articles","Frequency")
  Co=format(Co,justify="left",digits=3)
  row.names(Co)=1:kk
  print(Co,row.names=TRUE);cat("\n")
  cat("\nSCP: Single Country Publications\n\nMCP: Multiple Country Publications\n\n")


  if (pause==TRUE){
    cat("Hit <Return> to see next table: ")
    line <- readline()}

  # Total Citation per Country
  cat("\nTotal Citations per Country\n\n")
  ind=which(!is.na(object$TotalCitation))
  AC=aggregate(object$TotalCitation[ind],list(object$CO[ind]),"sum")
  CC=object$Countries[sort(row.names(object$Countries))]
  CC2=intersect(AC[,1],rownames(CC))
  AC$Articles=object$Countries[CC2]
  AC=AC[order(-AC[,2]),];AC=AC[,c(1,3,2)]
  AC$TCperArticles=AC[,3]/AC[,2]
  AC=AC[,-2]
  names(AC)=c("Country     ", "Total Citations", "Average Article Citations")
  AC=format(AC,justify="left",digits=3)[1:kk,]
  row.names(AC)=1:kk
  print(AC,row.names=TRUE);cat("\n")


  if (pause==TRUE){
    cat("Hit <Return> to see next table: ")
    line <- readline()}
  }

  if (!is.null(object$Sources)){
  # Most relevant Sources
  cat("\nMost Relevant Sources\n\n")
  kk=k
  if (length(object$Sources)<k){kk=length(object$Sources)}
  AA=data.frame(cbind(object$Sources[1:kk]))
  AA$MPA=row.names(AA);AA=AA[,c(2,1)]
  names(AA)=c("Sources       ", "Articles")
  AA=format(AA,justify="left",digits=3)
  row.names(AA)=1:kk
  print(AA,row.names=TRUE);cat("\n")


  if (pause==TRUE){
    cat("Hit <Return> to see next table: ")
    line <- readline()}
  }

  if (!is.null(object$ID) & !is.null(object$DE)){
  # Most relevant Keywords
  cat("\nMost Relevant Keywords\n\n")
  AAA=data.frame(cbind(object$DE[1:k]))
  AAA$MPA=row.names(AAA);AAA=AAA[,c(2,1)]
  names(AAA)=c("DE Keywords     ", "Articles")
  A2=data.frame(cbind(object$ID[1:k]))
  A2$MPA=row.names(A2);A2=A2[,c(2,1)]
  AAA[,c(3,4)]=A2
  names(AAA)=c("Author Keywords (DE)     ", "Articles","Keywords-Plus (ID)    ", "Articles" )
  AAA=format(AAA,justify="left",digits=3)
  row.names(AAA)=1:k
  print(AAA,row.names=TRUE);cat("\n")}

  summaryresults=list(MainInformation=MainInfo,AnnualProduction=Y,AnnualGrowthRate=GR,MostProdAuthors=A,MostCitedPapers=MostCitedPapers,MostProdCountries=Co,TCperCountries=AC,MostRelSources=AA,MostRelKeywords=AAA)

  invisible(summaryresults)
  }
