#' Summarizing bibliometric analysis results
#'
#' \code{summary} method for class '\code{bibliometrix}'
#' @param object is the object for which a summary is desired.
#' @param ... can accept two arguments:\cr
#' \code{k} integer, used for table formatting (number of rows). Default value is 10.\cr
#' \code{pause} logical, used to allow pause in screen scrolling of results. Default value is \code{pause = FALSE}.\cr
#' \code{width} integer, used to define screen output width. Default value is \code{width = 120}.
#' \code{verbose} logical, used to allow screen output. Default is TRUE.
#' @return The function \code{summary} computes and returns a list of summary statistics of the object of class \code{bibliometrics}.
#'
#' the list contains the following objects:
#' \tabular{lll}{
#' \code{MainInformation}   \tab   \tab Main Information about Data\cr
#' \code{AnnualProduction}  \tab   \tab Annual Scientific Production\cr
#' \code{AnnualGrowthRate}  \tab   \tab Annual Percentage Growth Rate\cr
#' \code{MostProdAuthors}   \tab   \tab Most Productive Authors\cr
#' \code{MostCitedPapers}   \tab   \tab Top manuscripts per number of citations\cr
#' \code{MostProdCountries} \tab   \tab Corresponding Author's Countries\cr
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
  if (sum(names(arguments)=="verbose")==0){verbose=TRUE} else {verbose=FALSE}
  K=k
  Co=NULL
  AC=NULL
  
  TCm <- format(mean(as.numeric(object$TotalCitation), na.rm=TRUE),digits = 4)
  TCmy <- format(mean(as.numeric(object$MostCitedPapers$TCperYear), na.rm = TRUE), digits = 4) 
  CollIndex <- format(object$AuMultiAuthoredArt/sum(object$nAUperPaper>1),digits=3)  # Collaboration Index
  MYfP <- as.numeric(substr(Sys.time(),1,4))-mean(object$Years,na.rm = TRUE)
  
  #Main Information about data
  MainInfo=toupper("\n\nMain Information about data\n\n")
  MainInfo[length(MainInfo)+1]=paste("Timespan                             ",min(object$Years,na.rm=T),":",max(object$Years,na.rm=T),"\n")
  MainInfo[length(MainInfo)+1]=paste("Sources (Journals, Books, etc)       ",length(object$Sources),"\n")
  MainInfo[length(MainInfo)+1]=paste("Documents                            ",object$Articles,"\n")
  MainInfo[length(MainInfo)+1]=paste("Average years from publication       ",format(MYfP,digits = 3),"\n")
  MainInfo[length(MainInfo)+1]=paste("Average citations per documents      ",format(TCm, digits = 3),"\n")
  MainInfo[length(MainInfo)+1]=paste("Average citations per year per doc   ",format(TCmy, digits = 3),"\n")
  MainInfo[length(MainInfo)+1]=paste("References                           ",object$nReferences,"\n")
  if (!is.na(object$Documents[1])) {
    MainInfo[length(MainInfo) + 1] = toupper(paste("\nDocument types                    ", "\n"))
    for (i in 1:length(object$Documents)) {
      MainInfo[length(MainInfo) + 1] = tolower(paste(names(object$Documents)[i],as.numeric(object$Documents)[i],"\n"))
     }
  }
  MainInfo[length(MainInfo)+1]=toupper("\nDocument Contents\n")
  MainInfo[length(MainInfo)+1]=paste("Keywords Plus (ID)                   ",length(object$ID),"\n")
  MainInfo[length(MainInfo)+1]=paste("Author's Keywords (DE)               ",length(object$DE),"\n")
  MainInfo[length(MainInfo)+1]=toupper("\nAuthors\n")
  MainInfo[length(MainInfo)+1]=paste("Authors                              ",object$nAuthors,"\n")
  MainInfo[length(MainInfo)+1]=paste("Author Appearances                   ",object$Appearances,"\n")
  MainInfo[length(MainInfo)+1]=paste("Authors of single-authored documents ",object$AuSingleAuthoredArt,"\n")
  MainInfo[length(MainInfo)+1]=paste("Authors of multi-authored documents  ",object$AuMultiAuthoredArt,"\n")
  MainInfo[length(MainInfo)+1]=toupper("\nAuthors Collaboration\n")
  MainInfo[length(MainInfo)+1]=paste("Single-authored documents            ",format(sum(object$nAUperPaper==1),digits=0),"\n")
  MainInfo[length(MainInfo)+1]=paste("Documents per Author                 ",format(object$Articles/object$nAuthors,digits=3),"\n")
  MainInfo[length(MainInfo)+1]=paste("Authors per Document                 ",format(object$nAuthors/object$Articles,digits=3),"\n")
  MainInfo[length(MainInfo)+1]=paste("Co-Authors per Documents             ",format(mean(object$nAUperPaper),digits=3),"\n")
  MainInfo[length(MainInfo)+1]=paste("Collaboration Index                  ",CollIndex,"\n")
  MainInfo[length(MainInfo)+1]=paste("\n")
  
  
  ## DF table for biblioshiny
  Description <- gsub('[0-9]+', '', MainInfo)
  Description <- gsub('\\\n','',Description)
  Description <- gsub(':','',Description)
  Description <- trimws(gsub('\\.','', Description))
  Results <- gsub("[^0-9\\.:]", "", MainInfo)
  MainInfoDF=data.frame("Description"=Description,"Results"=Results, stringsAsFactors = FALSE)
  
  if (isTRUE(verbose)){cat(MainInfo)}

  if (pause==TRUE & isTRUE(verbose)){
  cat("Hit <Return> to see next table: ")
  line <- readline()}

  if (isTRUE(verbose)) cat("\nAnnual Scientific Production\n\n")
  Y=data.frame(table(object$Years))
  names(Y)=c("Year   ", "Articles")
  if (isTRUE(verbose)) {print(Y,row.names=FALSE);cat("\n")}
  #  ny=dim(Y)[1]
  ny=max(as.numeric(levels(Y[,1])),na.rm=TRUE)-min(as.numeric(levels(Y[,1])),na.rm=TRUE)
  GR=((Y[nrow(Y),2]/Y[1,2])^(1/(ny))-1)*100
  if (isTRUE(verbose)){cat("Annual Percentage Growth Rate",GR,"\n\n")}


  if (pause==TRUE & isTRUE(verbose)){
    cat("Hit <Return> to see next table: ")
    line <- readline()}

  # Most Productive Authors
  if (isTRUE(verbose)){cat("\nMost Productive Authors\n\n")}
  if (K==Inf){
    k=length(object$Authors)
  }
  A=data.frame(cbind(object$Authors[1:k]))
  A$MPA=row.names(A);A=A[,c(2,1)]
  A[,3:4]=object$AuthorsFrac[1:k,]
  names(A)=c("Authors       ", "Articles", "Authors       ","Articles Fractionalized")
  A=format(A,justify="left",digits=3)
  row.names(A)=1:k
  if (isTRUE(verbose)) {print(A,row.names=TRUE);cat("\n")}
  
  if (pause==TRUE & isTRUE(verbose)){
    cat("Hit <Return> to see next table: ")
    line <- readline()}

  # Most Cited Manuscipts
  if (isTRUE(verbose)){cat("\nTop manuscripts per citations\n\n")}
  if (K==Inf){
    k=dim(object$MostCitedPapers)[1]
  }
  MostCitedPapers=object$MostCitedPapers[1:k,]
  MostCitedPapers=format(MostCitedPapers,justify="left",digits=3)
  row.names(MostCitedPapers)=1:k
  if (isTRUE(verbose)){print(MostCitedPapers,row.names=TRUE);cat("\n")}

  if (pause==TRUE & isTRUE(verbose)){
    cat("Hit <Return> to see next table: ")
    line <- readline()}
  if (K==Inf){
    k=length(object$Countries)
  }
  kk=k
  if (!is.null(object$Countries)){
  # Most Productive Countries
    if (isTRUE(verbose)){cat("\nCorresponding Author's Countries\n\n")}

  if (length(object$Countries)<k) {kk=length(object$Countries)}

  Co=data.frame(Country=names(object$Countries[1:kk]), Articles=as.numeric(object$Countries)[1:kk],Freq=0)
  Co$Freq=as.numeric(Co[,2])/sum(object$Countries)
  Co=cbind(Co,object$CountryCollaboration[1:kk,2:3])
  Co$MCP_Ratio=Co$MCP/Co$Articles
  Co=format(Co,justify="left",digits=3)
  row.names(Co)=1:kk
  if (isTRUE(verbose)){print(Co,row.names=TRUE);cat("\n")}
  if (isTRUE(verbose)){cat("\nSCP: Single Country Publications\n\nMCP: Multiple Country Publications\n\n")}


  if (pause==TRUE & isTRUE(verbose)){
    cat("Hit <Return> to see next table: ")
    line <- readline()}

  # Total Citation per Country
  if (isTRUE(verbose)){cat("\nTotal Citations per Country\n\n")}

  Co2=data.frame(Country=object$CO,TotalCitation=object$TotalCitation)
  Co2=Co2[!is.na(Co2[,1]),]
  AC=Co2 %>% group_by(.data$Country) %>% 
    summarise("TC"=sum(.data$TotalCitation),"Average Article Citations"=sum(.data$TotalCitation)/length(.data$TotalCitation)) %>%
    arrange(-.data$TC) %>% as.data.frame(.data,stringasfactor=FALSE)
  
  names(AC)=c("Country     ", "Total Citations", "Average Article Citations")
  AC=format(AC,justify="left",digits=3)[1:kk,]
  row.names(AC)=1:kk
  if (isTRUE(verbose)){print(AC,row.names=TRUE);cat("\n")}


  if (pause==TRUE & isTRUE(verbose)){
    cat("Hit <Return> to see next table: ")
    line <- readline()}
  }

  if (!is.null(object$Sources)){
  # Most relevant Sources
    if (isTRUE(verbose)){cat("\nMost Relevant Sources\n\n")}
    if (K==Inf){
      k=length(object$Sources)
    }
  kk=k
  if (length(object$Sources)<k){kk=length(object$Sources)}
  AA=data.frame(cbind(object$Sources[1:kk]))
  AA$MPA=row.names(AA);AA=AA[,c(2,1)]
  names(AA)=c("Sources       ", "Articles")
  AA=format(AA,justify="left",digits=3)
  row.names(AA)=1:kk
  if (isTRUE(verbose)){print(AA,row.names=TRUE);cat("\n")}


  if (pause==TRUE & isTRUE(verbose)){
    cat("Hit <Return> to see next table: ")
    line <- readline()}
  }

  if (!is.null(object$ID) & !is.null(object$DE)){
  # Most relevant Keywords
    if (isTRUE(verbose)){cat("\nMost Relevant Keywords\n\n")}
    if (K==Inf){
      k=min(c(length(object$DE),length(object$ID)))
    }
  AAA=data.frame(cbind(object$DE[1:k]))
  AAA$MPA=row.names(AAA);AAA=AAA[,c(2,1)]
  names(AAA)=c("DE Keywords     ", "Articles")
  A2=data.frame(cbind(object$ID[1:k]))
  A2$MPA=row.names(A2);A2=A2[,c(2,1)]
  AAA[,c(3,4)]=A2
  names(AAA)=c("Author Keywords (DE)     ", "Articles","Keywords-Plus (ID)    ", "Articles" )
  AAA=format(AAA,justify="left",digits=3)
  row.names(AAA)=1:k
  if (isTRUE(verbose)){print(AAA,row.names=TRUE);cat("\n")}
  } else (AAA=NULL)

  summaryresults=list(MainInformation=MainInfo,MainInformationDF=MainInfoDF, AnnualProduction=Y,AnnualGrowthRate=GR,MostProdAuthors=A,MostCitedPapers=MostCitedPapers,MostProdCountries=Co,TCperCountries=AC,MostRelSources=AA,MostRelKeywords=AAA)

  invisible(summaryresults)
  }
