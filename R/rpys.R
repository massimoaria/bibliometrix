##' Reference Publication Year Spectroscopy
#'
#' \code{rpys} computes a Reference Publication Year Spectroscopy for detecting 
#' the Historical Roots of Research Fields.
#' The method was introduced by Marx et al., 2014.\cr\cr
#' 
#' Reference:\cr
#' Marx, W., Bornmann, L., Barth, A., & Leydesdorff, L. (2014). 
#' Detecting the historical roots of research fields by reference publication 
#' year spectroscopy (RPYS). Journal of the Association for Information Science and Technology, 
#' 65(4), 751-764.\cr\cr
#' 
#' @param M is a data frame obtained by the converting function
#'   \code{\link{convert2df}}. It is a data matrix with cases corresponding to
#'   articles and variables to Field Tag in the original ISI or SCOPUS file.
#' @param sep is the cited-references separator character. This character separates cited-references in the CR
#' column of the data frame. The default is \code{sep = ";"}.
#' @param timespan is a numeric vector c(min year,max year). The default value is NULL (the entire timespan is considered).
#' @param graph is a logical. If TRUE the function plot the spectroscopy otherwise the plot is created but not drawn down.
#' @return a list containing the spectroscopy (class ggplot2) and two dataframes with the number of citations
#' per year and the list of the cited references for each year, respectively.
#'  
#'
#' @examples
#' 
#'
#' data(scientometrics)
#' res <- rpys(scientometrics, sep=";", graph = TRUE)
#'
#' @seealso \code{\link{convert2df}} to import and convert an ISI or SCOPUS
#'   Export file in a data frame.
#' @seealso \code{\link{biblioAnalysis}} to perform a bibliometric analysis.
#' @seealso \code{\link{biblioNetwork}} to compute a bibliographic network.
#' @export

rpys <- function(M, sep=";", timespan=NULL, graph=T){

  
  M$CR<-gsub("DOI;","DOI ", as.character(M$CR))
  
  Fi<-strsplit(M[,"CR"],sep)
  Fi<-lapply(Fi,trim.leading)
  Fi<-lapply(Fi,function(l) l<-l[nchar(l)>10])
  Fi<-(unlist(Fi))
  
  ## reference modify for ISI
  if (M$DB[1]=="ISI"){
  ref<-unlist(lapply(Fi, function(l){
      l<-gsub("\\).*",")",l)
      l<-gsub(","," ",l)
      l<-gsub(";"," ",l)
      l<-gsub("\\s+", " ", l)
      l<-l[nchar(l)>0]
      return(l)
    }))
  }else{
    ref<-unlist(lapply(Fi, function(l){
      l<-gsub(","," ",l)
      l<-gsub(";"," ",l)
      l<-gsub("\\s+", " ", l)
      l<-l[nchar(l)>0]
      return(l)
    }))
  }
  

Years=yearExtract(ref,db=M$DB[1])
Years=Years[!is.na(ref)]
ref=ref[!is.na(ref)] 

ref=ref[Years>=1700 & Years<=as.numeric(substr(Sys.Date(),1,4))]
Years=Years[Years>=1700 & Years<=as.numeric(substr(Sys.Date(),1,4))]

CR=data.frame(Year=Years,Reference=ref, stringsAsFactors = FALSE)

CR=dplyr::group_by(CR, .data$Year, .data$Reference) %>% dplyr::summarise(Freq = length(.data$Reference))

Years=Years[!(Years %in% "")]

RPYS=table(Years)

## calculating running median
yearSeq=as.numeric(names(RPYS))
X=seq(min(yearSeq),max(yearSeq))
Y=rep(0,length(X))
names(Y)=X
Y[names(Y) %in% names(RPYS)]=RPYS


YY=c(rep(0,4),Y)
Median=rep(0,length(Y))
for (i in 5:length(YY)){
  Median[i-4]=median(YY[(i-4):i])
}
####
#Median=runmed(Y,5)
diffMedian=Y-Median



if (length(timespan)==2){
  indTime=which(X>=timespan[1] & X<=timespan[2])
  X=X[indTime]
  Y=Y[indTime]
  diffMedian=diffMedian[indTime]
}
df=data.frame(X=X,Y=Y,diffMedian=diffMedian,stringsAsFactors = FALSE)
RPYS=data.frame(Year=names(Y),Citations=Y,diffMedian5=diffMedian)
g=ggplot(df, aes(x=.data$X,y=.data$Y,text=paste("Year: ",.data$X,"\nN. of References: ",.data$Y)))+
  geom_line(aes(group="NA")) +
  geom_area(aes(group="NA"),fill = '#002F80', alpha = .5) +
  geom_hline(aes(yintercept=0, color = 'grey'))+
  geom_line(aes(x=.data$X,y=.data$diffMedian, color="firebrick", group="NA"))+
  labs(x = 'Year'
       , y = 'Cited References'
       , title = "Reference Publication Year Spectroscopy",
       caption = "Number of Cited References (black line) - Deviation from the 5-Year Median (red line)") +
  scale_x_continuous(breaks= (df$X[seq(1,length(df$X),by=round(length(df$X)/30))])) +
  theme(text = element_text(color = "#444444"), legend.position="none"
        ,plot.caption = element_text(size = 9, hjust = 0.5,
                                     color = "black", face = "bold")
        ,panel.background = element_rect(fill = '#EFEFEF')
        ,panel.grid.minor = element_line(color = '#FFFFFF')
        ,panel.grid.major = element_line(color = '#FFFFFF')
        ,plot.title = element_text(size = 24)
        ,axis.title = element_text(size = 14, color = '#555555')
        ,axis.title.y = element_text(vjust = 1, angle = 90)
        ,axis.title.x = element_text(hjust = 0.95, angle = 0)
        ,axis.text.x = element_text(size=8,angle = 90)
  )

if (isTRUE(graph)){plot(g)}
    CR$Reference <- reduceRefs(CR$Reference)
    result=list(spectroscopy=g, rpysTable=RPYS, CR=CR)
    return(result)
}

yearExtract <- function(string,db){
  if (db=="ISI"){
  ind=regexpr(" [[:digit:]]{4} ",string)
  ind[is.na(ind)]=-1
  string[ind==-1]=" 0000 "
  ind[ind==-1]=1
  attr(ind[ind==-1],"match.length")=6
  y=trim(unlist(regmatches(string,ind)))
  }else{
    ind=regexpr("\\([[:digit:]]{4}\\)",string)
    ind[is.na(ind)]=-1
    string[ind==-1]="(0000)"
    ind[ind==-1]=1
    attr(ind[ind==-1],"match.length")=6
    y=unlist(regmatches(string,ind))
    y=substr(y,2,5)
  }
  return(y)
}

reduceRefs<- function(A){
  
  ind=unlist(regexec("*V[0-9]", A))
  A[ind>-1]=substr(A[ind>-1],1,(ind[ind>-1]-1))
  ind=unlist(regexec("*DOI ", A))
  A[ind>-1]=substr(A[ind>-1],1,(ind[ind>-1]-1))
  return(A)
}
