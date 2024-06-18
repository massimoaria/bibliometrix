utils::globalVariables(c("Year", "diffMedian", "Citations", "citedYears",
                         "Reference", "citingYears", "benchmark", "status", "citations",
                         "Freq", "diffMedian5"))
#' Reference Publication Year Spectroscopy
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
#' @return a list containing the spectroscopy (class ggplot2) and three dataframes with the number of citations
#' per year, the list of the cited references for each year, and the reference list with citations recorded year by year, respectively.
#'  
#'
#' @examples
#' 
#'
#' data(scientometrics, package = "bibliometrixData")
#' res <- rpys(scientometrics, sep=";", graph = TRUE)
#'
#' @seealso \code{\link{convert2df}} to import and convert an ISI or SCOPUS
#'   Export file in a data frame.
#' @seealso \code{\link{biblioAnalysis}} to perform a bibliometric analysis.
#' @seealso \code{\link{biblioNetwork}} to compute a bibliographic network.
#' @export

rpys <- function(M, sep=";", timespan=NULL, graph=T){

  options(dplyr.summarise.inform = FALSE)
  
  M$CR<-gsub("DOI;","DOI ", as.character(M$CR))
  
  Fi<-strsplit(M[,"CR"],sep)
  Fi<-lapply(Fi,trim.leading)
  Fi<-lapply(Fi,function(l) l<-l[nchar(l)>10])
  citingYears <- rep(M$PY,lengths(Fi))
  Fi<-(unlist(Fi))
  
  df <- data.frame(Reference=Fi, citingYears=citingYears) %>% 
    mutate(Reference = refCleaning(Reference, db=M$DB[1]))
  df$citedYears <- as.numeric(yearExtract(df$Reference, db=M$DB[1]))
  
  df <- df %>% 
    dplyr::filter(!is.na(Reference) & citedYears>1700 & citedYears<=as.numeric(substr(Sys.Date(),1,4))) %>% 
    group_by(citedYears,citingYears, Reference) %>% 
    summarize(citations = n()) %>% 
    group_by(citedYears,citingYears) %>% 
    mutate(benchmark = mean(citations,na.rm=T),
           status = sign(citations-benchmark)) %>% 
    ungroup() %>% 
    arrange(citedYears,Reference,citingYears) 

  
  
  CR <- df %>% 
    group_by(citedYears,Reference) %>% 
    select(-citingYears, -status) %>% 
    summarize(Freq = sum(citations))

RPYS <- CR %>% 
  select(-Reference) %>% 
  group_by(citedYears) %>% 
  summarize(n = sum(Freq, na.rm=TRUE))
  
yearSeq <- RPYS$citedYears
missingYears <- setdiff(seq(min(yearSeq),max(yearSeq)), yearSeq)
RPYS[(nrow(RPYS)+1):(nrow(RPYS)+length(missingYears)),] <- rbind(cbind(missingYears,rep(0,length(missingYears))))
RPYS <- RPYS %>% arrange(citedYears)


## calculating running median
YY <- c(rep(0,4),RPYS$n)
Median <- numeric(nrow(RPYS))
for (i in 5:length(YY)){
  Median[i-4]=median(YY[(i-4):i])
}
####
#Median=runmed(Y,5)
RPYS$diffMedian <- RPYS$n-Median



if (length(timespan)==2){
  RPYS <- RPYS %>% 
    dplyr::filter(citedYears>=min(timespan) & 
                    citedYears<=max(timespan))
}
names(RPYS) <- c("Year", "Citations", "diffMedian5")

RPYS <- RPYS %>% 
  mutate(diffMedian = ifelse(diffMedian5>0,diffMedian5,0))

data("logo",envir=environment())
logo <- grid::rasterGrob(logo,interpolate = TRUE)

x <- c(min(RPYS$Year),min(RPYS$Year)+diff(range(RPYS$Year))*0.125)+1
y <- c(min(c(RPYS$Citations,RPYS$diffMedian)),min(c(RPYS$Citations,RPYS$diffMedian))+diff(range(c(RPYS$Citations,RPYS$diffMedian)))*0.125)*1.05

RPYS <- RPYS %>% 
  left_join(CR %>% 
              group_by(citedYears) %>% 
              slice_max(order_by = Freq, n=3, with_ties = FALSE) %>% 
              summarize(References = paste(firstup(Reference),collapse="\n")), 
            by=c("Year" = "citedYears"))


g=ggplot(RPYS, aes(x=Year ,y=Citations,text=paste("Year: ",Year," - Total Citations: ",Citations,"\nTop 3 References:\n",References)))+
  geom_line(aes(group="NA")) +
  geom_line(aes(x=Year,y=diffMedian, color="firebrick", group="NA"))+
  labs(x = 'Year'
       , y = 'Cited References'
       , title = "Reference Publication Year Spectroscopy",
       caption = "Number of Cited References (black line) - Deviation from the 5-Year Median (red line)") +
  scale_x_continuous(breaks= (RPYS$Year[seq(1,length(RPYS$Year),by=round(length(RPYS$Year)/30))])) +
  theme(text = element_text(color = "#444444"), legend.position="none"
        ,plot.caption = element_text(size = 9, hjust = 0.5,
                                     color = "black", face = "bold")
        ,panel.background = element_rect(fill = '#FFFFFF')
        #,panel.grid.minor = element_line(color = '#FFFFFF')
        ,panel.grid.major = element_line(color = '#EFEFEF')
        ,plot.title = element_text(size = 24)
        ,axis.title = element_text(size = 14, color = '#555555')
        ,axis.title.y = element_text(vjust = 1, angle = 90)
        ,axis.title.x = element_text(hjust = 0.95, angle = 0)
        ,axis.text.x = element_text(size=8,angle = 90)
        ,axis.line.x = element_line(color="black", linewidth=0.5)
        ,axis.line.y = element_line(color="black", linewidth=0.5)
  ) + annotation_custom(logo, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 

    if (isTRUE(graph)){plot(g)}
    CR$Reference <- reduceRefs(CR$Reference)
    CR <- CR %>% 
      rename(Year = citedYears) %>% 
      ungroup()
    result=list(spectroscopy=g, 
                rpysTable=RPYS %>% select(-References), 
                CR=CR %>% mutate(Year = as.character(Year)), 
                df=df)
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

refCleaning <- function(l,db){
  if (db=="ISI"){
    #ref<-unlist(lapply(Fi, function(l){
      l<-gsub("\\).*",")",l)
      l<-gsub(","," ",l)
      l<-gsub(";"," ",l)
      l <- gsub("\\."," ",l)
      l <- trimws(trimES(l))
      l<-l[nchar(l)>0]
      #return(l)
   # }))
  }else{
    #ref<-unlist(lapply(Fi, function(l){
      l<-gsub(","," ",l)
      l<-gsub(";"," ",l) 
      l <- gsub("\\."," ",l)
      l <- trimws(trimES(l))
      l<-l[nchar(l)>0]
      return(l)
    #}))
  }
  return(l)
}
