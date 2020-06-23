#' Plotting bibliometric analysis results
#'
#' \code{plot} method for class '\code{bibliometrix}'
#' @param x is the object for which plots are desired.
#' @param ... can accept two arguments:\cr 
#' \code{k} is an integer, used for plot formatting (number of objects). Default value is 10.\cr
#' \code{pause} is a logical, used to allow pause in screen scrolling of results. Default value is \code{pause = FALSE}.
#' @return The function \code{plot} returns a list of plots of class \code{ggplot2}. 
#' 
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
  graphs=list()
  
  arguments <- list(...)
  if (sum(names(arguments)=="k")==0){k=10} else {k=arguments$k}
  if (sum(names(arguments)=="pause")==0){pause=FALSE} else {pause=arguments$pause}
  
  if (pause == TRUE){
    cat("Hit <Return> to see next plot: ")
    line <- readline()}

  # Authors
  #barplot(x$Authors[1:k],horiz=TRUE,las=2,cex.names=0.5,main="Most Productive Authors",xlab="Articles")
  xx=as.data.frame(x$Authors[1:k])
  g=ggplot(data=xx, aes(x=.data$AU, y=.data$Freq)) +
    geom_bar(stat="identity", fill="steelblue")+
    labs(title="Most productive Authors", x = "Authors")+
    labs(y = "N. of Documents")+
    theme_minimal() +
    coord_flip()
  plot(g)
  
  graphs$MostProdAuthors=g
  
  if (pause == TRUE){
    cat("Hit <Return> to see next plot: ")
    line <- readline()}
  if (!is.na(x$CountryCollaboration[1,1])){
  # Countries
  xx=x$CountryCollaboration[1:k,]
  xx=xx[order(-(xx$SCP+xx$MCP)),]
  xx1=cbind(xx[,1:2],rep("SCP",k))
  names(xx1)=c("Country","Freq","Collaboration")
  xx2=cbind(xx[,c(1,3)],rep("MCP",k))
  names(xx2)=c("Country","Freq","Collaboration")
  xx=rbind(xx2,xx1)
  xx$Country=factor(xx$Country,levels=xx$Country[1:dim(xx2)[1]])
  g=suppressWarnings(ggplot(data=xx, aes(x=.data$Country, y=.data$Freq,fill=.data$Collaboration)) +
    geom_bar(stat="identity")+
    scale_x_discrete(limits = rev(levels(xx$Country)))+
    scale_fill_discrete(name="Collaboration",
                        breaks=c("SCP","MCP"))+
    labs(title = "Most Productive Countries", x = "Countries", y = "N. of Documents", 
         caption = "SCP: Single Country Publications, MCP: Multiple Country Publications")+
    theme_minimal() +
    theme(plot.caption = element_text(size = 9, hjust = 0.5,
          color = "blue", face = "italic"))+
    coord_flip())
  plot(g)
  graphs$MostProdCountries=g
  } else {graphs$MostProdCountries=NA}
  
  if (pause == TRUE){
    cat("Hit <Return> to see next plot: ")
    line <- readline()}

    # Articles per Year
#  Y=table(x$Years)
#  Ym=table(x$Years[x$nAUperPaper>1])
#  plot(as.numeric(row.names(Y)),Y,type="l",main="Articles per Year",xlab="Year",ylab="N.Articles",col="blue")
#  lines(as.numeric(row.names(Ym)),Ym,col="red")
#  legend(x="topleft",c("Total Articles","Multi Auth. Articles"),col=c("blue","red"),lty = c(1, 1),cex=0.6,bty="n")
  
  Tab=table(x$Years)
  
  ## inserting missing years
  YY=setdiff(seq(min(x$Years, na.rm=TRUE),max(x$Years, na.rm=TRUE)),names(Tab))
  Y=data.frame(Year=as.numeric(c(names(Tab),YY)),Freq=c(as.numeric(Tab),rep(0,length(YY))))
  Y=Y[order(Y$Year),]
  
  names(Y)=c("Year","Freq")
  
  g=ggplot(Y, aes(x = .data$Year, y = .data$Freq)) +
    geom_line() +
    geom_area(fill = '#002F80', alpha = .5) +
    labs(x = 'Year'
         , y = 'Articles'
         , title = "Annual Scientific Production") +
    scale_x_continuous(breaks= (Y$Year[seq(1,length(Y$Year),by=2)])) +
    theme(text = element_text(color = "#444444")
          ,panel.background = element_rect(fill = '#EFEFEF')
          ,panel.grid.minor = element_line(color = '#FFFFFF')
          ,panel.grid.major = element_line(color = '#FFFFFF')
          ,plot.title = element_text(size = 24)
          ,axis.title = element_text(size = 14, color = '#555555')
          ,axis.title.y = element_text(vjust = 1, angle = 0)
          ,axis.title.x = element_text(hjust = 0)
    )   
  plot(g)
  graphs$AnnualScientProd=g
  
  
  Table2=NA
  if(!(x$DB %in% c("COCHRANE","PUBMED"))){
  
  if (pause == TRUE){
    cat("Hit <Return> to see next plot: ")
    line <- readline()}

  
  # Total Citation Plot
  Table2=aggregate(x$TotalCitation,by=list(x$Years),length)
  Table2$xx=aggregate(x$TotalCitation,by=list(x$Years),mean)$x
  Table2$Annual=NA
  d=date()
  d=as.numeric(substring(d,nchar(d)-3,nchar(d)))
  Table2$Years=d-Table2$Group.1
  Table2$Annual=Table2$xx/Table2$Years
  names(Table2)=c("Year","N","MeanTCperArt","MeanTCperYear","CitableYears")
  
  ## inserting missing years
  YY=setdiff(seq(min(x$Years,na.rm=TRUE),max(x$Years,na.rm=TRUE)),Table2$Year)
  if (length(YY>0)){
  YY=data.frame(YY,0,0,0,0)
  names(YY)=c("Year","N","MeanTCperArt","MeanTCperYear","CitableYears")
  Table2=rbind(Table2,YY)
  Table2=Table2[order(Table2$Year),]
  row.names(Table2)=Table2$Year}
  
  
  g=ggplot(Table2, aes(x = .data$Year, y = .data$MeanTCperYear)) +
    geom_line() +
    geom_area(fill = '#002F80', alpha = .5) +
    labs(x = 'Year'
         , y = 'Citations'
         , title = "Average Article Citations per Year")+
    scale_x_continuous(breaks= (Table2$Year[seq(1,length(Table2$Year),by=2)])) +
    theme(text = element_text(color = "#444444")
          ,panel.background = element_rect(fill = '#EFEFEF')
          ,panel.grid.minor = element_line(color = '#FFFFFF')
          ,panel.grid.major = element_line(color = '#FFFFFF')
          ,plot.title = element_text(size = 24)
          ,axis.title = element_text(size = 14, color = '#555555')
          ,axis.title.y = element_text(vjust = 1, angle = 0)
          ,axis.title.x = element_text(hjust = 0)
    )   
  plot(g)
  graphs$AverArtCitperYear=g
  
  if (pause == TRUE){
    cat("Hit <Return> to see next plot: ")
    line <- readline()}
  
  g=ggplot(Table2, aes(x = .data$Year, y = .data$MeanTCperArt)) +
    geom_line() +
    geom_area(fill = '#002F80', alpha = .5) +
    labs(x = 'Year'
         , y = 'Citations'
         , title = "Average Total Citations per Year")+
    scale_x_continuous(breaks= (Table2$Year[seq(1,length(Table2$Year),by=2)])) +
    theme(text = element_text(color = "#444444")
          ,panel.background = element_rect(fill = '#EFEFEF')
          ,panel.grid.minor = element_line(color = '#FFFFFF')
          ,panel.grid.major = element_line(color = '#FFFFFF')
          ,plot.title = element_text(size = 24)
          ,axis.title = element_text(size = 14, color = '#555555')
          ,axis.title.y = element_text(vjust = 1, angle = 0)
          ,axis.title.x = element_text(hjust = 0, angle = 0)
    )   
  plot(g)
  graphs$AverTotCitperYear=g
  } else {
    graphs$AverArtCitperYear=NA
    graphs$AverTotCitperYear=NA
  }
  invisible(graphs)
  
}
