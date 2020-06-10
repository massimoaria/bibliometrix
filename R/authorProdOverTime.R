#' Top-Authors' Productivity over the Time
#'
#' It calculates and plots the author production (in terms of number of publications) over the time. 
#' @param M is a bibliographic data frame obtained by \code{\link{convert2df}} function.
#' @param k is a integer. It is the number of top authors to analyze and plot. Default is \code{k = 10}.
#' @param graph is logical. If TRUE the function plots the author production over time graph. Default is \code{graph = TRUE}.
#' @return The function \code{authorProdOverTime} returns a list containing two objects:
#' \tabular{lll}{
#' \code{dfAU}  \tab   \tab is a data frame\cr
#' \code{dfpapersAU}\tab    \tab is a data frame\cr
#' \code{graph}   \tab   \tab a ggplot object}
#'
#' @examples
#' data(scientometrics)
#' res <- authorProdOverTime(scientometrics, k=10)
#' print(res$dfAU)
#' plot(res$graph)
#'
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis
#' @seealso \code{\link{summary}} method for class '\code{bibliometrix}'
#' 
#' @export
#' 
authorProdOverTime <- function(M,k=10, graph=TRUE){

  M$TC=as.numeric(M$TC)
  M$PY=as.numeric(M$PY)
  AU=names(tableTag(M,"AU"))
  k=min(k,length(AU))
  AU=AU[1:k]
  #AU=names(AU)
  df=data.frame("Author"="NA","year"=NA, "TI"="NA","SO"="NA","DOI"="NA", "TC"=NA,"TCpY"=NA,stringsAsFactors = FALSE)
  Y=as.numeric(substr(Sys.time(),1,4))
  if (!("DI" %in% names(M))){M$DI="NA"}
  for (i in 1:length(AU)){
   
    ind=which(regexpr(AU[i],M$AU)>-1)
    TCpY=M$TC[ind]/(Y-M$PY[ind]+1)
    dfAU=data.frame("Author"=rep(AU[i],length(ind)),"year"=M$PY[ind],"TI"=M$TI[ind],"SO"=M$SO[ind],"DOI"=M$DI[ind],"TC"=M$TC[ind], "TCpY"=TCpY,stringsAsFactors = TRUE)
    df=rbind(df,dfAU)
  }
  df=df[-1,]
  
  df2<-dplyr::group_by(df, .data$Author,.data$year) %>%
    dplyr::summarise(freq=length(.data$year),TC=sum(.data$TC),TCpY=sum(.data$TCpY))
  
  df2=as.data.frame(df2)
  df2$Author=factor(df2$Author,levels=AU[1:k])
  #theme_set(theme_bw())
  

  g <- ggplot(df2, aes(x=.data$Author, y=.data$year, text = paste("Author: ", .data$Author,"\nYear: ",.data$year ,"\nN. of Articles: ",.data$freq ,"\nTotal Citations per Year: ", round(.data$TCpY,2))))+
    geom_point(aes(alpha=.data$TCpY,size = .data$freq), color="dodgerblue4")+ 
    scale_size(range=c(2,6))+
    scale_alpha(range=c(0.3,1))+
    scale_y_continuous(breaks = seq(min(df2$year),max(df2$year), by=2))+
    guides(size = guide_legend(order = 1, "N.Articles"), alpha = guide_legend(order = 2, "TC per Year"))+
    theme(legend.position = 'right'
          ,text = element_text(color = "#444444")
          ,panel.background = element_rect(fill = 'gray97')
          ,panel.grid.minor = element_line(color = '#FFFFFF')
          ,panel.grid.major = element_line(color = '#FFFFFF')
          ,plot.title = element_text(size = 24)
          ,axis.title = element_text(size = 14, color = '#555555')
          ,axis.title.y = element_text(vjust = 1, angle = 0, face="bold")
          ,axis.title.x = element_text(hjust = .95, face="bold")
          ,axis.text.x = element_text(face="bold")
          ,axis.text.y = element_text(face="bold")
    )+
    labs(title="Top-Authors' Production over the Time", 
         x="Author",
         y="Year")+
    geom_line(data=df2,aes(x = .data$Author, y = .data$year, group=.data$Author),size=1.0, color="firebrick", alpha=0.3 )+
    scale_x_discrete(limits = rev(levels(df2$Author)))+
    coord_flip()
  
  df$DOI=as.character(df$DOI)
  res <- list(dfAU=df2,dfPapersAU=df,graph=g)
  if (isTRUE(graph)){plot(g)}
  return(res)
}

