#' Bradford's law 
#'
#' It estimates and draws the Bradford's law source distribution.
#' 
#' Bradford's law is a pattern first described by (\cite{Samuel C. Bradford, 1934}) that estimates the exponentially diminishing returns 
#' of searching for references in science journals. 
#' 
#' One formulation is that if journals in a field are sorted by number of articles into three groups, each with about one-third of all articles, 
#' then the number of journals in each group will be proportional to 1:n:n2.\cr\cr
#' 
#' Reference:\cr
#' Bradford, S. C. (1934). Sources of information on specific subjects. Engineering, 137, 85-86.\cr
#' 
#' @param M is a bibliographic dataframe.
#' @return The function \code{bradford} returns a list containing the following objects:
#' \tabular{lll}{
#' \code{table}  \tab   \tab a dataframe with the source distribution partitioned in the three zones\cr
#' \code{graph}   \tab   \tab the source distribution plot in ggplot2 format}
#'
#' @examples
#' 
#' \dontrun{
#' file <- 'https://www.bibliometrix.org/datasets/bibliometrics_articles.txt'
#' M <- convert2df(file = file, dbsource = "isi", format = "bibtex")
#' BR <- bradford(M)
#' }
#'
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis
#' @seealso \code{\link{summary}} method for class '\code{bibliometrix}'
#' 
#' @export

bradford<-function(M){

SO=sort(table(M$SO),decreasing = TRUE)
n=sum(SO)
cumSO=cumsum(SO)
cutpoints=round(c(1,n*0.33,n*0.67,Inf))
groups=cut(cumSO,breaks = cutpoints,labels=c("Zone 1", "Zone 2", "Zone 3"))
a=length(which(cumSO<n*0.33))+1
b=length(which(cumSO<n*0.67))+1
Z=c(rep("Zone 1",a),rep("Zone 2",b-a),rep("Zone 3",length(cumSO)-b))
df=data.frame(SO=names(cumSO),Rank=1:length(cumSO),Freq=as.numeric(SO),cumFreq=cumSO,Zone=Z, stringsAsFactors = FALSE)

g=ggplot2::ggplot(df, aes(x = log(.data$Rank), y = .data$Freq, text=paste("Source: ",.data$SO,"\nN. of Documents: ",.data$Freq))) +
  geom_line(aes(group="NA")) +
  geom_area(aes(group="NA"),fill = "dodgerblue", alpha = 0.5) +
  annotate("rect", xmin=0, xmax=log(df$Rank[a]), ymin=0, ymax=max(df$Freq), alpha=0.4)+
  labs(x = 'Source log(Rank)', y = 'Articles', title = "Bradford's Law") +
  annotate("text",x=log(df$Rank[a])/2, y=max(df$Freq)/2, label = "Core\nSources",fontface =2,alpha=0.5,size=10)+
  scale_x_continuous(breaks=log(df$Rank)[1:a],labels=as.character(substr(df$SO,1,50))[1:a]) +
  theme(text = element_text(color = "#444444")
        ,legend.position="none"
        ,panel.background = element_rect(fill = '#EFEFEF')
        ,panel.grid.minor = element_line(color = '#FFFFFF')
        ,panel.grid.major = element_line(color = '#FFFFFF')
        ,plot.title = element_text(size = 24)
        ,axis.title = element_text(size = 14, color = '#555555')
        ,axis.title.y = element_text(vjust = 1, angle = 90)
        ,axis.title.x = element_text(hjust = 0)
        ,axis.text.x = element_text(angle=90,hjust=1,size=8,face="bold")
  )

results=list(table=df,graph=g)
return(results)
}


