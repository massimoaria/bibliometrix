#' Create a thematic map
#'
#' It creates a thematic map based on co-word network analysis and clustering.
#' The methodology is inspired by the proposal of Cobo et al. (2011). 
#' 
#' \code{thematicMap} starts from a co-occurrence keyword network to plot in a 
#' two-dimesional map the typological themes of a domain.\cr\cr
#' 
#' Reference:\cr
#' Cobo, M. J., Lopez-Herrera, A. G., Herrera-Viedma, E., & Herrera, F. (2011). An approach for detecting, quantifying, 
#' and visualizing the evolution of a research field: A practical application to the fuzzy sets theory field. Journal of Informetrics, 5(1), 146-166.\cr
#' 
#' 
#' @param M is a bibliographic dataframe.
#' @param field is the textual attribute used to build up the thematic map. It can be \code{field = c("ID","DE", "TI", "AB")}.
#' \code{\link{biblioNetwork}} or \code{\link{cocMatrix}}.
#' @param n is an integer. It indicates the number of terms to include in the analysis.
#' @param minfreq is a integer. It indicates the minimum frequency (per thousand) of a cluster. It is a number in the range (0,1000).
#' @param stemming is logical. If it is TRUE the word (from titles or abstracts) will be stemmed (using the Porter's algorithm).
#' @param size is numerical. It indicates del size of the cluster circles and is a number in the range (0.01,1).
#' @param n.labels is integer. It indicates how many labels associate to each cluster. Default is \code{n.labels = 1}.
#' @param repel is logical. If it is TRUE ggplot uses geom_label_repel instead of geom_label.
#' @return a list containing:
#' \tabular{lll}{
#' \code{map}\tab   \tab The thematic map as ggplot2 object\cr
#' \code{clusters}\tab   \tab Centrality and Density values for each cluster. \cr
#' \code{words}\tab   \tab A list of words following in each cluster\cr
#' \code{nclust}\tab   \tab The number of clusters\cr
#' \code{net}\tab    \tab A list containing the network output (as provided from the networkPlot function)}
#' 
#'
#' @examples
#' 
#' data(scientometrics)
#' res <- thematicMap(scientometrics, field = "ID", n = 250, minfreq = 5, size = 0.5, repel = TRUE)
#' plot(res$map)
#'
#' @seealso \code{\link{biblioNetwork}} function to compute a bibliographic network.
#' @seealso \code{\link{cocMatrix}} to compute a bibliographic bipartite network.
#' @seealso \code{\link{networkPlot}} to plot a bibliographic network.
#'
#' @export

thematicMap <- function(M, field="ID", n=250, minfreq=5, stemming=FALSE, size=0.5, n.labels=1, repel=TRUE){
  
  minfreq <- max(2,floor(minfreq*nrow(M)/1000))
  
  switch(field,
         ID={
           NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
           TERMS=tolower(M$ID)
         },
         DE={
           NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "author_keywords", sep = ";")
           TERMS=tolower(M$DE)
         },
         TI={
           #if(!("TI_TM" %in% names(values$M))){values$M=termExtraction(values$M,Field="TI",verbose=FALSE, stemming = input$stemming)}
           M=termExtraction(M,Field="TI",verbose=FALSE, stemming = stemming)
           NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "titles", sep = ";")
           
         },
         AB={
           #if(!("AB_TM" %in% names(values$M))){values$M=termExtraction(values$M,Field="AB",verbose=FALSE, stemming = input$stemming)}
           M=termExtraction(M,Field="AB",verbose=FALSE, stemming = stemming)
           NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "abstracts", sep = ";")
           
         })
  
  #S <- normalizeSimilarity(NetMatrix, type = "association")
  #S=NetMatrix
  #t = tempfile();pdf(file=t) #### trick to hide igraph plot
  if (nrow(NetMatrix)>0){
    Net <- networkPlot(NetMatrix, normalize="association",n=n, Title = "Keyword co-occurrences",type="auto",
                     labelsize = 2, halo = F,cluster="louvain",remove.isolates=TRUE,
                     remove.multiple=FALSE, noloops=TRUE, weighted=TRUE,label.cex=T,edgesize=5, 
                     size=1,edges.min = 1, label.n=n, verbose = FALSE)
  }else{
    cat("\n\nNetwork matrix is empty!\nThe analysis cannot be performed\n\n")
    return()
  }
  #dev.off();file.remove(t) ### end of trick
  S=Net$S
  
  row.names(NetMatrix)=colnames(NetMatrix)=tolower(row.names(NetMatrix))
  net=Net$graph
  net_groups <- Net$cluster_obj
  group=net_groups$membership
  word=net_groups$name
  color=V(net)$color
  color[is.na(color)]="#D3D3D3"
  
  ###
  W=intersect(row.names(NetMatrix),word)
  index=which(row.names(NetMatrix) %in% W)
  ii=which(word %in% W)
  word=word[ii]
  group=group[ii]
  color=color[ii]
  ###


  C=diag(NetMatrix)

  sEij=S[index,index]
  #dim(sEij)
  sC=(C[index])


### centrality and density
  label_cluster=unique(group)
  word_cluster=word[group]
  centrality=c()
  density=c()
  labels=list()
  
  df_lab=data.frame(sC=sC,words=word,groups=group,color=color,cluster_label="NA",stringsAsFactors = FALSE)
  
  color=c()
  for (i in label_cluster){
    ind=which(group==i)
    w=df_lab$words[ind]
    wi=which.max(df_lab$sC[ind])
    df_lab$cluster_label[ind]=paste(w[wi[1:min(c(length(wi),3))]],collapse=";",sep="")
    centrality=c(centrality,sum(sEij[ind,-ind]))
    density=c(density,sum(sEij[ind,ind])/length(ind)*100)
    df_lab_g=df_lab[ind,]
    df_lab_g=df_lab_g[order(df_lab_g$sC,decreasing = T),]
    #if (dim(df_lab_g)[1]>2){k=3}else{k=1}
    k=1
    labels[[length(labels)+1]]=paste(df_lab_g$words[1:k],collapse = ";")
    color=c(color,df_lab$color[ind[1]])
  }
  #df_lab$cluster_label=gsub(";NA;",";",df_lab$cluster_label)
  
  centrality=centrality
  df=data.frame(centrality=centrality,density=density,rcentrality=rank(centrality),rdensity=rank(density),label=label_cluster,color=color)
 
  meandens=mean(df$rdensity)
  meancentr=mean(df$rcentrality)
  rangex=max(c(meancentr-min(df$rcentrality),max(df$rcentrality)-meancentr))
  rangey=max(c(meandens-min(df$rdensity),max(df$rdensity)-meandens))
  
  df$name=unlist(labels)
  df=df[order(df$label),]
  df_lab <- df_lab[df_lab$sC>=minfreq,]
  df=df[(df$name %in% intersect(df$name,df_lab$cluster_label)),]
  
  row.names(df)=df$label
  
  A <- group_by(df_lab, .data$groups) %>% summarise(freq = sum(.data$sC)) %>% as.data.frame
  
  df$freq=A[,2]
  
  W <- df_lab %>% group_by(.data$groups) %>% #dplyr::filter(.data$sC>1) %>% 
    arrange(-.data$sC, .by_group = TRUE) %>% 
    dplyr::top_n(10, .data$sC) %>%
    summarise(wordlist = paste(.data$words,.data$sC,collapse="\n")) %>% as.data.frame()
  
  df$words=W[,2]
  
  ### number of labels for each cluster
  labels=gsub("\\d", "",df$words)
  
  ### cut ties over 10 words
  df$words <- unlist(lapply(df$words, function(l){
    l <- unlist(strsplit(l,"\\\n"))
    l <- l[1:(min(length(l),10))]
    l <- paste0(l,collapse="\n")
  }))
  
  L=unlist(lapply(labels, function(l){
    l=strsplit(l," \\\n")
    l=paste(l[[1]][1:min(n.labels,lengths(l))], collapse="\n")
  }))
  df$name_full=L
  ###
  
  #meandens <- 0
  #meancentr <- 0
  #meandens=mean(df$rdensity)
  #meancentr=mean(df$rcentrality)
  #df=df[df$freq>=minfreq,]
  
  #rangex=max(c(meancentr-min(df$rcentrality),max(df$rcentrality)-meancentr))
  #rangey=max(c(meandens-min(df$rdensity),max(df$rdensity)-meandens))
  xlimits=c(meancentr-rangex-0.5,meancentr+rangex+0.5)
  ylimits=c(meandens-rangey-0.5,meandens+rangey+0.5)
  

  annotations <- data.frame(
    xpos = sort(c(xlimits,xlimits)),
    ypos = c(ylimits, ylimits),
    words = c("Emerging or\nDeclining Themes","Niche Themes","Basic Themes ","Motor Themes "),
    hjustvar = c(0,0,1,1) ,
    vjustvar = c(0,1.0,0,1))
  

  g=ggplot(df, aes(x=.data$rcentrality, y=.data$rdensity, text=c(.data$words))) +
    geom_point(group="NA",aes(size=log(as.numeric(.data$freq))),shape=20,col=adjustcolor(df$color,alpha.f=0.5))     # Use hollow circles
  if (size>0){
    if (isTRUE(repel)){
      g=g+geom_label_repel(aes(group="NA",label=ifelse(.data$freq>1,unlist(tolower(.data$name_full)),'')),size=3*(1+size),angle=0)}else{
      g=g+geom_text(aes(group="NA",label=ifelse(.data$freq>1,unlist(tolower(.data$name_full)),'')),size=3*(1+size),angle=0)
    }
  }
  
    g=g+geom_hline(yintercept = meandens,linetype=2, color=adjustcolor("black",alpha.f=0.7)) +
    geom_vline(xintercept = meancentr,linetype=2, color=adjustcolor("black",alpha.f=0.7)) + 
      theme(legend.position="none") +
    scale_radius(range=c(5*(1+size), 30*(1+size)))+
      labs(x = "Relevance degree\n(Centrality)", y = "Development degree\n(Density)")+
      xlim(xlimits)+
      ylim(ylimits)+
      annotate("text",x=annotations$xpos,y= annotations$ypos,hjust=annotations$hjustvar,
                                         vjust=annotations$vjustvar,label=annotations$words, color=adjustcolor("gray20", alpha.f=0.5),size=3*(1+size))+
      theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

  names(df_lab)=c("Occurrences", "Words", "Cluster", "Color","Cluster_Label")
  words=df_lab[order(df_lab$Cluster),]
  words=words[!is.na(words$Color),]
  words$Cluster=as.numeric(factor(words$Cluster))
  row.names(df)=NULL

  ### Adding column Topics
  M$TOPIC=""
  
  #View(res$words)
  if (field %in% c("ID", "DE")) {
    ID = paste(TERMS, ";", sep = "")
    for (i in 1:nrow(words)) {
      w = paste(words$Words[i], ";", sep = "")
      TOPIC = paste(words$Cluster_Label[i], ";", sep = "")
      ind = which(regexpr(w, ID) > -1)
      M$TOPIC[ind] = paste(M$TOPIC[ind], TOPIC, sep = "")
    }
  } else {M$TOPIC="NA"}
  
  results=list(map=g, clusters=df, words=words,nclust=dim(df)[1], net=Net, TOPIC=M$TOPIC)
return(results)
}