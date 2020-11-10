#' Coupling Analysis
#'
#' It performs a coupling network analysis and plots community detection results on a bi-dimensional map (Coupling Map).
#' 
#' The analysis can be performed on three different units: documents, authors or sources and 
#' the coupling strength can be measured using the classical approach (coupled by references) 
#' or a novel approach based on unit contents (keywords or terms from titles and abstracts) 
#' 
#' @param M is a bibliographic dataframe.
#' @param analysis is the textual attribute used to select the unit of analysis. It can be \code{analysis = c("documents", "authors", "sources")}.
#' @param field is the textual attribute used to measure the coupling strength. It can be \code{field = c("CR", "ID","DE", "TI", "AB")}.
#' @param n is an integer. It indicates the number of units to include in the analysis.
#' @param minfreq is a integer. It indicates the minimum frequency (per thousand) of a cluster. It is a number in the range (0,1000).
#' @param stemming is logical. If it is TRUE the word (from titles or abstracts) will be stemmed (using the Porter's algorithm).
#' @param size is numerical. It indicates the size of the cluster circles and is a number in the range (0.01,1).
#' @param n.labels is integer. It indicates how many labels associate to each cluster. Default is \code{n.labels = 1}.
#' @param repel is logical. If it is TRUE ggplot uses geom_label_repel instead of geom_label.
#' @return a list containing:
#' \tabular{lll}{
#' \code{map}\tab   \tab The coupling map as ggplot2 object\cr
#' \code{clusters}\tab   \tab Centrality and Density values for each cluster. \cr
#' \code{data}\tab   \tab A list of units following in each cluster\cr
#' \code{nclust}\tab   \tab The number of clusters\cr
#' \code{NCS}\tab     \tab The Normalized Citation Score dataframe 
#' \code{net}\tab    \tab A list containing the network output (as provided from the networkPlot function)}
#' 
#'
#' @examples
#' 
#' \dontrun{
#' data(management)
#' res <- couplingMap(management, analysis = "authors", field = "CR", n = 250, minfreq = 3, size = 0.5, repel = TRUE)
#' plot(res$map)
#' }
#'
#' @seealso \code{\link{biblioNetwork}} function to compute a bibliographic network.
#' @seealso \code{\link{cocMatrix}} to compute a bibliographic bipartite network.
#' @seealso \code{\link{networkPlot}} to plot a bibliographic network.
#'
#' @export

couplingMap <- function(M, analysis = "documents", field="CR", n=500, minfreq=5, stemming=FALSE, size=0.5, n.labels=1, repel=TRUE){
  
  
  if (!(analysis %in% c("documents", "authors", "sources"))) {
    cat('\nanalysis argument is incorrect.\n\nPlease select one of the following choices: "documents", "authors", "sources"\n\n')
    return(NA)
  }
  minfreq <- max(0,floor(minfreq*nrow(M)/1000))
  
  switch(analysis,
         documents = {
           switch(field,
                  CR = {
                    NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", shortlabel = FALSE)
                    type <- "D_CR"
                  },
                  {
                    #field ID, DE, TI, AB
                    if (field %in% c("TI","AB")){
                      M=termExtraction(M,Field=field,verbose=FALSE, stemming = stemming)
                      type <- "D_KW"
                    }
                  NetMatrix <- coupling(M, field, analysis = "documents")  
                  })
         },
         authors = {
           switch(field,
                  CR = {
                    NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "authors")
                    type <- "AU_CR"
                  },
                  {
                    #field ID, DE, TI, AB
                    if (field %in% c("TI","AB")){
                      M=termExtraction(M,Field=field,verbose=FALSE, stemming = stemming)
                      type <- "AU_KW"
                    }
                    NetMatrix <- coupling(M, field, analysis = "authors")  
                  })
           },
         sources = {
           switch(field,
                  CR = {
                    NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "sources")
                    type <- "SO_CR"
                  },
                  {
                    #field ID, DE, TI, AB
                    if (field %in% c("TI","AB")){
                      M=termExtraction(M,Field=field,verbose=FALSE, stemming = stemming)
                      type <- "SO_KW"
                    }
                    NetMatrix <- coupling(M, field, analysis = "sources")  
                  })
         })
  
  # delete empty vertices
  NetMatrix <- NetMatrix[nchar(colnames(NetMatrix)) != 0, nchar(colnames(NetMatrix)) != 0]

  if (nrow(NetMatrix)>0){
    Net <- networkPlot(NetMatrix, normalize="salton",n=n, Title = paste("Coupling network of ",analysis," using ",field,sep=""),type="auto",
                     labelsize = 2, halo = F,cluster="louvain",remove.isolates=TRUE,
                     remove.multiple=FALSE, noloops=TRUE, weighted=TRUE,label.cex=T,edgesize=5, 
                     size=1,edges.min = 1, label.n=n, verbose = FALSE)
  }else{
    cat("\n\nNetwork matrix is empty!\nThe analysis cannot be performed\n\n")
    return()
  }
  #dev.off();file.remove(t) ### end of trick
  S=Net$S
  
  
  net=Net$graph
  
  NCS <- normalizeCitationScore(M,field=analysis)
  
  ### Citation for documents
  label <- V(net)$name
  
  
  L <- tibble(id=toupper(label))
  names(L) <- analysis
  D <- left_join(L,NCS, by = analysis, copy=T) 
  
  row.names(NetMatrix)=colnames(NetMatrix)=tolower(row.names(NetMatrix))
  net_groups <- Net$cluster_obj
  group=net_groups$membership
  #word=net_groups$name   ora label
  color=V(net)$color
  color[is.na(color)]="#D3D3D3"
  
  D$group <- group
  
  sEij=S
  #dim(sEij)
  sC=D$MNLCS
  
  
  ### centrality and density
  label_cluster=unique(group)
  word_cluster=label[group]
  centrality=c()
  density=c()
  labels=list()
  
  df_lab=data.frame(sC=sC,words=label,groups=group,color=color,cluster_label="NA",stringsAsFactors = FALSE)
  
  color=c()
  
  for (i in label_cluster){
    #print(i)
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
  
  centrality=centrality*10
  df=data.frame(centrality=centrality,density=density,rcentrality=rank(centrality),rdensity=rank(density),label=label_cluster,color=color)
  df$name=unlist(labels)
  df=df[order(df$label),]
  #df_lab <- df_lab[df_lab$sC>=minfreq,]
  #df=df[(df$name %in% intersect(df$name,df_lab$cluster_label)),]
  
  row.names(df)=df$label
  
  A <- group_by(df_lab, .data$groups) %>% summarise(freq = length(.data$sC)) %>% as.data.frame
  
  df$freq=A[,2]
  
  W <- df_lab %>% mutate(sC = round(.data$sC,2)) %>%
    group_by(.data$groups) %>% #dplyr::filter(.data$sC>1) %>% 
    arrange(-.data$sC, .by_group = TRUE) %>% 
    dplyr::top_n(10, .data$sC) %>%
    summarise(wordlist = paste(.data$words,.data$sC,collapse="\n")) %>% as.data.frame()
  
  df$words=W[,2]
  
  ### number of labels for each cluster
  #labels=gsub("\\d", "",df$words)
  
  ### cut ties over 10 words
  df$words <- unlist(lapply(df$words, function(l){
    l <- unlist(strsplit(l,"\\\n"))
    l <- l[1:(min(length(l),10))]
    l <- paste0(l,collapse="\n")
  }))
  
  L=unlist(lapply(df$words, function(l){
    l <- unlist(strsplit(l,"\\\n"))
    l=paste(l[1:min(n.labels,length(l))], collapse="\n")
  }))
  df$name_full=L
  
  meandens=mean(df$rdensity)
  meancentr=mean(df$rcentrality)
  df=df[df$freq>=minfreq,]
  
  rangex=max(c(meancentr-min(df$rcentrality),max(df$rcentrality)-meancentr))
  rangey=max(c(meandens-min(df$rdensity),max(df$rdensity)-meandens))
  xlimits=c(meancentr-rangex-0.5,meancentr+rangex+0.5)
  ylimits=c(meandens-rangey-0.5,meandens+rangey+0.5)
  
  
  annotations <- data.frame(
    xpos = sort(c(xlimits,xlimits)),
    ypos = c(ylimits, ylimits),
    words = c("Emerging or\nDeclining Themes","Niche Themes","Basic Themes ","Motor Themes "),
    hjustvar = c(0,0,1,1) ,
    vjustvar = c(0,1.0,0,1))
  
  #quadrant_names=rep(" ",4) ## empty tooltips for quadrant names
  
  g=ggplot(df, aes(x=.data$rcentrality, y=.data$rdensity, text=(.data$words))) +
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
    labs(x = "Relevance\n(Centrality)",y = "Affinity\n(Density)")+
    xlim(xlimits)+
    ylim(ylimits)+
    #annotate("text",x=annotations$xpos,y= annotations$ypos,hjust=annotations$hjustvar,
    #         vjust=annotations$vjustvar,label=annotations$words, color=adjustcolor("gray20", alpha.f=0.5),size=3*(1+size))+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  df_lab <- df_lab %>% 
    group_by(.data$groups) %>% 
    arrange(desc(.data$sC), .by_group=TRUE)
  
  names(df_lab)=c("NormalizedLocalCitationScore", analysis, "Cluster", "Color","Cluster_Label")
  words=df_lab[order(df_lab$Cluster),]
  words=words[!is.na(words$Color),]
  words$Cluster=as.numeric(factor(words$Cluster))
  row.names(df)=NULL
  
  
  results=list(map=g, clusters=df, data=words,nclust=dim(df)[1], NCS = D, net=Net)
  return(results)
}

coupling <- function(M,field, analysis){
  switch(analysis,
         documents = {
           WF <- t(cocMatrix(M, Field = field))
           NetMatrix <- crossprod(WF, WF)
         },
         authors = {
           WF <- cocMatrix(M, Field = field)
           WA <- cocMatrix(M,Field = "AU")
           FA <- t(crossprod(WA,WF))
           NetMatrix <- crossprod(FA,FA)
         },
         sources = {
           WF <- cocMatrix(M, Field = field)
           WS <- cocMatrix(M,Field = "SO")
           FS <- t(crossprod(WS,WF))
           NetMatrix <- crossprod(FS,FS)
         })
  return(NetMatrix)
}