#' Coupling Analysis
#'
#' It performs a coupling network analysis and plots community detection results on a bi-dimensional map (Coupling Map).
#' 
#' The analysis can be performed on three different units: documents, authors or sources and 
#' the coupling strength can be measured using the classical approach (coupled by references) 
#' or a novel approach based on unit contents (keywords or terms from titles and abstracts) 
#' 
#' The x-axis measures the cluster centrality (by Callon's Centrality index) while the y-axis measures the cluster impact 
#' by Mean Normalized Local Citation Score (MNLCS). 
#' The Normalized Local Citation Score (NLCS) of a document is calculated 
#' by dividing the actual count of local citing items by the expected citation rate for documents with the same year of publication.
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
#' \code{NCS}\tab     \tab The Normalized Citation Score dataframe\cr
#' \code{net}\tab    \tab A list containing the network output (as provided from the networkPlot function)}
#' 
#' @seealso \code{\link{nomalizeCitationScore}}.
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
  

  Net <- network(M, analysis=analysis, field=field, stemming=stemming,n=n)
  
  net=Net$graph
  
  NCS <- normalizeCitationScore(M,field=analysis) 
  NCS[,1] <- toupper(NCS[,1])
  
  ### Citation for documents
  label <- V(net)$name
  
  
  L <- tibble(id=toupper(label))
  names(L) <- analysis
  D <- left_join(L,NCS, by = analysis, copy=T) 
  
  L <- tibble(id=tolower(label))
  names(L) <- names(Net$cluster_res)[1] <- analysis

  C <- left_join(L,Net$cluster_res, by = analysis, copy=T)
  

  group=Net$cluster_obj$membership
  color=V(net)$color
  color[is.na(color)]="#D3D3D3"
  
  D$group <- group
  D$color <- color
  
  DC <- cbind(D,C[,-1])
  DC$name <- DC[,1] 
  df_lab <- DC %>% group_by(.data$group) %>%
    mutate(MNLCS2 = replace(.data$MNLCS,.data$MNLCS<1,NA),  ## remove NCS<1
           MNLCS = round(.data$MNLCS,2),
           name = tolower(.data$name),
           freq = length(.data$MNLCS))  %>%
    arrange(desc(.data$MNLCS), .by_group = TRUE)
  
  df <- df_lab %>%
    mutate(centrality = mean(.data$pagerank_centrality),
           impact = mean(.data$MNLCS2,na.rm=TRUE),
           impact = replace(impact, is.na(impact),NA)) %>%
    top_n(.data$MNLCS, n=10) %>%
    summarize(freq = freq[1],
              centrality = .data$centrality[1]*100,
              impact = .data$impact[1],
              label_cluster = .data$group[1],
              color = .data$color[1],
              label = tolower(paste(.data$name[1:min(n.labels,length(.data$name))],collapse = "\n")),
              words = tolower(paste(.data$name,.data$MNLCS,collapse = "\n"))) %>%
    mutate(rcentrality = rank(.data$centrality), 
           words = unlist(lapply(.data$words, function(l){
                        l <- unlist(strsplit(l,"\\\n"))
                        l <- l[1:(min(length(l),10))]
                        l <- paste0(l,collapse="\n")  
                        })),
           rimpact = rank(impact)) %>%
    arrange(.data$group) %>% as.data.frame()
  
  
  row.names(df) <- df$label
  
  meandens <- mean(df$rimpact)
  meancentr <- mean(df$rcentrality)
  df <- df[df$freq>=minfreq,]
  
  rangex <- max(c(meancentr-min(df$rcentrality),max(df$rcentrality)-meancentr))
  rangey <- max(c(meandens-min(df$rimpact),max(df$rimpact)-meandens))
  xlimits <- c(meancentr-rangex-0.5,meancentr+rangex+0.5)
  ylimits <- c(meandens-rangey-0.5,meandens+rangey+0.5)
  
  
  #quadrant_names=rep(" ",4) ## empty tooltips for quadrant names
  
  g=ggplot(df, aes(x=.data$rcentrality, y=.data$rimpact, text=(.data$words))) +
    geom_point(group="NA",aes(size=log(as.numeric(.data$freq))),shape=20,col=adjustcolor(df$color,alpha.f=0.5))     # Use hollow circles
  if (size>0){
    if (isTRUE(repel)){
      g=g+geom_label_repel(aes(group="NA",label=ifelse(.data$freq>1,unlist(tolower(.data$label)),'')),size=3*(1+size),angle=0)}else{
        g=g+geom_text(aes(group="NA",label=ifelse(.data$freq>1,unlist(tolower(.data$label)),'')),size=3*(1+size),angle=0)
      }
  }
  
  g=g+geom_hline(yintercept = meandens,linetype=2, color=adjustcolor("black",alpha.f=0.7)) +
    geom_vline(xintercept = meancentr,linetype=2, color=adjustcolor("black",alpha.f=0.7)) + 
    theme(legend.position="none") +
    scale_radius(range=c(10*(1+size), 30*(1+size)))+
    labs(x = "Centrality",y = "Impact") +
    xlim(xlimits)+
    ylim(ylimits)+
    ggtitle(paste("Clusters by ", toupper(substr(analysis,1,1)),substr(analysis,2,nchar(analysis))," Coupling" ,sep="")) +
    theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  df_lab <- df_lab[,c(1,7,15,8,4)]
  names(df_lab)=c(analysis, "Cluster","ClusterFrequency", "ClusterColor","NormalizedLocalCitationScore")
  df_lab$ClusterName <- df$label[df_lab$Cluster]
  
  row.names(df)=NULL
  df <- df %>% rename(items = words)
  
  results=list(map=g, clusters=df, data=df_lab,nclust=dim(df)[1], NCS = D, net=Net)
  return(results)
}

coupling <- function(M,field, analysis){
  if (field=="TI") field <- "TI_TM"
  if (field=="AB") field <- "AB_TM"
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

network <- function(M, analysis,field, stemming, n){
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
  
  return(Net)
}
