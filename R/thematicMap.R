globalVariables(".")
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
#' @param ngrams is an integer between 1 and 4. It indicates the type of n-gram to extract from texts. 
#' An n-gram is a contiguous sequence of n terms. The function can extract n-grams composed by 1, 2, 3 or 4 terms. Default value is \code{ngrams=1}.
#' @param stemming is logical. If it is TRUE the word (from titles or abstracts) will be stemmed (using the Porter's algorithm).
#' @param size is numerical. It indicates del size of the cluster circles and is a number in the range (0.01,1).
#' @param n.labels is integer. It indicates how many labels associate to each cluster. Default is \code{n.labels = 1}.
#' @param community.repulsion is a real. It indicates the repulsion force among network communities. It is a real number between 0 and 1. Default is \code{community.repulsion = 0.1}.
#' @param repel is logical. If it is TRUE ggplot uses geom_label_repel instead of geom_label.
#' @param remove.terms is a character vector. It contains a list of additional terms to delete from the documents before term extraction. The default is \code{remove.terms = NULL}.
#' @param synonyms is a character vector. Each element contains a list of synonyms, separated by ";",  that will be merged into a single term (the first word contained in the vector element). The default is \code{synonyms = NULL}.
#' @param cluster is a character. It indicates the type of cluster to perform among ("optimal", "louvain","leiden", "infomap","edge_betweenness","walktrap", "spinglass", "leading_eigen", "fast_greedy").
#' @param subgraphs is a logical. If TRUE cluster subgraphs are returned.
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
#' \dontrun{
#' data(scientometrics, package = "bibliometrixData")
#' res <- thematicMap(scientometrics, field = "ID", n = 250, minfreq = 5, size = 0.5, repel = TRUE)
#' plot(res$map)
#' }
#'
#' @seealso \code{\link{biblioNetwork}} function to compute a bibliographic network.
#' @seealso \code{\link{cocMatrix}} to compute a bibliographic bipartite network.
#' @seealso \code{\link{networkPlot}} to plot a bibliographic network.
#'
#' @export

thematicMap <- function(M, field="ID", n=250, minfreq=5, ngrams=1, stemming=FALSE, size=0.5, n.labels=1, community.repulsion = 0.1, repel=TRUE, remove.terms=NULL, synonyms=NULL, cluster="walktrap", subgraphs=FALSE){
  
  minfreq <- max(2,floor(minfreq*nrow(M)/1000))
  
  switch(field,
         ID={
           NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", n = n, sep = ";", remove.terms=remove.terms, synonyms = synonyms)
           TERMS=tolower(M$ID)
         },
         DE={
           NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "author_keywords", n = n, sep = ";", remove.terms=remove.terms, synonyms = synonyms)
           TERMS=tolower(M$DE)
         },
         TI={
           #if(!("TI_TM" %in% names(values$M))){values$M=termExtraction(values$M,Field="TI",verbose=FALSE, stemming = input$stemming)}
           M=termExtraction(M,Field="TI", ngrams=ngrams, verbose=FALSE, stemming = stemming, remove.terms=remove.terms, synonyms = synonyms)
           NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "titles", n = n, sep = ";")
           
         },
         AB={
           #if(!("AB_TM" %in% names(values$M))){values$M=termExtraction(values$M,Field="AB",verbose=FALSE, stemming = input$stemming)}
           M=termExtraction(M,Field="AB", ngrams=ngrams, verbose=FALSE, stemming = stemming, remove.terms=remove.terms, synonyms = synonyms)
           NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "abstracts", n = n, sep = ";")
           
         })
  
  if (nrow(NetMatrix)>0){
    Net <- networkPlot(NetMatrix, normalize="association", Title = "Keyword co-occurrences",type="auto",
                     labelsize = 2, halo = F,cluster=cluster,remove.isolates=TRUE,community.repulsion = community.repulsion,
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

  centr <- networkStat(Net$graph,stat="all", type="closeness")$vertex
  df_lab <- data.frame(sC=sC,words=word,groups=group,color=color,cluster_label="NA")
   
  ## new code using tidyvverse
  df_lab <- df_lab %>% 
    dplyr::filter(.data$sC>=minfreq) %>% 
    group_by(.data$groups) %>% 
    mutate(freq = sum(.data$sC),
           cluster_label = .data$words[which.max(.data$sC)]) 
  
  sEij <- triu(sEij)
  df_lab_top <- df_lab %>% select(.data$words,.data$groups)
  
  sEij_df <- as.matrix(sEij) %>% 
    data.frame() %>% 
    rename_with(~ row.names(sEij), .cols = colnames(.)) %>% 
    mutate(words1 = row.names(sEij)) %>% 
    pivot_longer(cols=!.data$words1, names_to = "words2", values_to = "eij") %>% 
    dplyr::filter(.data$eij>0) %>% 
    left_join(df_lab_top, by=c("words1" = "words")) %>% 
    left_join(df_lab_top, by=c("words2" = "words")) %>% 
    rename(groups =.data$groups.x,
           groups2 =.data$groups.y)
    
  df_lab_top <- df_lab %>% select(.data$groups, .data$cluster_label,.data$color, .data$freq) %>% slice_head(n=1)
  
  df <- sEij_df %>% 
    dplyr::filter(.data$words1 %in% unique(df_lab$words) & .data$words2 %in% unique(df_lab$words)) %>% 
    group_by(.data$groups) %>% 
    mutate(ext = as.numeric(.data$groups!=.data$groups2)) %>% 
    summarize(n=length(unique(.data$words1)),
              centrality = sum(.data$eij*.data$ext),
              density = sum((.data$eij*(1-.data$ext))/.data$n)*100
              ) %>% 
    mutate(rcentrality=rank(.data$centrality),
      rdensity=rank(.data$density)
    ) %>% 
    left_join(., df_lab_top, by = "groups") %>% 
    rename(label = .data$cluster_label)
  
  
  meandens=mean(df$rdensity)
  meancentr=mean(df$rcentrality)
  rangex=max(c(meancentr-min(df$rcentrality),max(df$rcentrality)-meancentr))
  rangey=max(c(meandens-min(df$rdensity),max(df$rdensity)-meandens))
  
  df <- df_lab %>% group_by(.data$groups) %>% #dplyr::filter(.data$sC>1) %>% 
    arrange(-.data$sC, .by_group = TRUE) %>% 
    dplyr::slice_max(n=10, .data$sC, with_ties=FALSE) %>%
    summarise(wordlist = paste(.data$words,.data$sC,collapse="\n"),
              name_full = paste(.data$words[1:min(n.labels,n())], collapse="\n")) %>% 
    right_join(., df, by = "groups") %>% 
    rename(name = .data$label,
      words = .data$wordlist)

  xlimits=c(meancentr-(rangex*1.2),meancentr+rangex*1.2)
  ylimits=c(meandens-(rangey*1.2),meandens+rangey*1.2)
  

  annotations <- data.frame(
    xpos = sort(c(xlimits,xlimits)),
    ypos = c(ylimits, ylimits),
    words = c("Emerging or\nDeclining Themes","Niche Themes","Basic Themes ","Motor Themes "),
    hjustvar = c(0,0,1,1) ,
    vjustvar = c(0,1.0,0,1))
  
  data("logo",envir=environment())
  logo <- grid::rasterGrob(logo,interpolate = TRUE)
  
  x <- c(max(df$rcentrality)-0.02-diff(range(df$rcentrality))*0.125, max(df$rcentrality)-0.02)+0.6
  y <- c(min(df$rdensity),min(df$rdensity)+diff(range(df$rdensity))*0.125)

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
            panel.background = element_rect(fill = '#FFFFFF'),
            axis.line.x = element_line(color="black",size=0.5),
            axis.line.y = element_line(color="black",size=0.5),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()
      ) + annotation_custom(logo, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
    
  names(df_lab)=c("Occurrences", "Words", "Cluster", "Color","Cluster_Label", "Cluster_Frequency")
  
  df_lab <- df_lab %>% 
    arrange(.data$Cluster) %>% 
    dplyr::filter(!is.na(.data$Color)) %>% 
    data.frame() %>% 
    mutate(Cluster = as.numeric(factor(.data$Cluster)))

  ## Add centrality measure to words
  cluster_res <- Net$cluster_res %>% 
    dplyr::select(!.data$cluster)
  
  df_lab <- df_lab %>% 
    dplyr::left_join(cluster_res, by=c("Words" = "vertex"))
  
  documentToClusters <- clusterAssignment(M, words=df_lab, field, remove.terms, synonyms, threshold=0.5)
  
  params <- list(field=field, 
                 n=n, 
                 minfreq=minfreq, 
                 ngrams=ngrams, 
                 stemming=stemming, 
                 size=size, 
                 n.labels=n.labels, 
                 community.repulsion = community.repulsion, 
                 repel=repel, 
                 remove.terms=remove.terms, 
                 synonyms=synonyms, 
                 cluster=cluster)
  params <- data.frame(params=names(unlist(params)),values=unlist(params), row.names = NULL)
  
  ## cluster subgraphs
  if (isTRUE(subgraphs)){
    gcl <- list()
    color <- unique(df$color)
    
    for (i in color){
      ind <- which(V(Net$graph)$color==i)
      gcl[[i]] <- induced_subgraph(graph = Net$graph, vids = ind, impl = "create_from_scratch")
    }
  } else {
    gcl <- NA
  }
  
  results=list(map=g, clusters=df, words=df_lab,nclust=dim(df)[1], net=Net, subgraphs=gcl, documentToClusters=documentToClusters, params=params)
return(results)
}

# Probability calculation fro cluster assignment
clusterAssignment <- function(M, words, field, remove.terms, synonyms, threshold){

  #### integrate stopwords and synonyms in M original field  
  if (field %in% c("AB","TI")) field <- paste0(field,"_TM")
  Fi<-strsplit(M[,field],";")
  nf <- lengths(Fi)
  allField <- data.frame(terms=trim.leading(unlist(Fi)), SR=rep(M$SR,nf))
  # remove terms
  if (!is.null(remove.terms)){
    allField <- anti_join(allField,data.frame(terms=trimws(toupper(remove.terms))), by="terms")
  }
    # Merge synonyms in the vector synonyms
  if (!is.null(synonyms)){
    s <- strsplit(toupper(synonyms),";")
    snew <- unlist(lapply(s, function(l) l[1]))
    sold <- lapply(s, function(l) trim.leading(l[-1]))
    syn <- data.frame(new=rep(snew, lengths(sold)), terms=unlist(sold))
    allField <- allField %>% left_join(syn, by="terms")
    ind <- which(!is.na(allField$new))
    allField$terms[ind] <- allField$new[ind]
    allField <- allField[c("SR", "terms")]

  }
### stop integration in M  
  
  words <- words %>% 
    mutate(p_w = 1/.data$Occurrences) %>% 
    group_by(.data$Cluster) %>% 
    rename(p_c = .data$pagerank_centrality)
    #mutate(p_c = 1/length(.data$Cluster))

  TERMS <- allField %>% 
    mutate(terms = .data$terms %>% tolower()) %>% 
    left_join(words, by = c("terms" = "Words"))
  
  TERMS <- TERMS %>% 
    group_by(.data$SR) %>% 
    mutate(pagerank = sum(.data$p_c,na.rm = T)) %>% 
    group_by(.data$SR, .data$Cluster_Label) %>% 
    summarize(weigth = sum(.data$p_w),
              pagerank = max(.data$pagerank, na.rm=TRUE)) %>% 
    mutate(p = .data$weigth/sum(.data$weigth, na.rm=T)) %>% 
    drop_na(.data$Cluster_Label) %>% 
    ungroup()
  
  TERMS <- TERMS %>% 
    select(-.data$weigth) %>% 
    group_by(.data$SR)
  
  ## Assign docs to cluser with p_max>=threshold
  TERMS_Max <- TERMS %>% 
    dplyr::filter(.data$p>=threshold) %>% 
    group_by(.data$SR) %>% 
    slice_max(order_by = .data$p, n=1) %>% 
    summarize(Assigned_cluster = paste(.data$Cluster_Label, collapse = ";"))
  
  ### doc pagerank centrality for the assigned cluster 
  TERMS_pagerank <- TERMS %>% 
    select(!.data$p) %>%
    left_join(TERMS_Max, by="SR") %>% 
    dplyr::filter(.data$Cluster_Label==.data$Assigned_cluster) %>% 
    select(.data$SR,.data$pagerank)

  TERMS <- TERMS %>% 
    select(!.data$pagerank) %>% 
    pivot_wider(names_from = .data$Cluster_Label, values_from = .data$p) %>% 
    left_join(TERMS_Max, by = "SR") %>% 
    left_join(TERMS_pagerank, by="SR")
  
  if (!("DI" %in% names(M))) M$DI <- NA
  
  year <- as.numeric(substr(Sys.time(),1,4))+1
  
  TERMS <- M %>% 
    mutate(TCpY =.data$TC/(year-.data$PY)) %>% 
    group_by(.data$PY) %>% 
    mutate(NTC = .data$TC/mean(.data$TC, na.rm=TRUE)) %>% 
    ungroup() %>% 
    select(.data$DI, .data$AU, .data$TI, .data$SO, .data$PY,.data$TC, .data$TCpY, .data$NTC,.data$SR) %>% 
    left_join(TERMS, by = "SR") %>% 
    mutate_if(is.numeric, ~replace_na(., 0)) %>% 
    group_by(.data$Assigned_cluster) %>% 
    arrange(desc(.data$TC), .by_group = TRUE)
  
  
  return(TERMS)
}
