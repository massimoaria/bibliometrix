#' Creating and plotting conceptual structure map of a scientific field
#'
#' The function \code{conceptualStructure} creates a conceptual structure map of 
#' a scientific field performing Correspondence Analysis (CA), Multiple Correspondence Analysis (MCA) or Metric Multidimensional Scaling (MDS) and Clustering 
#' of a bipartite network of terms extracted from keyword, title or abstract fields.
#' 
#' @param M is a data frame obtained by the converting function
#'   \code{\link{convert2df}}. It is a data matrix with cases corresponding to
#'   articles and variables to Field Tag in the original ISI or SCOPUS file.
#' @param field is a character object. It indicates one of the field tags of the
#'   standard ISI WoS Field Tag codify. 
#'   field can be equal to one of these tags:
#'   \tabular{lll}{ 
#'   \code{ID}\tab   \tab Keywords Plus associated by ISI or SCOPUS database\cr 
#'   \code{DE}\tab   \tab Author's keywords\cr 
#'   \code{ID_TM}\tab   \tab Keywords Plus stemmed through the Porter's stemming algorithm\cr
#'   \code{DE_TM}\tab   \tab Author's Keywords stemmed through the Porter's stemming algorithm\cr
#'   \code{TI}\tab   \tab Terms extracted from titles\cr
#'   \code{AB}\tab   \tab Terms extracted from abstracts}
#' @param method is a character object. It indicates the factorial method used to create the factorial map. Use \code{method="CA"} for Correspondence Analysis,
#'  \code{method="MCA"} for Multiple Correspondence Analysis or \code{method="MDS"} for Metric Multidimensional Scaling. The default is \code{method="MCA"}
#' @param minDegree is an integer. It indicates the minimum occurrences of terms to analize and plot. The default value is 2.
#' @param clust is an integer or a character. If clust="auto", the number of cluster is chosen automatically, otherwise clust can be an integer between 2 and 8.
#' @param k.max is an integer. It indicates the maximum number of cluster to keep. The default value is 5. The max value is 20.
#' @param stemming is logical. If TRUE the Porter's Stemming algorithm is applied to all extracted terms. The default is \code{stemming = FALSE}.
#' @param labelsize is an integer. It indicates the label size in the plot. Default is \code{labelsize=10}
#' @param quali.supp is a vector indicating the indexes of the categorical supplementary variables. It is used only for CA and MCA.
#' @param quanti.supp is a vector indicating the indexes of the quantitative supplementary variables. It is used only for CA and MCA.
#' @param documents is an integer. It indicates the number of documents per cluster to plot in the factorial map. The default value is 2. It is used only for CA and MCA.
#' @param graph is logical. If TRUE the function plots the maps otherwise they are saved in the output object. Default value is TRUE
#' @return It is an object of the class \code{list} containing the following components:
#'
#' \tabular{lll}{
#' net \tab  \tab bipartite network\cr
#' res \tab       \tab Results of CA, MCA or MDS method\cr
#' km.res \tab      \tab Results of cluster analysis\cr
#' graph_terms \tab      \tab Conceptual structure map (class "ggplot2")\cr
#' graph_documents_Contrib \tab      \tab Factorial map of the documents with the highest contributes (class "ggplot2")\cr
#' graph_docuemnts_TC \tab      \tab Factorial map of the most cited documents (class "ggplot2")}
#' 
#' @examples
#' # EXAMPLE Conceptual Structure using Keywords Plus
#'
#' data(scientometrics)
#'
#' CS <- conceptualStructure(scientometrics, field="ID", method="CA", 
#'              stemming=FALSE, minDegree=3, k.max = 5)
#' 
#' @seealso \code{\link{termExtraction}} to extract terms from a textual field (abstract, title, 
#' author's keywords, etc.) of a bibliographic data frame.
#' @seealso \code{\link{biblioNetwork}} to compute a bibliographic network.
#' @seealso \code{\link{cocMatrix}} to compute a co-occurrence matrix.
#' @seealso \code{\link{biblioAnalysis}} to perform a bibliometric analysis.
#' 
#' @export
conceptualStructure<-function(M,field="ID", method="MCA", quali.supp=NULL, quanti.supp=NULL, minDegree=2, clust="auto", k.max=5, stemming=FALSE, labelsize=10,documents=2, graph=TRUE){
  
  #cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  cbPalette <- c(brewer.pal(9, 'Set1')[-6], brewer.pal(8, 'Set2')[-7], brewer.pal(12, 'Paired')[-11],brewer.pal(12, 'Set3')[-c(2,8,12)])
  
  if (!is.null(quali.supp)){
    QSUPP=data.frame(M[,quali.supp])
    names(QSUPP)=names(M)[quali.supp]
    row.names(QSUPP)=tolower(row.names(M))
  }
  
  if (!is.null(quanti.supp)){
    SUPP=data.frame(M[,quanti.supp])
    names(SUPP)=names(M)[quanti.supp]
    row.names(SUPP)=tolower(row.names(M))
  }
  binary=FALSE
  if (method=="MCA"){binary=TRUE}
  
  switch(field,
         ID={
           # Create a bipartite network of Keyword plus
           #
           # each row represents a manuscript
           # each column represents a keyword (1 if present, 0 if absent in a document)
           CW <- cocMatrix(M, Field = "ID", type="matrix", sep=";",binary=binary)
           # Define minimum degree (number of occurrences of each Keyword)
           CW=CW[,colSums(CW)>=minDegree]
           # Delete empty rows
           CW=CW[,!(colnames(CW) %in% "NA")]
           CW=CW[rowSums(CW)>0,]
           
  
         },
         DE={
           CW <- cocMatrix(M, Field = "DE", type="matrix", sep=";",binary=binary)
           # Define minimum degree (number of occurrences of each Keyword)
           CW=CW[,colSums(CW)>=minDegree]
           # Delete empty rows
           CW=CW[rowSums(CW)>0,]
           CW=CW[,!(colnames(CW) %in% "NA")]
  
         },
         ID_TM={
           M=termExtraction(M,Field="ID",remove.numbers=TRUE, stemming=stemming, language="english", remove.terms=NULL, keep.terms=NULL, verbose=FALSE)
           
           CW <- cocMatrix(M, Field = "ID_TM", type="matrix", sep=";",binary=binary)
           # Define minimum degree (number of occurrences of each Keyword)
           CW=CW[,colSums(CW)>=minDegree]
           CW=CW[,!(colnames(CW) %in% "NA")]
           # Delete empty rows
           CW=CW[rowSums(CW)>0,]
       
           
         },
         DE_TM={
           M=termExtraction(M,Field="DE",remove.numbers=TRUE, stemming=stemming, language="english", remove.terms=NULL, keep.terms=NULL, verbose=FALSE)
           
           CW <- cocMatrix(M, Field = "DE_TM", type="matrix", sep=";",binary=binary)
           # Define minimum degree (number of occurrences of each Keyword)
           CW=CW[,colSums(CW)>=minDegree]
           # Delete empty rows
           CW=CW[,!(colnames(CW) %in% "NA")]
           CW=CW[rowSums(CW)>0,]
          
         },
         TI={
           M=termExtraction(M,Field="TI",remove.numbers=TRUE, stemming=stemming, language="english", remove.terms=NULL, keep.terms=NULL, verbose=FALSE)
           
           CW <- cocMatrix(M, Field = "TI_TM", type="matrix", sep=";",binary=binary)
           # Define minimum degree (number of occurrences of each Keyword)
           CW=CW[,colSums(CW)>=minDegree]
           # Delete empty rows
           CW=CW[,!(colnames(CW) %in% "NA")]
           CW=CW[rowSums(CW)>0,]
          
         },
         AB={
           M=termExtraction(M,Field="AB",remove.numbers=TRUE, stemming=stemming, language="english", remove.terms=NULL, keep.terms=NULL, verbose=FALSE)
           
           CW <- cocMatrix(M, Field = "AB_TM", type="matrix", sep=";",binary=binary)
           # Define minimum degree (number of occurrences of each Keyword)
           CW=CW[,colSums(CW)>=minDegree]
           # Delete empty rows
           CW=CW[rowSums(CW)>0,]
           CW=CW[,!(colnames(CW) %in% "NA")]
           # Recode as dataframe
           #CW=data.frame(apply(CW,2,factor))
         }
  )
  
  
  colnames(CW)=tolower(colnames(CW))
  rownames(CW)=tolower(rownames(CW))
  p=dim(CW)[2] 
  quali=NULL
  quanti=NULL
  # Perform Multiple Correspondence Analysis (MCA)
  if (!is.null(quali.supp)){
    ind=which(row.names(QSUPP) %in% row.names(CW))
    QSUPP=as.data.frame(QSUPP[ind,])
    CW=cbind(CW,QSUPP)
    quali=(p+1):dim(CW)[2]
    names(CW)[quali]=names(M)[quali.supp]
  }
  if (!is.null(quanti.supp)){
    ind=which(row.names(SUPP) %in% row.names(CW))
    SUPP=as.data.frame(SUPP[ind,])
    CW=cbind(CW,SUPP)
    quanti=(p+1+length(quali)):dim(CW)[2]
    names(CW)[quanti]=names(M)[quanti.supp]
  }
  
  results <- factorial(CW,method=method,quanti=quanti,quali=quali)
  res.mca <- results$res.mca
  df <- results$df
  docCoord <- results$docCoord
  df_quali <- results$df_quali
  df_quanti <- results$df_quanti
  
  ### Total Citations of documents
  if ("TC" %in% names(M) & method!="MDS"){docCoord$TC=as.numeric(M[toupper(rownames(docCoord)),"TC"])}
  
  
  # Selection of optimal number of clusters (gap statistics)
  #a=fviz_nbclust((df), kmeans, method = "gap_stat",k.max=k.max)['data']$data$y
  km.res=hclust(dist(df),method="average")
  
  if (clust=="auto"){
      clust=min((length(km.res$height)-which.max(diff(km.res$height))+1),k.max)
      }else{clust=max(2,min(as.numeric(clust),k.max))}
  
  km.res$data=df
  km.res$cluster=cutree(km.res,k=clust)
  km.res$data.clust=cbind(km.res$data,km.res$cluster)
  names(km.res$data.clust)[3]="clust"
  centers<- km.res$data.clust %>% group_by(.data$clust) %>% 
    summarise("Dim.1"=mean(.data$Dim.1),"Dim.2"=mean(.data$Dim.2)) %>% 
    as.data.frame()
  
  km.res$centers=centers[,c(2,3,1)]
  
  b=fviz_cluster(km.res, stand=FALSE, data = df,labelsize=labelsize, repel = TRUE)+
    theme_minimal()+
    scale_color_manual(values = cbPalette[1:clust])+
    scale_fill_manual(values = cbPalette[1:clust]) +
    labs(title= paste("Conceptual Structure Map - method: ",method,collapse="",sep="")) +
    geom_point() +
    geom_hline(yintercept=0, linetype="dashed", color = adjustcolor("grey40",alpha.f = 0.7))+
    geom_vline(xintercept=0, linetype="dashed", color = adjustcolor("grey40",alpha.f = 0.7))+
    theme(panel.border =  element_rect(fill=NA, size = 0.3, linetype = 'dashed', colour = adjustcolor("gray60",alpha.f = 0.7)),
          text = element_text(size=labelsize),
          axis.title=element_text(size=labelsize,face="bold"),
          plot.title=element_text(size=labelsize+1,face="bold"),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.3, linetype = 'dashed', colour = adjustcolor("gray60",alpha.f = 0.7)),
          panel.grid.minor = element_blank())
   if (method!="MDS"){
     b=b+xlab(paste("Dim 1 (",round(res.mca$eigCorr$perc[1],2),"%)",sep=""))+
       ylab(paste("Dim 2 (",round(res.mca$eigCorr$perc[2],2),"%)",sep=""))
   }else{b=b+xlab("Dim 1")+ylab("Dim 2")}
  
  if (!is.null(quali.supp)){
    s_df_quali=df_quali[(abs(df_quali[,1]) >= quantile(abs(df_quali[,1]),0.75) | abs(df_quali[,2]) >= quantile(abs(df_quali[,2]),0.75)),]
    names(s_df_quali)=c("x","y")
    s_df_quali$label=row.names(s_df_quali)
    x=s_df_quali$x
    y=s_df_quali$y
    label=s_df_quali$label
    b=b+geom_point(aes(x=x,y=y),data=s_df_quali,colour="red",size=1) +
      geom_label_repel(aes(x=x,y=y,label=label,size=1),data=s_df_quali)
  }
  
  if (!is.null(quanti.supp)){
    names(df_quanti)=c("x","y")
    df_quanti$label=row.names(df_quanti)
    x=df_quanti$x
    y=df_quanti$y
    label=df_quanti$label
    b=b+geom_point(aes(x=x,y=y),data=df_quanti,colour="blue",size=1) +
      geom_label_repel(aes(x=x,y=y,label=label,size=1),data=df_quanti) +
      geom_segment(data=df_quanti,aes(x=0,y=0,xend = x, yend = y), size=1.5,arrow = arrow(length = unit(0.3,"cm")))
  }
  b=b + theme(legend.position="none")
  
  if (isTRUE(graph)){plot(b)}
  
  b_dend <- fviz_dend(km.res, rect = TRUE, k=clust, 
                                       labelsize=labelsize, main="Topic Dendrogram",
                                       k_colors = cbPalette[clust:1])+ 
    #scale_color_manual(values = cbPalette[(clust+1):1])+
    #scale_fill_manual(values = cbPalette[(clust+1):1])+
    theme(plot.title=element_text(size=labelsize+1,face="bold"), 
          axis.title=element_text(size=labelsize,face="bold") ,
          panel.background = element_rect(fill = "white",
                                          colour = "white"),
          #size = 1, linetype = "solid"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  if (isTRUE(graph)){plot(b_dend)}
  
  if (method !="MDS"){
  ## Factorial map of most contributing documents
  
    
  if (documents>dim(docCoord)[1]){documents=dim(docCoord)[1]}
    
    centers=data.frame(dim1=km.res$centers[,1],dim2=km.res$centers[,2])
    centers$color=cbPalette[1:dim(centers)[1]]
    row.names(centers)=paste("cluster",as.character(1:dim(centers)[1]),sep="")
    #A=docCoord[1:documents,1:2]
    #A=docCoord[,1:2]
    A=euclDist(docCoord[,1:2],centers)
    docCoord$Cluster=A$color
    #A=A[1:documents,]
    A$color=cbPalette[A$color]
    
    A$contrib <- docCoord$contrib
    A <- A %>%
      mutate(names=row.names(A)) %>%
      group_by(.data$color) %>%
      top_n(.data$contrib,n=documents) %>%
      select(!"contrib")%>%
      as.data.frame() 
    
    row.names(A) <- A$names
    A <- A[,-4]
    
    names(centers)=names(A)
    A=rbind(A,centers)
    x=A$dim1
    y=A$dim2
    A[,4]=row.names(A)
    
    names(A)[4]="nomi"
    
    df_all=rbind(as.matrix(df),as.matrix(A[,1:2]))
    rangex=c(min(df_all[,1]),max(df_all[,1]))
    rangey=c(min(df_all[,2]),max(df_all[,2]))

    b_doc=ggplot(aes(x=.data$dim1,y=.data$dim2,label=.data$nomi),data=A)+
      geom_point(size = 2, color = A$color)+
      labs(title= "Factorial map of the documents with the highest contributes") +
      geom_label_repel(box.padding = unit(0.5, "lines"),size=(log(labelsize*3)), fontface = "bold", 
                       fill=adjustcolor(A$color,alpha.f=0.6), color = "white", segment.alpha=0.5, segment.color="gray")+
      scale_x_continuous(limits = rangex, breaks=seq(round(rangex[1]), round(rangex[2]), 1))+
      scale_y_continuous(limits = rangey, breaks=seq(round(rangey[1]), round(rangey[2]), 1))+
      geom_hline(yintercept=0, linetype="dashed", color = adjustcolor("grey40",alpha.f = 0.7))+
      geom_vline(xintercept=0, linetype="dashed", color = adjustcolor("grey40",alpha.f = 0.7))+
      theme(plot.title=element_text(size=labelsize+1,face="bold"), 
            axis.title=element_text(size=labelsize,face="bold") ,
            panel.border =  element_rect(fill=NA, size = 0.3, linetype = 'dashed', colour = adjustcolor("gray60",alpha.f = 0.7)),
            panel.background = element_rect(fill = "white",
                                            colour = "white"),
            #size = 1, linetype = "solid"),
            panel.grid.major = element_line(size = 0.3, linetype = 'dashed', colour = adjustcolor("gray90",alpha.f = 0.7)),
            panel.grid.minor = element_blank())
    
    if (method!="MDS"){
      b_doc=b_doc+xlab(paste("Dim 1 (",round(res.mca$eigCorr$perc[1],2),"%)",sep=""))+
        ylab(paste("Dim 2 (",round(res.mca$eigCorr$perc[2],2),"%)",sep=""))
    }else{b_doc=b_doc+xlab("Dim 1")+ylab("Dim 2")}
      
    if (isTRUE(graph)){(plot(b_doc))}
    
    ## Factorial map of the most cited documents
    docCoord=docCoord[order(-docCoord$TC),]
    #B=docCoord[1:documents,1:2]
    B=euclDist(docCoord[,1:2],centers)
    B$color=cbPalette[B$color]
    
    B$TC <- docCoord$TC
    B <- B %>%
      mutate(names=row.names(B)) %>%
      group_by(.data$color) %>%
      top_n(.data$TC,n=documents) %>%
      select(!"TC")%>%
      as.data.frame() 
    
    row.names(B) <- B$names
    B <- B[,-4]
    B=rbind(B,centers)
    x=B$dim1
    y=B$dim2
    B[,4]=row.names(B)
    names(B)[4]="nomi"
    df_all_TC=rbind(as.matrix(df),as.matrix(B[,1:2]))
    rangex=c(min(df_all_TC[,1]),max(df_all_TC[,1]))
    rangey=c(min(df_all_TC[,2]),max(df_all_TC[,2]))
    
    b_doc_TC=ggplot(aes(x=.data$dim1,y=.data$dim2,label=.data$nomi),data=B)+
      geom_point(size = 2, color = B$color)+
      labs(title= "Factorial map of the most cited documents") +
      geom_label_repel(box.padding = unit(0.5, "lines"),size=(log(labelsize*3)), fontface = "bold", 
                       fill=adjustcolor(B$color,alpha.f=0.6), color = "white", segment.alpha=0.5, segment.color="gray")+
      scale_x_continuous(limits = rangex, breaks=seq(round(rangex[1]), round(rangex[2]), 1))+
      scale_y_continuous(limits = rangey, breaks=seq(round(rangey[1]), round(rangey[2]), 1))+
      xlab(paste("Dim 1 (",round(res.mca$eigCorr$perc[1],2),"%)",sep=""))+
      ylab(paste("Dim 2 (",round(res.mca$eigCorr$perc[2],2),"%)",sep=""))+
      geom_hline(yintercept=0, linetype="dashed", color = adjustcolor("grey60",alpha.f = 0.7))+
      geom_vline(xintercept=0, linetype="dashed", color = adjustcolor("grey60",alpha.f = 0.7))+
      theme(plot.title=element_text(size=labelsize+1,face="bold"), 
            axis.title=element_text(size=labelsize,face="bold") ,
            panel.border =  element_rect(fill=NA, size = 0.3, linetype = 'dashed', colour = adjustcolor("gray60",alpha.f = 0.7)),
            panel.background = element_rect(fill = "white", colour = "white"),
           panel.grid.major = element_line(size = 0.3, linetype = 'dashed', colour = adjustcolor("gray90",alpha.f = 0.7)),
           panel.grid.minor = element_blank())
      
    
    if (isTRUE(graph)){plot(b_doc_TC)}

    semanticResults=list(net=CW,res=res.mca,km.res=km.res,graph_terms=b,graph_dendogram=b_dend,graph_documents_Contrib=b_doc,graph_documents_TC=b_doc_TC,docCoord=docCoord)
    
  }else{

    semanticResults=list(net=CW,res=res.mca,km.res=km.res,graph_terms=b,graph_dendogram=b_dend,graph_documents_Contrib=NULL,graph_documents_TC=NULL,docCoord=NULL)
    }
  
  
  
  return(semanticResults)
}


factorial<-function(X,method,quanti,quali){
  df_quali=data.frame()
  df_quanti=data.frame()
  
  switch(method,
         ### CORRESPONDENCE ANALYSIS ###
         CA={
           res.mca <- CA(X, quanti.sup=quanti, quali.sup=quali, ncp=2, graph=FALSE)
           
           # Get coordinates of keywords 
           coord=get_ca_col(res.mca)
           df=data.frame(coord$coord)
           if (!is.null(quali)){
             df_quali=data.frame(res.mca$quali.sup$coord)
           }
           if (!is.null(quanti)){
             df_quanti=data.frame(res.mca$quanti.sup$coord)
           }
           coord_doc=get_ca_row(res.mca)
           df_doc=data.frame(coord_doc$coord)
           
           },
         ### MULTIPLE CORRESPONDENCE ANALYSIS ###
         MCA={
           if(length(quanti)>0){
           X[,-quanti]=data.frame(apply(X[,-quanti],2,factor))} else{X=data.frame(apply(X,2,factor))}
           res.mca <- MCA(X, quanti.sup=quanti, quali.sup=quali, ncp=2, graph=FALSE)
           # Get coordinates of keywords (we take only categories "1"")
           coord=get_mca_var(res.mca)
           df=data.frame(coord$coord)[seq(2,dim(coord$coord)[1],by=2),]
           row.names(df)=gsub("_1","",row.names(df))
           if (!is.null(quali)){
             df_quali=data.frame(res.mca$quali.sup$coord)[seq(1,dim(res.mca$quali.sup$coord)[1],by=2),]
             row.names(df_quali)=gsub("_1","",row.names(df_quali))
           }
           if (!is.null(quanti)){
             df_quanti=data.frame(res.mca$quanti.sup$coord)[seq(1,dim(res.mca$quanti.sup$coord)[1],by=2),]
             row.names(df_quanti)=gsub("_1","",row.names(df_quanti))
           } 
           coord_doc=get_mca_ind(res.mca)
           df_doc=data.frame(coord_doc$coord)
           
           },
         MDS={
           NetMatrix=Matrix::crossprod(X,X)
           Net=1-normalizeSimilarity(NetMatrix, type="association")
           Matrix::diag(Net)=0
           #Net=as.matrix(Net)
           res.mca <- Net %>%
             #dist() %>%          
             cmdscale()  
           colnames(res.mca) <- c("Dim.1", "Dim.2")
           df=data.frame(res.mca)
           row.names(df)=row.names(Net)
         }
  )
  
  if (method!="MDS"){
  #
    docCoord=as.data.frame(cbind(df_doc,rowSums(coord_doc$contrib)))
    names(docCoord)=c("dim1","dim2","contrib")
    docCoord=docCoord[order(-docCoord$contrib),]
    
    # Benzecrì eigenvalue correction
    res.mca <- eigCorrection(res.mca)
    
    results=list(res.mca=res.mca,df=df,df_doc=df_doc,df_quali=df_quali,df_quanti=df_quanti,docCoord=docCoord)
    
    
  }else{
    results=list(res.mca=res.mca,df=df,df_doc=NA,df_quali=NA,df_quanti=NA,docCoord=NA)
  }
  return(results)
}

euclDist<-function(x,y){
  
  df=as.data.frame(matrix(NA,dim(x)[1],dim(y)[1]))
  row.names(df)=row.names(x)
  colnames(df)=row.names(y)
  for (i in 1:dim(y)[1]){
    ref=y[i,1:2]
    df[,i]=apply(x,1,function(x)sqrt(sum((x-ref)^2)))
    
  }
  x$color=apply(df,1,function(m){which(m==min(m))})
  return(x)
}

eigCorrection <- function(res) {
  # Benzecri correction calculation
  
  n <- nrow(res$eig)
  
  
  e <- res$eig[,1]
  eigBenz <- ((n / (n - 1)) ^ 2) * ((e - (1 / n)) ^ 2)
  eigBenz[e< 1/n] <- 0
  perc <- eigBenz / sum(eigBenz) * 100
  cumPerc = cumsum(perc)
  res$eigCorr <- data.frame(eig=e, eigBenz=eigBenz, perc=perc, cumPerc=cumPerc)
  return(res)
}
