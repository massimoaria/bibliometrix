#' Creating and plotting conceptual structure map of a scientific field
#'
#' The function \code{conceptualStructure} creates a conceptual structure map of 
#' a scientific field performing Multiple Correspondence Analysis (MCA) and Clustering 
#' of a bipartite network of terms extracted from keyword, title or abstract fields.
#' 
#' @param M is a data frame obtained by the converting function
#'   \code{\link{convert2df}}. It is a data matrix with cases corresponding to
#'   articles and variables to Field Tag in the original ISI or SCOPUS file.
#' @param field is a character object. It indicates one of the field tags of the
#'   standard ISI WoS Field Tag codify. 
#'   field can be equal to one of this tags:
#'   \tabular{lll}{ 
#'   \code{ID}\tab   \tab Keywords Plus associated by ISI or SCOPUS database\cr 
#'   \code{DE}\tab   \tab Author's keywords\cr 
#'   \code{ID_TM}\tab   \tab Keywords Plus stemmed through the Porter's stemming algorithm\cr
#'   \code{DE_TM}\tab   \tab Author's Keywords stemmed through the Porter's stemming algorithm\cr
#'   \code{TI}\tab   \tab Terms extracted from titles\cr
#'   \code{AB}\tab   \tab Terms extracted from abstracts}
#' @param minDegree is an integer. It indicates the minimun occurrences of terms to analize and plot. The default value is 2.
#' @param k.max is an integer. It indicates the maximum numebr of cluster to keep. The default value is 5. The max value is 8.
#' @param stemming is logical. If TRUE the Porter's Stemming algorithm is applied to all extracted terms. The default is \code{stemming = FALSE}.
#' @param labelsize is an integer. It indicates the label size in the plot. Default is \code{labelsize=2}
#' @param quali.supp is a vector indicating the indexes of the categorical supplementary variables.
#' @param quanti.supp is a vector indicating the indexes of the quantitative supplementary variables.
#' @return It is an object of the class \code{list} containing the following components:
#'
#' \tabular{lll}{
#' net \tab  \tab bipartite network\cr
#' res.mca \tab       \tab Results of Multiple Correspondence Analysis\cr
#' km.res \tab      \tab Results of cluster analysis}
#' 
#' @examples
#' # EXAMPLE Conceptual Structure using Keywords Plus
#'
#' data(scientometrics)
#'
#' S <- conceptualStructure(scientometrics, field="ID_TM", stemming=TRUE, minDegree=5, k.max = 5)
#' 
#' @seealso \code{\link{termExtraction}} to extract terms from a textual field (abstract, title, 
#' author's keywords, etc.) of a bibliographic data frame.
#' @seealso \code{\link{biblioNetwork}} to compute a bibliographic network.
#' @seealso \code{\link{cocMatrix}} to compute a co-occurrence matrix.
#' @seealso \code{\link{biblioAnalysis}} to perform a bibliometric analysis.
#' 
#' @export
conceptualStructure <- function(M,field="ID", quali.supp=NULL, quanti.supp=NULL, minDegree=2, k.max=5, stemming=FALSE, labelsize=3){

  
  if (!is.null(quali.supp)){
    QSUPP=data.frame(M[,quali.supp])
    names(QSUPP)=names(M)[quali.supp]
    row.names(QSUPP)=row.names(M)
  }
  
  if (!is.null(quanti.supp)){
    SUPP=data.frame(M[,quanti.supp])
    names(SUPP)=names(M)[quanti.supp]
    row.names(SUPP)=row.names(M)
  }
  
switch(field,
       ID={
         # Create a bipartite network of Keyword plus
         #
         # each row represents a manuscript
         # each column represents a keyword (1 if present, 0 if absent in a document)
         CW <- cocMatrix(M, Field = "ID", type="matrix", sep=";")
         # Define minimum degree (number of occurrences of each Keyword)
         CW=CW[,colSums(CW)>=minDegree]
         # Delete empty rows
         CW=CW[,!(colnames(CW) %in% "NA")]
         CW=CW[rowSums(CW)>0,]
         
         # Recode as dataframe
         CW=data.frame(apply(CW,2,factor))
         
         },
       DE={
         CW <- cocMatrix(M, Field = "DE", type="matrix", sep=";")
         # Define minimum degree (number of occurrences of each Keyword)
         CW=CW[,colSums(CW)>=minDegree]
         # Delete empty rows
         CW=CW[rowSums(CW)>0,]
         CW=CW[,!(colnames(CW) %in% "NA")]
         # Recode as dataframe
         CW=data.frame(apply(CW,2,factor))
         },
       ID_TM={
         M=termExtraction(M,Field="ID",remove.numbers=TRUE, stemming=stemming, language="english", remove.terms=NULL, keep.terms=NULL, verbose=FALSE)
         
         CW <- cocMatrix(M, Field = "ID_TM", type="matrix", sep=";")
         # Define minimum degree (number of occurrences of each Keyword)
         CW=CW[,colSums(CW)>=minDegree]
         CW=CW[,!(colnames(CW) %in% "NA")]
         # Delete empty rows
         CW=CW[rowSums(CW)>0,]
         # Recode as dataframe
         CW=data.frame(apply(CW,2,factor))
         
       },
       DE_TM={
         M=termExtraction(M,Field="DE",remove.numbers=TRUE, stemming=stemming, language="english", remove.terms=NULL, keep.terms=NULL, verbose=FALSE)
         
         CW <- cocMatrix(M, Field = "DE_TM", type="matrix", sep=";")
         # Define minimum degree (number of occurrences of each Keyword)
         CW=CW[,colSums(CW)>=minDegree]
         # Delete empty rows
         CW=CW[,!(colnames(CW) %in% "NA")]
         CW=CW[rowSums(CW)>0,]
         # Recode as dataframe
         CW=data.frame(apply(CW,2,factor))
       },
       TI={
         M=termExtraction(M,Field="TI",remove.numbers=TRUE, stemming=stemming, language="english", remove.terms=NULL, keep.terms=NULL, verbose=FALSE)
         
         CW <- cocMatrix(M, Field = "TI_TM", type="matrix", sep=";")
         # Define minimum degree (number of occurrences of each Keyword)
         CW=CW[,colSums(CW)>=minDegree]
         # Delete empty rows
         CW=CW[,!(colnames(CW) %in% "NA")]
         CW=CW[rowSums(CW)>0,]
         # Recode as dataframe
         CW=data.frame(apply(CW,2,factor))
         },
       AB={
         M=termExtraction(M,Field="AB",remove.numbers=TRUE, stemming=stemming, language="english", remove.terms=NULL, keep.terms=NULL, verbose=FALSE)
         
         CW <- cocMatrix(M, Field = "AB_TM", type="matrix", sep=";")
         # Define minimum degree (number of occurrences of each Keyword)
         CW=CW[,colSums(CW)>=minDegree]
         # Delete empty rows
         CW=CW[rowSums(CW)>0,]
         CW=CW[,!(colnames(CW) %in% "NA")]
         # Recode as dataframe
         CW=data.frame(apply(CW,2,factor))
       }
       )

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
  
  
res.mca <- MCA(CW, quanti.sup=quanti, quali.sup=quali, ncp=2, graph=FALSE)

# Get coordinates of keywords (we take only categories "1"")
coord=get_mca_var(res.mca)
df=data.frame(coord$coord)[seq(2,dim(coord$coord)[1],by=2),]
row.names(df)=gsub("_1","",row.names(df))
if (!is.null(quali.supp)){
  df_quali=data.frame(res.mca$quali.sup$coord)[seq(1,dim(res.mca$quali.sup$coord)[1],by=2),]
  row.names(df_quali)=gsub("_1","",row.names(df_quali))
}
if (!is.null(quanti.supp)){
  df_quanti=data.frame(res.mca$quanti.sup$coord)[seq(1,dim(res.mca$quanti.sup$coord)[1],by=2),]
  row.names(df_quanti)=gsub("_1","",row.names(df_quanti))
} 

# K-means clustering

# Selection of optimal number of clusters (silhouette method)
a=fviz_nbclust(scale(df), kmeans, method = "silhouette",k.max=k.max)['data']
clust=as.numeric(a$data[order(-a$data$y),][1,1])

# Perform the K-means clustering
km.res <- kmeans(scale(df), clust, nstart = 25)

# Plot of the conceptual map
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

b=fviz_cluster(km.res, data = df,labelsize=labelsize, repel = TRUE)+
  theme_minimal()+
  scale_color_manual(values = cbPalette[1:clust])+
  scale_fill_manual(values = cbPalette[1:clust]) +
  labs(title= "     ") +
  geom_point() +
  theme(text = element_text(size=labelsize),axis.title=element_text(size=labelsize,face="bold"))

if (!is.null(quali.supp)){
  s_df_quali=df_quali[(abs(df_quali[,1]) >= quantile(abs(df_quali[,1]),0.75) | abs(df_quali[,2]) >= quantile(abs(df_quali[,2]),0.75)),]
  #s_df_quali=subset(df_quali,subset(abs(df_quali[,1]) >= quantile(abs(df_quali[,1]),0.80) | abs(df_quali[,2]) >= quantile(abs(df_quali[,2]),0.80)))
  names(s_df_quali)=c("x","y")
  s_df_quali$label=row.names(s_df_quali)
  x=s_df_quali$x
  y=s_df_quali$y
  label=s_df_quali$label
  b=b+geom_point(aes(x=x,y=y),data=s_df_quali,colour="red",size=1) +
  geom_text(aes(x=x,y=y,label=label,size=labelsize/3),data=s_df_quali)
}

if (!is.null(quanti.supp)){
  names(df_quanti)=c("x","y")
  df_quanti$label=row.names(df_quanti)
  x=df_quanti$x
  y=df_quanti$y
  label=df_quanti$label
  b=b+geom_point(aes(x=x,y=y),data=df_quanti,colour="blue",size=1) +
    geom_text(aes(x=x,y=y,label=label,size=labelsize/3),data=df_quanti)
}

plot(b)

semanticResults=list(net=CW,res.mca=res.mca,km.res=km.res)
return(semanticResults)
}