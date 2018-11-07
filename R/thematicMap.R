#' Create a thematic map
#'
#' It creates a thematic map based on co-word network analysis and clustering.
#' The methodology is inspired by the proposal of Cobo et al. (2011). 
#' 
#' \code{thematicMap} starts from a co-occurrence keyword network to plot in a 
#' two-dimesional map the typological themes of a domain.
#' 
#' 
#' @param Net is a igraph object created by \code{\link{networkPlot}} function.
#' @param NetMatrix is a co-occurence matrix obtained by the network functions 
#' \code{\link{biblioNetwork}} or \code{\link{cocMatrix}}.
#' @param S is a similarity matrix obtained by the \code{\link{normalizeSimilarity}} function. 
#' If S is NULL, map is created using co-occurrence counts.
#' @param minfreq is a integer. It indicates the minimun frequency of a cluster.
#' @return a list containing:
#' \tabular{lll}{
#' \code{map}\tab   \tab The thematic map as ggplot2 object\cr
#' \code{clusters}\tab   \tab Centrality and Density values for each cluster. \cr
#' \code{words}\tab   \tab A list of words following in each cluster}
#' 
#'
#' @examples
#' 
#' data(scientometrics)
#' NetMatrix <- biblioNetwork(scientometrics, analysis = "co-occurrences", 
#'               network = "keywords", sep = ";")
#' S <- normalizeSimilarity(NetMatrix, type = "association")
#' net <- networkPlot(S, n = 100, Title = "co-occurrence network",type="fruchterman",
#'      labelsize = 0.7, halo = FALSE, cluster = "walktrap",remove.isolates=FALSE,
#'      remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
#' res <- thematicMap(net, NetMatrix, S)
#' plot(res$map)
#'
#' @seealso \code{\link{biblioNetwork}} function to compute a bibliographic network.
#' @seealso \code{\link{cocMatrix}} to compute a bibliographic bipartite network.
#' @seealso \code{\link{networkPlot}} to plot a bibliographic network.
#'
#' @export

thematicMap <- function(Net, NetMatrix, S=NULL, minfreq=5){
  
  net=Net$graph
  if (is.null(S)){S=NetMatrix}
  net_groups <- Net$cluster_obj
  groups=net_groups$membership
  words=net_groups$name
  color=V(net)$color
  color[is.na(color)]="#D3D3D3"
  
  ###
  W=intersect(row.names(NetMatrix),words)
  index=which(row.names(NetMatrix) %in% W)
  ii=which(words %in% W)
  words=words[ii]
  groups=groups[ii]
  color=color[ii]
  ###
  
  
  # 
  # if (length(words)>length(index)){
  #   ii=which(words==setdiff(words,row.names(S)))
  #   words=words[-ii]
  #   groups=groups[-ii]
  #   color=color[-ii]
  # }

  C=diag(NetMatrix)

  sEij=S[index,index]
  #dim(sEij)
  sC=(C[index])


### centrality and density
  label_cluster=unique(groups)
  word_cluster=words[groups]
  centrality=c()
  density=c()
  labels=list()
  
  df_lab=data.frame(sC=sC,words=words,groups=groups,color=color,cluster_label="NA",stringsAsFactors = FALSE)
  
  color=c()
  for (i in label_cluster){
    ind=which(groups==i)
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
  row.names(df)=df$label
  A=aggregate(df_lab$sC,by=list(groups),'max')
  df$freq=A[,2]
  
  meandens=mean(df$rdensity)
  meancentr=mean(df$rcentrality)
  df=df[df$freq>=minfreq,]

  g=ggplot(df, aes(x=df$rcentrality, y=df$rdensity)) +
    geom_point(aes(size=log(as.numeric(df$freq))),shape=20,col=df$color)     # Use hollow circles
  g=g+geom_label_repel(aes(label=ifelse(df$freq>1,unlist(tolower(df$name)),'')),size=3,angle=0)+ geom_hline(yintercept = meandens,linetype=2) +
    geom_vline(xintercept = meancentr,linetype=2) + theme(legend.position="none") +
    scale_radius(range=c(1, 50))+labs(x = "Centrality", y = "Density")+
    theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

  names(df_lab)=c("Occurrences", "Words", "Cluster", "Color","Cluster_Label")
  words=df_lab[order(df_lab$Cluster),]
  words=words[!is.na(words$Color),]
  row.names(df)=NULL
  results=list(map=g, clusters=df, words=words,nclust=dim(df)[1])
return(results)
}