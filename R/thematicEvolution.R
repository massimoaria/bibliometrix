#' Create an Evolution Thematic Map
#'
#' It creates an Evolution thematic map based on co-word network analysis and clustering.
#' The methodology is inspired by the proposal of Cobo et al. (2011). 
#' 
#' \code{\link{thematicEvolution}} starts from two or more thematic maps created by \code{\link{thematicMap}} function.
#'  
#' 
#' @param ... is a sequence of names of thematic maps created by \code{\link{thematicMap}} function.
#' @param weighted is a logical. If FALSE, a thematic nexus is measures by the classical inclusion index (calculated using the 
#' number of keywords). If TRUE, the inclusion index is calculated considering the occurrences of keywords.
#' @param labelsize
#' @param size
#' @return a list containing:
#' \tabular{lll}{
#' \code{nets}\tab   \tab The thematic nexus graph for each comparison\cr
#' \code{incMatrix}\tab   \tab Some useful statistics about the thematic nexus}
#' 
#'
#' @examples
#' 
#' data(scientometrics)
#' years=c(2000)
#' list_df=timeslice(scientometrics, breaks = years)
#' M1=list_df[[1]]
#' M2=list_df[[2]]
#' NetMatrix1 <- biblioNetwork(M1, analysis = "co-occurrences", 
#'               network = "keywords", sep = ";")
#' S1 <- normalizeSimilarity(NetMatrix1, type = "association")
#' net1 <- networkPlot(NetMatrix1, normalize = "association",n = 50, 
#'      Title = "co-occurrence network",type="fruchterman",
#'      labelsize = 0.7, halo = FALSE, cluster = "walktrap",remove.isolates=FALSE,
#'      remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
#' res1 <- thematicMap(net1, NetMatrix1, S1, minfreq = 1)
#' #plot(res1$map)
#' 
#' NetMatrix2 <- biblioNetwork(M2, analysis = "co-occurrences", 
#'               network = "keywords", sep = ";")
#' S2 <- normalizeSimilarity(NetMatrix2, type = "association")
#' net2 <- networkPlot(NetMatrix2, normalize = "association",n = 50, 
#'      Title = "co-occurrence network",type="fruchterman",
#'      labelsize = 0.7, halo = FALSE, cluster = "walktrap",remove.isolates=FALSE,
#'      remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
#' res2 <- thematicMap(net2, NetMatrix2, S2, minfreq = 1)
#' #plot(res2$map)
#' 
#' nexus <- thematicEvolution(res1,res2,weighted=FALSE)
#' 
#' @seealso \code{\link{thematicMap}} function to create a thematic map based on co-word network analysis and clustering.
#' @seealso \code{\link{cocMatrix}} to compute a bibliographic bipartite network.
#' @seealso \code{\link{networkPlot}} to plot a bibliographic network.
#'
#' @export

thematicEvolution <- function(...,weighted=FALSE,labelsize=0.5,size=1){
  
  arguments <- list(...)
  K=length(arguments)
  par(mfrow=c(1, (K-1)))
  
  if (K<2){
    print("Error")
    return()
  }
  net=list()
  incMatrix=list()
  for (k in 2:K){
    res1=arguments[[(k-1)]]
    res2=arguments[[(k)]]
    CL1=unique(res1$clusters$label)
    CL2=unique(res2$clusters$label)
    
    Inc=data.frame(CL1=NA,CL2=NA,Inc_index=NA,Words="NA",Occ=NA,Tot=NA,Inc_Weighted=NA, stringsAsFactors = FALSE)
    cont=0
    incNet=matrix(0,length(CL1),length(CL2))
    for (i in CL1){
      w1=as.character(res1$words$Words[res1$words$Cluster==i])
      for (j in CL2){
        
        cont=cont+1
        w2=as.character(res2$words$Words[res2$words$Cluster==j])  
        Inc[cont,1]=i
        Inc[cont,2]=j
        Inc[cont,3]=length(intersect(w1,w2))/min(length(w1),length(w2))
        Inc[cont,4]=paste(intersect(w1,w2),collapse=";")
        wi=intersect(w1,w2)
        si=sum(res1$words$Occurrences[res1$words$Words %in% wi])
        s1=min(c(res1$clusters$freq[res1$clusters$label==i],res2$clusters$freq[res2$clusters$label==j]),na.rm=T)
        Inc[cont,5]=si
        Inc[cont,6]=s1
        Inc[cont,7]=si/s1
        if (isTRUE(weighted)){incNet[i,j]=Inc[cont,7]} else {incNet[i,j]=Inc[cont,3]}
        
      }
    }
    colnames(incNet) <- res2$clusters$name 
    rownames(incNet) <- res1$clusters$name
    
    ### network 
    edgesize=5
    net2way=graph_from_incidence_matrix(incNet, directed = F, mode = "out", multiple = FALSE, weighted = T)
    E(net2way)$width <- (E(net2way)$weight + min(E(net2way)$weight))/max(E(net2way)$weight + min(E(net2way)$weight)) *edgesize
    V(net2way)$color=c(res1$clusters$color,res2$clusters$color)
    
    ### plot
    n1=dim(res1$clusters)[1]
    n2=dim(res2$clusters)[1]
    l <- layout.fruchterman.reingold(net2way)
    l[,1]=c(rep(1,n1),rep(2,n2))
    l[(1:n1),2]=seq(1,10,length.out = n1)
    l[((n1+1):(n1+n2)),2]=seq(1,10,length.out = n2)
    #labelsize=0.75
    Title=paste("From sub-period",(k-1),"to sub-period",k)
    V(net2way)$size=size
    plot(net2way,layout=l,vertex.label.dist = 0.4, vertex.frame.color = 'black', vertex.label.color = 'black', vertex.label.font = 1, vertex.label = V(net2way)$name, vertex.label.cex = labelsize, main=Title)
    net[[k-1]]=net2way
    incMatrix[[k-1]]=Inc
  }
  results=list(nets=net,incMatrix=incMatrix)
  return(results)
}