#' Perform a Thematic Evolution Analysis
#'
#' It performs a Thematic Evolution Analysis based on co-word network analysis and clustering.
#' The methodology is inspired by the proposal of Cobo et al. (2011). 
#' 
#' \code{\link{thematicEvolution}} starts from two or more thematic maps created by \code{\link{thematicMap}} function.
#'  
#' 
#' @param M is a bibliographic data frame obtained by the converting function \code{\link{convert2df}}.
#' @param field is a character object. It indicates the content field to use. Field can be one of c=("ID","DE","TI","AB"). Default value is \code{field="ID"}.
#' @param years is a numeric vector of two or more unique cut points.
#' @param n is numerical. It indicates the number of words to use in the network analysis
#' @param minFreq is numerical. It indicates the min frequency of words included in to a cluster.
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
#' 
#' nexus <- thematicEvolution(scientometrics,field="ID", years=years, n=100,minFreq=2)
#' 
#' @seealso \code{\link{thematicMap}} function to create a thematic map based on co-word network analysis and clustering.
#' @seealso \code{\link{cocMatrix}} to compute a bibliographic bipartite network.
#' @seealso \code{\link{networkPlot}} to plot a bibliographic network.
#'
#' @export

thematicEvolution <- function(M, field="ID", years,n=250,minFreq=2){
  
  #net=list()
  #arguments <- list(...)
  #K=length(arguments)
  
  list_df=timeslice(M, breaks = years)
  
  K=length(list_df)
  S=net=res=list()
  
  pdf(file = NULL)  ## to prevent the network plotting
  for (k in 1:K){
    
    Mk=list_df[[k]]
    
    switch(field,
           ID={
             NetMatrixk <- biblioNetwork(Mk, analysis = "co-occurrences", network = "keywords", sep = ";")
             
           },
           DE={
             NetMatrixk <- biblioNetwork(Mk, analysis = "co-occurrences", network = "author_keywords", sep = ";")
             
           },
           TI={
             if(!("TI_TM" %in% names(Mk))){Mk=termExtraction(Mk,Field="TI",verbose=FALSE)}
             NetMatrixk <- biblioNetwork(Mk, analysis = "co-occurrences", network = "titles", sep = ";")
             
           },
           AB={
             if(!("AB_TM" %in% names(Mk))){Mk=termExtraction(Mk,Field="AB",verbose=FALSE)}
             NetMatrixk <- biblioNetwork(Mk, analysis = "co-occurrences", network = "abstracts", sep = ";")
             
           })
    
    Sk <- normalizeSimilarity(NetMatrixk, type = "association")
    #N = dim(NetMatrixk)[1]
    if (n>dim(NetMatrixk)[1]){n = dim(NetMatrixk)[1]}
    netk <- networkPlot(NetMatrixk, normalize = "association",n = n, 
                        Title = "co-occurrence network",type="auto",
                        size=0.3,size.cex=FALSE,label.cex=FALSE,labelsize = 0.1, halo = FALSE, cluster="louvain",remove.isolates=FALSE,
                        remove.multiple=FALSE, noloops=TRUE, weighted=TRUE,label.n=0)
    resk <- thematicMap(netk, NetMatrixk, Sk, minfreq = minFreq)
    #S[[k]]=Sk
    #net[[k]]=netk
    res[[k]]=resk
  }
  dev.off() 
  
  par(mfrow=c(1, (K-1)))
  
  if (K<2){
    print("Error")
    return()
  }
  net=list()
  incMatrix=list()
  for (k in 2:K){
    res1=res[[(k-1)]]
    res2=res[[(k)]]
    
    ### check for empty cluster list
    if (res1$nclust==0 | res2$nclust==0){
      cat(paste("\nNo topics in the period ",k-1," with this set of input parameters\n\n"))
      return(list(check=FALSE))
    }
    
    ####
     res1$words$Cluster=paste(res1$clusters$name[res1$words$Cluster],"--",LETTERS[k-1],sep="")
     res1$clusters$label=paste(res1$clusters$name,"--",LETTERS[k-1],sep="")
     res2$words$Cluster=paste(res2$clusters$name[res2$words$Cluster],"--",LETTERS[k],sep="")
     res2$clusters$label=paste(res2$clusters$name,"--",LETTERS[k],sep="")
    ##
    
    
    CL1=unique(res1$clusters$label)
    CL2=unique(res2$clusters$label)
    
    Inc=data.frame(CL1=NA,CL2=NA,Inc_index=NA,Words="NA",Occ=NA,Tot=NA,Inc_Weighted=NA, Tot_CL1=NA,Tot_CL2=NA, Stability=NA, stringsAsFactors = FALSE)
    cont=0
    
    for (ii in 1:length(CL1)){
      i=CL1[ii]
      w1=as.character(res1$words$Words[res1$words$Cluster==i])
      for (jj in 1:length(CL2)){
        j=CL2[jj]
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
        Inc[cont,8]=length(w1)
        Inc[cont,9]=length(w2)
        Inc[cont,10]=length(wi)/(length(w1)+length(w2)-length(wi))
        
        
      }
    }
    
    incMatrix[[k-1]]=Inc
    
  }
  
  INC=incMatrix[[1]]
  if (length(incMatrix)>1){
    for (i in 2:length(incMatrix)){
      INC=rbind(INC,incMatrix[[i]])
    }
  }
  
  ### sankey plot data
  edges = INC[,c(1,2,3,10)]
  edges=edges[edges[,3]>0,]
  nodes = data.frame("name"=unique(c(edges$CL1,edges$CL2)),stringsAsFactors = FALSE)
  nodes$group=nodes$name
  
  cont=0
  edges[,5]=edges[,1]
  for (i in nodes$name){
    
    ind=which(edges[,1]==i)
    edges[ind,1]=cont
    ind1=which(edges[,2]==i)
    edges[ind1,2]=cont
    cont=cont+1
  }
  
  names(edges)=c("from","to","Inclusion","Stability","group")
  edges$from=as.numeric(edges$from)
  edges$to=as.numeric(edges$to)
  
  results=list(Nodes=nodes,Edges=edges,Data=INC,check=TRUE, TM=res)
  
  return(results)
  
  
}
