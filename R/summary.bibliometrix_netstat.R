#' Summarizing network analysis results
#'
#' \code{summary} method for class '\code{bibliometrix_netstat}'
#' @param object is the object for which a summary is desired.
#' @param ... can accept two arguments:\cr
#' \code{k} integer, used for table formatting (number of rows). Default value is 10.\cr
#' @return The function \code{summary} computes and returns on display several statistics both at network and vertex level.
#'
#' 
#'
#' @examples
#' 
#' # to run the example, please remove # from the beginning of the following lines
#' #data(scientometrics)
#' 
#' #NetMatrix <- biblioNetwork(scientometrics, analysis = "collaboration", 
#' #                   network = "authors", sep = ";")
#' #netstat <- networkStat(NetMatrix, stat = "all", type = "degree")
#' #summary(netstat)
#'
#' @method summary bibliometrix_netstat
#' @export

summary.bibliometrix_netstat<-function(object, ...){
  
  
  if (class(object)!="bibliometrix_netstat"){cat('\n argument "object" have to be an object of class "netstat"\n');return(NA)}
  
  arguments <- list(...)
  if (sum(names(arguments)=="k")==0){k=10} else {k=arguments$k}
  
  #Main Statistics about network
  MainStatNet=("\n\nMain statistics about the network\n\n")
  MainStatNet[2]=paste("Size                                 ",object$network$networkSize,"\n")
  MainStatNet[3]=paste("Density                              ",round(object$network$networkDensity,3),"\n")
  MainStatNet[4]=paste("Transitivity                         ",round(object$network$networkTransitivity,3),"\n")
  MainStatNet[5]=paste("Diameter                             ",round(object$network$networkDiameter,3),"\n")
  switch(object$type,
         degree={
           MainStatNet[6]=paste("Degree Centralization                ",round(object$network$networkCentrDegree,3),"\n")
           },
         pagerank={
           MainStatNet[6]=paste("Degree Centralization                ",round(object$network$networkCentrDegree,3),"\n")
         },
         hub={
           MainStatNet[6]=paste("Degree Centralization                ",round(object$network$networkCentrDegree,3),"\n")
         },
         authority={
           MainStatNet[6]=paste("Degree Centralization                ",round(object$network$networkCentrDegree,3),"\n")
         },
         closeness={
           MainStatNet[6]=paste("Closeness Centralization             ",round(object$network$networkCentrCloseness,3),"\n")
           },
         betweenness={
           MainStatNet[6]=paste("Betweenness Centralization           ",round(object$network$networkCentrbetweenness,3),"\n")
         },
         eigenvector={
           MainStatNet[6]=paste("Eigenvector Centralization           ",round(object$network$networkCentrEigen,3),"\n")
         }
         )
 
  MainStatNet[7]=paste("Average path length                  ",round(object$network$NetworkAverPathLeng,3),"\n")
  MainStatNet[8]=paste("\n")
  cat(MainStatNet)
  cat("\n\n\n")
  
  if (object$stat=="all"){
  
    switch(object$type,
           degree={
             # Main measures of centrality and prestige of vertices
             cat("\n\nMain measures of centrality and prestige of vertices\n\n")
             # Centrality Degree
             cat("\nDegree Centrality: Top vertices\n\n")
             CD=object$vertex[,1:2]
             
             A=CD[order(-CD$vertexCentrDegree),]
             names(A)=c("Vertex ID             ", "Degree Centrality")
             A=format(A[1:k,],justify="left",digits=3)
             row.names(A)=1:k
             print(A,row.names=TRUE);cat("\n")
           },
           closeness={
             # Centrality Closeness
             cat("\nCloseness Centrality: Top vertices\n\n")
             CD=object$vertex[,c(1,3)]
             
             A=CD[order(-CD$vertexCentrCloseness),]
             names(A)=c("Vertex ID             ", "Closeness Centrality")
             A=format(A[1:k,],justify="left",digits=3)
             row.names(A)=1:k
             print(A,row.names=TRUE);cat("\n")
           },
           eigenvector={
             # Centrality Eigenvectors
             cat("\nEigenvector Centrality: Top vertices\n\n")
             CD=object$vertex[,c(1,4)]
             
             A=CD[order(-CD$vertexCentrEigen),]
             names(A)=c("Vertex ID             ", "Eigenvector Centrality")
             A=format(A[1:k,],justify="left",digits=3)
             row.names(A)=1:k
             print(A,row.names=TRUE);cat("\n")
           },
           betweenness={
             # Centrality betweeness
             cat("\nBetweenness Centrality: Top vertices\n\n")
             CD=object$vertex[,c(1,5)]
             
             A=CD[order(-CD$vertexCentrBetweenness),]
             names(A)=c("Vertex ID             ", "Betweenness Centrality")
             A=format(A[1:k,],justify="left",digits=3)
             row.names(A)=1:k
             print(A,row.names=TRUE);cat("\n")
           },
           pagerank={
             # pagerank
             cat("\nPageRank Score: Top vertices\n\n")
             CD=object$vertex[,c(1,6)]
             
             A=CD[order(-CD$vertexPageRank),]
             names(A)=c("Vertex ID             ", "Pagerank Score")
             A=format(A[1:k,],justify="left",digits=3)
             row.names(A)=1:k
             print(A,row.names=TRUE);cat("\n")
           },
           hub={
             # hub
             cat("\nHub Score: Top vertices\n\n")
             CD=object$vertex[,c(1,7)]
             
             A=CD[order(-CD$vertexHub),]
             names(A)=c("Vertex ID             ", "Hub Score")
             A=format(A[1:k,],justify="left",digits=3)
             row.names(A)=1:k
             print(A,row.names=TRUE);cat("\n")
           },
           authority={
             # Authority
             cat("\nAuthority Score: Top vertices\n\n")
             CD=object$vertex[,c(1,8)]
             
             A=CD[order(-CD$vertexAuthority),]
             names(A)=c("Vertex ID             ", "Authority Score")
             A=format(A[1:k,],justify="left",digits=3)
             row.names(A)=1:k
             print(A,row.names=TRUE);cat("\n")
           }
           )
  
  }
  
  invisible(TRUE)
}

