#' Three Fields Plot
#'
#' Visualize the main items of three fields (e.g. authors, keywords, journals), and how they are related through a Sankey diagram.
#' 
#' @param M is a bibliographic data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to manuscripts and variables to Field Tag in the original SCOPUS and Thomson Reuters' ISI Web of Knowledge file.
#' @param fields is a character vector. It indicates the fields to analize using the standard WoS field tags. 
#'        Default is \code{fields = c("AU","DE", "SO")}.
#' @param n is a integer vector. It indicates how many items to plot, for each of the three fields. 
#'        Default is \code{n = c(20, 20, 20)}
#' @param width is an integer. It indicates the plot width (in pixel). Default is \code{width=1200}.
#' @param height is an integer. It indicates the plot height (in pixel). Default is \code{height=600}.
#' @return a sankeyPlot
#' 
#'
#' @examples
#' 
#' #data(scientometrics)
#' 
#' #threeFieldsPlot(scientometrics, fields=c("DE","AU","CR"),n=c(20,20,20))
#'
#' @export
#' 
threeFieldsPlot <- function(M, fields=c("AU","DE","SO"),n=c(20,20,20), width=1200,height=600){
  
  if ("CR_SO" %in% fields){
    M=metaTagExtraction(M,"CR_SO")
  }
  if ("AU_UN" %in% fields){
    M=metaTagExtraction(M,"AU_UN")
  }
  if ("AB_TM" %in% fields){
    M=termExtraction(M,"AB")
  }
  if ("TI_TM" %in% fields){
    M=termExtraction(M,"TI")
  }
  if ("AU_CO" %in% fields){
    M=metaTagExtraction(M,"AU_CO")
  }
  
  
  
  ### Document x Attribute matrix Field LEFT
  WL=cocMatrix(M,fields[1], binary=FALSE)
  n1=n[1]
  TopL=names(sort(Matrix::colSums(WL),decreasing = TRUE))[1:n1]
  WL=WL[,TopL]
  
  ### Document x Attribute matrix Field MIDDLE
  WM=cocMatrix(M,fields[2], binary=FALSE)
  n2=n[2]
  TopM=names(sort(Matrix::colSums(WM),decreasing = TRUE))[1:n2]
  WM=WM[,TopM]
  
  ### Document x Attribute matrix Field RIGHT
  WR=cocMatrix(M,fields[3], binary=FALSE)
  n3=n[3]
  TopR=names(sort(Matrix::colSums(WR),decreasing = TRUE))[1:n3]
  WR=WR[,TopR]
  
  ### Co-Occurrence Matrices
  LM=Matrix::crossprod(WL,WM)
  MR=Matrix::crossprod(WM,WR)
  
  
  row.names(LM)=1:n1
  colnames(LM)=row.names(MR)=(n1+1):(n2+n1)
  colnames(MR)=(n2+n1+1):(n1+n2+n3)
  
  LMm=reshape2::melt(as.matrix(LM))
  LMm$group=NA
  MRm=reshape2::melt(as.matrix(MR))
  MRm$group=NA
  
  Edges=rbind(LMm,MRm)
  names(Edges)=c("from","to","Value","group")
  Edges$from=Edges$from-1
  Edges$to=Edges$to-1
  
  Nodes=tolower(c(TopL,TopM,TopR))
  Nodes=data.frame(Nodes=Nodes,group=Nodes, stringsAsFactors = FALSE)
  
  
  min.flow=1
  names(Edges)[3]="weight"
  Edges=Edges[Edges$weight>=min.flow,]
  ind=which(!((0:(sum(n)-1)) %in% c(Edges$from,Edges$to)))
  Nodes[ind,]=c("","")

  
  networkD3::sankeyNetwork(Links = Edges, Nodes = Nodes, Source = "from", Target = "to", 
                           NodeID = "Nodes", Value = "weight", width = width,height=height,fontSize = 12,
                           nodeWidth = 30,  NodeGroup = "group",LinkGroup = "group")
  
  
}
