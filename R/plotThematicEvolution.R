#' Plot a Thematic Evolution Analysis
#'
#' It plot a Thematic Evolution Analysis performed using the \code{\link{thematicEvolution}} function.
#' 
#' @param Nodes is a list of nodes obtained by \code{\link{thematicEvolution}} function.
#' @param Edges is a list of edges obtained by \code{\link{thematicEvolution}} function.
#' @param measure is a character. It can be \code{measure=("inclusion","stability", "weighted")}.
#' @param min.flow is numerical. It indicates the minimum value of measure to plot a flow.
#' @return a sankeyPlot
#' 
#'
#' @examples
#' 
#' data(scientometrics)
#' years=c(2000)
#' 
#' nexus <- thematicEvolution(scientometrics,field="ID",years=years,n=100,minFreq=2)
#' 
#' #plotThematicEvolution(nexus$Nodes,nexus$Edges)
#' 
#' @seealso \code{\link{thematicMap}} function to create a thematic map based on co-word network analysis and clustering.
#' @seealso \code{\link{thematicMap}} function to perform a thematic evolution analysis.
#' @seealso \code{\link{networkPlot}} to plot a bibliographic network.
#'
#' @export

plotThematicEvolution<-function(Nodes,Edges,measure="inclusion", min.flow=0){
  
  switch(measure,
         inclusion={
           Edges=Edges[-c(4,5)]
         },
         stability={
           Edges=Edges[-c(3,4)]
         },
         weighted={
           Edges=Edges[,-c(3,5)]
         })
  
  names(Edges)[3]="weight"
  Edges=Edges[Edges$weight>=min.flow,]
  Edges$weight=Edges$weight*100
  networkD3::sankeyNetwork(Links = Edges, Nodes = Nodes, Source = "from", Target = "to", 
                           NodeID = "name", Value = "weight", width = 900, fontSize = 12,
                           nodeWidth = 30,  NodeGroup = "group",LinkGroup = "group")
                           #colourScale = networkD3::JS('function(){d3.select("body").style("background-color", "#DAE3F9"); return 50;}'))
}


