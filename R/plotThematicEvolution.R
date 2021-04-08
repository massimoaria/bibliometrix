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
#' data(scientometrics, package = "bibliometrixData")
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

plotThematicEvolution <- function (Nodes, Edges, measure = "inclusion", min.flow = 0){
  

  
  Kx <- length(table(Nodes$group))
  Ky <- nrow(Nodes)
  Nodes <-Nodes %>% 
    mutate(coordX=factor(.data$group, labels = seq(from= 0, to= 1, by= 1/(Kx-0.8)))) %>%  # before: seq(from= 0, to= 1, by= 1/(Kx-1))
    mutate(coordY= rep(0.1, Ky))
  

  ################
  switch(measure, inclusion = {
    Edges = Edges[-c(4, 5)]
  }, stability = {
    Edges = Edges[-c(3, 4)]
  }, weighted = {
    Edges = Edges[, -c(3, 5)]
  })
  names(Edges)[3] = "weight"
  Edges = Edges[Edges$weight >= min.flow, ]
  Edges$weight = Edges$weight * 100
  
  
  plotly::plot_ly(
    type = "sankey",
    arrangement = "snap",
    node = list(
      label = Nodes$name,
      x = Nodes$coordX,
      y = Nodes$coordY,
      color = Nodes$color,
      pad = 10), # 10 Pixel
    link = list(
      source = Edges$from,
      target = Edges$to,
      value = Edges$weight)
  ) %>% 
    plotly::add_annotations(x = Nodes$coordX,
                    y = -0.03,
                    text = factor(Nodes$group) ,
                    showarrow=F,xanchor = "center",
                    font = list(color = 'Dark',
                                family = "Arial",
                                size = 18))
}