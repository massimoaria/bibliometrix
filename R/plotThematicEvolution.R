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
#' \dontrun{
#' data(scientometrics, package = "bibliometrixData")
#' years=c(2000)
#' 
#' nexus <- thematicEvolution(scientometrics,field="ID",years=years,n=100,minFreq=2)
#' 
#' plotThematicEvolution(nexus$Nodes,nexus$Edges)
#' }
#' 
#' @seealso \code{\link{thematicMap}} function to create a thematic map based on co-word network analysis and clustering.
#' @seealso \code{\link{thematicMap}} function to perform a thematic evolution analysis.
#' @seealso \code{\link{networkPlot}} to plot a bibliographic network.
#'
#' @export

plotThematicEvolution <- function (Nodes, Edges, measure = "inclusion", min.flow = 0){
  

  
  Kx <- length(unique(Nodes$group))
  Ky <- nrow(Nodes)
  Nodes <-Nodes %>% 
    mutate(
      coordX=rep(seq(from= 0, to= 1, by= 1/(Kx-0.8)),as.numeric(table(.data$group))),
      coordY= rep(0.1, Ky)
      )
  

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
  
  Nodes$id <- (1:nrow(Nodes))-1
 
   ## identify and remove nodes with empty edges
  ind <- setdiff(Nodes$id,unique(c(Edges$from,Edges$to)))
  if(length(ind)>0) {
    Nodes <- Nodes[-(ind+1),]
    Nodes$idnew <- (1:nrow(Nodes))-1
    ## replace old edge ids with new ones
    for (i in 1:nrow(Nodes)){
      old <- Nodes$id[i]
      new <- Nodes$idnew[i] 
      Edges$from[Edges$from==old] <- new
      Edges$to[Edges$to==old] <- new
    }
  }
  
  #Edges$color <- "lightgrey"
  
  # plotly margins
  m <- list(
    l = 50,
    r = 50,
    b = 100,
    t = 100,
    pad = 4
  )
  
  plotly::plot_ly(
    type = "sankey",
    arrangement = "snap",
    node = list(
      label = Nodes$name,
      x = Nodes$coordX,
      y = Nodes$coordY,
      color = Nodes$color,
      pad = 4), # 10 Pixel
    link = list(
      source = Edges$from,
      target = Edges$to,
      value = Edges$weight
      #,color = Edges$color
      )
  ) %>% 
    layout(margin = m) %>%
    plotly::add_annotations(x = Nodes$coordX,
                    y = 1.08,
                    text = factor(Nodes$group) ,
                    showarrow=F,xanchor = "center",
                    font = list(color = 'Dark',
                                family = "TimesNewRoman",
                                size = 18))
}