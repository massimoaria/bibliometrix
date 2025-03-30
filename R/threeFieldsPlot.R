utils::globalVariables(c("level"))
#' Three Fields Plot
#'
#' Visualize the main items of three fields (e.g. authors, keywords, journals), and how they are related through a Sankey diagram.
#'
#' @param M is a bibliographic data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to manuscripts and variables to Field Tag in the original SCOPUS and Clarivate Analytics WoS file.
#' @param fields is a character vector. It indicates the fields to analyze using the standard WoS field tags.
#'        Default is \code{fields = c("AU","DE", "SO")}.
#' @param n is a integer vector. It indicates how many items to plot, for each of the three fields.
#'        Default is \code{n = c(20, 20, 20)}
#' @return a sankeyPlot
#'
#'
#' @examples
#'
#' # data(scientometrics, package = "bibliometrixData")
#'
#' # threeFieldsPlot(scientometrics, fields=c("DE","AU","CR"),n=c(20,20,20))
#'
#' @export
#'
threeFieldsPlot <- function(M, fields = c("DE", "AU", "SO"), n = c(20, 20, 20)) {
  if ("CR_SO" %in% fields) {
    M <- metaTagExtraction(M, "CR_SO")
  }
  if ("AU_UN" %in% fields) {
    M <- metaTagExtraction(M, "AU_UN")
  }
  if ("AB_TM" %in% fields) {
    M <- termExtraction(M, "AB")
  }
  if ("TI_TM" %in% fields) {
    M <- termExtraction(M, "TI")
  }
  if ("AU_CO" %in% fields) {
    M <- metaTagExtraction(M, "AU_CO")
  }


  Binary <- rep(FALSE, 3)
  ind <- which(fields %in% "CR")
  if (length(ind) > 0) Binary[ind] <- TRUE

  ### Document x Attribute matrix Field MIDDLE
  WM <- cocMatrix(M, fields[2], binary = Binary[2], n = n[2])
  n2 <- min(n[2], ncol(WM))
  TopM <- colnames(WM)

  docInd <- which(Matrix::rowSums(WM[, TopM]) > 0)

  ### Document x Attribute matrix Field LEFT
  WL <- cocMatrix(M, fields[1], binary = Binary[1], n = n[1])
  n1 <- min(n[1], ncol(WL))
  # TopL=names(sort(Matrix::colSums(WL),decreasing = TRUE))[1:n1]
  TopL <- colnames(WL)
  # WL=WL[,TopL]



  ### Document x Attribute matrix Field RIGHT
  WR <- cocMatrix(M, fields[3], binary = Binary[3], n = n[3])
  n3 <- min(n[3], ncol(WR))
  # TopR=names(sort(Matrix::colSums(WR),decreasing = TRUE))[1:n3]
  TopR <- colnames(WR)
  # WR=WR[,TopR]

  ### Co-Occurrence Matrices
  LM <- Matrix::crossprod(WL[docInd, ], WM[docInd, ])
  MR <- Matrix::crossprod(WM[docInd, ], WR[docInd, ])

  row.names(LM) <- 1:n1
  colnames(LM) <- row.names(MR) <- (n1 + 1):(n2 + n1)
  colnames(MR) <- (n2 + n1 + 1):(n1 + n2 + n3)

  LMm <- meltx(as.matrix(LM))
  LMm$group <- NA
  MRm <- meltx(as.matrix(MR))
  MRm$group <- NA

  Edges <- rbind(LMm, MRm)
  Edges$Var1 <- as.numeric(Edges$Var1)
  Edges$Var2 <- as.numeric(Edges$Var2)
  names(Edges) <- c("from", "to", "Value", "group")
  Edges <- Edges[!is.na(Edges$to) & !is.na(Edges$from), ]
  Edges$from <- Edges$from - 1
  Edges$to <- Edges$to - 1
  Edges <- Edges[, -4]

  Nodes <- tolower(c(TopL, TopM, TopR))

  Nodes <- data.frame(
    Nodes = Nodes,
    group = c(rep(fields[1], length(TopL)), rep(fields[2], length(TopM)), rep(fields[3], length(TopR))),
    level = c(rep(1, length(TopL)), rep(2, length(TopM)), rep(3, length(TopR)))
  )


  min.flow <- 1
  names(Edges)[3] <- "weight"
  Edges <- Edges[Edges$weight >= min.flow, ]
  # ind=which(!((0:(sum(n)-1)) %in% c(Edges$from,Edges$to)))
  # Nodes[ind,]=c("","")

  ###########
  Kx <- length(table(Nodes$group))
  Ky <- nrow(Nodes)
  Nodes <- Nodes %>%
    # mutate(coordX=factor(group, labels = seq(from= 0, to= 1, by= 1/(Kx-0.8)))) %>%  # before: seq(from= 0, to= 1, by= 1/(Kx-1))
    mutate(
      coordX = rep(seq(from = 0, to = 1, by = 1 / (Kx - 0.8)), as.numeric(table(level))),
      coordY = rep(0.1, Ky)
    )

  colornodes <- c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")
  Nodes$color <- colorRampPalette(colornodes)(nrow(Nodes))
  Nodes$id <- (1:nrow(Nodes)) - 1

  ## identify and remove nodes with empty edges
  ind <- setdiff(Nodes$id, unique(c(Edges$from, Edges$to)))
  if (length(ind) > 0) {
    Nodes <- Nodes[-(ind + 1), ]
    Nodes$idnew <- (1:nrow(Nodes)) - 1
    ## replace old edge ids with new ones
    for (i in 1:nrow(Nodes)) {
      old <- Nodes$id[i]
      new <- Nodes$idnew[i]
      Edges$from[Edges$from == old] <- new
      Edges$to[Edges$to == old] <- new
    }
  }

  ## Build sankey plot
  m <- list(
    l = 50,
    r = 50,
    b = 100,
    t = 100,
    pad = 2
  )

  plotly::plot_ly(
    type = "sankey",
    arrangement = "snap",
    node = list(
      label = Nodes$Nodes,
      x = Nodes$coordX,
      y = Nodes$coordY,
      # color = "black",
      color = Nodes$color
      # colors = colorRampPalette(brewer.pal(10,"Spectral"))(nrow(Nodes)),
      , pad = 4
    ), # 10 Pixel
    link = list(
      source = Edges$from,
      target = Edges$to,
      value = Edges$weight
      # ,color = adjustcolor(Edges$color,alpha.f=0.4)
    )
  ) %>%
    layout(margin = m) %>%
    plotly::add_annotations(
      x = Nodes$coordX,
      y = 1.08,
      text = factor(Nodes$group),
      showarrow = F, xanchor = "center",
      font = list(
        color = "Dark",
        family = "TimesNewRoman",
        size = 14
      )
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "toImage",
        "sendDataToCloud",
        "pan2d",
        "select2d",
        "lasso2d",
        "toggleSpikelines",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )

  # networkD3::sankeyNetwork(Links = Edges, Nodes = Nodes, Source = "from", Target = "to",
  #                          NodeID = "Nodes", Value = "weight", width = width,height=height,fontSize = 12,
  #                          nodeWidth = 30,  NodeGroup = "group",LinkGroup = "group")
}



## function to melt data
meltx <- function(LM) {
  var1 <- rep((1:nrow(LM)), ncol(LM))
  var2 <- sort(var1)
  LMM <-
    data.frame(
      Var1 = rownames(LM)[var1],
      Var2 = colnames(LM)[var2],
      value = matrix(LM, length(LM), 1)
    )
  return(LMM)
}
