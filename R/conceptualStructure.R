utils::globalVariables(c(
  "clust", "Dim1", "Dim2", "label", "id", "shape",
  "color", "contrib", "dim1", "dim2", "nomi", "TC"
))
#' Creating and plotting conceptual structure map of a scientific field
#'
#' The function \code{conceptualStructure} creates a conceptual structure map of
#' a scientific field performing Correspondence Analysis (CA), Multiple Correspondence Analysis (MCA) or Metric Multidimensional Scaling (MDS) and Clustering
#' of a bipartite network of terms extracted from keyword, title or abstract fields.
#'
#' @param M is a data frame obtained by the converting function
#'   \code{\link{convert2df}}. It is a data matrix with cases corresponding to
#'   articles and variables to Field Tag in the original ISI or SCOPUS file.
#' @param field is a character object. It indicates one of the field tags of the
#'   standard ISI WoS Field Tag codify.
#'   field can be equal to one of these tags:
#'   \tabular{lll}{
#'   \code{ID}\tab   \tab Keywords Plus associated by ISI or SCOPUS database\cr
#'   \code{DE}\tab   \tab Author's keywords\cr
#'   \code{KW_Merged}\tab   \tab All keywords\cr
#'   \code{ID_TM}\tab   \tab Keywords Plus stemmed through the Porter's stemming algorithm\cr
#'   \code{DE_TM}\tab   \tab Author's Keywords stemmed through the Porter's stemming algorithm\cr
#'   \code{TI}\tab   \tab Terms extracted from titles\cr
#'   \code{AB}\tab   \tab Terms extracted from abstracts}
#' @param ngrams is an integer between 1 and 3. It indicates the type of n-gram to extract from texts.
#' An n-gram is a contiguous sequence of n terms. The function can extract n-grams composed by 1, 2, 3 or 4 terms. Default value is \code{ngrams=1}.
#' @param method is a character object. It indicates the factorial method used to create the factorial map. Use \code{method="CA"} for Correspondence Analysis,
#'  \code{method="MCA"} for Multiple Correspondence Analysis or \code{method="MDS"} for Metric Multidimensional Scaling. The default is \code{method="MCA"}
#' @param minDegree is an integer. It indicates the minimum occurrences of terms to analyze and plot. The default value is 2.
#' @param clust is an integer or a character. If clust="auto", the number of cluster is chosen automatically, otherwise clust can be an integer between 2 and 8.
#' @param k.max is an integer. It indicates the maximum number of cluster to keep. The default value is 5. The max value is 20.
#' @param stemming is logical. If TRUE the Porter's Stemming algorithm is applied to all extracted terms. The default is \code{stemming = FALSE}.
#' @param labelsize is an integer. It indicates the label size in the plot. Default is \code{labelsize=10}
#' @param quali.supp is a vector indicating the indexes of the categorical supplementary variables. It is used only for CA and MCA.
#' @param quanti.supp is a vector indicating the indexes of the quantitative supplementary variables. It is used only for CA and MCA.
#' @param documents is an integer. It indicates the number of documents per cluster to plot in the factorial map. The default value is 2. It is used only for CA and MCA.
#' @param graph is logical. If TRUE the function plots the maps otherwise they are saved in the output object. Default value is TRUE
#' @param remove.terms is a character vector. It contains a list of additional terms to delete from the documents before term extraction. The default is \code{remove.terms = NULL}.
#' @param synonyms is a character vector. Each element contains a list of synonyms, separated by ";",  that will be merged into a single term (the first word contained in the vector element). The default is \code{synonyms = NULL}.
#' @return It is an object of the class \code{list} containing the following components:
#'
#' \tabular{lll}{
#' net \tab  \tab bipartite network\cr
#' res \tab       \tab Results of CA, MCA or MDS method\cr
#' km.res \tab      \tab Results of cluster analysis\cr
#' graph_terms \tab      \tab Conceptual structure map (class "ggplot2")\cr
#' graph_documents_Contrib \tab      \tab Factorial map of the documents with the highest contributes (class "ggplot2")\cr
#' graph_docuemnts_TC \tab      \tab Factorial map of the most cited documents (class "ggplot2")}
#'
#' @examples
#' # EXAMPLE Conceptual Structure using Keywords Plus
#'
#' data(scientometrics, package = "bibliometrixData")
#'
#' CS <- conceptualStructure(scientometrics,
#'   field = "ID", method = "CA",
#'   stemming = FALSE, minDegree = 3, k.max = 5
#' )
#'
#' @seealso \code{\link{termExtraction}} to extract terms from a textual field (abstract, title,
#' author's keywords, etc.) of a bibliographic data frame.
#' @seealso \code{\link{biblioNetwork}} to compute a bibliographic network.
#' @seealso \code{\link{cocMatrix}} to compute a co-occurrence matrix.
#' @seealso \code{\link{biblioAnalysis}} to perform a bibliometric analysis.
#'
#' @export
conceptualStructure <- function(M, field = "ID", ngrams = 1, method = "MCA", quali.supp = NULL, quanti.supp = NULL, minDegree = 2,
                                clust = "auto", k.max = 5, stemming = FALSE, labelsize = 10, documents = 2, graph = TRUE,
                                remove.terms = NULL, synonyms = NULL) {
  # cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  cbPalette <- colorlist() # c(brewer.pal(9, 'Set1')[-6], brewer.pal(8, 'Set2')[-7], brewer.pal(12, 'Paired')[-11],brewer.pal(12, 'Set3')[-c(2,8,12)])

  # if (!is.null(quali.supp)){
  #   QSUPP=data.frame(M[,quali.supp])
  #   names(QSUPP)=names(M)[quali.supp]
  #   row.names(QSUPP)=tolower(row.names(M))
  # }
  #
  # if (!is.null(quanti.supp)){
  #   SUPP=data.frame(M[,quanti.supp])
  #   names(SUPP)=names(M)[quanti.supp]
  #   row.names(SUPP)=tolower(row.names(M))
  # }
  binary <- FALSE
  if (method == "MCA") {
    binary <- TRUE
  }

  switch(field,
    ID = {
      # Create a bipartite network of Keyword plus
      #
      # each row represents a manuscript
      # each column represents a keyword (1 if present, 0 if absent in a document)
      CW <- cocMatrix(M, Field = "ID", type = "matrix", sep = ";", binary = binary, remove.terms = remove.terms, synonyms = synonyms)
      # Define minimum degree (number of occurrences of each Keyword)
      CW <- CW[, colSums(CW) >= minDegree]
      # Delete empty rows
      CW <- CW[, !(colnames(CW) %in% "NA")]
      CW <- CW[rowSums(CW) > 0, ]
    },
    DE = {
      CW <- cocMatrix(M, Field = "DE", type = "matrix", sep = ";", binary = binary, remove.terms = remove.terms, synonyms = synonyms)
      # Define minimum degree (number of occurrences of each Keyword)
      CW <- CW[, colSums(CW) >= minDegree]
      # Delete empty rows
      CW <- CW[rowSums(CW) > 0, ]
      CW <- CW[, !(colnames(CW) %in% "NA")]
    },
    KW_Merged = {
      CW <- cocMatrix(M, Field = "KW_Merged", type = "matrix", sep = ";", binary = binary, remove.terms = remove.terms, synonyms = synonyms)
      # Define minimum degree (number of occurrences of each Keyword)
      CW <- CW[, colSums(CW) >= minDegree]
      # Delete empty rows
      CW <- CW[rowSums(CW) > 0, ]
      CW <- CW[, !(colnames(CW) %in% "NA")]
    },
    ID_TM = {
      M <- termExtraction(M, Field = "ID", remove.numbers = TRUE, stemming = stemming, language = "english", remove.terms = remove.terms, synonyms = synonyms, keep.terms = NULL, verbose = FALSE)

      CW <- cocMatrix(M, Field = "ID_TM", type = "matrix", sep = ";", binary = binary)
      # Define minimum degree (number of occurrences of each Keyword)
      CW <- CW[, colSums(CW) >= minDegree]
      CW <- CW[, !(colnames(CW) %in% "NA")]
      # Delete empty rows
      CW <- CW[rowSums(CW) > 0, ]
    },
    DE_TM = {
      M <- termExtraction(M, Field = "DE", remove.numbers = TRUE, stemming = stemming, language = "english", remove.terms = remove.terms, synonyms = synonyms, keep.terms = NULL, verbose = FALSE)

      CW <- cocMatrix(M, Field = "DE_TM", type = "matrix", sep = ";", binary = binary)
      # Define minimum degree (number of occurrences of each Keyword)
      CW <- CW[, colSums(CW) >= minDegree]
      # Delete empty rows
      CW <- CW[, !(colnames(CW) %in% "NA")]
      CW <- CW[rowSums(CW) > 0, ]
    },
    TI = {
      M <- termExtraction(M, Field = "TI", remove.numbers = TRUE, stemming = stemming, language = "english", remove.terms = remove.terms, synonyms = synonyms, keep.terms = NULL, verbose = FALSE, ngrams = ngrams)

      CW <- cocMatrix(M, Field = "TI_TM", type = "matrix", sep = ";", binary = binary)
      # Define minimum degree (number of occurrences of each Keyword)
      CW <- CW[, colSums(CW) >= minDegree]
      # Delete empty rows
      CW <- CW[, !(colnames(CW) %in% "NA")]
      CW <- CW[rowSums(CW) > 0, ]
    },
    AB = {
      M <- termExtraction(M, Field = "AB", remove.numbers = TRUE, stemming = stemming, language = "english", remove.terms = remove.terms, synonyms = synonyms, keep.terms = NULL, verbose = FALSE, ngrams = ngrams)

      CW <- cocMatrix(M, Field = "AB_TM", type = "matrix", sep = ";", binary = binary)
      # Define minimum degree (number of occurrences of each Keyword)
      CW <- CW[, colSums(CW) >= minDegree]
      # Delete empty rows
      CW <- CW[rowSums(CW) > 0, ]
      CW <- CW[, !(colnames(CW) %in% "NA")]
      # Recode as dataframe
      # CW=data.frame(apply(CW,2,factor))
    }
  )

  colnames(CW) <- label <- tolower(colnames(CW))
  rownames(CW) <- tolower(rownames(CW))
  p <- dim(CW)[2]
  quali <- NULL
  quanti <- NULL

  results <- factorial(CW, method = method, quanti = quanti, quali = quali)
  res.mca <- results$res.mca
  df <- results$df
  row.names(df) <- label
  # row.names(df) <- gsub("\\."," ",row.names(df))
  docCoord <- results$docCoord
  df_quali <- results$df_quali
  df_quanti <- results$df_quanti

  ### Total Citations of documents
  if ("TC" %in% names(M) & method != "MDS") {
    docCoord$TC <- as.numeric(M[toupper(rownames(docCoord)), "TC"])
  }


  # Selection of optimal number of clusters (gap statistics)
  # a=fviz_nbclust((df), kmeans, method = "gap_stat",k.max=k.max)['data']$data$y
  km.res <- hclust(dist(df), method = "average")

  if (clust == "auto") {
    clust <- min((length(km.res$height) - which.max(diff(km.res$height)) + 1), k.max)
  } else {
    clust <- max(1, min(as.numeric(clust), k.max))
  }

  km.res$data <- df
  km.res$cluster <- cutree(km.res, k = clust)
  km.res$data.clust <- data.frame(km.res$data, clust = km.res$cluster)
  centers <- km.res$data.clust %>%
    group_by(clust) %>%
    summarise("Dim1" = mean(Dim1), "Dim2" = mean(Dim2)) %>%
    as.data.frame()

  km.res$centers <- centers[, c(2, 3, 1)]

  data("logo", envir = environment())
  logo <- grid::rasterGrob(logo, interpolate = TRUE)

  df_clust <- km.res$data.clust %>%
    mutate(
      shape = "1",
      label = row.names(.)
    ) %>%
    bind_rows(km.res$centers %>% mutate(shape = "0", label = "")) %>%
    mutate(color = colorlist()[clust])

  hull_data <-
    df_clust %>%
    group_by(clust) %>%
    slice(chull(Dim1, Dim2))

  hull_data <- hull_data %>%
    bind_rows(
      hull_data %>% group_by(clust) %>% slice_head(n = 1)
    ) %>%
    mutate(id = row_number()) %>%
    arrange(clust, id)

  size <- labelsize

  b <- ggplot(df_clust, aes(x = Dim1, y = Dim2, shape = shape, color = color)) +
    geom_point() +
    geom_polygon(
      data = hull_data,
      aes(
        fill = color,
        colour = color
      ),
      alpha = 0.3,
      show.legend = FALSE
    ) +
    ggrepel::geom_text_repel(aes(label = label)) +
    theme_minimal() +
    labs(title = paste("Conceptual Structure Map - method: ", method, collapse = "", sep = "")) +
    geom_hline(yintercept = 0, linetype = "dashed", color = adjustcolor("grey40", alpha.f = 0.7)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = adjustcolor("grey40", alpha.f = 0.7)) +
    theme(
      text = element_text(size = size),
      axis.title = element_text(size = size, face = "bold"),
      plot.title = element_text(size = size + 1, face = "bold"),
      panel.background = element_rect(fill = "white", colour = "white"),
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      axis.line.y = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  if (method != "MDS") {
    b <- b + xlab(paste("Dim 1 (", round(res.mca$eigCorr$perc[1], 2), "%)", sep = "")) +
      ylab(paste("Dim 2 (", round(res.mca$eigCorr$perc[2], 2), "%)", sep = ""))
  } else {
    b <- b + xlab("Dim 1") + ylab("Dim 2")
  }

  b <- b + theme(legend.position = "none")

  ## logo coordinates
  coord_b <- plotCoord(b)

  b <- b + annotation_custom(logo, xmin = coord_b[1], xmax = coord_b[2], ymin = coord_b[3], ymax = coord_b[4])

  if (isTRUE(graph)) {
    plot(b)
  }

  b_dend <- dendPlot(km.res, clust = clust, label.cex = labelsize * 0.07, graph = FALSE)

  if (isTRUE(graph)) {
    plot(b_dend)
  }

  if (method != "MDS") {
    ## Factorial map of most contributing documents


    if (documents > dim(docCoord)[1]) {
      documents <- dim(docCoord)[1]
    }

    centers <- data.frame(dim1 = km.res$centers[, 1], dim2 = km.res$centers[, 2])
    centers$color <- cbPalette[1:dim(centers)[1]]
    row.names(centers) <- paste("cluster", as.character(1:dim(centers)[1]), sep = "")
    # A=docCoord[1:documents,1:2]
    # A=docCoord[,1:2]
    A <- euclDist(docCoord[, 1:2], centers)
    docCoord$Cluster <- A$color
    # A=A[1:documents,]
    A$color <- cbPalette[A$color]

    A$contrib <- docCoord$contrib
    A <- A %>%
      mutate(names = row.names(A)) %>%
      group_by(color) %>%
      top_n(n = documents, wt = contrib) %>%
      select(!"contrib") %>%
      as.data.frame()

    row.names(A) <- A$names
    A <- A[, -4]

    names(centers) <- names(A)
    A <- rbind(A, centers)
    x <- A$dim1
    y <- A$dim2
    A[, 4] <- row.names(A)

    names(A)[4] <- "nomi"

    df_all <- rbind(as.matrix(df), as.matrix(A[, 1:2]))
    rangex <- c(min(df_all[, 1]), max(df_all[, 1]))
    rangey <- c(min(df_all[, 2]), max(df_all[, 2]))

    b_doc <- ggplot(aes(x = dim1, y = dim2, label = nomi), data = A) +
      geom_point(size = 2, color = A$color) +
      labs(title = "Factorial map of the documents with the highest contributes") +
      geom_label_repel(
        box.padding = unit(0.5, "lines"), size = (log(labelsize * 3)), fontface = "bold",
        fill = adjustcolor(A$color, alpha.f = 0.6), color = "white", segment.alpha = 0.5, segment.color = "gray"
      ) +
      scale_x_continuous(limits = rangex, breaks = seq(round(rangex[1]), round(rangex[2]), 1)) +
      scale_y_continuous(limits = rangey, breaks = seq(round(rangey[1]), round(rangey[2]), 1)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = adjustcolor("grey40", alpha.f = 0.7)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = adjustcolor("grey40", alpha.f = 0.7)) +
      theme(
        # panel.border =  element_rect(fill=NA, size = 0.3, linetype = 'dashed', colour = adjustcolor("gray60",alpha.f = 0.7)),
        text = element_text(size = labelsize),
        axis.title = element_text(size = labelsize, face = "bold"),
        plot.title = element_text(size = labelsize + 1, face = "bold"),
        panel.background = element_rect(fill = "white", colour = "white"),
        # panel.grid.major = element_line(size = 0.3, linetype = 'dashed', colour = adjustcolor("gray60",alpha.f = 0.7)),
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )



    if (method != "MDS") {
      b_doc <- b_doc + xlab(paste("Dim 1 (", round(res.mca$eigCorr$perc[1], 2), "%)", sep = "")) +
        ylab(paste("Dim 2 (", round(res.mca$eigCorr$perc[2], 2), "%)", sep = ""))
    } else {
      b_doc <- b_doc + xlab("Dim 1") + ylab("Dim 2")
    }

    ## logo coordinates
    xl <- c(rangex[2] - 0.02 - diff(rangex) * 0.125, rangex[2] - 0.02)
    yl <- c(rangey[1], rangey[1] + diff(rangey) * 0.125) + 0.02
    b_doc <- b_doc + annotation_custom(logo, xmin = xl[1], xmax = xl[2], ymin = yl[1], ymax = yl[2])
    ##

    if (isTRUE(graph)) {
      (plot(b_doc))
    }

    ## Factorial map of the most cited documents
    docCoord <- docCoord[order(-docCoord$TC), ]
    # B=docCoord[1:documents,1:2]
    B <- euclDist(docCoord[, 1:2], centers)
    B$color <- cbPalette[B$color]

    B$TC <- docCoord$TC
    B <- B %>%
      mutate(names = row.names(B)) %>%
      group_by(color) %>%
      top_n(n = documents, wt = TC) %>%
      select(!"TC") %>%
      as.data.frame()

    row.names(B) <- B$names
    B <- B[, -4]
    B <- rbind(B, centers)
    x <- B$dim1
    y <- B$dim2
    B[, 4] <- row.names(B)
    names(B)[4] <- "nomi"
    df_all_TC <- rbind(as.matrix(df), as.matrix(B[, 1:2]))
    rangex <- c(min(df_all_TC[, 1]), max(df_all_TC[, 1]))
    rangey <- c(min(df_all_TC[, 2]), max(df_all_TC[, 2]))

    b_doc_TC <- ggplot(aes(x = dim1, y = dim2, label = nomi), data = B) +
      geom_point(size = 2, color = B$color) +
      labs(title = "Factorial map of the most cited documents") +
      geom_label_repel(
        box.padding = unit(0.5, "lines"), size = (log(labelsize * 3)), fontface = "bold",
        fill = adjustcolor(B$color, alpha.f = 0.6), color = "white", segment.alpha = 0.5, segment.color = "gray"
      ) +
      scale_x_continuous(limits = rangex, breaks = seq(round(rangex[1]), round(rangex[2]), 1)) +
      scale_y_continuous(limits = rangey, breaks = seq(round(rangey[1]), round(rangey[2]), 1)) +
      xlab(paste("Dim 1 (", round(res.mca$eigCorr$perc[1], 2), "%)", sep = "")) +
      ylab(paste("Dim 2 (", round(res.mca$eigCorr$perc[2], 2), "%)", sep = "")) +
      geom_hline(yintercept = 0, linetype = "dashed", color = adjustcolor("grey60", alpha.f = 0.7)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = adjustcolor("grey60", alpha.f = 0.7)) +
      theme(
        # panel.border =  element_rect(fill=NA, size = 0.3, linetype = 'dashed', colour = adjustcolor("gray60",alpha.f = 0.7)),
        text = element_text(size = labelsize),
        axis.title = element_text(size = labelsize, face = "bold"),
        plot.title = element_text(size = labelsize + 1, face = "bold"),
        panel.background = element_rect(fill = "white", colour = "white"),
        # panel.grid.major = element_line(size = 0.3, linetype = 'dashed', colour = adjustcolor("gray60",alpha.f = 0.7)),
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )

    ## logo coordinates
    xl <- c(rangex[2] - 0.02 - diff(rangex) * 0.125, rangex[2] - 0.02)
    yl <- c(rangey[1], rangey[1] + diff(rangey) * 0.125) + 0.02
    b_doc_TC <- b_doc_TC + annotation_custom(logo, xmin = xl[1], xmax = xl[2], ymin = yl[1], ymax = yl[2])
    ##


    if (isTRUE(graph)) {
      plot(b_doc_TC)
    }

    semanticResults <- list(
      net = CW, res = res.mca, km.res = km.res, graph_terms = b, graph_dendogram = b_dend,
      graph_documents_Contrib = b_doc, graph_documents_TC = b_doc_TC, docCoord = docCoord, coord = results$coord, hull_data = hull_data
    )
  } else {
    semanticResults <- list(
      net = CW, res = res.mca, km.res = km.res, graph_terms = b, graph_dendogram = b_dend,
      graph_documents_Contrib = NULL, graph_documents_TC = NULL, docCoord = NULL, coord = NA, hull_data = hull_data
    )
  }

  params <- list(
    field = field,
    ngrams = ngrams,
    method = method,
    quali.supp = quali.supp,
    quanti.supp = quanti.supp,
    minDegree = minDegree,
    clust = clust,
    k.max = k.max,
    stemming = stemming,
    labelsize = labelsize,
    documents = documents,
    graph = graph,
    remove.terms = remove.terms,
    synonyms = synonyms
  )

  semanticResults$params <- data.frame(params = names(unlist(params)), values = unlist(params), row.names = NULL)


  return(semanticResults)
}


factorial <- function(X, method, quanti = NULL, quali = NULL) {
  df_quali <- data.frame()
  df_quanti <- data.frame()
  switch(method,
    ### CORRESPONDENCE ANALYSIS ###
    CA = {
      res.mca <- ca::ca(X, nd = 2)
      # Get coordinates of keywords
      coord <- list(
        coord = res.mca$colcoord,
        contrib = data.frame((res.mca$colcoord[, 1:2]^2) * res.mca$colmass),
        cos2 = data.frame(((res.mca$colcoord[, 1:2]^2) / (res.mca$coldist)))
      )
      # df <- data.frame(coord$coord)
      coord_doc <- list(
        coord = res.mca$rowcoord,
        contrib = data.frame((res.mca$rowcoord[, 1:2]^2) * res.mca$rowmass),
        cos2 = data.frame(((res.mca$rowcoord[, 1:2]^2) / (res.mca$rowdist)))
      )
      # df_doc <- data.frame(coord_doc$coord)
    },
    ### MULTIPLE CORRESPONDENCE ANALYSIS ###
    MCA = {
      X <- data.frame(apply(X, 2, factor))
      res.mca <- ca::mjca(X, nd = 2, lambda = "indicator", ps = "_")
      # Get coordinates of keywords (we take only categories "1"")
      K <- 2
      I <- dim(res.mca$rowcoord)[1]
      J <- dim(res.mca$colcoord)[1]
      evF <- matrix(rep(res.mca$sv[1:K], I), I, K, byrow = TRUE)
      evG <- matrix(rep(res.mca$sv[1:K], J), J, K, byrow = TRUE)
      rpc <- res.mca$rowcoord[, 1:2] * evF
      cpc <- res.mca$colcoord[, 1:2] * evG

      coord <- list(
        coord = data.frame(Dim1 = cpc[, 1], Dim2 = cpc[, 2], label = res.mca$levelnames, row.names = res.mca$levelnames) %>%
          dplyr::filter(substr(label, nchar(label) - 1, nchar(label)) == "_1") %>%
          select(-"label"),
        contrib = data.frame(cpc^2 * res.mca$colmass / res.mca$sv[1:2], label = res.mca$levelnames, row.names = res.mca$levelnames) %>%
          dplyr::filter(substr(label, nchar(label) - 1, nchar(label)) == "_1") %>%
          select(-"label"),
        cos2 = data.frame(((cpc^2) / (res.mca$coldist)), label = res.mca$levelnames, row.names = res.mca$levelnames) %>%
          dplyr::filter(substr(label, nchar(label) - 1, nchar(label)) == "_1") %>%
          select(-"label")
      )
      row.names(coord$coord) <- row.names(coord$contrib) <- row.names(coord$cos2) <- substr(row.names(coord$coord), 1, nchar(row.names(coord$coord)) - 2)
      # df <- coord$coord

      coord_doc <- list(
        coord = data.frame(Dim1 = rpc[, 1], Dim2 = rpc[, 1], row.names = row.names(X)),
        contrib = data.frame((rpc[, 1:2]^2) * res.mca$rowmass / res.mca$sv[1:2]),
        cos2 = data.frame(res.mca$rowmass * rpc^2 / res.mca$rowinertia)
      )
      # df_doc <- coord_doc$coord
    },
    MDS = {
      NetMatrix <- Matrix::crossprod(X, X)
      Net <- 1 - normalizeSimilarity(NetMatrix, type = "association")
      Matrix::diag(Net) <- 0
      # Net=as.matrix(Net)
      res.mca <- Net %>%
        # dist() %>%
        cmdscale()
      colnames(res.mca) <- c("Dim1", "Dim2")
      df <- data.frame(res.mca)
      row.names(df) <- row.names(Net)
    }
  )

  if (method != "MDS") {
    #
    docCoord <- as.data.frame(cbind(coord_doc$coord, rowSums(coord_doc$contrib)))
    names(docCoord) <- c("dim1", "dim2", "contrib")
    docCoord <- docCoord[order(-docCoord$contrib), ]

    # Benzecrì eigenvalue correction
    res.mca <- eigCorrection(res.mca)

    res.mca$coord_doc <- coord_doc

    results <- list(res.mca = res.mca, df = coord$coord, df_doc = coord_doc$coord, df_quali = df_quali, df_quanti = df_quanti, docCoord = docCoord, coord = coord)
  } else {
    results <- list(res.mca = res.mca, df = df, df_doc = NA, df_quali = NA, df_quanti = NA, docCoord = NA, coord = NA)
  }
  return(results)
}

euclDist <- function(x, y) {
  df <- as.data.frame(matrix(NA, dim(x)[1], dim(y)[1]))
  row.names(df) <- row.names(x)
  colnames(df) <- row.names(y)
  for (i in 1:dim(y)[1]) {
    ref <- y[i, 1:2]
    df[, i] <- apply(x, 1, function(x) sqrt(sum((x - ref)^2)))
  }
  x$color <- apply(df, 1, function(m) {
    which(m == min(m))
  })
  return(x)
}

eigCorrection <- function(res) {
  # Benzecri correction calculation
  n <- length(res$sv)
  e <- res$sv^2
  # n <- nrow(res$eig)
  # e <- res$eig[,1]
  eigBenz <- ((n / (n - 1))^2) * ((e - (1 / n))^2)
  eigBenz[e < 1 / n] <- 0
  perc <- eigBenz / sum(eigBenz) * 100
  cumPerc <- cumsum(perc)
  res$eigCorr <- data.frame(eig = e, eigBenz = eigBenz, perc = perc, cumPerc = cumPerc)
  return(res)
}


plotCoord <- function(g, side = "b") {
  a <- ggplot_build(g)$data

  ymin <- unlist(lapply(a, function(l) {
    if ("y" %in% names(l)) {
      min(l["y"])
    }
  })) %>% min(na.rm = TRUE)

  ymax <- unlist(lapply(a, function(l) {
    if ("y" %in% names(l)) {
      max(l["y"])
    }
  })) %>% max(na.rm = TRUE)

  xmin <- unlist(lapply(a, function(l) {
    if ("x" %in% names(l)) {
      min(l["x"])
    }
  })) %>% min(na.rm = TRUE)

  xmax <- unlist(lapply(a, function(l) {
    if ("x" %in% names(l)) {
      max(l["x"])
    }
  })) %>% max(na.rm = TRUE)

  coord <- c(xmin, xmax, ymin, ymax)

  xl <- c(xmax - 0.02 - diff(c(xmin, xmax)) * 0.125, xmax - 0.02)
  if (side == "b") {
    yl <- c(ymin, ymin + diff(c(ymin, ymax)) * 0.125) + 0.02
  } else {
    yl <- c(ymax - 0.02 - diff(c(ymin, ymax)) * 0.125, ymax - 0.02)
  }
  coord <- c(xl, yl)
}


dendPlot <- function(km.res, clust, label.cex, graph = FALSE) {
  # Dendrogram object
  dend <- as.dendrogram(km.res)
  # vector of colors
  labelColors <- colorlist()[1:clust]

  # cut dendrogram in k clusters
  clusMember <- cutree(km.res, clust)

  # function to get color labels
  colLab <- function(n) {
    if (is.leaf(n)) {
      a <- attributes(n)
      labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
      attr(n, "nodePar") <- c(a$nodePar$lab.col, list(lab.col = labCol, lab.cex = label.cex))
      # attr(n, "label_cex") <- c(a$nodePar$lab.cex, label_cex = 0.1)
    }
    n
  }

  # using dendrapply
  clusDendro <- dendrapply(dend, colLab)
  k <- clust
  n <- length(km.res$labels)
  MidPoint <- (km.res$height[n - k] + km.res$height[n - k + 1]) / 2

  plotRes <- list(dend = clusDendro, line = MidPoint)
  class(plotRes) <- c("bibliodendrogram")

  if (graph) plot(plotRes)

  return(plotRes)
}
