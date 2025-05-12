utils::globalVariables(c(
  "group", "MNLCS", "name", "pagerank_centrality",
  "MNLCS2", "impact", "freq", "centrality", "color",
  "words", "rcentrality", "rimpact", "label", "Cluster",
  "w"
))
#' Coupling Analysis
#'
#' It performs a coupling network analysis and plots community detection results on a bi-dimensional map (Coupling Map).
#'
#' The analysis can be performed on three different units: documents, authors or sources and
#' the coupling strength can be measured using the classical approach (coupled by references)
#' or a novel approach based on unit contents (keywords or terms from titles and abstracts)
#'
#' The x-axis measures the cluster centrality (by Callon's Centrality index) while the y-axis measures the cluster impact
#' by Mean Normalized Local Citation Score (MNLCS).
#' The Normalized Local Citation Score (NLCS) of a document is calculated
#' by dividing the actual count of local citing items by the expected citation rate for documents with the same year of publication.
#'
#' @param M is a bibliographic dataframe.
#' @param analysis is the textual attribute used to select the unit of analysis. It can be \code{analysis = c("documents", "authors", "sources")}.
#' @param field is the textual attribute used to measure the coupling strength. It can be \code{field = c("CR", "ID","DE", "TI", "AB")}.
#' @param n is an integer. It indicates the number of units to include in the analysis.
#' @param label.term is a character. It indicates which content metadata have to use for cluster labeling. It can be \code{label.term = c("ID","DE","TI","AB")}.
#' If \code{label.term = NULL} cluster items will be use for labeling.
#' @param ngrams is an integer between 1 and 4. It indicates the type of n-gram to extract from texts.
#' An n-gram is a contiguous sequence of n terms. The function can extract n-grams composed by 1, 2, 3 or 4 terms. Default value is \code{ngrams=1}.
#' @param impact.measure is a character. It indicates the impact measure used to rank cluster elements (documents, authors or sources).
#' It can be \code{impact.measure = c("local", "global")}.\\
#' With \code{impact.measure = "local"}, \link{couplingMap} calculates elements impact using the Normalized Local Citation Score while
#' using \code{impact.measure = "global"}, the function uses the Normalized Global Citation Score to measure elements impact.
#' @param minfreq is a integer. It indicates the minimum frequency (per thousand) of a cluster. It is a number in the range (0,1000).
#' @param community.repulsion is a real. It indicates the repulsion force among network communities. It is a real number between 0 and 1. Default is \code{community.repulsion = 0.1}.
#' @param stemming is logical. If it is TRUE the word (from titles or abstracts) will be stemmed (using the Porter's algorithm).
#' @param size is numerical. It indicates the size of the cluster circles and is a number in the range (0.01,1).
#' @param n.labels is integer. It indicates how many labels associate to each cluster. Default is \code{n.labels = 1}.
#' @param repel is logical. If it is TRUE ggplot uses geom_label_repel instead of geom_label.
#' @param cluster is a character. It indicates the type of cluster to perform among ("optimal", "louvain","leiden", "infomap","edge_betweenness","walktrap", "spinglass", "leading_eigen", "fast_greedy").
#' @return a list containing:
#' \tabular{lll}{
#' \code{map}\tab   \tab The coupling map as ggplot2 object\cr
#' \code{clusters}\tab   \tab Centrality and Density values for each cluster. \cr
#' \code{data}\tab   \tab A list of units following in each cluster\cr
#' \code{nclust}\tab   \tab The number of clusters\cr
#' \code{NCS}\tab     \tab The Normalized Citation Score dataframe\cr
#' \code{net}\tab    \tab A list containing the network output (as provided from the networkPlot function)}
#'
#' @examples
#' \dontrun{
#' data(management, package = "bibliometrixData")
#' res <- couplingMap(management,
#'   analysis = "authors", field = "CR", n = 250, impact.measure = "local",
#'   minfreq = 3, size = 0.5, repel = TRUE
#' )
#' plot(res$map)
#' }
#'
#' @seealso \code{\link{biblioNetwork}} function to compute a bibliographic network.
#' @seealso \code{\link{cocMatrix}} to compute a bibliographic bipartite network.
#' @seealso \code{\link{networkPlot}} to plot a bibliographic network.
#'
#' @export

couplingMap <- function(M, analysis = "documents", field = "CR", n = 500, label.term = NULL, ngrams = 1, impact.measure = "local", minfreq = 5,
                        community.repulsion = 0.1,
                        stemming = FALSE, size = 0.5, n.labels = 1, repel = TRUE, cluster = "walktrap") {
  if (!(analysis %in% c("documents", "authors", "sources"))) {
    cat('\nanalysis argument is incorrect.\n\nPlease select one of the following choices: "documents", "authors", "sources"\n\n')
    return(NA)
  }
  minfreq <- max(0, floor(minfreq * nrow(M) / 1000))


  Net <- network(M, analysis = analysis, field = field, stemming = stemming, n = n, cluster = cluster, community.repulsion = community.repulsion)

  net <- Net$graph

  NCS <- normalizeCitationScore(M, field = analysis, impact.measure = impact.measure)

  if (impact.measure == "global") {
    NCS$MNLCS <- NCS$MNGCS
    NCS$LC <- NCS$TC
  }

  NCS[, 1] <- toupper(NCS[, 1])

  ### Citation for documents
  label <- V(net)$name


  L <- tibble(id = toupper(label))
  names(L) <- analysis
  D <- left_join(L, NCS, by = analysis, copy = T)

  L <- tibble(id = tolower(label))
  names(L) <- names(Net$cluster_res)[1] <- analysis

  C <- left_join(L, Net$cluster_res, by = analysis, copy = T)


  group <- Net$cluster_obj$membership
  color <- V(net)$color
  color[is.na(color)] <- "#D3D3D3"

  D$group <- group
  D$color <- color

  DC <- cbind(D, C[, -1])
  DC$name <- DC[, 1]
  df_lab <- DC %>%
    group_by(group) %>%
    mutate(
      MNLCS2 = replace(MNLCS, MNLCS < 1, NA), ## remove NCS<1
      MNLCS = round(MNLCS, 2),
      name = tolower(name),
      freq = length(MNLCS)
    ) %>%
    arrange(desc(MNLCS), .by_group = TRUE)

  df <- df_lab %>%
    group_by(group) %>% 
    mutate(
      centrality = mean(pagerank_centrality),
      impact = mean(MNLCS2, na.rm = TRUE),
      impact = replace(impact, is.na(impact), 0)
    ) %>%
    slice_max(MNLCS, n = 10) %>%
    summarize(
      freq = freq[1],
      centrality = centrality[1] * 100,
      impact = impact[1],
      label_cluster = group[1],
      color = color[1],
      label = tolower(paste(name[1:min(n.labels, length(name))], collapse = "\n")),
      words = tolower(paste(name, MNLCS, collapse = "\n"))
    ) %>%
    mutate(
      rcentrality = rank(centrality),
      words = unlist(lapply(words, function(l) {
        l <- unlist(strsplit(l, "\\\n"))
        l <- l[1:(min(length(l), 10))]
        l <- paste0(l, collapse = "\n")
      })),
      rimpact = rank(impact)
    ) %>%
    arrange(group) %>%
    as.data.frame()

  row.names(df) <- df$label

  meandens <- mean(df$rimpact)
  meancentr <- mean(df$rcentrality)
  df <- df[df$freq >= minfreq, ]

  df_lab <- df_lab %>%
    dplyr::filter(group %in% df$group)

  rangex <- max(c(meancentr - min(df$rcentrality), max(df$rcentrality) - meancentr))
  rangey <- max(c(meandens - min(df$rimpact), max(df$rimpact) - meandens))
  xlimits <- c(meancentr - rangex - 0.5, meancentr + rangex + 0.5)
  ylimits <- c(meandens - rangey - 0.5, meandens + rangey + 0.5)


  df_lab <- df_lab[, c(1, 7, 15, 8, 4)]
  names(df_lab) <- c(analysis, "Cluster", "ClusterFrequency", "ClusterColor", "NormalizedLocalCitationScore")
  df_lab$ClusterName <- df$label[df_lab$Cluster]
  # quadrant_names=rep(" ",4) ## empty tooltips for quadrant names

  if (is.null(label.term)) {
    label.term <- "null"
  }
  if (label.term %in% c("DE", "ID", "TI", "AB")) {
    w <- labeling(M, df_lab, term = label.term, n = n, n.labels = n.labels, analysis = analysis, ngrams = ngrams)
    df$label <- w
  }

  data("logo", envir = environment())
  logo <- grid::rasterGrob(logo, interpolate = TRUE)
  x <- c(max(df$rcentrality) - 0.02 - diff(range(df$rcentrality)) * 0.125, max(df$rcentrality) - 0.02) + 0.5
  y <- c(min(df$rimpact), min(df$rimpact) + diff(range(df$rimpact)) * 0.125)

  g <- ggplot(df, aes(x = rcentrality, y = rimpact, text = (words))) +
    geom_point(group = "NA", aes(size = log(as.numeric(freq))), shape = 20, col = adjustcolor(df$color, alpha.f = 0.5)) # Use hollow circles
  if (size > 0) {
    if (isTRUE(repel)) {
      g <- g + geom_label_repel(aes(group = "NA", label = ifelse(freq > 1, unlist(tolower(label)), "")), size = 3 * (1 + size), angle = 0)
    } else {
      g <- g + geom_text(aes(group = "NA", label = ifelse(freq > 1, unlist(tolower(label)), "")), size = 3 * (1 + size), angle = 0)
    }
  }

  g <- g + geom_hline(yintercept = meandens, linetype = 2, color = adjustcolor("black", alpha.f = 0.7)) +
    geom_vline(xintercept = meancentr, linetype = 2, color = adjustcolor("black", alpha.f = 0.7)) +
    theme(legend.position = "none") +
    scale_radius(range = c(10 * (1 + size), 30 * (1 + size))) +
    labs(x = "Centrality", y = "Impact") +
    xlim(xlimits) +
    ylim(ylimits) +
    ggtitle(paste("Clusters by ", toupper(substr(analysis, 1, 1)), substr(analysis, 2, nchar(analysis)), " Coupling", sep = "")) +
    theme(
      axis.text.x = element_blank(),
      panel.background = element_rect(fill = "#FFFFFF"),
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      axis.line.y = element_line(color = "black", linewidth = 0.5),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    annotation_custom(logo, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2])



  row.names(df) <- NULL
  df <- df %>% rename(items = words)

  params <- list(
    analysis = analysis,
    field = field,
    n = n,
    minfreq = minfreq,
    label.term = label.term,
    ngrams = ngrams,
    impact.measure = impact.measure,
    stemming = stemming,
    n.labels = n.labels,
    size = size,
    community.repulsion = community.repulsion,
    repel = repel,
    cluster = cluster
  )
  params <- data.frame(params = names(unlist(params)), values = unlist(params), row.names = NULL)

  results <- list(map = g, clusters = df, data = df_lab, nclust = dim(df)[1], NCS = D, net = Net, params = params)
  return(results)
}

coupling <- function(M, field, analysis) {
  if (field == "TI") {
    field <- "TI_TM"
  }
  if (field == "AB") field <- "AB_TM"
  switch(analysis,
    documents = {
      WF <- t(cocMatrix(M, Field = field, short = TRUE))
      NetMatrix <- crossprod(WF, WF)
    },
    authors = {
      WF <- cocMatrix(M, Field = field, short = TRUE)
      WA <- cocMatrix(M, Field = "AU", short = TRUE)
      FA <- t(crossprod(WA, WF))
      NetMatrix <- crossprod(FA, FA)
    },
    sources = {
      WF <- cocMatrix(M, Field = field, short = TRUE)
      WS <- cocMatrix(M, Field = "SO", short = TRUE)
      FS <- t(crossprod(WS, WF))
      NetMatrix <- crossprod(FS, FS)
    }
  )
  return(NetMatrix)
}

network <- function(M, analysis, field, stemming, n, cluster, community.repulsion) {
  switch(analysis,
    documents = {
      switch(field,
        CR = {
          NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", short = TRUE, shortlabel = FALSE, sep = ";")
          type <- "D_CR"
        },
        {
          # field ID, DE, TI, AB
          if (field %in% c("TI", "AB")) {
            M <- termExtraction(M, Field = field, verbose = FALSE, stemming = stemming)
            type <- "D_KW"
          }
          NetMatrix <- coupling(M, field, analysis = "documents")
        }
      )
    },
    authors = {
      switch(field,
        CR = {
          NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "authors", short = TRUE)
          type <- "AU_CR"
        },
        {
          # field ID, DE, TI, AB
          if (field %in% c("TI", "AB")) {
            M <- termExtraction(M, Field = field, verbose = FALSE, stemming = stemming)
            type <- "AU_KW"
          }
          NetMatrix <- coupling(M, field, analysis = "authors")
        }
      )
    },
    sources = {
      switch(field,
        CR = {
          NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "sources", short = TRUE)
          type <- "SO_CR"
        },
        {
          # field ID, DE, TI, AB
          if (field %in% c("TI", "AB")) {
            M <- termExtraction(M, Field = field, verbose = FALSE, stemming = stemming)
            type <- "SO_KW"
          }
          NetMatrix <- coupling(M, field, analysis = "sources")
        }
      )
    }
  )

  # delete empty vertices
  NetMatrix <- NetMatrix[nchar(colnames(NetMatrix)) != 0, nchar(colnames(NetMatrix)) != 0]

  if (nrow(NetMatrix) > 0) {
    Net <- networkPlot(NetMatrix,
      normalize = "salton", n = n, Title = paste("Coupling network of ", analysis, " using ", field, sep = ""), type = "auto",
      labelsize = 2, halo = F, cluster = cluster, remove.isolates = TRUE, community.repulsion = community.repulsion,
      remove.multiple = FALSE, noloops = TRUE, weighted = TRUE, label.cex = T, edgesize = 5,
      size = 1, edges.min = 1, label.n = n, verbose = FALSE
    )
  } else {
    cat("\n\nNetwork matrix is empty!\nThe analysis cannot be performed\n\n")
    return()
  }

  return(Net)
}

## cluster labeling
labeling <- function(M, df_lab, term, n, n.labels, analysis, ngrams) {
  if (term %in% c("TI", "AB")) {
    M <- termExtraction(M, Field = term, ngrams = ngrams, verbose = FALSE)
    term <- paste(term, "_TM", sep = "")
  }
  switch(analysis,
    documents = {
      df <- left_join(df_lab, M, by = c("documents" = "SR"))
    },
    authors = {
      WF <- cocMatrix(M, Field = term, short = TRUE)
      WA <- cocMatrix(M, Field = "AU", n = n, short = TRUE)
      AF <- (crossprod(WA, WF))
      A <- apply(AF, 1, function(x) {
        paste(rep(names(x)[x > 0], x[x > 0]), collapse = ";")
      })
      A <- data.frame(AU = names(A), words = A)
      names(A)[2] <- term
      df <- left_join(df_lab, A, by = c("authors" = "AU"))
    },
    sources = {
      df <- left_join(df_lab, M, by = c("sources" = "SO"))
    }
  )

  # clusters <- unique(df$Cluster)
  # w <- character(length(clusters))
  df$SR <- df[, 1]
  tab_global <- tableTag(df, term)
  tab_global <- data.frame(label = names(tab_global), tot = as.numeric(tab_global), n = nrow(M))

  df <- df %>%
    group_by(Cluster) %>%
    do(w = best_lab(.data, tab_global, n.labels, term)) %>%
    unnest(w) %>%
    as.data.frame()

  return(df$w)
}

best_lab <- function(d, tab_global, n.labels, term) {
  tab <- tableTag(d, term)
  tab <- tab[!is.na(names(tab)) & names(tab) != ""]
  
  if (length(tab) == 0) return(tibble(w = "no_label"))
  
  tab1 <- data.frame(label = names(tab), value = as.numeric(tab), stringsAsFactors = FALSE)
  
  tab1 <- tab1 %>%
    left_join(tab_global, by = "label") %>%
    mutate(
      conf = round(.data$value / .data$tot * 100, 1),
      supp = round(.data$tot / n * 100, 1),
      relevance = round(.data$conf * .data$supp / 100, 1)
    ) %>%
    arrange(desc(.data$relevance)) %>%
    slice(1:n.labels)
  
  tibble(w = tolower(paste(tab1$label, " - Conf ", tab1$conf, "%", sep = "", collapse = "\n")))
}



# best_lab <- function(d, tab_global, n.labels, term) {
#   tab <- tableTag(d, term)
#   tab1 <- data.frame(label = names(tab), value = as.numeric(tab), stringsAsFactors = FALSE)
#   print(names(tab1))
#   tab1 <- tab1 %>%
#     left_join(tab_global, by = "label") %>%
#     mutate(
#       conf = round(.data$value / .data$tot * 100, 1),
#       supp = round(.data$tot / n * 100, 1),
#       relevance = round(.data$conf * .data$supp / 100, 1)
#     ) %>%
#     arrange(desc(.data$relevance)) %>%
#     slice(1:n.labels)
# 
#   # tolower(paste(tab$label," Supp ",tab$supp,"% - Conf ", tab$conf,"%", sep="", collapse="\n"))
#   tolower(paste(tab1$label, " - Conf ", tab1$conf, "%", sep = "", collapse = "\n"))
# }
