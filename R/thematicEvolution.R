utils::globalVariables(c(
  "params",
  "Cluster_Label",
  "Occurrences",
  "Words",
  "Cluster_Label.y",
  "Cluster_Label.x",
  "min",
  "tot",
  "len.x",
  "len.y",
  "Occurrences.x",
  "Occurrences.y",
  "tot.x",
  "tot.y",
  "Cluster.x",
  "Cluster.y",
  "Occ",
  "name",
  "group",
  "Inc_Weighted",
  "PageRank",
  "PageRankIndex",
  "PageRankProd",
  "PageRankTot.x",
  "PageRankTot.y",
  "PageRankXY",
  "PageRankXYTot",
  "has_natural_successor",
  "idnew",
  "is_natural_lineage",
  "lineage_strength",
  "new old_combined",
  "pagerank_assigned",
  "Cluster_PageRank",
  "Cluster_PageRank.x",
  "Cluster_PageRank.y",
  "Lineage_Strength",
  "fuzzy_cardinality",
  "rel_freq_temp",
  "separate_longer_delim",
  "total_pagerank"
))

#' Perform a Thematic Evolution Analysis
#'
#' It performs a Thematic Evolution Analysis based on co-word network analysis and clustering,
#' following the integrated framework of Aria, D'Aniello, Misuraca & Spano (2026).
#'
#' \code{\link{thematicEvolution}} starts from two or more thematic maps created by \code{\link{thematicMap}} function:
#' in each period the themes are the communities of the association-strength co-occurrence network, and each
#' document belongs to every theme with a graded (fuzzy) membership based on the PageRank of its terms within the theme.
#' Themes of consecutive periods are linked by their \emph{lineage strength}
#' \deqn{L = \alpha I_w + (1 - \alpha) \Omega,}
#' where the weighted inclusion index \eqn{I_w} is the share of the source theme's PageRank carried by the terms it
#' shares with the target (directional), and the importance index \eqn{\Omega} is the PageRank-weighted overlap of the
#' shared terms in both themes. A link enters the evolutionary graph when \eqn{L \ge} \code{min.lineage}, or when it
#' is among the \code{top.k} strongest links leaving its source theme.\cr\cr
#'
#' References:\cr
#' Aria, M., D'Aniello, L., Misuraca, M., & Spano, M. (2026). Rethinking thematic evolution in science mapping:
#' An integrated framework for longitudinal analysis. Journal of Informetrics, 20, 101877. \doi{10.1016/j.joi.2026.101877}\cr
#'
#' Cobo, M. J., Lopez-Herrera, A. G., Herrera-Viedma, E., & Herrera, F. (2011). An approach for detecting, quantifying,
#' and visualizing the evolution of a research field: A practical application to the fuzzy sets theory field. Journal of Informetrics, 5(1), 146-166.\cr
#'
#' @param M is a bibliographic data frame obtained by the converting function \code{\link{convert2df}}.
#' @param field is a character object. It indicates the content field to use. Field can be one of c=("ID","DE","KW_Merged","TI","AB"). Default value is \code{field="ID"}.
#' @param years is a numeric vector of one or more unique cut points.
#' @param n is numerical. It indicates the number of words to use in the network analysis
#' @param minFreq is numerical. It indicates the minimum frequency of a term (per thousand documents of the period, and at least 2, see \code{\link{thematicMap}}): terms occurring less often are left out before the co-occurrence network of the period is built.
#' @param ngrams is an integer between 1 and 4. It indicates the type of n-gram to extract from texts.
#' An n-gram is a contiguous sequence of n terms. The function can extract n-grams composed by 1, 2, 3 or 4 terms. Default value is \code{ngrams=1}.
#' @param stemming is logical. If it is TRUE the word (from titles or abstracts) will be stemmed (using the Porter's algorithm).
#' @param size is numerical. It indicates del size of the cluster circles and is a number in the range (0.01,1).
#' @param n.labels is integer. It indicates how many labels associate to each cluster. Default is \code{n.labels = 1}.
#' @param repel is logical. If it is TRUE ggplot uses geom_label_repel instead of geom_label.
#' @param remove.terms is a character vector. It contains a list of additional terms to delete from the documents before term extraction. The default is \code{remove.terms = NULL}.
#' @param synonyms is a character vector. Each element contains a list of synonyms, separated by ";",  that will be merged into a single term (the first word contained in the vector element). The default is \code{synonyms = NULL}.
#' @param cluster is a character. It indicates the type of cluster to perform among ("optimal", "louvain","leiden", "infomap","edge_betweenness","walktrap", "spinglass", "leading_eigen", "fast_greedy").
#' @param seed is numerical. It indicates the seed for random number generator to obtain always the same results. Default value is \code{seed = 1234}.
#' @param alpha is a number in [0, 1]. The weight of the weighted inclusion index (directional coverage) against the
#' importance index (structural relevance) in the lineage strength. Default is \code{alpha = 0.5}.
#' @param min.lineage is a number. The minimum lineage strength of a link of the evolutionary graph (absolute criterion).
#' Default is \code{min.lineage = 0.10}.
#' @param top.k is an integer. Each theme also keeps its \code{top.k} strongest links to the next period, whatever their
#' strength (relative criterion). Default is \code{top.k = 2}.
#' @param assign.evolution.colors is a list. If \code{assign.evolution.colors = list(assign = TRUE)}, the links of the
#' evolutionary graph are coloured by lineage (see \code{\link{assignEvolutionColors}}). Default is
#' \code{assign.evolution.colors = list(assign = TRUE)}. An \code{alpha} element, as in earlier versions, is still
#' accepted and replaces the \code{alpha} argument.
#' @return a list containing:
#' \tabular{lll}{
#' \code{Nodes}\tab   \tab The themes of all periods, with their fuzzy cardinality, their number of incoming and
#' outgoing links in the evolutionary graph and their pattern (emergent, disappearing, continuation, split, merge)\cr
#' \code{Edges}\tab   \tab All links between themes of consecutive periods sharing at least one term, with their
#' \code{lineage_strength}; \code{retained} marks the links of the evolutionary graph\cr
#' \code{pathways}\tab   \tab The maximal directed sequences of themes in the evolutionary graph, with their length (periods),
#' strength (product of the lineage strengths) and cumulative size (sum of the fuzzy cardinalities)\cr
#' \code{Data}\tab   \tab The statistics of every link (inclusion indices, importance index, lineage strength)\cr
#' \code{TM}\tab   \tab The thematic maps of the periods\cr
#' \code{Net}\tab   \tab The co-occurrence networks of the periods}
#'
#'
#' @examples
#' \dontrun{
#' data(management, package = "bibliometrixData")
#' years=c(2004,2008,2015)
#'
#' nexus <- thematicEvolution(management,field="DE",years=years,n=100,minFreq=2)
#' }
#'
#' @seealso \code{\link{thematicMap}} function to create a thematic map based on co-word network analysis and clustering.
#' @seealso \code{\link{cocMatrix}} to compute a bibliographic bipartite network.
#' @seealso \code{\link{networkPlot}} to plot a bibliographic network.
#'
#' @export

thematicEvolution <- function(
  M,
  field = "ID",
  years,
  n = 250,
  minFreq = 2,
  size = 0.5,
  ngrams = 1,
  stemming = FALSE,
  n.labels = 1,
  repel = TRUE,
  remove.terms = NULL,
  synonyms = NULL,
  cluster = "louvain",
  seed = 1234,
  alpha = 0.5,
  min.lineage = 0.10,
  top.k = 2,
  assign.evolution.colors = list(assign = TRUE)
) {
  if (!is.list(assign.evolution.colors) ||
    is.null(assign.evolution.colors$assign)) {
    assign.evolution.colors <- list(assign = TRUE)
  }
  # alpha used to be passed inside assign.evolution.colors: still accepted
  if (!is.null(assign.evolution.colors$alpha)) {
    alpha <- assign.evolution.colors$alpha
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha < 0 || alpha > 1) {
    stop("thematicEvolution(): alpha must be a number between 0 and 1.", call. = FALSE)
  }
  list_df <- timeslice(M, breaks = years)
  # K is the number of periods, and it is always at least two: timeslice()
  # builds its cut points as c(min(PY) - 1, years, max(PY)), so any years
  # vector gives length(years) + 1 periods, and no years at all falls back on
  # k = 5. A branch refusing K < 2 sat after the loop below -- printing the
  # string "Error" and returning NULL -- and has been removed as unreachable.
  K <- length(list_df)

  S <- net <- res <- list()
  Y <- NULL

  for (k in 1:K) {
    Mk <- list_df[[k]]

    # An empty period has no years to be named by -- min() of nothing is Inf,
    # which would label it "Inf--Inf" below -- so it is reported by the
    # interval timeslice() cut it at. A gap in the publication years is enough
    # to produce one.
    if (nrow(Mk) == 0) {
      stop(
        "thematicEvolution(): the period ",
        names(list_df)[k],
        " holds no document, so the evolution cannot be built across it. ",
        "Choose cut points that leave no period empty.",
        call. = FALSE
      )
    }

    Y[k] <- paste(min(Mk$PY), "-", max(Mk$PY), sep = "", collapse = "")
    resk <- thematicMap(
      Mk,
      field = field,
      n = n,
      minfreq = minFreq,
      ngrams = ngrams,
      stemming = stemming,
      size = size,
      n.labels = n.labels,
      repel = repel,
      remove.terms = remove.terms,
      synonyms = synonyms,
      cluster = cluster,
      subgraphs = FALSE,
      seed = seed
    )
    # thematicMap() returns NULL when the co-occurrence network of the slice is
    # empty, which happens when the period holds too few documents or none of
    # its terms reaches minFreq. Reading resk$params then stopped with "no
    # applicable method for 'filter' applied to an object of class NULL",
    # naming neither the period nor the cause.
    if (is.null(resk)) {
      stop(
        "thematicEvolution(): the period ",
        Y[k],
        " holds ",
        nrow(Mk),
        if (nrow(Mk) == 1) " document" else " documents",
        " and yields no co-occurrence network, so the evolution cannot be ",
        "built across it. Choose cut points that give that period more ",
        "documents; if it already holds enough, its terms do not reach ",
        "minFreq.",
        call. = FALSE
      )
    }

    resk$params <- resk$params %>% dplyr::filter(params != "minfreq")
    res[[k]] <- resk
    net[[k]] <- resk$net
  }

  incMatrix <- list()
  for (k in 2:K) {
    res1 <- res[[(k - 1)]]
    res2 <- res[[(k)]]
    if (res1$nclust == 0 | res2$nclust == 0) {
      cat(paste(
        "\nNo topics in the period ",
        k - 1,
        " with this set of input parameters\n\n"
      ))
      return(list(check = FALSE))
    }
    res1$words$Cluster <- paste(
      res1$clusters$name[res1$words$Cluster],
      "--",
      Y[k - 1],
      sep = ""
    )
    res1$clusters$label <- paste(res1$clusters$name, "--", Y[k - 1], sep = "")
    res2$words$Cluster <- paste(
      res2$clusters$name[res2$words$Cluster],
      "--",
      Y[k],
      sep = ""
    )
    res2$clusters$label <- paste(res2$clusters$name, "--", Y[k], sep = "")
    cluster1 <- res1$words %>%
      group_by(Cluster_Label) %>%
      mutate(
        len = length(Words),
        tot = sum(Occurrences),
        PageRankTot = sum(Cluster_PageRank, na.rm = TRUE)
      )
    cluster2 <- res2$words %>%
      group_by(Cluster_Label) %>%
      mutate(
        len = length(Words),
        tot = sum(Occurrences),
        PageRankTot = sum(Cluster_PageRank, na.rm = TRUE)
      )
    A <- inner_join(cluster1, cluster2, by = "Words") %>%
      group_by(Cluster_Label.x, Cluster_Label.y) %>%
      rowwise() %>%
      mutate(
        min = min(
          Occurrences.x,
          Occurrences.y
        ),
        Occ = sum(Occurrences.x),
        tot = min(tot.x, tot.y),
        PageRankTot.x = min(PageRankTot.x),
        PageRankTot.y = min(PageRankTot.y),
        PageRankXY = Cluster_PageRank.x * Cluster_PageRank.y
      ) %>%
      ungroup() %>%
      mutate(PageRankProd = PageRankTot.x * PageRankTot.y)
    B <- A %>%
      group_by(Cluster_Label.x, Cluster_Label.y) %>%
      reframe(
        CL1 = Cluster.x[1],
        CL2 = Cluster.y[1],
        Words = paste0(Words, collapse = ";", sep = ""),
        sum = sum(min),
        # weighted inclusion (Aria et al., 2026, Eq. 7): the share of the
        # source cluster's PageRank carried by the terms it shares with the
        # target; directional, unlike Inc_index
        Inc_Weighted = sum(Cluster_PageRank.x) / PageRankTot.x[1],
        # n() is the number of shared words: Words is already collapsed here
        Inc_index = n() /
          min(
            len.x,
            len.y
          ),
        Occ = Occ[1],
        Tot = tot[1],
        Stability = n() /
          (len.x[1] +
            len.y[1] -
            n()),
        PageRank1 = sum(Cluster_PageRank.x),
        PageRank2 = sum(Cluster_PageRank.y),
        PageRankXYTot = sum(PageRankXY),
        # importance index (Aria et al., 2026, Eq. 8)
        PageRankIndex = sqrt(PageRankXYTot / PageRankProd[1])
      ) %>%
      data.frame() %>%
      distinct()
    incMatrix[[k - 1]] <- B
  }
  INC <- incMatrix[[1]]
  if (length(incMatrix) > 1) {
    for (i in 2:length(incMatrix)) {
      INC <- rbind(INC, incMatrix[[i]])
    }
  }

  # lineage strength (Aria et al., 2026, Eq. 9) and dual threshold (Eq. 10-11):
  # a link is kept when its strength reaches min.lineage or when it is among
  # the top.k strongest links leaving its source cluster
  INC <- INC %>%
    mutate(Lineage_Strength = alpha * Inc_Weighted + (1 - alpha) * PageRankIndex) %>%
    group_by(CL1) %>%
    mutate(
      Retained = Lineage_Strength >= min.lineage |
        rank(-Lineage_Strength, ties.method = "first") <= top.k
    ) %>%
    ungroup() %>%
    data.frame()
  edges <- INC[, c(
    "CL1",
    "CL2",
    "Inc_index",
    "Inc_Weighted",
    "Stability",
    "PageRankIndex"
  )] #%>%
  #distinct()
  # edges = edges[edges[, 3] > 0, ]
  nodes <- data.frame(name = unique(c(edges$CL1, edges$CL2)))
  nodes$group <- nodes$name

  cont <- 0
  edges[, 7] <- edges[, 1]
  for (i in nodes$name) {
    ind <- which(edges[, 1] == i)
    edges[ind, 1] <- cont
    ind1 <- which(edges[, 2] == i)
    edges[ind1, 2] <- cont
    cont <- cont + 1
  }
  names(edges) <- c(
    "from",
    "to",
    "Inclusion",
    "Inc_Weighted",
    "Stability",
    "PageRankIndex",
    "group"
  )
  edges$from <- as.numeric(edges$from)
  edges$to <- as.numeric(edges$to)
  edges$lineage_strength <- INC$Lineage_Strength
  edges$retained <- INC$Retained

  ### for colors
  nodes <- nodes %>%
    mutate(label = name) %>%
    separate(sep = "--", col = "name", into = c("name", "group"))

  # The nodes of the evolution are the clusters joined by an inclusion edge, so
  # a period none of whose themes shares a word with an adjacent period has no
  # node at all. factor(group, labels = 1:K) then met fewer levels than labels
  # and stopped inside a dplyr verb ("invalid 'labels'"), naming neither the
  # period nor the cause.
  unlinked <- setdiff(Y, unique(nodes$group))
  if (length(unlinked) == K) {
    stop(
      "thematicEvolution(): no theme of any period shares a word with a theme ",
      "of an adjacent period, so there is no evolution to trace. Choose ",
      "different cut points, or lower minFreq so that more words enter each map.",
      call. = FALSE
    )
  }
  if (length(unlinked) > 0) {
    stop(
      "thematicEvolution(): the themes of ",
      if (length(unlinked) == 1) "the period " else "the periods ",
      paste(unlinked, collapse = ", "),
      " share no word with those of the adjacent ",
      if (length(unlinked) == 1) "period" else "periods",
      ", so there is no evolution to trace across ",
      if (length(unlinked) == 1) "it" else "them",
      ". Choose different cut points, or lower minFreq so that more words enter each map.",
      call. = FALSE
    )
  }

  nodes <- nodes %>%
    mutate(slice = factor(group, labels = 1:K))
  Nodes <- data.frame()
  for (i in 1:K) {
    Nodes <- rbind(
      Nodes,
      left_join(
        subset(nodes, nodes$slice == i),
        res[[i]]$clusters[c("color", "name")],
        by = "name"
      )
    )
  }

  # Preparing data for plot
  Nodes$id <- 0:(nrow(Nodes) - 1)
  Nodes <- Nodes %>%
    left_join(
      rbind(
        INC[, -c(1, 2)] %>% select(CL1, sum) %>% rename(label = CL1),
        INC[, -c(1, 2)] %>% select(CL2, sum) %>% rename(label = CL2)
      ) %>%
        group_by(label) %>%
        reframe(sum = max(sum)),
      by = "label"
    )
  Nodes <- Nodes %>%
    group_by(slice) %>%
    mutate(sum = sum / sum(sum, na.rm = T)) %>%
    ungroup()

  params <- list(
    field = field,
    years = years,
    n = n,
    minFreq = minFreq,
    size = size,
    ngrams = ngrams,
    stemming = stemming,
    n.labels = n.labels,
    repel = repel,
    remove.terms = remove.terms,
    synonyms = synonyms,
    cluster = cluster
  )

  params <- data.frame(
    params = names(unlist(params)),
    values = unlist(params),
    row.names = NULL
  )

  periods <- sort(unique(Nodes$group))

  clusters <- data.frame(name = NULL, freq = NULL, group = NULL)

  for (i in 1:length(res)) {
    df <- res[[i]]$clusters %>%
      select(name, freq, fuzzy_cardinality)
    df$group <- periods[i]
    clusters <- bind_rows(clusters, df)
  }

  Nodes <- Nodes %>%
    left_join(clusters, by = c("group", "name"))

  edges$color <- "#D3D3D380"

  evo <- evolutionPatterns(Nodes, edges)
  Nodes <- evo$Nodes

  nexus <- list(
    Nodes = Nodes,
    Edges = edges,
    clusters = clusters,
    pathways = evo$pathways,
    Data = INC[, -c(1, 2)],
    check = TRUE,
    TM = res,
    Net = net,
    params = params
  )

  if (assign.evolution.colors$assign) {
    ## Assign colors based on lineages
    nexus <- assignEvolutionColors(
      nexus = nexus,
      threshold = 0.5,
      palette = NULL,
      alpha = alpha
    )
  }

  return(nexus)
}

# Evolutionary patterns and pathways (Aria et al., 2026, Sect. 3.5), on the
# links kept by the dual threshold. A theme with no incoming link after the
# first period is emergent, one with no outgoing link before the last is
# disappearing; more than one outgoing link is a split, more than one incoming
# link a merge; a single link to a theme with a single incoming link is a
# continuation. A pathway is a maximal directed sequence of themes: its
# strength is the product of the lineage strengths along it (Eq. 13), its size
# the sum of the fuzzy cardinalities of its themes (Eq. 14).
evolutionPatterns <- function(Nodes, Edges) {
  kept <- Edges[Edges$retained, c("from", "to", "lineage_strength")]
  K <- max(as.integer(as.character(Nodes$slice)))
  slice <- as.integer(as.character(Nodes$slice))
  n_in <- tabulate(match(kept$to, Nodes$id), nrow(Nodes))
  n_out <- tabulate(match(kept$from, Nodes$id), nrow(Nodes))
  succ_in <- vapply(seq_len(nrow(Nodes)), function(i) {
    to <- kept$to[kept$from == Nodes$id[i]]
    if (length(to) == 1) n_in[match(to, Nodes$id)] else NA_integer_
  }, integer(1))
  Nodes$n_in <- n_in
  Nodes$n_out <- n_out
  Nodes$pattern <- vapply(seq_len(nrow(Nodes)), function(i) {
    p <- c(
      if (slice[i] > 1 && n_in[i] == 0) "emergent",
      if (slice[i] < K && n_out[i] == 0) "disappearing",
      if (n_out[i] == 1 && isTRUE(succ_in[i] == 1)) "continuation",
      if (n_out[i] > 1) "split",
      if (n_in[i] > 1) "merge"
    )
    paste(p, collapse = ", ")
  }, character(1))

  pathways <- data.frame(
    pathway = character(0),
    length = integer(0),
    strength = numeric(0),
    cumulative_size = numeric(0)
  )
  if (nrow(kept) > 0) {
    g <- igraph::graph_from_data_frame(
      data.frame(from = as.character(kept$from), to = as.character(kept$to)),
      directed = TRUE,
      vertices = data.frame(name = as.character(Nodes$id))
    )
    w <- setNames(kept$lineage_strength, paste(kept$from, kept$to))
    sources <- which(n_in == 0 & n_out > 0)
    sinks <- as.character(Nodes$id[n_out == 0 & n_in > 0])
    paths <- unlist(lapply(sources, function(v) {
      igraph::all_simple_paths(g, from = as.character(Nodes$id[v]), to = sinks, mode = "out")
    }), recursive = FALSE)
    if (length(paths) > 0) {
      pathways <- do.call(rbind, lapply(paths, function(p) {
        id <- as.numeric(names(p))
        i <- match(id, Nodes$id)
        data.frame(
          pathway = paste(Nodes$label[i], collapse = " -> "),
          length = length(id),
          strength = prod(w[paste(head(id, -1), id[-1])]),
          cumulative_size = sum(Nodes$fuzzy_cardinality[i], na.rm = TRUE)
        )
      }))
      pathways <- pathways[order(-pathways$strength, -pathways$length), ]
      rownames(pathways) <- NULL
    }
  }
  list(Nodes = Nodes, pathways = pathways)
}
