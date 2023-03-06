#' Perform a Thematic Evolution Analysis
#'
#' It performs a Thematic Evolution Analysis based on co-word network analysis and clustering.
#' The methodology is inspired by the proposal of Cobo et al. (2011).
#' 
#' \code{\link{thematicEvolution}} starts from two or more thematic maps created by \code{\link{thematicMap}} function.\cr\cr
#' 
#' Reference:\cr
#' Cobo, M. J., Lopez-Herrera, A. G., Herrera-Viedma, E., & Herrera, F. (2011). An approach for detecting, quantifying, 
#' and visualizing the evolution of a research field: A practical application to the fuzzy sets theory field. Journal of Informetrics, 5(1), 146-166.\cr
#' 
#' @param M is a bibliographic data frame obtained by the converting function \code{\link{convert2df}}.
#' @param field is a character object. It indicates the content field to use. Field can be one of c=("ID","DE","TI","AB"). Default value is \code{field="ID"}.
#' @param years is a numeric vector of one or more unique cut points.
#' @param n is numerical. It indicates the number of words to use in the network analysis
#' @param minFreq is numerical. It indicates the min frequency of words included in to a cluster.
#' @param ngrams is an integer between 1 and 4. It indicates the type of n-gram to extract from texts. 
#' An n-gram is a contiguous sequence of n terms. The function can extract n-grams composed by 1, 2, 3 or 4 terms. Default value is \code{ngrams=1}.
#' @param stemming is logical. If it is TRUE the word (from titles or abstracts) will be stemmed (using the Porter's algorithm).
#' @param size is numerical. It indicates del size of the cluster circles and is a number in the range (0.01,1).
#' @param n.labels is integer. It indicates how many labels associate to each cluster. Default is \code{n.labels = 1}.
#' @param repel is logical. If it is TRUE ggplot uses geom_label_repel instead of geom_label.
#' @param remove.terms is a character vector. It contains a list of additional terms to delete from the documents before term extraction. The default is \code{remove.terms = NULL}.
#' @param synonyms is a character vector. Each element contains a list of synonyms, separated by ";",  that will be merged into a single term (the first word contained in the vector element). The default is \code{synonyms = NULL}.
#' @param cluster is a character. It indicates the type of cluster to perform among ("optimal", "louvain","leiden", "infomap","edge_betweenness","walktrap", "spinglass", "leading_eigen", "fast_greedy").
#' @return a list containing:
#' \tabular{lll}{
#' \code{nets}\tab   \tab The thematic nexus graph for each comparison\cr
#' \code{incMatrix}\tab   \tab Some useful statistics about the thematic nexus}
#' 
#'
#' @examples
#' 
#' \dontrun{
#' data(scientometrics, package = "bibliometrixData")
#' years=c(2000)
#' 
#' nexus <- thematicEvolution(scientometrics,field="ID", years=years, n=100,minFreq=2)
#' }
#' 
#' @seealso \code{\link{thematicMap}} function to create a thematic map based on co-word network analysis and clustering.
#' @seealso \code{\link{cocMatrix}} to compute a bibliographic bipartite network.
#' @seealso \code{\link{networkPlot}} to plot a bibliographic network.
#'
#' @export

thematicEvolution <- function(M, field = "ID", years, n = 250, minFreq = 2, size = 0.5, ngrams=1, stemming = FALSE, n.labels = 1, repel = TRUE, remove.terms = NULL, synonyms = NULL, cluster="walktrap") 
{
  list_df <-  timeslice(M, breaks = years)
  K <-  length(list_df)
  S <-  net <-  res <-  list()
  Y <-  NULL
  pdf(file = NULL) ## to improve adding graph=FALSE in thematicMap
  for (k in 1:K) {
    Mk <-  list_df[[k]]
    Y[k] <-  paste(min(Mk$PY), "-", max(Mk$PY), sep = "", collapse = "")
    resk <- thematicMap(Mk, field = field, n = n, minfreq = minFreq, ngrams=ngrams,
                        stemming = stemming, size = size, n.labels = n.labels, 
                        repel = repel, remove.terms = remove.terms, synonyms = synonyms, cluster=cluster)
    resk$params <- resk$params %>% dplyr::filter(.data$params!="minfreq")
    res[[k]] <-  resk
    net[[k]] <-  resk$net
  }
  dev.off()
  par(mfrow = c(1, (K - 1)))
  if (K < 2) {
    print("Error")
    return()
  }
  incMatrix <-  list()
  for (k in 2:K) {
    res1 <-  res[[(k - 1)]]
    res2 <-  res[[(k)]]
    if (res1$nclust == 0 | res2$nclust == 0) {
      cat(paste("\nNo topics in the period ", k - 1, " with this set of input parameters\n\n"))
      return(list(check = FALSE))
    }
    res1$words$Cluster <-  paste(res1$clusters$name[res1$words$Cluster], 
                               "--", Y[k - 1], sep = "")
    res1$clusters$label <-  paste(res1$clusters$name, "--", 
                                Y[k - 1], sep = "")
    res2$words$Cluster <-  paste(res2$clusters$name[res2$words$Cluster], 
                               "--", Y[k], sep = "")
    res2$clusters$label <-  paste(res2$clusters$name, "--", 
                                Y[k], sep = "")
    cluster1 <- res1$words %>% group_by(.data$Cluster_Label) %>% 
      mutate(len = length(.data$Words), tot = sum(.data$Occurrences))
    cluster2 <- res2$words %>% group_by(.data$Cluster_Label) %>% 
      mutate(len = length(.data$Words), tot = sum(.data$Occurrences))
    A <- inner_join(cluster1, cluster2, by = "Words") %>% 
      group_by(.data$Cluster_Label.x, .data$Cluster_Label.y) %>% 
      rowwise() %>% mutate(min = min(.data$Occurrences.x, 
                                     .data$Occurrences.y), Occ = sum(.data$Occurrences.x), 
                           tot = min(.data$tot.x, .data$tot.y)) %>% ungroup()
    B <- A %>% group_by(.data$Cluster_Label.x, .data$Cluster_Label.y) %>% 
      summarise(CL1 = .data$Cluster.x[1], CL2 = .data$Cluster.y[1], 
                Words = paste0(.data$Words, collapse = ";", sep = ""), 
                sum = sum(.data$min), Inc_Weighted = sum(.data$min)/min(.data$tot), 
                Inc_index = length(.data$Words)/min(.data$len.x, 
                                                    .data$len.y), Occ = .data$Occ[1], Tot = .data$tot[1], 
                Stability = length(.data$Words)/(.data$len.x[1] + 
                                                   .data$len.y[1] - length(.data$Words))) %>% 
      data.frame()
    incMatrix[[k - 1]] <-  B
  }
  INC = incMatrix[[1]]
  if (length(incMatrix) > 1) {
    for (i in 2:length(incMatrix)) {
      INC <-  rbind(INC, incMatrix[[i]])
    }
  }
  edges <-  INC[, c("CL1", "CL2", "Inc_index", "Inc_Weighted", 
                  "Stability")]
  # edges = edges[edges[, 3] > 0, ]
  nodes <-  data.frame(name = unique(c(edges$CL1, edges$CL2)))
  nodes$group <-  nodes$name
  
  cont <-  0
  edges[, 6] <-  edges[, 1]
  for (i in nodes$name) {
    ind <-  which(edges[, 1] == i)
    edges[ind, 1] <-  cont
    ind1 <-  which(edges[, 2] == i)
    edges[ind1, 2] <-  cont
    cont <-  cont + 1
  }
  names(edges) <-  c("from", "to", "Inclusion", "Inc_Weighted", 
                   "Stability", "group")
  edges$from <-  as.numeric(edges$from)
  edges$to <-  as.numeric(edges$to)
  
  ###for colors
  nodes <-nodes %>% mutate(label=.data$name) %>% 
    separate(sep = "--", col = "name", into = c("name", "group")) %>% 
    mutate(slice=factor(.data$group,labels =1:K))
  Nodes<-data.frame()
  for (i in 1:K) {
    Nodes<-rbind(Nodes,left_join(subset(nodes,nodes$slice==i), res[[i]]$clusters[c("color","name")], by="name"))
  }
  ################ 
  
  params <- list(field = field, 
                 years = years, 
                 n = n, 
                 minFreq = minFreq, 
                 size = size, 
                 ngrams=ngrams, 
                 stemming = stemming, 
                 n.labels = n.labels, 
                 repel = repel, 
                 remove.terms = remove.terms, 
                 synonyms = synonyms, 
                 cluster=cluster)
  
  params <- data.frame(params=names(unlist(params)),values=unlist(params), row.names = NULL)
  
  results <-  list(Nodes = Nodes, Edges = edges, Data = INC[, -c(1, 2)], 
                   check = TRUE, TM = res, Net = net, params=params)
  return(results)
}