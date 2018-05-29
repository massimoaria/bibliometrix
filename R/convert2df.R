#' Convert a Clarivate Analytics WoS, SCOPUS and COCHRANE Database Export files or RISmed PubMed/MedLine object into a data frame
#'
#' It converts a SCOPUS, Clarivate Analytics WoS and COCHRANE Database export files or RISmed PubMed/MedLine object into a data frame, with cases corresponding to articles and variables to Field Tags as used in WoS.
#'
#' Actually the function allows to convert both SCOPUS/WoS files in bibtext format and just WoS files in plain text format.
#'
#' @param file can be: a) a character array containing data read from a Clarivate Analytics WoS Export file (in plain text or bibtex format) or SCOPUS Export file (exclusively in bibtex format);
#' b) an object of the class \code{pubmed (package RISmed)} containing a collection obtained from a query performed with RISmed package.
#' @param dbsource is a character indicating the bibliographic database. \code{dbsource} can be \code{"isi"}, \code{"scopus"} or \code{pubmed}. Default is \code{dbsource = "isi"}.
#' @param format is a character indicating the format of the SCOPUS and Clarivate Analytics WoS export file. \code{format} can be \code{"bibtex"} or \code{"plaintext"}. Default is \code{format = "plaintext"}.
#' @return a data frame with cases corresponding to articles and variables to Field Tags in the original export file.
#'
#' data frame columns are named using the standard Clarivate Analytics WoS Field Tag codify. The main field tags are:
#'
#' \tabular{lll}{
#' \code{AU}\tab   \tab Authors\cr
#' \code{TI}\tab   \tab Document Title\cr
#' \code{SO}\tab   \tab Publication Name (or Source)\cr
#' \code{JI}\tab   \tab ISO Source Abbreviation\cr
#' \code{DT}\tab   \tab Document Type\cr
#' \code{DE}\tab   \tab Authors' Keywords\cr
#' \code{ID}\tab   \tab Keywords associated by SCOPUS or WoS database \cr
#' \code{AB}\tab   \tab Abstract\cr
#' \code{C1}\tab   \tab Author Address\cr
#' \code{RP}\tab   \tab Reprint Address\cr
#' \code{CR}\tab   \tab Cited References\cr
#' \code{TC}\tab   \tab Times Cited\cr
#' \code{PY}\tab   \tab Year\cr
#' \code{SC}\tab   \tab Subject Category\cr
#' \code{UT}\tab   \tab Unique Article Identifier\cr
#' \code{DB}\tab   \tab Database\cr}
#'
#' for a complete list of field tags see: \href{http://www.bibliometrix.org/documents/Field_Tags_bibliometrix.pdf}{Field Tags used in bibliometrix}
#' 
#' @examples
#' # An ISI or SCOPUS Export file can be read using \code{\link{readLines}} function:
#'
#' # D <- readFiles('filename1.txt','filename2.txt','filename3.txt')
#'
#' # filename1.txt, filename2.txt and filename3.txt are WoS or SCOPUS Export file 
#' # in plain text or bibtex format.
#'
#' #  biblio <- readFiles('http://www.bibliometrix.org/datasets/bibliometrics_articles.txt')
#'
#' data(biblio)
#'
#' biblio_df_df <- convert2df(file = biblio, dbsource = "isi", format = "bibtex")
#'
#' @seealso \code{\link{scopus2df}} for converting SCOPUS Export file (in bibtex format)
#' @seealso \code{\link{isibib2df}} for converting ISI Export file (in bibtex format)
#' @seealso \code{\link{isi2df}} for converting ISI Export file (in plain text format)
#' @seealso \code{\link{pubmed2df}} for converting an object of the class pubmed (RISmed package)
#' @family converting functions
#' 
#' @export
#' @import stats
#' @import ggplot2
#' @import RISmed
#' @import ggrepel
#' @importFrom stringdist stringdistmatrix
#' @importFrom rscopus author_search
#' @importFrom rscopus get_complete_author_info
#' @importFrom RColorBrewer brewer.pal
#' @importFrom FactoMineR MCA
#' @importFrom FactoMineR CA
#' @importFrom FactoMineR PCA
#' @importFrom factoextra get_mca_var
#' @importFrom factoextra get_mca_ind
#' @importFrom factoextra get_ca_row
#' @importFrom factoextra get_ca_col
#' @importFrom factoextra fviz_nbclust
#' @importFrom factoextra fviz_cluster
#' @importFrom igraph get.edgelist
#' @importFrom igraph graph.adjacency
#' @importFrom igraph degree
#' @importFrom igraph plot.igraph
#' @importFrom igraph delete.vertices
#' @importFrom igraph E
#' @importFrom igraph E<-
#' @importFrom igraph V
#' @importFrom igraph V<-
#' @importFrom igraph vcount
#' @importFrom igraph edge_density
#' @importFrom igraph transitivity
#' @importFrom igraph diameter
#' @importFrom igraph degree_distribution
#' @importFrom igraph centr_degree
#' @importFrom igraph centr_clo
#' @importFrom igraph centr_betw
#' @importFrom igraph centr_eigen
#' @importFrom igraph mean_distance
#' @importFrom igraph closeness
#' @importFrom igraph eigen_centrality
#' @importFrom igraph arpack_defaults
#' @importFrom igraph authority_score
#' @importFrom igraph page_rank
#' @importFrom igraph hub_score
#' @importFrom igraph graph_from_incidence_matrix
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom igraph simplify
#' @importFrom igraph layout.auto
#' @importFrom igraph layout.circle
#' @importFrom igraph layout.sphere
#' @importFrom igraph layout.mds
#' @importFrom igraph layout.kamada.kawai
#' @importFrom igraph layout.fruchterman.reingold
#' @importFrom igraph layout.star
#' @importFrom igraph write.graph
#' @importFrom igraph cluster_walktrap
#' @importFrom igraph cluster_optimal
#' @importFrom igraph cluster_infomap
#' @importFrom igraph cluster_edge_betweenness
#' @importFrom igraph cluster_louvain
#' @importFrom igraph count_multiple
#' @importFrom igraph membership
#' @importFrom igraph layout.norm
#' @importFrom igraph delete.edges
#' @importFrom igraph betweenness
#' @importFrom Matrix %&%
#' @importFrom Matrix abIseq
#' @importFrom Matrix abIseq1
#' @importFrom Matrix all.equal
#' @importFrom Matrix anyDuplicatedT
#' @importFrom Matrix Arith
#' @importFrom Matrix as.array
#' @importFrom Matrix as.matrix
#' @importFrom Matrix band
#' @importFrom Matrix bandSparse
#' @importFrom Matrix bdiag
#' @importFrom Matrix cbind2
#' @importFrom Matrix chol
#' @importFrom Matrix chol2inv
#' @importFrom Matrix Cholesky
#' @importFrom Matrix coerce
#' @importFrom Matrix colMeans
#' @importFrom Matrix colSums
#' @importFrom Matrix Compare
#' @importFrom Matrix condest
#' @importFrom Matrix cov2cor
#' @importFrom Matrix crossprod
#' @importFrom Matrix det
#' @importFrom Matrix determinant
#' @importFrom Matrix diag
#' @importFrom Matrix diag<-
#' @importFrom Matrix diagN2U
#' @importFrom Matrix Diagonal
#' @importFrom Matrix diagU2N
#' @importFrom Matrix diff
#' @importFrom Matrix drop
#' @importFrom Matrix drop0
#' @importFrom Matrix expand
#' @importFrom Matrix expm
#' @importFrom Matrix fac2sparse
#' @importFrom Matrix forceSymmetric
#' @importFrom Matrix format
#' @importFrom Matrix formatSparseM
#' @importFrom Matrix formatSpMatrix
#' @importFrom Matrix graph2T
#' @importFrom Matrix head
#' @importFrom Matrix image
#' @importFrom Matrix invPerm
#' @importFrom Matrix is.null.DN
#' @importFrom Matrix isDiagonal
#' @importFrom Matrix isLDL
#' @importFrom Matrix isSymmetric
#' @importFrom Matrix isTriangular
#' @importFrom Matrix kronecker
#' @importFrom Matrix Logic
#' @importFrom Matrix lu
#' @importFrom Matrix Math
#' @importFrom Matrix Math2
#' @importFrom Matrix Matrix
#' @importFrom Matrix MatrixClass
#' @importFrom Matrix mean
#' @importFrom Matrix nnzero
#' @importFrom Matrix norm
#' @importFrom Matrix onenormest
#' @importFrom Matrix Ops
#' @importFrom Matrix pack
#' @importFrom Matrix print
#' @importFrom Matrix printSpMatrix
#' @importFrom Matrix printSpMatrix2
#' @importFrom Matrix qr
#' @importFrom Matrix qr.coef
#' @importFrom Matrix qr.fitted
#' @importFrom Matrix qr.Q
#' @importFrom Matrix qr.qty
#' @importFrom Matrix qr.qy
#' @importFrom Matrix qr.R
#' @importFrom Matrix qr.resid
#' @importFrom Matrix qrR
#' @importFrom Matrix rankMatrix
#' @importFrom Matrix rbind2
#' @importFrom Matrix rcond
#' @importFrom Matrix readHB
#' @importFrom Matrix readMM
#' @importFrom Matrix rep2abI
#' @importFrom Matrix rowMeans
#' @importFrom Matrix rowSums
#' @importFrom Matrix rsparsematrix
#' @importFrom Matrix show
#' @importFrom Matrix skewpart
#' @importFrom Matrix solve
#' @importFrom Matrix sparse.model.matrix
#' @importFrom Matrix sparseMatrix
#' @importFrom Matrix sparseVector
#' @importFrom Matrix spMatrix
#' @importFrom Matrix summary
#' @importFrom Matrix Summary
#' @importFrom Matrix symmpart
#' @importFrom Matrix t
#' @importFrom Matrix T2graph
#' @importFrom Matrix tail
#' @importFrom Matrix tcrossprod
#' @importFrom Matrix tril
#' @importFrom Matrix triu
#' @importFrom Matrix uniqTsparse
#' @importFrom Matrix unname
#' @importFrom Matrix unpack
#' @importFrom Matrix update
#' @importFrom Matrix updown
#' @importFrom Matrix which
#' @importFrom Matrix writeMM
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom graphics barplot
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics plot
#' @importFrom graphics par
#' @importFrom utils data
#' @importFrom utils adist
#' @importFrom SnowballC wordStem
#' @importFrom SnowballC getStemLanguages

convert2df<-function(file,dbsource="isi",format="plaintext"){

  cat("\nConverting your",dbsource,"collection into a bibliographic dataframe\n\n")
  if (length(setdiff(dbsource,c("isi","scopus","pubmed","cochrane")))>0){
    cat("\n 'dbsource' argument is not properly specified")
    cat("\n 'dbsource' argument has to be a character string matching 'isi, 'scopus' or 'pubmed'.\n")}
  if (length(setdiff(format,c("plaintext","bibtex","pubmed","cochrane")))>0){
    cat("\n 'format' argument is not properly specified")
    cat("\n 'format' argument has to be a character string matching 'plaintext or 'bibtex'.\n")}
  if (length(setdiff(format,c("plaintext","bibtex")))>0){
    file=iconv(file, "latin1", "ASCII", sub="")}
  
  switch(dbsource,
    isi={
      switch(format,
             bibtex={M=isibib2df(file)},
             plaintext={M=isi2df(file)}
      )},
    scopus={M=scopus2df(file)
    },
    pubmed={M=pubmed2df(file)
    M$CR="none"
    },
    cochrane={M=cochrane2df(file)
    M$CR="none"
    }
)
  if ("PY" %in% names(M)){M$PY=as.numeric(M$PY)} else {M$PY=NA}
  if ("TC" %in% names(M)){M$TC=as.numeric(M$TC)} else {M$TC=NA}
  
  if (dbsource!="cochrane"){M$AU=gsub(intToUtf8(8217),intToUtf8(39),M$AU)}
  
  cat("Done!\n\n")
  
  ## AU_UN field creation
  if ("C1" %in% names(M)){
    cat("\nGenerating affiliation field tag AU_UN from C1:  ")
    
    M <- metaTagExtraction(M, Field="AU_UN")
    cat("Done!\n\n")
    } else{
    M$C1=NA
    M$AU_UN=NA}

  ### SR field creation
  suppressWarnings(M <- metaTagExtraction(M, Field="SR"))
  
  
  ### identify duplicated SRs 
    SR=M$SR
    tab=table(SR)
    tab2=table(tab)
    ind=as.numeric(names(tab2))
    ind=ind[which(ind>1)]
    if (length(ind)>0){
      for (i in ind){
        indice=names(which(tab==i))
        for (j in indice){
          indice2=which(SR==j)
          SR[indice2]=paste(SR[indice2],as.character(1:length(indice2)),sep=" ")
        }
      }
    }
 
  row.names(M) <- SR
    
  
  
  return(M)

}
