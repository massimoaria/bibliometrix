#' Convert an ISI or SCOPUS Export file into a data frame
#'
#' It converts a SCOPUS and Thomson Reuters' ISI Web of Knowledge export file and create a data frame from it, with cases corresponding to articles and variables to Field Tag in the original file.
#'
#' Actually the function allows to convert both SCOPUS/ISI files in bibtext format and just ISI files in plain text format.
#'
#' @param file is a character array containing data read from an ISI WoK Export file (in plain text or bibtex format) or SCOPUS Export file (exclusively in bibtex format).
#' @param dbsource is a character indicating the bibliographic database. \code{dbsource} can be \code{"isi"} or \code{"scopus"}. Default is \code{dbsource = "isi"}.
#' @param format is a character indicating the format of the SCOPUS and Thomson Reuters' ISI Web of Knowledge export file. \code{format} can be \code{"bibtex"} or \code{"plaintext"}. Default is \code{format = "bibtex"}.
#' @return a data frame with cases corresponding to articles and variables to Field Tag in the original export file.
#'
#' data frame columns are named using the standard ISI WoS Field Tag codify. The main field tags are:
#'
#' \tabular{lll}{
#' \code{AU}\tab   \tab Authors\cr
#' \code{TI}\tab   \tab Document Title\cr
#' \code{SO}\tab   \tab Publication Name (or Source)\cr
#' \code{JI}\tab   \tab ISO Source Abbreviation\cr
#' \code{DT}\tab   \tab Document Type\cr
#' \code{DE}\tab   \tab Authors' Keywords\cr
#' \code{ID}\tab   \tab Keywords associated by SCOPUS or ISI database \cr
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
#' for a complete list of field tags see: \href{https://images.webofknowledge.com/WOK50B6/help/WOS/h_fieldtags.html}{ISI WoS Field Tags}
#' @examples
#' # An ISI or SCOPUS Export file can be read using \code{\link{readLines}} function:
#'
#' # largechar <- readLines('filename.txt')
#'
#' # filename.txt is an ISI or SCOPUS Export file in plain text or bibtex format.
#' # The file have to be saved without Byte order mark (U+FEFF) at the beginning
#' # and EoF code at the end of file.
#' # The original file (exported by ISI or SCOPUS search web site) can be modified
#' # using an advanced text editor like Notepad++ or Emacs.
#'
#' #  biblio <- readLines('http://www.bibliometrix.org/datasets/bibliometrics_articles.txt')
#'
#' data(biblio)
#'
#' biblio_df_df <- convert2df(file = biblio, dbsource = "isi", format = "bibtex")
#'
#' @seealso \code{\link{scopus2df}} for converting SCOPUS Export file (in bibtex format)
#' @seealso \code{\link{isibib2df}} for converting ISI Export file (in bibtex format)
#' @seealso \code{\link{isi2df}} for converting ISI Export file (in plain text format)
#' @family converting functions
#' 
#' @export
#' @import stats
#' @import ggplot2
#' @importFrom stringdist stringdistmatrix
#' @importFrom rscopus author_search
#' @importFrom rscopus get_complete_author_info
#' @importFrom RColorBrewer brewer.pal
#' @importFrom FactoMineR MCA
#' @importFrom factoextra get_mca_var
#' @importFrom factoextra fviz_nbclust
#' @importFrom factoextra fviz_cluster
#' @importFrom igraph graph.adjacency
#' @importFrom igraph degree
#' @importFrom igraph plot.igraph
#' @importFrom igraph delete.vertices
#' @importFrom igraph E
#' @importFrom igraph E<-
#' @importFrom igraph V
#' @importFrom igraph V<-
#' @importFrom igraph graph_from_incidence_matrix
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom igraph simplify
#' @importFrom igraph layout.circle
#' @importFrom igraph layout.sphere
#' @importFrom igraph layout.mds
#' @importFrom igraph layout.kamada.kawai
#' @importFrom igraph layout.fruchterman.reingold
#' @importFrom igraph write.graph
#' @importFrom igraph cluster_walktrap
#' @importFrom igraph cluster_optimal
#' @importFrom igraph cluster_infomap
#' @importFrom igraph cluster_edge_betweenness
#' @importFrom igraph cluster_louvain
#' @importFrom igraph membership
#' @importFrom igraph layout.norm
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
#' @importFrom Matrix cBind
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
#' @importFrom Matrix rBind
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

convert2df<-function(file,dbsource="isi",format="bibtex"){

  if (length(setdiff(dbsource,c("isi","scopus")))>0){
    cat("\n 'dbsource' argument is not properly specified")
    cat("\n 'dbsource' argument has to be a character string matching 'isi or 'scopus'.\n")}
  if (length(setdiff(format,c("plaintext","bibtex")))>0){
    cat("\n 'format' argument is not properly specified")
    cat("\n 'format' argument has to be a character string matching 'plaintext or 'bibtex'.\n")}

  file=iconv(file, "latin1", "ASCII", sub="")
  switch(dbsource,
    isi={
      switch(format,
             bibtex={M=isibib2df(file)},
             plaintext={M=isi2df(file)}
      )},
    scopus={M=scopus2df(file)
    }
)
  M$PY=as.numeric(M$PY)
  M$TC=as.numeric(M$TC)
  
  ## Author data cleaning
  if ("AU" %in% names(M)){
    M$AU=gsub(","," ",M$AU)
    M$AU=gsub("  "," ",M$AU)
    AUlist=strsplit(M$AU,";")
    AU=lapply(AUlist,function(l){
      l=trim(l)
      name=strsplit(l," ")
      lastname=unlist(lapply(name,function(ln){ln[1]}))
      firstname=lapply(name,function(ln){
        ln=ln[-1]
        ln=gsub(" ","",ln)
        #f=paste(ln,collapse="")
      })
      AU=paste(lastname,unlist(firstname),sep=" ",collapse=";")
      return(AU)
    })
    M$AU=unlist(AU)
    
  }
  return(M)

}
