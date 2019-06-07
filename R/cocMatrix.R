#' Co-occurrence matrix
#'
#' \code{cocMatrix} computes co-occurences between elements of a Tag Field from a bibliographic data frame. Manuscript is the unit of analysis.
#'
#' This co-occurrence matrix can be tranformed into a collection of compatible
#' networks. Through matrix multiplication you can obtain different networks. 
#' The fuction follows the approach proposed by Batagely and Cerinsek (2013).\cr\cr
#' @param M is a data frame obtained by the converting function
#'   \code{\link{convert2df}}. It is a data matrix with cases corresponding to
#'   articles and variables to Field Tag in the original ISI or SCOPUS file.
#' @param Field is a character object. It indicates one of the field tags of the
#'   standard ISI WoS Field Tag codify. Field can be equal to one of this tags:
#'   \tabular{lll}{ \code{AU}\tab   \tab Authors\cr \code{SO}\tab   \tab
#'   Publication Name (or Source)\cr \code{JI}\tab   \tab ISO Source
#'   Abbreviation\cr \code{DE}\tab   \tab Author Keywords\cr \code{ID}\tab
#'   \tab Keywords associated by ISI or SCOPUS database \cr \code{CR}\tab   \tab
#'   Cited References}
#'
#'   for a complete list of filed tags see:
#'   \href{http://www.bibliometrix.org/documents/Field_Tags_bibliometrix.pdf}{Field Tags used in bibliometrix}
#'
#' @param type indicates the output format of co-occurrences: \tabular{lll}{
#'   \code{type = "matrix"} \tab   \tab produces an object of class
#'   \code{matrix}\cr \code{type = "sparse"} \tab   \tab produces an object of
#'   class \code{dgMatrix} of the package \code{\link{Matrix}}. "sparse"
#'   argument generates a compact representation of the matrix.}
#' @param sep is the field separator character. This character separates strings in each 
#' column of the data frame. The default is \code{sep = ";"}.
#' @param binary is a logical. If TRUE each cell contains a 0/1. if FALSE each cell contains the frequency. 
#' @return a co-occurrence matrix with cases corresponding to manuscripts and variables to the
#'   objects extracted from the Tag \code{Field}.
#'
#' @examples
#' # EXAMPLE 1: Articles x Authors co-occurrence matrix
#'
#' data(scientometrics)
#' WA <- cocMatrix(scientometrics, Field = "AU", type = "sparse", sep = ";")
#'
#' # EXAMPLE 2: Articles x Cited References co-occurrence matrix
#'
#' # data(scientometrics)
#'
#' # WCR <- cocMatrix(scientometrics, Field = "CR", type = "sparse", sep = ";")
#'
#' # EXAMPLE 3: Articles x Cited First Authors co-occurrence matrix
#'
#' # data(scientometrics)
#' # scientometrics <- metaTagExtraction(scientometrics, Field = "CR_AU", sep = ";")
#' # WCR <- cocMatrix(scientometrics, Field = "CR_AU", type = "sparse", sep = ";")
#' 
#' @seealso \code{\link{convert2df}} to import and convert an ISI or SCOPUS
#'   Export file in a data frame.
#' @seealso \code{\link{biblioAnalysis}} to perform a bibliometric analysis.
#' @seealso \code{\link{biblioNetwork}} to compute a bibliographic network.
#' @export

cocMatrix<-function(M, Field = "AU", type = "sparse", sep = ";",binary=TRUE){
#
# The function creates co-occurences data between Works and Field
#
# type indicates the output format of co-occurrences:
#   "matrix" argument generates a W x Field sparse matrix
#   "sparse" argument generates a compact representation of the matrix (using the package Matrix)
#    it represents a compact representation of a co-occurrences matrix.
# Field indicates the ISI Tag
# if Field is AU -> WA (Works x Authors)
# if Field is CR -> WR (Works x References)
# if Field is DE -> WK (Works x Keywords)
# etc.
#crossprod <- Matrix::crossprod
size<-dim(M)

if (Field=="CR"){M$CR<-str_replace_all(as.character(M$CR),"DOI;","DOI ")}

if (Field %in% names(M)){
  Fi<-strsplit(M[,Field],sep)} else{return(print(paste("Field",Field,"is not a column name of input data frame")))}
  Fi<-lapply(Fi,trim.leading)
if (Field=="CR"){Fi<-lapply(Fi,function(l) l<-l[nchar(l)>10])}  ## delete not congruent references

  ## Scelta dell'informazione contenuta in CR da utilizzare (Reference, Autore, Affiliation, ecc.)



# vector of unique units
uniqueField<-unique(unlist(Fi))
uniqueField<-uniqueField[!is.na(uniqueField)]

if (Field=="CR"){
  S<-gsub("\\).*",")",uniqueField)
  S<-gsub(","," ",S)
  S<-gsub(";"," ",S)
  S<-reduceRefs(S)
  uniqueField<-unique(trimES(S))
  Fi<-lapply(Fi, function(l){
    l<-gsub("\\).*",")",l)
    l<-gsub(","," ",l)
    l<-gsub(";"," ",l)
    l<-l[nchar(l)>0]
    l<-reduceRefs(l)
    l<-trimES(l)
    return(l)
  })
} else {
  # normalize reference names
  S<-gsub("\\,",";",uniqueField)
  S<-sub("\\;",",",S)
  S<-sub("\\;",",",S)
  S<-gsub("\\;.*","",S)
  uniqueField<-unique(S)
  Fi<-lapply(Fi, function(l){
    l<-gsub("\\,",";",l)
    l<-sub("\\;",",",l)
    l<-sub("\\;",",",l)
    l<-gsub("\\;.*","",l)
    l<-l[nchar(l)>0]
    return(l)
  })
  }

if (type=="matrix" | !isTRUE(binary)){
  # Initialization of WA matrix
  WF<-matrix(0,size[1],length(uniqueField))} else if (type=="sparse"){
    WF<-Matrix(0,size[1],length(uniqueField))} else {print("error in type argument");return()}
colnames(WF)<-uniqueField
rownames(WF)<-rownames(M)
  # Population of WA matrix
  for (i in 1:size[1]){
    if (length(Fi[[i]])>0 & !is.na(Fi[[i]][1])) {
      #print(i)
      #if (Field=="CR"){Fi[[i]]=reduceRefs(Fi[[i]])}
      if (isTRUE(binary)){
        ## binary counting
      WF[i,uniqueField %in% Fi[[i]]]<-1}else{
        ## full counting
        tab=table(Fi[[i]])
        name=names(tab)
        WF[i,name[nchar(name)>0]]=tab[nchar(name)>0]
        }
      }
	}

if (type=="sparse" & !isTRUE(binary)){
  WF=Matrix(WF)
}

  WF=WF[,!is.na(uniqueField)]
  #WF=attrPY(M,WF)  # Median Year of each attribute
  
return(WF)
}

reduceRefs<- function(A){
  
  ind=unlist(regexec("*V[0-9]", A))
  A[ind>-1]=substr(A[ind>-1],1,(ind[ind>-1]-1))
  ind=unlist(regexec("*DOI ", A))
  A[ind>-1]=substr(A[ind>-1],1,(ind[ind>-1]-1))
  return(A)
}

