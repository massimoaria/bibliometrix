#' Historical co-citation network
#'
#' \code{histNetwork} creates a historical co-citation network from a bibliographic data frame.
#'
#' @param M is a bibliographic data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to manuscripts and variables to Field Tag in the original SCOPUS and Thomson Reuters' ISI Web of Knowledge file.
#' @param n is an integer, indicating the number of most cited references to select. Default value is 10.\cr
#' @param sep is the field separator character. This character separates strings in CR column of the data frame. The default is \code{sep = ";"}.
#' @return \code{histNetwork} returns an object of \code{class} "list" containing the following components:
#'
#' \tabular{lll}{
#' NetMatrix \tab  \tab the historical co-citation network matrix\cr
#' Degree \tab       \tab the min degree of the network\cr
#' histData \tab      \tab the set of n most cited references}
#'
#'
#' @examples
#' data(scientometrics)
#'
#' histResults <- histNetwork(scientometrics, n = 10, sep = ";")
#'
#'
#' @seealso \code{\link{convert2df}} to import and convert an ISI or SCOPUS Export file in a bibliographic data frame.
#' @seealso \code{\link{summary}} to obtain a summary of the results.
#' @seealso \code{\link{plot}} to draw some useful plots of the results.
#' @seealso \code{\link{biblioNetwork}} to compute a bibliographic network.
#' 
#' @export

histNetwork<-function(M, n=10, sep = ";"){


### Calculate Co-citation matrix
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = sep)
NetMatrix=NetMatrix[(!is.na(colnames(NetMatrix))),(!is.na(colnames(NetMatrix)))]
TC=Matrix::diag(NetMatrix)

Degree <- sort(TC,decreasing=TRUE)[n]

### Cited papers list
Papers=colnames(NetMatrix)

Papers=Papers[TC>=Degree]


### Extract year from cited papers
Year=sapply(Papers, function(P){
  Y=gsub("[^0-9]", "", unlist(strsplit(P,",")))
  Y=as.numeric(Y[nchar(Y)==4])[1]
  })
Year=as.numeric(Year)
Year[Year<=1100]=NA

X=as.matrix(NetMatrix[TC>=Degree,TC>=Degree])
rownames(X)=colnames(X)=Year

X=X[sort(rownames(X)),sort(rownames(X))]

df=data.frame(Paper=Papers,Year=Year)
df=df[order(df$Year),]  
TC=diag(X)
df$LCS=TC

for (i in 1:length(df$Paper)){
  a=regexpr(df$Year[i],df$Paper[i])[1]
  df$PaperShort[i]=substr(df$Paper[i], 1, a+3)
}
X[upper.tri(X)]=0
row.names(X)=colnames(X)=df$PaperShort
row.names(df)=paste(df$Year,rep("-",dim(df)[1]),1:dim(df)[1])

results=list(NetMatrix=X,Degree=Degree,histData=df[,-4])
return(results)
}