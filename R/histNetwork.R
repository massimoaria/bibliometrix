#' Historical co-citation network
#'
#' \code{histNetwork} creates a historical citation network from a bibliographic data frame.
#'
#' @param M is a bibliographic data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to manuscripts and variables to Field Tag in the original SCOPUS and Thomson Reuters' ISI Web of Knowledge file.
#' @param sep is the field separator character. This character separates strings in CR column of the data frame. The default is \code{sep = ";"}.
#' @return \code{histNetwork} returns an object of \code{class} "list" containing the following components:
#'
#' \tabular{lll}{
#' NetMatrix \tab  \tab the historical co-citation network matrix\cr
#' Degree \tab       \tab the min degree of the network\cr
#' histData \tab      \tab the set of n most cited references\cr
#' M \tab      \tab the bibliographic data frame}
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

histNetwork<-function(M, sep = ";"){
  
  if (M$DB[1]!="ISI"){cat("\nSorry, but for the moment histNetwork works only with WoS collections\n\n")
    return()}
  
  M=M[order(M$PY),]
  N=dim(M)[1]
  rows=c(1:N)
  
  if (!("SR" %in% names(M))){M=metaTagExtraction(M,Field="SR")} 
  
  lCit=Matrix(0, N,N)
  for (i in 1:N){
    if (i%%100==0 | i==N) cat("Articles analysed  ",i,"\n")
    x=M$SR[i]
    Year=M$PY[i]
    pos = grep(x, M$CR[M$PY>=Year])
    pos = rows[M$PY>=Year][pos]
    if ("DI" %in% names(M)){
      if (!is.na(M$DI[i])){
      pos2 = grep(M$DI[i],M$CR[M$PY>=Year],fixed=TRUE)
      pos2 = rows[M$PY>=Year][pos2]
      pos=unique(pos,pos2)}
      }
    
      if (length(pos)>0){
      lCit[i,pos]=1
    }
  }
  
  LCS=rowSums(lCit)
  M$LCS=LCS
  row.names(lCit)=colnames(lCit)=M$SR
  
  ####### old to remove
  #s=sort(LCS,decreasing = TRUE)[n]
  #ind=which(LCS>=s)
  #lCit=lCit[ind,ind]
  #Y=M$PY[ind]

### Cited papers list
#if (!("DI" %in% names(M))){M$DI=NA}
#df=data.frame(Paper=M$SR[ind],DOI=M$DI[ind],Year=Y,LCS=LCS[ind],GCS=M$TC[ind],stringsAsFactors = F)
#df=df[order(df$Year),]  

  if (!("DI" %in% names(M))){M$DI=NA}
  df=data.frame(Paper=M$SR,DOI=M$DI,Year=M$PY,LCS=LCS,GCS=M$TC,stringsAsFactors = F)
  df=df[order(df$Year),]  
  

row.names(df)=paste(df$Year,rep("-",dim(df)[1]),1:dim(df)[1])

#results=list(NetMatrix=t(lCit),Degree=s,histData=df,M=M,LCS=LCS[ind])

results=list(NetMatrix=t(lCit),histData=df,M=M,LCS=LCS)

return(results)
}
