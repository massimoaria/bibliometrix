#' Historical co-citation network
#'
#' \code{histNetwork} creates a historical citation network from a bibliographic data frame.
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
#' histData \tab      \tab the set of n most cited references
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

histNetwork<-function(M, n=10, sep = ";"){

  N=dim(M)[1]
  
  listAU=strsplit(as.character(M$AU),sep)
  listAU=lapply(listAU, function(l) trim.leading(l))
    listAU=lapply(listAU,function(l){
    l=trim(l)
    l=sub(" ",",",l, fixed = TRUE)
    l=sub(",,",",",l, fixed = TRUE)
    l=gsub(" ","",l, fixed = TRUE)})
  FirstAuthors=gsub(","," ",unlist(lapply(listAU,function(l) l[[1]])))
  
  if (!is.null(M$J9)){
    SR=paste(FirstAuthors,M$PY,M$J9,sep=", ")}else{J9=trim(gsub("\\."," ",M$JI))
    SR=paste(FirstAuthors,M$PY,J9,sep=", ")}
    M$SR=SR

  lCit=Matrix(0, N,N)
  for (i in 1:N){
    
    x=M$SR[i]
    pos = grep(x, M$CR)
    
    if ("DI" %in% names(M)){
      if (!is.na(M$DI[i])){
      pos2 = grep(M$DI[i],M$CR)
      if (length(pos2)>0){pos=pos2}}
      }
    
      if (length(pos)>0){
      lCit[i,pos]=1
    }
  }
  
  LCS=rowSums(lCit)
  M$LCS=LCS
  row.names(lCit)=colnames(lCit)=SR
  
  s=sort(LCS,decreasing = TRUE)[n]
  ind=which(LCS>=s)
  lCit=lCit[ind,ind]
  Y=M$PY[ind]

### Cited papers list
df=data.frame(Paper=SR[ind],DOI=M$DI[ind],Year=Y,LCS=LCS[ind],GCS=M$TC[ind])
df=df[order(df$Year),]  


row.names(df)=paste(df$Year,rep("-",dim(df)[1]),1:dim(df)[1])

results=list(NetMatrix=t(lCit),Degree=s,histData=df,M=M,LCS=LCS[ind])
return(results)
}

