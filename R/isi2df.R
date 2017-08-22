#' Convert an ISI WoK Export file into a data frame
#'
#' It converts an ISI Wok Export file and create a data frame from it, with cases corresponding to articles and variables to Field Tag in the original file.
#'
#' @param D is a character array containing data read from a ISI Export file (in plain text format).
#' @return a data frame with cases corresponding to articles and variables to Field Tag in the original ISI file.
#' @examples
#' # An ISI Export file can be read using \code{\link{readLines}} function:
#'
#' # largechar <- readLines('filename.txt')
#'
#' # filename.txt is an ISI Export file in plain text format.
#' # The file have to be saved without Byte order mark (U+FEFF) at the beginning
#' # and EoF code at the end of file.
#' # The original file (exported by ISI search web site) can be modified
#' # using an advanced text editor like Notepad++ or Emacs.
#'
#' # scientometrics_text <- readLines('http://www.bibliometrix.org/datasets/scientometrics.txt')
#' # data(scientometrics_text)
#' # scient_df <- isi2df(scientometrics_text)
#'
#' @seealso \code{\link{scopus2df}} for converting SCOPUS Export file (in bibtex format)
#' @family converting functions
#' @export

isi2df<-function(D){

  #D=iconv(D, "latin1", "ASCII", sub="")
  
  Papers=which(regexpr("PT ",D)==1)

  nP=length(Papers)

  Tag=which(regexpr("  ",D)==-1)

  nTag=(Tag[1:length(Tag)-1]-Tag[2:length(Tag)])*-1

  df=data.frame(Tag=substr(D[Tag[1:length(Tag)-1]],1,2),Frow=Tag[1:length(Tag)-1],Rows=nTag)

  uniqueTag=unique(substr(D[Tag[1:length(Tag)-1]],1,2))
  uniqueTag=uniqueTag[nchar(uniqueTag)==2]
  uniqueTag=uniqueTag[uniqueTag!="FN" & uniqueTag!="VR"]

  DATA=data.frame(matrix(NA,nP,length(uniqueTag)))
  names(DATA)=uniqueTag

  for (i in 1:nP){
    if (i%%100==0 | i==nP) cat("Articles extracted  ",i,"\n")
    iStart=Papers[i]
    if (i==nP){iStop=length(D)} else {iStop=Papers[i+1]-1}
    pTag=iStart+which(regexpr("  ",D[iStart:iStop])==1)-1

    for (j in uniqueTag){

      indTag=iStart+which(regexpr(j,D[iStart:iStop])==1)-1
      if (length(indTag)>0){
        it=0
        repeat {
          if (sum(pTag>(indTag+it))==0){break}
          if (pTag[pTag>indTag+it][1]-(indTag+it)==1){it=it+1} else {break}}

        DATA[[j]][i]=paste(D[indTag:(indTag+it)],collapse=";")
        DATA[[j]][i]=substr(DATA[[j]][i],4,nchar(DATA[[j]][i]))} else {DATA[[j]][i]=NA}
    }
  }

  if ("AB" %in% uniqueTag){DATA$AB=str_replace_all(DATA$AB,";  ","")}
  if ("TI" %in% uniqueTag){DATA$TI=str_replace_all(DATA$TI,";  ","")}
  #DATA <- mutate_each(DATA, funs(toupper))
  DATA <- data.frame(lapply(DATA,toupper),stringsAsFactors = FALSE)
  DATA$UT=gsub("WOS:","ISI",DATA$UT)
  #row.names(DATA)=DATA$UT
  if ("PY" %in% names(DATA)){
    DATA$PY=as.numeric(DATA$PY)}
  DATA$DB="ISI"
  listAU=strsplit(DATA$AU, ";")
  listAU=lapply(listAU,function(l){
    l=gsub(",", ' ', l,fixed=TRUE)
    l=gsub(".", '', l,fixed=TRUE)
    l=gsub("\\s+", " ",l)
    l=trim(l)
    l=paste(l,collapse=";")
  })
  
  # AU=lapply(listAU,function(l){
  #   l=trim(l)
  #   name=strsplit(l," ")
  #   lastname=unlist(lapply(name,function(ln){ln[1]}))
  #   firstname=lapply(name,function(ln){
  #     ln=ln[-1]
  #     ln=substr(ln,1,1)
  #     ln=gsub(" ","",ln)
  #     ln=paste(ln,collapse="")
  #     
  #   })
  #   AU=paste(lastname,unlist(firstname),sep=" ",collapse=";")
  #   return(AU)
  # })
  DATA$AU=unlist(listAU)
  return(DATA)
}


