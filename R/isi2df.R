#' Convert an ISI WoK Export file into a data frame
#'
#' It converts an ISI Wok Export file and create a data frame from it, with cases corresponding to articles and variables to Field Tag in the original file.
#'
#' @param D is a character array containing data read from a ISI Export file (in plain text format).
#' @return a data frame with cases corresponding to articles and variables to Field Tag in the original ISI file.
#' @examples
#' # A group of ISI Export files can be read using \code{\link{readFiles}} function:
#'
#' # largechar <- readFiles('filename1.txt','filename2.txt','filename3.txt')
#'
#' # scientometrics_text <- readFiles('http://www.bibliometrix.org/datasets/scientometrics.txt')
#' # data(scientometrics_text)
#' # scient_df <- isi2df(scientometrics_text)
#'
#' @seealso \code{\link{scopus2df}} for converting SCOPUS Export file (in bibtex format)
#' @family converting functions
#' @export

isi2df<-function(D){

  Papers=which(regexpr("PT ",D)==1)  # first row of each document

  nP=length(Papers)  # number of docuemnts

  Tag=which(regexpr("  ",D)==-1)   # first row of each field tags 
  lt=length(Tag)   # number of field tags
  st1=seq(1,(lt-1))
  
  uniqueTag=unique(substr(D[Tag[st1]],1,2))  # list of field tags
  uniqueTag=uniqueTag[nchar(uniqueTag)==2]
  uniqueTag=uniqueTag[uniqueTag!="FN" & uniqueTag!="VR"]

  ## Bibliographic data frame initialization
  DATA=data.frame(matrix(NA,nP,length(uniqueTag)))
  names(DATA)=uniqueTag
  
  ## Fields which need to be collapsed with ";" 
  specialSep=c("AU","AF","CR","C1","RP")
  
  for (i in 1:nP){
    if (i%%100==0 | i==nP) cat("Articles extracted  ",i,"\n")
    #print(i)
    iStart=Papers[i]
    if (i==nP){iStop=length(D)} else {iStop=Papers[i+1]-1}
    Seq=seq(iStart,iStop)
    pTag=iStart+which(regexpr("  ",D[Seq])==1)-1

    for (j in uniqueTag){
      
      ## choose the correct separator for the current field
      if (j %in% specialSep){sep=";"}else{sep=" "}
      #####################################################
      
      indTag=iStart+which(regexpr(j,D[Seq])==1)-1
      if (length(indTag)>0){
        it=0
        repeat {
          if (sum(pTag>(indTag+it))==0){break}
          if (pTag[pTag>indTag+it][1]-(indTag+it)==1){it=it+1} else {break}}

        DATA[[j]][i]=paste(D[indTag:(indTag+it)],collapse=sep)
        DATA[[j]][i]=substr(DATA[[j]][i],4,nchar(DATA[[j]][i]))} else {DATA[[j]][i]=NA}
    }
  }

  
  DATA <- data.frame(lapply(DATA,toupper),stringsAsFactors = FALSE)
  
  ## remove extra spaces
  DATA <- data.frame(lapply(DATA,function(x){x=gsub("\\s+", " ", x)}),stringsAsFactors = FALSE)
  
  DATA$UT=gsub("WOS:","ISI",DATA$UT)
  
  if ("PY" %in% names(DATA)){DATA$PY=as.numeric(DATA$PY)}
  
  DATA$DB="ISI"
  
  # Authors' names cleaning (surname and initials)
  listAU=strsplit(DATA$AU, ";")
  listAU=lapply(listAU,function(l){
    l=gsub(",", ' ', l,fixed=TRUE)
    l=gsub(".", '', l,fixed=TRUE)
    l=gsub("\\s+", " ",l)
    l=trim(l)
    l=paste(l,collapse=";")
  })
  
  
  DATA$AU=unlist(listAU)
  if (names(DATA)[1]!="PT"){DATA=DATA[,-(which(names(DATA)=="X.U.FEFF.F"))]}
  return(DATA)
}


