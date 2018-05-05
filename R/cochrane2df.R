#' Convert a Cochrane Database Export file into a data frame
#'
#' It converts a Cochrane Database Export file and create a data frame from it, with cases corresponding to articles and variables to Field Tag in the original file.
#'
#' @param D is a character array containing data read from a ISI Export file (in plain text format).
#' @return a data frame with cases corresponding to articles and variables to Field Tag in the original ISI file.
#' @examples
#' # A group of Cochrane Database Export files can be read using \code{\link{readFiles}} function:
#'
#' # largechar <- readFiles('filename1.txt','filename2.txt','filename3.txt')
#'
#' # filename.txt is a Cochrane Database Export file in plain text format.
#'
#' # scientometrics_text <- readFiles('http://www.bibliometrix.org/datasets/cochrane.txt')
#' # scient_df <- cochrane2df(cochrane_text)
#'
#' @seealso \code{\link{scopus2df}} for converting SCOPUS Export file (in bibtex format)
#' @family converting functions
#' @export

cochrane2df<-function(D){

  Papers=which(regexpr("Record ",D)==1)  # first row of each document
  D=gsub("DOI: ","DI: ",D)
  nP=length(Papers)  # number of docuemnts

  Tag=which(regexpr("  ",D)==-1)   # first row of each field tags 
  lt=length(Tag)   # number of field tags
  st1=seq(1,(lt-1))
  
  uniqueTag=unique(substr(D[Tag[st1]],1,3))  # list of field tags
  uniqueTag=uniqueTag[nchar(uniqueTag)==3]
  uniqueTag=uniqueTag[uniqueTag!="Rec"]

  ## Bibliographic data frame initialization
  DATA=data.frame(matrix(NA,nP,length(uniqueTag)))
  names(DATA)=uniqueTag

  for (i in 1:nP){
    if (i%%100==0 | i==nP) cat("Articles extracted  ",i,"\n")
    iStart=Papers[i]
    if (i==nP){iStop=length(D)} else {iStop=Papers[i+1]-1}
    Seq=seq(iStart,iStop)
    #pTag=iStart+which(regexpr("  ",D[Seq])==1)-1

    document=D[Seq]
    for (j in uniqueTag){

      ind=which(regexpr(j,document)==1)
      if (length(ind)>0){
        corpus=trim.leading(gsub(j,"",document[ind]))
        if (j=="KY:"){
          corpus=gsub("\\[[^\\]]*\\]", "", corpus, perl=TRUE)
          corpus=trim(unlist(strsplit(corpus,";")))
        }
        DATA[[j]][i]=paste(corpus,collapse=";")
      }else{DATA[[j]][i]=NA}
      
    }
  }

  names(DATA)=gsub(":","",names(DATA))
  DATA <- data.frame(lapply(DATA,toupper),stringsAsFactors = FALSE)
 
  names(DATA)=gsub("YR","PY",names(DATA))
  names(DATA)=gsub("ID","UT",names(DATA))
  names(DATA)=gsub("KY","ID",names(DATA))
  names(DATA)=gsub("US","LINK",names(DATA))
  names(DATA)=gsub("NO","NR",names(DATA))
  
  #if ("PY" %in% names(DATA)){DATA$PY=as.numeric(DATA$PY)}
  
  DATA$DE=DATA$ID
  DATA$JI=DATA$J9="CDSR"
  DATA$DB="COCHRANE"
  
  return(DATA)
}


