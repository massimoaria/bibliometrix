#' Convert an Clarivate Analitycs WoS Export file into a data frame
#'
#' It converts an Clarivate Analitycs WoS Export file and create a data frame from it, with cases corresponding to articles and variables to Field Tag in the original file.
#'
#' @param D is a character array containing data read from an WoS Export file (in bibtex format).
#' @return a data frame with cases corresponding to articles and variables to Field Tag in the original SCOPUS file.
#' @examples
#' # A ISI Export file can be read using \code{\link{readLines}} function:
#'
#' # largechar <- readFiles('filename1.bib','filename2.bib2,...)
#'
#' # filename.bib is a Clarivate Analytics WoS Export file in plain text format.
#'
#' # largechar <- readFiles('http://www.bibliometrix.org/datasets/ranking.bib')
#' 
#' # ranking <- isibib2df(largechar)
#'
#' @seealso \code{\link{isi2df}} for converting ISI Export file (in plain text format)
#' @family converting functions
#' 
#' @export

isibib2df<-function(D){

  #D=iconv(D, "latin1", "ASCII", sub="")
  #AT=which(regexpr("\\@",D)==1)
  D <- gsub("\\@","Manuscript =",D)

  # first row of each document
  Papers <- which(regexpr("Manuscript =",D)==1)
  Papers <- c(Papers,length(D))

  # number of documents
  nP <- length(Papers)-1

  uniqueTag <- c("AU","TI","SO","JI","VO","NU","PP","BO","DT","DT2","DE","ID","AB","C1","CR","TC","PY","SC","UT","DI","RP")

  DATA <- data.frame(matrix(NA,nP,length(uniqueTag)))
  names(DATA) <- uniqueTag
  Tag <- c("Author =","Title =","Journal =","Journal-ISO =","Volume =","Number =","Pages =","Booktitle =","Manuscript =","Type =","Keywords =","Keywords-Plus =","Abstract =","Affiliation =","Cited-References =","Times-Cited =","Year =","Web-of-Science-Categories  =","Unique-ID =","DOI =")

  for (i in 1:nP){

    iP <- Papers[i]
    iPs <- Papers[i+1]-1
    if (i%%100==0 | i==nP) cat("Articles extracted  ",i,"\n")
    
    iPiPs <- seq(iP,iPs)

    for (j in 1:length(Tag)){
      #print(Tag[j])
      POS <- which(regexpr(Tag[j],D[iPiPs])==1)+iP-1
      if (length(POS)==1){
        Seq <- seq(POS,iPs)
        END <- which(regexpr(".*\\}",D[Seq])==1)[1]
        POSEND <- seq(POS,(POS+END-1))
      if (uniqueTag[j]!="C1"){
        DATA[[uniqueTag[j]]][i] <- paste0(D[POSEND],collapse="")}
      else {DATA[[uniqueTag[j]]][i] <- paste0(gsub(";",",",D[POSEND]),collapse=";")}
      if (uniqueTag[j]=="DI"){
        DOI <- gsub("DOI = ","",D[POS])
        DATA[[uniqueTag[j]]][i] <- gsub(",","",DOI)
      }
      }
    }

 }

  DATA <- as.data.frame(apply(DATA,2,function(d) gsub("\\{","",d)),stringsAsFactors = FALSE)
  DATA <- as.data.frame(apply(DATA,2,function(d) gsub("\\},","",d)),stringsAsFactors = FALSE)
  DATA <- as.data.frame(apply(DATA,2,function(d) gsub("\\}","",d)),stringsAsFactors = FALSE)


for (i in 1:length(Tag)){
    DATA[[uniqueTag[i]]] <- gsub(paste0(Tag[i]," ",collapse=""),"",DATA[[uniqueTag[i]]],fixed=TRUE)
  }

  DATA$DT <- gsub("Manuscript =","",unlist(lapply(strsplit(DATA$DT,","),function(l) l[1])))
  DATA$DT <- gsub("ISI:.*","",DATA$DT)

  # Authors' names cleaning (surname and initials)
  
  listAU <- strsplit(DATA$AU, " and ")
  
  AU <- lapply(listAU,function(l){
    
    lastname <- trim(gsub(",.*","",l))
    firstname <- strsplit(trim(gsub(".*,","",l))," ")
    firstname <- gsub("[^:A-Z:]","",firstname)
    #firstname <- lapply(firstname,function(ln){
     # ln <- gsub("[^:A-Z:]","",ln)
      #ln <- trim(gsub("-"," ",ln))
      #ln <- substr(ln,1,1)
      #ln <- gsub(" ","",ln)
      #ln <- paste(ln,collapse="")
      
    #})
    AU <- paste(lastname,unlist(firstname),sep=" ",collapse=";")
    return(AU)
  })
  
  DATA$AU <- unlist(AU)
  
  # TC post-processing
  DATA$TC <- as.numeric(sub("\\D*(\\d+).*", "\\1", DATA$TC))

  # Year
  DATA$PY <- as.numeric(sub("\\D*(\\d+).*", "\\1", DATA$PY))

  DATA$UT <- gsub(":","",DATA$UT,fixed=TRUE)

  DATA$RP <- unlist(lapply(strsplit(DATA$C1,"\\."),function (l) l[1]))
  
  #DATA <- mutate_each(DATA, funs(toupper))
  DATA <- data.frame(lapply(DATA,toupper),stringsAsFactors = FALSE)
  
  # keywords post-processing (missing ";" in some rows)
  DATA$ID <- gsub("   ",";",DATA$ID)
  DATA$DE <- gsub("   ",";",DATA$DE)
  #row.names(DATA)=DATA$UT
  
  DATA$DB="ISI"
  
  ### merge Sources and Proceedings
  ind <- which(is.na(DATA$SO))
  DATA$SO[ind] <- DATA$BO[ind]
  DATA <- DATA[,-(which(names(DATA)=="BO"))]
  
  DATA$NU <- as.numeric(gsub("[^0-9]", "", DATA$NU))
  
  return(DATA)
}
