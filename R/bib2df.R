#' Convert a bibtex file into a data frame
#'
#' It is an internal function used by \code{\link{convert2df}} to convert a bibtex file and create a data frame from it, with cases corresponding to articles and variables to Field Tag in the original file.
#'
#' @param D is a character array containing bibliographic data in bibtex format.
#' @param dbsource is a character indicating the bibliographic database. \code{dbsource} can be \code{"isi"}, \code{"scopus"} or \code{generic}. Default is \code{dbsource = "isi"}.
#' @return a data frame with cases corresponding to articles and variables to Field Tag in the original SCOPUS file.
#' @examples
#' # A ISI Export file can be read using \code{\link{readLines}} function:
#'
#' # largechar <- readFiles('filename1.bib','filename2.bib2,...)
#'
#' # filename.bib is a bibtex file in plain text format.
#'
#' # largechar <- readFiles('http://www.bibliometrix.org/datasets/ranking.bib')
#' 
#' # ranking <- bib2df(largechar)
#'
#' @seealso \code{\link{isi2df}} for converting ISI Export file (in plain text format)
#' @family converting functions
#' 
#' @export

bib2df<-function(D, dbsource="isi"){
  
  bibtag <- NULL
  data("bibtag",envir=environment())
  bibtag=as.data.frame(bibtag)
  
  txt <- preprocessing(D)
  
  D=txt$D
  DD=txt$DD
  Tag=txt$Tag
  
  switch(dbsource,
         isi={
           ind=which(bibtag[,"ISI"] %in% Tag)
           bibtag2=bibtag[ind,c("TAG","ISI")]},
         scopus={
           ind=which(bibtag[,"SCOPUS"] %in% Tag)
           bibtag2=bibtag[ind,c("TAG","SCOPUS")]
         },
         generic={
           ind=which(bibtag[,"GENERIC"] %in% Tag)
           bibtag2=bibtag[ind,c("TAG","GENERIC")]
         })
  uniqueTag=bibtag2$TAG
  Tag=gsub("\\{","",bibtag2[,2])
  
  # first row of each document
  Papers <- which(regexpr("manuscript=",D)==1)
  Papers <- c(Papers,length(D))
  
  # number of documents
  nP <- length(Papers)-1
  
  DATA <- data.frame(matrix(NA,nP,length(uniqueTag)))
  names(DATA) <- uniqueTag

  for (i in 1:nP){
    
    if (!is.null(shiny::getDefaultReactiveDomain())){shiny::incProgress(1/nP)}
    #print(i)
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
        
        ### Affiliations
        if (uniqueTag[j]=="C1" & dbsource!="isi"){
          DATA[[uniqueTag[j]]][i] <- paste0(DD[POSEND],collapse=";")
        } else if (uniqueTag[j]=="C1" & dbsource=="isi"){
          DATA[[uniqueTag[j]]][i] <- paste0(gsub(";",",",DD[POSEND]),collapse=";")
        }
        
        ### References
        if (uniqueTag[j]=="CR" & length(POSEND)>1){
          DATA[[uniqueTag[j]]][i] <- paste0(gsub(";",",",DD[POSEND]),collapse=";")
        } else if (uniqueTag[j]=="CR" & length(POSEND)==1){
          DATA[[uniqueTag[j]]][i] <- paste0(DD[POSEND],collapse=";")
        }
        
        ### Funding and Acknowledgements
        if (uniqueTag[j] %in% c("FU","FX")){
          DATA[[uniqueTag[j]]][i] <- paste0(DD[POSEND], collapse=" ")} 
        
        ### DOI
        if (uniqueTag[j]=="DI"){
          DOI <- gsub("doi = ","",DD[POS])
          DATA[[uniqueTag[j]]][i] <- gsub(",","",DOI)
        }
        
        ### all other categories
        if (!(uniqueTag[j] %in% c("C1","CR","DI","FU","FX"))){
          DATA[[uniqueTag[j]]][i] <- paste0(gsub(";",",",DD[POSEND]),collapse=" ")} 
      }
    }
  }
    
  
    
  
  if ("DT2" %in% names(DATA)){
  DATA$DT2=substr(DATA$DT2,1,regexpr("\\{",DATA$DT2)-1)}
  
  # remove tags from fields
  for (i in 1:length(Tag)){
    
    pattern=substr(DATA[[uniqueTag[i]]][!is.na(DATA[[uniqueTag[i]]])][1],1,regexpr("=",DATA[[uniqueTag[i]]][!is.na(DATA[[uniqueTag[i]]])][1]))
    DATA[[uniqueTag[i]]][!is.na(DATA[[uniqueTag[i]]])] <- gsub(pattern,"",DATA[[uniqueTag[i]]][!is.na(DATA[[uniqueTag[i]]])],fixed=TRUE)
  }
  
  ## removing { and }
  DATA <- as.data.frame(apply(DATA,2,function(d) gsub("\\{","",d)),stringsAsFactors = FALSE)
  DATA <- as.data.frame(apply(DATA,2,function(d) gsub("\\},","",d)),stringsAsFactors = FALSE)
  DATA <- as.data.frame(apply(DATA,2,function(d) gsub("\\}","",d)),stringsAsFactors = FALSE)
  
  DATA <- postprocessing(DATA, dbsource)
  
  return(DATA)
}

### TEXT file preporcessing
preprocessing <- function(D){
  
  ## normalize bibtex data
  ind=which(regexpr("\\@",D)==1)
  D=trim(D)
  D=gsub("\\s+", " ",D)
  D=gsub("\\{\\[\\}","\\[",D)
  #D=gsub("\\}\\]\\}","\\]",D)
  D=gsub("\\{\\{","\\{",D)
  D=gsub("\\}\\}","\\}",D)
  D=gsub("\\{''\\}","",D)
  
  D=gsub(" = ","=",D)
  D1=D
  D1[ind]=gsub("\\@","manuscript={",D[ind])
  D[ind] <- gsub("\\@","manuscript=",D[ind])
  D1=D1[which(regexpr("=\\{",D1)>-1)]
  Tag<-unique(gsub("(=\\{).*","\\1",D1))
  D=gsub("@","",D)
  
  txt=list(D=tolower(D),DD=D,Tag=tolower(Tag))
  return(txt)
}

### DATA FRAME postprocessing
postprocessing <-function(DATA,dbsource){
  
           # Authors' names cleaning (surname and initials)
           #remove ; and 2 or more spaces
           DATA$AU=gsub("\\s+", " ", DATA$AU)
           
           listAU <- strsplit(DATA$AU, " and ")
           
           AU <- lapply(listAU,function(l){
             
             lastname <- trim(gsub(",.*","",l))
             firstname <- strsplit(trim(gsub(".*,","",l))," ")
             firstname <- gsub("[^:A-Z:]","",firstname)
             AU <- paste(lastname,unlist(firstname),sep=" ",collapse=";")
             return(AU)
           })
  
           
           DATA$AU <- unlist(AU)
           
           # TC post-processing
           if ("TC" %in% names(DATA)){
             DATA$TC <- as.numeric(sub("\\D*(\\d+).*", "\\1", DATA$TC))
           }
           # Year
           if ("PY" %in% names(DATA)){
             DATA$PY <- as.numeric(sub("\\D*(\\d+).*", "\\1", DATA$PY))
           }
           
           if ("UT" %in% names(DATA)){
             DATA$UT <- gsub(":","",DATA$UT,fixed=TRUE)
           }
           
           if (!("RP" %in% names(DATA)) & ("C1" %in% names(DATA))){
             DATA$RP <- unlist(lapply(strsplit(DATA$C1,"\\."),function (l) l[1]))
           }
           
           # keywords post-processing (missing ";" in some rows)
           if ("ID" %in% names(DATA)){
             DATA$ID <- gsub("   ",";",DATA$ID)
             DATA$ID <- gsub(",",";",DATA$ID)
           }
           
           if ("DE" %in% names(DATA)){
             DATA$DE <- gsub("   ",";",DATA$DE)
             DATA$DE <- gsub(",",";",DATA$DE)
           }
           #row.names(DATA)=DATA$UT
           
           ### merge Sources and Proceedings
           if (("SO" %in% names(DATA)) & ("BO" %in% names(DATA))){
             ind <- which(is.na(DATA$SO))
             DATA$SO[ind] <- DATA$BO[ind]
           }
           
           if ("PN" %in% names(DATA)){
             DATA$PN <- as.numeric(gsub("[^0-9]", "", DATA$PN))
           }
           
           if (dbsource!="generic"){
             DATA$DB=dbsource
           } else {DATA$DB="SCOPUS"}
        
  
  DATA <- data.frame(lapply(DATA,toupper),stringsAsFactors = FALSE)
  return(DATA)
}
