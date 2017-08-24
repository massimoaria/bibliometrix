#' Convert a SCOPUS Export file into a data frame
#'
#' It converts a SCOPUS Export file and create a data frame from it, with cases corresponding to articles and variables to Field Tag in the original file.
#'
#' @param D is a character array containing data read from a SCOPUS Export file (in bibtex format).
#' @return a data frame with cases corresponding to articles and variables to Field Tag in the original SCOPUS file.
#' @examples
#' # A SCOPUS Export file can be read using \code{\link{readFiles}} function:
#'
#' # largechar <- readFiles('filename1.bib','filename2.bib2,...)
#'
#' # filename.bib is a SCOPUS Export file in plain text format.
#'
#' #largechar <- readFiles('http://www.bibliometrix.org/datasets/scopus.bib')
#'
#' 
#' #scopus_df <- scopus2df(largechar)
#'
#' @seealso \code{\link{isi2df}} for converting ISI Export file (in plain text format)
#' @family converting functions
#' 
#' @export

scopus2df<-function(D){

  #D=iconv(D, "latin1", "ASCII", sub="")
  #AT=which(regexpr("\\@",D)==1)
  D  <-  gsub("\\@","Manuscript=",D)

  #individua la prima riga di ogni paper
  Papers  <-  which(regexpr("Manuscript=",D)==1)
  Papers  <-  c(Papers,length(D))

  #individua il numero totale di paper
  nP  <-  length(Papers)-1

  uniqueTag  <-  c("AU","TI","SO","JI","DT","DE","ID","AB","C1","RP","CR","TC","PY","UT")

  DATA  <-  data.frame(matrix(NA,nP,length(uniqueTag)))
  names(DATA)  <-  uniqueTag
  Tag  <-  c("author=","title=","journal=","abbrev_source_title=","Manuscript=","author_keywords=","keywords=","abstract=","affiliation=","correspondence_address1=","references=","note=","year=")

  for (i in 1:nP){

    iP <- Papers[i]
    iPs  <-  Papers[i+1]-2
    iPiPs <- seq(iP,iPs)
    if (i%%100==0 | i==nP) cat("Articles extracted  ",i,"\n")

    for (j in 1:length(Tag)){
      #print(Tag[j])
      POS <- which(regexpr(Tag[j],D[iPiPs])==1)+iP-1
      
      if (length(POS)==1){
        Seq <- seq(POS,iPs)
        END <- which(regexpr(".*\\}",D[Seq])==1)[1]
        POSEND <- seq(POS,(POS+END-1))
        DATA[[uniqueTag[j]]][i] <- paste0(D[POSEND],collapse="")}
    }

  }

  DT <- which(uniqueTag=="DT")
  DATA[,-DT] <- as.data.frame(apply(DATA[,-DT],2,function(d) gsub("\\{","",d)),stringsAsFactors = FALSE)
  DATA <- as.data.frame(apply(DATA,2,function(d) gsub("\\},","",d)),stringsAsFactors = FALSE)
  DATA <- as.data.frame(apply(DATA,2,function(d) gsub("\\}","",d)),stringsAsFactors = FALSE)


  for (i in 1:length(Tag)){
    DATA[[uniqueTag[i]]] <- gsub(Tag[i],"",DATA[[uniqueTag[i]]],fixed=TRUE)
  }

  DATA$DT <- gsub("Manuscript=","",unlist(lapply(strsplit(DATA$DT,","),function(l) l[1])))
  DATA$UT <- gsub(".*\\{","",DATA$DT)
  DATA$DT <- gsub("\\{.*","",DATA$DT)


  # Author post-processing
  listAU <- strsplit(DATA$AU, " and ")

  for (i in 1:length(letters)){
    listAU <- lapply(listAU,function(l) gsub(paste(".",letters[i]," ",sep=""), '', l,fixed=TRUE) )}

  for (i in 1:length(letters)){
    listAU <- lapply(listAU,function(l) gsub(paste(" ",letters[i]," ",sep=""), '', l,fixed=TRUE) )}

  listAU <- lapply(listAU,function(l){
    l <- gsub(" ", '', l,fixed = TRUE)
    l <- gsub(",", ' ', l,fixed=TRUE)
    l <- gsub(".", '', l,fixed=TRUE)
    l <- trim(l)})


  DATA$AU <- unlist(lapply(listAU, function(l) paste0(l,collapse=";")))


  # TC post-processing
  DATA$TC <- as.numeric(sub("\\D*(\\d+).*", "\\1", DATA$TC))

  # Year
  DATA$PY <- as.numeric(sub("\\D*(\\d+).*", "\\1", DATA$PY))

  #DATA$UT <- gsub(":","",DATA$UT,fixed=TRUE)
  DATA <- data.frame(lapply(DATA,toupper),stringsAsFactors = FALSE)
  #DATA <- mutate_each(DATA, funs(toupper))

  DATA$DB <- "SCOPUS"
  return(DATA)
}
