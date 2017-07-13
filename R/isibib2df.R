#' Convert an ISI WoK Export file into a data frame
#'
#' It converts an ISI WoK Export file and create a data frame from it, with cases corresponding to articles and variables to Field Tag in the original file.
#'
#' @param D is a character array containing data read from an ISI Export file (in bibtex format).
#' @return a data frame with cases corresponding to articles and variables to Field Tag in the original SCOPUS file.
#' @examples
#' # A ISI Export file can be read using \code{\link{readLines}} function:
#'
#' # largechar <- readLines('filename.bib')
#'
#' # filename.bib is an ISI Export file in plain text format.
#' # The file have to be saved without Byte order mark (U+FEFF) at the
#' # beginning and EoF code at the end of file.
#' # The original file (exported by ISI search web site) can be modified
#' # using an advanced text editor like Notepad++ or Emacs.
#'
#' # largechar <- readLines('http://www.bibliometrix.org/datasets/ranking.bib')
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
  D=gsub("\\@","Manuscript =",D)

  #individua la prima riga di ogni paper
  Papers=which(regexpr("Manuscript =",D)==1)
  Papers=c(Papers,length(D))

  #individua il numero totale di paper
  nP=length(Papers)-1

  uniqueTag=c("AU","TI","SO","JI","VO","NU","PP","BO","DT","DE","ID","AB","C1","CR","TC","PY","SC","UT","RP")

  DATA=data.frame(matrix(NA,nP,length(uniqueTag)))
  names(DATA)=uniqueTag
  Tag=c("Author =","Title =","Journal =","Journal-ISO =","Volume =","Number =","Pages =","Booktitle =","Manuscript =","Keywords =","Keywords-Plus =","Abstract =","Affiliation =","Cited-References =","Times-Cited =","Year =","Web-of-Science-Categories  =","Unique-ID =")

  for (i in 1:nP){

    iP=Papers[i]
    iPs=Papers[i+1]-1
    if (i%%100==0 | i==nP) cat("Articles extracted  ",i,"\n")

    for (j in 1:length(Tag)){
      #print(Tag[j])
      POS=which(regexpr(Tag[j],D[iP:iPs])==1)+iP-1
      if (length(POS)==1){
      END=which(regexpr(".*\\}",D[POS:iPs])==1)[1]
      if (uniqueTag[j]!="C1"){
        DATA[[uniqueTag[j]]][i]=paste0(D[(POS):(POS+END-1)],collapse="")}
      else {DATA[[uniqueTag[j]]][i]=paste0(gsub(";",",",D[(POS):(POS+END-1)]),collapse=";")}
      }
    }

 }

  DATA=as.data.frame(apply(DATA,2,function(d) gsub("\\{","",d)),stringsAsFactors = FALSE)
  DATA=as.data.frame(apply(DATA,2,function(d) gsub("\\},","",d)),stringsAsFactors = FALSE)
  DATA=as.data.frame(apply(DATA,2,function(d) gsub("\\}","",d)),stringsAsFactors = FALSE)


for (i in 1:length(Tag)){
    DATA[[uniqueTag[i]]]=gsub(paste0(Tag[i]," ",collapse=""),"",DATA[[uniqueTag[i]]],fixed=TRUE)
  }

  DATA$DT=gsub("Manuscript =","",unlist(lapply(strsplit(DATA$DT,","),function(l) l[1])))
  DATA$DT=gsub("ISI:.*","",DATA$DT)

  listAU=strsplit(DATA$AU, " and ")

  listAU=lapply(listAU,function(l) gsub(" ", '', l,fixed=TRUE) )
  listAU=lapply(listAU,function(l) gsub(",", ' ', l,fixed=TRUE) )
  listAU=lapply(listAU,function(l) gsub(".", '', l,fixed=TRUE) )

  DATA$AU=unlist(lapply(listAU, function(l) paste0(l,collapse=" ;")))

  # TC post-processing
  DATA$TC=as.numeric(sub("\\D*(\\d+).*", "\\1", DATA$TC))

  # Year
  DATA$PY=as.numeric(sub("\\D*(\\d+).*", "\\1", DATA$PY))

  DATA$UT=gsub(":","",DATA$UT,fixed=TRUE)

  DATA$RP=unlist(lapply(strsplit(DATA$C1,"\\."),function (l) l[1]))
  
  #DATA <- mutate_each(DATA, funs(toupper))
  DATA <- data.frame(lapply(DATA,toupper),stringsAsFactors = FALSE)
  
  # keywords post-processing (missing ";" in some rows)
  DATA$ID=gsub("   ",";",DATA$ID)
  DATA$DE=gsub("   ",";",DATA$DE)
  #row.names(DATA)=DATA$UT
  
  DATA$DB="ISI"
  
  ### merge Sources and Proceedings
  ind=which(is.na(DATA$SO))
  DATA$SO[ind]=DATA$BO[ind]
  DATA=DATA[,-(which(names(DATA)=="BO"))]
  
  DATA$NU=as.numeric(gsub("[^0-9]", "", DATA$NU))
  
  return(DATA)
}
