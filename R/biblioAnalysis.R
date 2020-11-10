#' Bibliometric Analysis
#'
#' It performs a bibliometric analysis of a dataset imported from SCOPUS and Clarivate Analytics Web of Science databases.
#'
#' @param M is a bibliographic data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to manuscripts and variables to Field Tag in the original SCOPUS and Clarivate Analytics Web of Science file.
#' @param sep is the field separator character. This character separates strings in each column of the data frame. The default is \code{sep = ";"}.
#' @return \code{biblioAnalysis} returns an object of \code{class} "bibliometrix".
#'
#' The functions \code{\link{summary}} and \code{\link{plot}} are used to obtain or print a summary and some useful plots of the results.
#'
#' An object of \code{class} "bibliometrix" is a list containing the following components:
#'
#' \tabular{lll}{
#' Articles \tab  \tab the total number of manuscripts\cr
#' Authors \tab       \tab the authors' frequency distribution\cr
#' AuthorsFrac \tab      \tab the authors' frequency distribution (fractionalized)\cr
#' FirstAuthors \tab      \tab corresponding author of each manuscript\cr
#' nAUperPaper \tab      \tab the number of authors per manuscript\cr
#' Appearances \tab      \tab the number of author appearances\cr
#' nAuthors \tab       \tab the number of authors\cr
#' AuMultiAuthoredArt \tab      \tab the number of authors of multi-authored articles\cr
#' MostCitedPapers \tab      \tab the list of manuscripts sorted by citations\cr
#' Years \tab      \tab publication year of each manuscript\cr
#' FirstAffiliation \tab      \tab the affiliation of the first author\cr
#' Affiliations \tab      \tab the frequency distribution of affiliations (of all co-authors for each paper)\cr
#' Aff_frac \tab      \tab the fractionalized frequency distribution of affiliations (of all co-authors for each paper)\cr
#' CO \tab      \tab the affiliation country of the first author\cr
#' Countries \tab      \tab the affiliation countries' frequency distribution\cr
#' CountryCollaboration \tab      \tab Intra-country (SCP) and intercountry (MCP) collaboration indices\cr
#' TotalCitation \tab      \tab the number of times each manuscript has been cited\cr
#' TCperYear \tab      \tab the yearly average number of times each manuscript has been cited\cr
#' Sources \tab      \tab the frequency distribution of sources (journals, books, etc.)\cr
#' DE \tab      \tab the frequency distribution of authors' keywords\cr
#' ID \tab      \tab the frequency distribution of keywords associated to the manuscript by SCOPUS and Clarivate Analytics Web of Science database}
#'
#'
#' @examples
#' data(management)
#'
#' results <- biblioAnalysis(management)
#'
#' summary(results, k = 10, pause = FALSE)
#'
#' @seealso \code{\link{convert2df}} to import and convert an WoS or SCOPUS Export file in a bibliographic data frame.
#' @seealso \code{\link{summary}} to obtain a summary of the results.
#' @seealso \code{\link{plot}} to draw some useful plots of the results.
#'
#' @export

biblioAnalysis<-function(M,sep=";"){
  # initialize variables
  Authors=NULL
  Authors_frac=NULL
  FirstAuthors=NULL
  PY=NULL
  FAffiliation=NULL
  Affiliation=NULL
  Affiliation_frac=NULL
  CO=rep(NA,dim(M)[1])
  TC=NULL
  TCperYear=NULL
  SO=NULL
  Country=NULL
  DE=NULL
  ID=NULL
  MostCitedPapers=NULL
  
 



# M is the bibliographic dataframe
Tags<-names(M)

if (!("SR" %in% Tags)){M=metaTagExtraction(M,"SR")}

# temporal analyis

#if ("PY" %in% Tags){Years=table(M$PY)}

# Author's distribution

if ("AU" %in% Tags){
  listAU=strsplit(as.character(M$AU),sep)
  listAU=lapply(listAU, function(l) trim(l))
  nAU=unlist(lapply(listAU,length))  # num. of authors per paper
  fracAU=unlist(lapply(nAU,function(x){rep(1/x,x)}))  # fractional frequencies
  AU=unlist(listAU)

  Authors=sort(table(AU),decreasing=TRUE)
  Authors_frac=aggregate(fracAU,by=list(AU),'sum')
  names(Authors_frac)=c("Author","Frequency")
  Authors_frac=Authors_frac[order(-Authors_frac$Frequency),]
  FirstAuthors=unlist(lapply(listAU,function(l){
    if (length(l)>0){l=l[[1]]} else {l=NA}
    return(l)
  }))
  
  AuSingleAuthoredArt=length(unique(FirstAuthors[nAU==1]))
  AuMultiAuthoredArt=length(Authors)-AuSingleAuthoredArt
  }

#Total Citation Distribution
if ("TC" %in% Tags){
  TC=as.numeric(M$TC)
  PY=as.numeric(M$PY)
  CurrentYear=as.numeric(format(Sys.Date(),"%Y"))
  TCperYear=TC/(CurrentYear-PY+1)
  if (!("DI" %in% names(M))) M$DI <- ""
  MostCitedPapers <- data.frame(M$SR,M$DI,TC,TCperYear,PY) %>%
    group_by(.data$PY) %>%
    mutate(NTC = .data$TC/mean(.data$TC)) %>%
    ungroup() %>% 
    select(-.data$PY) %>%
    arrange(desc(.data$TC)) %>%
    as.data.frame()

  names(MostCitedPapers)=c("Paper         ","DOI","TC","TCperYear","NTC")
}

# References
nReferences <- 0
if ("CR" %in% Tags){
  CR <- tableTag(M,"CR",sep)
  nReferences <- length(CR)
  }

# ID Keywords
if ("ID" %in% Tags){ID <- tableTag(M,"ID",sep)}

# DE Keywords
if ("DE" %in% Tags){DE=tableTag(M,"DE",sep)}

# Sources
if ("SO" %in% Tags){
  SO=gsub(",","",M$SO,fixed=TRUE)
  SO=sort(table(SO),decreasing = TRUE)
  }

# All Affiliations, First Affiliation and Countries
if (("C1" %in% Tags) & (sum(!is.na(M$C1))>0)){
  if(!("AU_UN" %in% Tags)){M=metaTagExtraction(M,Field="AU_UN")}
  AFF=M$AU_UN
  listAFF=strsplit(AFF,sep,fixed=TRUE)
  nAFF=unlist(lapply(listAFF,length))
  listAFF[nAFF==0]="NA"
  fracAFF=unlist(sapply(nAFF,function(x){
    if(x>0){x=rep(1/x,x)}else{
      x=0}
    }))  # fractional frequencies
  AFF=trim.leading(unlist(listAFF))  # delete spaces
  Affiliation=sort(table(AFF),decreasing=TRUE)
  Affiliation_frac=aggregate(fracAFF,by=list(AFF),'sum')
  names(Affiliation_frac)=c("Affiliation","Frequency")
  Affiliation_frac=Affiliation_frac[order(-Affiliation_frac$Frequency),]
  
  # First Affiliation
  FAffiliation=lapply(listAFF,function(l) l[1])

  # Countries
  data("countries",envir=environment())
  countries=as.character(countries[[1]])
  
  ### new code{
    if (!("AU1_CO" %in% names(M))){
      M=metaTagExtraction(M,Field="AU1_CO",sep)}
    CO=M$AU1_CO

    Country=tableTag(M,"AU1_CO")
    
    SCP_MCP=countryCollaboration(M,Country,k=length(Country),sep)
  
}else{
    M$AU1_CO=NA
    SCP_MCP=data.frame(Country=rep(NA,1),SCP=rep(NA,1))
}
if ("DT" %in% names(M)){
  Documents=table(M$DT)
  n=max(nchar(names(Documents)))
  names(Documents)=substr(paste(names(Documents),"                                              ",sep=""),1,n+5)
}else{Documents=NA}

results=list(Articles=dim(M)[1],             # Articles
             Authors=Authors,                # Authors' frequency distribution
             AuthorsFrac=Authors_frac,       # Authors' frequency distribution (fractionalized)
             FirstAuthors=FirstAuthors,      # First Author's list
             nAUperPaper=nAU,                # N. Authors per Paper
             Appearances=sum(nAU),            # Author appearances
             nAuthors=dim(Authors),          # N. of Authors
             AuMultiAuthoredArt=AuMultiAuthoredArt, # N. of Authors of multi-authored articles
             AuSingleAuthoredArt=AuSingleAuthoredArt, # N. of Authors of single-authored articles
             MostCitedPapers=MostCitedPapers,# Papers sorted by citations
             Years=PY,                       # Years
             FirstAffiliation=unlist(FAffiliation),  # Affiliation of First Author
             Affiliations=Affiliation,       # Affiliations of all authors
             Aff_frac=Affiliation_frac,      # Affiliations of all authors (fractionalized)
             CO=CO,                          # Country of each paper
             Countries=Country,              # Countries' frequency distribution
             CountryCollaboration=SCP_MCP,   # Intracountry (SCP) and intercountry (MCP) collaboration
             TotalCitation=TC,               # Total Citations
             TCperYear=TCperYear,            # Total Citations per year
             Sources=SO,                     # Sources
             DE=DE,                          # Keywords
             ID=ID,                          # Authors' keywords
             Documents=Documents,
             nReferences = nReferences,      # N. of References
             DB=M$DB[1])
  class(results)<-"bibliometrix"

  return(results)
}
countryCollaboration<-function(M,Country,k,sep){
  if (!("AU_CO" %in% names(M))){M=metaTagExtraction(M,Field="AU_CO",sep)}
  M$SCP=0
  M$SCP_CO=NA
  for (i in 1:dim(M)[1]){
    if (!is.na(M$AU_CO[i])){
      co=M$AU_CO[i]
      co=table(unlist(strsplit(co,";")))
      if (length(co)==1){M$SCP[i]=1}
      M$SCP_CO[i]=M$AU1_CO[i]
      } else {M$SCP[i]=NA}
  }
  
  CO=names(Country)[1:k]
  
  df=data.frame(Country=rep(NA,k),SCP=rep(0,k))
  for (i in 1:length(CO)){
    co=CO[i]
    df$Country[i]=co
    df$SCP[i]=sum(M$SCP[M$SCP_CO==co],na.rm = T)
  }
  df$MCP=as.numeric(tableTag(M,"AU1_CO")[1:k])-df$SCP
  return(df)
}
