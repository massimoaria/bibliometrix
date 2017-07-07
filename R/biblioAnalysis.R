#' Bibliometric Analysis
#'
#' It performs a bibliometric analysis of a dataset imported from SCOPUS and Thomson Reuters' ISI Web of Knowledge databases.
#'
#' @param M is a bibliographic data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to manuscripts and variables to Field Tag in the original SCOPUS and Thomson Reuters' ISI Web of Knowledge file.
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
#' FirstAuthors \tab      \tab first author of each manuscript\cr
#' nAUperPaper \tab      \tab the number of authors per manuscript\cr
#' Appearances \tab      \tab the number of author appearances\cr
#' nAuthors \tab       \tab the number of authors\cr
#' AuMultiAuthoredArt \tab      \tab the number of authors of multi authored articles\cr
#' MostCitedPapers \tab      \tab The list of manuscripts sorted by citations\cr
#' Years \tab      \tab pubblication year of each manuscript\cr
#' FirstAffiliation \tab      \tab the affiliation of the first author\cr
#' Affiliations \tab      \tab the frequency distribution of affiliations (of all co-authors for each paper)\cr
#' Aff_frac \tab      \tab the fractionalized frequency distribution of affiliations (of all co-authors for each paper)\cr
#' CO \tab      \tab the affiliation country of first author\cr
#' Countries \tab      \tab the affiliation countries' frequency distribution\cr
#' TotalCitation \tab      \tab the number of times each manuscript has been cited\cr
#' TCperYear \tab      \tab the yearly average number of times each manuscript has been cited\cr
#' Sources \tab      \tab the frequency distribution of sources (journals, books, etc.)\cr
#' DE \tab      \tab the frequency distribution of authors' keywords\cr
#' ID \tab      \tab the frequency distribution of keywords associated to the manuscript bySCOPUS and Thomson Reuters' ISI Web of Knowledge database}
#'
#'
#' @examples
#' data(scientometrics)
#'
#' results <- biblioAnalysis(scientometrics)
#'
#' summary(results, k = 10, pause = FALSE)
#'
#' @seealso \code{\link{convert2df}} to import and convert an ISI or SCOPUS Export file in a bibliographic data frame.
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

# temporal analyis

if ("PY" %in% Tags){Years=table(PY)}

# Author's distribution

if ("AU" %in% Tags){
  listAU=strsplit(as.character(M$AU),sep)
  listAU=lapply(listAU, function(l) trim.leading(l))
  #if (M$DB[1]=="ISI"){
     listAU=lapply(listAU,function(l){
      l=trim.leading(l)
      l=sub(" ",",",l, fixed = TRUE)
      l=sub(",,",",",l, fixed = TRUE)
      l=gsub(" ","",l, fixed = TRUE)})
     #}
    
  #if (M$DB[1]=="SCOPUS"){
      #listAU=lapply(listAU,function(l){
      #l=trim.leading(l)
      #l=sub(" ",",",l, fixed = TRUE)
      #l=gsub(" ","",l, fixed = TRUE)})
      #}
    
  nAU=unlist(lapply(listAU,length))  # num. of authors per paper
  fracAU=unlist(sapply(nAU,function(x){rep(1/x,x)}))  # fractional frequencies
  AU=gsub(" ", "", unlist(listAU), fixed = TRUE)
  #if (M$DB[1]=="ISI"){AU=gsub(" ", "", unlist(listAU), fixed = TRUE)} # delete spaces
  #if (M$DB[1]=="SCOPUS"){AU=sub(" ",",",unlist(listAU),fixed=TRUE);AU=gsub(" ","",AU,fixed=TRUE)}
  Authors=sort(table(AU),decreasing=TRUE)
  Authors_frac=aggregate(fracAU,by=list(AU),'sum')
  names(Authors_frac)=c("Author","Frequency")
  Authors_frac=Authors_frac[order(-Authors_frac$Frequency),]
  FirstAuthors=lapply(listAU,function(l) l[[1]])
  listAUU=strsplit(as.character(M$AU[nAU>1]),sep)
  AuMultiAuthoredArt=length(unique(gsub(" ", "", unlist(listAUU), fixed = TRUE)))
  }

#Total Citation Distribution
if ("TC" %in% Tags){
  TC=as.numeric(M$TC)
  PY=as.numeric(M$PY)
  CurrentYear=as.numeric(format(Sys.Date(),"%Y"))
  TCperYear=TC/(CurrentYear-PY)
  if (sum(names(M) %in% "JI")==1){
    MostCitedPapers=data.frame(paste(M$AU,paste("(",M$PY,")",sep=""),M$JI,sep=","),TC,TCperYear)
  }else{MostCitedPapers=data.frame(paste(M$AU,paste("(",M$PY,")",sep=""),M$SO,sep=","),TC,TCperYear)}
  MostCitedPapers=MostCitedPapers[order(TC,decreasing=TRUE),]
  names(MostCitedPapers)=c("Paper         ","TC","TCperYear")
}

# References
if ("CR" %in% Tags){CR=tableTag(M,"CR",sep)}

# ID Keywords
if ("ID" %in% Tags){ID=tableTag(M,"ID",sep)}

# DE Keywords
if ("DE" %in% Tags){DE=tableTag(M,"DE",sep)}

# Sources
if ("SO" %in% Tags){
  SO=gsub(",","",M$SO,fixed=TRUE)
  SO=sort(table(SO),decreasing = TRUE)
  }

# All Affiliations, First Affiliation and Countries
if (("C1" %in% Tags) & (sum(!is.na(M$C1))>0)){
  AFF=gsub("\\[.*?\\] ", "", M$C1)
  listAFF=strsplit(AFF,sep,fixed=TRUE)
  nAFF=unlist(lapply(listAFF,length))  # num. of references per paper
  listAFF[nAFF==0]="NA"
  fracAFF=unlist(sapply(nAFF,function(x){rep(1/x,x)}))  # fractional frequencies
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


  if (M$DB[1]=="SCOPUS"){
    FA=paste(FAffiliation,";",sep="")
    RP=paste(M$RP,";",sep="")
    countries=as.character(sapply(countries,function(s) paste0(s,";",collapse="")))}
  else if (M$DB[1]=="ISI"){
    #FA=paste(FAffiliation,".",sep="")
    FA=FAffiliation
    RP=paste(M$RP,".",sep="")
    countries=as.character(sapply(countries,function(s) paste0(s,".",collapse="")))}

  for (i in 1:length(countries)){

    ind=which(regexpr(countries[i],FA,fixed=TRUE)!=-1)
    if (length(ind)>0){CO[ind]=countries[i]}

    indd=which(regexpr(countries[i],RP,fixed=TRUE)!=-1)
    if (length(indd)>0){CO[indd]=countries[i]}
  }
  CO=gsub(";","",CO)
  CO=gsub("\\.","",CO)
  CO=gsub("UNITED STATES","USA",CO)

  Country=sort(table(CO),decreasing = TRUE)


}

results=list(Articles=dim(M)[1],             # Articles
             Authors=Authors,                # Authors' frequency distribution
             AuthorsFrac=Authors_frac,       # Authors' frequency distribution (fractionalized)
             FirstAuthors=unlist(FirstAuthors),# First Author's list
             nAUperPaper=nAU,                # N. Authors per Paper
             Appearances=sum(nAU),            # Author appearances
             nAuthors=dim(Authors),          # N. of Authors
             AuMultiAuthoredArt=AuMultiAuthoredArt, # N. of Authors of multi authored articles
             MostCitedPapers=MostCitedPapers,# Papers sorted by citations
             Years=PY,                       # Years
             FirstAffiliation=unlist(FAffiliation),  # Affiliation of First Author
             Affiliations=Affiliation,       # Affiliations of all authors
             Aff_frac=Affiliation_frac,      # Affiliations of all authors (fractionalized)
             CO=CO,                          # Country of each paper
             Countries=Country,              # Countries' frequency distribution
             TotalCitation=TC,               # Total Citations
             TCperYear=TCperYear,            # Total Citations per year
             Sources=SO,                     # Sources
             DE=DE,
             ID=ID)
  class(results)<-"bibliometrix"

  return(results)
}
