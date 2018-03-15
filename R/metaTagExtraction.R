#' Meta-Field Tag Extraction
#'
#' It extracts other field tags, different from the standard ISI/SCOPUS codify.
#' @param M is a data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to articles and variables to Field Tag in the original ISI or SCOPUS file.
#' @param Field is a character object. New tag exctracted from aggregated data is specified by this string. 
#' Field can be equal to one of this tags:
#' \tabular{lll}{
#' \code{"CR_AU"}\tab   \tab First Author of each cited reference\cr
#' \code{"CR_SO"}\tab   \tab Source of each cited reference\cr
#' \code{"AU_CO"}\tab   \tab Country of affiliation for each co-author\cr
#' \code{"AU1_CO"}\tab   \tab Country of affiliation for the first author\cr
#' \code{"AU_UN"}\tab   \tab University of affiliation for each co-author\cr
#' \code{"SR"}\tab     \tab Short tag of the document (as used in reference lists)}
#'
#' @param sep is the field separator character. This character separates strings in each column of the data frame. The default is \code{sep = ";"}.
#' @return the bibliometric data frame with a new column containing data about new field tag indicated in the argument \code{Field}.
#'
#'
#'
#' @examples
#' # Example 1: First Authors for each cited reference
#'
#' data(scientometrics)
#' scientometrics <- metaTagExtraction(scientometrics, Field = "CR_AU", sep = ";")
#' unlist(strsplit(scientometrics$CR_AU[1], ";"))
#'
#'
#' #Example 2: Source for each cited reference
#'
#' data(scientometrics)
#' scientometrics <- metaTagExtraction(scientometrics, Field = "CR_SO", sep = ";")
#' unlist(strsplit(scientometrics$CR_SO[1], ";"))
#'
#' #Example 3: Affiliation country for co-author
#'
#' data(scientometrics)
#' scientometrics <- metaTagExtraction(scientometrics, Field = "AU_CO", sep = ";")
#' scientometrics$AU_CO[1:10]
#'
#' @seealso \code{\link{scopus2df}} for converting ISO or SCPUS Export file into a data frame.
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis
#' 
#' @export

metaTagExtraction<-function(M, Field = "CR_AU", sep = ";"){


### SR field creation

if (Field=="SR"){
  
  listAU=strsplit(as.character(M$AU),";")
  listAU=lapply(listAU, function(l) trim.leading(l))
  if (M$DB[1]=="scopus"){
    listAU=lapply(listAU,function(l){
      l=trim(l)
      l=sub(" ",",",l, fixed = TRUE)
      l=sub(",,",",",l, fixed = TRUE)
      l=gsub(" ","",l, fixed = TRUE)})}
  FirstAuthors=gsub(","," ",unlist(lapply(listAU,function(l) l[[1]])))
  
  if (!is.null(M$J9)){
    ## replace full title in no iso names
    no_art=which(is.na(M$J9) & is.na(M$JI))
    M$J9[no_art]=M$SO[no_art]
    ## repleace NA in J9 with JIO
    ind=which(is.na(M$J9))
    M$J9[ind]=trim(gsub("\\."," ",M$JI[ind]))
    SR=paste(FirstAuthors,M$PY,M$J9,sep=", ")}else{
      no_art=which(is.na(M$JI))
      M$JI[no_art]=M$SO[no_art]
      J9=trim(gsub("\\."," ",M$JI))
      SR=paste(FirstAuthors,M$PY,J9,sep=", ")}
  
  ## assign an unique name to each document
  st<-i<-0
  while(st==0){
    ind <- which(duplicated(SR))
    if (length(ind)>0){
      i <- i+1
      SR[ind]=paste0(SR[ind],"-",letters[i],sep="")}else{st <- 1}}
  
  M$SR<- gsub("\\s+", " ", SR)
}


size=dim(M)
FCAU=list(NULL)
CCR=NULL
if ("CR" %in% names(M)){
  M$CR=str_replace_all(as.character(M$CR),"DOI;","DOI ")
  CR=M$CR}
  
if (Field=="CR_AU"){

listCAU=strsplit(as.character(CR),sep)
listCAU=lapply(listCAU,function(l) l=l[nchar(l)>10])  ## delete not congruent references

  # vector of cited authors
  for (i in 1:size[1]){
    FCAU[[i]]=str_replace_all(trim.leading(sub(",.*", "", listCAU[[i]])), "[[:punct:]]", "")
    CCR[i]=paste(FCAU[[i]],collapse=";")}

  M$CR_AU=CCR
  }

### CR_SO field creation

if (Field=="CR_SO"){
  listCAU=strsplit(as.character(CR),sep)
  FCAU=list(NULL)

  # vector of cited Journals
  if (M$DB[1]=="ISI"){
  for (i in 1:size[1]){

    elem=strsplit(as.character(listCAU[[i]]),",")
    ind=lengths(elem)
    if (max(ind)>2) {
    elem=elem[ind>2]
    FCAU[[i]]=trim.leading(unlist(lapply(elem,function(l) l[[3]])))
    CCR[i]=paste(FCAU[[i]],collapse=";")}
    else {CCR[[i]]=NA}}

    } else if (M$DB[1]=="SCOPUS") {


    for (i in 1:size[1]){

      listCAU[[i]]=gsub(".*?\\) ", "", listCAU[[i]])
      elem=strsplit(as.character(listCAU[[i]]),",")
      ind=lengths(elem)
      CCR[[i]]=NA
      if (length(ind)>0){
      if (max(ind)>2) {
        elem=elem[ind>2]
        FCAU[[i]]=trim.leading(unlist(lapply(elem,function(l) l[[1]])))
        CCR[i]=paste(FCAU[[i]],collapse=";")}}
      } 
  }

  M$CR_SO=CCR
}

### AU_CO field creation

if (Field=="AU_CO"){
  # Countries
  
  data("countries",envir=environment())
  countries=as.character(countries[[1]])
  if (M$DB[1]=="ISI"){
  countries=as.character(sapply(countries,function(s) paste0(s,".",collapse="")))
  } else if (M$DB[1]=="SCOPUS"){
    countries=as.character(sapply(countries,function(s) paste0(s,";",collapse="")))}

  M$AU_CO=NA
  C1=M$C1
  C1[which(is.na(C1))]=M$RP[which(is.na(C1))]
  C1=gsub("\\[.*?\\] ", "", C1)
  if (M$DB[1]=="ISI"){ C1=lastChar(C1,last=".")}
  if (M$DB[1]=="SCOPUS"){ C1=lastChar(C1,last=";")}
  
  #C1=gsub("[[:punct:][:blank:]]+", " ", C1)
  RP=M$RP
  #RP[which(is.na(RP))]=M$RRP)
  RP=paste(RP,";",sep="")
  RP=gsub("[[:punct:][:blank:]]+", " ", RP)
  
  for (i in 1:size[1]){
    if (!is.na(C1[i])){
    ind=unlist(sapply(countries, function (l) (gregexpr ( l , C1[i],fixed=TRUE))))
    if (sum(ind>-1)>0) {M$AU_CO[i]=paste(unique(names(ind[ind>-1])),collapse=";")}
    }
    if (is.na(M$AU_CO[i])){
      ind=unlist(sapply(countries, function (l) (gregexpr ( l , RP[i],fixed=TRUE))))
      if (sum(ind>-1)>0) {M$AU_CO[i]=paste(unique(names(ind[ind>-1])),collapse=";")}  
    }
  }
  M$AU_CO=gsub("[[:digit:]]","",M$AU_CO)
  M$AU_CO=gsub(".", "", M$AU_CO, fixed = TRUE)
  M$AU_CO=gsub(";;", ";", M$AU_CO, fixed = TRUE)
  
  if (M$DB[1]=="ISI"){M$AU_CO=removeLastChar(M$AU_CO,last=".")}
  if (M$DB[1]=="SCOPUS"){M$AU_CO=removeLastChar(M$AU_CO,last=";")}
  
  
}

if (Field=="AU1_CO"){
  # Countries
  data("countries",envir=environment())
  countries=as.character(countries[[1]])
  #if (M$DB[1]=="ISI"){
  #countries=as.character(sapply(countries,function(s) paste0(" ",s," ",collapse="")))
  #} else if (M$DB[1]=="SCOPUS"){
  #  countries=as.character(sapply(countries,function(s) paste0(s,";",collapse="")))}
  countries=paste(" ",countries," ",sep="")
  M$AU1_CO=NA
  C1=M$C1
  C1[which(!is.na(M$RP))]=M$RP[which(!is.na(M$RP))]
  C1=unlist(lapply(strsplit(C1,sep),function(l) l[1]))
  C1=gsub("\\[.*?\\] ", "", C1)
  C1=gsub("[[:punct:][:blank:]]+", " ", C1)
  C1=paste(trim(C1)," ",sep="")
  if (M$DB[1]!="PUBMED"){
  RP=M$RP
  #RP[which(is.na(RP))]=M$RRP)
  RP=paste(RP,";",sep="")
  RP=gsub("[[:punct:][:blank:]]+", " ", RP)} else {RP=C1}
  
  for (i in 1:size[1]){
    if (!is.na(C1[i])){
      ind=unlist(sapply(countries, function (l) (gregexpr ( l , C1[i],fixed=TRUE))))
      if (sum(ind>-1)>0) {M$AU1_CO[i]=paste(unique(names(ind[ind>-1][1])),collapse=";")
      #print(i)
      #print(M$AU1_CO[i])
      }
    }
    if (is.na(M$AU1_CO[i])){
      ind=unlist(sapply(countries, function (l) (gregexpr ( l , RP[i],fixed=TRUE))))
      if (sum(ind>-1)>0) {M$AU1_CO[i]=paste(unique(names(ind[ind>-1][1])),collapse=";")}  
    }
  }
  M$AU1_CO=trim(gsub("[[:digit:]]","",M$AU1_CO))
  #M$AU1_CO=gsub(".", "", M$AU1_CO, fixed = TRUE)
  #M$AU1_CO=gsub(";;", ";", M$AU1_CO, fixed = TRUE)
  
}

# UNIVERSITY AFFILIATION
if (Field=="AU_UN"){
  
  AFF=gsub("\\[.*?\\] ", "", M$C1)
  indna=which(is.na(AFF))
  if (length(indna)>0){AFF[indna]=M$RP[indna]}
  nc=nchar(AFF)
  AFF[nc==0]=NA
  
  listAFF=strsplit(AFF,sep,fixed=TRUE)
  
  
  AFFL=lapply(listAFF, function(l){
    #l=gsub(","," ,",l)
    l<-gsub("\\(REPRINT AUTHOR\\)","",l)
    index=NULL
    
    for (i in 1:length(l)){
      ind=list()
      affL=unlist(strsplit(l[i],",",fixed=TRUE))
    #affL=l[i]
      ind[[1]]=which(regexpr("UNIV",affL,fixed=TRUE)!=-1)
      ind[[2]]=which(regexpr("COLL",affL,fixed=TRUE)!=-1)
      ind[[3]]=which(regexpr("SCH",affL,fixed=TRUE)!=-1)
      ind[[4]]=which(regexpr("INST",affL,fixed=TRUE)!=-1)
      ind[[5]]=which(regexpr("ACAD",affL,fixed=TRUE)!=-1)
      ind[[6]]=which(regexpr("ECOLE",affL,fixed=TRUE)!=-1)
      ind[[7]]=which(regexpr("CTR",affL,fixed=TRUE)!=-1)
      ind[[8]]=which(regexpr("SCI",affL,fixed=TRUE)!=-1)
      ind[[9]]=which(regexpr("HOSP",affL,fixed=TRUE)!=-1)
      ind[[10]]=which(regexpr("CENTRE",affL,fixed=TRUE)!=-1)
      ind[[11]]=which(regexpr("CENTER",affL,fixed=TRUE)!=-1)
      ind[[12]]=which(regexpr("CENTRO",affL,fixed=TRUE)!=-1)
      ind[[13]]=which(regexpr("ASSOC",affL,fixed=TRUE)!=-1)
      ind[[14]]=which(regexpr("FONDAZ",affL,fixed=TRUE)!=-1)
      ind[[15]]=which(regexpr("FOUNDAT",affL,fixed=TRUE)!=-1)
      ind[[16]]=which(regexpr("ISTIT",affL,fixed=TRUE)!=-1)
      ind[[17]]=which(regexpr("LAB",affL,fixed=TRUE)!=-1)
      ind[[18]]=which(regexpr("TECH",affL,fixed=TRUE)!=-1)
      ind[[19]]=which(regexpr("RES",affL,fixed=TRUE)!=-1)
      ind[[20]]=which(regexpr("CNR",affL,fixed=TRUE)!=-1)
      ind[[21]]=which(regexpr("ARCH",affL,fixed=TRUE)!=-1)
      ind[[22]]=which(regexpr("SCUOLA",affL,fixed=TRUE)!=-1)
      ind[[23]]=which(regexpr("PATENT OFF",affL,fixed=TRUE)!=-1)
      ind[[24]]=which(regexpr("CENT LIB",affL,fixed=TRUE)!=-1)
      ind[[25]]=which(regexpr("LIBRAR",affL,fixed=TRUE)!=-1)
      ind[[26]]=which(regexpr("CLIN",affL,fixed=TRUE)!=-1)
      ind[[27]]=which(regexpr("FDN",affL,fixed=TRUE)!=-1)
      ind[[28]]=which(regexpr("OECD",affL,fixed=TRUE)!=-1)
      ind[[29]]=which(regexpr("FAC",affL,fixed=TRUE)!=-1)
      ind[[30]]=which(regexpr("WORLD BANK",affL,fixed=TRUE)!=-1)
      ind[[31]]=which(regexpr("POLITECN",affL,fixed=TRUE)!=-1)
      ind[[32]]=which(regexpr("INT MONETARY FUND",affL,fixed=TRUE)!=-1)
      
      
      for (a in 1:length(ind)){
        indd=ind[[a]]
        if (length(indd)>0){break()}
      }
      
      if (length(indd)==0){index=append(index,"NR")
      } else if (grepl("[[:digit:]]", affL[indd[1]])){index=append(index,"ND")
      } else {index=append(index,affL[indd[1]])}
      
    }
      #index=unique(c(ind1,ind2,ind3,ind4,ind5,ind6,ind7,ind8))
    x=""
    if (length(index)>0){
      #x=paste0(trim.leading((affL[index])),collapse=",")
      x=paste0(trim.leading(index),collapse=";")
      x=gsub(" ,",";",x)}
    return(x)
  })
  AFFL=unlist(AFFL)
  M$AU_UN=AFFL
  M$AU_UN=gsub("\\\\&","AND",M$AU_UN)
  M$AU_UN=gsub("\\&","AND",M$AU_UN)

  ## identification of NR affiliations
  M$AU_UN_NR=NA
  listAFF2=strsplit(M$AU_UN,sep)
  cont=lapply(listAFF2, function(l){
    l=unlist(l)
    ind=which(l %in% "NR")
  })
  
  for (i in 1:length(cont)){
    if (length(cont[[i]])>0){
      M$AU_UN_NR[i]=paste(trim(listAFF[[i]][cont[[i]]]),collapse=";")
    }
  }
}

return(M)
}

lastChar<-function(C,last="."){
  A=substr(C,nchar(C),nchar(C))
  ind=which((A!=last) & (!is.na(A)))
  C[ind]=paste0(C[ind],last)
  return(C)
}

removeLastChar<-function(C,last="."){
  A=substr(C,nchar(C),nchar(C))
  ind=which((A==last) & (!is.na(A)))
  C[ind]=substr(C[ind],1,(nchar(C[ind])-1))
  return(C)
}
