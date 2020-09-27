#' Meta-Field Tag Extraction
#'
#' It extracts other field tags, different from the standard WoS/SCOPUS codify.
#' @param M is a data frame obtained by the converting function \code{\link{convert2df}}.
#'        It is a data matrix with cases corresponding to articles and variables to Field Tag in the original WoS or SCOPUS file.
#' @param Field is a character object. New tag extracted from aggregated data is specified by this string. 
#' Field can be equal to one of these tags:
#' \tabular{lll}{
#' \code{"CR_AU"}\tab   \tab First Author of each cited reference\cr
#' \code{"CR_SO"}\tab   \tab Source of each cited reference\cr
#' \code{"AU_CO"}\tab   \tab Country of affiliation for co-authors \cr
#' \code{"AU1_CO"}\tab   \tab Country of affiliation for the first author\cr
#' \code{"AU_UN"}\tab   \tab University of affiliation for each co-author and the corresponding author (AU1_UN)\cr
#' \code{"SR"}\tab     \tab Short tag of the document (as used in reference lists)}
#'
#' @param sep is the field separator character. This character separates strings in each column of the data frame. The default is \code{sep = ";"}.
#' @param aff.disamb is a logical. If TRUE and Field="AU_UN", then a disambiguation algorithm is used to identify and match scientific affiliations 
#' (univ, research centers, etc.). The default is \code{aff.disamb=TRUE}.
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
#' #Example 3: Affiliation country for co-authors
#'
#' data(scientometrics)
#' scientometrics <- metaTagExtraction(scientometrics, Field = "AU_CO", sep = ";")
#' scientometrics$AU_CO[1:10]
#'
#' @seealso \code{\link{convert2df}} for importing and converting bibliographic files into a data frame.
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis
#' 
#' @export

metaTagExtraction<-function(M, Field = "CR_AU", sep = ";", aff.disamb=TRUE){
  
  
  ### data cleaning
  if ("CR" %in% names(M)){
    M$CR=gsub("DOI;","DOI ",as.character(M$CR))
  }
  
  
  ### SR field creation
  
  if (Field=="SR"){
    M<-SR(M)
  }
  
  
  if (Field=="CR_AU"){
    M<-CR_AU(M,sep)
  }
  
  ### CR_SO field creation
  
  if (Field=="CR_SO"){
    M<-CR_SO(M,sep)
  }
  ### AU_CO field creation
  
  if (Field=="AU_CO"){
    M<-AU_CO(M)  
  }
  
  if (Field=="AU1_CO"){
    M<-AU1_CO(M,sep)
  }
  
  
  # UNIVERSITY AFFILIATION OF ALL AUTHORS AND CORRESPONDING AUTHOR
  if (Field=="AU_UN"){
    ### with disambiguation
    if(isTRUE(aff.disamb)){M<-AU_UN(M,sep)
    }else{
      ### without disambiguation
      M$AU_UN=gsub("\\[.*?\\] ", "", M$C1)
      M$AU1_UN=unlist(lapply(strsplit(M$RP, sep), function(l){
        l=l[1]
        return(l)
      }))
      ind=regexpr("\\),", M$AU1_UN)
      a=which(ind>-1)
      M$AU1_UN[a]=trim(substr(M$AU1_UN[a],ind[a]+2,nchar(M$AU1_UN[a])))
    }
  }
  
  return(M)
}


### SR field
SR <- function(M){
  listAU=strsplit(as.character(M$AU),";")
  listAU=lapply(listAU, function(l) trim.leading(l))
  if (M$DB[1]=="scopus"){
    listAU=lapply(listAU,function(l){
      l=trim(l)
      l=sub(" ",",",l, fixed = TRUE)
      l=sub(",,",",",l, fixed = TRUE)
      l=gsub(" ","",l, fixed = TRUE)})}
  FirstAuthors=gsub(","," ",unlist(lapply(listAU, function(l){
    if (length(l>0)) {l=l[[1]]} else (l="NA")
    return(l)
  })))
  
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
  
  M$SR_FULL<- gsub("\\s+", " ", SR)
  
  ## assign an unique name to each document
  SR<- gsub("\\s+", " ", SR)
  st<-i<-0
  while(st==0){
    ind <- which(duplicated(SR))
    if (length(ind)>0){
      i <- i+1
      SR[ind]=paste0(SR[ind],"-",letters[i],sep="")}else{st <- 1}}
  M$SR<- SR
  #M$SR<- gsub("\\s+", " ", SR)
  return(M)
}

### CR_AU field
CR_AU<-function(M,sep){
  FCAU=list(NULL)
  CCR=NULL
  size=dim(M)
  CR=M$CR
  listCAU=strsplit(as.character(CR),sep)
  listCAU=lapply(listCAU,function(l) l=l[nchar(l)>10])  ## delete not congruent references
  
  # vector of cited authors
  for (i in 1:size[1]){
    FCAU[[i]]=gsub("[[:punct:]]", "",trim.leading(sub(",.*", "", listCAU[[i]])))
    CCR[i]=paste(FCAU[[i]],collapse=";")}
  
  M$CR_AU=CCR
  return(M)
}

### CR_SO field
CR_SO<-function(M,sep){
  FCAU=list(NULL)
  CCR=NULL
  size=dim(M)
  CR<-M$CR
  listCAU=strsplit(as.character(CR),sep)
  
  
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
  return(M)
}

### AU_CO field
AU_CO<-function(M){
  # Countries
  size=dim(M)[1]
  data("countries",envir=environment())
  countries=as.character(countries[[1]])
  if (M$DB[1] %in% c("ISI", "PUBMED")){
    countries=as.character(sapply(countries,function(s) paste0(s,".",collapse="")))
  } else if (M$DB[1]=="SCOPUS"){
    countries=as.character(sapply(countries,function(s) paste0(s,";",collapse="")))}
  
  M$AU_CO=NA
  C1=M$C1
  ## must replace all NA before "removing reprint info", or NA_character_ became string "NA"
  C1[which(is.na(C1))]=M$RP[which(is.na(C1))]
  
  ## remove reprint information from C1
  C1=unlist(lapply(C1,function(l){
    l=unlist(strsplit(l,";"))
    #l=l[regexpr("REPRINT AUTHOR",l)==-1]
    l=paste0(l,collapse=";")
  }))
  ### above changes all NA to 'NA'
  
  C1=gsub("\\[.*?\\] ", "", C1)
  ## change 'NA' back to NA
  C1[which(C1 == "NA")]=NA
  if (M$DB[1]=="ISI"){ C1=lastChar(C1,last=".")}
  if (M$DB[1]=="SCOPUS"){ C1=lastChar(C1,last=";")}
  
  #C1=gsub("[[:punct:][:blank:]]+", " ", C1)
  RP=M$RP
  #RP[which(is.na(RP))]=M$RRP)
  RP=paste(RP,";",sep="")
  #RP = gsub("[[:punct:][:blank:]]+", " ", RP)
  ## this will make gregexpr(l, RP[i], fixed=TRUE) fail as countries has the format '_COUNTRY_.' or '_COUNTRY_;'
  
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
  M$AU_CO=gsub("UNITED STATES","USA",M$AU_CO)
  M$AU_CO=gsub("TAIWAN","CHINA",M$AU_CO)
  M$AU_CO=gsub("ENGLAND","UNITED KINGDOM",M$AU_CO)
  M$AU_CO=gsub("SCOTLAND","UNITED KINGDOM",M$AU_CO)
  M$AU_CO=gsub("WALES","UNITED KINGDOM",M$AU_CO)
  M$AU_CO=gsub("NORTH IRELAND","UNITED KINGDOM",M$AU_CO)
  
  if (M$DB[1]=="ISI"){M$AU_CO=removeLastChar(M$AU_CO,last=".")}
  if (M$DB[1]=="SCOPUS"){M$AU_CO=removeLastChar(M$AU_CO,last=";")}
  
  return(M) 
}

### AU1_CO field
AU1_CO<-function(M,sep){
  size=dim(M)[1]
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
  ## do this before strsplit(), otherwise entries with multiple (reprint) author would be split between first group of authors
  C1=gsub("\\[.*?\\] ", "", C1)
  ## remove string before the first "(REPRINT AUTHOR)", otherwise C1 may get split between first group of authors, thus removing address, forcing it to default to RP.
  C1=gsub("^.*?\\(REPRINT\\sAUTHOR\\)", "", C1)
  C1=unlist(lapply(strsplit(C1,sep),function(l) l[1]))
  ## remove all characters before the last comma, thus constantly leaving only country, or in the case of US, state + zip_code + country.
  ## this way the need to distinguish Georgia and Georgia, US is eliminated.
  C1=gsub("^(.+)?,", "", C1)
  C1=gsub("[[:punct:][:blank:]]+", " ", C1)
  C1=paste(trim(C1)," ",sep="")
  if (M$DB[1]!="PUBMED"){
    RP=M$RP
    #RP[which(is.na(RP))]=M$RRP)
    RP=paste(RP,";",sep="")
    RP=gsub("[[:punct:][:blank:]]+", " ", RP)} else {
      RP <- C1 <-paste(" ",gsub("[[:punct:]]","",C1),sep="")
      }
  
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
  M$AU1_CO=gsub("UNITED STATES","USA",M$AU1_CO)
  M$AU1_CO=gsub("TAIWAN","CHINA",M$AU1_CO)
  M$AU1_CO=gsub("ENGLAND","UNITED KINGDOM",M$AU1_CO)
  M$AU1_CO=gsub("SCOTLAND","UNITED KINGDOM",M$AU1_CO)
  M$AU1_CO=gsub("WALES","UNITED KINGDOM",M$AU1_CO)
  M$AU1_CO=gsub("NORTH IRELAND","UNITED KINGDOM",M$AU1_CO)
  #M$AU1_CO=gsub(".", "", M$AU1_CO, fixed = TRUE)
  #M$AU1_CO=gsub(";;", ";", M$AU1_CO, fixed = TRUE)
  return(M)
}

### AU_UN field
AU_UN<-function(M,sep){
  
  ## remove reprint information from C1
  C1=M$C1
  # C1=unlist(lapply(C1,function(l){
  #   l=unlist(strsplit(l,";"))
  #   #l=l[regexpr("REPRINT AUTHOR",l)==-1]
  #   l=paste0(l,collapse=";")
  # }))
  ###
  AFF=gsub("\\[.*?\\] ", "", C1)
  indna=which(is.na(AFF))
  if (length(indna)>0){AFF[indna]=M$RP[indna]}
  nc=nchar(AFF)
  AFF[nc==0]=NA
  
  listAFF=strsplit(AFF,sep,fixed=TRUE)
  
  uTags=c("UNIV","COLL","SCH","INST","ACAD","ECOLE","CTR","SCI","CENTRE","CENTER","CENTRO","HOSP","ASSOC","COUNCIL",
          "FONDAZ","FOUNDAT","ISTIT","LAB","TECH","RES","CNR","ARCH","SCUOLA","PATENT OFF","CENT LIB","HEALTH","NATL",
          "LIBRAR","CLIN","FDN","OECD","FAC","WORLD BANK","POLITECN","INT MONETARY FUND","CLIMA","METEOR","OFFICE","ENVIR",
          "CONSORTIUM","OBSERVAT","AGRI", "MIT ", "INFN", "SUNY ")
  
  AFFL=lapply(listAFF, function(l){
    #l=gsub(","," ,",l)
    l<-gsub("\\(REPRINT AUTHOR\\)","",l)
    index=NULL
    
    for (i in 1:length(l)){
      #ind=list()
      affL=unlist(strsplit(l[i],",",fixed=TRUE))
      
      indd=unlist(lapply(uTags,function(x) which(regexpr(x,affL,fixed=TRUE)!=-1)))
      
      
      # if (length(indd)==0){index=append(index,"NR")
      #   } else if (grepl("[[:digit:]]", affL[indd[1]])){index=append(index,"ND")
      #   } else {index=append(index,affL[indd[1]])}
      
      if (length(indd)==0){index=append(index,"NOTREPORTED")
      } else if (isTRUE(ND(affL,indd)$cond)){index=append(index,"NOTDECLARED")
      } else {index=append(index,ND(affL,indd)$affL)}
      
      
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
  
  
  ## identification of Corresponding author affiliation
  if (!("RP" %in% names(M))){
    M$RP <- NA
  }
  RP <- M$RP
  RP[is.na(RP)]=M$C1[is.na(RP)]
  AFF=gsub("\\[.*?\\] ", "", RP)
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
      #ind=list()
      affL=unlist(strsplit(l[i],",",fixed=TRUE))
      
      indd=unlist(lapply(uTags,function(x) which(regexpr(x,affL,fixed=TRUE)!=-1)))
      
      if (length(indd)==0){index=append(index,"NOTREPORTED")
      } else if (grepl("[[:digit:]]", affL[indd[1]])){index=append(index,"NOTDECLARED")
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
  M$AU1_UN=AFFL
  M$AU1_UN=gsub("\\\\&","AND",M$AU1_UN)
  M$AU1_UN=gsub("\\&","AND",M$AU1_UN)
  
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
  M$AU_UN[is.na(AFF)]=NA
  M$AU_UN[M$AU_UN=="NOTDECLARED"]=NA
  M$AU_UN[M$AU_UN=="NOTREPORTED"]=NA
  M$AU_UN=gsub("NOTREPORTED;","",M$AU_UN)
  M$AU_UN=gsub(";NOTREPORTED","",M$AU_UN)
  M$AU_UN=gsub("NOTDECLARED;","",M$AU_UN)
  M$AU_UN=gsub("NOTDECLARED","",M$AU_UN)
  
  return(M)
}


### tools
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
### remove non interesting field
ND<-function(affL,indd){
  aff=affL[!grepl("[[:digit:]]", affL)]
  ind=indd[!grepl("[[:digit:]]", affL[indd])]
  cond=length(ind)<1
  r=list(affL=aff[ind[1]],cond=cond)
  return(r)
}