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


