ValueBoxes <- function(M){
  # calculate statistics for Biblioshiny ValueBoxes
  df <- data.frame(Description=rep(NA,12), Results=rep(NA,12))
  
  ## VB  1 - Time span
  df[1,] <- c("Timespan",paste(range(M$PY),collapse=":"))
  
  ## VB  2 - Authors
  listAU <- (strsplit(M$AU, ";"))
  nAU <- lengths(listAU)
  listAU <- unique(trimws((unlist(listAU))))
  listAU <- listAU[!is.na(listAU)]
  df[2,] <- c("Authors",length(listAU))
  
  ## VB  3 - Author's Keywords (DE)
  DE <- unique(trimws(gsub("\\s+|\\.|\\,"," ",unlist(strsplit(M$DE, ";")))))
  DE <- DE[!is.na(DE)]
  df[3,] <- c("Author's Keywords (DE)",length(DE))
  
  ## VB  4 - Sources
  df[4,] <- c("Sources (Journals, Books, etc)",length(unique(M$SO)))
  
  ## VB  5 - Authors of single-authored docs
  
  df[5,] <- c("Authors of single-authored docs",length(unique(M$AU[nAU==1])))
  
  ## VB  6 - References
  CR <- trimws(gsub("\\s+|\\.|\\,"," ",unlist(strsplit(M$CR,";"))))
  CR <- CR[nchar(CR)>0 & !is.na(CR)]
  df[6,] <- c("References",length(unique(CR)))
  
  ## VB  7 - Documents
  df[7,] <- c("Documents", nrow(M))
  
  ## VB  8 - International Co-Authorship
  if (!"AU_CO" %in% names(M)){
    M <- metaTagExtraction(M, "AU_CO")
  }
  AU_CO <- strsplit(M$AU_CO,";")
  Coll <- unlist(lapply(AU_CO, function(l){
    length(unique(l))>1
  }))
  Coll <- sum(Coll)/nrow(M)*100
  df[8,] <- c("International co-authorships %", format(Coll,digits=4))
  
  ## VB  9 - Document Average Age
  age <- as.numeric(substr(Sys.Date(),1,4))-M$PY
  df[9,] <- c("Document Average Age", format(mean(age,na.rm=TRUE),digits=3))
  
  ## VB 10 - Annual Growth Rate
  Y=table(M$PY)
  ny=diff(range(M$PY, na.rm=TRUE))
  CAGR<-as.numeric(round(((Y[length(Y)]/Y[1])^(1/(ny))-1)*100,2))
  df[10,] <- c("Annual Growth Rate %", CAGR)
  
  ## VB 11 - Co-Authors per Doc
  df[11,] <- c("Co-Authors per Doc", format(mean(nAU, na.rm=T), digit = 3))
  
  ## VB 12 - Average citations per doc
  df[12,] <- c("Average citations per doc", format(mean(M$TC, na.rm=T), digit = 4))
  
  return(df)
}


countryCollab<-function(M){
  sep=";"
  if (!("AU_CO" %in% names(M))){M=metaTagExtraction(M,Field="AU_CO",sep)}
  if (!("AU1_CO" %in% names(M))){M=metaTagExtraction(M,Field="AU1_CO",sep)}
  
  M$nCO <- as.numeric(unlist(lapply(strsplit(M$AU_CO,";"), function(l){
    length(unique(l))>1
  })))
  
  df <- M %>% group_by(.data$AU1_CO) %>% 
    select(.data$AU1_CO,.data$nCO) %>% 
    summarize(Articles=n(),
              SCP=sum(.data$nCO==0),
              MCP=sum(.data$nCO==1)) %>% 
    rename(Country = .data$AU1_CO) %>% 
    arrange(desc(.data$Articles))
  
  return(df)
}
