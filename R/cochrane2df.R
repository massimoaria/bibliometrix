cochrane2df<-function(D){
  
  D <- D[nchar(D)>0]  # remove empty rows

  Papers <- which(substr(D,1,8)=="Record #") # first row of each document
  nP=length(Papers)  # number of documents
  
  rowPapers <- diff(c(Papers, length(D)+1))
  
  numPapers <- rep(1:nP,rowPapers)
  
  DATA <- data.frame(Tag = substr(D,1,4), content = substr(D,5,nchar(D)), Paper=numPapers, stringsAsFactors = FALSE)
  DATA$Tag <- gsub(" ","",gsub(":","",DATA$Tag))
  df <- DATA %>% group_by(.data$Paper, .data$Tag) %>%
    summarise(cont=paste(.data$content, collapse="---",sep="")) %>%
    arrange(.data$Tag, .data$Paper) %>%
    pivot_wider(names_from =  .data$Tag,values_from = .data$cont) %>%
    ungroup() %>%
    rename("PY" = "YR",
           "UT" = "ID",
           "ID" = "KY",
           "URL" = "US",
           "DI" = "DOI",
           "NR" = "NO") %>%
    as.data.frame()
  
  df$PY <- as.numeric(df$PY)
  
  ### replace "---" with ";"
  tagsComma <- c("AU","ID")
  df1 <- data.frame(lapply(df[tagsComma],function(x){
    gsub("---",";",x)
  }),stringsAsFactors = FALSE)
  
  ### replace "---" with " "
  otherTags <- setdiff(names(df),tagsComma)
  df2 <- data.frame(lapply(df[otherTags],function(x){
    trimES(gsub("---"," ",x))
  }),stringsAsFactors = FALSE)
  df <- cbind(df1,df2)
  rm(df1,df2)
  
  df$ID=gsub(" ;",";",gsub("; ",";", gsub("\\[[^\\]]*\\]", "", df$ID, perl=TRUE)))
  
  df$DB <- "COCHRANE"
  
  # Authors
  #df$AU <- trimES(gsub("-","",df$AU))
  
  # Toupper
  DI <- df$DI
  df <- data.frame(lapply(df,toupper),stringsAsFactors = FALSE)
  df$DI <- gsub(" ","",DI)
  
  df <- df[!(names(df) %in% c("Paper", "Reco"))]
  
  df$DE <- df$ID
  
  df$JI <- df$J9 <- df$SO
  
  return(df)
}

