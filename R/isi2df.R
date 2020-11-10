isi2df<-function(D){
  
  D <- D[nchar(D)>0]  # remove empty rows
  D <- D[!(substr(D,1,3) %in% c("FN ", "VR "))]
  
  for (i in 1:length(D)){
    if (substr(D[i],1,3)=="   ") substr(D[i],1,3) <- substr(D[i-1],1,3) 
  }
  Papers <- which(substr(D,1,3)=="PT ") # first row of each document
  nP=length(Papers)  # number of documents
  
  rowPapers <- diff(c(Papers, length(D)+1))
  
  numPapers <- rep(1:nP,rowPapers)
  
  DATA <- data.frame(Tag = substr(D,1,3), content = substr(D,4,nchar(D)), Paper=numPapers, stringsAsFactors = FALSE)
  DATA$Tag <- gsub(" ","",DATA$Tag)
  df <- DATA %>% group_by(.data$Paper, .data$Tag) %>%
    summarise(cont=paste(.data$content, collapse="---",sep="")) %>%
    arrange(.data$Tag, .data$Paper) %>%
    pivot_wider(names_from =  .data$Tag,values_from = .data$cont) %>%
    ungroup() %>%
    as.data.frame()
  
  
  df$PY <- as.numeric(df$PY)
  
  ### replace "---" with ";"
  tagsComma <- c("AU","AF","CR")
  
  nolab <- setdiff(tagsComma,names(df))
  if (length(nolab)>0){
    cat("\nWarning:\nIn your file, some mandatory metadata are missing. Bibliometrix functions may not work properly!\n
Please, take a look at the vignettes:
- 'Data Importing and Converting' (https://cran.r-project.org/web/packages/bibliometrix/vignettes/Data-Importing-and-Converting.html)
- 'A brief introduction to bibliometrix' (https://cran.r-project.org/web/packages/bibliometrix/vignettes/bibliometrix-vignette.html)\n\n")
    cat("\nMissing fields: ",nolab)
  }
  
  tagsComma <- tagsComma[(!(tagsComma %in% nolab))]
  
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
  
  df$DB <- "ISI"
  
  # Authors
  df$AU <- trimES(gsub(","," ",df$AU))
  
  # Toupper
  DI <- df$DI
  df <- data.frame(lapply(df,toupper),stringsAsFactors = FALSE)
  df$DI <- DI
  
  # add sep ; to affiliations
  df$C1 <- trim(gsub("\\[.*?\\]", "", df$C1)) # to remove author info in square brackets
  df$C1 <- gsub("\\.",".;",df$C1)

  df <- df[names(df)!="Paper"]
  
  return(df)
}

