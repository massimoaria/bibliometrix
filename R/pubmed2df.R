pubmed2df<-function(D){
  
  D <- D[nchar(D)>0]  # remove empty rows
  
  for (i in 1:length(D)){
    if (substr(D[i],1,4)=="    ") substr(D[i],1,4) <- substr(D[i-1],1,4) 
  }
  
  Papers=which(regexpr("PMID-",D)==1)  # first row of each document
  nP=length(Papers)  # number of docuemnts
  
  rowPapers <- diff(c(Papers, length(D)+1))
  
  numPapers <- rep(1:nP,rowPapers)
  
  DATA <- data.frame(Tag = substr(D,1,4), content = substr(D,7,nchar(D)), Paper=numPapers, stringsAsFactors = FALSE)
  DATA$Tag <- gsub(" ","",DATA$Tag)
  df <- DATA %>% group_by(.data$Paper, .data$Tag) %>%
    summarise(cont=paste(.data$content, collapse="---",sep="")) %>%
    arrange(.data$Tag, .data$Paper) %>%
    pivot_wider(names_from =  .data$Tag,values_from = .data$cont) %>%
    ungroup() %>%
    as.data.frame()
  
  # rename field tags
  error <- 0
  old_labs <- c("AD","AUID","FAU","IS","IP","SO","JT","TA","MH","PG","PT","VI","DP")
  new_labs <- c("C1","OI","AF","SN","IS","SO2","SO","J9","DE","PP","DT","VL","PY")
  lab <- names(df)
  for (j in 1:length(old_labs)){
    i <- which(lab %in% old_labs[j])
    if (length(i)>0) {lab[i] <- new_labs[j]}else{error <- 1}
  }
  names(df) <- lab
  if (error == 1){
    cat("\nWarning:\nIn your file, some mandatory metadata are missing. Bibliometrix functions may not work properly!\n
Please, take a look at the vignettes:
- 'Data Importing and Converting' (https://cran.r-project.org/web/packages/bibliometrix/vignettes/Data-Importing-and-Converting.html)
- 'A brief introduction to bibliometrix' (https://cran.r-project.org/web/packages/bibliometrix/vignettes/bibliometrix-vignette.html)\n\n")
  }
  
  # extract DOIs
  df$DI <- trimws(unlist(lapply(strsplit(df$LID,"\\["), "[",1)))
  df$PY <- as.numeric(substr(df$PY,1,4))
  
  
  ### replace "---" with ";"
  tagsComma <- c("AU","AF","DE","AID","OT","PHST","DT")
  nolab <- setdiff(tagsComma,names(df))
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
  
  df$DB <- "PUBMED"
  
  # remove * char from keywords
  df$DE <- df$ID <- gsub("\\*","",df$DE)
  df <- data.frame(lapply(df,toupper),stringsAsFactors = FALSE)
  
  # add sep ; to affiliations
  df$C1 <- gsub("\\.",".;",df$C1)
  df$RP <- NA
  df <- df[names(df)!="Paper"]
  
  return(df)
}

