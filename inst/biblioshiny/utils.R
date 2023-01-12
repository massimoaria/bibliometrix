### COMMON FUNCTIONS ####

getFileNameExtension <- function (fn) {
  # remove a path
  splitted    <- strsplit(x=fn, split='/')[[1]]   
  # or use .Platform$file.sep in stead of '/'
  fn          <- splitted [length(splitted)]
  ext         <- ''
  splitted    <- strsplit(x=fn, split='\\.')[[1]]
  l           <-length (splitted)
  if (l > 1 && sum(splitted[1:(l-1)] != ''))  ext <-splitted [l] 
  # the extention must be the suffix of a non-empty name    
  ext
}

# string preview (stopwords)
strPreview <- function(string, sep=","){
  str1 <- unlist(strsplit(string, sep))
  str1 <- str1[1:min(c(length(str1),5))]
  str1 <- paste(str1, collapse=sep)
  HTML(paste("<pre>", "File Preview: ", str1,"</pre>", sep = '<br/>'))
}

# string preview (synonyms)
strSynPreview <- function(string){
  string <- string[1]
  str1 <- unlist(strsplit(string, ";"))
  str1 <- str1[1:min(c(length(str1),5))]
  str1 <- paste(paste(str1[1], " <- ",collapse=""),paste(str1[-1], collapse=";"), collapse="")
  HTML(paste("<pre>", "File Preview: ", str1,"</pre>", sep = '<br/>'))
}

# from igraph to png file
igraph2PNG <- function(x, filename, width = 10, height = 7, dpi=75){
  V(x)$centr <- centr_betw(x)$res
  df <- data.frame(name=V(x)$label,cluster=V(x)$color, centr=V(x)$centr) %>% 
    group_by(.data$cluster) %>% 
    slice_head(n=3)
  V(x)$label[!(V(x)$label %in% df$name)] <- ""
  png(filename = filename, width = width, height = height, unit="in", res=dpi) 
  grid::grid.draw(plot(x))
  dev.off()
}

# from ggplot to plotly
plot.ly <- function(g, flip=FALSE, side="r", aspectratio=1, size=0.15,data.type=2, height=0, customdata=NA){
  
  g <- g + labs(title=NULL)
  
  gg <- ggplotly(g, tooltip = "text") %>% 
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c(
             'toImage',
             'sendDataToCloud',
             'pan2d', 
             'select2d', 
             'lasso2d',
             'toggleSpikelines',
             'hoverClosestCartesian',
             'hoverCompareCartesian'
           )) 
  
  return(gg)
}

freqPlot <- function(xx,x,y, textLaby,textLabx, title, values){
  
  
  xl <- c(max(xx[,x])-0.02-diff(range(xx[,x]))*0.125, max(xx[,x])-0.02)+1
  yl <- c(1,1+length(unique(xx[,y]))*0.125)
  
  Text <- paste(textLaby,": ",xx[,y],"\n",textLabx, ": ",xx[,x])
  
  g <- ggplot(xx, aes(x =xx[,x], y = xx[,y], label = xx[,x], text=Text)) +
    geom_segment(aes(x = 0, y = xx[,y], xend = xx[,x], yend = xx[,y]), color = "grey50") +
    geom_point(aes(color=-xx[,x], size=xx[,x]), show.legend = FALSE) +
    scale_radius(range=c(5, 12))+
    geom_text(color = "white", size = 3) +
    scale_y_discrete(limits = rev(xx[,y])) +
    scale_fill_continuous(type = "gradient")+
    labs(title=title, y = textLaby)+
    labs(x = textLabx)+
    expand_limits(y= c(1, length(xx[,y]) + 1))+
    theme_minimal()+
    theme(axis.text.y  = element_text(angle=0, hjust=0)) + 
    annotation_custom(values$logoGrid, xmin = xl[1], xmax = xl[2], ymin = yl[1], ymax = yl[2]) 
  
  return(g)
}

emptyPlot<-function(errortext){
  g=ggplot()+
    theme_void() + theme(legend.position="none")+
    annotate("text", x = 4, y = 25, label = errortext, size=10)
  plot(g)
}

count.duplicates <- function(DF){
  x <- do.call('paste', c(DF, sep = '\r'))
  ox <- order(x)
  rl <- rle(x[ox])
  cbind(DF[ox[cumsum(rl$lengths)],,drop=FALSE],count = rl$lengths)
  
}

reduceRefs<- function(A){
  
  ind=unlist(regexec("*V[0-9]", A))
  A[ind>-1]=substr(A[ind>-1],1,(ind[ind>-1]-1))
  ind=unlist(regexec("*DOI ", A))
  A[ind>-1]=substr(A[ind>-1],1,(ind[ind>-1]-1))
  return(A)
}

notifications <- function(){
  
  ## check connection and download notifications
  online <- is_online()
  location <- "https://www.bibliometrix.org/bs_notifications/biblioshiny_notifications.csv"
  notifOnline=NULL
  if (isTRUE(is_online())){
    ## add check to avoid blocked app when internet connection is to slow
    envir <- environment()
    setTimeLimit(cpu = 1, elapsed = 1, transient = TRUE)
    on.exit({
      setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
    })
    tryCatch({
      eval(expr=suppressWarnings(notifOnline <- read.csv(location, header=TRUE, sep=",")), envir = envir)
    }, error = function(ex) {notifOnLine <- NULL}
    )
    if (is.null(notifOnline)){online <- FALSE}else{
      notifOnline$href[nchar(notifOnline$href)<6] <- NA
    }
  }
  
  ## check if a file exists on the local machine and load it
  switch(Sys.info()[['sysname']],
         Windows= {home <- Sys.getenv('R_USER')},
         Linux  = {home <- Sys.getenv('HOME')},
         Darwin = {home <- Sys.getenv('HOME')})
  
  file <- paste(home,"/biblioshiny_notifications.csv", sep="")
  fileTrue <- file.exists(file)
  if (isTRUE(fileTrue)){
    suppressWarnings(notifLocal <- read.csv(file, header=TRUE, sep=","))
    #notifLocal <- readLines(file)
    #linksLocal[nchar(linksLocal)<6] <- NA
  }
  
  
  A <- c("noA","A")
  B <- c("noB","B")
  status <- paste(A[online+1],B[fileTrue+1],sep="")
  
  switch(status,
         # missing both files (online and local)
         noAnoB={
           notifTot <- data.frame(nots="No notifications", href=NA, status="info") %>% mutate(status = "info")
         },
         # missing online file. The local one exists.
         noAB={
           notifTot <- notifLocal %>% filter(.data$action == TRUE) %>% mutate(status = "info")
         },
         # missing the local file. The online one exists.
         AnoB={
           notifOnline <- notifOnline %>% 
             dplyr::slice_head(n=5)
           notifTot <- notifOnline %>% filter(.data$action == TRUE) %>% mutate(status = "danger") 
           notifOnline %>% filter(.data$action == TRUE) %>% write.csv(file=file, quote = FALSE, row.names = FALSE)
         },
         # both files exist.
         AB={
           notifTot <- left_join(notifOnline %>% mutate(status = "danger"),
                                 notifLocal%>% mutate(status = "info"), by="nots") %>% 
             mutate(status = replace_na(.data$status.y,"danger")) %>% 
             rename(href = .data$href.x,
                    action = .data$action.x) %>% 
             select(.data$nots, .data$href, .data$action, .data$status) %>% 
             arrange(.data$status) %>% 
             filter(.data$action == TRUE) %>% 
             dplyr::slice_head(n=5)
           notifTot %>% select(-.data$status) %>% write.csv(file=file, quote = FALSE, row.names = FALSE)   
           
         })
  
  #notifTot <- notifTot[1:(min(5,nrow(notifTot))),]
  return(notifTot)
}

is_online <- function(){
  ## add check to avoid blocked app when internet connection is to slow
  envir <- environment()
  setTimeLimit(cpu = 1, elapsed = 1, transient = TRUE)
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })
  tryCatch({
    eval(expr=suppressWarnings(resp <- curl::has_internet()), envir = envir)
  }, error = function(ex) {resp <- FALSE}
  )
  return(resp)
}

initial <- function(values){
  values$results <- list("NA")
  values$log <- "working..."
  values$load <- "FALSE"
  values$field = values$cocngrams = "NA"
  values$citField = values$colField = values$citSep= "NA"
  values$NetWords = values$NetRefs = values$ColNetRefs=matrix(NA,1,1)
  values$Title <- "Network"
  values$Histfield <- "NA"
  values$histlog <- "working..."
  values$kk <- 0
  values$histsearch <- "NA"
  values$citShortlabel <- "NA"
  values$S <- list("NA")
  values$GR <- "NA"
  
  return(values)
}


### ANALYSIS FUNCTIONS ####
### Descriptive functions ----

ValueBoxes <- function(M){
  # calculate statistics for Biblioshiny ValueBoxes
  df <- data.frame(Description=rep(NA,12), Results=rep(NA,12))
  
  ## VB  1 - Time span
  df[1,] <- c("Timespan",paste(range(M$PY, na.rm=T),collapse=":"))
  
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
  
  DT <- M %>% mutate(DT = tolower(.data$DT)) %>% 
    count(.data$DT) %>% 
    rename(Description = .data$DT,
           Results = .data$n)
  
  # Indexed Keywords (ID)
  ID <- unique(trimws(gsub("\\s+|\\.|\\,"," ",unlist(strsplit(M$ID, ";")))))
  ID <- ID[!is.na(ID)]
  df[nrow(df)+1,] <- c("Keywords Plus (ID)",length(ID))
  
  # Single authored docs
  
  df[nrow(df)+1,] <- c("Single-authored docs",sum(nAU==1))
  
  df2 <- data.frame(Description = c("MAIN INFORMATION ABOUT DATA","Timespan","Sources (Journals, Books, etc)","Documents",
                                    "Annual Growth Rate %","Document Average Age","Average citations per doc","References",
                                    "DOCUMENT CONTENTS","Keywords Plus (ID)","Author's Keywords (DE)","AUTHORS","Authors","Authors of single-authored docs",
                                    "AUTHORS COLLABORATION","Single-authored docs","Co-Authors per Doc","International co-authorships %", "DOCUMENT TYPES"))
  
  df <- left_join(df2,df,by = "Description") %>% rbind(DT) %>% 
    mutate(Results = replace_na(.data$Results, ""))
  
  return(df)
}

countryCollab<-function(M){
  sep=";"
  if (!("AU_CO" %in% names(M))){M=metaTagExtraction(M,Field="AU_CO",sep)}
  if (!("AU1_CO" %in% names(M))){M=metaTagExtraction(M,Field="AU1_CO",sep)}
  
  M$nCO <- as.numeric(unlist(lapply(strsplit(M$AU_CO,";"), function(l){
    length(unique(l))>1
  })))
  
  M$AU1_CO=trim(gsub("[[:digit:]]","",M$AU1_CO))
  M$AU1_CO=gsub("UNITED STATES","USA",M$AU1_CO)
  M$AU1_CO=gsub("RUSSIAN FEDERATION","RUSSIA",M$AU1_CO)
  M$AU1_CO=gsub("TAIWAN","CHINA",M$AU1_CO)
  M$AU1_CO=gsub("ENGLAND","UNITED KINGDOM",M$AU1_CO)
  M$AU1_CO=gsub("SCOTLAND","UNITED KINGDOM",M$AU1_CO)
  M$AU1_CO=gsub("WALES","UNITED KINGDOM",M$AU1_CO)
  M$AU1_CO=gsub("NORTH IRELAND","UNITED KINGDOM",M$AU1_CO)
  
  df <- M %>% group_by(.data$AU1_CO) %>% 
    select(.data$AU1_CO,.data$nCO) %>% 
    summarize(Articles=n(),
              SCP=sum(.data$nCO==0),
              MCP=sum(.data$nCO==1)) %>% 
    rename(Country = .data$AU1_CO) %>% 
    arrange(desc(.data$Articles))
  
  return(df)
}

Hindex_plot <- function(values, type, input){
  
  hindex<-function(values,type,input){
    
    switch(type,
           author={
             AU <- trim(gsub(",","",names(tableTag(values$M,"AU"))))
             values$H <- Hindex(values$M, field = "author", elements = AU, sep = ";", years=Inf)$H %>% 
               arrange(desc(.data$h_index))
           },
           source={
             SO <- names(sort(table(values$M$SO),decreasing = TRUE))
             values$H <- Hindex(values$M, field = "source", elements = SO, sep = ";", years=Inf)$H %>% 
               arrange(desc(.data$h_index))
           }
    )
    
    return(values)
  }
  
  values<-hindex(values, type = type, input)
  
  xx=values$H
  if (type=="author"){
    K=input$Hkauthor
    measure=input$HmeasureAuthors
    title="Authors' Local Impact"
    xn="Authors"
  } else {
    K=input$Hksource
    measure=input$HmeasureSources
    title="Sources' Local Impact"
    xn="Sources"
  }
  if (K>dim(xx)[1]){
    k=dim(xx)[1]
  } else {k=K}
  
  switch(measure,
         h={m=2},
         g={m=3},
         m={m=4
         xx[,m] <-round(xx[,m],2) },
         tc={m=5}
  )
  xx <- xx[order(-xx[,m]),]
  xx <- xx[1:k,c(1,m)]
  
  
  g <- freqPlot(xx,x=2,y=1, textLaby = xn, textLabx = paste("Impact Measure:",toupper(measure)), title = paste(title,"by",toupper(measure),"index"), values)
  
  res<-list(values=values,g=g)
  return(res)
}

descriptive <- function(values,type){
  
  switch(type,
         "tab2"={
           TAB <- values$M %>% group_by(.data$PY) %>% 
             count() %>% 
             rename(Year = .data$PY,
                    Articles = .data$n) %>% 
             right_join(data.frame(Year=seq(min(values$M$PY,na.rm=TRUE),max(values$M$PY, na.rm=TRUE))), by="Year") %>% 
             mutate(Articles = replace_na(.data$Articles,0)) %>% 
             arrange(.data$Year) %>% as.data.frame()
           
           ny=diff(range(TAB$Year))
           values$GR=round(((TAB[nrow(TAB),2]/TAB[1,2])^(1/(ny))-1)*100, digits = 2)
         },
         "tab3"={
           listAU <- (strsplit(values$M$AU, ";"))
           nAU <- lengths(listAU)
           fracAU <- rep(1/nAU,nAU)
           TAB <- tibble(Author=unlist(listAU), fracAU=fracAU) %>% 
             group_by(.data$Author) %>% 
             summarize(
               Articles = n(),
               AuthorFrac = sum(.data$fracAU)
             ) %>% 
             arrange(desc(.data$Articles)) %>% as.data.frame()
           names(TAB)=c("Authors","Articles","Articles Fractionalized")
           #print(S$MostProdAuthors)
         },
         "tab4"={
           y <- as.numeric(substr(Sys.Date(),1,4))
           TAB <- values$M %>% 
             mutate(TCperYear = .data$TC/(y+1-.data$PY)) %>% 
             select(.data$SR,.data$DI, .data$TC, .data$TCperYear, .data$PY) %>% 
             group_by(.data$PY) %>%
             mutate(NTC = .data$TC/mean(.data$TC)) %>%
             ungroup() %>% 
             select(-.data$PY) %>%
             arrange(desc(.data$TC)) %>%
             as.data.frame()
           names(TAB)=c("Paper", "DOI","Total Citations","TC per Year","Normalized TC")
         },
         "tab5"={
           
           TAB <- countryCollab(values$M)
           TAB <- TAB %>% 
             mutate(Freq = .data$Articles/sum(.data$Articles)) %>% 
             mutate(MCP_Ratio = .data$MCP/.data$Articles)
         },
         "tab6"={
           if (!"AU1_CO" %in% names(values$M)){
             values$M <- metaTagExtraction(values$M, "AU1_CO")
           }
           TAB <- values$M %>% 
             select(.data$AU1_CO, .data$TC) %>% 
             drop_na(.data$AU1_CO) %>% 
             rename(Country = .data$AU1_CO,
                    TotalCitation = .data$TC) %>% 
             group_by(.data$Country) %>% 
             summarise("TC"=sum(.data$TotalCitation),"Average Article Citations"=sum(.data$TotalCitation)/length(.data$TotalCitation)) %>%
             arrange(-.data$TC) %>% as.data.frame(.data,stringasfactor=FALSE)
         },
         "tab7"={
           TAB <- values$M %>% 
             select(.data$SO) %>% 
             group_by(.data$SO) %>% 
             count() %>% 
             arrange(desc(.data$n)) %>% 
             rename(Sources = .data$SO,
                    Articles = .data$n) %>% 
             as.data.frame()
         },
         
         "tab10"={
           TAB<-mapworld(values$M)$tab
         },
         "tab11"={
           if(!("AU_UN" %in% names(values$M))){values$M=metaTagExtraction(values$M,Field="AU_UN")}
           TAB <- data.frame(Affiliation=unlist(strsplit(values$M$AU_UN, ";"))) %>% 
             group_by(.data$Affiliation) %>% 
             count() %>% 
             drop_na(.data$Affiliation) %>% 
             arrange(desc(.data$n)) %>% 
             rename(Articles = .data$n) %>% 
             as.data.frame()
         },
         "tab12"={
           TAB=tableTag(values$M,"C1")
           TAB=data.frame(Affiliations=names(TAB), Articles=as.numeric(TAB),stringsAsFactors = FALSE)
           TAB=TAB[nchar(TAB[,1])>4,]
           #names(TAB)=c("Affiliations", "Articles")
           
         },
         "tab13"={
           CR<-localCitations(values$M,fast.search = FALSE, verbose = FALSE)
           TAB <- CR$Authors
           #TAB=data.frame(Authors=names(CR$Authors$Author), Citations=as.numeric(CR$Cited),stringsAsFactors = FALSE)
         }
  )
  values$TAB=TAB
  res=list(values=values,TAB=TAB)
  return(res)
}

AffiliationOverTime <- function(values,n){
  if(!("AU_UN" %in% names(values$M))){values$M=metaTagExtraction(values$M,Field="AU_UN")}
  AFF <- strsplit(values$M$AU_UN, ";")
  nAFF <- lengths(AFF)
  
  AFFY <- data.frame(Affiliation=unlist(AFF),Year=rep(values$M$PY,nAFF)) %>% 
    drop_na(.data$Affiliation,.data$Year) %>% 
    group_by(.data$Affiliation, .data$Year) %>% 
    count() %>% 
    group_by(.data$Affiliation) %>% 
    arrange(.data$Year) %>% 
    ungroup() %>% 
    pivot_wider(.data$Affiliation, names_from = .data$Year, values_from = .data$n) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    pivot_longer(cols = !Affiliation, names_to = "Year", values_to = "Articles") %>% 
    group_by(.data$Affiliation) %>% 
    mutate(Articles = cumsum(.data$Articles))
  
  Affselected <- AFFY %>% 
    filter(.data$Year == max(.data$Year)) %>% 
    ungroup() %>% 
    slice_max(.data$Articles, n=n)
  
  values$AffOverTime <- AFFY %>% 
    filter(.data$Affiliation %in% Affselected$Affiliation) %>% 
    mutate(Year = .data$Year %>% as.numeric())
  
  Text <- paste(values$AffOverTime$Affiliation," (",values$AffOverTime$Year,") ",values$AffOverTime$Articles, sep="")
  width_scale <- 1.7 * 26 / length(unique(values$AffOverTime$Affiliation))
  x <- c(max(values$AffOverTime$Year)-0.02-diff(range(values$AffOverTime$Year))*0.15, max(values$AffOverTime$Year)-0.02)+1
  y <- c(min(values$AffOverTime$Articles),min(values$AffOverTime$Articles)+diff(range(values$AffOverTime$Articles))*0.15)
  
  
  values$AffOverTimePlot <- ggplot(values$AffOverTime, aes(x=.data$Year,y=.data$Articles, group=.data$Affiliation, color=.data$Affiliation, text=Text))+
    geom_line()+
    labs(x = 'Year'
         , y = "Articles"
         , title = "Affiliations' Production over Time") +
    scale_x_continuous(breaks= (values$AffOverTime$Year[seq(1,length(values$AffOverTime$Year),by=ceiling(length(values$AffOverTime$Year)/20))])) +
    geom_hline(aes(yintercept=0), alpha=0.1)+
    labs(color = "Affiliation")+
    theme(text = element_text(color = "#444444"),
          legend.text=ggplot2::element_text(size=width_scale),
          legend.box.margin = margin(6, 6, 6, 6),
          legend.title=ggplot2::element_text(size=1.5*width_scale,face="bold"),
          legend.position="bottom",
          legend.direction = "vertical",
          legend.key.size = grid::unit(width_scale/50, "inch"),
          legend.key.width = grid::unit(width_scale/50, "inch")
          ,plot.caption = element_text(size = 9, hjust = 0.5, color = "black", face = "bold")
          ,panel.background = element_rect(fill = '#FFFFFF')
          ,panel.grid.minor = element_line(color = '#EFEFEF')
          ,panel.grid.major = element_line(color = '#EFEFEF')
          ,plot.title = element_text(size = 24)
          ,axis.title = element_text(size = 14, color = '#555555')
          ,axis.title.y = element_text(vjust = 1, angle = 90)
          ,axis.title.x = element_text(hjust = 0.95, angle = 0)
          ,axis.text.x = element_text(size=10, angle = 90)
          ,axis.line.x = element_line(color="black",size=0.5)
          ,axis.line.y = element_line(color="black",size=0.5)
    ) + annotation_custom(values$logoGrid, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
  return(values)
}

CountryOverTime <- function(values,n){
  if(!("AU_CO" %in% names(values$M))){values$M=metaTagExtraction(values$M,Field="AU_CO")}
  AFF <- strsplit(values$M$AU_CO, ";")
  nAFF <- lengths(AFF)
  
  AFFY <- data.frame(Affiliation=unlist(AFF),Year=rep(values$M$PY,nAFF)) %>% 
    drop_na(.data$Affiliation,.data$Year) %>% 
    group_by(.data$Affiliation, .data$Year) %>% 
    count() %>% 
    group_by(.data$Affiliation) %>% 
    arrange(.data$Year) %>% 
    ungroup() %>% 
    pivot_wider(.data$Affiliation, names_from = .data$Year, values_from = .data$n) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    pivot_longer(cols = !Affiliation, names_to = "Year", values_to = "Articles") %>% 
    group_by(.data$Affiliation) %>% 
    mutate(Articles = cumsum(.data$Articles))
  
  Affselected <- AFFY %>% 
    filter(.data$Year == max(.data$Year)) %>% 
    ungroup() %>% 
    slice_max(.data$Articles, n=n)
  
  values$CountryOverTime <- AFFY %>% 
    filter(.data$Affiliation %in% Affselected$Affiliation) %>% 
    mutate(Year = .data$Year %>% as.numeric()) %>% 
    rename(Country = .data$Affiliation)
  
  Text <- paste(values$CountryOverTime$Country," (",values$CountryOverTime$Year,") ",values$CountryOverTime$Articles, sep="")
  width_scale <- 1.7 * 26 / length(unique(values$CountryOverTime$Country))
  x <- c(max(values$CountryOverTime$Year)-0.02-diff(range(values$CountryOverTime$Year))*0.15, max(values$CountryOverTime$Year)-0.02)+1
  y <- c(min(values$CountryOverTime$Articles),min(values$CountryOverTime$Articles)+diff(range(values$CountryOverTime$Articles))*0.15)
  
  
  values$CountryOverTimePlot <- ggplot(values$CountryOverTime, aes(x=.data$Year,y=.data$Articles, group=.data$Country, color=.data$Country, text=Text))+
    geom_line()+
    labs(x = 'Year'
         , y = "Articles"
         , title = "Country Production over Time") +
    scale_x_continuous(breaks= (values$CountryOverTime$Year[seq(1,length(values$CountryOverTime$Year),by=ceiling(length(values$CountryOverTime$Year)/20))])) +
    geom_hline(aes(yintercept=0), alpha=0.1)+
    labs(color = "Country")+
    theme(text = element_text(color = "#444444"),
          legend.text=ggplot2::element_text(size=width_scale),
          legend.box.margin = margin(6, 6, 6, 6),
          legend.title=ggplot2::element_text(size=1.5*width_scale,face="bold"),
          legend.position="bottom",
          legend.direction = "vertical",
          legend.key.size = grid::unit(width_scale/50, "inch"),
          legend.key.width = grid::unit(width_scale/50, "inch")
          ,plot.caption = element_text(size = 9, hjust = 0.5, color = "black", face = "bold")
          ,panel.background = element_rect(fill = '#FFFFFF')
          ,panel.grid.minor = element_line(color = '#EFEFEF')
          ,panel.grid.major = element_line(color = '#EFEFEF')
          ,plot.title = element_text(size = 24)
          ,axis.title = element_text(size = 14, color = '#555555')
          ,axis.title.y = element_text(vjust = 1, angle = 90)
          ,axis.title.x = element_text(hjust = 0.95, angle = 0)
          ,axis.text.x = element_text(size=10, angle = 90)
          ,axis.line.x = element_line(color="black",size=0.5)
          ,axis.line.y = element_line(color="black",size=0.5)
    ) + annotation_custom(values$logoGrid, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
  return(values)
}

wordlist <- function(M, Field, n, measure, ngrams, remove.terms=NULL, synonyms=NULL){
  switch(Field,
         ID={v=tableTag(M,"ID", remove.terms  = remove.terms, synonyms = synonyms)},
         DE={v=tableTag(M,"DE", remove.terms = remove.terms, synonyms = synonyms)},
         TI={
           if (!("TI_TM" %in% names(M))){
             v=tableTag(M,"TI", ngrams=ngrams, remove.terms=remove.terms, synonyms = synonyms)
             
           }},
         AB={if (!("AB_TM" %in% names(M))){
           v=tableTag(M,"AB", ngrams=ngrams, remove.terms = remove.terms, synonyms = synonyms)
         }},
         WC={
           v=tableTag(M,"WC")
         }
  )
  names(v)=tolower(names(v))
  #v=tableTag(values$M,"ID")
  n=min(c(n,length(v)))
  Words=data.frame(Terms=names(v)[1:n], Frequency=(as.numeric(v)[1:n]), stringsAsFactors = FALSE)
  W=Words
  switch(measure,
         identity={},
         sqrt={W$Frequency=sqrt(W$Frequency)},
         log={W$Frequency=log(W$Frequency+1)},
         log10={W$Frequency=log10(W$Frequency+1)}
  )
  
  results=list(v=v,W=W, Words=Words)
  return(results)
}

readStopwordsFile <- function(file, sep=","){
  if (!is.null(file)){
    req(file$datapath)
    remove.terms <- unlist(strsplit(readr::read_lines(file$datapath), sep))
  }else{remove.terms <- NULL}
  return(remove.terms)
}

readSynWordsFile <- function(file, sep=","){
  if (!is.null(file)){
    req(file$datapath)
    syn.terms <- readr::read_lines(file$datapath)
    if (sep!=";") syn.terms <- gsub(sep,";",syn.terms)
  }else{syn.terms <- NULL}
  return(syn.terms)
}

mapworld <- function(M, values){
  if (!("AU_CO" %in% names(M))){M=metaTagExtraction(M,"AU_CO")}
  CO=as.data.frame(tableTag(M,"AU_CO"),stringsAsFactors = FALSE)
  CO$Tab=gsub("[[:digit:]]","",CO$Tab)
  CO$Tab=gsub(".", "", CO$Tab, fixed = TRUE)
  CO$Tab=gsub(";;", ";", CO$Tab, fixed = TRUE)
  CO$Tab=gsub("UNITED STATES","USA",CO$Tab)
  CO$Tab=gsub("RUSSIAN FEDERATION","RUSSIA",CO$Tab)
  CO$Tab=gsub("TAIWAN","CHINA",CO$Tab)
  CO$Tab=gsub("ENGLAND","UNITED KINGDOM",CO$Tab)
  CO$Tab=gsub("SCOTLAND","UNITED KINGDOM",CO$Tab)
  CO$Tab=gsub("WALES","UNITED KINGDOM",CO$Tab)
  CO$Tab=gsub("NORTH IRELAND","UNITED KINGDOM",CO$Tab)
  CO$Tab=gsub("UNITED KINGDOM","UK",CO$Tab)
  CO$Tab=gsub("KOREA","SOUTH KOREA",CO$Tab)
  
  
  map.world <- map_data("world")
  map.world$region=toupper(map.world$region)
  
  dplyr::anti_join(CO, map.world, by = c('Tab' = 'region'))
  
  country.prod <- dplyr::left_join( map.world, CO, by = c('region' = 'Tab')) 
  
  tab=data.frame(country.prod %>%
                   dplyr::group_by(.data$region) %>%
                   dplyr::summarise(Freq=mean(.data$Freq)))
  
  tab=tab[!is.na(tab$Freq),]
  
  tab=tab[order(-tab$Freq),]
  
  breaks=as.numeric(round(quantile(CO$Freq,c(0.2,0.4,0.6,0.8,1))))
  names(breaks)=breaks
  breaks=log(breaks)
  
  g <- ggplot(country.prod, aes( x = .data$long, y = .data$lat, group=.data$group, text=paste("Country: ",.data$region,"\nN.of Documents: ",.data$Freq))) +
    geom_polygon(aes(fill = log(.data$Freq), group=.data$group)) +
    scale_fill_continuous(low='dodgerblue', high='dodgerblue4',breaks=breaks)+
    guides(fill = guide_legend(reverse = T)) +
    #geom_text(data=centroids, aes(label = centroids$Tab, x = centroids$long, y = centroids$lat, group=centroids$Tab)) +
    labs(fill = 'N.Documents'
         ,title = 'Country Scientific Production'
         ,x = NULL
         ,y = NULL) +
    theme(text = element_text(color = '#333333')
          ,plot.title = element_text(size = 28)
          ,plot.subtitle = element_text(size = 14)
          ,axis.ticks = element_blank()
          ,axis.text = element_blank()
          ,panel.grid = element_blank()
          ,panel.background = element_rect(fill = '#FFFFFF')  #'#333333'
          ,plot.background = element_rect(fill = '#FFFFFF')
          ,legend.position = c(.18,.36)
          ,legend.background = element_blank()
          ,legend.key = element_blank()
    ) + annotation_custom(values$logoGrid, xmin = 143, xmax = 189.5, ymin = -69, ymax = -48) 
  
  results=list(g=g,tab=tab)
  return(results)
}

### Structure fuctions ----
CAmap <- function(input, values){
  if ((input$CSfield %in% names(values$M))){
    
    if (input$CSfield %in% c("TI","AB")){
      ngrams <- as.numeric(input$CSngrams)
    }else{
      ngrams <- 1
    }
    
    ### load file with terms to remove
    if (input$CSStopFile=="Y"){
      remove.terms <- trimws(readStopwordsFile(file=input$CSStop, sep=input$CSSep))
    }else{remove.terms <- NULL}
    values$CSremove.terms <- remove.terms
    ### end of block
    ### load file with synonyms
    if (input$FASynFile=="Y"){
      synonyms <- trimws(readSynWordsFile(file=input$FASyn, sep=input$FASynSep))
    }else{synonyms <- NULL}
    values$FAsyn.terms <- synonyms
    ### end of block
    
    tab=tableTag(values$M,input$CSfield, ngrams=ngrams)
    if (length(tab>=2)){
      
      minDegree=as.numeric(tab[input$CSn])
      
      values$CS <- conceptualStructure(values$M, method=input$method , field=input$CSfield, minDegree=minDegree, clust=input$nClustersCS, 
                                       k.max = 8, stemming=F, labelsize=input$CSlabelsize,documents=input$CSdoc,graph=FALSE, ngrams=ngrams, 
                                       remove.terms=remove.terms, synonyms = synonyms)
      
      CSData=values$CS$docCoord
      CSData=data.frame(Documents=row.names(CSData),CSData,stringsAsFactors = FALSE)
      CSData$dim1=round(CSData$dim1,2)
      CSData$dim2=round(CSData$dim2,2)
      CSData$contrib=round(CSData$contrib,2)
      values$CS$CSData <- CSData
      
      switch(input$method,
             CA={
               WData=data.frame(word=row.names(values$CS$km.res$data.clust), values$CS$km.res$data.clust, 
                                stringsAsFactors = FALSE)
               names(WData)[4]="cluster"
             },
             MCA={
               WData=data.frame(word=row.names(values$CS$km.res$data.clust), values$CS$km.res$data.clust, 
                                stringsAsFactors = FALSE)
               names(WData)[4]="cluster"
             },
             MDS={
               WData=data.frame(word=row.names(values$CS$res), values$CS$res, 
                                cluster=values$CS$km.res$cluster,stringsAsFactors = FALSE)
             })
      
      WData$Dim.1=round(WData$Dim.1,2)
      WData$Dim.2=round(WData$Dim.2,2)
      values$CS$WData <- WData
      
    }else{emptyPlot("Selected field is not included in your data collection")
      values$CS=list("NA")}
    
  }else{
    emptyPlot("Selected field is not included in your data collection")
    values$CS=list("NA")
    
  }
}

historiograph <- function(input,values){
  
  min.cit <- 1

  #if (values$Histfield=="NA"){
    values$histResults <- histNetwork(values$M, min.citations=min.cit, sep = ";")
  # values$Histfield="done"
  #}
  
  #titlelabel <- input$titlelabel
  values$histlog<- (values$histPlot <- histPlot(values$histResults, n=input$histNodes, size =input$histsize, labelsize = input$histlabelsize, label = input$titlelabel, verbose=FALSE))
  values$histResults$histData$DOI<- paste0('<a href=\"https://doi.org/',values$histResults$histData$DOI,'\" target=\"_blank\">',values$histResults$histData$DOI,'</a>')
  values$histResults$histData <- values$histResults$histData %>% 
    left_join(
      values$histPlot$layout %>% 
        select(.data$name,.data$color), by= c("Paper" = "name")
    ) %>% 
    drop_na(.data$color) %>% 
    mutate(cluster = match(.data$color,unique(.data$color))) %>% 
    select(!.data$color) %>% 
    group_by(.data$cluster) %>% 
    arrange(.data$Year, .by_group = TRUE)
  return(values)
}


### Network functions ----
degreePlot <- function(net){
  #deg <- data.frame(node = names(net$nodeDegree), x= (1:length(net$nodeDegree)), y = net$nodeDegree)
  ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}
  
  deg <- net$nodeDegree %>% 
    mutate(x = row_number())
 
  p <- ggplot(data = deg, aes(x=.data$x, y=.data$degree, 
                              text=paste(.data$node," - Degree ",round(.data$degree,3), sep="")))+
    geom_point()+
    geom_line(aes(group="NA"),color = '#002F80', alpha = .5) +
    #geom_hline(yintercept=cutting$degree, linetype="dashed",color = '#002F80', alpha = .5)+
    theme(text = element_text(color = "#444444")
          ,panel.background = element_rect(fill = '#FFFFFF')
          ,panel.grid.minor = element_line(color = '#EFEFEF')
          ,panel.grid.major = element_line(color = '#EFEFEF')
          ,plot.title = element_text(size = 24)
          ,axis.title = element_text(size = 14, color = '#555555')
          ,axis.title.y = element_text(vjust = 1, angle = 0)
          ,axis.title.x = element_text(hjust = 0)
          ,axis.line.x = element_line(color="black",size=0.5)
          ,axis.line.y = element_line(color="black",size=0.5)
    ) +
    labs(x = "Node", y="Cumulative Degree", title = "Node Degrees")
  return(p)
}

cocNetwork <- function(input,values){
  
  n = input$Nodes
  label.n = input$Labels
  
  ### load file with terms to remove
  if (input$COCStopFile=="Y"){
    remove.terms <- trimws(readStopwordsFile(file=input$COCStop, sep=input$COCSep))
  }else{remove.terms <- NULL}
  values$COCremove.terms <- remove.terms
  ### end of block
  ### load file with synonyms
  if (input$COCSynFile=="Y"){
    synonyms <- trimws(readSynWordsFile(file=input$COCSyn, sep=input$COCSynSep))
  }else{synonyms <- NULL}
  values$COCsyn.terms <- synonyms
  ### end of block
  
  if ((input$field %in% names(values$M))){
    
    if ((dim(values$NetWords)[1])==1 | !(input$field==values$field) | !(input$cocngrams==values$cocngrams) | ((dim(values$NetWords)[1])!=input$Nodes) ){
      
      values$field=input$field
      values$ngrams <- input$cocngrams
      
      switch(input$field,
             ID={
               values$NetWords <- biblioNetwork(values$M, analysis = "co-occurrences", network = "keywords", n = n, sep = ";", remove.terms=remove.terms, synonyms = synonyms)
               values$Title= "Keywords Plus Network"
             },
             DE={
               values$NetWords <- biblioNetwork(values$M, analysis = "co-occurrences", network = "author_keywords", n = n, sep = ";", remove.terms=remove.terms, synonyms = synonyms)
               values$Title= "Authors' Keywords network"
             },
             TI={
               #if(!("TI_TM" %in% names(values$M))){
               values$M=termExtraction(values$M,Field="TI",verbose=FALSE, ngrams=as.numeric(input$cocngrams), remove.terms=remove.terms, synonyms = synonyms)
               #}
               values$NetWords <- biblioNetwork(values$M, analysis = "co-occurrences", network = "titles", n = n, sep = ";")
               values$Title= "Title Words network"
             },
             AB={
               #if(!("AB_TM" %in% names(values$M))){
               values$M=termExtraction(values$M,Field="AB",verbose=FALSE, ngrams=as.numeric(input$cocngrams), remove.terms=remove.terms, synonyms = synonyms)
               #}
               values$NetWords <- biblioNetwork(values$M, analysis = "co-occurrences", network = "abstracts", n = n, sep = ";")
               values$Title= "Abstract Words network"
             },
             WC={
               WSC <- cocMatrix(values$M, Field="WC", binary=FALSE)
               values$NetWords <- crossprod(WSC,WSC)
               values$Title= "Subject Categories network"
             })
      
    }
    
    if (label.n>n){label.n=n}
    if (input$normalize=="none"){normalize=NULL}else{normalize=input$normalize}
    if (input$label.cex=="Yes"){label.cex=TRUE}else{label.cex=FALSE}
    if (input$coc.curved=="Yes"){curved=TRUE}else{curved=FALSE}
    
    #par(bg="grey92", mar=c(0,0,0,0))
    values$cocnet=networkPlot(values$NetWords, normalize=normalize, Title = values$Title, type = input$layout, 
                              size.cex=TRUE, size=5 , remove.multiple=F, edgesize = input$edgesize*3, labelsize=input$labelsize,label.cex=label.cex,
                              label.n=label.n,edges.min=input$edges.min,label.color = F, curved=curved,alpha=input$cocAlpha,
                              cluster=input$cocCluster, remove.isolates = (input$coc.isolates=="yes"), 
                              community.repulsion = input$coc.repulsion/2, verbose = FALSE)
    if (input$cocyears=="Yes"){
      Y=fieldByYear(values$M, field = input$field, graph=FALSE)
      g=values$cocnet$graph
      label=igraph::V(g)$name
      ind=which(tolower(Y$df$item) %in% label)
      df=Y$df[ind,]

      col=hcl.colors((diff(range(df$year_med))+1)*10, palette="Blues 3")
      igraph::V(g)$color=col[(max(df$year_med)-df$year_med+1)*10]
      igraph::V(g)$year_med=df$year_med
      values$cocnet$graph=g
    }
    
  }else{
    emptyPlot("Selected field is not included in your data collection")
  }
  return(values)
}

intellectualStructure <- function(input,values){
  n = input$citNodes
  label.n = input$citLabels
  
  if ((dim(values$NetRefs)[1])==1 | !(input$citField==values$citField) | !(input$citSep==values$citSep) | !(input$citShortlabel==values$citShortlabel) | ((dim(values$NetRefs)[1])!=input$citNodes)){
    
    values$citField=input$citField
    values$citSep=input$citSep
    if (input$citShortlabel=="Yes"){shortlabel=TRUE}else{shortlabel=FALSE}
    values$citShortlabel=input$citShortlabel
    switch(input$citField,
           CR={
             values$NetRefs <- biblioNetwork(values$M, analysis = "co-citation", network = "references", n = n, sep = input$citSep, shortlabel=shortlabel)
             values$Title= "Cited References network"
             
           },
           CR_AU={
             if(!("CR_AU" %in% names(values$M))){values$M=metaTagExtraction(values$M,Field="CR_AU", sep = input$citSep)}
             values$NetRefs <- biblioNetwork(values$M, analysis = "co-citation", network = "authors", n = n, sep = input$citSep)
             values$Title= "Cited Authors network"
           },
           CR_SO={
             if(!("CR_SO" %in% names(values$M))){values$M=metaTagExtraction(values$M,Field="CR_SO", sep = input$citSep)}
             values$NetRefs <- biblioNetwork(values$M, analysis = "co-citation", network = "sources", n = n, sep = input$citSep)
             values$Title= "Cited Sources network"
           })
    
  }
  
  if (label.n>n){label.n=n}
  if (input$citlabel.cex=="Yes"){label.cex=TRUE}else{label.cex=FALSE}
  if (input$cocit.curved=="Yes"){curved=TRUE}else{curved=FALSE}
  
  values$cocitnet=networkPlot(values$NetRefs, normalize=NULL, Title = values$Title, type = input$citlayout, 
                              size.cex=TRUE, size=5 , remove.multiple=F, edgesize = input$citedgesize*3, 
                              labelsize=input$citlabelsize,label.cex=label.cex, curved=curved,
                              label.n=label.n,edges.min=input$citedges.min,label.color = F,remove.isolates = (input$cit.isolates=="yes"),
                              alpha=0.7, cluster=input$cocitCluster, 
                              community.repulsion = input$cocit.repulsion/2, verbose = FALSE)
  return(values)
}

socialStructure<-function(input,values){
  n = input$colNodes
  label.n = input$colLabels
  
  if ((dim(values$ColNetRefs)[1])==1 | !(input$colField==values$colField) | ((dim(values$ColNetRefs)[1])!=input$colNodes)){
    
    values$colField=input$colField
    
    
    values$cluster="walktrap"
    switch(input$colField,
           COL_AU={
             values$ColNetRefs <- biblioNetwork(values$M, analysis = "collaboration", network = "authors", n = n, sep = ";")
             values$Title= "Author Collaboration network"
             
           },
           COL_UN={
             if(!("AU_UN" %in% names(values$M))){values$M=metaTagExtraction(values$M,Field="AU_UN", sep=";")}
             values$ColNetRefs <- biblioNetwork(values$M, analysis = "collaboration", network = "universities", n = n, sep = ";")
             values$Title= "Edu Collaboration network"
           },
           COL_CO={
             if(!("AU_CO" %in% names(values$M))){values$M=metaTagExtraction(values$M,Field="AU_CO", sep=";")}
             values$ColNetRefs <- biblioNetwork(values$M, analysis = "collaboration", network = "countries", n = n, sep = ";")
             values$Title= "Country Collaboration network"
             #values$cluster="none"
           })
    
  }
  
  if (label.n>n){label.n=n}
  if (input$colnormalize=="none"){normalize=NULL}else{normalize=input$colnormalize}
  if (input$collabel.cex=="Yes"){label.cex=TRUE}else{label.cex=FALSE}
  if (input$soc.curved=="Yes"){curved=TRUE}else{curved=FALSE}
  
  type=input$collayout
  if (input$collayout=="worldmap"){type="auto"}
  
  values$colnet=networkPlot(values$ColNetRefs, normalize=normalize, Title = values$Title, type = type, 
                            size.cex=TRUE, size=5 , remove.multiple=F, edgesize = input$coledgesize*3, 
                            labelsize=input$collabelsize,label.cex=label.cex, curved=curved,
                            label.n=label.n,edges.min=input$coledges.min,label.color = F,alpha=input$colAlpha,
                            remove.isolates = (input$col.isolates=="yes"), cluster=input$colCluster, 
                            community.repulsion = input$col.repulsion/2, verbose = FALSE)
  
  return(values)
  
}

countrycollaboration <- function(M,label,edgesize,min.edges, values){
  M=metaTagExtraction(M,"AU_CO")
  net=biblioNetwork(M,analysis="collaboration",network="countries")
  CO=data.frame(Tab=rownames(net),Freq=diag(net),stringsAsFactors = FALSE)
  bsk.network=igraph::graph_from_adjacency_matrix(net,mode="undirected")
  COedges=as.data.frame(igraph::ends(bsk.network,igraph::E(bsk.network),names=TRUE),stringsAsFactors = FALSE)
  
  map.world <- map_data("world")
  map.world$region=toupper(map.world$region)
  map.world$region=gsub("UK","UNITED KINGDOM",map.world$region)
  map.world$region=gsub("SOUTH KOREA","KOREA",map.world$region)
  
  country.prod <- dplyr::left_join( map.world, CO, by = c('region' = 'Tab')) 
  
  breaks=as.numeric(round(quantile(CO$Freq,c(0.2,0.4,0.6,0.8,1))))
  names(breaks)=breaks
  breaks=log(breaks)
  data("countries",envir=environment())
  names(countries)[1]="Tab"
  
  COedges=dplyr::inner_join(COedges,countries, by=c('V1'='Tab'))
  COedges=dplyr::inner_join(COedges,countries, by=c('V2'='Tab'))
  COedges=COedges[COedges$V1!=COedges$V2,]
  COedges=count.duplicates(COedges)
  tab=COedges
  COedges=COedges[COedges$count>=min.edges,]
  
  g=ggplot(country.prod, aes( x = .data$long, y = .data$lat, group = .data$group )) +
    geom_polygon(aes(fill = log(.data$Freq))) +
    scale_fill_continuous(low='dodgerblue', high='dodgerblue4',breaks=breaks)+
    #guides(fill = guide_legend(reverse = T)) +
    guides(colour=FALSE, fill=FALSE)+
    geom_curve(data=COedges, aes(x = .data$Longitude.x , y = .data$Latitude.x, xend = .data$Longitude.y, yend = .data$Latitude.y,     # draw edges as arcs
                                 color = "firebrick4", size = .data$count, group=.data$continent.x),
               curvature = 0.33,
               alpha = 0.5) +
    scale_size_continuous(guide = FALSE, range = c(0.25, edgesize))+
    labs(title = NULL, x = "Latitude", y = "Longitude") +
    theme(text = element_text(color = '#333333')
          ,plot.title = element_text(size = 28)
          ,plot.subtitle = element_text(size = 14)
          ,axis.ticks = element_blank()
          ,axis.text = element_blank()
          ,panel.grid = element_blank()
          ,panel.background = element_rect(fill = '#FFFFFF')  #'#333333'
          ,plot.background = element_rect(fill = '#FFFFFF')
          ,legend.position = c(.18,.36)
          ,legend.background = element_blank()
          ,legend.key = element_blank()
    ) + annotation_custom(values$logoGrid, xmin = 143, xmax = 189.5, ymin = -69, ymax = -48) 
  if (isTRUE(label)){
    CO=dplyr::inner_join(CO,countries, by=c('Tab'='Tab'))
    g=g+
      ggrepel::geom_text_repel(data=CO, aes(x = .data$Longitude, y = .data$Latitude, label = .data$Tab, group=.data$continent),             # draw text labels
                               hjust = 0, nudge_x = 1, nudge_y = 4,
                               size = 3, color = "orange", fontface = "bold")
  }
  
  results=list(g=g,tab=tab)
  return(results)
}
### visNetwork tools ----
netLayout <- function(type){
  switch(type,
         auto={l <- "layout_nicely"},
         circle={l <- "layout_in_circle"},
         mds={l <- "layout_with_mds"},
         star={l <- "layout_as_star"},
         
         sphere={l <- "layout_on_sphere"},
         fruchterman={l <- "layout_with_fr"},
         kamada={l <- "layout_with_kk"}
  )
  return(l)
}

savenetwork <- function(con, values){
  
  values$network$VIS %>% 
    visOptions(height = "800px") %>% 
    visNetwork::visSave(con)
}

igraph2vis<-function(g,curved,labelsize,opacity,type,shape, net, shadow=TRUE, edgesize=5){
  
  LABEL=igraph::V(g)$name
  
  LABEL[igraph::V(g)$labelsize==0]=""
  
  vn <- visNetwork::toVisNetworkData(g)
  
  vn$nodes$label=LABEL
  vn$edges$num=1
  vn$edges$dashes=FALSE
  vn$edges$dashes[vn$edges$lty==2]=TRUE
  
  ## opacity
  vn$nodes$color=adjustcolor(vn$nodes$color,alpha.f=min(c(opacity,1)))
  ## set a darkest gray for iter-cluster edges
  vn$edges$color <- paste(substr(vn$edges$color,1,7),"90",sep="")
  vn$edges$color[substr(vn$edges$color,1,7)=="#B3B3B3"] <- "#69696960"
    vn$edges$color <- adjustcolor(vn$edges$color,alpha.f=opacity)
    
    ## removing multiple edges
    vn$edges <- unique(vn$edges)

    vn$edges$width <- vn$edges$width^2/(max(vn$edges$width^2))*(10+edgesize)

    # if (edgesize==0){
    #   vn$edges$hidden <- TRUE
    #   }else{vn$edges$hidden <- FALSE}
        
    ## labelsize
    vn$nodes$font.size <- vn$nodes$deg
    scalemin <- 20
    scalemax <- 150
    Min <- min(vn$nodes$font.size)
    Max <- max(vn$nodes$font.size)
    if (Max>Min){
      size=(vn$nodes$font.size-Min)/(Max-Min)*15*labelsize+10
    } else {size=10*labelsize}
    size[size<scalemin]=scalemin
    size[size>scalemax]=scalemax
    vn$nodes$font.size=size
    l<-netLayout(type)
    
    ### TO ADD SHAPE AND FONT COLOR OPTIONS
    coords <- net$layout
    
    vn$nodes$size <- vn$nodes$font.size*0.7
    
    #vn$nodes$font.color <- adjustcolor("black", alpha.f = min(c(opacity,1)))
    
    if (shape %in% c("dot","square")){
      vn$nodes$font.vadjust <- -0.7*vn$nodes$font.size
    }else{
      vn$nodes$font.vadjust <-0
    }
    
    opacity_font <- sqrt((vn$nodes$font.size-min(vn$nodes$font.size))/diff(range(vn$nodes$font.size)))*opacity+0.3
    if(is.nan(opacity_font[1])) opacity_font <- rep(0.3,length(opacity_font))
    
    if (labelsize>0){
      vn$nodes$font.color <- unlist(lapply(opacity_font, function(x) adjustcolor("black",alpha.f = x)))
    }else{
        vn$nodes$font.color <- adjustcolor("black", alpha.f = 0)
        }

    VIS<-
      visNetwork::visNetwork(nodes = vn$nodes, edges = vn$edges, type="full", smooth=TRUE, physics=FALSE) %>%
      visNetwork::visNodes(shadow=shadow, shape=shape, font=list(color=vn$nodes$font.color, size=vn$nodes$font.size,vadjust=vn$nodes$font.vadjust)) %>%
      visNetwork::visIgraphLayout(layout = "layout.norm", layoutMatrix = coords, type = "full") %>%
      visNetwork::visEdges(smooth = list(type="horizontal")) %>%
      visNetwork::visOptions(highlightNearest =list(enabled = T, hover = T, degree=1), nodesIdSelection = T) %>%
      visNetwork::visInteraction(dragNodes = TRUE, navigationButtons = F, hideEdgesOnDrag = TRUE) %>%
      visNetwork::visOptions(manipulation = curved, height ="100%", width = "100%")
    
    return(list(VIS=VIS,vn=vn, type=type, l=l, curved=curved))
}

## visnetwork for subgraphs
igraph2visClust<-function(g,curved=FALSE,labelsize=3,opacity=0.7,shape="dot",shadow=TRUE, edgesize=5){
  
  LABEL=igraph::V(g)$name
  
  LABEL[igraph::V(g)$labelsize==0]=""
  
  vn <- visNetwork::toVisNetworkData(g)
  
  vn$nodes$label=LABEL
  vn$edges$num=1
  vn$edges$dashes=FALSE
  vn$edges$dashes[vn$edges$lty==2]=TRUE
  
  ## opacity
  vn$nodes$color=adjustcolor(vn$nodes$color,alpha.f=min(c(opacity,1)))
  ## set a darkest gray for iter-cluster edges
  vn$edges$color <- paste(substr(vn$edges$color,1,7),"90",sep="")
  vn$edges$color[substr(vn$edges$color,1,7)=="#B3B3B3"] <- "#69696960"
    vn$edges$color <- adjustcolor(vn$edges$color,alpha.f=opacity)
    
    ## removing multiple edges
    vn$edges <- unique(vn$edges)
    
    vn$edges$width <- vn$edges$width^2/(max(vn$edges$width^2))*(5+edgesize)
    
    ## labelsize
    scalemin <- 20
    scalemax <- 100
    # aggiunta
    vn$nodes$font.size <- vn$nodes$deg
    #
    Min <- min(vn$nodes$font.size)
    Max <- max(vn$nodes$font.size)
    if (Max>Min){
      size=(vn$nodes$font.size-Min)/(Max-Min)*15*labelsize#+10
    } else {size=5*labelsize}
    size[size<scalemin]=scalemin
    size[size>scalemax]=scalemax
    vn$nodes$font.size=size
    #l<-netLayout(type)
    
    ### TO ADD SHAPE AND FONT COLOR OPTIONS

    vn$nodes$size <- vn$nodes$font.size*0.4
    
    if (shape %in% c("dot","square")){
      vn$nodes$font.vadjust <- -0.7*vn$nodes$font.size
    }else{
      vn$nodes$font.vadjust <-0
    }
    
    opacity_font <- sqrt((vn$nodes$font.size-min(vn$nodes$font.size))/diff(range(vn$nodes$font.size)))*opacity+0.3
    if(is.nan(opacity_font[1])) opacity_font <- rep(0.3,length(opacity_font)) 
    
    if (labelsize>0){
      vn$nodes$font.color <- unlist(lapply(opacity_font, function(x) adjustcolor("black",alpha.f = x)))
    }else{
      vn$nodes$font.color <- adjustcolor("black", alpha.f = 0)
    }
    
    VIS<-
      visNetwork::visNetwork(nodes = vn$nodes, edges = vn$edges, type="full", smooth=TRUE, physics=FALSE) %>%
      visNetwork::visNodes(shadow=shadow, shape=shape, font=list(color=vn$nodes$font.color, size=vn$nodes$font.size,vadjust=vn$nodes$font.vadjust)) %>%
      visNetwork::visIgraphLayout(layout = "layout_nicely", type = "full") %>%
      visNetwork::visEdges(smooth = list(type="horizontal")) %>%
      visNetwork::visOptions(highlightNearest =list(enabled = T, hover = T, degree=1), nodesIdSelection = T) %>%
      visNetwork::visInteraction(dragNodes = TRUE, navigationButtons = F, hideEdgesOnDrag = TRUE) %>%
      visNetwork::visOptions(manipulation = curved, height ="100%", width = "100%")
    
    return(list(VIS=VIS,vn=vn))
}


hist2vis<-function(net, labelsize = 2, nodesize= 2, curved=FALSE, shape="dot", opacity=0.7, labeltype="short", timeline=TRUE){
  
  LABEL <- igraph::V(net$net)$id
  
  LABEL[igraph::V(net$net)$labelsize==0] <- ""
  
  layout <- net$layout %>% 
    dplyr::select(.data$x,.data$y,.data$color,.data$name)
  
  vn <- visNetwork::toVisNetworkData(net$net)
  
  if (labeltype != "short"){
    vn$nodes$label <- paste0(vn$nodes$years,": ",LABEL)
  }else{
    vn$nodes$label <- LABEL
  }
  
  vn$nodes <- dplyr::left_join(vn$nodes,layout, by=c("id"="name"))
  
  vn$edges$num <- 1
  vn$edges$dashes <- FALSE
  vn$edges$dashes[vn$edges$lty==2] <- TRUE
  vn$edges$color <- "grey"
    
  ## opacity
  vn$nodes$font.color <- vn$nodes$color
  
  vn$nodes$color <- adjustcolor(vn$nodes$color,alpha.f=min(c(opacity-0.2,1)))
  vn$edges$color <- adjustcolor(vn$edges$color,alpha.f=opacity-0.2)
  vn$edges$smooth <- curved 
  
  ## removing multiple edges
  vn$edges=unique(vn$edges)
  
  ## labelsize
  scalemin=20
  scalemax=150
  size=10*labelsize
  size[size<scalemin]=scalemin
  size[size>scalemax]=scalemax
  vn$nodes$font.size=size*0.5
  vn$nodes$size <- nodesize*2
  
  if (shape %in% c("dot","square")){
    vn$nodes$font.vadjust <- -0.7*vn$nodes$font.size
  }else{
    vn$nodes$font.vadjust <-0
  }
  
  text_data <- net$graph.data %>% 
    select(.data$Label, .data$DOI, .data$LCS,.data$GCS) %>% 
    rename(id = .data$Label) %>% 
    filter(!duplicated(.data$id))
  
  vn$nodes <- vn$nodes %>% left_join(text_data, by = "id")
  
  ## split node tooltips into two strings
  title <- strsplit(stringi::stri_trans_totitle(vn$nodes$title), " ")
  
  vn$nodes$title <- unlist(lapply(title, function(l){
    n <- floor(length(l)/2)
    paste0(paste(l[1:n], collapse=" ", sep=""),"<br>",paste(l[(n+1):length(l)], collapse=" ", sep=""))
  }))
  
  vn$nodes <- vn$nodes %>%
    #select(!.data$LCS.y) %>% 
    #rename(LCS = .data$LCS.x) %>% 
    mutate(title = paste("<b>Title</b>: ",
                         .data$title,
                         "<br><b>DOI</b>: ",
                         paste0(
                           '<a href=\"https://doi.org/',
                           .data$DOI,
                           '\" target=\"_blank\">',
                           #"DOI: ",
                           .data$DOI, '</a>'),
                         "<br><b>GCS</b>: ",
                         .data$GCS, "<br><b>LCS</b>: ",
                         .data$LCS, sep=""))
  
  ## add time line
  vn$nodes$group <- "normal"
  vn$nodes$shape <- "dot"
  vn$nodes$shadow <- TRUE
  
  nr <- nrow(vn$nodes)
  y <- max(vn$nodes$y)
  
  vn$nodes[nr+1,c("id","title","label","color","font.color")] <-
    c(rep("logo",3),"black","white")
  vn$nodes$x[nr+1] <- max(vn$nodes$x, na.rm=TRUE)+1
  vn$nodes$y[nr+1] <- y
  vn$nodes$size[nr+1] <- vn$nodes$size[nr]*4
  vn$nodes$years[nr+1] <- as.numeric(vn$nodes$x[nr+1])
  vn$nodes$font.size[nr+1] <- vn$nodes$font.size[nr]
  vn$nodes$group[nr+1] <- "logo"
  vn$nodes$shape[nr+1] <- "image"
  vn$nodes$image[nr+1] <- "logo.jpg"
  vn$nodes$fixed.x <- TRUE
  vn$nodes$fixed.y <- FALSE
  vn$nodes$fixed.y[nr+1] <- TRUE
  vn$nodes$shadow[nr+1] <- FALSE
  
  coords <- vn$nodes[,c("x","y")] %>% 
    as.matrix()
  
  coords[,2] <- coords[,2]^(1/2)
  
  tooltipStyle = ('position: fixed;visibility:hidden;padding: 5px;white-space: nowrap;
                  font-size:12px;font-color:black;background-color:white;')
  
  ## Font opacity
  vn$nodes$LCS[is.na(vn$nodes$LCS)] <- max(vn$nodes$LCS, na.rm=TRUE)
  opacity_font <- sqrt((vn$nodes$LCS-min(vn$nodes$LCS))/diff(range(vn$nodes$LCS)))*0.6+0.4
  
  vn$nodes$size <- opacity_font*5*nodesize
  vn$nodes$size[nrow(vn$nodes)] <- max(5*nodesize)

  for (i in 1:nrow(vn$nodes)) vn$nodes$font.color[i] <-adjustcolor(vn$nodes$font.color[i], alpha.f = opacity_font[i])
  
  VIS <-
    visNetwork::visNetwork(nodes = vn$nodes, edges = vn$edges, type="full", smooth=TRUE, physics=FALSE) %>% 
    visNetwork::visNodes(shadow=vn$nodes$shadow, shape=shape, size = vn$nodes$size, font=list(color=vn$nodes$font.color, size=vn$nodes$font.size,vadjust=vn$nodes$font.vadjust)) %>%
    visNetwork::visIgraphLayout(layout = "layout.norm", layoutMatrix = coords, type = "full") %>%
    visNetwork::visEdges(smooth = list(type="horizontal"), arrows=list(to = list(enabled = TRUE, scaleFactor = 0.5))) %>% 
    visNetwork::visInteraction(dragNodes = T, navigationButtons = F, hideEdgesOnDrag =F, tooltipStyle = tooltipStyle) %>% 
    visNetwork::visOptions(highlightNearest =list(enabled = T, hover = T, degree = list(from = 1), algorithm = "hierarchical"), nodesIdSelection = F,
                           manipulation = FALSE, height = "100%", width = "100%")

  return(list(VIS=VIS,vn=vn, type="historiograph", curved=curved))
}


### Excel report functions
addDataWb <- function(list_df, wb, sheetname){
  l <- length(list_df)
  startRow <- 1
  for (i in 1:l){
    df <- list_df[[i]]
    n <- nrow(df)
    writeDataTable(wb, sheetname, df, startRow = startRow, startCol = 1, tableStyle = "TableStyleMedium20")
    startRow <- startRow + n + 3
  }
  return(wb)
}

addDataScreenWb <- function(list_df, wb, sheetname){
  ind <- which(regexpr(sheetname,wb$sheet_names)>-1)
  if (length(ind)>0){
    sheetname <- paste(sheetname,"(",length(ind)+1,")",sep="")
  } 
  addWorksheet(wb=wb, sheetName=sheetname, gridLines = FALSE)
  if (!is.null(list_df)){
    addDataWb(list_df, wb, sheetname)
    col <- max(unlist(lapply(list_df,ncol))) + 2
  } else {
    col <- 1
  }
  
  results <- list(wb=wb,col=col, sheetname=sheetname)
  return(results)
}

addGgplotsWb <- function(list_plot, wb, sheetname, col, width=10, height=7, dpi=75){
  l <- length(list_plot)
  startRow <- 1
  for (i in 1:l){
    fileName <- tempfile(pattern = "figureImage",
                         fileext = ".png")
    if (inherits(list_plot[[i]], "ggplot")){
      ggsave(plot = list_plot[[i]], filename = fileName, width = width, height = height,
             units = "in", dpi = dpi)
    }
    if (inherits(list_plot[[i]], "igraph")){
      igraph2PNG(x = list_plot[[i]], filename = fileName, width = width, height = height, dpi=dpi)
    }  
    insertImage(wb = wb, sheet = sheetname, file = fileName, width = width, 
                height = height, startRow = startRow, startCol = col, 
                units = "in", dpi = dpi)
    startRow <- startRow + (height*6)+1
  }
  return(wb)
}

screenSh <- function(selector){
  fileName <- tempfile(pattern = "figureImage",
                       tmpdir = "",
                       fileext = "") %>% substr(.,2,nchar(.))
  if (is.null(selector)){
    shinyscreenshot::screenshot(filename=fileName, download=FALSE, server_dir = tempdir())
  } else {
    shinyscreenshot::screenshot(selector=selector, filename=fileName, download=FALSE, server_dir = tempdir())
  }
  file <- paste(tempdir(),"/",fileName,".png",sep="")
  return(file)
}

addScreenWb <- function(df, wb, width=14, height=8, dpi=75){
  names(df) <- c("sheet","file","n")
  if (nrow(df)>0){
    sheet <- unique(df$sheet)
    for (i in 1:length(sheet)){
      sh <- sheet[i]
      df_sh <- df %>% dplyr::filter(.data$sheet==sh)
      l <- nrow(df_sh)
      startRow <- 1
      for (j in 1:l){
        fileName <- df_sh$file[j]
        insertImage(wb = wb, sheet = sh, file = fileName, width = width, 
                    height = height, startRow = startRow, startCol = df_sh$n[j], 
                    units = "in", dpi = dpi)
        startRow <- startRow + (height*10)+3
      }
    }
  }
  return(wb)
}

addSheetToReport <- function(list_df, list_plot, sheetname, wb, dpi=75){
  ind <- which(regexpr(sheetname,wb$sheet_names)>-1)
  if (length(ind)>0){
    sheetname <- paste(sheetname,"(",length(ind)+1,")",sep="")
  } 
  addWorksheet(wb, sheetname, gridLines = FALSE)
  
  if (!is.null(list_df)) {
    col <- max(unlist(lapply(list_df,ncol))) + 2
    wb <- addDataWb(list_df, wb = wb, sheetname = sheetname)
  } else {col <- 1}
  
    if (!is.null(list_plot)){
      wb <- addGgplotsWb(list_plot, wb = wb, sheetname = sheetname, col = col, dpi = dpi)
    }
  #values$sheet_name <- sheetname
  return(wb)
}

short2long <- function(df, myC){
  z <- unlist(lapply(myC, function(x){
    y <- gsub(r"{\s*\([^\)]+\)}","",x)
    gsub(y,df$long[df$short==y],x)
  }))
  names(myC) <- z
  return(myC)
}

dfLabel <- function(){
  short <- c("Empty Report", "MainInfo",            "AnnualSciProd",       "AnnualCitPerYear",    "ThreeFieldsPlot",     "MostRelSources",      "MostLocCitSources",   "BradfordLaw",         "SourceLocImpact",    
             "SourceProdOverTime",  "MostRelAuthors",      "MostLocCitAuthors",   "AuthorProdOverTime",  "LotkaLaw",            "AuthorLocImpact",     "MostRelAffiliations", "AffOverTime",        
             "CorrAuthCountries",   "CountrySciProd",      "CountryProdOverTime", "MostCitCountries",    "MostGlobCitDocs",     "MostLocCitDocs",      "MostLocCitRefs",      "RPYS",               
             "MostFreqWords",       "WordCloud",           "TreeMap",             "WordFreqOverTime",        "TrendTopics",         "CouplingMap", "CoWordNet",           "ThematicMap",         "ThematicEvolution",  
             "TE_Period_1","TE_Period_2", "TE_Period_3","TE_Period_4","TE_Period_5",       "FactorialAnalysis",   "CoCitNet",            "Historiograph",       "CollabNet",           "CollabWorldMap")
  
  long <- c("Empty Report", "Main Information", "Annual Scientific Production", "Annual Citation Per Year", "Three-Field Plot", "Most Relevant Sources","Most Local Cited Sources","Bradfords Law","Sources Local Impact",
            "Sources Production over Time", "Most Relevant Authors","Most Local Cited Authors","Authors Production over Time", "Lotkas Law","Authors Local Impact","Most Relevant Affiliations","Affiliations Production over Time",
            "Corresponding Authors Countries","Countries Scientific Production","Countries Production over Time","Most Cited Countries", "Most Global Cited Documents","Most Local Cited Documents","Most Local Cited References","Reference Spectroscopy",
            "Most Frequent Words","WordCloud", "TreeMap", "Words Frequency over Time", "Trend Topics", "Clustering by Coupling","Co-occurence Network", "Thematic Map", "Thematic Evolution", 
            "TE_Period_1","TE_Period_2", "TE_Period_3","TE_Period_4","TE_Period_5","Factorial Analysis", "Co-citation Network", "Historiograph", "Collaboration Network", "Countries Collaboration World Map")
  data.frame(short=short,long=long)
}

## Ad to Report PopUp
popUp <- function(title=NULL, type="success", btn_labels="OK"){
  switch(type,
         success={
           title <- paste(title,"\n added to report",sep="")
           subtitle <- ""
           btn_colors = "#1d8fe1"
           showButton = TRUE
           timer = 3000
         },
         error={
           title <- "No results to add to the report "
           subtitle <- "Please Run the analysis and then Add it to the report"
           btn_colors = "#913333"
           showButton = TRUE
           timer = 3000
         },
         waiting={
           title <- "Please wait... "
           subtitle <- "Adding results to report"
           btn_colors = "#FFA800"
           showButton = FALSE
           btn_labels = NA
           timer = NA
         })
  
show_alert(
    title = title,
    text = subtitle,
    type = type,
    size = "s", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    showConfirmButton = showButton,
    showCancelButton = FALSE,
    btn_labels = btn_labels,
    btn_colors = btn_colors,
    timer = timer,
    imageUrl = "",
    animation = TRUE
  )
}

