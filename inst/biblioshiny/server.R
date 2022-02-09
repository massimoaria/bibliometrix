# Biblioshiny Server ----
server <- function(input, output, session) {
 
# Server settings ---- 
  ## stop the R session
  session$onSessionEnded(stopApp)
  ##
  
  ## suppress warnings
  options(warn = -1)
  ##
  
  ## file upload max size
  maxUploadSize <- 200 # default value
  maxUploadSize <- getShinyOption("maxUploadSize", maxUploadSize)
  #options(shiny.maxRequestSize=200*1024^2)
  options(shiny.maxRequestSize=maxUploadSize*1024^2)
  
  ### initial values 
  data("logo",package="bibliometrix",envir=environment())
  values = reactiveValues()
  values$logo <- logo
  values$logoGrid <- grid::rasterGrob(logo,interpolate = TRUE)
  values$h <- 7
  values$w <- 14 
  values$results <- list("NA")
  values$log <- "working..."
  values$load="FALSE"
  values$field = values$cocngrams = "NA"
  values$citField=values$colField=values$citSep="NA"
  values$NetWords=values$NetRefs=values$ColNetRefs=matrix(NA,1,1)
  values$Title="Network"
  values$Histfield="NA"
  values$histlog="working..."
  values$kk=0
  #values$MRWremove.terms=NULL
  values$M=data.frame(PY=0)
  values$histsearch="NA"
  values$citShortlabel="NA"
  values$S=list("NA")
  values$GR="NA"
  values$dsToken <- "Wrong account or password"
  values$dsSample <- 0
  values$dsQuery <- ""
  values$pmQuery <- " "
  values$pmSample <- 0
  values$ApiOk <- 0
  
  
  
  
  
# LOAD MENU ----

## format identification function ----
  format <- function(obj){
    ext<- sub('.*\\.', '', obj[1])
    switch(ext,
           txt ={
             format <- "plaintext"
           },
           csv ={
             format <- "csv"
           },
           bib ={
             format <- "bibtex"
           },
           ciw ={
             format <- "endnote"
           },
           xlsx={
             format <- "excel"
           }
           )
    return(format)
  }
  
## smart_load function ----
  smart_load <- function(file){
    var <- load(file)
    n <- length(var)
    if (!"M" %in% var){
      if (n == 1) {
        eval(parse(text = paste0("M <- ", var)))
      } else {
        stop("I could not find bibliometrixDB object in your data file: ", file)
      }
    }
    rm(list = var[var != "M"])
    if ( ("M" %in% ls()) & inherits(M, "bibliometrixDB") ){
      return(M)
    } else {
      stop("Please make sure your RData/Rda file contains a bibliometrixDB object (M).")
    }
  }

## loading function ----
  
  DATAloading<- eventReactive(input$applyLoad,{
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    if (input$load=="demo"){
      data(management, package="bibliometrixData")

      values = initial(values)
      values$M <- management
      values$Morig = management
      values$Histfield = "NA"
      values$results = list("NA")
      return()
    }
    inFile <- input$file1
    
    if (!is.null(inFile) & input$load=="import") {
      ext <- getFileNameExtension(inFile$datapath)
      switch(
        input$dbsource,
        isi = {
          switch(ext,
                 ###  WoS ZIP Files
                 zip = {
                   D <-  unzip(inFile$datapath)
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(D,
                                                  dbsource = input$dbsource,
                                                  format = format(D))
                                })
                 },
                 ### WoS Txt/Bib Files
                 {
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(inFile$datapath,
                                                  dbsource = input$dbsource,
                                                  format = format(inFile$datapath))
                                })
                 })
        },
        scopus = {
          switch(ext,
                 ###  Scopus ZIP Files
                 zip = {
                   D <- unzip(inFile$datapath)
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(D,
                                                  dbsource = input$dbsource,
                                                  format = format(D))
                                })
                 },
                 ### Scopus CSV/Bib Files
                 csv = {
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(inFile$datapath,
                                                  dbsource = input$dbsource,
                                                  format = "csv")
                                })
                 },
                 bib = {
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(inFile$datapath,
                                                  dbsource = input$dbsource,
                                                  format = "bibtex")
                                })
                 })
          
        },
        lens = {
          switch(ext,
                 ###  Lens.org ZIP Files
                 zip = {
                   D <-  unzip(inFile$datapath)
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(D,
                                                  dbsource = input$dbsource,
                                                  format = format(D))
                                })
                 },
                 ### Lens.org CSV Files
                 {
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(inFile$datapath,
                                                  dbsource = input$dbsource,
                                                  format = format(inFile$datapath))
                                })
                 })
        },
        cochrane = {
          switch(ext,
                 ###  Cochrane ZIP Files
                 zip = {
                   D <- unzip(inFile$datapath)
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(D,
                                                  dbsource = input$dbsource,
                                                  format = format(D))
                                })
                 },
                 ### Cochrane txt files
                 {
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(inFile$datapath,
                                                  dbsource = input$dbsource,
                                                  format = "plaintext")
                                })
                 })
          
        },
        pubmed = {
          switch(ext,
                 ###  Pubmed ZIP Files
                 zip = {
                   D <- unzip(inFile$datapath)
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(D,
                                                  dbsource = input$dbsource,
                                                  format = "pubmed")
                                })
                 },
                 ### Pubmed txt Files
                 txt = {
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(inFile$datapath,
                                                  dbsource = input$dbsource,
                                                  format = "pubmed")
                                })
                 })
        },
        dimensions = {
          switch(ext,
                 ###  Dimensions ZIP Files
                 zip = {
                   D = unzip(inFile$datapath)
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <-
                                    convert2df(D,
                                               dbsource = input$dbsource,
                                               format = format(D))
                                })
                 },
                 ### Dimensions Xlsx/csv Files
                 xlsx = {
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <-
                                    convert2df(
                                      inFile$datapath,
                                      dbsource = "dimensions",
                                      format = "excel"
                                    )
                                })
                 },
                 csv = {
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <-
                                    convert2df(
                                      inFile$datapath,
                                      dbsource = "dimensions",
                                      format = "csv"
                                    )
                                })
                 })
          
        }
      )
      
    } else if (!is.null(inFile) & input$load=="load") {
      ext <- tolower(getFileNameExtension(inFile$datapath))
      #print(ext)
      switch(ext,
             ### excel format
             xlsx={
               #M <- rio::import(inFile$datapath)
               M <- readxl::read_excel(inFile$datapath) %>% as.data.frame(stringsAsFactors=FALSE)
               class(M) <- c("bibliometrixDB", "data.frame")
               ### M row names
               ### identify duplicated SRs 
               SR=M$SR
               tab=table(SR)
               tab2=table(tab)
               ind=as.numeric(names(tab2))
               ind=ind[which(ind>1)]
               if (length(ind)>0){
                 for (i in ind){
                   indice=names(which(tab==i))
                   for (j in indice){
                     indice2=which(SR==j)
                     SR[indice2]=paste(SR[indice2],as.character(1:length(indice2)),sep=" ")
                   }
                 }
               }
               
               row.names(M) <- SR
             },
             ### RData format
             rdata={
               M <- smart_load(inFile$datapath)
             },
             rda={
               M <- smart_load(inFile$datapath)
             },
             rds={
               M <- readRDS(inFile$datapath)
             })
    } else if (is.null(inFile)) {return(NULL)}
    
    values = initial(values)
    values$M <- M
    values$Morig = M
    values$Histfield = "NA"
    values$results = list("NA")
    
    
  })
  output$contents <- DT::renderDT({
    
    DATAloading()   
    MData = as.data.frame(apply(values$M, 2, function(x) {
      substring(x, 1, 150)
    }), stringsAsFactors = FALSE)
    MData$DOI <-
      paste0(
        '<a href=\"https://doi.org/',
        MData$DI,
        '\" target=\"_blank\">',
        MData$DI,
        '</a>'
      )
    nome = c("DOI", names(MData)[-length(names(MData))])
    MData = MData[nome]
    DT::datatable(MData,escape = FALSE,rownames = FALSE, extensions = c("Buttons"),
                  options = list(
                    pageLength = 50,
                    dom = 'Bfrtip',
                    buttons = list(list(extend = 'pageLength'),
                                   list(extend = 'print')),
                    lengthMenu = list(c(10, 25, 50, -1),
                                      c('10 rows', '25 rows', '50 rows', 'Show all')),
                    columnDefs = list(list(
                      className = 'dt-center', targets = 0:(length(names(MData)) - 1)
                    ))
                  ),
                  class = 'cell-border compact stripe'
    )  %>%
      formatStyle(
        names(MData),
        backgroundColor = 'white',
        textAlign = 'center',
        fontSize = '70%'
      ) 
  })
  
## export functions ----
  output$collection.save <- downloadHandler(
    filename = function() {
      
      paste("Bibliometrix-Export-File-", Sys.Date(), ".",input$save_file, sep="")
    },
    content <- function(file) {
      switch(input$save_file,
             #xlsx={suppressWarnings(rio::export(values$M, file=file))},
             xlsx={suppressWarnings(openxlsx::write.xlsx(values$M, file=file))},
             RData={
               M=values$M
               save(M, file=file)
             })
      
    },
    contentType = input$save_file
  )
  
  output$collection.save_api <- downloadHandler(
    filename = function() {
      
      paste("Bibliometrix-Export-File-", Sys.Date(), ".",input$save_file_api, sep="")
    },
    content <- function(file) {
      switch(input$save_file_api,
             #xlsx={suppressWarnings(rio::export(values$M, file=file))},
             xlsx={suppressWarnings(openxlsx::write.xlsx(values$M, file=file))},
             RData={
               M=values$M
               save(M, file=file)
             })
      
    },
    contentType = input$save_file_api
  )
  
  output$textLog <- renderUI({  
    k=dim(values$M)[1]
    if (k==1){k=0}
    log=paste("Number of Documents ",k)
    textInput("textLog", "Conversion results", 
              value=log)
  })
  
## API MENU ----
### API MENU: Dimensions ----
  
  ### Dimensions modal 
  dsModal <- function(failed = FALSE) {
    modalDialog(
      title = "Dimensions API",
      size = "l",
      
      h4(em(
        strong("1) Get a token using your Dimensions credentials")
      )),
      textInput(
        "dsAccount",
        "Account",
        "",
        width = NULL,
        placeholder = NULL
      ),
      passwordInput("dsPassword",
                    "Password",
                    "",
                    width = NULL,
                    placeholder = NULL),
      
      actionButton("dsToken", "Get a token "),
      h5(tags$b("Token")),
      verbatimTextOutput("tokenLog", placeholder = FALSE),
      tags$hr(),
      h4(em(strong("2) Create a query"))),
      textInput(
        "dsWords",
        "Words",
        "",
        width = NULL,
        placeholder = NULL
      ),
      selectInput(
        "dsFullsearch",
        label = "search field",
        choices = c("Title and Abstract only" = FALSE,
                    "Full text" = TRUE),
        selected = FALSE
      ),
      textInput(
        "dsCategories",
        "Science Categories",
        "",
        width = NULL,
        placeholder = NULL
      ),
      numericInput("dsStartYear", "Start Year", value = 1990),
      numericInput("dsEndYear", "End Year", value = as.numeric(substr(Sys.time(), 1, 4))),
      
      
      actionButton("dsQuery", "Create the query "),
      
      h5(tags$b("Your query")),
      verbatimTextOutput("queryLog", placeholder = FALSE),
      h5(tags$b("Documents returned using your query")),
      verbatimTextOutput("sampleLog", placeholder = FALSE),
      
      
      uiOutput("sliderLimit"),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("dsok", "OK")
      )
    )
  }
  
  ### Show Dimensions modal when button is clicked.
  observeEvent(input$dsShow, {
    showModal(dsModal())
  })
  
  observeEvent(input$dsok, {
    removeModal()
    values$M <- data.frame(Message="Waiting for data")
  })
  
  output$tokenLog <- renderText({ 
    input$dsToken 
    isolate({
      capture.output(Token <- dsAuth(username = input$dsAccount, password = input$dsPassword))
      #Token <- dsAuth(username = input$dsAccount, password = input$dsPassword)
      if (Token==1){
        values$dsToken <- "Wrong account or password"
      }else{
        values$dsToken <- Token
      }
      values$dsToken
      
    })
  })
  
  DSQUERYload<- eventReactive(input$dsQuery,{
    values$dsQuery <- dsQueryBuild(item = "publications", 
                                   words = input$dsWords, 
                                   full.search = input$dsFullsearch,
                                   type = "article", 
                                   categories = input$dsCategories, 
                                   start_year = input$dsStartYear, end_year = input$dsEndYear)
    dsSample <- 0
    capture.output(dsSample <- dsApiRequest(token = values$dsToken, query = values$dsQuery, limit = 0))
    if (class(dsSample)=="numeric"){
      values$dsSample <- 0
    }else{values$dsSample <- dsSample$total_count}
  })
  
  output$queryLog <- renderText({ 
    
    DSQUERYload()
    values$dsQuery
    
  })
  
  output$queryLog2 <- renderText({ 
    DSQUERYload()
    values$dsQuery
  })
  
  output$sampleLog <- renderText({ 
    DSQUERYload()
    mes <- paste("Dimensions returns ",values$dsSample, " documents", collapse="",sep="")
    mes
  }) 
  
  output$sampleLog2 <- renderText({ 
    
    if (nrow(values$M)<2) {n <- 0}else{n <- nrow(values$M)}
    mes <- paste("Dimensions API returns ",n, " documents", collapse="",sep="")
    values$ApiOk <- 0
    return(mes)
  }) 
  
  output$sliderLimit <- renderUI({
    
    sliderInput("sliderLimit", "Total document to download", min = 1,
                max = values$dsSample, value = values$dsSample, step = 1)
  })
  
### API MENU: PubMed ----
  ### PubMed modal 
  pmModal <- function(failed = FALSE) {
    modalDialog(
      title = "PubMed API",
      size = "l",
      h4(em(strong(
        "1) Generate a valid query"
      ))),
      textInput(
        "pmQueryText",
        "Search terms",
        " ",
        width = NULL,
        placeholder = NULL
      ),
      numericInput("pmStartYear", "Start Year", value = 1990),
      numericInput("pmEndYear", "End Year", value = as.numeric(substr(Sys.time(
      ), 1, 4))),
      actionButton("pmQuery", "Try the query "),
      h5(tags$b("Query Translation")),
      verbatimTextOutput("pmQueryLog", placeholder = FALSE),
      #h5(tags$b("Generated query")),
      h5(tags$b("Documents returned using your query")),
      verbatimTextOutput("pmSampleLog", placeholder = FALSE),
      tags$hr(),
      h4(em(
        strong("2) Choose how many documents to download")
      )),
      
      uiOutput("pmSliderLimit"),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("pmok", "OK")
      )
    )
  }
  
  # Show modal when button is clicked.
  observeEvent(input$pmShow, {
    showModal(pmModal())
  })
  
  observeEvent(input$pmok, {
    removeModal()
  })
  
  pmQUERYLOAD <- eventReactive(input$pmQuery,{
    
    query = paste(input$pmQueryText,"[Title/Abstract] AND english[LA] AND Journal Article[PT] AND "
                  ,input$pmStartYear,":",input$pmEndYear,"[DP]", sep="")
    res <- pmQueryTotalCount(query = query, api_key = NULL)
    if (class(res)=="list"){
      values$pmSample <- res$total_count
      values$pmQuery <- res$query_translation}
    values$pmQuery <- res$query_translation
    
  })
  output$pmQueryLog <- renderText({ 
    pmQUERYLOAD()
    values$pmQuery
  })
  
  output$pmQueryLog2 <- renderText({ 
    pmQUERYLOAD()
    values$pmQuery
  })
  
  output$pmSampleLog <- renderText({ 
    pmQUERYLOAD()
    mes <- paste("PubMed returns ",values$pmSample, " documents", collapse="",sep="")
    mes
    
  }) 
  output$pmSampleLog2 <- renderText({ 
    if (nrow(values$M)<2) {n <- 0}else{n <- nrow(values$M)}
    
    mes <- paste("PubMed API returns ",n, " documents", collapse="",sep="")
    values$ApiOk <- 0
    return(mes)
  }) 
  
  output$pmSliderLimit <- renderUI({
    sliderInput("pmSliderLimit", "Total document to download", min = 1,
                max = values$pmSample, value = values$pmSample, step = 1)
  })
### API MENU: Content Download ----
  APIDOWNLOAD <- eventReactive(input$apiApply,{
    values = initial(values)
    values$M <- data.frame(Message="Waiting for data")
    
    switch(input$dbapi,
           ds={
             if (input$dsWords!="") {
               #capture.output(
               D <-
                 dsApiRequest(
                   token = values$dsToken,
                   query = values$dsQuery,
                   limit = input$sliderLimit
                 )
               #)
               M <- convert2df(D, "dimensions", "api")
               values$ApiOk <- 1
               values$M <- M
               values$Morig = M
               
               values$Histfield = "NA"
               values$results = list("NA")
               
               contentTable(values)
             }
           },
           pubmed={
             #if (exists("input$pmQueryText")){
             if (input$pmQueryText !=" ") {
               #capture.output(
               D <-
                 pmApiRequest(
                   query = values$pmQuery,
                   limit = input$pmSliderLimit,
                   api_key = NULL
                 )
               #)
               M <- convert2df(D, "pubmed", "api")
               values$ApiOk <- 1
               values$M <- M
               values$Morig = M
               
               values$Histfield = "NA"
               values$results = list("NA")
               
               #contentTable(values)
             }
             
           })
  })
  
  
  
  output$apiContents <- DT::renderDT({
    APIDOWNLOAD()
    contentTable(values)
    #}
    
  })
  
### function returns a formatted data.frame ----
  contentTable <- function(values){
    MData = as.data.frame(apply(values$M, 2, function(x) {
      substring(x, 1, 150)
    }), stringsAsFactors = FALSE)
    MData$DOI <-
      paste0(
        '<a href=\"https://doi.org/',
        MData$DI,
        '\" target=\"_blank\">',
        MData$DI,
        '</a>'
      )
    nome = c("DOI", names(MData)[-length(names(MData))])
    MData = MData[nome]
    DT::datatable(MData,escape = FALSE,rownames = FALSE, extensions = c("Buttons"),
                  options = list(
                    pageLength = 50,
                    dom = 'Bfrtip',
                    buttons = list(list(extend = 'pageLength'),
                                   list(extend = 'print')),
                    lengthMenu = list(c(10, 25, 50, -1),
                                      c('10 rows', '25 rows', '50 rows', 'Show all')),
                    columnDefs = list(list(
                      className = 'dt-center', targets = 0:(length(names(MData)) - 1)
                    ))
                  ),
                  class = 'cell-border compact stripe'
    )  %>%
      formatStyle(
        names(MData),
        backgroundColor = 'white',
        textAlign = 'center',
        fontSize = '70%'
      )
  }
  
  

# FILTERS MENU ----
## Filters uiOutput ----
  # output$textDim <- renderUI({
  #   dimMatrix=paste("Documents ",dim(values$M)[1]," of ",dim(values$Morig)[1],"\nSources ",length(unique(values$M$SO)))
  #   textInput("textDim", "Number of Documents", 
  #             value=dimMatrix)
  # })
  # 
  output$textDim <-  renderUI({
    str1=paste("Documents  ",dim(values$M)[1]," of ",dim(values$Morig)[1])
    str2=paste("Sources    ",length(unique(values$M$SO))," of ", length(unique(values$Morig$SO)))
    str3=paste("Authors    ",length(unique(unlist(strsplit(values$M$AU,";"))))," of ", length(unique(unlist(strsplit(values$Morig$AU,";")))))
    HTML(paste("<pre>", str1, str2, str3,"</pre>", sep = '<br/>'))
  })
  
  output$selectType <- renderUI({
    
    artType=sort(unique(values$Morig$DT))
    selectInput("selectType", "Document Type", 
                choices = artType,
                selected = artType,
                multiple =TRUE )
  })
  output$selectLA <- renderUI({
    
    if ("LA" %in% names(values$Morig)){
    LA <- sort(unique(values$Morig$LA))} else {
      values$Morig$LA <- "N.A."
      LA <- "N.A."
    }
    selectInput("selectLA", "Language", 
                choices = LA,
                selected = LA,
                multiple =TRUE )
  })
  
  output$sliderPY <- renderUI({
    
    sliderInput("sliderPY", "Publication Year", min = min(values$Morig$PY,na.rm=T),sep="",
                max = max(values$Morig$PY,na.rm=T), value = c(min(values$Morig$PY,na.rm=T),max(values$Morig$PY,na.rm=T)))
  })
  
  output$selectSource <- renderUI({
    SO=sort(unique(values$Morig$SO))
    selectInput("selectSource", "Source", 
                choices = SO,
                selected = SO,
                multiple = TRUE)
    
  })
  
  output$sliderTCpY <- renderUI({
    Y <- as.numeric(substr(Sys.time(),1,4))
    values$Morig <- values$Morig %>% 
      mutate(Age = Y - .data$PY+1,
             TCpY = round(.data$TC/Age,2)) %>% 
      group_by(.data$PY) %>% 
      mutate(NTC = .data$TC/mean(.data$TC, na.rm=T)) %>% 
      as.data.frame()
    sliderInput("sliderTCpY", "Average Citation per Year", min = floor(min(values$Morig$TCpY, na.rm=T)),
                max = ceiling(max(values$Morig$TCpY,na.rm=T)), step=0.1,
                value = c(floor(min(values$Morig$TCpY, na.rm=T)),ceiling(max(values$Morig$TCpY,na.rm=T))))
  })

## Update filtered data ----
  
  DTfiltered <- eventReactive(input$applyFilter,{
    M <- values$Morig
    B <- bradford(M)$table
    M <- subset(M, M$PY>=input$sliderPY[1] & M$PY<=input$sliderPY[2])
    M <- subset(M, M$TCpY>=input$sliderTCpY[1] & M$TCpY<=input$sliderTCpY[2])
    M <- subset(M, M$DT %in% input$selectType)
    M <- subset(M, M$LA %in% input$selectLA)
    switch(input$bradfordSources,
           "core"={
             SO=B$SO[B$Zone %in% "Zone 1"]
           },
           "zone2"={
             SO=B$SO[B$Zone %in% c("Zone 1", "Zone 2")]
           },
           "all"={SO=B$SO})
    M=M[M$SO %in% SO,]
    values<-initial(values)
    values$M=M
    Mdisp=as.data.frame(apply(values$M,2,function(x){substring(x,1,150)}),stringsAsFactors = FALSE)    
    if (dim(Mdisp)[1]>0){
      DT::datatable(Mdisp, rownames = FALSE, extensions = c("Buttons"),
                    options = list(pageLength = 50, dom = 'Bfrtip',
                                   buttons = list('pageLength',
                                                  list(extend = 'copy'),
                                                  list(extend = 'csv',
                                                       filename = 'Filtered_DataTable',
                                                       title = "My Title",
                                                       header = TRUE),
                                                  list(extend = 'excel',
                                                       filename = 'Filtered_DataTable',
                                                       title = "My Title",
                                                       header = TRUE),
                                                  list(extend = 'print')),
                                   lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                   columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(Mdisp))-1)))),
                    class = 'cell-border compact stripe') %>%
        formatStyle(names(Mdisp),  backgroundColor = 'white',textAlign = 'center', fontSize = '70%')
    }else{Mdisp=data.frame(Message="Empty collection",stringsAsFactors = FALSE, row.names = " ")}
  })
  
  output$dataFiltered <- DT::renderDT({
    
    DTfiltered()
    
  })
  
  
  

# DATASET MENU ----
## Main Info ----  
  output$MainInfo <- DT::renderDT({
    res <- descriptive(values,type="tab1")
    TAB<-res$TAB
    values <-res$values
    
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 50, dom = 'Bfrtip',ordering=F,
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Main_Information',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Main_Information',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Main_Information',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')), 
                                 #lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 #columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                 #              list(width = '50px', targets = 0)), 
                                 columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                   list(width = '350px', targets = 0))),
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB)[1],  backgroundColor = 'white',textAlign = 'left', fontSize = '110%') %>%
      formatStyle(names(TAB)[2],  backgroundColor = 'white',textAlign = 'right', fontSize = '110%')
  })

## Annual Production ----
  output$CAGR <- renderText({
    Y=table(values$M$PY)
    ny=dim(Y)[1]
    values$GR<-round(((Y[ny]/Y[1])^(1/(ny-1))-1)*100,2)
    paste("Annual Growth Rate: ",values$GR,"%",collapse="",sep="")
  })
  
  output$AnnualProdPlot <- renderPlotly({
    res <- descriptive(values,type="tab2")
    values <-res$values
    Tab=table(values$results$Years)
    
    ## inserting missing years
    YY=setdiff(seq(min(values$results$Years, na.rm = T),max(values$results$Years,na.rm = T)),names(Tab))
    Y=data.frame(Year=as.numeric(c(names(Tab),YY)),Freq=c(as.numeric(Tab),rep(0,length(YY))))
    Y=Y[order(Y$Year),]
    
    names(Y)=c("Year","Freq")
    x <- c(max(Y$Year)-0.02-diff(range(Y$Year))*0.125, max(Y$Year)-0.02)+1
    y <- c(min(Y$Freq),min(Y$Freq)+diff(range(Y$Freq))*0.125)
    
    
    g=ggplot2::ggplot(Y, aes(x = .data$Year, y = .data$Freq, text=paste("Year: ",.data$Year,"\nN .of Documents: ",.data$Freq))) +
      geom_line(aes(group="NA")) +
      geom_area(aes(group="NA"),fill = '#002F80', alpha = .5) +
      labs(x = 'Year'
           , y = 'Articles'
           , title = "Annual Scientific Production") +
      scale_x_continuous(breaks= (Y$Year[seq(1,length(Y$Year),by=2)])) +
      theme(text = element_text(color = "#444444")
            ,panel.background = element_rect(fill = '#EFEFEF')
            ,panel.grid.minor = element_line(color = '#FFFFFF')
            ,panel.grid.major = element_line(color = '#FFFFFF')
            ,plot.title = element_text(size = 24)
            ,axis.title = element_text(size = 14, color = '#555555')
            ,axis.title.y = element_text(vjust = 1, angle = 0)
            ,axis.title.x = element_text(hjust = 0)
      ) +
      annotation_custom(values$logoGrid, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
    values$ASPplot <- g
    
    plot.ly(g,flip=FALSE, side="r", aspectratio=1.7, size=0.10)
  })#, height = 500, width =900)
  
  output$ASPplot.save <- downloadHandler(
    filename = function() {
    
      paste("AnnualScientificProduction-", Sys.Date(), ".png", sep="")
    },

    content <- function(file) {
      ggsave(filename = file, plot = values$ASPplot, dpi = as.numeric(input$ASPdpi), height = input$ASPh, width = input$ASPh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$AnnualProdTable <- DT::renderDT({
    TAB <- values$TAB
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Annual_Production',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Annual_Production',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Annual_Production',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')), 
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TAB))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%')
    
  })

## Annual Citation per Year ----
  output$AnnualTotCitperYearPlot <- renderPlotly({
    
    if (values$results[[1]]=="NA"){
      values$results=biblioAnalysis(values$M)}
    x=values$results
    
    # Total Citation Plot
    Table2=aggregate(x$TotalCitation,by=list(x$Years),length)
    Table2$xx=aggregate(x$TotalCitation,by=list(x$Years),mean)$x
    Table2$Annual=NA
    d=date()
    d=as.numeric(substring(d,nchar(d)-3,nchar(d)))
    Table2$Years=d-Table2$Group.1
    Table2$Annual=Table2$xx/Table2$Years
    names(Table2)=c("Year","N","MeanTCperArt","MeanTCperYear","CitableYears")
    
    ## inserting missing years
    YY=setdiff(seq(min(x$Years,na.rm=TRUE),max(x$Years,na.rm=TRUE)),Table2$Year)
    if (length(YY>0)){
      YY=data.frame(YY,0,0,0,0)
      names(YY)=c("Year","N","MeanTCperArt","MeanTCperYear","CitableYears")
      Table2=rbind(Table2,YY)
      Table2=Table2[order(Table2$Year),]
      row.names(Table2)=Table2$Year}
    
    values$AnnualTotCitperYear=Table2
    Table2$group="A"
    
    x <- c(max(Table2$Year)-0.02-diff(range(Table2$Year))*0.125, max(Table2$Year)-0.02)+1
    y <- c(min(Table2$MeanTCperYear),min(Table2$MeanTCperYear)+diff(range(Table2$MeanTCperYear))*0.125)
    
    g <- ggplot(Table2, aes(x = .data$Year, y =.data$MeanTCperYear,text=paste("Year: ",.data$Year,"\nAverage Citations per Year: ",round(.data$MeanTCperYear,1)))) +
      geom_line(aes(x = .data$Year, y = .data$MeanTCperYear, group=.data$group)) +
      geom_area(aes(x = .data$Year, y = .data$MeanTCperYear, group=.data$group),fill = '#002F80', alpha = .5) +
      labs(x = 'Year'
           , y = 'Citations'
           , title = "Average Article Citations per Year")+
      scale_x_continuous(breaks= (Table2$Year[seq(1,length(Table2$Year),by=2)])) +
      theme(text = element_text(color = "#444444")
            ,panel.background = element_rect(fill = '#EFEFEF')
            ,panel.grid.minor = element_line(color = '#FFFFFF')
            ,panel.grid.major = element_line(color = '#FFFFFF')
            ,plot.title = element_text(size = 24)
            ,axis.title = element_text(size = 14, color = '#555555')
            ,axis.title.y = element_text(vjust = 1, angle = 0)
            ,axis.title.x = element_text(hjust = 0)
      ) + 
      annotation_custom(values$logoGrid, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
    values$ACpYplot <- g
    plot.ly(g,flip=FALSE, side="r", aspectratio=1.7, size=0.10)
  })
  
  output$ACpYplot.save <- downloadHandler(
    filename = function() {
      
      paste("AverageArticleCitationPerYear-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$ACpYplot, dpi = as.numeric(input$ACpYdpi), height = input$ACpYh, width = input$ACpYh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$AnnualTotCitperYearTable <- DT::renderDT({
    
    TAB <- values$AnnualTotCitperYear
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Annual_Total_Citation_per_Year',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Annual_Total_Citation_per_Year',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Annual_Total_Citation_per_Year',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TAB))-1)))),
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%')
    
  })
 
## Three Fileds Plot ---- 
  TFP <- eventReactive(input$apply3F,{
    fields=c(input$LeftField, input$CentralField, input$RightField)
    threeFieldsPlot(values$M, fields=fields,n=c(input$LeftFieldn, input$CentralFieldn,input$RightFieldn))
  })

  output$ThreeFieldsPlot <- renderPlotly({
    TFP()  
  })
  

# SOURCES MENU ----
## Most Relevant Sources ----
  MRSources <- eventReactive(input$applyMRSources,{
    res <- descriptive(values,type="tab7")
    values <-res$values
    values$TABSo<-values$TAB
    #xx=as.data.frame(values$results$Sources)
    xx<- values$TAB
    if (input$MostRelSourcesK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostRelSourcesK}
    #xx=xx[1:k,]
    xx=subset(xx, row.names(xx) %in% row.names(xx)[1:k])
    xx$Articles=as.numeric(xx$Articles)
    xx$Sources=substr(xx$Sources,1,50)
    
    g <- freqPlot(xx,x=2,y=3, textLaby = "Sources", textLabx = "N. of Documents", title = "Most Relevant Sources")

    values$MRSplot <- g
    return(g)
  })
  
  output$MRSplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostRelevantSources-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MRSplot, dpi = as.numeric(input$MRSdpi), height = input$MRSh, width = input$MRSh*2, bg="white")
    },
    contentType = "png"
  )
  
  
  output$MostRelSourcesPlot <- renderPlotly({
    g <- MRSources()
    plot.ly(g,flip=FALSE, side="r", aspectratio=1.1, size=0.10)
  })#, height = 500, width =900)
  
  
  output$MostRelSourcesTable <- DT::renderDT({
    
    g <- MRSources()
    
    TAB <- values$TABSo
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Most_Relevant_Sources',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Most_Relevant_Sources',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Most_Relevant_Sources',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TAB))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%')
    
  })

## Most Local Cited Sources ---- 
  MLCSources <- eventReactive(input$applyMLCSources,{
    values$M=metaTagExtraction(values$M,"CR_SO")
    TAB=tableTag(values$M,"CR_SO")
    TAB=data.frame(Sources=names(TAB),Articles=as.numeric(TAB),stringsAsFactors = FALSE)
    values$TABSoCit<-TAB
    #xx=as.data.frame(values$results$Sources)
    xx<- TAB
    if (input$MostRelCitSourcesK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostRelCitSourcesK}
    #xx=xx[1:k,]
    xx=subset(xx, row.names(xx) %in% row.names(xx)[1:k])
    xx$Articles=as.numeric(xx$Articles)
    xx$Sources=substr(xx$Sources,1,50)
    
    g <- freqPlot(xx,x=2,y=1, textLaby = "Cited Sources", textLabx = "N. of Local Citations", title = "Most Local Cited Sources")
    
    values$MLCSplot <- g
    return(g)
  })
  
  output$MLCSplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostLocalCitedSources-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MLCSplot, dpi = as.numeric(input$MLCSdpi), height = input$MLCSh, width = input$MLCSh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$MostRelCitSourcesPlot <- renderPlotly({
    
    g <- MLCSources()
    
    plot.ly(g,flip=FALSE, side="r", aspectratio=1.3, size=0.10)
  })#, height = 500, width =900)
  
  output$MostRelCitSourcesTable <- DT::renderDT({
    
    g <- MLCSources()
    TAB <- values$TABSoCit
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Most_Cited_Sources',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Most_Cited_Sources',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Most_Cited_Sources',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TAB))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%')
    
  })
## Bradford's Law ---- 
  output$bradfordPlot <- renderPlotly({
    
    values$bradford=bradford(values$M)
    plot.ly(values$bradford$graph,flip=FALSE, side="r", aspectratio=1.6, size=0.15)
    
  })#,height = 600)
  
  output$BLplot.save <- downloadHandler(
    filename = function() {
      
      paste("BradfordLaws-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$bradford$graph, dpi = as.numeric(input$BLdpi), height = input$BLh, width = input$BLh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$bradfordTable <- DT::renderDT({
    
    DT::datatable(values$bradford$table, rownames = FALSE,
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Bradford_Law',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Bradford_Law',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Bradford_Law',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(values$bradford$table))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(values$bradford$table),  backgroundColor = 'white',textAlign = 'center')
  })
## Sources' Impact ----  
  Hsource <- eventReactive(input$applyHsource,{
    withProgress(message = 'Calculation in progress',
                 value = 0, {
                   res <- Hindex_plot(values,type="source")
                 })
    values$SIplot <- res$g
    plot.ly(res$g,flip=FALSE, side="r", aspectratio=1.3, size=0.10)
  })
  
  output$SIplot.save <- downloadHandler(
    filename = function() {
      
      paste("SourceImpact-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$SIplot, dpi = as.numeric(input$SIdpi), height = input$SIh, width = input$SIh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$SourceHindexPlot <- renderPlotly({
    Hsource()
  })#, height = 500, width =900)
  
  output$SourceHindexTable <- DT::renderDT({
    
    DT::datatable(values$H, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Source_Impact',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Source_Impact',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Source_Impact',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(values$H))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(values$H),  backgroundColor = 'white',textAlign = 'center')
    
  })
## Source Growth ----  
  SOGrowth <- eventReactive(input$applySOGrowth,{
    #if (input$SOse=="Yes"){se=TRUE}else{se=FALSE}
    
    if (input$cumSO=="Cum"){
      cdf=TRUE
      laby="Cumulate occurrences"
    }else{
      cdf=FALSE
      laby="Annual occurrences"} 
    
    values$PYSO=sourceGrowth(values$M,input$topSO[2], cdf=cdf)
    if (input$topSO[1]>1){
      values$PYSO <- values$PYSO[-c(2:(input$topSO[1]))]
    }
    
    term=names(values$PYSO)[-1]
    
    term=rep(term,each=dim(values$PYSO)[1])
    n=dim(values$PYSO)[1]*(dim(values$PYSO)[2]-1)
    freq=matrix(as.matrix(values$PYSO[,-1]),n,1)
    values$SODF=data.frame(Year=rep(values$PYSO$Year,(dim(values$PYSO)[2]-1)),Source=term, Freq=freq, stringsAsFactors = TRUE)
    
    Text <- paste(values$SODF$Source," (",values$SODF$Year,") ",values$SODF$Freq, sep="")
    
    width_scale <- 1.7 * 26 / length(unique(values$SODF$Source))
    
    x <- c(max(values$SODF$Year)-0.02-diff(range(values$SODF$Year))*0.15, max(values$SODF$Year)-0.02)+1
    y <- c(min(values$SODF$Freq),min(values$SODF$Freq)+diff(range(values$SODF$Freq))*0.15)
    
    g=ggplot(values$SODF, aes(x=values$SODF$Year,y=values$SODF$Freq, group=values$SODF$Source, color=values$SODF$Source, text=Text))+
      geom_line()+
      labs(x = 'Year'
           , y = laby
           , title = "Source Growth") +
      scale_x_continuous(breaks= (values$PYSO$Year[seq(1,length(values$PYSO$Year),by=ceiling(length(values$PYSO$Year)/20))])) +
      geom_hline(aes(yintercept=0), alpha=0.1)+
      labs(color = "Source")+
      #guides(fill = guide_legend(nrow = 5))+
      theme(text = element_text(color = "#444444"),
            legend.text=ggplot2::element_text(size=width_scale),
            legend.box.margin = margin(6, 6, 6, 6),
            legend.title=ggplot2::element_text(size=1.5*width_scale,face="bold"),
            legend.position="bottom",
            legend.direction = "vertical",
            legend.key.size = grid::unit(width_scale/50, "inch"),
            legend.key.width = grid::unit(width_scale/50, "inch")
            ,plot.caption = element_text(size = 9, hjust = 0.5, color = "black", face = "bold")
            ,panel.background = element_rect(fill = '#EFEFEF')
            ,panel.grid.minor = element_line(color = '#FFFFFF')
            ,panel.grid.major = element_line(color = '#FFFFFF')
            ,plot.title = element_text(size = 24)
            ,axis.title = element_text(size = 14, color = '#555555')
            ,axis.title.y = element_text(vjust = 1, angle = 90)
            ,axis.title.x = element_text(hjust = 0.95, angle = 0)
            ,axis.text.x = element_text(size=10, angle = 90)
      ) + annotation_custom(values$logoGrid, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
    
    values$SDplot <- g
    return(g)
    #suppressWarnings(plot(g))
  }) 
  
  output$SDplot.save <- downloadHandler(
    filename = function() {
      
      paste("SourceDynamics-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$SDplot, dpi = as.numeric(input$SDdpi), height = input$SDh, width = input$SDh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$soGrowthPlot <- renderPlotly({
    
    g <- SOGrowth()
    
    leg <- list(
      orientation = 'h', 
      y = -0.15,
      font = list(
        family = "sans-serif",
        size = 10,
        color = "#000"),
      bgcolor = "#FFFFFF",
      bordercolor = "#FFFFFF",
      borderwidth = 2) 
    
    plot.ly(g, flip=FALSE, side="r", aspectratio=1.8, size=0.10) %>%
      layout(legend = leg) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c(
               'sendDataToCloud',
               'pan2d', 
               'select2d', 
               'lasso2d',
               'toggleSpikelines'
             )) %>%
      layout(hovermode = 'compare')
    #suppressWarnings(plot(g))
    
    
  #}, width = "auto", height = reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*2/5,0)), res = 150) #height = 600, width = 900)
  })
    
  output$soGrowthtable <- DT::renderDT({
    
    g <- SOGrowth()
    
    soData=values$PYSO
    
    DT::datatable(soData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Source_Dynamics',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Source_Dynamics',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Source_Dynamics',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(soData))-1))))) %>%
      formatStyle(names(soData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  

# AUTHORS MENU ----
## Most Relevant Authors ----
  MRAuthors <- eventReactive(input$applyMRAuthors,{
    res <- descriptive(values,type="tab3")
    values <-res$values
    values$TABAu<-values$TAB
    
    #xx=as.data.frame(values$results$Authors, stringsAsFactors = FALSE)
    xx=values$TABAu
    switch(input$AuFreqMeasure,
           t={
             lab="N. of Documents"
             xx=xx[,1:2]
           },
           p={xx=xx[,1:2]
           xx[,2]=as.numeric(xx[,2])/dim(values$M)[1]*100
           lab="N. of Documents (in %)"
           },
           f={
             xx=xx[,c(1,3)]
             lab="N. of Documents (Fractionalized)"
           })
    
    xx[,2]=as.numeric(xx[,2])
    
    if (input$MostRelAuthorsK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostRelAuthorsK}
    
    xx=xx[1:k,]
    xx[,2]=round(xx[,2],1)
    
    xx <- xx[order(-xx[,2]),]
    
    g <- freqPlot(xx,x=2,y=1, textLaby = "Authors", textLabx = lab, title = "Most Relevant Authors")

    values$MRAplot <- g
    return(g)
  })
  
  output$MRAplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostRelevantAuthors-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MRAplot, dpi = as.numeric(input$MRAdpi), height = input$MRAh, width = input$MRAh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$MostRelAuthorsPlot <- renderPlotly({
    
    g <- MRAuthors()
    plot.ly(g,flip=FALSE, side="r", aspectratio=1.3, size=0.10)
  })#, height = 500, width =900)
  
  output$MostRelAuthorsTable <- DT::renderDT({
    
    TAB <- values$TABAu
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Most_Local_Cited_Authors',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Most_Local_Cited_Authors',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Most_Relevant_Authors',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TAB))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%') %>%
      formatRound(names(TAB)[3], digits=2)
    
  })
## Most Cited Authors ----  
  MLCAuthors <- eventReactive(input$applyMLCAuthors,{
    res <- descriptive(values,type="tab13")
    values <-res$values
    values$TABAuCit<-values$TAB
    
    #xx=as.data.frame(values$results$Authors, stringsAsFactors = FALSE)
    xx <- values$TABAuCit
    lab <- "Local Citations"
    xx[,2]=as.numeric(xx[,2])
    
    if (input$MostCitAuthorsK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostCitAuthorsK}
    
    xx=xx[1:k,]
    xx[,2]=round(xx[,2],1)
    
    xx <- xx[order(-xx[,2]),]
    
    g <- freqPlot(xx,x=2,y=1, textLaby = "Authors", textLabx = lab, title = "Most Local Cited Authors")

    values$MLCAplot <- g
    return(g)
  })
  
  output$MLCAplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostLocalCitedAuthors-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MLCAplot, dpi = as.numeric(input$MLCAdpi), height = input$MLCAh, width = input$MLCAh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$MostCitAuthorsPlot <- renderPlotly({
    
    g <- MLCAuthors()
    plot.ly(g,flip=FALSE, side="r", aspectratio=1.3, size=0.10)
  })#, height = 500, width =900)
  
  output$MostCitAuthorsTable <- DT::renderDT({
    
    TAB <- values$TABAuCit
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Most_Local_Cited_Authors',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Most_Local_Cited_Authors',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Most_Relevant_Authors',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TAB))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%') 
    
  })
## Authors' Impact ----  
  HAuthors <- eventReactive(input$applyHAuthors,{
    withProgress(message = 'Calculation in progress',
                 value = 0, {
                   res <- Hindex_plot(values,type="author")
                 })
    values$AIplot <- res$g
    return(res)
  })
  
  output$AIplot.save <- downloadHandler(
    filename = function() {
      
      paste("AuthorImpact-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$AIplot, dpi = as.numeric(input$AIdpi), height = input$AIh, width = input$AIh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$AuthorHindexPlot <- renderPlotly({
    
    res <- HAuthors()
    plot.ly(res$g,flip=FALSE, side="r", aspectratio=1.3, size=0.10)
    
  })#, height = 500, width =900)
  
  output$AuthorHindexTable <- DT::renderDT({
    
    DT::datatable(values$H, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Author_Impact',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Author_Impact',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Author_Impact',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(values$H))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(values$H),  backgroundColor = 'white',textAlign = 'center') %>%
      formatRound(names(values$H)[4], 3)
    
  })
## Authors Production Over Time ----  
  AUoverTime <- eventReactive(input$applyAUoverTime,{
    values$AUProdOverTime <- authorProdOverTime(values$M, k=input$TopAuthorsProdK, graph=FALSE)
  })
  
  output$APOTplot.save <- downloadHandler(
    filename = function() {
      
      paste("AuthorsProductionOverTime-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$AUProdOverTime$graph, dpi = as.numeric(input$APOTdpi), height = input$APOTh, width = input$APOTh*2.5, bg="white")
    },
    contentType = "png"
  )
  
  output$TopAuthorsProdPlot <- renderPlotly({
    AUoverTime()
    
    plot.ly(values$AUProdOverTime$graph, flip=TRUE, side="l", aspectratio=1)
    
    
  })#, height = 550, width =1100)
  
  output$TopAuthorsProdTable <- DT::renderDT({
    AUoverTime()
    
    TAB <- values$AUProdOverTime$dfAU
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Author_Production_Over_Time',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Author_Production_Over_Time',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Author_Production_Over_Time',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TAB))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%') %>%
      formatRound(names(TAB)[dim(TAB)[2]], 3)
    
  })
  
  output$TopAuthorsProdTablePapers <- DT::renderDT({
    AUoverTime()
    TAB <- values$AUProdOverTime$dfPapersAU
    TAB$DOI=paste0('<a href=\"https://doi.org/',TAB$DOI,'\" target=\"_blank\">',TAB$DOI,'</a>')
    DT::datatable(TAB, rownames = FALSE, escape = FALSE,extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Author_Production_Over_Time_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Author_Production_Over_Time_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Author_Production_Over_Time_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TAB))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%') %>%
      formatRound(names(TAB)[dim(TAB)[2]], 3)
    
  })
## Lotka Law ----  
  output$lotkaPlot <- renderPlotly({
    
    values$lotka=lotka(biblioAnalysis(values$M))
    AuProd=values$lotka$AuthorProd
    AuProd$Theoretical=10^(log10(values$lotka$C)-2*log10(AuProd[,1]))
    AuProd$Theoretical=AuProd$Theoretical/sum(AuProd$Theoretical)
    
    x <- c(max(AuProd$N.Articles)-0.02-diff(range(AuProd$N.Articles))*0.125, max(AuProd$N.Articles)-0.02)+1
    y <- c(min(AuProd$Freq*100),min(AuProd$Freq*100)+diff(range(AuProd$Freq*100))*0.125)
    
    g=ggplot2::ggplot(AuProd, aes(x = .data$N.Articles, y = .data$Freq*100, text=paste("N.Articles: ",.data$N.Articles,"\n% of production: ",round(.data$Freq*100,1)))) +
      geom_line(aes(group="NA")) +
      geom_area(aes(group="NA"),fill = '#002F80', alpha = .5) +
      geom_line(data=AuProd, aes(y=.data$Theoretical*100, group="NA"),linetype = "dashed",color="black",alpha=0.8)+
      xlim(0,max(AuProd$N.Articles)+1)+
      labs(x = 'Documents written'
           , y = '% of Authors'
           , title = "The Frequency Distribution of Scientific Productivity") +
      #scale_x_continuous(breaks= (Y$Year[seq(1,length(Y$Year),by=2)])) +
      theme(text = element_text(color = "#444444")
            ,panel.background = element_rect(fill = '#EFEFEF')
            ,panel.grid.minor = element_line(color = '#FFFFFF')
            ,panel.grid.major = element_line(color = '#FFFFFF')
            ,plot.title = element_text(size = 24)
            ,axis.title = element_text(size = 14, color = '#555555')
            ,axis.title.y = element_text(vjust = 1, angle = 90)
            ,axis.title.x = element_text(hjust = 0)
      ) + annotation_custom(values$logoGrid, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
    values$LLplot <- g
    plot.ly(g,flip=FALSE, side="r", aspectratio=1.4, size=0.10)
    
  })#,height = 600)
  
  output$LLplot.save <- downloadHandler(
    filename = function() {
      
      paste("LotkaLaw-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$LLplot, dpi = as.numeric(input$LLdpi), height = input$LLh, width = input$LLh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$lotkaTable <- DT::renderDT({
    names(values$lotka$AuthorProd)=c("Documents written","N. of Authors","Proportion of Authors")
    DT::datatable(values$lotka$AuthorProd, rownames = FALSE,
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Lotka_Law',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Lotka_Law',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Lotka_Law',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(values$lotka$AuthorProd))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(values$lotka$AuthorProd),  backgroundColor = 'white',textAlign = 'center') %>%
      formatRound(names(values$lotka$AuthorProd)[3], 3)
  })
  

# Affiliations ----
## Most Relevant Affliations ---- 
  MRAffiliations <- eventReactive(input$applyMRAffiliations,{
    if (input$disAff=="Y"){
      res <- descriptive(values,type="tab11")
      xx=as.data.frame(values$results$Affiliations, stringsAsFactors = FALSE)
    }else{
      res <- descriptive(values,type="tab12")
      xx=values$TAB
      names(xx)=c("AFF","Freq")
    }
    
    values <-res$values
    values$TABAff <- values$TAB
    
    
    if (input$MostRelAffiliationsK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostRelAffiliationsK}
    
    xx=xx[1:k,]
    
    g <- freqPlot(xx,x=2,y=1, textLaby = "Affiliations", textLabx = "Articles", title = "Most Relevant Affiliations")
    
    values$AFFplot <- g
    return(g)
  })
  
  output$AFFplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostRelevantAffiliations-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$AFFplot, dpi = as.numeric(input$AFFdpi), height = input$AFFh, width = input$AFFh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$MostRelAffiliationsPlot <- renderPlotly({
    
    g <- MRAffiliations()
    
    plot.ly(g,flip=FALSE, side="r", aspectratio=1, size=0.15)
  })#, height = 500, width =900)
  
  output$MostRelAffiliationsTable <- DT::renderDT({
    g <- MRAffiliations()
    
    TAB <- values$TABAff
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Most_Relevant_Affiliations',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Most_Relevant_Affiliations',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Most_Relevant_Affiliations',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TAB))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%')
    
  })
  

# Countries ----
## Country by Corresponding Authors ----  
  CAUCountries <- eventReactive(input$applyCAUCountries,{
    res <- descriptive(values,type="tab5")
    values <-res$values
    values$TABCo <- values$TAB
    
    k=input$MostRelCountriesK
    xx=values$results$CountryCollaboration[1:k,]
    xx=xx[order(-(xx$SCP+xx$MCP)),]
    xx1=cbind(xx[,1:2],rep("SCP",k))
    names(xx1)=c("Country","Freq","Collaboration")
    xx2=cbind(xx[,c(1,3)],rep("MCP",k))
    names(xx2)=c("Country","Freq","Collaboration")
    xx=rbind(xx2,xx1)
    xx$Country=factor(xx$Country,levels=xx$Country[1:dim(xx2)[1]])
    
    xx2 <- xx %>% dplyr::group_by(.data$Country) %>%
      dplyr::summarize(Freq = sum(.data$Freq))
    
    #x <- c(length(levels(xx2$Country))*(1-0.125)-0.02, length(levels(xx2$Country))-0.02)
    x <- c(0.5,0.5+length(levels(xx2$Country))*0.125)+1
    y <- c(max(xx2$Freq)-0.02-diff(range(xx2$Freq))*0.125,max(xx2$Freq)-0.02)
    
    g=suppressWarnings(ggplot2::ggplot(data=xx, aes(x=.data$Country, y=.data$Freq,fill=.data$Collaboration, text=paste("Country: ",.data$Country,"\nN.of Documents: ",.data$Freq))) +
                         geom_bar(aes(group="NA"),stat="identity")+
                         scale_x_discrete(limits = rev(levels(xx$Country)))+
                         scale_fill_discrete(name="Collaboration",
                                             breaks=c("SCP","MCP"))+
                         labs(title = "Corresponding Author's Country", x = "Countries", y = "N. of Documents", 
                              caption = "SCP: Single Country Publications, MCP: Multiple Country Publications")+
                         theme_minimal() +
                         theme(plot.caption = element_text(size = 9, hjust = 0.5,
                                                           color = "blue", face = "italic"))+
                         coord_flip()) + 
      annotation_custom(values$logoGrid, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
    
    values$MRCOplot <- g
    return(g)
  }) 
  
  output$MRCOplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostRelevantCountries-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MRCOplot, dpi = as.numeric(input$MRCOdpi), height = input$MRCOh, width = input$MRCOh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$MostRelCountriesPlot <- renderPlotly({
    
    g <- CAUCountries()
    
    plot.ly(g,flip=T, side="r", aspectratio=1.4, size=0.10, data.type=1)
    
  })#, height = 500, width =900)
  
  output$MostRelCountriesTable <- DT::renderDT({
    g <- CAUCountries()
    
    TAB <- values$TABCo
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Most_Relevant_Countries_By_Corresponding_Author',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Most_Relevant_Countries_By_Corresponding_Author',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Most_Relevant_Countries_By_Corresponding_Author',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TAB))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%')
    
  })

## Country Production ----  
  output$countryProdPlot <- renderPlotly({
    values$mapworld<-mapworld(values$M)
    plot.ly(values$mapworld$g,flip=FALSE, side="r", aspectratio=1.7, size=0.07, data.type=1,height=15)
  })#, height = 500, width =900)
  
  output$CSPplot.save <- downloadHandler(
    filename = function() {
      
      paste("CountryScientificProduction-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$mapworld$g, dpi = as.numeric(input$CSPdpi), height = input$CSPh, width = input$CSPh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$countryProdTable <- DT::renderDT({
    
    TAB <- values$mapworld$tab
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Country_Production',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Country_Production',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Country_Production',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TAB))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%')
    
  })
## Most Cited Country ----    
  MCCountries <- eventReactive(input$applyMCCountries,{
    res <- descriptive(values,type="tab6")
    values <-res$values
    values$TABCitCo <- values$TAB
    
    xx=values$TAB
    xx[,2]=as.numeric(xx[,2])
    xx[,3]=as.numeric(xx[,3])
    if (input$MostCitCountriesK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostRelAffiliationsK}
    if (input$CitCountriesMeasure=="TC"){
      xx=xx[1:k,c(1,2)]
      laby="N. of Citations"
    } else {
      xx=xx[order(-xx[,3]),]
      xx=xx[1:k,c(1,3)]
      laby="N. of Citations per Year"
    }
    
    g <- freqPlot(xx,x=2,y=1, textLaby = "Countries", textLabx = laby, title = "Most Cited Countries")

    values$MCCplot <- g
    return(g)
  })
  
  output$MCCplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostCitedCountries-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MCCplot, dpi = as.numeric(input$MCCdpi), height = input$MCCh, width = input$MCCh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$MostCitCountriesPlot <- renderPlotly({
    g <- MCCountries()
    plot.ly(g,flip=FALSE, side="r", aspectratio=1.3, size=0.10)
  })#, height = 500, width =900)
  
  output$MostCitCountriesTable <- DT::renderDT({
    g <- MCCountries()
    TAB <- values$TABCitCo
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Most_Cited_Countries',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Most_Cited_Countries',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Most_Cited_Countries',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TAB))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%')
    
  })
  

# DOCUMENTS MENU ----
## Most Global Cited Documents ----
  
  MGCDocuments <- eventReactive(input$applyMGCDocuments,{
    res <- descriptive(values,type="tab4")
    values <-res$values
    values$TABGlobDoc <- values$TAB
    
    if (input$CitDocsMeasure=="TC"){
      xx=data.frame(values$results$MostCitedPapers[1],values$results$MostCitedPapers[3], stringsAsFactors = FALSE,row.names=NULL)
      lab="Global Citations"} else {
        xx=data.frame(values$results$MostCitedPapers[1],values$results$MostCitedPapers[4], stringsAsFactors = FALSE,row.names=NULL)
        lab="Global Citations per Year"
      }
    
    if (input$MostCitDocsK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostCitDocsK}
    
    xx=xx[1:k,]
    
    g <- freqPlot(xx,x=2,y=1, textLaby = "Documents", textLabx = lab, title = "Most Global Cited Documents")

    values$MGCDplot <- g
    return(g)
  })
  
  output$MGCDplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostGlobalCitedDocuments-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MGCDplot, dpi = as.numeric(input$MGCDdpi), height = input$MGCDh, width = input$MGCDh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$MostCitDocsPlot <- renderPlotly({
    
    g <- MGCDocuments()
    plot.ly(g,flip=FALSE, side="r", aspectratio=1, size=0.10)
  })#, height = 500, width =900)
  
  output$MostCitDocsTable <- DT::renderDT({
    g <- MGCDocuments()
    TAB <- values$TABGlobDoc
    TAB$DOI<- paste0('<a href=\"https://doi.org/',TAB$DOI,'\" target=\"_blank\">',TAB$DOI,'</a>')
    DT::datatable(TAB, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Most_Global_Cited_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Most_Global_Cited_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Most_Global_Cited_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TAB))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%')
    
  })
## Most Local Cited Documents ----  
  MLCDocuments <- eventReactive(input$applyMLCDocuments,{
    withProgress(message = 'Calculation in progress',
                 value = 0, {
                   TAB <-localCitations(values$M, fast.search=FALSE, sep = input$LocCitSep)$Paper
                   TAB <- TAB %>%
                     group_by(.data$Year) %>%
                     mutate(Ratio = .data$LCS/.data$GCS*100,
                            NLCS = .data$LCS/mean(.data$LCS),
                            NGCS = .data$GCS/mean(.data$GCS)) %>%
                     ungroup() %>%
                     as.data.frame()
  
                 })
    
    xx=data.frame(Document=as.character(TAB[,1]), DOI=as.character(TAB[,2]), Year=TAB[,3], 
                  "Local Citations"=TAB[,4], "Global Citations"=TAB[,5],"LC/GC Ratio"=TAB[6], 
                  "Normalized Local Citations"=TAB[,7],"Normalized Global Citations"=TAB[,8], stringsAsFactors = FALSE)
    values$TABLocDoc=xx
    
    if (input$MostLocCitDocsK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostLocCitDocsK}
    
    xx=xx[1:k,]
    
    g <- freqPlot(xx,x=4,y=1, textLaby = "Documents", textLabx = "Local Citations", title = "Most Local Cited Documents")
    
    values$MLCDplot <- g
    return(g)
  })
  
  output$MLCDplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostLocalCitedDocuments-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MLCDplot, dpi = as.numeric(input$MLCDdpi), height = input$MLCDh, width = input$MLCDh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$MostLocCitDocsPlot <- renderPlotly({
    
    g <- MLCDocuments()
    plot.ly(g,flip=FALSE, side="r", aspectratio=1, size=0.10)
  })#, height = 500, width =900)
  
  output$MostLocCitDocsTable <- DT::renderDT({
    
    TAB <- values$TABLocDoc
    TAB$DOI <- paste0('<a href=\"https://doi.org/',TAB$DOI,'\" target=\"_blank\">',TAB$DOI,'</a>')

    names(TAB)[4:8] <- c("Local Citations", "Global Citations","LC/GC Ratio (%)", "Normalized Local Citations","Normalized Global Citations")
    DT::datatable(TAB, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Most_Local_Cited_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Most_Local_Cited_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Most_Local_Cited_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TAB))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%') %>%
      formatRound(names(TAB)[c(6:8)], digits=2)
    
  })
  
## Most Local Cited References ----
  MLCReferences <- eventReactive(input$applyMLCReferences,{
    CR=citations(values$M,sep=input$CitRefsSep)$Cited
    TAB=data.frame(names(CR),as.numeric(CR),stringsAsFactors = FALSE)
    names(TAB)=c("Cited References", "Citations")
    values$TABCitRef=TAB
    
    xx=values$TABCitRef
    if (input$MostCitRefsK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostCitRefsK}
    
    xx=xx[1:k,]
    #xx[,1]=substr(xx[,1],1,50)
    
    g <- freqPlot(xx,x=2,y=1, textLaby = "References", textLabx = "Local Citations", title = "Most Local Cited References")
    
    values$MLCRplot <- g
    return(g)
  })
  
  output$MLCRplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostLocalCitedReferences-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MLCRplot, dpi = as.numeric(input$MLCRdpi), height = input$MLCRh, width = input$MLCRh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$MostCitRefsPlot <- renderPlotly({
    
    g <- MLCReferences()
    plot.ly(g,flip=FALSE, side="r", aspectratio=0.6, size=0.20)
  })#, height = 500, width =900)
  
  output$MostCitRefsTable <- DT::renderDT({
    g <- MLCReferences()
    TAB <- values$TABCitRef
    
    TAB$link <- trimES(gsub("[[:punct:]]" , " ",reduceRefs(TAB[,1])))
    
    
    TAB$link <- paste0('<a href=\"https://scholar.google.it/scholar?hl=en&as_sdt=0%2C5&q=',TAB$link,'\" target=\"_blank\">','link','</a>')
    
    TAB=TAB[,c(3,1,2)]
    names(TAB)[1]="Google Scholar"
    DT::datatable(TAB, rownames = FALSE, escape=FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Most_Local_Cited_References',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Most_Local_Cited_References',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Most_Local_Cited_References',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TAB))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%')
    
  })
## Reference Spectroscopy ---- 
  RPYS <- eventReactive(input$applyRPYS,{
    
    timespan <- c(-Inf,Inf)
    if (!is.na(input$rpysMinYear)){
      timespan[1] <- input$rpysMinYear
    }
    if (!is.na(input$rpysMaxYear)){
      timespan[2] <- input$rpysMaxYear
    }
    values$res <- rpys(values$M, sep=input$rpysSep, timespan=timespan, graph=FALSE)
  })
  
  output$RSplot.save <- downloadHandler(
    filename = function() {
      
      paste("ReferenceSpectroscopy-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$res$spectroscopy, dpi = as.numeric(input$RSdpi), height = input$RSh, width = input$RSh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$rpysPlot <- renderPlotly({
    
    RPYS()
    plot.ly(values$res$spectroscopy, side="l", aspectratio = 1.3, size=0.10)
    
  })#,height = 600, width = 900)
  
  output$rpysTable <- DT::renderDT({
    RPYS()
    rpysData=values$res$rpysTable
    
    DT::datatable(rpysData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'RPYS',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'RPYS',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'RPYS',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(rpysData))-1))))) %>%
      formatStyle(names(rpysData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$crTable <- DT::renderDT({
    RPYS()
    crData=values$res$CR
    crData$link <- paste0('<a href=\"https://scholar.google.it/scholar?hl=en&as_sdt=0%2C5&q=',crData$Reference,'\" target=\"_blank\">','link','</a>')
    
    crData=crData[order(-as.numeric(crData$Year),-crData$Freq),]
    names(crData)=c("Year", "Reference", "Local Citations", "Google link")
    crData <- crData[,c(1,4,2,3)] 
    DT::datatable(crData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'RPYS_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'RPYS_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'RPYS_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(crData))-1))))) %>%
      formatStyle(names(crData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  

# Words ----
## Most Frequent Words ----
  MFWords <- eventReactive(input$applyMFWords,{
    if (input$MostRelWords %in% c("TI","AB")){
      ngrams <- as.numeric(input$MRWngrams)
    }else{
      ngrams <- 1
    }
    
    ### load file with terms to remove
    if (input$MostRelWordsStopFile=="Y"){
      remove.terms <- trimws(readStopwordsFile(file=input$MostRelWordsStop, sep=input$MostRelWordsSep))
    }else{remove.terms <- NULL}
    values$MRWremove.terms <- remove.terms
    ### end of block
    
    ### load file with synonyms
    if (input$MRWSynFile=="Y"){
      synonyms <- trimws(readSynWordsFile(file=input$MRWSyn, sep=input$MRWSynSep))
    }else{synonyms <- NULL}
    values$MRWsyn.terms <- synonyms
    ### end of block
    
    WR=wordlist(values$M,Field=input$MostRelWords,n=Inf,measure="identity", ngrams=ngrams, remove.terms = remove.terms, synonyms = synonyms)$v
    
    TAB=data.frame(names(WR),as.numeric(WR),stringsAsFactors = FALSE)
    names(TAB)=c("Words", "Occurrences")
    values$TABWord=TAB
    
    xx=values$TABWord
    if (input$MostRelWordsN>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostRelWordsN}
    
    xx=xx[1:k,]
    switch(input$MostRelWords,
           ID={lab="Keywords Plus"},
           DE={lab="Auhtor's Keywords"},
           TI={lab="Title's Words"},
           AB={lab="Abstract's Words"})
    
    g <- freqPlot(xx,x=2,y=1, textLaby = lab, textLabx = "Occurrences", title = "Most Relevant Words")
    
    values$MRWplot <- g
    return(g)
    
  })
  
  output$MostRelWordsStopPreview <-  renderUI({
    
    if (!is.null(values$MRWremove.terms) | exists("values$MRWremove.terms")){
      strPreview(values$MRWremove.terms, input$MostRelWordsSep)  
    }else{
      strPreview(" ", input$MostRelWordsSep)
    }
  })
  
  output$MRWSynPreview <-  renderUI({
    
    if (!is.null(values$MRWsyn.terms) | exists("values$MRWsyn.terms")){
      strSynPreview(values$MRWsyn.terms)  
    }else{
      strSynPreview(" ; ")
    }
  })
  
  output$MRWplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostRelevantWords-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MRWplot, dpi = as.numeric(input$MRWdpi), height = input$MRWh, width = input$MRWh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$MostRelWordsPlot <- renderPlotly({
    g <- MFWords()
    plot.ly(g, side="r", aspectratio = 1.3, size=0.10)
  })#, height = 500, width =900)
  
  output$MostRelWordsTable <- DT::renderDT({
    g <- MFWords()
    
    TAB <- values$TABWord
    
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Most_Frequent_Words',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Most_Frequent_Words',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Most_Frequent_Words',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TAB))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%')
    
  })
## WordCloud ----  
  WordCloud <- eventReactive(input$applyWordCloud,{
    
    if (input$summaryTerms %in% c("TI","AB")){
      ngrams <- as.numeric(input$summaryTermsngrams)
    }else{
      ngrams <- 1
    }
    
    ### load file with terms to remove
    if (input$WCStopFile=="Y"){
      remove.terms <- trimws(readStopwordsFile(file=input$WCStop, sep=input$WCSep))
    }else{remove.terms <- NULL}
    values$WCremove.terms <- remove.terms
    ### end of block
    
    ### load file with synonyms
    if (input$WCSynFile=="Y"){
      synonyms <- trimws(readSynWordsFile(file=input$WCSyn, sep=input$WCSynSep))
    }else{synonyms <- NULL}
    values$WCsyn.terms <- synonyms
    ### end of block
    print(values$WCsyn.terms )
    
    resW=wordlist(M=values$M, Field=input$summaryTerms, n=input$n_words, measure=input$measure, ngrams=ngrams, remove.terms = remove.terms, synonyms = values$WCsyn.terms)
    
    W=resW$W
    values$Words=resW$Words
    
    wordcloud2::wordcloud2(W, size = input$scale, minSize = 0, gridSize =  input$padding,
                           fontFamily = input$font, fontWeight = 'normal',
                           color = input$wcCol, backgroundColor = "white", #input$wcBGCol,
                           minRotation = 0, maxRotation = input$rotate/10, shuffle = TRUE,
                           rotateRatio = 0.7, shape = input$wcShape, ellipticity = input$ellipticity,
                           widgetsize = NULL, figPath = NULL, hoverFunction = NULL)
  })
  output$wordcloud <- wordcloud2::renderWordcloud2({
    
    WordCloud()
  })
  
## TreeMap ----  
  TreeMap <- eventReactive(input$applyTreeMap,{
    if (input$treeTerms %in% c("TI","AB")){
      ngrams <- as.numeric(input$treeTermsngrams)
    }else{
      ngrams <- 1
    }
    ### load file with terms to remove
    if (input$TreeMapStopFile=="Y"){
      remove.terms <- trimws(readStopwordsFile(file=input$TreeMapStop, sep=input$TreeMapSep))
    }else{remove.terms <- NULL}
    values$TreeMapremove.terms <- remove.terms
    ### end of block
    ### load file with synonyms
    if (input$TreeMapSynFile=="Y"){
      synonyms <- trimws(readSynWordsFile(file=input$TreeMapSyn, sep=input$TreeMapSynSep))
    }else{synonyms <- NULL}
    values$TreeMapsyn.terms <- synonyms
    ### end of block
    
    resW=wordlist(M=values$M, Field=input$treeTerms, n=input$treen_words, measure="identity", ngrams=ngrams, remove.terms=remove.terms, synonyms = synonyms)
    
    W=resW$W
    values$TreeMap <- plot_ly(
      type='treemap',
      labels=W[,1],
      parents="Tree",
      values= W[,2],
      textinfo="label+value+percent entry",
      domain=list(column=0))
    
    values$WordsT=resW$Words
    return(resW$Words)
    
  })
  
  output$treemap <- renderPlotly({
    TreeMap()
    values$TreeMap
  })
  
  output$TreeMapStopPreview <-  renderUI({
    
    if (!is.null(values$TreeMapremove.terms) | exists("values$TreeMapremove.terms")){
      strPreview(values$TreeMapremove.terms, input$TreeMapSep)  
    }else{
      strPreview(" ", input$TreeMapSep)
    }
  })
  
  output$TreeMapSynPreview <-  renderUI({
    
    if (!is.null(values$TreeMapsyn.terms) | exists("values$TreeMapsyn.terms")){
      strSynPreview(values$TreeMapsyn.terms)  
    }else{
      strSynPreview(" ")
    }
  })
  
  
  output$wordTable <- DT::renderDT({
    WordCloud()
    
    DT::datatable(values$Words, rownames = FALSE,
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Most_Frequent_Words',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Most_Frequent_Words',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Most_Frequent_Words',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(values$Words))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(values$Words),  backgroundColor = 'white',textAlign = 'center')
  })
  
  output$treeTable <- DT::renderDT({
    WordsT <- TreeMap()
    
    DT::datatable(values$WordsT, rownames = FALSE,
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Most_Frequent_Words',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Most_Frequent_Words',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Most_Frequent_Words',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(values$WordsT))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(values$WordsT),  backgroundColor = 'white',textAlign = 'center')
    
  },height = 600, width = 900)
## Word Dynamics ----   
  WDynamics <- eventReactive(input$applyWD,{
    if (input$cumTerms=="Cum"){
      cdf=TRUE
      laby="Cumulate occurrences"
    }else{
      cdf=FALSE
      laby="Annual occurrences"}
    #if (input$se=="Yes"){se=TRUE}else{se=FALSE}
    
    ### load file with terms to remove
    if (input$WDStopFile=="Y"){
      remove.terms <- trimws(readStopwordsFile(file=input$WDStop, sep=input$WDSep))
    }else{remove.terms <- NULL}
    values$WDremove.terms <- remove.terms
    ### end of block
    ### load file with synonyms
    if (input$WDSynFile=="Y"){
      synonyms <- trimws(readSynWordsFile(file=input$WDSyn, sep=input$WDSynSep))
    }else{synonyms <- NULL}
    values$WDsyn.terms <- synonyms
    ### end of block
    
    switch(input$growthTerms,
           ID={
             KW=KeywordGrowth(values$M, Tag = "ID", sep = ";", top = input$topkw[2], cdf = cdf, remove.terms=remove.terms, synonyms = synonyms)
             
           },
           DE={
             KW=KeywordGrowth(values$M, Tag = "DE", sep = ";", top = input$topkw[2], cdf = cdf, remove.terms=remove.terms, synonyms = synonyms)
           },
           TI={
             #if (!("TI_TM" %in% names(values$M))){
               values$M=termExtraction(values$M,Field = "TI", verbose=FALSE, ngrams=as.numeric(input$growthTermsngrams), remove.terms=remove.terms, synonyms = synonyms)
             #}
             KW=KeywordGrowth(values$M, Tag = "TI_TM", sep = ";", top = input$topkw[2], cdf = cdf)
           },
           AB={
             #if (!("AB_TM" %in% names(values$M))){
               values$M=termExtraction(values$M,Field = "AB", verbose=FALSE, ngrams=as.numeric(input$growthTermsngrams), remove.terms=remove.terms, synonyms = synonyms)
             #}
             KW=KeywordGrowth(values$M, Tag = "AB_TM", sep = ";", top = input$topkw[2], cdf = cdf)
           }
    )
    
    values$KW=KW[,c(1,seq(input$topkw[1],input$topkw[2])+1)]
    
    term=names(values$KW)[-1]
    term=rep(term,each=dim(values$KW)[1])
    n=dim(values$KW)[1]*(dim(values$KW)[2]-1)
    freq=matrix(as.matrix(values$KW[,-1]),n,1)
    values$DF=data.frame(Year=rep(values$KW$Year,(dim(values$KW)[2]-1)),Term=term, Freq=freq, stringsAsFactors = TRUE)
  
    width_scale <- 2.5 * 26 / length(unique(values$DF$Term))
    
    Text <- paste(values$DF$Term," (",values$DF$Year,") ",values$DF$Freq, sep="")
    
    x <- c(max(values$DF$Year)-0.02-diff(range(values$SO$Year))*0.20, max(values$DF$Year)-0.02)-1
    y <- c(min(values$DF$Freq),min(values$DF$Freq)+diff(range(values$DF$Freq))*0.20)
    
    g <- ggplot(values$DF, aes(x=.data$Year,y=.data$Freq, group=.data$Term, color=.data$Term, text = Text))+
      geom_line()+
      labs(x = 'Year'
           , y = laby
           , title = "Word Growth") +
      #ylim(0, NA) +
      scale_x_continuous(breaks= (values$KW$Year[seq(1,length(values$KW$Year),by=ceiling(length(values$KW$Year)/20))])) +
      geom_hline(aes(yintercept=0), alpha=0.1)+
      labs(color = "Term")+
      #guides(fill = guide_legend(nrow = 5))+
      theme(text = element_text(color = "#444444"),
            legend.text=ggplot2::element_text(size=width_scale),
            legend.box.margin = margin(6, 6, 6, 6),
            legend.title=ggplot2::element_text(size=1.5*width_scale,face="bold"),
            legend.position="bottom",
            legend.direction = "vertical",
            legend.key.size = grid::unit(width_scale/50, "inch"),
            legend.key.width = grid::unit(width_scale/50, "inch")
            ,plot.caption = element_text(size = 9, hjust = 0.5, color = "black", face = "bold")
            ,panel.background = element_rect(fill = '#EFEFEF')
            ,panel.grid.minor = element_line(color = '#FFFFFF')
            ,panel.grid.major = element_line(color = '#FFFFFF')
            ,plot.title = element_text(size = 24)
            ,axis.title = element_text(size = 14, color = '#555555')
            ,axis.title.y = element_text(vjust = 1, angle = 90)
            ,axis.title.x = element_text(hjust = 0.95, angle = 0)
            ,axis.text.x = element_text(size=10, angle = 90)
      ) + annotation_custom(values$logoGrid, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
    
    values$WDplot <- g
    return(g)
  })
  
  output$WDStopPreview <-  renderUI({
    
    if (!is.null(values$WDremove.terms) | exists("values$WDremove.terms")){
      strPreview(values$WDremove.terms)  
    }else{
      strPreview(" ")
    }
  })
  
  output$WDSynPreview <-  renderUI({
    
    if (!is.null(values$WDsyn.terms) | exists("values$WDsyn.terms")){
      strSynPreview(values$WDsyn.terms)  
    }else{
      strSynPreview(" ")
    }
  })
  
  output$WDplot.save <- downloadHandler(
    filename = function() {
      
      paste("WordDynamics-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$WDplot, dpi = as.numeric(input$WDdpi), height = input$WDh, width = input$WDh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$kwGrowthPlot <- renderPlotly({
    
    g <- WDynamics()
    
    leg <- list(
      orientation = 'h', 
      y = -0.15,
      font = list(
        family = "sans-serif",
        size = 10,
        color = "#000"),
      bgcolor = "#FFFFFF",
      bordercolor = "#FFFFFF",
      borderwidth = 2) 
    
    plot.ly(g, flip=FALSE, side="r", aspectratio=1.6, size=0.10) %>%
      layout(legend = leg) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c(
               'sendDataToCloud',
               'pan2d', 
               'select2d', 
               'lasso2d',
               'toggleSpikelines'
             )) %>%
      layout(hovermode = 'compare')
    
  #}, width = "auto", height = reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*2/5,0)), res = 150) #height = 600, width = 900)
  })
  
  output$kwGrowthtable <- DT::renderDT({
    g <- WDynamics()
    kwData=values$KW
    
    DT::datatable(kwData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Word_Dynamics',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Word_Dynamics',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Word_Dynamics',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(kwData))-1))))) %>%
      formatStyle(names(kwData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
## Trend Topics ----
  output$trendSliderPY <- renderUI({
    
    sliderInput("trendSliderPY", "Timespan", min = min(values$M$PY,na.rm=T),sep="",
                max = max(values$M$PY,na.rm=T), value = c(min(values$M$PY,na.rm=T),max(values$M$PY,na.rm=T)))
  })
  
  TrendTopics <- eventReactive(input$applyTrendTopics,{
    
    ### load file with terms to remove
    if (input$TTStopFile=="Y"){
      remove.terms <- trimws(readStopwordsFile(file=input$TTStop, sep=input$TTSep))
    }else{remove.terms <- NULL}
    values$TTremove.terms <- remove.terms
    ### end of block
    ### load file with synonyms
    if (input$TTSynFile=="Y"){
      synonyms <- trimws(readSynWordsFile(file=input$TTSyn, sep=input$TTSynSep))
    }else{synonyms <- NULL}
    values$TTsyn.terms <- synonyms
    ### end of block
    
    if (input$trendTerms %in% c("TI","AB")){
      values$M=termExtraction(values$M, Field = input$trendTerms, stemming = input$trendStemming, verbose = FALSE, ngrams=as.numeric(input$trendTermsngrams))
      field=paste(input$trendTerms,"_TM",sep="")
    } else {field=input$trendTerms}
    values$trendTopics <- fieldByYear(values$M, field = field, timespan = input$trendSliderPY, min.freq = input$trendMinFreq,
                                      n.items = input$trendNItems, remove.terms = remove.terms, synonyms = synonyms, 
                                      dynamic.plot=TRUE, graph = FALSE)
    return(values$trendTopics$graph)
    
  })
  
  output$TTStopPreview <-  renderUI({
    
    if (!is.null(values$TTremove.terms) | exists("values$TTremove.terms")){
      strPreview(values$TTremove.terms)  
    }else{
      strPreview(" ")
    }
  })
  
  output$TTSynPreview <-  renderUI({
    
    if (!is.null(values$TTsyn.terms) | exists("values$TTsyn.terms")){
      strSynPreview(values$TTsyn.terms)  
    }else{
      strSynPreview(" ")
    }
  })
  
  output$TTplot.save <- downloadHandler(
    filename = function() {
      
      paste("TrendTopics-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$trendTopics$graph, dpi = as.numeric(input$TTdpi), height = input$TTh, width = input$TTh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$trendTopicsPlot <- renderPlotly({
    g <- TrendTopics()
    plot.ly(g, flip=TRUE, side="r", size=0.1, aspectratio=1.3)
  })#, height = 500, width =900)
  
  # output$trendTopicsPlot <- renderPlot({
  #   
  #   TrendTopics()
  #   plot(values$trendTopics$graph)
  #   
  # }, width = "auto", height = reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*2/5,0)), res = 150)  #height = 700)
  
  
  output$trendTopicsTable <- DT::renderDT({
    TrendTopics()
    tpData=values$trendTopics$df_graph
    
    DT::datatable(tpData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Trend_Topics',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Trend_Topics',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Trend_Topics',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tpData))-1))))) %>%
      formatStyle(names(tpData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
# Coupling ----
  
  CMMAP <- eventReactive(input$applyCM,{
    
    values$CM <- couplingMap(values$M, analysis=input$CManalysis, field=input$CMfield, 
                             n=input$CMn, minfreq=input$CMfreq,
                             ngrams=as.numeric(input$CMngrams),
                             impact.measure=input$CMimpact,
                             stemming=input$CMstemming, size=input$sizeCM, 
                             label.term = input$CMlabeling,
                             n.labels=input$CMn.labels, repel=FALSE)
    
    validate(
      need(values$CM$nclust > 0, "\n\nNo clusters in one or more periods. Please select a different set of parameters.")
    )
  })
  output$CMPlot <- renderPlotly({
    
    CMMAP()
    plot.ly(values$CM$map, size=0.15, aspectratio = 1.3)
    
  })#, height = 650, width = 800)
  
  output$CMNetPlot <- renderVisNetwork({
    CMMAP()
    values$networkCM<-igraph2vis(g=values$CM$net$graph,curved=(input$coc.curved=="Yes"), 
                                 labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                 shape=input$coc.shape, net=values$CM$net,shadow=FALSE)
    
    values$networkCM$VIS
    
  })
  
  output$CMplot.save <- downloadHandler(
    filename = function() {
      
      paste("CouplingMap-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$CM$map, dpi = as.numeric(input$CMdpi), height = input$CMh, width = input$CMh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$CMTable <- DT::renderDT({
    CMMAP()
    cmData=values$CM$data[,c(2,1,3,5)]

    DT::datatable(cmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'CouplingMap',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'CouplingMap',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'CouplingMap',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(cmData))-1))))) %>%
      formatStyle(names(cmData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$CMTableCluster <- DT::renderDT({
    CMMAP()
    cmData=values$CM$clusters[,c(7,1:4,6)]
    
    DT::datatable(cmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'CouplingMap_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'CouplingMap_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'CouplingMap_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(cmData))-1))))) %>%
      formatStyle(names(cmData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  ### Conceptual Structure  #####
  
  ### Co-occurrences network ----
  COCnetwork <- eventReactive(input$applyCoc,{
    
    values <- cocNetwork(input,values)
    
    
    values$network<-igraph2vis(g=values$cocnet$graph,curved=(input$coc.curved=="Yes"), 
                               labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                               shape=input$coc.shape, net=values$cocnet, shadow=(input$coc.shadow=="Yes"))
    
    
  })
  
  output$cocPlot <- renderVisNetwork({  
    
    COCnetwork()
    
    values$network$VIS
    
  })
  
  output$COCStopPreview <-  renderUI({
    
    if (!is.null(values$COCremove.terms) | exists("values$COCremove.terms")){
      strPreview(values$COCremove.terms)  
    }else{
      strPreview(" ")
    }
  })
  
  output$COCSynPreview <-  renderUI({
    
    if (!is.null(values$COCsyn.terms) | exists("values$COCsyn.terms")){
      strSynPreview(values$COCsyn.terms)  
    }else{
      strSynPreview(" ")
    }
  })
  
  # output$cocPlotComm <- renderVisNetwork({  
  #   
  #   g <- splitCommunities(values$cocnet$graph, n=NULL)
  #   igraph2vis(g=g,curved=(input$coc.curved=="Yes"), 
  #                              labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
  #                              shape=input$coc.shape, net=values$cocnet)$VIS
  #   
  # })
  
  output$network.coc <- downloadHandler(
    filename = "Co_occurrence_network.net",
    content <- function(file) {
      igraph::write.graph(values$cocnet$graph_pajek,file=file, format="pajek")
      
    },
    contentType = "net"
  )
  
  ### save coc network image as html ####
  output$networkCoc.fig <- downloadHandler(
    filename = "network.html",
    content <- function(con) {
      savenetwork(con)
    },
    contentType = "html"
  )
  
  output$cocTable <- DT::renderDT({
    COCnetwork()
    cocData=values$cocnet$cluster_res
    names(cocData)=c("Node", "Cluster", "Betweenness", "Closeness", "PageRank")
    DT::datatable(cocData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"), filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'CoWord_Network_Analysis',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'CoWord_Network_Analysis',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'CoWord_Network_Analysis',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(cocData))-1))))) %>%
      formatStyle(names(cocData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  ### Degree Plot Co-word analysis ####
  output$cocDegree <- renderPlotly({
    COCnetwork()
    p <- degreePlot(values$cocnet)
    plot.ly(p)
  })
  
  
  ### Correspondence Analysis ----
  
  CSfactorial <- eventReactive(input$applyCA,{
    values <- CAmap(input,values)
  })
  
  output$CSStopPreview <-  renderUI({
    
    if (!is.null(values$CSremove.terms) | exists("values$CSremove.terms")){
      strPreview(values$CSremove.terms)  
    }else{
      strPreview(" ")
    }
  })
  
  output$FASynPreview <-  renderUI({
    
    if (!is.null(values$FAsyn.terms) | exists("values$FAsyn.terms")){
      strSynPreview(values$FAsyn.terms)  
    }else{
      strSynPreview(" ")
    }
  })
  
  output$FA1plot.save <- downloadHandler(
    filename = function() {
      
      paste("FactorialMap-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$CS$graph_terms, dpi = as.numeric(input$FAdpi), height = input$FAh, width = input$FAh*1.5, bg="white")
    },
    contentType = "png"
  )
  output$FA2plot.save <- downloadHandler(
    filename = function() {
      
      paste("Dendrogram-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$CS$graph_dendogram, dpi = as.numeric(input$FAdpi), height = input$FAh, width = input$FAh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$FA3plot.save <- downloadHandler(
    filename = function() {
      
      paste("MostContribDocuments-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$CS$graph_documents_Contrib, dpi = as.numeric(input$FAdpi), height = input$FAh, width = input$FAh*1.5, bg="white")
    },
    contentType = "png"
  )
  
  output$FA4plot.save <- downloadHandler(
    filename = function() {
      
      paste("MostCitedDocuments-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$CS$graph_documents_TC, dpi = as.numeric(input$FAdpi), height = input$FAh, width = input$FAh*1.5, bg="white")
    },
    contentType = "png"
  )
  
  output$CSPlot1 <- renderPlot({
    CSfactorial()
    plot(values$CS$graph_terms)
    
  }, width = "auto", height = reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*2/5,0)), res = 150) #height = 650, width = 800)
  
  output$CSPlot2 <- renderPlot({
    CSfactorial()
    if (input$method!="MDS"){
      
      if (values$CS[[1]][1]!="NA"){
        
        plot(values$CS$graph_documents_Contrib)
      }else{
        emptyPlot("Selected field is not included in your data collection")
      }
    }else{
      emptyPlot("This plot is available only for CA or MCA analyses")
    }
    
  }, width = "auto", height = reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*2/5,0)), res = 150) #height = 650, width = 800)
  
  output$CSPlot3 <- renderPlot({
    CSfactorial()
    if (input$method!="MDS"){
      if (values$CS[[1]][1]!="NA"){
        plot(values$CS$graph_documents_TC)
      }else{
        emptyPlot("Selected field is not included in your data collection")
      }
    }else{
      emptyPlot("This plot is available only for CA or MCA analyses")
    }
    
    
  }, width = "auto", height = reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*2/5,0)), res = 150)  #height = 650, width = 800)
  
  output$CSPlot4 <- renderPlot({
    
    CSfactorial()
    if (values$CS[[1]][1]!="NA"){
      plot(values$CS$graph_dendogram)
    }else{
      emptyPlot("Selected field is not included in your data collection")
    }
    
  }, width = "auto", height = reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*2/5,0)), res = 150) #height = 650, width = 1000)
  
  output$CSTableW <- DT::renderDT({
    CSfactorial()
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
    
    DT::datatable(WData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'CoWord_Factorial_Analysis_Words_By_Cluster',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'CoWord_Factorial_Analysis_Words_By_Cluster',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'CoWord_Factorial_Analysis_Words_By_Cluster',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(WData))-1))))) %>%
      formatStyle(names(WData),  backgroundColor = 'white')
    #return(Data)
    
  })
  
  output$CSTableD <- DT::renderDT({
    CSfactorial()
    CSData=values$CS$docCoord
    CSData=data.frame(Documents=row.names(CSData),CSData,stringsAsFactors = FALSE)
    CSData$dim1=round(CSData$dim1,2)
    CSData$dim2=round(CSData$dim2,2)
    CSData$contrib=round(CSData$contrib,2)
    DT::datatable(CSData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'CoWord_Factorial_Analysis_Articles_By_Cluster',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'CoWord_Factorial_Analysis_Articles_By_Cluster',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'CoWord_Factorial_Analysis_Articles_By_Cluster',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(CSData))-1))))) %>%
      formatStyle(names(CSData),  backgroundColor = 'white') 
    
  })
  
  ### Thematic Map ----
  TMAP <- eventReactive(input$applyTM,{
    
    if (input$TMfield %in% c("TI","AB")){
      ngrams <- as.numeric(input$TMngrams)
    }else{
      ngrams <- 1
    }
    
    ### load file with terms to remove
    if (input$TMStopFile=="Y"){
      remove.terms <- trimws(readStopwordsFile(file=input$TMStop, sep=input$TMSep))
    }else{remove.terms <- NULL}
    values$TMremove.terms <- remove.terms
    ### end of block
    ### load file with synonyms
    if (input$TMapSynFile=="Y"){
      synonyms <- trimws(readSynWordsFile(file=input$TMapSyn, sep=input$TMapSynSep))
    }else{synonyms <- NULL}
    values$TMapsyn.terms <- synonyms
    ### end of block
    
    values$TM <- thematicMap(values$M, field=input$TMfield, 
                             n=input$TMn, minfreq=input$TMfreq, ngrams=ngrams,
                             stemming=input$TMstemming, size=input$sizeTM, 
                             n.labels=input$TMn.labels, repel=FALSE, remove.terms=remove.terms, synonyms=synonyms)
    
    validate(
      need(values$TM$nclust > 0, "\n\nNo topics in one or more periods. Please select a different set of parameters.")
    )
  })
  output$TMPlot <- renderPlotly({
    
    TMAP()
    plot.ly(values$TM$map, size=0.07, aspectratio = 1.3)
    
  })#, height = 650, width = 800)
  
  output$NetPlot <- renderVisNetwork({
    TMAP()
    values$networkTM<-igraph2vis(g=values$TM$net$graph,curved=(input$coc.curved=="Yes"), 
                                 labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                 shape=input$coc.shape, net=values$TM$net)
    
    values$networkTM$VIS
    
  })
  
  output$TMStopPreview <-  renderUI({
    
    if (!is.null(values$TMremove.terms) | exists("values$TMremove.terms")){
      strPreview(values$TMremove.terms)  
    }else{
      strPreview(" ")
    }
  })
  
  output$TMapSynPreview <-  renderUI({

    if (!is.null(values$TMapsyn.terms) | exists("values$TMapsyn.terms")){
      strSynPreview(values$TMapsyn.terms)
    }else{
      strSynPreview(" ")
    }
  })
  
  output$TMplot.save <- downloadHandler(
    filename = function() {
      
      paste("ThematicMap-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
               ggsave(filename = file, plot = values$TM$map, dpi = as.numeric(input$TMdpi),  height = input$TMh, width = input$TMh*1.5, bg="white")
             },
    contentType = "png"
  )
  
  output$TMTable <- DT::renderDT({
    TMAP()
    tmData=values$TM$words[,-4]
    
    
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$TMTableCluster <- DT::renderDT({
    TMAP()
    tmData <- values$TM$clusters[,c(7,1:4,6)]
    names(tmData) <- c("Cluster", "CallonCentrality","CallonDensity","RankCentrality","RankDensity","ClusterColor") 
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  ### Thematic Evolution ----
  output$sliders <- renderUI({
    numSlices <- as.integer(input$numSlices)
    v=quantile(values$M$PY, seq(0,1,by=(1/(numSlices+1))), na.rm=TRUE)
    v=round(v[-c(1,length(v))],0)
    lapply(1:numSlices, function(i) {
      # sliderInput(inputId = paste0("Slice", i), label = paste("Cutting Year", i),
      #             min=1990,max=2018,value=1990)
      
      numericInput(inputId = paste0("Slice", i), label = paste("Cutting Year", i),value=v[i],min=min(values$M$PY, na.rm = TRUE)+1,max=max(values$M$PY, na.rm = TRUE)-1, step=1)
      #numericInput(inputId = paste0("Slice", i), label = paste("Cutting Year", i),value=median(values$M$PY),min=min(values$M$PY)+1,max=max(values$M$PY)-1, step=1)
    })
  })
  
  TEMAP <- eventReactive(input$applyTE,{
    if (input$TEfield %in% c("TI","AB")){
      ngrams <- as.numeric(input$TEngrams)
    }else{
      ngrams <- 1
    }
    
    ### load file with terms to remove
    if (input$TEStopFile=="Y"){
      remove.terms <- trimws(readStopwordsFile(file=input$TEStop, sep=input$TESep))
    }else{remove.terms <- NULL}
    values$TEremove.terms <- remove.terms
    ### end of block
    ### load file with synonyms
    if (input$TESynFile=="Y"){
      synonyms <- trimws(readSynWordsFile(file=input$TESyn, sep=input$TESynSep))
    }else{synonyms <- NULL}
    values$TEsyn.terms <- synonyms
    ### end of block
    
    values$yearSlices <- as.numeric()
    for (i in 1:as.integer(input$numSlices)){
      if (length(input[[paste0("Slice", i)]])>0){values$yearSlices=c(values$yearSlices,input[[paste0("Slice", i)]])}
    }
    
    if (length(values$yearSlices)>0){
      values$nexus <- thematicEvolution(values$M, field=input$TEfield, values$yearSlices, n = input$nTE, minFreq = input$fTE, size = input$sizeTE, 
                                        n.labels=input$TEn.labels, repel=FALSE, ngrams=ngrams, remove.terms = remove.terms, synonyms = synonyms)
      
      validate(
        need(values$nexus$check != FALSE, "\n\nNo topics in one or more periods. Please select a different set of parameters.")
      )
      
      plotThematicEvolution(Nodes = values$nexus$Nodes,Edges = values$nexus$Edges, measure = input$TEmeasure, min.flow = input$minFlowTE)
    }
  })
  
  output$TEPlot <- plotly::renderPlotly({
    
    TEMAP()
    
  })
  
  output$TEStopPreview <-  renderUI({
    
    if (!is.null(values$TEremove.terms) | exists("values$TEremove.terms")){
      strPreview(values$TEremove.terms)  
    }else{
      strPreview(" ")
    }
  })
  
  output$TESynPreview <-  renderUI({
    
    if (!is.null(values$TEsyn.terms) | exists("values$TEsyn.terms")){
      strSynPreview(values$TEsyn.terms)  
    }else{
      strSynPreview(" ")
    }
  })
  
  output$TETable <- DT::renderDT({
    TEMAP()
    TEData=values$nexus$Data
    TEData=TEData[TEData$Inc_index>0,-c(4,8)]
    names(TEData)=c("From", "To", "Words", "Weighted Inclusion Index", "Inclusion Index", "Occurrences", "Stability Index")
    DT::datatable(TEData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Evolution',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Evolution',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Evolution',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TEData))-1))))) %>%
      formatStyle(names(TEData),  backgroundColor = 'white') %>%
      formatRound(names(TEData)[4], 2) %>%
      formatRound(names(TEData)[5], 2) %>%
      formatRound(names(TEData)[7], 2) 
    #return(Data)
    
  })
  
  output$TMPlot1 <-  renderPlotly({
    TEMAP()
    #input$applyTM
    if (length(values$nexus$TM)>=1){
      plot.ly(values$nexus$TM[[1]]$map, size=0.07, aspectratio = 1.3)
    } else {emptyPlot("You have selected fewer periods!")}
    
  })#, height = 650, width = 800)
  
  output$TMPlot2 <-  renderPlotly({
    TEMAP()
    #input$applyTM
    if (length(values$nexus$TM)>=2){
      plot.ly(values$nexus$TM[[2]]$map, size=0.07, aspectratio = 1.3)
    } else {emptyPlot("You have selected fewer periods!")}
    
  })#, height = 650, width = 800)
  
  output$TMPlot3 <-  renderPlotly({
    TEMAP()
    #input$applyTM
    if (length(values$nexus$TM)>=3){
      plot.ly(values$nexus$TM[[3]]$map, size=0.07, aspectratio = 1.3)
    } else {emptyPlot("You have selected fewer periods!")}
    
  })#, height = 650, width = 800)
  
  output$TMPlot4 <-  renderPlotly({
    TEMAP()
    #input$applyTM
    if (length(values$nexus$TM)>=4){
      plot.ly(values$nexus$TM[[4]]$map, size=0.07, aspectratio = 1.3)
    } else (emptyPlot("You have selected fewer periods!"))
    
  })#, height = 650, width = 800)
  
  output$TMPlot5 <-  renderPlotly({
    TEMAP()
    #input$applyTM
    if (length(values$nexus$TM)>=5){
      plot.ly(values$nexus$TM[[5]]$map, size=0.07, aspectratio = 1.3)
    } else (emptyPlot("You have selected fewer periods!"))
    
  })#, height = 650, width = 800)
  
  output$NetPlot1 <- renderVisNetwork({
    TEMAP()
    k=1
    values$network1<-igraph2vis(g=values$nexus$Net[[k]]$graph,curved=(input$coc.curved=="Yes"), 
                                labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                shape=input$coc.shape, net=values$nexus$Net[[k]])
    
    values$network1$VIS
    
  })
  
  output$NetPlot2 <- renderVisNetwork({
    TEMAP()
    k=2
    values$network2<-igraph2vis(g=values$nexus$Net[[k]]$graph,curved=(input$coc.curved=="Yes"), 
                                labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                shape=input$coc.shape, net=values$nexus$Net[[k]])
    
    values$network2$VIS
    
  })
  
  output$NetPlot3 <- renderVisNetwork({
    TEMAP()
    k=3
    values$network3<-igraph2vis(g=values$nexus$Net[[k]]$graph,curved=(input$coc.curved=="Yes"), 
                                labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                shape=input$coc.shape, net=values$nexus$Net[[k]])
    
    values$network3$VIS
    
  })
  
  output$NetPlot4 <- renderVisNetwork({
    TEMAP()
    k=4
    values$network4<-igraph2vis(g=values$nexus$Net[[k]]$graph,curved=(input$coc.curved=="Yes"), 
                                labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                shape=input$coc.shape, net=values$nexus$Net[[k]])
    
    values$network4$VIS
    
  })
  
  output$NetPlot5 <- renderVisNetwork({
    TEMAP()
    k=5
    values$network5<-igraph2vis(g=values$nexus$Net[[k]]$graph,curved=(input$coc.curved=="Yes"), 
                                labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                shape=input$coc.shape, net=values$nexus$Net[[k]])
    
    values$network5$VIS
    
  })
  
  output$TMTable1 <- DT::renderDT({
    TEMAP()
    
    tmData=values$nexus$TM[[1]]$words[,-4]
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_1',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_1',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_1',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$TMTable2 <- DT::renderDT({
    TEMAP()
    
    tmData=values$nexus$TM[[2]]$words[,-4]
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_2',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_2',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_2',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$TMTable3 <- DT::renderDT({
    TEMAP()
    
    tmData=values$nexus$TM[[3]]$words[,-4]
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_3',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_3',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_3',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$TMTable4 <- DT::renderDT({
    TEMAP()
    
    tmData=values$nexus$TM[[4]]$words[,-4]
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_4',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_4',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_4',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$TMTable5 <- DT::renderDT({
    TEMAP()
    
    tmData=values$nexus$TM[[5]]$words[,-4]
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_5',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_5',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_5',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$TMTableCluster1 <- DT::renderDT({
    TEMAP()
    tmData <- values$nexus$TM[[1]]$clusters[,c(7,1:4,6)]
    names(tmData) <- c("Cluster", "CallonCentrality","CallonDensity","RankCentrality","RankDensity","ClusterColor") 
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$TMTableCluster2 <- DT::renderDT({
    TEMAP()
    tmData <- values$nexus$TM[[2]]$clusters[,c(7,1:4,6)]
    names(tmData) <- c("Cluster", "CallonCentrality","CallonDensity","RankCentrality","RankDensity","ClusterColor") 
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$TMTableCluster3 <- DT::renderDT({
    TEMAP()
    tmData <- values$nexus$TM[[3]]$clusters[,c(7,1:4,6)]
    names(tmData) <- c("Cluster", "CallonCentrality","CallonDensity","RankCentrality","RankDensity","ClusterColor") 
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$TMTableCluster4 <- DT::renderDT({
    TEMAP()
    tmData <- values$nexus$TM[[4]]$clusters[,c(7,1:4,6)]
    names(tmData) <- c("Cluster", "CallonCentrality","CallonDensity","RankCentrality","RankDensity","ClusterColor") 
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$TMTableCluster5 <- DT::renderDT({
    TEMAP()
    tmData <- values$nexus$TM[[5]]$clusters[,c(7,1:4,6)]
    names(tmData) <- c("Cluster", "CallonCentrality","CallonDensity","RankCentrality","RankDensity","ClusterColor") 
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  ### INTELLECTUAL STRUCTURE ####
  
  ### Co-citation network ----
  COCITnetwork <- eventReactive(input$applyCocit,{
    
    values <- intellectualStructure(input,values)
    #dev.off();file.remove(t) ### end of trick
    
    values$network<-igraph2vis(g=values$cocitnet$graph,curved=(input$cocit.curved=="Yes"), 
                               labelsize=input$citlabelsize, opacity=0.7,type=input$citlayout,
                               shape=input$cocit.shape, net=values$cocitnet, shadow=(input$cocit.shadow=="Yes"))
  })
  
  output$cocitPlot <- renderVisNetwork({  
    
    COCITnetwork()
    
    isolate(values$network$VIS)
    
  })
  
  output$network.cocit <- downloadHandler(
    filename = "Co_citation_network.net",
    content <- function(file) {
      igraph::write.graph(values$cocitnet$graph_pajek,file=file, format="pajek")
      #rio::export(values$M, file=file)
    },
    contentType = "net"
  )
  
  output$cocitTable <- DT::renderDT({
    COCITnetwork()
    cocitData=values$cocitnet$cluster_res
    names(cocitData)=c("Node", "Cluster", "Betweenness", "Closeness", "PageRank")
    DT::datatable(cocitData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'CoCitation_Network',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'CoCitation_Network',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'CoCitation_Network',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(cocitData))-1))))) %>%
      formatStyle(names(cocitData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  ### save coc network image as html ####
  output$networkCocit.fig <- downloadHandler(
    filename = "network.html",
    content <- function(con) {
      savenetwork(con)
    },
    contentType = "html"
  )
  
  ### Degree Plot Co-citation analysis ####
  output$cocitDegree <- renderPlotly({
    COCITnetwork()
    p <- degreePlot(values$cocitnet)
    plot.ly(p)
  })
  
  ### Historiograph ----
  Hist <- eventReactive(input$applyHist,{
    
    withProgress(message = 'Calculation in progress',
                 value = 0, {
                   values <- historiograph(input,values)
                 })
    
    fx <- list(
      family = "Old Standard TT, serif",
      size = 11,
      color = "black"
    )
    
    a <- list(
      ticks = "outside",
      autotick = FALSE,
      ticktext = values$histPlot$axis$label, 
      tickvals = values$histPlot$axis$values,
      tickmode = "array",
      showticklabels = TRUE,
      tickangle = 270,
      tickfont = fx,
      ticklen = 2,
      tickwidth = 2,
      tickcolor = toRGB("black")
    )
    
    g <- plot.ly(values$histPlot$g, side="r", size=0.05, aspectratio = 1.5, height=-0.1) %>% 
      layout(xaxis = a, autosize=TRUE ,showlegend = FALSE, 
             hoverlabel = list(font=list(size=input$histlabelsize+9)))
    return(g)
  })
  
  output$HGplot.save <- downloadHandler(
    filename = function() {
      
      paste("Historiograph-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$histPlot$g, dpi = as.numeric(input$HGdpi),  height = input$HGh, width = input$HGh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$histPlot <- renderPlotly({
    
    Hist()

  })
  output$histTable <- DT::renderDT({
    LCS=values$histResults$LCS
    s=sort(LCS,decreasing = TRUE)[input$histNodes]
    ind=which(LCS>=s)
    Data=values$histResults$histData
    Data=Data[ind,]
    Data$DOI<- paste0('<a href=\"https://doi.org/',Data$DOI,'\" target=\"_blank\">',Data$DOI,'</a>')
    DT::datatable(Data, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Historiograph_Network',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Historiograph_Network',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Historiograph_Network',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(Data))-1))))) %>%
      formatStyle(names(Data),  backgroundColor = 'white') %>%
      formatStyle(
        'GCS',
        background = styleColorBar(Data$GCS, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'LCS',
        background = styleColorBar(Data$LCS, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    #return(Data)
    
  })
  
  ### SOCIAL STRUCTURE ####
  ### Collaboration network ----
  COLnetwork <- eventReactive(input$applyCol,{
    
    values <- socialStructure(input,values)
    
    values$network<-igraph2vis(g=values$colnet$graph,curved=(input$soc.curved=="Yes"), 
                               labelsize=input$collabelsize, opacity=input$colAlpha,type=input$collayout,
                               shape=input$col.shape, net=values$colnet, shadow=(input$col.shadow=="Yes"))
  })
  output$colPlot <- renderVisNetwork({  
    
    COLnetwork()
    
    values$network$VIS
    
  })
  
  output$network.col <- downloadHandler(
    filename = "Collaboration_network.net",
    content <- function(file) {
      igraph::write.graph(values$colnet$graph_pajek,file=file, format="pajek")
      #rio::export(values$M, file=file)
    },
    contentType = "net"
  )
  
  output$colTable <- DT::renderDT({
    COLnetwork()
    colData=values$colnet$cluster_res
    names(colData)=c("Node", "Cluster", "Betweenness", "Closeness", "PageRank")
    
    DT::datatable(colData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"), filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Collaboration_Network',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Collaboration_Network',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Collaboration_Network',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(colData))-1))))) %>%
      formatStyle(names(colData),  backgroundColor = 'white') 
    #return(Data)
    
  })    
  ### save coc network image as html ####
  output$networkCol.fig <- downloadHandler(
    filename = "network.html",
    content <- function(con) {
      savenetwork(con)
    },
    contentType = "html"
  )
  
  
  ### Degree Plot Collaboration analysis ####
  output$colDegree <- renderPlotly({
    COLnetwork()
    p <- degreePlot(values$colnet)
    plot.ly(p)
  })
  
  ### WPPlot ----
  WMnetwork<- eventReactive(input$applyWM,{
    values$WMmap=countrycollaboration(values$M,label=FALSE,edgesize=input$WMedgesize/2,min.edges=input$WMedges.min)
  })
  
  output$CCplot.save <- downloadHandler(
    filename = function() {
      
      paste("CountryCollaborationMap-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$WMmap$g, dpi = as.numeric(input$CCdpi),  height = input$CCh, width = input$CCh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$WMPlot<- renderPlot({
    
    WMnetwork()  
    plot(values$WMmap$g)
    
  }, width = "auto", height = reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*2/5,0)), res = 150) #height = 750)#, width = 750
  
  output$WMTable <- DT::renderDT({
    WMnetwork()  
    colData=values$WMmap$tab
    colData=colData[,c(1,2,9)]
    names(colData)=c("From","To","Frequency")
    
    DT::datatable(colData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"), filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'World_Collaboration_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'World_Collaboration_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'World_Collaboration_Map',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(colData))-1))))) %>%
      formatStyle(names(colData),  backgroundColor = 'white') 
    #return(Data)
    
  }) 
  
  ### COMMON FUNCTIONS ####
  
  
  # displayResolution <- function() {
  #   session$clientData$output_plot1_width
  # }
  
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
  
  # from ggplot to plotly
  plot.ly <- function(g, flip=FALSE, side="r", aspectratio=1, size=0.15,data.type=2, height=0){
    
    a <- ggplot_build(g)$data
    
    ymin <- unlist(lapply(a, function(l){
      if ("y" %in% names(l)){
        min(l["y"])  
      }
    })) %>% min(na.rm=TRUE)
    
    ymax <- unlist(lapply(a, function(l){
      if ("y" %in% names(l)){
        max(l["y"])  
      }
    })) %>% max(na.rm=TRUE)
    
    xmin <- unlist(lapply(a, function(l){
      if ("x" %in% names(l)){
        min(l["x"])  
      }
    })) %>% min(na.rm=TRUE)
    
    xmax <- unlist(lapply(a, function(l){
      if ("x" %in% names(l)){
        max(l["x"])  
      }
    })) %>% max(na.rm=TRUE)

    if (isTRUE(flip)){
      xrange <- c(ymin,ymax)
      yrange <- c(xmin,xmax)
    }else{
      yrange <- c(ymin,ymax)
      xrange <- c(xmin,xmax)
    }
        
    # if (isTRUE(flip)){
    #   yrange <- range(ggplot_build(g)$data[[data.type]]$x)
    #   xrange <- range(ggplot_build(g)$data[[data.type]]$y)
    # }else{
    #   xrange <- range(ggplot_build(g)$data[[data.type]]$x)
    #   yrange <- range(ggplot_build(g)$data[[data.type]]$y)
    # }
    
    sizex = diff(xrange)*size
    sizey = diff(yrange)*size*aspectratio
    
    y <- min(yrange)+0.2
    
    if (side=="l"){
      x <- min(xrange)+0.2
    }else{
      x <- max(xrange)-0.2-sizex
    }
    
    ggplotly(g, tooltip = "text") %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c(
               'sendDataToCloud',
               'pan2d', 
               'select2d', 
               'lasso2d',
               'toggleSpikelines',
               'hoverClosestCartesian',
               'hoverCompareCartesian'
             )) %>%
      layout(
        images = list(
          source = raster2uri(as.raster(values$logo)),
          x = x, y = y+height,
          sizex = sizex, sizey = sizey,
          xref = "x", yref = "y",
          xanchor = "left", yanchor = "bottom",
          sizing = "stretch"
        )
      )
  }
  
  freqPlot <- function(xx,x,y, textLaby,textLabx, title){
    

    xl <- c(max(xx[,x])-0.02-diff(range(xx[,x]))*0.125, max(xx[,x])-0.02)+1
    yl <- c(1,1+length(unique(xx[,y]))*0.125)
    
    Text <- paste(textLaby,": ",xx[,y],"\n",textLabx, ": ",xx[,x])
    
    g <- ggplot(xx, aes(x =xx[,x], y = xx[,y], label = xx[,x], text=Text)) +
      geom_segment(aes(x = 0, y = xx[,y], xend = xx[,x], yend = xx[,y]), color = "grey50") +
      geom_point(aes(color=-xx[,x], size=xx[,x]), show.legend = FALSE) +
      scale_radius(range=c(7, 15))+
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
  Hindex_plot <- function(values, type){
    
    hindex<-function(values,type){
      
      switch(type,
             author={
               AU <- trim(gsub(",","",names(tableTag(values$M,"AU"))))
               values$H <- Hindex(values$M, field = "author", elements = AU, sep = ";", years=Inf)$H
             },
             source={
               SO <- names(sort(table(values$M$SO),decreasing = TRUE))
               values$H <- Hindex(values$M, field = "source", elements = SO, sep = ";", years=Inf)$H
             }
      )
      
      return(values)
    }
    
    values<-hindex(values, type = type)
    
    xx=values$H
    if (type=="author"){
      K=input$Hkauthor
      measure=input$HmeasureAuthors
      title="Author Local Impact"
      xn="Authors"
    } else {
      K=input$Hksource
      measure=input$HmeasureSources
      title="Source Local Impact"
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
    
    
    g <- freqPlot(xx,x=2,y=1, textLaby = "Authors", textLabx = paste("Impact Measure:",toupper(measure)), title = paste(title,"by",toupper(measure),"index"))
    
    res<-list(values=values,g=g)
    return(res)
  }
  
  descriptive <- function(values,type){
    if (values$results[[1]]=="NA"){
      values$results=biblioAnalysis(values$M)}
    if (values$S[[1]][1]=="NA"){
      values$S=summary(values$results,k=Inf,verbose=FALSE)}
    
    switch(type,
           "tab1"={
             #TAB=data.frame(Information=gsub("[[:digit:]]", "", S$MainInformation), Data=gsub("[^0-9]", "", S$MainInformation))
             TAB=data.frame(values$S$MainInformationDF)
             #cat(S$MainInformation)
           },
           "tab2"={
             
             TAB=values$S$AnnualProduction
             names(TAB)=c("Year","Articles")
             #print(S$AnnualProduction)
             #cat("\n\n")
             #cat("Annual Growth Rate ",round(S$AnnualGrowthRate, digits=2),"%")
           },
           "tab3"={
             #TAB=values$S$MostProdAuthors
             AU <- data.frame(Author=names(values$results$Authors), 
                              freq=as.numeric(values$results$Authors), 
                              stringsAsFactors = FALSE)
             TAB <- dplyr::left_join(AU,values$results$AuthorsFrac)
             names(TAB)=c("Authors","Articles","Articles Fractionalized")
             #print(S$MostProdAuthors)
           },
           "tab4"={
             TAB=values$S$MostCitedPapers
             names(TAB)=c("Paper", "DOI","Total Citations","TC per Year","Normalized TC")
             #print(S$MostCitedPapers)
           },
           "tab5"={
             TAB=values$S$MostProdCountries
             #print(S$MostProdCountries)
           },
           "tab6"={
             TAB=values$S$TCperCountries
             #print(S$TCperCountries)
           },
           "tab7"={
             TAB=values$S$MostRelSources
             #print(S$MostRelSources)
           },
           
           "tab10"={
             TAB<-mapworld(values$M)$tab
           },
           "tab11"={
             TAB=as.data.frame(values$results$Affiliations,stringsAsFactors = FALSE)
             names(TAB)=c("Affiliations", "Articles")
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
  
  wordlist <- function(M, Field, n, measure, ngrams, remove.terms=NULL, synonyms=NULL){
    switch(Field,
           ID={v=tableTag(values$M,"ID", remove.terms  = remove.terms, synonyms = synonyms)},
           DE={v=tableTag(values$M,"DE", remove.terms = remove.terms, synonyms = synonyms)},
           TI={
             if (!("TI_TM" %in% names(M))){
               v=tableTag(M,"TI", ngrams=ngrams, remove.terms=remove.terms, synonyms = synonyms)
               
             }},
           AB={if (!("AB_TM" %in% names(M))){
             v=tableTag(M,"AB", ngrams=ngrams, remove.terms = remove.terms, synonyms = synonyms)
           }}
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
  
  mapworld <- function(M){
    if (!("AU_CO" %in% names(M))){M=metaTagExtraction(M,"AU_CO")}
    CO=as.data.frame(tableTag(M,"AU_CO"),stringsAsFactors = FALSE)
    CO$Tab=gsub("UNITED KINGDOM","UK",CO$Tab)
    CO$Tab=gsub("KOREA","SOUTH KOREA",CO$Tab)
    
    map.world <- map_data("world")
    map.world$region=toupper(map.world$region)
    
    dplyr::anti_join(CO, map.world, by = c('Tab' = 'region'))
    
    country.prod <- dplyr::left_join( map.world, CO, by = c('region' = 'Tab')) 
    
    tab=data.frame(country.prod %>%
                     dplyr::group_by(region) %>%
                     dplyr::summarise(Freq=mean(Freq)))
    
    tab=tab[!is.na(tab$Freq),]
    
    tab=tab[order(-tab$Freq),]
    
    breaks=as.numeric(round(quantile(CO$Freq,c(0.2,0.4,0.6,0.8,1))))
    names(breaks)=breaks
    breaks=log(breaks)
    
    g <- ggplot(country.prod, aes( x = .data$long, y = .data$lat, group=.data$group, text=paste("Country: ",.data$region,"\nN.of Documents: ",.data$Freq))) +
      geom_polygon(aes(fill = log(Freq), group=group)) +
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
        
        
      }else{emptyPlot("Selected field is not included in your data collection")
        values$CS=list("NA")}
      
    }else{
      emptyPlot("Selected field is not included in your data collection")
      values$CS=list("NA")
      
    }
  }
  
  historiograph <- function(input,values){
    
    min.cit <- 1
    # if (input$histsearch=="FAST"){
    #   min.cit=quantile(values$M$TC,0.75, na.rm = TRUE)
    # }else{min.cit=1}
    
    if (values$Histfield=="NA"){
      values$histResults <- histNetwork(values$M, min.citations=min.cit, sep = ";")
      values$Histfield="done"
    }
    titlelabel <- input$titlelabel=="TRUE"
    values$histlog<- (values$histPlot <- histPlot(values$histResults, n=input$histNodes, size =input$histsize, labelsize = input$histlabelsize, title_as_label = titlelabel, verbose=FALSE))
    return(values)
  }
  
  
  ### Network functions ----
  
  degreePlot <- function(net){
    deg <- data.frame(node = names(net$nodeDegree), x= (1:length(net$nodeDegree)), y = net$nodeDegree)
    p <- ggplot(data = deg, aes(x=.data$x, y=.data$y, 
                                text=paste("Node ",.data$x," - Degree ",.data$y, sep="")))+
      geom_point()+
      geom_line(aes(group="NA"),color = '#002F80', alpha = .5) +
      theme(text = element_text(color = "#444444")
            ,panel.background = element_rect(fill = '#EFEFEF')
            ,panel.grid.minor = element_line(color = '#FFFFFF')
            ,panel.grid.major = element_line(color = '#FFFFFF')
            ,plot.title = element_text(size = 24)
            ,axis.title = element_text(size = 14, color = '#555555')
            ,axis.title.y = element_text(vjust = 1, angle = 0)
            ,axis.title.x = element_text(hjust = 0)
      ) + 
      labs(x = "Node", y="Degree", title = "Node Degrees")
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
        
        #bluefunc <- colorRampPalette(c("lightblue", "darkblue"))
        #col=bluefunc((diff(range(df$year))+1)*10)
        col=heat.colors((diff(range(df$year))+1)*10)
        igraph::V(g)$color=col[(max(df$year)-df$year+1)*10]
        igraph::V(g)$year=df$year
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
  
  countrycollaboration <- function(M,label,edgesize,min.edges){
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
      geom_polygon(aes(fill = log(Freq))) +
      scale_fill_continuous(low='dodgerblue', high='dodgerblue4',breaks=breaks)+
      #guides(fill = guide_legend(reverse = T)) +
      guides(colour=FALSE, fill=FALSE)+
      geom_curve(data=COedges, aes(x = .data$Longitude.x , y = .data$Latitude.x, xend = .data$Longitude.y, yend = .data$Latitude.y,     # draw edges as arcs
                                   color = "firebrick4", size = .data$count, group=.data$continent.x),
                 curvature = 0.33,
                 alpha = 0.5) +
      labs(title = "Country Collaboration Map", x = "Latitude", y = "Longitude")+
      scale_size_continuous(guide = FALSE, range = c(0.25, edgesize))+
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
  
  savenetwork <- function(con){
    vn=values$network$vn
    visNetwork(nodes = vn$nodes, edges = vn$edges, type="full", smooth=TRUE, physics=FALSE, height = "2000px",width = "2000px" ) %>%
      visNodes(shape="box", font=list(color="black"),scaling=list(label=list(enables=TRUE))) %>%
      visIgraphLayout(layout = values$network$l) %>%
      visEdges(smooth = values$network$curved) %>%
      visOptions(highlightNearest =list(enabled = T, hover = T, degree=1), nodesIdSelection = T) %>%
      visInteraction(dragNodes = TRUE, navigationButtons = TRUE, hideEdgesOnDrag = TRUE)  %>% visExport() %>%
      visPhysics(enabled = FALSE) %>% visSave(con)
  }
  
  igraph2vis<-function(g,curved,labelsize,opacity,type,shape, net, shadow=TRUE){
    
    LABEL=igraph::V(g)$name
    
    LABEL[igraph::V(g)$labelsize==0]=""
    
    vn <- toVisNetworkData(g)
    
    vn$nodes$label=LABEL
    vn$edges$num=1
    vn$edges$dashes=FALSE
    vn$edges$dashes[vn$edges$lty==2]=TRUE
    
    ## opacity
    vn$nodes$color=adjustcolor(vn$nodes$color,alpha=min(c(opacity+0.2,1)))
    vn$edges$color=adjustcolor(vn$edges$color,alpha=opacity)
    
    ## removing multiple edges
    vn$edges=unique(vn$edges)
    
    ## labelsize
    scalemin=20
    scalemax=150
    Min=min(vn$nodes$font.size)
    Max=max(vn$nodes$font.size)
    if (Max>Min){
      size=(vn$nodes$font.size-Min)/(Max-Min)*15*labelsize+10
    } else {size=10*labelsize}
    size[size<scalemin]=scalemin
    size[size>scalemax]=scalemax
    vn$nodes$font.size=size
    l<-netLayout(type)
    
    ### TO ADD SHAPE AND FONT COLOR OPTIONS
    coords <- net$layout
    
    vn$nodes$size <- vn$nodes$font.size*0.8
    
    if (shape %in% c("text")){
      vn$nodes$font.color <- vn$nodes$color
    }else{
      vn$nodes$font.color <- "black"
    }
    
    if (shape %in% c("dot","square")){
      vn$nodes$font.vadjust <- -0.7*vn$nodes$font.size
    }else{
      vn$nodes$font.vadjust <-0
      }
    
    VIS<-
      visNetwork(nodes = vn$nodes, edges = vn$edges, type="full", smooth=TRUE, physics=FALSE) %>%
      #visNodes(shape=shape, font=list(color="black")) %>%
      visNodes(shadow=shadow, shape=shape, font=list(color="black", size=vn$nodes$font.size,vadjust=vn$nodes$font.vadjust)) %>%
      visIgraphLayout(layout = "layout.norm", layoutMatrix = coords) %>%
      visEdges(smooth = curved) %>%
      visOptions(highlightNearest =list(enabled = T, hover = T, degree=1), nodesIdSelection = T) %>%
      visInteraction(dragNodes = TRUE, navigationButtons = TRUE, hideEdgesOnDrag = TRUE) %>%
      visOptions(manipulation = TRUE) %>%
      visExport(type = "png", name = "network",
                label = paste0("Export graph as png"), background = "#fff",
                float = "right", style = NULL, loadDependencies = TRUE)
    values$COCVIS=VIS
    return(list(VIS=VIS,vn=vn, type=type, l=l, curved=curved))
  }
  
} ## End of Server
