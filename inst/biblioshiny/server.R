# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  ## stop the R session
  session$onSessionEnded(stopApp)
  ##
  
  ## file upload max size
  options(shiny.maxRequestSize=200*1024^2) 
  
  ### initial values ####
  values = reactiveValues()
  values$results=list("NA")
  values$log="working..."
  values$load="FALSE"
  values$field="NA"
  values$citField=values$colField=values$citSep="NA"
  values$NetWords=values$NetRefs=values$ColNetRefs=matrix(NA,1,1)
  values$Title="Network"
  values$Histfield="NA"
  values$histlog="working..."
  values$kk=0
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
  
  
  
  
  
  ### LOAD MENU ####
  DATAloading<- eventReactive(input$applyLoad,{
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (!is.null(inFile) & input$load=="import") {
      ext <- getFileNameExtension(inFile$datapath)
      switch(
        input$dbsource,
        isi = {
          switch(ext,
                 ###  WoS ZIP Files
                 zip = {
                   D = unzip(inFile$datapath)
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(D,
                                                  dbsource = input$dbsource,
                                                  format = input$format)
                                })
                 },
                 ### WoS Txt/Bib Files
                 {
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(inFile$datapath,
                                                  dbsource = input$dbsource,
                                                  format = input$format)
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
                                                  format = input$format)
                                })
                 },
                 ### Scopus CSV/Bib Files
                 csv = {
                   
                   #D = readFiles(inFile$datapath)
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(inFile$datapath,
                                                  dbsource = input$dbsource,
                                                  format = "csv")
                                })
                 },
                 bib = {
                   
                   #D = readFiles(inFile$datapath)
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(inFile$datapath,
                                                  dbsource = input$dbsource,
                                                  format = "bibtex")
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
                                                  format = "plaintext")
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
                   files = unzip(inFile$datapath)
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <-
                                    convert2df(files,
                                               dbsource = input$dbsource,
                                               format = input$format)
                                })
                 },
                 ### Dimensions Xlsx/csv Files
                 xlsx = {
                   #D = readFiles(inFile$datapath)
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
                   #D = readFiles(inFile$datapath)
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
               M <- rio::import(inFile$datapath)
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
               load(inFile$datapath)
             },
             rda={
               load(inFile$datapath)
             },
             rds={
               load(inFile$datapath)
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
  
  output$collection.save <- downloadHandler(
    filename = function() {
      
      paste("Bibliometrix-Export-File-", Sys.Date(), ".",input$save_file, sep="")
    },
    content <- function(file) {
      switch(input$save_file,
             xlsx={suppressWarnings(rio::export(values$M, file=file))},
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
             xlsx={suppressWarnings(rio::export(values$M, file=file))},
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
  
  ### API MENU ####
  ### API MENU: Dimensions ####
  
  ### Dimensions modal ####
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
  
  # Show modal when button is clicked.
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
  
  ### API MENU: PubMed ####
  ### PubMed modal ####
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
  ### API MENU: Content Download ####
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
    #     if (nrow(values$M) == 1 & ncol(values$M) == 1) {
    #   M <- data.frame(Message="Waiting for data")
    # }else{
    contentTable(values)
    #}
    
  })
  
  ### function returns a formatted data.table
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
  
  
  ### FILTERS MENU ####
  ### Filters uiOutput
  output$textDim <- renderUI({
    dimMatrix=paste("Documents ",dim(values$M)[1]," of ",dim(values$Morig)[1])
    textInput("textDim", "Number of Documents", 
              value=dimMatrix)
  })
  
  output$selectType <- renderUI({
    
    artType=sort(unique(values$Morig$DT))
    selectInput("selectType", "Document Type", 
                choices = artType,
                selected = artType,
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
  
  output$sliderTC <- renderUI({
    
    sliderInput("sliderTC", "Total Citation", min = min(values$Morig$TC, na.rm=T),
                max = max(values$Morig$TC, na.rm=T), value = c(min(values$Morig$TC, na.rm=T),max(values$Morig$TC,na.rm=T)))
  })
  ### End Filters uiOutput
  
  DTfiltered <- eventReactive(input$applyFilter,{
    M=values$Morig
    B=bradford(M)$table
    M=subset(M, M$PY>=input$sliderPY[1] & M$PY<=input$sliderPY[2])
    M=subset(M, M$TC>=input$sliderTC[1] & M$TC<=input$sliderTC[2])
    M=subset(M, M$DT %in% input$selectType)
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
  
  
  
  ### DATASET MENU ####
  
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
      )
    values$ASPplot <- g
    plot.ly(g)
  })#, height = 500, width =900)
  
  output$ASPplot.save <- downloadHandler(
    filename = function() {
      
      paste("AnnualScientificProduction-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$ASPplot, dpi = as.numeric(input$ASPdpi))
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
    
    g=ggplot(Table2, aes(x = .data$Year, y =.data$MeanTCperYear,text=paste("Year: ",.data$Year,"\nAverage Citations per Year: ",round(.data$MeanTCperYear,1)))) +
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
      )
    values$ACpYplot <- g
    plot.ly(g)
  })
  
  output$ACpYplot.save <- downloadHandler(
    filename = function() {
      
      paste("AverageArticleCitationPerYear-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$ACpYplot, dpi = as.numeric(input$ACpYdpi))
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
  
  TFP <- eventReactive(input$apply3F,{
    fields=c(input$LeftField, input$CentralField, input$RightField)
    threeFieldsPlot(values$M, fields=fields,n=c(input$LeftFieldn, input$CentralFieldn,input$RightFieldn), width=1200,height=600)
  })
  
  output$ThreeFielsPlot <- networkD3::renderSankeyNetwork({
    TFP()  
  })
  
  ### SOURCES MENU ####
  
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
    
    
    g=ggplot2::ggplot(data=xx, aes(x=.data$Sources, y=.data$Articles, fill=-.data$Articles,text=paste("Source: ",.data$Sources,"\nN. of Documents: ",.data$Articles))) +
      geom_bar(aes(group="NA"),stat="identity")+
      scale_fill_continuous(type = "gradient")+
      scale_x_discrete(limits = rev(xx$Sources))+
      labs(title="Most Relevant Sources", x = "Sources")+
      labs(y = "N. of Documents")+
      theme_minimal()+
      guides(fill=FALSE)+
      coord_flip()
    
    values$MRSplot <- g
    return(g)
  })
  
  output$MRSplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostRelevantSources-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MRSplot, dpi = as.numeric(input$MRSdpi))
    },
    contentType = "png"
  )
  
  
  output$MostRelSourcesPlot <- renderPlotly({
    g <- MRSources()
    plot.ly(g)
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
    
    
    g=ggplot2::ggplot(data=xx, aes(x=.data$Sources, y=.data$Articles, fill=-.data$Articles,text=paste("Source: ",.data$Sources,"\nN. of Documents: ",.data$Articles))) +
      geom_bar(aes(group="NA"),stat="identity")+
      scale_fill_continuous(type = "gradient")+
      scale_x_discrete(limits = rev(xx$Sources))+
      labs(title="Most Cited Sources", x = "Sources")+
      labs(y = "N. of Documents")+
      theme_minimal()+
      guides(fill=FALSE)+
      coord_flip()
    values$MLCSplot <- g
    return(g)
  })
  
  output$MLCSplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostLocalCitedSources-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MLCSplot, dpi = as.numeric(input$MLCSdpi))
    },
    contentType = "png"
  )
  
  output$MostRelCitSourcesPlot <- renderPlotly({
    
    g <- MLCSources()
    
    plot.ly(g)
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
  
  output$bradfordPlot <- renderPlotly({
    
    values$bradford=bradford(values$M)
    plot.ly(values$bradford$graph)
    
  })#,height = 600)
  
  output$BLplot.save <- downloadHandler(
    filename = function() {
      
      paste("BradfordLaws-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$bradford$graph, dpi = as.numeric(input$BLdpi))
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
  
  Hsource <- eventReactive(input$applyHsource,{
    withProgress(message = 'Calculation in progress',
                 value = 0, {
                   res <- Hindex_plot(values,type="source")
                 })
    values$SIplot <- res$g
    plot.ly(res$g)
  })
  
  output$SIplot.save <- downloadHandler(
    filename = function() {
      
      paste("SourceImpact-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$SIplot, dpi = as.numeric(input$SIdpi))
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
  
  SOGrowth <- eventReactive(input$applySOGrowth,{
    if (input$SOse=="Yes"){se=TRUE}else{se=FALSE}
    
    if (input$cumSO=="Cum"){
      cdf=TRUE
      laby="Cumulate occurrences (loess smoothing)"
    }else{
      cdf=FALSE
      laby="Annual occurrences (loess smoothing)"} 
    
    values$PYSO=sourceGrowth(values$M,input$topSO, cdf=cdf)
    
    term=names(values$PYSO)[-1]
    term=rep(term,each=dim(values$PYSO)[1])
    n=dim(values$PYSO)[1]*(dim(values$PYSO)[2]-1)
    freq=matrix(as.matrix(values$PYSO[,-1]),n,1)
    values$SODF=data.frame(Year=rep(values$PYSO$Year,(dim(values$PYSO)[2]-1)),Source=term, Freq=freq, stringsAsFactors = TRUE)
    
    g=ggplot(values$SODF)+
      geom_smooth(aes(x=values$SODF$Year,y=values$SODF$Freq, group=values$SODF$Source, color=values$SODF$Source),se=se, method = "loess", formula="y ~ x")+
      labs(x = 'Year'
           , y = laby
           , title = "Source Growth") +
      #ylim(0, NA) +
      scale_x_continuous(breaks= (values$PYSO$Year[seq(1,length(values$PYSO$Year),by=ceiling(length(values$PYSO$Year)/20))])) +
      geom_hline(aes(yintercept=0, alpha=0.1))+
      theme(text = element_text(color = "#444444"), legend.position="none"
            ,plot.caption = element_text(size = 9, hjust = 0.5, color = "black", face = "bold")
            ,panel.background = element_rect(fill = '#EFEFEF')
            ,panel.grid.minor = element_line(color = '#FFFFFF')
            ,panel.grid.major = element_line(color = '#FFFFFF')
            ,plot.title = element_text(size = 24)
            ,axis.title = element_text(size = 14, color = '#555555')
            ,axis.title.y = element_text(vjust = 1, angle = 90)
            ,axis.title.x = element_text(hjust = 0.95, angle = 0)
            ,axis.text.x = element_text(size=10)
      )
    
    DFsmooth=(ggplot_build(g)$data[[1]])
    DFsmooth$group=factor(DFsmooth$group, labels=levels(values$SODF$Source))
    
    maximum=sort(unique(DFsmooth$x),decreasing=TRUE)[2]
    DF2=subset(DFsmooth, x == maximum)
    g=g+
      ggrepel::geom_text_repel(data = DF2, aes(label = DF2$group, colour = DF2$group, x =DF2$x, y = DF2$y), hjust = -.1)
    values$SDplot <- g
    return(g)
    #suppressWarnings(plot(g))
  })
  
  output$SDplot.save <- downloadHandler(
    filename = function() {
      
      paste("SourceDynamics-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$SDplot, dpi = as.numeric(input$SDdpi))
    },
    contentType = "png"
  )
  
  output$soGrowthPlot <- renderPlot({
    
    g <- SOGrowth()
    plot(g)
    #suppressWarnings(plot(g))
    
    
  }, width = "auto", height = reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*2/5,0)), res = 150) #height = 600, width = 900)
  
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
  
  ### AUTHORS MENU ####
  ### Authors ----
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
    g=ggplot2::ggplot(data=xx, aes(x=xx[,1], y=xx[,2], fill=-xx[,2], text=paste("Author: ",xx[,1],"\n",lab,": ",xx[,2]))) +
      geom_bar(aes(group="NA"),stat="identity")+
      scale_fill_continuous(type = "gradient")+
      scale_x_discrete(limits = rev(xx[,1]))+
      labs(title="Most Relevant Authors", x = "Authors")+
      labs(y = lab)+
      theme_minimal() +
      guides(fill=FALSE)+
      coord_flip()
    values$MRAplot <- g
    return(g)
  })
  
  output$MRAplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostRelevantAuthors-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MRAplot, dpi = as.numeric(input$MRAdpi))
    },
    contentType = "png"
  )
  
  output$MostRelAuthorsPlot <- renderPlotly({
    
    g <- MRAuthors()
    plot.ly(g)
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
  
  MLCAuthors <- eventReactive(input$applyMLCAuthors,{
    res <- descriptive(values,type="tab13")
    values <-res$values
    values$TABAuCit<-values$TAB
    
    #xx=as.data.frame(values$results$Authors, stringsAsFactors = FALSE)
    xx <- values$TABAuCit
    lab <- "Citations"
    xx[,2]=as.numeric(xx[,2])
    
    if (input$MostCitAuthorsK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostCitAuthorsK}
    
    xx=xx[1:k,]
    xx[,2]=round(xx[,2],1)
    g=ggplot2::ggplot(data=xx, aes(x=xx[,1], y=xx[,2], fill=-xx[,2], text=paste("Author: ",xx[,1],"\n",lab,": ",xx[,2]))) +
      geom_bar(aes(group="NA"),stat="identity")+
      scale_fill_continuous(type = "gradient")+
      scale_x_discrete(limits = rev(xx[,1]))+
      labs(title="Most Local Cited Authors", x = "Authors")+
      labs(y = lab)+
      theme_minimal() +
      guides(fill=FALSE)+
      coord_flip()
    values$MLCAplot <- g
    return(g)
  })
  
  output$MLCAplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostLocalCitedAuthors-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MLCAplot, dpi = as.numeric(input$MLCAdpi))
    },
    contentType = "png"
  )
  
  output$MostCitAuthorsPlot <- renderPlotly({
    
    g <- MLCAuthors()
    plot.ly(g)
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
      ggsave(filename = file, plot = values$AIplot, dpi = as.numeric(input$AIdpi))
    },
    contentType = "png"
  )
  
  output$AuthorHindexPlot <- renderPlotly({
    
    res <- HAuthors()
    plot.ly(res$g)
    
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
  
  AUoverTime <- eventReactive(input$applyAUoverTime,{
    values$AUProdOverTime <- authorProdOverTime(values$M, k=input$TopAuthorsProdK, graph=FALSE)
  })
  
  output$APOTplot.save <- downloadHandler(
    filename = function() {
      
      paste("AuthorsProductionOverTime-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$AUProdOverTime$graph, dpi = as.numeric(input$APOTdpi))
    },
    contentType = "png"
  )
  
  output$TopAuthorsProdPlot <- renderPlotly({
    AUoverTime()
    
    plot.ly(values$AUProdOverTime$graph)
    
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
  
  output$lotkaPlot <- renderPlotly({
    
    values$lotka=lotka(biblioAnalysis(values$M))
    AuProd=values$lotka$AuthorProd
    AuProd$Theoretical=10^(log10(values$lotka$C)-2*log10(AuProd[,1]))
    AuProd$Theoretical=AuProd$Theoretical/sum(AuProd$Theoretical)
    
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
      )
    values$LLplot <- g
    plot.ly(g)
    
  })#,height = 600)
  
  output$LLplot.save <- downloadHandler(
    filename = function() {
      
      paste("LotkaLaw-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$LLplot, dpi = as.numeric(input$LLdpi))
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
  
  ### Affiliations ----
  
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
    g=ggplot2::ggplot(data=xx, aes(x=.data$AFF, y=.data$Freq, 
                                   fill=-.data$Freq, text=paste("Affiliation: ",
                                                                .data$AFF,"\nN.of Documents: ",.data$Freq))) +
      geom_bar(aes(group="NA"),stat="identity")+
      scale_fill_continuous(type = "gradient")+
      scale_x_discrete(limits = rev(xx$AFF))+
      labs(title="Most Relevant Affiliations", x = "Affiliations")+
      labs(y = "N. of Documents")+
      theme_minimal() +
      guides(fill=FALSE)+
      coord_flip()
    values$AFFplot <- g
    return(g)
  })
  
  output$AFFplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostRelevantAffiliations-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$AFFplot, dpi = as.numeric(input$AFFdpi))
    },
    contentType = "png"
  )
  
  output$MostRelAffiliationsPlot <- renderPlotly({
    
    g <- MRAffiliations()
    
    plot.ly(g)
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
  
  ### Countries ----
  
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
                         coord_flip())
    values$MRCOplot <- g
    return(g)
  }) 
  
  output$MRCOplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostRelevantCountries-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MRCOplot, dpi = as.numeric(input$MRCOdpi))
    },
    contentType = "png"
  )
  
  output$MostRelCountriesPlot <- renderPlotly({
    
    g <- CAUCountries()
    
    plot.ly(g)
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
  
  output$countryProdPlot <- renderPlotly({
    values$mapworld<-mapworld(values$M)
    plot.ly(values$mapworld$g)
  })#, height = 500, width =900)
  
  output$CSPplot.save <- downloadHandler(
    filename = function() {
      
      paste("CountryScientificProduction-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$mapworld$g, dpi = as.numeric(input$CSPdpi))
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
    
    g=ggplot2::ggplot(data=xx, aes(x=xx[,1], y=xx[,2], fill=-xx[,2],text=paste("Country: ",xx[,1],"\n",laby,": ",xx[,2]))) +
      geom_bar(aes(group="NA"),stat="identity")+
      scale_fill_continuous(type = "gradient")+
      scale_x_discrete(limits = rev(xx[,1]))+
      labs(title="Most Cited Countries", x = "Countries")+
      labs(y = laby)+
      theme_minimal() +
      guides(fill=FALSE)+
      coord_flip()
    values$MCCplot <- g
    return(g)
  })
  
  output$MCCplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostCitedCountries-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MCCplot, dpi = as.numeric(input$MCCdpi))
    },
    contentType = "png"
  )
  
  output$MostCitCountriesPlot <- renderPlotly({
    g <- MCCountries()
    plot.ly(g)
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
  
  ### DOCUMENTS MENU ####
  
  ### Documents ----
  
  MGCDocuments <- eventReactive(input$applyMGCDocuments,{
    res <- descriptive(values,type="tab4")
    values <-res$values
    values$TABGlobDoc <- values$TAB
    
    if (input$CitDocsMeasure=="TC"){
      xx=data.frame(values$results$MostCitedPapers[1],values$results$MostCitedPapers[3], stringsAsFactors = FALSE,row.names=NULL)
      lab="Total Citations"} else {
        xx=data.frame(values$results$MostCitedPapers[1],values$results$MostCitedPapers[4], stringsAsFactors = FALSE,row.names=NULL)
        lab="Total Citations per Year"
      }
    
    if (input$MostCitDocsK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostCitDocsK}
    
    xx=xx[1:k,]
    
    g=ggplot2::ggplot(data=xx, aes(x=xx[,1], y=xx[,2], fill=-xx[,2], text=paste("Document: ", xx[,1],"\nGlobal Citations: ",xx[,2]))) +
      geom_bar(stat="identity")+
      scale_fill_continuous(type = "gradient")+
      scale_x_discrete(limits = rev(xx[,1]))+
      labs(title="Most Cited Documents", x = "Documents")+
      labs(y = lab)+
      theme_minimal() +
      guides(fill=FALSE)+
      coord_flip()
    values$MGCDplot <- g
    return(g)
  })
  
  output$MGCDplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostGlobalCitedDocuments-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MGCDplot, dpi = as.numeric(input$MGCDdpi))
    },
    contentType = "png"
  )
  
  output$MostCitDocsPlot <- renderPlotly({
    
    g <- MGCDocuments()
    plot.ly(g)
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
    
    g=ggplot2::ggplot(data=xx, aes(x=xx[,1], y=xx[,4], fill=-xx[,4], text=paste("Document: ",xx[,1],"\nLocal Citations: ",xx[,4]))) +
      geom_bar(aes(group="NA"),stat="identity")+
      scale_fill_continuous(type = "gradient")+
      scale_x_discrete(limits = rev(xx[,1]))+
      labs(title="Most Local Cited Documents", x = "Documents")+
      labs(y = "Local Citations")+
      theme_minimal() +
      guides(fill=FALSE)+
      coord_flip()
    values$MLCDplot <- g
    return(g)
  })
  
  output$MLCDplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostLocalCitedDocuments-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MLCDplot, dpi = as.numeric(input$MLCDdpi))
    },
    contentType = "png"
  )
  
  output$MostLocCitDocsPlot <- renderPlotly({
    
    g <- MLCDocuments()
    plot.ly(g)
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
  
  ### Cited References ----
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
    
    g=ggplot2::ggplot(data=xx, aes(x=xx[,1], y=xx[,2], fill=-xx[,2], text=paste("Reference: ",xx[,1],"\nLocal Citations: ",xx[,2]))) +
      geom_bar(aes(group="NA"),stat="identity")+
      scale_fill_continuous(type = "gradient")+
      scale_x_discrete(limits = rev(xx[,1]), labels=substr(rev(xx[,1]),1,50))+
      labs(title="Most Cited References", x = "References")+
      labs(y = "Local Citations")+
      theme_minimal() +
      guides(fill=FALSE)+
      coord_flip()
    values$MLCRplot <- g
    return(g)
  })
  
  output$MLCRplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostLocalCitedReferences-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MLCRplot, dpi = as.numeric(input$MLCRdpi))
    },
    contentType = "png"
  )
  
  output$MostCitRefsPlot <- renderPlotly({
    
    g <- MLCReferences()
    plot.ly(g)
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
  
  RPYS <- eventReactive(input$applyRPYS,{
    values$res <- rpys(values$M, sep=input$rpysSep, graph=FALSE)
  })
  
  output$RSplot.save <- downloadHandler(
    filename = function() {
      
      paste("ReferenceSpectroscopy-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$res$spectroscopy, dpi = as.numeric(input$RSdpi))
    },
    contentType = "png"
  )
  
  output$rpysPlot <- renderPlotly({
    
    RPYS()
    plot.ly(values$res$spectroscopy)
    
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
  
  ### Words ----
  MFWords <- eventReactive(input$applyMFWords,{
    WR=wordlist(values$M,Field=input$MostRelWords,n=Inf,measure="identity")$v
    
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
    
    g <- ggplot2::ggplot(data=xx, aes(x=xx[,1], y=xx[,2], fill=-xx[,2], text=paste(lab,": ",xx[,1],"\nOccurrences: ",xx[,2]))) +
      geom_bar(aes(group="NA"),stat="identity")+
      scale_fill_continuous(type = "gradient")+
      scale_x_discrete(limits = rev(xx[,1]))+
      labs(title="Most Relevant Words", x = lab)+
      labs(y = "Occurrences")+
      theme_minimal() +
      guides(fill=FALSE)+
      coord_flip()
    values$MRWplot <- g
    return(g)
    
  })
  
  output$MRWplot.save <- downloadHandler(
    filename = function() {
      
      paste("MostRelevantWords-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MRWplot, dpi = as.numeric(input$MRWdpi))
    },
    contentType = "png"
  )
  
  output$MostRelWordsPlot <- renderPlotly({
    g <- MFWords()
    plot.ly(g)
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
  
  WordCloud <- eventReactive(input$applyWordCloud,{
    resW=wordlist(M=values$M, Field=input$summaryTerms, n=input$n_words, measure=input$measure)
    
    W=resW$W
    values$Words=resW$Words
    
    wordcloud2::wordcloud2(W, size = input$scale, minSize = 0, gridSize =  input$padding,
                           fontFamily = input$font, fontWeight = 'normal',
                           color = input$wcCol, backgroundColor = input$wcBGCol,
                           minRotation = 0, maxRotation = input$rotate/10, shuffle = TRUE,
                           rotateRatio = 0.7, shape = input$wcShape, ellipticity = input$ellipticity,
                           widgetsize = NULL, figPath = NULL, hoverFunction = NULL)
  })
  output$wordcloud <- wordcloud2::renderWordcloud2({
    
    WordCloud()
  })
  
  TreeMap <- eventReactive(input$applyTreeMap,{
    #resW=wordlist(M=values$M, Field=input$treeTerms, n=input$treen_words, measure=input$treemeasure)
    resW=wordlist(M=values$M, Field=input$treeTerms, n=input$treen_words, measure="identity")
    
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
  
  WDynamics <- eventReactive(input$applyWD,{
    if (input$cumTerms=="Cum"){
      cdf=TRUE
      laby="Cumulate occurrences (loess smoothing)"
    }else{
      cdf=FALSE
      laby="Annual occurrences (loess smoothing)"}
    if (input$se=="Yes"){se=TRUE}else{se=FALSE}
    
    switch(input$growthTerms,
           ID={
             KW=KeywordGrowth(values$M, Tag = "ID", sep = ";", top = input$topkw[2], cdf = cdf)
             
           },
           DE={
             KW=KeywordGrowth(values$M, Tag = "DE", sep = ";", top = input$topkw[2], cdf = cdf)
           },
           TI={
             if (!("TI_TM" %in% names(values$M))){
               values$M=termExtraction(values$M,Field = "TI", verbose=FALSE)
             }
             KW=KeywordGrowth(values$M, Tag = "TI_TM", sep = ";", top = input$topkw[2], cdf = cdf)
           },
           AB={
             if (!("AB_TM" %in% names(values$M))){
               values$M=termExtraction(values$M,Field = "AB", verbose=FALSE)
             }
             KW=KeywordGrowth(values$M, Tag = "AB_TM", sep = ";", top = input$topkw[2], cdf = cdf)
           }
    )
    
    values$KW=KW[,c(1,seq(input$topkw[1],input$topkw[2])+1)]
    
    term=names(values$KW)[-1]
    term=rep(term,each=dim(values$KW)[1])
    n=dim(values$KW)[1]*(dim(values$KW)[2]-1)
    freq=matrix(as.matrix(values$KW[,-1]),n,1)
    values$DF=data.frame(Year=rep(values$KW$Year,(dim(values$KW)[2]-1)),Term=term, Freq=freq, stringsAsFactors = TRUE)
    
    g <- ggplot(values$DF)+
      geom_smooth(aes(x=.data$Year,y=.data$Freq, group=.data$Term, color=.data$Term),se = se,method = "loess",formula ='y ~ x')+
      labs(x = 'Year'
           , y = laby
           , title = "Word Growth") +
      #ylim(0, NA) +
      scale_x_continuous(breaks= (values$KW$Year[seq(1,length(values$KW$Year),by=ceiling(length(values$KW$Year)/20))])) +
      geom_hline(aes(yintercept=0, alpha=0.1))+
      theme(text = element_text(color = "#444444"), legend.position="none"
            ,plot.caption = element_text(size = 9, hjust = 0.5, color = "black", face = "bold")
            ,panel.background = element_rect(fill = '#EFEFEF')
            ,panel.grid.minor = element_line(color = '#FFFFFF')
            ,panel.grid.major = element_line(color = '#FFFFFF')
            ,plot.title = element_text(size = 24)
            ,axis.title = element_text(size = 14, color = '#555555')
            ,axis.title.y = element_text(vjust = 1, angle = 90)
            ,axis.title.x = element_text(hjust = 0.95, angle = 0)
            ,axis.text.x = element_text(size=10)
      )
    
    DFsmooth=(ggplot_build(g)$data[[1]])
    DFsmooth$group=factor(DFsmooth$group, labels=levels(values$DF$Term))
    
    maximum=sort(unique(DFsmooth$x),decreasing=TRUE)[2]
    DF2=subset(DFsmooth, x == maximum)
    g=g+
      ggrepel::geom_text_repel(data = DF2, aes(label = .data$group, colour = .data$group, x =.data$x, y = .data$y), hjust = -.1)
    values$WDplot <- g
    return(g)
  })
  
  output$WDplot.save <- downloadHandler(
    filename = function() {
      
      paste("WordDynamics-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$WDplot, dpi = as.numeric(input$WDdpi))
    },
    contentType = "png"
  )
  
  output$kwGrowthPlot <- renderPlot({
    
    g <- WDynamics()
    plot(g)
    
  }, width = "auto", height = reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*2/5,0)), res = 150) #height = 600, width = 900)
  
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
  
  #### Trend Topics ####
  output$trendSliderPY <- renderUI({
    
    sliderInput("trendSliderPY", "Timespan", min = min(values$M$PY,na.rm=T),sep="",
                max = max(values$M$PY,na.rm=T), value = c(min(values$M$PY,na.rm=T),max(values$M$PY,na.rm=T)))
  })
  
  TrendTopics <- eventReactive(input$applyTrendTopics,{
    
    if (input$trendTerms %in% c("TI","AB")){
      values$M=termExtraction(values$M, Field = input$trendTerms, stemming = input$trendStemming, verbose = FALSE)
      field=paste(input$trendTerms,"_TM",sep="")
    } else {field=input$trendTerms}
    values$trendTopics <- fieldByYear(values$M, field = field, timespan = input$trendSliderPY, min.freq = input$trendMinFreq,
                                      n.items = input$trendNItems, labelsize = input$trendSize, graph = FALSE)
    
    
  })
  
  output$TTplot.save <- downloadHandler(
    filename = function() {
      
      paste("TrendTopics-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$trendTopics$graph, dpi = as.numeric(input$TTdpi))
    },
    contentType = "png"
  )
  
  output$trendTopicsPlot <- renderPlot({
    
    TrendTopics()
    plot(values$trendTopics$graph)
    
  }, width = "auto", height = reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*2/5,0)), res = 150)  #height = 700)
  
  
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
  
  #### Coupling ####
  
  CMMAP <- eventReactive(input$applyCM,{
    
    values$CM <- couplingMap(values$M, analysis=input$CManalysis, field=input$CMfield, 
                             n=input$CMn, minfreq=input$CMfreq, 
                             stemming=input$CMstemming, size=input$sizeCM, 
                             n.labels=input$CMn.labels, repel=FALSE)
    
    validate(
      need(values$CM$nclust > 0, "\n\nNo clusters in one or more periods. Please select a different set of parameters.")
    )
  })
  output$CMPlot <- renderPlotly({
    
    CMMAP()
    plot.ly(values$CM$map)
    
  })#, height = 650, width = 800)
  
  output$CMNetPlot <- renderVisNetwork({
    CMMAP()
    values$networkCM<-igraph2vis(g=values$CM$net$graph,curved=(input$coc.curved=="Yes"), 
                                 labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                 shape=input$coc.shape)
    
    values$networkCM$VIS
    
  })
  
  output$CMplot.save <- downloadHandler(
    filename = function() {
      
      paste("CouplingMap-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$CM$map, dpi = as.numeric(input$CMdpi))
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
                               shape=input$coc.shape)
    
    
  })
  output$cocPlot <- renderVisNetwork({  
    
    COCnetwork()
    
    values$network$VIS
    
  })
  
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
  
  output$FA1plot.save <- downloadHandler(
    filename = function() {
      
      paste("FactorialMap-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$CS$graph_terms, dpi = as.numeric(input$FAdpi))
    },
    contentType = "png"
  )
  output$FA2plot.save <- downloadHandler(
    filename = function() {
      
      paste("Dendrogram-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$CS$graph_dendogram, dpi = as.numeric(input$FAdpi))
    },
    contentType = "png"
  )
  
  output$FA3plot.save <- downloadHandler(
    filename = function() {
      
      paste("MostContribDocuments-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$CS$graph_documents_Contrib, dpi = as.numeric(input$FAdpi))
    },
    contentType = "png"
  )
  
  output$FA4plot.save <- downloadHandler(
    filename = function() {
      
      paste("MostCitedDocuments-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$CS$graph_documents_TC, dpi = as.numeric(input$FAdpi))
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
    
    values$TM <- thematicMap(values$M, field=input$TMfield, 
                             n=input$TMn, minfreq=input$TMfreq, 
                             stemming=input$TMstemming, size=input$sizeTM, 
                             n.labels=input$TMn.labels, repel=FALSE)
    
    validate(
      need(values$TM$nclust > 0, "\n\nNo topics in one or more periods. Please select a different set of parameters.")
    )
  })
  output$TMPlot <- renderPlotly({
    
    TMAP()
    plot.ly(values$TM$map)
    
  })#, height = 650, width = 800)
  
  output$NetPlot <- renderVisNetwork({
    TMAP()
    values$networkTM<-igraph2vis(g=values$TM$net$graph,curved=(input$coc.curved=="Yes"), 
                                 labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                 shape=input$coc.shape)
    
    values$networkTM$VIS
    
  })
  
  output$TMplot.save <- downloadHandler(
    filename = function() {
      
      paste("ThematicMap-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
               ggsave(filename = file, plot = values$TM$map, dpi = as.numeric(input$TMdpi))
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
    
    
    values$yearSlices <- as.numeric()
    for (i in 1:as.integer(input$numSlices)){
      if (length(input[[paste0("Slice", i)]])>0){values$yearSlices=c(values$yearSlices,input[[paste0("Slice", i)]])}
    }
    
    if (length(values$yearSlices)>0){
      values$nexus <- thematicEvolution(values$M, field=input$TEfield, values$yearSlices, n = input$nTE, minFreq = input$fTE, size = input$sizeTE, n.labels=input$TEn.labels, repel=FALSE)
      
      validate(
        need(values$nexus$check != FALSE, "\n\nNo topics in one or more periods. Please select a different set of parameters.")
      )
      
      plotThematicEvolution(Nodes = values$nexus$Nodes,Edges = values$nexus$Edges, measure = input$TEmeasure, min.flow = input$minFlowTE)
    }
  })
  
  output$TEPlot <- networkD3::renderSankeyNetwork({
    
    TEMAP()
    
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
      plot.ly(values$nexus$TM[[1]]$map)
    } else {emptyPlot("You have selected fewer periods!")}
    
  })#, height = 650, width = 800)
  
  output$TMPlot2 <-  renderPlotly({
    TEMAP()
    #input$applyTM
    if (length(values$nexus$TM)>=2){
      plot.ly(values$nexus$TM[[2]]$map)
    } else {emptyPlot("You have selected fewer periods!")}
    
  })#, height = 650, width = 800)
  
  output$TMPlot3 <-  renderPlotly({
    TEMAP()
    #input$applyTM
    if (length(values$nexus$TM)>=3){
      plot.ly(values$nexus$TM[[3]]$map)
    } else {emptyPlot("You have selected fewer periods!")}
    
  })#, height = 650, width = 800)
  
  output$TMPlot4 <-  renderPlotly({
    TEMAP()
    #input$applyTM
    if (length(values$nexus$TM)>=4){
      plot.ly(values$nexus$TM[[4]]$map)
    } else (emptyPlot("You have selected fewer periods!"))
    
  })#, height = 650, width = 800)
  
  output$TMPlot5 <-  renderPlotly({
    TEMAP()
    #input$applyTM
    if (length(values$nexus$TM)>=5){
      plot.ly(values$nexus$TM[[5]]$map)
    } else (emptyPlot("You have selected fewer periods!"))
    
  })#, height = 650, width = 800)
  
  output$NetPlot1 <- renderVisNetwork({
    TEMAP()
    k=1
    values$network1<-igraph2vis(g=values$nexus$Net[[k]]$graph,curved=(input$coc.curved=="Yes"), 
                                labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                shape=input$coc.shape)
    
    values$network1$VIS
    
  })
  
  output$NetPlot2 <- renderVisNetwork({
    TEMAP()
    k=2
    values$network2<-igraph2vis(g=values$nexus$Net[[k]]$graph,curved=(input$coc.curved=="Yes"), 
                                labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                shape=input$coc.shape)
    
    values$network2$VIS
    
  })
  
  output$NetPlot3 <- renderVisNetwork({
    TEMAP()
    k=3
    values$network3<-igraph2vis(g=values$nexus$Net[[k]]$graph,curved=(input$coc.curved=="Yes"), 
                                labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                shape=input$coc.shape)
    
    values$network3$VIS
    
  })
  
  output$NetPlot4 <- renderVisNetwork({
    TEMAP()
    k=4
    values$network4<-igraph2vis(g=values$nexus$Net[[k]]$graph,curved=(input$coc.curved=="Yes"), 
                                labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                shape=input$coc.shape)
    
    values$network4$VIS
    
  })
  
  output$NetPlot5 <- renderVisNetwork({
    TEMAP()
    k=5
    values$network5<-igraph2vis(g=values$nexus$Net[[k]]$graph,curved=(input$coc.curved=="Yes"), 
                                labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                shape=input$coc.shape)
    
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
                               labelsize=input$citlabelsize, opacity=input$cocitAlpha,type=input$citlayout,
                               shape=input$cocit.shape)
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
  })
  
  output$HGplot.save <- downloadHandler(
    filename = function() {
      
      paste("Historiograph-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$histPlot$g, dpi = as.numeric(input$HGdpi))
    },
    contentType = "png"
  )
  
  output$histPlot <- renderPlot({
    
    Hist()
   #g <- values$histPlot$g + 
   #    coord_equal()
   #  plot(g)
  plot(values$histPlot$g)
  }, width = "auto", height = reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*2/5,0)), res = 150) #height = 610, width = 1100, res=150)
  
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
                               shape=input$col.shape)
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
      ggsave(filename = file, plot = values$WMmap$g, dpi = as.numeric(input$CCdpi))
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
  
  
  plot.ly <- function(g){
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
             ))
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
    values$results=list("NA")
    values$log="working..."
    values$load="FALSE"
    values$field="NA"
    values$citField=values$colField=values$citSep="NA"
    values$NetWords=values$NetRefs=values$ColNetRefs=matrix(NA,1,1)
    values$Title="Network"
    values$Histfield="NA"
    values$histlog="working..."
    values$kk=0
    values$histsearch="NA"
    values$citShortlabel="NA"
    values$S=list("NA")
    values$GR="NA"
    
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
      title="Author Impact"
      xn="Authors"
    } else {
      K=input$Hksource
      measure=input$HmeasureSources
      title="Source Impact"
      xn="Sources"
    }
    if (K>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=K}
    
    switch(measure,
           h={m=2},
           g={m=3},
           m={m=4},
           tc={m=5}
    )
    xx=xx[order(-xx[,m]),]
    xx=xx[1:k,c(1,m)]
    
    g=ggplot2::ggplot(data=xx, aes(x=xx[,1], y=xx[,2], fill=-xx[,2], text=paste(xn,": ",xx[,1],"\n", names(values$H)[m],": ",xx[,2]))) +
      #geom_bar(stat="identity", fill="steelblue")+
      geom_bar(aes(group="NA"),stat="identity")+
      scale_fill_continuous(type = "gradient")+
      scale_x_discrete(limits = rev((xx[,1])))+
      labs(title=title, x = xn)+
      labs(y = names(values$H)[m])+
      theme_minimal() +
      guides(fill=FALSE)+
      coord_flip()
    
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
             CR<-citations(values$M,field="author")
             TAB=data.frame(Authors=names(CR$Cited), Citations=as.numeric(CR$Cited),stringsAsFactors = FALSE)
           }
    )
    values$TAB=TAB
    res=list(values=values,TAB=TAB)
    return(res)
  }
  
  wordlist <- function(M, Field, n, measure){
    switch(Field,
           ID={v=tableTag(values$M,"ID")},
           DE={v=tableTag(values$M,"DE")},
           TI={
             if (!("TI_TM" %in% names(M))){
               v=tableTag(M,"TI")
             }},
           AB={if (!("AB_TM" %in% names(M))){
             v=tableTag(M,"AB")
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
    
    g=ggplot(country.prod, aes( x = .data$long, y = .data$lat, group=.data$group, text=paste("Country: ",.data$region,"\nN.of Documents: ",.data$Freq))) +
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
      ) 
    results=list(g=g,tab=tab)
    return(results)
  }
  
  ### Structure fuctions ----
  CAmap <- function(input, values){
    if ((input$CSfield %in% names(values$M))){
      
      tab=tableTag(values$M,input$CSfield)
      if (length(tab>=2)){
        
        minDegree=as.numeric(tab[input$CSn])
        
        values$CS <- conceptualStructure(values$M, method=input$method , field=input$CSfield, minDegree=minDegree, clust=input$nClustersCS, k.max = 8, stemming=F, labelsize=input$CSlabelsize,documents=input$CSdoc,graph=FALSE)
        #plot(values$CS$graph_terms)
        
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
    if ((input$field %in% names(values$M))){
      
      if ((dim(values$NetWords)[1])==1 | !(input$field==values$field)){
        
        values$field=input$field
        
        switch(input$field,
               ID={
                 values$NetWords <- biblioNetwork(values$M, analysis = "co-occurrences", network = "keywords", sep = ";")
                 values$Title= "Keywords Plus Network"
               },
               DE={
                 values$NetWords <- biblioNetwork(values$M, analysis = "co-occurrences", network = "author_keywords", sep = ";")
                 values$Title= "Authors' Keywords network"
               },
               TI={
                 if(!("TI_TM" %in% names(values$M))){values$M=termExtraction(values$M,Field="TI",verbose=FALSE)}
                 values$NetWords <- biblioNetwork(values$M, analysis = "co-occurrences", network = "titles", sep = ";")
                 values$Title= "Title Words network"
               },
               AB={
                 if(!("AB_TM" %in% names(values$M))){values$M=termExtraction(values$M,Field="AB",verbose=FALSE)}
                 values$NetWords <- biblioNetwork(values$M, analysis = "co-occurrences", network = "abstracts", sep = ";")
                 values$Title= "Abstract Words network"
               })
        
      }
      
      if (n>dim(values$NetWords)[1]){n=dim(values$NetWords)[1]}
      if (label.n>n){label.n=n}
      if (input$normalize=="none"){normalize=NULL}else{normalize=input$normalize}
      if (input$label.cex=="Yes"){label.cex=TRUE}else{label.cex=FALSE}
      if (input$coc.curved=="Yes"){curved=TRUE}else{curved=FALSE}
      
      #par(bg="grey92", mar=c(0,0,0,0))
      values$cocnet=networkPlot(values$NetWords, normalize=normalize,n = n, Title = values$Title, type = input$layout, 
                                size.cex=TRUE, size=5 , remove.multiple=F, edgesize = input$edgesize*3, labelsize=input$labelsize,label.cex=label.cex,
                                label.n=label.n,edges.min=input$edges.min,label.color = F, curved=curved,alpha=input$cocAlpha,
                                cluster=input$cocCluster, remove.isolates = (input$coc.isolates=="yes"), verbose = FALSE)
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
    
    if ((dim(values$NetRefs)[1])==1 | !(input$citField==values$citField) | !(input$citSep==values$citSep) | !(input$citShortlabel==values$citShortlabel)){
      
      values$citField=input$citField
      values$citSep=input$citSep
      if (input$citShortlabel=="Yes"){shortlabel=TRUE}else{shortlabel=FALSE}
      values$citShortlabel=input$citShortlabel
      switch(input$citField,
             CR={
               values$NetRefs <- biblioNetwork(values$M, analysis = "co-citation", network = "references", sep = input$citSep, shortlabel=shortlabel)
               values$Title= "Cited References network"
               
             },
             CR_AU={
               if(!("CR_AU" %in% names(values$M))){values$M=metaTagExtraction(values$M,Field="CR_AU", sep = input$citSep)}
               values$NetRefs <- biblioNetwork(values$M, analysis = "co-citation", network = "authors", sep = input$citSep)
               values$Title= "Cited Authors network"
             },
             CR_SO={
               if(!("CR_SO" %in% names(values$M))){values$M=metaTagExtraction(values$M,Field="CR_SO", sep = input$citSep)}
               values$NetRefs <- biblioNetwork(values$M, analysis = "co-citation", network = "sources", sep = input$citSep)
               values$Title= "Cited Sources network"
             })
      
    }
    
    if (n>dim(values$NetRefs)[1]){n=dim(values$NetRefs)[1]}
    if (label.n>n){label.n=n}
    if (input$citlabel.cex=="Yes"){label.cex=TRUE}else{label.cex=FALSE}
    if (input$cocit.curved=="Yes"){curved=TRUE}else{curved=FALSE}
    
    values$cocitnet=networkPlot(values$NetRefs, normalize=NULL, n = n, Title = values$Title, type = input$citlayout, 
                                size.cex=TRUE, size=5 , remove.multiple=F, edgesize = input$citedgesize*3, 
                                labelsize=input$citlabelsize,label.cex=label.cex, curved=curved,
                                label.n=label.n,edges.min=input$citedges.min,label.color = F,remove.isolates = (input$cit.isolates=="yes"),
                                alpha=input$cocitAlpha, cluster=input$cocitCluster, verbose = FALSE)
    return(values)
  }
  
  socialStructure<-function(input,values){
    n = input$colNodes
    label.n = input$colLabels
    
    if ((dim(values$ColNetRefs)[1])==1 | !(input$colField==values$colField)){
      
      values$colField=input$colField
      
      
      values$cluster="walktrap"
      switch(input$colField,
             COL_AU={
               values$ColNetRefs <- biblioNetwork(values$M, analysis = "collaboration", network = "authors", sep = ";")
               values$Title= "Author Collaboration network"
               
             },
             COL_UN={
               if(!("AU_UN" %in% names(values$M))){values$M=metaTagExtraction(values$M,Field="AU_UN", sep=";")}
               values$ColNetRefs <- biblioNetwork(values$M, analysis = "collaboration", network = "universities", sep = ";")
               values$Title= "Edu Collaboration network"
             },
             COL_CO={
               if(!("AU_CO" %in% names(values$M))){values$M=metaTagExtraction(values$M,Field="AU_CO", sep=";")}
               values$ColNetRefs <- biblioNetwork(values$M, analysis = "collaboration", network = "countries", sep = ";")
               values$Title= "Country Collaboration network"
               #values$cluster="none"
             })
      
    }
    
    if (n>dim(values$ColNetRefs)[1]){n=dim(values$ColNetRefs)[1]}
    if (label.n>n){label.n=n}
    if (input$colnormalize=="none"){normalize=NULL}else{normalize=input$colnormalize}
    if (input$collabel.cex=="Yes"){label.cex=TRUE}else{label.cex=FALSE}
    if (input$soc.curved=="Yes"){curved=TRUE}else{curved=FALSE}
    
    type=input$collayout
    if (input$collayout=="worldmap"){type="auto"}
    
    values$colnet=networkPlot(values$ColNetRefs, normalize=normalize, n = n, Title = values$Title, type = type, 
                              size.cex=TRUE, size=5 , remove.multiple=F, edgesize = input$coledgesize*3, 
                              labelsize=input$collabelsize,label.cex=label.cex, curved=curved,
                              label.n=label.n,edges.min=input$coledges.min,label.color = F,alpha=input$colAlpha,
                              remove.isolates = (input$col.isolates=="yes"), cluster=input$colCluster, verbose = FALSE)
    
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
      )
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
  
  igraph2vis<-function(g,curved,labelsize,opacity,type,shape){
    
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
      size=(vn$nodes$font.size-Min)/(Max-Min)*10*labelsize+10
    } else {size=10*labelsize}
    size[size<scalemin]=scalemin
    size[size>scalemax]=scalemax
    vn$nodes$font.size=size
    l<-netLayout(type)
    
    ### TO ADD SHAPE AND FONT COLOR OPTIONS
    
    VIS<-visNetwork(nodes = vn$nodes, edges = vn$edges, type="full", smooth=TRUE, physics=FALSE) %>%
      visNodes(shape=shape, font=list(color="black")) %>%
      visIgraphLayout(layout = l) %>%
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