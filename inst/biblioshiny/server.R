source("utils.R", local=TRUE)

#### SERVER ####
server <- function(input, output,session){
  session$onSessionEnded(stopApp)

  ## suppress warnings
  options(warn = -1)
  
  ## file upload max size
  maxUploadSize <- 200 # default value
  maxUploadSize <- getShinyOption("maxUploadSize", maxUploadSize)
  options(shiny.maxRequestSize=maxUploadSize*1024^2)
  
  ## initial values
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
  values$checkControlBar <-FALSE
  
## NOTIFICATION ITEM ----
  
  output$notificationMenu <- renderMenu({
    notifTot <- notifications()
    values$nots <- apply(notifTot, 1, function(row) {
      
      ## extract href from messages
      if (is.na(row[["href"]])){href <- NULL
      }else{
        href <- paste("javascript:void(window.open('",row[["href"]],"', '_blank'))", sep="")
      }
      
      ## add bold to new messages and split the long ones in two rows
      if (row[["status"]]=="danger"){  ### new messages
        textRows <- paste("tags$strong('",row[["nots"]],"')", sep="")
        textRows <- strsplit(substr(textRows,1,85), "(?<=.{48})", perl = TRUE)[[1]]
        if (length(textRows)>1){
          textRows <- paste("tags$div(",textRows[1],"',tags$br(),'",textRows[2],")", sep="")
        }else{
          textRows <- paste("tags$div(",textRows,")", sep="")
        }
      }else{ ## old messages
        textRows <- strsplit(substr(row[["nots"]],1,70), "(?<=.{35})", perl = TRUE)[[1]]
        if (length(textRows)>1){
          textRows <- paste("tags$div('",textRows[1],"',tags$br(),'",textRows[2],"')", sep="")
        }else{
          textRows <- paste("tags$div('",textRows,"')", sep="")
        }
      }
      
      notificationItem(
        text = eval(parse(text=textRows)),
        icon = if (row[["status"]]=="danger") {fa_i(name ="envelope")}else{fa_i(name ="envelope-open")},
        status = row[["status"]],
        href = href
      )
    })
    
    if ("danger" %in% notifTot[["status"]]){
      badge = "danger"
      icon_name ="envelope"
    } else {
      badge = NULL
      icon_name ="envelope-open"
    }
    
    dropdownMenu(type = "notifications", 
                 .list = values$nots, 
                 headerText ="",
                 badgeStatus = NULL, 
                 icon = fa_i(name = icon_name)
    )
  })
  
  ## SIDEBAR MENU ----
  ### Apply Data----
  observe({
    toggleElement(
      id ="rest_of_sidebar",
      condition = input[["applyLoad"]]
    )
  })
  
  output$rest_of_sidebar <- renderMenu({
    sidebarMenu(
      menuItem("Filters",tabName = "filters",icon = fa_i(name ="filter")),
      menuItem("Overview",tabName = "overview",icon=fa_i(name = "table"),startExpanded = FALSE,
               menuSubItem("Main Information",tabName="mainInfo",icon = icon("chevron-right",lib = "glyphicon")),
               menuSubItem("Annual Scientific Production",tabName = "annualScPr",icon = icon("chevron-right",lib = "glyphicon")),
               menuSubItem("Average Citations per Year",tabName = "averageCitPerYear",icon = icon("chevron-right",lib = "glyphicon")),
               menuSubItem("Three-Field Plot", tabName ="threeFieldPlot",icon = icon("chevron-right",lib = "glyphicon"))),
      menuItem("Sources", tabName = "sources",icon = fa_i(name ="book"), startExpanded = FALSE,
               menuSubItem("Most Relevant Sources", tabName = "relevantSources",icon = icon("chevron-right",lib = "glyphicon")),
               menuSubItem("Most Local Cited Sources",tabName = "localCitedSources",icon = icon("chevron-right",lib = "glyphicon")),
               menuSubItem("Bradford's Law",tabName = "bradford",icon = icon("chevron-right",lib = "glyphicon")),
               menuSubItem("Source Impact",tabName = "sourceImpact",icon = icon("chevron-right",lib = "glyphicon")),
               menuSubItem("Source Dynamics",tabName = "sourceDynamics",icon = icon("chevron-right",lib = "glyphicon"))),
      menuItem("Authors", tabName = "authors",icon = fa_i(name="user"),startExpanded = FALSE,
               "Authors",
               menuSubItem("Most Relevant Authors", tabName = "mostRelAuthors",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Most Local Cited Authors",tabName = "mostLocalCitedAuthors",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Authors' Production over Time",tabName = "authorsProdOverTime",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Lotka's Law",tabName = "lotka",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Author Impact",tabName = "authorImpact",icon = icon("chevron-right", lib = "glyphicon")),
               "Affiliations",
               menuSubItem("Most Relevant Affiliations",tabName = "mostRelAffiliations",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Affiliations' Production over Time",tabName = "AffOverTime",icon = icon("chevron-right", lib = "glyphicon")),
               "Countries",
               menuSubItem("Corresponding Author's Country",tabName = "correspAuthorCountry",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Country Scientific Production",tabName = "countryScientProd",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Countries' Production over Time",tabName = "COOverTime",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Most Cited Countries",tabName = "mostCitedCountries",icon = icon("chevron-right", lib = "glyphicon"))
      ),
      menuItem("Documents", tabName = "documents",icon = fa_i(name="layer-group"), startExpanded = FALSE,
               "Documents",
               menuSubItem("Most Global Cited Documents",tabName = "mostGlobalCitDoc",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Most Local Cited Documents",tabName = "mostLocalCitDoc",icon = icon("chevron-right", lib = "glyphicon")),
               "Cited References",
               menuSubItem("Most Local Cited References",tabName = "mostLocalCitRef",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("References Spectroscopy",tabName = "ReferenceSpect",icon = icon("chevron-right", lib = "glyphicon")),
               "Words",
               menuSubItem("Most Frequent Words",tabName = "mostFreqWords",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("WordCloud", tabName = "wcloud",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("TreeMap",tabName = "treemap",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Word Dynamics",tabName = "wordDynamics",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Trend Topics",tabName = "trendTopic",icon = icon("chevron-right", lib = "glyphicon"))
      ),
      menuItem("Clustering", tabName = "clustering",icon = fa_i(name ="spinner"),startExpanded = FALSE,
               menuSubItem("Clustering by Coupling",tabName = "coupling",icon = icon("chevron-right", lib = "glyphicon"))),
      menuItem("Conceptual Structure",tabName = "concepStructure",icon = fa_i(name="spell-check"),startExpanded = FALSE,
               "Network Approach",
               menuSubItem("Co-occurence Network",tabName = "coOccurenceNetwork",icon = icon("chevron-right", lib = "glyphicon") ),
               menuSubItem("Thematic Map",tabName = "thematicMap", icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Thematic Evolution",tabName = "thematicEvolution", icon = icon("chevron-right", lib = "glyphicon")),
               "Factorial Approach",
               menuSubItem("Factorial Analysis", tabName = "factorialAnalysis", icon = icon("chevron-right", lib = "glyphicon"))),
      menuItem("Intellectual Structure",tabName = "intStruct",icon = fa_i(name="gem"), startExpanded = FALSE,
               menuSubItem("Co-citation Network",tabName = "coCitationNetwork", icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Historiograph",tabName = "historiograph", icon = icon("chevron-right", lib = "glyphicon"))),
      menuItem("Social Structure",tabName = "socialStruct", icon = fa_i("users"),startExpanded = FALSE,
               menuSubItem("Collaboration Network",tabName = "collabNetwork",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Collaboration WorldMap", tabName = "collabWorldMap",icon = icon("chevron-right", lib = "glyphicon")))
    )
  })
  
  ### Apply API ----
  observe({
    toggleElement(
      id ="rest_of_sidebar",
      condition = input[["apiApply"]]
    )
  })
  
  output$rest_of_sidebar <- renderMenu({
    sidebarMenu(
      menuItem("Filters",tabName = "filters",icon = fa_i(name ="filter")),
      menuItem("Overview",tabName = "overview",icon=fa_i(name = "table"),startExpanded = FALSE,
               menuSubItem("Main Information",tabName="mainInfo",icon = icon("chevron-right",lib = "glyphicon")),
               menuSubItem("Annual Scientific Production",tabName = "annualScPr",icon = icon("chevron-right",lib = "glyphicon")),
               menuSubItem("Average Citations per Year",tabName = "averageCitPerYear",icon = icon("chevron-right",lib = "glyphicon")),
               menuSubItem("Three-Field Plot", tabName ="threeFieldPlot",icon = icon("chevron-right",lib = "glyphicon"))),
      menuItem("Sources", tabName = "sources",icon = fa_i(name ="book"), startExpanded = FALSE,
               menuSubItem("Most Relevant Sources", tabName = "relevantSources",icon = icon("chevron-right",lib = "glyphicon")),
               menuSubItem("Most Local Cited Sources",tabName = "localCitedSources",icon = icon("chevron-right",lib = "glyphicon")),
               menuSubItem("Bradford's Law",tabName = "bradford",icon = icon("chevron-right",lib = "glyphicon")),
               menuSubItem("Source Impact",tabName = "sourceImpact",icon = icon("chevron-right",lib = "glyphicon")),
               menuSubItem("Source Dynamics",tabName = "sourceDynamics",icon = icon("chevron-right",lib = "glyphicon"))),
      menuItem("Authors", tabName = "authors",icon = fa_i(name="user"),startExpanded = FALSE,
               "Authors",
               menuSubItem("Most Relevant Authors", tabName = "mostRelAuthors",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Most Local Cited Authors",tabName = "mostLocalCitedAuthors",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Authors' Production over Time",tabName = "authorsProdOverTime",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Lotka's Law",tabName = "lotka",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Author Impact",tabName = "authorImpact",icon = icon("chevron-right", lib = "glyphicon")),
               "Affiliations",
               menuSubItem("Most Relevant Affiliations",tabName = "mostRelAffiliations",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Affiliations' Production over Time",tabName = "AffOverTime",icon = icon("chevron-right", lib = "glyphicon")),
               "Countries",
               menuSubItem("Corresponding Author's Country",tabName = "correspAuthorCountry",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Country Scientific Production",tabName = "countryScientProd",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Countries' Production over Time",tabName = "COOverTime",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Most Cited Countries",tabName = "mostCitedCountries",icon = icon("chevron-right", lib = "glyphicon"))
      ),
      menuItem("Documents", tabName = "documents",icon = fa_i(name="layer-group"), startExpanded = FALSE,
               "Documents",
               menuSubItem("Most Global Cited Documents",tabName = "mostGlobalCitDoc",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Most Local Cited Documents",tabName = "mostLocalCitDoc",icon = icon("chevron-right", lib = "glyphicon")),
               "Cited References",
               menuSubItem("Most Local Cited References",tabName = "mostLocalCitRef",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Reference Spectroscopy",tabName = "ReferenceSpect",icon = icon("chevron-right", lib = "glyphicon")),
               "Words",
               menuSubItem("Most Frequent Words",tabName = "mostFreqWords",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("WordCloud", tabName = "wcloud",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("TreeMap",tabName = "treemap",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Word Dynamics",tabName = "wordDynamics",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Trend Topics",tabName = "trendTopic",icon = icon("chevron-right", lib = "glyphicon"))
      ),
      menuItem("Clustering", tabName = "clustering",icon = fa_i(name ="spinner"),startExpanded = FALSE,
               menuSubItem("Clustering by Coupling",tabName = "coupling",icon = icon("chevron-right", lib = "glyphicon"))),
      menuItem("Conceptual Structure",tabName = "concepStructure",icon = fa_i(name="spell-check"),startExpanded = FALSE,
               "Network Approach",
               menuSubItem("Co-occurence Network",tabName = "coOccurenceNetwork",icon = icon("chevron-right", lib = "glyphicon") ),
               menuSubItem("Thematic Map",tabName = "thematicMap", icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Thematic Evolution",tabName = "thematicEvolution", icon = icon("chevron-right", lib = "glyphicon")),
               "Factorial Approach",
               menuSubItem("Factorial Analysis", tabName = "factorialAnalysis", icon = icon("chevron-right", lib = "glyphicon"))),
      menuItem("Intellectual Structure",tabName = "intStruct",icon = fa_i(name="gem"), startExpanded = FALSE,
               menuSubItem("Co-citation Network",tabName = "coCitationNetwork", icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Historiograph",tabName = "historiograph", icon = icon("chevron-right", lib = "glyphicon"))),
      menuItem("Social Structure",tabName = "socialStruct", icon = fa_i("users"),startExpanded = FALSE,
               menuSubItem("Collaboration Network",tabName = "collabNetwork",icon = icon("chevron-right", lib = "glyphicon")),
               menuSubItem("Collaboration WorldMap", tabName = "collabWorldMap",icon = icon("chevron-right", lib = "glyphicon")))
    )
  })
  
  ## Load Menu ----
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
  
  DATAloading<- eventReactive(input$applyLoad,{
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    if (input$load=="demo"){
      data(management, package="bibliometrixData")
      values = initial(values)
      row.names(management) <- management$SR
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
      switch(ext,
             ### excel format
             xlsx={
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
                    pageLength = 5,
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
                    placeholder = NULL
                    ),
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
               D <-
                 dsApiRequest(
                   token = values$dsToken,
                   query = values$dsQuery,
                   limit = input$sliderLimit
                 )
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
             if (input$pmQueryText !=" ") {
               D <-
                 pmApiRequest(
                   query = values$pmQuery,
                   limit = input$pmSliderLimit,
                   api_key = NULL
                 )
               M <- convert2df(D, "pubmed", "api")
               values$ApiOk <- 1
               values$M <- M
               values$Morig = M
               values$Histfield = "NA"
               values$results = list("NA")
             }
           })
  })
  
  output$apiContents <- DT::renderDT({
    APIDOWNLOAD()
    contentTable(values)
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
                    pageLength = 5,
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
  
  output$textDim <-  renderUI({
    str1=paste("Documents    ",dim(values$M)[1]," of ",dim(values$Morig)[1])
    str2=paste("Sources      ",length(unique(values$M$SO))," of ", length(unique(values$Morig$SO)))
    str3=paste("Authors      ",length(unique(unlist(strsplit(values$M$AU,";"))))," of ", length(unique(unlist(strsplit(values$Morig$AU,";")))))
    HTML(paste("<pre class='tab'>",str1, str2, str3, sep = '<br/>'))
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
                    options = list(pageLength = 10, dom = 'Bfrtip',
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
  
  # OVERVIEW ----
  ### Main Info ----
  output$MainInfo <- DT::renderDT({
    DT::datatable(values$TABvb , rownames = FALSE, extensions = c("Buttons"),
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
                                 columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                   list(width = '350px', targets = 0))),
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(values$TABvb)[1],  backgroundColor = 'white',textAlign = 'left', fontSize = '110%') %>%
      formatStyle(names(values$TABvb)[2],  backgroundColor = 'white',textAlign = 'right', fontSize = '110%')
  })

  #### box1 ---------------
  output$Timespan <- renderValueBox({
    TAB <- ValueBoxes(values$M)
    values$TABvb <- TAB
    valueBox(value = p(TAB[TAB$Description=="Timespan", 1], style = 'font-size:16px;color:white;'),
             subtitle = p(strong((TAB[TAB$Description=="Timespan", 2])), style = 'font-size:36px;color:white;', align="center"), 
             icon = fa_i(name="hourglass"), color = "blue",
             width = NULL)
  })
  
  #### box2 ---------------
  output$au <- renderValueBox({
    TAB <- values$TABvb
    valueBox(value = p(TAB[TAB$Description=="Authors", 1], style = 'font-size:16px;color:white;'),
             subtitle = p(strong(TAB[TAB$Description=="Authors", 2]), style = 'font-size:36px;color:white;',align="center"), 
             icon = fa_i(name="user"), color = "light-blue",
             width = NULL)
  })

  #### box3 ------------
  output$kw <- renderValueBox({
    TAB <- values$TABvb
    valueBox(value = p(TAB[TAB$Description=="Author's Keywords (DE)", 1], style = 'font-size:16px;color:white;'),
             subtitle = p(strong(TAB[TAB$Description=="Author's Keywords (DE)", 2]), style = 'font-size:36px;color:white;',align="center"), 
             icon = fa_i(name="spell-check"), color = "aqua",
             width = NULL)
  })
  
  #### box4 ---------------
  output$so <- renderValueBox({
    TAB <- values$TABvb
    valueBox(value = p("Sources", style = 'font-size:16px;color:white;'),
             subtitle = p(strong(TAB[TAB$Description=="Sources (Journals, Books, etc)", 2]), style = 'font-size:36px;color:white;',align="center"), 
             icon = fa_i(name ="book"), color = "blue",
             width = NULL)
  })
  
  #### box5 --------------------
  output$auS1 <- renderValueBox({
    TAB <- values$TABvb
    valueBox(value = p(TAB[TAB$Description=="Authors of single-authored docs", 1], style = 'font-size:16px;color:white;'),
             subtitle = p(strong(TAB[TAB$Description=="Authors of single-authored docs", 2]), style = 'font-size:36px;color:white;',align="center"), 
             icon = fa_i(name="pen-fancy"), color = "light-blue",
             width = NULL)
  }) 
  
  #### box6 -------------
  output$cr <- renderValueBox({
    TAB <- values$TABvb
    valueBox(value = p(TAB[TAB$Description=="References", 1], style = 'font-size:16px;color:white;'),
             subtitle = p(strong(TAB[TAB$Description=="References", 2]), style = 'font-size:36px;color:white;',align="center"), 
             icon = fa_i(name="file"), color = "aqua",
             width = NULL)
  })
  
  #### box7 ----------------
  output$doc <- renderValueBox({
    TAB <- values$TABvb
    valueBox(value = p(TAB[TAB$Description=="Documents", 1], style = 'font-size:16px;color:white;'),
             subtitle = p(strong(TAB[TAB$Description=="Documents", 2]), style = 'font-size:36px;color:white;',align="center"), 
             icon = fa_i(name="layer-group"), color = "blue",
             width = NULL)
  })
  
  #### box8 ---------------
  output$col <- renderValueBox({
    TAB <- values$TABvb
    valueBox(value = p(strong("International Co-Authorship"), style = 'font-size:16px;color:white;'),
             subtitle = p(strong(TAB[TAB$Description=="International co-authorships %", 2]," %"), style = 'font-size:36px;color:white;',align="center"), 
             icon = icon("globe",lib = "glyphicon"), color = "light-blue",
             width = NULL)
  })

  #### box9 ---------------
  output$agePerDoc <- renderValueBox({
    TAB <- values$TABvb
    valueBox(value = p(TAB[TAB$Description=="Document Average Age", 1], style = 'font-size:16px;color:white;'),
             subtitle = p(strong(TAB[TAB$Description=="Document Average Age", 2]), style = 'font-size:36px;color:white;',align="center"), 
             icon = fa_i(name="calendar"), color = "aqua",
             width = NULL)
  })
  
  #### box10 ------------------
  output$cagr <- renderValueBox({
    TAB <- values$TABvb
    valueBox(value = p(strong("Annual Growth Rate"), style = 'font-size:16px;color:white;'),
             subtitle = p(strong(TAB[TAB$Description=="Annual Growth Rate %", 2]," %"), style = 'font-size:36px;color:white;',align="center"), 
             icon = icon("arrow-up", lib="glyphicon"), color = "blue",
             width = NULL)
  })
  
  #### box11 ------
  output$coAuPerDoc <- renderValueBox({
    TAB <- values$TABvb
    valueBox(value = p(TAB[TAB$Description=="Co-Authors per Doc", 1], style = 'font-size:16px;color:white;'),
             subtitle = p(strong(TAB[TAB$Description=="Co-Authors per Doc", 2]), style = 'font-size:36px;color:white;',align="center"), 
             icon = fa_i(name="users"), color = "light-blue",
             width = NULL)
  })
  
  #### box12 -------
  output$tc <- renderValueBox({
    TAB <- values$TABvb
    valueBox(value = p(TAB[TAB$Description=="Average citations per doc", 1], style = 'font-size:16px;color:white;'),
             subtitle = p(strong(TAB[TAB$Description=="Average citations per doc", 2]), style = 'font-size:36px;color:white;',align="center"), 
             icon = icon("volume-up", lib = "glyphicon"), color = "aqua",
             width = NULL)
  })
  
  # Annual Production ----
  output$CAGR <- renderText({
    paste0(values$GR," %")
  })
  
  output$AnnualProdPlot <- renderPlotly({
    res <- descriptive(values,type="tab2")
    values <-res$values
    Y <- values$TAB
    
    names(Y)=c("Year","Freq")
    x <- c(max(Y$Year)-0.02-diff(range(Y$Year))*0.125, max(Y$Year)-0.02)+1
    y <- c(min(Y$Freq),min(Y$Freq)+diff(range(Y$Freq))*0.125)

    g=ggplot2::ggplot(Y, aes(x = .data$Year, y = .data$Freq, text=paste("Year: ",.data$Year,"\nN .of Documents: ",.data$Freq))) +
      geom_line(aes(group="NA")) +
      geom_area(aes(group="NA"),fill = 'grey90', alpha = .5) +
      labs(x = 'Year'
           , y = 'Articles'
           , title = "Annual Scientific Production") +
      scale_x_continuous(breaks= (Y$Year[seq(1,length(Y$Year),by=2)])) +
      theme(text = element_text(color = "#444444")
            ,panel.background = element_rect(fill = '#FFFFFF')
            ,panel.grid.minor = element_line(color = '#EFEFEF')
            ,panel.grid.major = element_line(color = '#EFEFEF')
            ,plot.title = element_text(size = 24)
            ,axis.title = element_text(size = 14, color = '#555555')
            ,axis.title.y = element_text(vjust = 1, angle = 0)
            ,axis.title.x = element_text(hjust = 0)
            ,axis.text.x = element_text(vjust = 1, angle = 90)
            ,axis.line.x = element_line(color="black",size=0.5)
            ,axis.line.y = element_line(color="black",size=0.5)
           ) +
      annotation_custom(values$logoGrid, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
    values$ASPplot <- g
    
    plot.ly(g,flip=FALSE, side="r", aspectratio=1.7, size=0.10)
  })
  
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
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
      geom_area(aes(x = .data$Year, y = .data$MeanTCperYear, group=.data$group),fill = 'grey90', alpha = .5) +
      labs(x = 'Year'
           , y = 'Citations'
           , title = "Average Article Citations per Year")+
      scale_x_continuous(breaks= (Table2$Year[seq(1,length(Table2$Year),by=2)])) +
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
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%') %>% 
      formatRound(names(TAB)[c(3:4)], digits=2)
  })
  
  ## Three Fields Plot ---- 
  TFP <- eventReactive(input$apply3F,{
    fields=c(input$LeftField, input$CentralField, input$RightField)
    threeFieldsPlot(values$M, fields=fields,n=c(input$LeftFieldn, input$CentralFieldn,input$RightFieldn))
  })
  
  output$ThreeFieldsPlot <- renderPlotly({
    TFP()  
  })
  
  # SOURCES MENU ----
  ### Most Relevant Sources ----
  MRSources <- eventReactive(input$applyMRSources,{
    res <- descriptive(values,type="tab7")
    values <-res$values
    values$TABSo<-values$TAB
    xx<- values$TAB %>% 
      drop_na()
    if (input$MostRelSourcesK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostRelSourcesK}
    xx <- xx %>% 
      slice_head(n=k)
    xx$Sources=substr(xx$Sources,1,50)
    
    g <- freqPlot(xx,x=2,y=1, textLaby = "Sources", textLabx = "N. of Documents", title = "Most Relevant Sources", values)
    
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
  })
  
  output$MostRelSourcesTable <- DT::renderDT({
    
    g <- MRSources()
    
    TAB <- values$TABSo %>% drop_na()
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  
  ### Most Local Cited Sources ---- 
  MLCSources <- eventReactive(input$applyMLCSources,{
    values$M=metaTagExtraction(values$M,"CR_SO")
    TAB=tableTag(values$M,"CR_SO")
    TAB=data.frame(Sources=names(TAB),Articles=as.numeric(TAB),stringsAsFactors = FALSE)
    values$TABSoCit<-TAB
    xx<- TAB
    if (input$MostRelCitSourcesK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostRelCitSourcesK}
    xx=subset(xx, row.names(xx) %in% row.names(xx)[1:k])
    xx$Articles=as.numeric(xx$Articles)
    xx$Sources=substr(xx$Sources,1,50)
    
    g <- freqPlot(xx,x=2,y=1, textLaby = "Cited Sources", textLabx = "N. of Local Citations", title = "Most Local Cited Sources", values)
    
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
  })
  
  output$MostRelCitSourcesTable <- DT::renderDT({
    
    g <- MLCSources()
    TAB <- values$TABSoCit
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  
  ### Bradford's Law ---- 
  output$bradfordPlot <- renderPlotly({
    values$bradford=bradford(values$M)
    plot.ly(values$bradford$graph,flip=FALSE, side="r", aspectratio=1.6, size=0.15)
  })
  
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
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  
  ### Sources' Impact ----  
  Hsource <- eventReactive(input$applyHsource,{
    withProgress(message = 'Calculation in progress',
                 value = 0, {
                   res <- Hindex_plot(values,type="source", input)
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
  })
  
  output$SourceHindexTable <- DT::renderDT({
    
    DT::datatable(values$H, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
      formatStyle(names(values$H),  backgroundColor = 'white',textAlign = 'center') %>% 
      formatRound(names(values$H)[4], digits=3)
  })
  
  ### Source Growth ----  
  SOGrowth <- eventReactive(input$applySOGrowth,{

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
    
    values$SDplot <- g
    return(g)
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
  })
  
  output$soGrowthtable <- DT::renderDT({
    
    g <- SOGrowth()
    soData=values$PYSO
    
    DT::datatable(soData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  })
  
  # AUTHORS MENU ----
  ## Authors ----
  ### Most Relevant Authors ----
  MRAuthors <- eventReactive(input$applyMRAuthors,{
    res <- descriptive(values,type="tab3")
    values <-res$values
    values$TABAu<-values$TAB
    
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
    
    g <- freqPlot(xx,x=2,y=1, textLaby = "Authors", textLabx = lab, title = "Most Relevant Authors", values)
    
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
  })
  
  output$MostRelAuthorsTable <- DT::renderDT({
    
    TAB <- values$TABAu
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  
  ### Most Cited Authors ----  
  MLCAuthors <- eventReactive(input$applyMLCAuthors,{
    res <- descriptive(values,type="tab13")
    values <-res$values
    values$TABAuCit<-values$TAB
    
    xx <- values$TABAuCit
    lab <- "Local Citations"
    xx[,2]=as.numeric(xx[,2])
    
    if (input$MostCitAuthorsK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostCitAuthorsK}
    
    xx=xx[1:k,]
    xx[,2]=round(xx[,2],1)
    xx <- xx[order(-xx[,2]),]
    
    g <- freqPlot(xx,x=2,y=1, textLaby = "Authors", textLabx = lab, title = "Most Local Cited Authors", values)
    
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
  })
  
  output$MostCitAuthorsTable <- DT::renderDT({
    
    TAB <- values$TABAuCit
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  
  ### Authors' Impact ----  
  HAuthors <- eventReactive(input$applyHAuthors,{
    withProgress(message = 'Calculation in progress',
                 value = 0, {
                   res <- Hindex_plot(values,type="author", input)
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
  })
  
  output$AuthorHindexTable <- DT::renderDT({
    
    DT::datatable(values$H, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  
  ### Authors Production Over Time ----  
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
  })
  
  output$TopAuthorsProdTable <- DT::renderDT({
    AUoverTime()
    
    TAB <- values$AUProdOverTime$dfAU
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  
  ### Lotka Law ----  
  output$lotkaPlot <- renderPlotly({
    
    values$lotka=lotka(biblioAnalysis(values$M))
    AuProd=values$lotka$AuthorProd
    AuProd$Theoretical=10^(log10(values$lotka$C)-2*log10(AuProd[,1]))
    AuProd$Theoretical=AuProd$Theoretical/sum(AuProd$Theoretical)
    
    x <- c(max(AuProd$N.Articles)-0.02-diff(range(AuProd$N.Articles))*0.125, max(AuProd$N.Articles)-0.02)+1
    y <- c(min(AuProd$Freq*100),min(AuProd$Freq*100)+diff(range(AuProd$Freq*100))*0.125)
    
    g=ggplot2::ggplot(AuProd, aes(x = .data$N.Articles, y = .data$Freq*100, text=paste("N.Articles: ",.data$N.Articles,"\n% of production: ",round(.data$Freq*100,1)))) +
      geom_line(aes(group="NA")) +
      geom_area(aes(group="NA"),fill = 'grey90', alpha = .5) +
      geom_line(data=AuProd, aes(y=.data$Theoretical*100, group="NA"),linetype = "dashed",color="black",alpha=0.8)+
      xlim(0,max(AuProd$N.Articles)+1)+
      labs(x = 'Documents written'
           , y = '% of Authors'
           , title = "The Frequency Distribution of Scientific Productivity") +
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
      annotation_custom(values$logoGrid, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
    values$LLplot <- g
    plot.ly(g,flip=FALSE, side="r", aspectratio=1.4, size=0.10)
  })
  
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
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  ### Most Relevant Affiliations ---- 
  MRAffiliations <- eventReactive(input$applyMRAffiliations,{
    if (input$disAff=="Y"){
      res <- descriptive(values,type="tab11")
    }else{
      res <- descriptive(values,type="tab12")
    }
    xx=values$TAB
    names(xx)=c("AFF","Freq")
    values <-res$values
    values$TABAff <- values$TAB
    
    if (input$MostRelAffiliationsK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostRelAffiliationsK}
    
    xx=xx[1:k,]
    
    g <- freqPlot(xx,x=2,y=1, textLaby = "Affiliations", textLabx = "Articles", title = "Most Relevant Affiliations", values)
    
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
  })
  
  output$MostRelAffiliationsTable <- DT::renderDT({
    g <- MRAffiliations()
    
    TAB <- values$TABAff
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  
  ### Affiliation OverTime ----  
  AFFGrowth <- eventReactive(input$applyAFFGrowth,{
    
    values <- AffiliationOverTime(values,input$topAFF)
    
  }) 
  
  output$AffOverTimeplot.save <- downloadHandler(
    filename = function() {
      paste("AffiliationOverTime-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$AffOverTimePlot, dpi = as.numeric(input$AFFGrowthdpi), height = input$SDh, width = input$AFFGrowthh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$AffOverTimePlot <- renderPlotly({
    
    AFFGrowth()
    g <- values$AffOverTimePlot
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
  })
  
  output$AffOverTimeTable <- DT::renderDT({
    
    AFFGrowth()
    afftimeData=values$AffOverTime
    
    DT::datatable(afftimeData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Affiliation_over_Time',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Affiliation_over_Time',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Affiliation_over_Time',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(afftimeData))-1))))) %>%
      formatStyle(names(afftimeData),  backgroundColor = 'white') 
  })
  
  # Countries ----
  ### Country by Corresponding Authors ----  
  CAUCountries <- eventReactive(input$applyCAUCountries,{
    res <- descriptive(values,type="tab5")
    values <-res$values
    values$TABCo <- values$TAB
    
    k=input$MostRelCountriesK
    xx <- values$TABCo %>% slice_head(n=k) %>% 
      select(.data$Country,.data$SCP,.data$MCP)
    xx=xx[order(-(xx$SCP+xx$MCP)),]
    xx1=cbind(xx[,1:2],rep("SCP",k))
    names(xx1)=c("Country","Freq","Collaboration")
    xx2=cbind(xx[,c(1,3)],rep("MCP",k))
    names(xx2)=c("Country","Freq","Collaboration")
    xx=rbind(xx2,xx1)
    xx$Country=factor(xx$Country,levels=xx$Country[1:dim(xx2)[1]])
    
    xx2 <- xx %>% dplyr::group_by(.data$Country) %>%
      dplyr::summarize(Freq = sum(.data$Freq))
    
    x <- c(0.5,0.5+length(levels(xx2$Country))*0.125)+1
    y <- c(max(xx2$Freq)-0.02-diff(range(xx2$Freq))*0.125,max(xx2$Freq)-0.02)
    
    g=suppressWarnings(ggplot2::ggplot(data=xx, aes(x=.data$Country, y=.data$Freq,fill=.data$Collaboration, text=paste("Country: ",.data$Country,"\nN.of Documents: ",.data$Freq))) +
                         geom_bar(aes(group="NA"),stat="identity")+
                         scale_x_discrete(limits = rev(levels(xx$Country)))+
                         scale_fill_discrete(name="Collaboration",
                                             breaks=c("SCP","MCP"))+
                         labs(title = "Corresponding Author's Country", x = "Countries", y = "N. of Documents", 
                              caption = "SCP: Single Country Publications, MCP: Multiple Country Publications")+
                         theme(plot.caption = element_text(size = 9, hjust = 0.5,
                                                           color = "blue", face = "italic")
                               ,panel.background = element_rect(fill = '#FFFFFF')
                               ,panel.grid.major.y  = element_line(color = '#EFEFEF')
                               ,plot.title = element_text(size = 24)
                               ,axis.title = element_text(size = 14, color = '#555555')
                               ,axis.title.y = element_text(vjust = 1, angle = 0)
                               ,axis.title.x = element_text(hjust = 0)
                               ,axis.line.x = element_line(color="black",size=0.5)
                               ) +
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
  })
  
  output$MostRelCountriesTable <- DT::renderDT({
    g <- CAUCountries()
    
    TAB <- values$TABCo
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%') %>% 
      formatRound(names(TAB)[5], digits=3) %>% 
      formatRound(names(TAB)[6], digits=3)
  })
  
  ### Country Production ----  
  output$countryProdPlot <- renderPlotly({
    values$mapworld<-mapworld(values$M, values)
    plot.ly(values$mapworld$g,flip=FALSE, side="r", aspectratio=1.7, size=0.07, data.type=1,height=15)
  })
  
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
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  
  ### Countries' Production OverTime ----  
  COGrowth <- eventReactive(input$applyCOGrowth,{
    
    values <- CountryOverTime(values,input$topCO)
    
  }) 
  
  output$CountryOverTimeplot.save <- downloadHandler(
    filename = function() {
      paste("CountryOverTime-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$CountryOverTimePlot, dpi = as.numeric(input$COGrowthdpi), height = input$COGrowthh, width = input$COGrowthh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$CountryOverTimePlot <- renderPlotly({
    
    COGrowth()
    g <- values$CountryOverTimePlot
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
  })
  
  output$CountryOverTimeTable <- DT::renderDT({
    
    COGrowth()
    cotimeData=values$CountryOverTime
    
    DT::datatable(cotimeData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Countries_Production_Over_Time',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Countries_Production_Over_Time',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Countries_Production_Over_Time',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(cotimeData))-1))))) %>%
      formatStyle(names(cotimeData),  backgroundColor = 'white') 
  })
  
  ### Most Cited Country ----    
  MCCountries <- eventReactive(input$applyMCCountries,{
    res <- descriptive(values,type="tab6")
    values <-res$values
    values$TABCitCo <- values$TAB
    
    xx=values$TAB
    xx[,2]=as.numeric(xx[,2])
    xx[,3]=as.numeric(xx[,3])
    if (input$MostCitCountriesK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostCitCountriesK}
    if (input$CitCountriesMeasure=="TC"){
      xx=xx[1:k,c(1,2)]
      laby="N. of Citations"
    } else {
      xx=xx[order(-xx[,3]),]
      xx=xx[1:k,c(1,3)]
      laby="N. of Citations per Year"
    }
    
    g <- freqPlot(xx,x=2,y=1, textLaby = "Countries", textLabx = laby, title = "Most Cited Countries", values)
    
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
  })
  
  output$MostCitCountriesTable <- DT::renderDT({
    g <- MCCountries()
    TAB <- values$TABCitCo
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%') %>% 
      formatRound(names(TAB)[3], 2)
  })
  
  # DOCUMENTS MENU ----
  ## Documents ----
  ### Most Global Cited Documents ----
  
  MGCDocuments <- eventReactive(input$applyMGCDocuments,{
    res <- descriptive(values,type="tab4")
    values <-res$values
    values$TABGlobDoc <- values$TAB
    
    if (input$CitDocsMeasure=="TC"){
      xx <- values$TABGlobDoc %>% select(1,3)
      lab="Global Citations"} else {
        xx <- values$TABGlobDoc %>% select(1,4)
        lab="Global Citations per Year"
      }
    
    if (input$MostCitDocsK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostCitDocsK}
    
    xx=xx[1:k,]
    
    g <- freqPlot(xx,x=2,y=1, textLaby = "Documents", textLabx = lab, title = "Most Global Cited Documents", values)
    
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
  })
  
  output$MostCitDocsTable <- DT::renderDT({
    g <- MGCDocuments()
    TAB <- values$TABGlobDoc
    TAB$DOI<- paste0('<a href=\"https://doi.org/',TAB$DOI,'\" target=\"_blank\">',TAB$DOI,'</a>')
    DT::datatable(TAB, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%') %>% 
      formatRound(names(TAB)[4], 2) %>%
      formatRound(names(TAB)[5], 2)
  })
  
  ### Most Local Cited Documents ----  
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
    
    g <- freqPlot(xx,x=4,y=1, textLaby = "Documents", textLabx = "Local Citations", title = "Most Local Cited Documents", values)
    
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
  })
  
  output$MostLocCitDocsTable <- DT::renderDT({
    
    TAB <- values$TABLocDoc
    TAB$DOI <- paste0('<a href=\"https://doi.org/',TAB$DOI,'\" target=\"_blank\">',TAB$DOI,'</a>')
    
    names(TAB)[4:8] <- c("Local Citations", "Global Citations","LC/GC Ratio (%)", "Normalized Local Citations","Normalized Global Citations")
    DT::datatable(TAB, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  
  ## Cited References ----
  ### Most Local Cited References ----
  MLCReferences <- eventReactive(input$applyMLCReferences,{
    CR <- citations(values$M,sep=input$CitRefsSep)$Cited
    TAB <- data.frame(names(CR),as.numeric(CR),stringsAsFactors = FALSE)
    names(TAB) <- c("Cited References", "Citations")
    values$TABCitRef <- TAB %>% filter(.data$`Cited References`!="ANONYMOUS, NO TITLE CAPTURED")
    
    xx=values$TABCitRef
    if (input$MostCitRefsK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostCitRefsK}
    
    xx=xx[1:k,]

    g <- freqPlot(xx,x=2,y=1, textLaby = "References", textLabx = "Local Citations", title = "Most Local Cited References", values)
    
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
  })
  
  output$MostCitRefsTable <- DT::renderDT({
    g <- MLCReferences()
    TAB <- values$TABCitRef
    
    TAB$link <- trimES(gsub("[[:punct:]]" , " ",reduceRefs(TAB[,1])))
    TAB$link <- paste0('<a href=\"https://scholar.google.it/scholar?hl=en&as_sdt=0%2C5&q=',TAB$link,'\" target=\"_blank\">','link','</a>')
    
    TAB=TAB[,c(3,1,2)]
    names(TAB)[1]="Google Scholar"
    DT::datatable(TAB, rownames = FALSE, escape=FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  
  ### Reference Spectroscopy ---- 
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
  })
  
  output$rpysTable <- DT::renderDT({
    RPYS()
    rpysData=values$res$rpysTable
    
    DT::datatable(rpysData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  })
  
  output$crTable <- DT::renderDT({
    RPYS()
    crData=values$res$CR
    crData$link <- paste0('<a href=\"https://scholar.google.it/scholar?hl=en&as_sdt=0%2C5&q=',crData$Reference,'\" target=\"_blank\">','link','</a>')
    
    crData=crData[order(-as.numeric(crData$Year),-crData$Freq),]
    names(crData)=c("Year", "Reference", "Local Citations", "Google link")
    crData <- crData[,c(1,4,2,3)] 
    DT::datatable(crData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  })
  
  ## Words ----
  ### Most Frequent Words ----
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
           DE={lab="Author's Keywords"},
           TI={lab="Title's Words"},
           AB={lab="Abstract's Words"},
           WC={lab="Subject Categories"})
    
    g <- freqPlot(xx,x=2,y=1, textLaby = lab, textLabx = "Occurrences", title = "Most Relevant Words", values)
    
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
  })
  
  output$MostRelWordsTable <- DT::renderDT({
    g <- MFWords()
    
    TAB <- values$TABWord
    
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
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
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TAB))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%')
  })
  
  ### WordCloud ----  
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
  
  ### TreeMap ----  
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
  
  ### Word Dynamics ----   
  WDynamics <- eventReactive(input$applyWD,{
    if (input$cumTerms=="Cum"){
      cdf=TRUE
      laby="Cumulate occurrences"
    }else{
      cdf=FALSE
      laby="Annual occurrences"}

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
             values$M=termExtraction(values$M,Field = "TI", verbose=FALSE, ngrams=as.numeric(input$growthTermsngrams), remove.terms=remove.terms, synonyms = synonyms)
             KW=KeywordGrowth(values$M, Tag = "TI_TM", sep = ";", top = input$topkw[2], cdf = cdf)
           },
           AB={
             values$M=termExtraction(values$M,Field = "AB", verbose=FALSE, ngrams=as.numeric(input$growthTermsngrams), remove.terms=remove.terms, synonyms = synonyms)
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
      scale_x_continuous(breaks= (values$KW$Year[seq(1,length(values$KW$Year),by=ceiling(length(values$KW$Year)/20))])) +
      geom_hline(aes(yintercept=0), alpha=0.1)+
      labs(color = "Term")+
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
      ) + 
      annotation_custom(values$logoGrid, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
    
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
  })
  
  output$kwGrowthtable <- DT::renderDT({
    g <- WDynamics()
    kwData=values$KW
    
    DT::datatable(kwData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  })
  
  ### Trend Topics ----
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
  })
  
  output$trendTopicsTable <- DT::renderDT({
    TrendTopics()
    tpData=values$trendTopics$df_graph
    
    DT::datatable(tpData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  })
  
  # CLUSTERING ----
  ### Clustering by Coupling ----
  CMMAP <- eventReactive(input$applyCM,{
    
    values$CM <- couplingMap(values$M, analysis=input$CManalysis, field=input$CMfield, 
                             n=input$CMn, minfreq=input$CMfreq,
                             ngrams=as.numeric(input$CMngrams),
                             community.repulsion = input$CMrepulsion,
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
  })
  
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
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  })
  
  output$CMTableCluster <- DT::renderDT({
    CMMAP()
    cmData=values$CM$clusters[,c(7,1:4,6)]
    
    DT::datatable(cmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  })
  
  # CONCEPTUAL STRUCTURE ----
  ### Network approach ----
  #### Co-occurrences network ----
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
  
  output$network.coc <- downloadHandler(
    filename = "Co_occurrence_network.net",
    content <- function(file) {
      igraph::write.graph(values$cocnet$graph_pajek,file=file, format="pajek")
    },
    contentType = "net"
  )
  
  ##### save coc network image as html ####
  output$networkCoc.fig <- downloadHandler(
    filename = "network.html",
    content <- function(con) {
      savenetwork(con, values)
    },
    contentType = "html"
  )
  
  ### save coc network as png ###
  observeEvent(input$cocPlot.save, {
    file <- paste("Co_occurrence_Network-", Sys.Date(), ".png", sep="")
    screenshot(selector="#cocPlot", scale=input$cocRes, filename=file)
  })
  
  output$cocTable <- DT::renderDT({
    COCnetwork()
    cocData=values$cocnet$cluster_res
    names(cocData)=c("Node", "Cluster", "Betweenness", "Closeness", "PageRank")
    DT::datatable(cocData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"), filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  })
  
  ### Degree Plot Co-word analysis ----
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
  },  width = exprToFunction(as.numeric(input$dimension[1])*0.6), 
  height = exprToFunction(as.numeric(input$dimension[2])*0.85),
  res = 150)

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
  }, width = exprToFunction(as.numeric(input$dimension[1])*0.6), 
  height = exprToFunction(as.numeric(input$dimension[2])*0.85),
  res = 150)
  
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
  }, width = exprToFunction(as.numeric(input$dimension[1])*0.6), 
  height = exprToFunction(as.numeric(input$dimension[2])*0.85),
  res = 150)
  
  output$CSPlot4 <- renderPlot({
    CSfactorial()
    if (values$CS[[1]][1]!="NA"){
      plot(values$CS$graph_dendogram)
    }else{
      emptyPlot("Selected field is not included in your data collection")
    }
  }, width = exprToFunction(as.numeric(input$dimension[1])*0.6), 
  height = exprToFunction(as.numeric(input$dimension[2])*0.85),
  res = 150)
  
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
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  })
  
  output$CSTableD <- DT::renderDT({
    CSfactorial()
    CSData=values$CS$docCoord
    CSData=data.frame(Documents=row.names(CSData),CSData,stringsAsFactors = FALSE)
    CSData$dim1=round(CSData$dim1,2)
    CSData$dim2=round(CSData$dim2,2)
    CSData$contrib=round(CSData$contrib,2)
    DT::datatable(CSData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
                             community.repulsion = input$TMrepulsion,
                             stemming=input$TMstemming, size=input$sizeTM, cluster=input$TMCluster,
                             n.labels=input$TMn.labels, repel=FALSE, remove.terms=remove.terms, synonyms=synonyms)
    
    validate(
      need(values$TM$nclust > 0, "\n\nNo topics in one or more periods. Please select a different set of parameters.")
    )
  })
  output$TMPlot <- renderPlotly({
    TMAP()
    plot.ly(values$TM$map, size=0.07, aspectratio = 1.3)
  })
  
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
    tmData=values$TM$words[,-c(4,6)]

    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Terms',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Terms',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Terms',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
  })
  
  output$TMTableCluster <- DT::renderDT({
    TMAP()
    tmData <- values$TM$clusters[,c(9,5:8,11)]
    names(tmData) <- c("Cluster", "CallonCentrality","CallonDensity","RankCentrality","RankDensity","ClusterFrequency") 
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
  })
  
  output$TMTableDocument <- DT::renderDT({
    TMAP()
    tmDataDoc <- values$TM$documentToClusters
    tmDataDoc$DI<- paste0('<a href=\"https://doi.org/',tmDataDoc$DI,'\" target=\"_blank\">',tmDataDoc$DI,'</a>')
    names(tmDataDoc)[1:9] <- c("DOI", "Authors","Title","Source","Year","TotalCitation","TCperYear","NTC","SR") 
    
    DT::datatable(tmDataDoc, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmDataDoc))-1))))) %>%
      formatStyle(names(tmDataDoc),  backgroundColor = 'white') %>% 
      formatRound(names(tmDataDoc)[7:8], 3) %>% 
      formatRound(names(tmDataDoc)[c(10:(ncol(tmDataDoc)-2),ncol(tmDataDoc))], 3) %>% 
      formatRound(names(tmDataDoc)[ncol(tmDataDoc)], 3) 
  })
  
  ### Thematic Evolution ----
  output$sliders <- renderUI({
    numSlices <- as.integer(input$numSlices)
    v=quantile(values$M$PY, seq(0,1,by=(1/(numSlices+1))), na.rm=TRUE)
    v=round(v[-c(1,length(v))],0)
    lapply(1:numSlices, function(i) {
      numericInput(inputId = paste0("Slice", i), label = paste("Cutting Year", i),value=v[i],min=min(values$M$PY, na.rm = TRUE)+1,max=max(values$M$PY, na.rm = TRUE)-1, step=1)
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
                                        cluster=input$TECluster,
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
  
  output$TEplot.save <- downloadHandler(
    filename = function() {
      #
      paste("ThematicEvolution-", Sys.Date(), ".zip", sep="")
    },
    content <- function(file) {
      #go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      for (i in 1:length(values$nexus$TM)){
        fileName <- paste("ThematicEvolution-Map_",i,"_",Sys.Date(), ".png", sep="")
        ggsave(filename = fileName, plot = values$nexus$TM[[i]]$map, dpi = as.numeric(input$TEdpi),  height = input$TEh, width = input$TEh*1.5, bg="white")
        files <- c(fileName,files)
      }
      zip(file,files)
    },
    contentType = "zip"
  )
  
  output$TETable <- DT::renderDT({
    TEMAP()
    TEData=values$nexus$Data
    TEData=TEData[TEData$Inc_index>0,-c(4,8)]
    names(TEData)=c("From", "To", "Words", "Weighted Inclusion Index", "Inclusion Index", "Occurrences", "Stability Index")
    DT::datatable(TEData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  })
  
  output$TMPlot1 <-  renderPlotly({
    TEMAP()
    if (length(values$nexus$TM)>=1){
      plot.ly(values$nexus$TM[[1]]$map, size=0.07, aspectratio = 1.3)
    } else {emptyPlot("You have selected fewer periods!")}
  })
  
  output$TMPlot2 <-  renderPlotly({
    TEMAP()
    if (length(values$nexus$TM)>=2){
      plot.ly(values$nexus$TM[[2]]$map, size=0.07, aspectratio = 1.3)
    } else {emptyPlot("You have selected fewer periods!")}
  })
  
  output$TMPlot3 <-  renderPlotly({
    TEMAP()
    if (length(values$nexus$TM)>=3){
      plot.ly(values$nexus$TM[[3]]$map, size=0.07, aspectratio = 1.3)
    } else {emptyPlot("You have selected fewer periods!")}
  })
  
  output$TMPlot4 <-  renderPlotly({
    TEMAP()
    if (length(values$nexus$TM)>=4){
      plot.ly(values$nexus$TM[[4]]$map, size=0.07, aspectratio = 1.3)
    } else (emptyPlot("You have selected fewer periods!"))
  })
  
  output$TMPlot5 <-  renderPlotly({
    TEMAP()
    if (length(values$nexus$TM)>=5){
      plot.ly(values$nexus$TM[[5]]$map, size=0.07, aspectratio = 1.3)
    } else (emptyPlot("You have selected fewer periods!"))
  })
  
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
    tmData=values$nexus$TM[[1]]$words[,-c(4,6)]
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_1_Terms',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_1_Terms',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_1_Terms',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
  })
  
  output$TMTable2 <- DT::renderDT({
    TEMAP()
    tmData=values$nexus$TM[[2]]$words[,-c(4,6)]
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_2_Terms',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_2_Terms',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_2_Terms',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
  })
  
  output$TMTable3 <- DT::renderDT({
    TEMAP()
    tmData=values$nexus$TM[[3]]$words[,-c(4,6)]
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_3_Terms',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_3_Terms',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_3_Terms',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
  })
  
  output$TMTable4 <- DT::renderDT({
    TEMAP()
    tmData=values$nexus$TM[[4]]$words[,-c(4,6)]
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_4_Terms',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_4_Terms',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_4_Terms',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
  })
  
  output$TMTable5 <- DT::renderDT({
    TEMAP()
    tmData=values$nexus$TM[[5]]$words[,-c(4,6)]
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_5_Terms',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_5_Terms',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_5_Terms',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
  })
  
  output$TMTableCluster1 <- DT::renderDT({
    TEMAP()
    tmData <- values$nexus$TM[[1]]$clusters[,c(9,5:8,11)]
    names(tmData) <- c("Cluster", "CallonCentrality","CallonDensity","RankCentrality","RankDensity","ClusterFrequency") 
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_1_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_1_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_1_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
  })
  
  output$TMTableCluster2 <- DT::renderDT({
    TEMAP()
    tmData <- values$nexus$TM[[2]]$clusters[,c(9,5:8,11)]
    names(tmData) <- c("Cluster", "CallonCentrality","CallonDensity","RankCentrality","RankDensity","ClusterFrequency")
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_2_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_2_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_2_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
  })
  
  output$TMTableCluster3 <- DT::renderDT({
    TEMAP()
    tmData <- values$nexus$TM[[3]]$clusters[,c(9,5:8,11)]
    names(tmData) <- c("Cluster", "CallonCentrality","CallonDensity","RankCentrality","RankDensity","ClusterFrequency")
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_3_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_3_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_3_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
  })
  
  output$TMTableCluster4 <- DT::renderDT({
    TEMAP()
    tmData <- values$nexus$TM[[4]]$clusters[,c(9,5:8,11)]
    names(tmData) <- c("Cluster", "CallonCentrality","CallonDensity","RankCentrality","RankDensity","ClusterFrequency")
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_4_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_4_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_4_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
  })
  
  output$TMTableCluster5 <- DT::renderDT({
    TEMAP()
    tmData <- values$nexus$TM[[5]]$clusters[,c(9,5:8,11)]
    names(tmData) <- c("Cluster", "CallonCentrality","CallonDensity","RankCentrality","RankDensity","ClusterFrequency")
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_5_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_5_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_5_Clusters',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
  })
  
  output$TMTableDocument1 <- DT::renderDT({
    TEMAP()
    tmDataDoc <- values$nexus$TM[[1]]$documentToClusters
    tmDataDoc$DI<- paste0('<a href=\"https://doi.org/',tmDataDoc$DI,'\" target=\"_blank\">',tmDataDoc$DI,'</a>')
    names(tmDataDoc)[1:9] <- c("DOI", "Authors","Title","Source","Year","TotalCitation","TCperYear","NTC","SR") 
    
    DT::datatable(tmDataDoc, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_1_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_1_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_1_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmDataDoc))-1))))) %>%
      formatStyle(names(tmDataDoc),  backgroundColor = 'white') %>% 
      formatRound(names(tmDataDoc)[7:8], 3) %>% 
      formatRound(names(tmDataDoc)[10:(ncol(tmDataDoc)-2)], 3) %>% 
      formatRound(names(tmDataDoc)[ncol(tmDataDoc)], 3)
  })
  
  output$TMTableDocument2 <- DT::renderDT({
    TEMAP()
    tmDataDoc <- values$nexus$TM[[2]]$documentToClusters
    tmDataDoc$DI<- paste0('<a href=\"https://doi.org/',tmDataDoc$DI,'\" target=\"_blank\">',tmDataDoc$DI,'</a>')
    names(tmDataDoc)[1:9] <- c("DOI", "Authors","Title","Source","Year","TotalCitation","TCperYear","NTC","SR") 
    
    DT::datatable(tmDataDoc, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_2_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_2_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_2_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmDataDoc))-1))))) %>%
      formatStyle(names(tmDataDoc),  backgroundColor = 'white') %>% 
      formatRound(names(tmDataDoc)[7:8], 3) %>% 
      formatRound(names(tmDataDoc)[10:(ncol(tmDataDoc)-2)], 3) %>% 
      formatRound(names(tmDataDoc)[ncol(tmDataDoc)], 3)
  })
  
  output$TMTableDocument3 <- DT::renderDT({
    TEMAP()
    tmDataDoc <- values$nexus$TM[[3]]$documentToClusters
    tmDataDoc$DI<- paste0('<a href=\"https://doi.org/',tmDataDoc$DI,'\" target=\"_blank\">',tmDataDoc$DI,'</a>')
    names(tmDataDoc)[1:9] <- c("DOI", "Authors","Title","Source","Year","TotalCitation","TCperYear","NTC","SR") 
    
    DT::datatable(tmDataDoc, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_3_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_3_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_3_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmDataDoc))-1))))) %>%
      formatStyle(names(tmDataDoc),  backgroundColor = 'white') %>% 
      formatRound(names(tmDataDoc)[7:8], 3) %>% 
      formatRound(names(tmDataDoc)[10:(ncol(tmDataDoc)-2)], 3) %>% 
      formatRound(names(tmDataDoc)[ncol(tmDataDoc)], 3)
  })
  
  output$TMTableDocument4 <- DT::renderDT({
    TEMAP()
    tmDataDoc <- values$nexus$TM[[4]]$documentToClusters
    tmDataDoc$DI<- paste0('<a href=\"https://doi.org/',tmDataDoc$DI,'\" target=\"_blank\">',tmDataDoc$DI,'</a>')
    names(tmDataDoc)[1:9] <- c("DOI", "Authors","Title","Source","Year","TotalCitation","TCperYear","NTC","SR") 
    
    DT::datatable(tmDataDoc, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_4_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_4_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_4_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmDataDoc))-1))))) %>%
      formatStyle(names(tmDataDoc),  backgroundColor = 'white') %>% 
      formatRound(names(tmDataDoc)[7:8], 3) %>% 
      formatRound(names(tmDataDoc)[10:(ncol(tmDataDoc)-2)], 3) %>% 
      formatRound(names(tmDataDoc)[ncol(tmDataDoc)], 3)
  })
  
  output$TMTableDocument5 <- DT::renderDT({
    TEMAP()
    tmDataDoc <- values$nexus$TM[[5]]$documentToClusters
    tmDataDoc$DI<- paste0('<a href=\"https://doi.org/',tmDataDoc$DI,'\" target=\"_blank\">',tmDataDoc$DI,'</a>')
    names(tmDataDoc)[1:9] <- c("DOI", "Authors","Title","Source","Year","TotalCitation","TCperYear","NTC","SR") 
    
    DT::datatable(tmDataDoc, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = list('pageLength',
                                                list(extend = 'copy'),
                                                list(extend = 'csv',
                                                     filename = 'Thematic_Map_Period_5_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'excel',
                                                     filename = 'Thematic_Map_Period_5_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'pdf',
                                                     filename = 'Thematic_Map_Period_5_Documents',
                                                     title = " ",
                                                     header = TRUE),
                                                list(extend = 'print')),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmDataDoc))-1))))) %>%
      formatStyle(names(tmDataDoc),  backgroundColor = 'white') %>% 
      formatRound(names(tmDataDoc)[7:8], 3) %>% 
      formatRound(names(tmDataDoc)[10:(ncol(tmDataDoc)-2)], 3) %>% 
      formatRound(names(tmDataDoc)[ncol(tmDataDoc)], 3)
  })
  
  # INTELLECTUAL STRUCTURE ####
  ### Co-citation network ----
  COCITnetwork <- eventReactive(input$applyCocit,{
    values <- intellectualStructure(input,values)
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
    },
    contentType = "net"
  )
  
  observeEvent(input$cocitPlot.save, {
    file <- paste("Co_citation_Network-", Sys.Date(), ".png", sep="")
    screenshot(selector="#cocitPlot", scale=input$cocitRes, filename=file)
  })
  
  output$cocitTable <- DT::renderDT({
    COCITnetwork()
    cocitData=values$cocitnet$cluster_res
    names(cocitData)=c("Node", "Cluster", "Betweenness", "Closeness", "PageRank")
    DT::datatable(cocitData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  })
  
  #### save coc network image as html ----
  output$networkCocit.fig <- downloadHandler(
    filename = "network.html",
    content <- function(con) {
      savenetwork(con, values)
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
    
    # fx <- list(
    #   family = "Old Standard TT, serif",
    #   size = 11,
    #   color = "black"
    # )
    # 
    # a <- list(
    #   ticks = "outside",
    #   autotick = FALSE,
    #   ticktext = values$histPlot$axis$label, 
    #   tickvals = values$histPlot$axis$values,
    #   tickmode = "array",
    #   showticklabels = TRUE,
    #   tickangle = 270,
    #   tickfont = fx,
    #   ticklen = 2,
    #   tickwidth = 2,
    #   tickcolor = toRGB("black")
    # )
    # 
    # g <- plot.ly(values$histPlot$g, side="r", size=0.05, aspectratio = 1.5, height=-0.1) %>% 
    #   layout(xaxis = a, autosize=TRUE ,showlegend = FALSE, 
    #          hoverlabel = list(font=list(size=input$histlabelsize+9)))
    # return(g)
  })

  
  # output$HGplot.save <- downloadHandler(
  #   filename = function() {
  #     paste("Historiograph-", Sys.Date(), ".png", sep="")
  #   },
  #   content <- function(file) {
  #     ggsave(filename = file, plot = values$histPlot$g, dpi = as.numeric(input$HGdpi),  height = input$HGh, width = input$HGh*2, bg="white")
  #   },
  #   contentType = "png"
  # )
  
  ### screenshot Button Historiograph
  observeEvent(input$HGplot.save, {
    file <- paste("Historiograph-", Sys.Date(), ".png", sep="")
    screenshot(selector="#histPlotVis", scale=input$HGh, filename=file)
  })
  
  # output$histPlot <- renderPlotly({
  #   Hist()
  # })
  
  output$histPlotVis <- renderVisNetwork({  
    g <- Hist()
    values$histPlotVis<-hist2vis(values$histPlot,curved=FALSE, 
                               labelsize=input$histlabelsize, 
                               nodesize=input$histsize,
                               opacity=0.7,
                               shape="dot",
                               labeltype=input$titlelabel,
                               timeline=FALSE)
    values$histPlotVis$VIS
  })
  
  output$histTable <- DT::renderDT({

    Data <- values$histResults$histData
    #Data <- Data[ind,]
    Data$DOI<- paste0('<a href=\"https://doi.org/',Data$DOI,'\" target=\"_blank\">',Data$DOI,'</a>')
    Data <- Data %>% 
      left_join(
        values$histPlot$layout %>% 
          select(.data$name,.data$color), by= c("Paper" = "name")
        ) %>% 
      drop_na(.data$color) %>% 
      mutate(cluster = match(.data$color,unique(.data$color))) %>% 
      select(!.data$color) %>% 
      group_by(.data$cluster) %>% 
      arrange(.data$Year, .by_group = TRUE)
    
    DT::datatable(Data, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  })
  
  # SOCIAL STRUCTURE ####
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
    },
    contentType = "net"
  )
  
  observeEvent(input$colPlot.save, {
    file <- paste("Collaboration_Network-", Sys.Date(), ".png", sep="")
    screenshot(selector="#colPlot", scale=input$colRes, filename=file)
  })
  
  output$colTable <- DT::renderDT({
    COLnetwork()
    colData=values$colnet$cluster_res
    names(colData)=c("Node", "Cluster", "Betweenness", "Closeness", "PageRank")
    
    DT::datatable(colData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"), filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  })   
  
  #### save coc network image as html ####
  output$networkCol.fig <- downloadHandler(
    filename = "network.html",
    content <- function(con) {
      savenetwork(con, values)
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
    values$WMmap=countrycollaboration(values$M,label=FALSE,edgesize=input$WMedgesize/2,min.edges=input$WMedges.min, values)
  })
  
  output$CCplot.save <- downloadHandler(
    filename = function() {
      paste("CountryCollaborationMap-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      g <- values$WMmap$g + labs(title = "Country Collaboration Map")
      ggsave(filename = file, plot = g, dpi = as.numeric(input$CCdpi),  height = input$CCh, width = input$CCh*2, bg="white")
    },
    contentType = "png"
  )
  
  output$WMPlot<- renderPlot({
    WMnetwork()  
    plot(values$WMmap$g)
  },
  width = exprToFunction(as.numeric(input$dimension[1])*0.6), 
  height = exprToFunction(as.numeric(input$dimension[2])*0.85),
  res = 150)
  
  output$WMTable <- DT::renderDT({
    WMnetwork()  
    colData=values$WMmap$tab
    colData=colData[,c(1,2,9)]
    names(colData)=c("From","To","Frequency")
    
    DT::datatable(colData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"), filter = 'top',
                  options = list(pageLength = 10, dom = 'Bfrtip',
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
  }) 
  
  # OPTIONS MENU ----
  observe({
    if (!(input$sidebarmenu %in% c("biblioshinyy","mainInfo")) & !isTRUE(values$checkControlBar)){
      updateControlbar("controlbar2")
      values$checkControlBar <- TRUE
    }
    if ((input$sidebarmenu %in% c("biblioshinyy","mainInfo")) & isTRUE(values$checkControlBar)){
      updateControlbar("controlbar2")
      values$checkControlBar <- FALSE
    }
  })
  
  output$controlbar <- renderUI({
    controlbarMenu(
      controlbarItem(
        h2(strong("Options"),align="center"),
        fluidPage(
          fluidRow(
            column(width = 1),
            column(width=11,
                   ### Load Data ----
                   conditionalPanel(condition = 'input.sidebarmenu == "loadData"',
                                    h3(strong("Import or Load ")),
                                    selectInput(
                                      "load",
                                      label = "Please, choose what to do",
                                      choices = c(
                                        " " = "null",
                                        "Import raw file(s)" = "import",
                                        "Load bibliometrix file(s)" = "load",
                                        "Use a sample collection" = "demo"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(
                                      condition = "input.load == 'demo'",
                                      helpText(h4(strong("The use of bibliometric approaches in business and management disciplines.")),
                                               h5(strong("Dataset 'Management'")),
                                               em("A collection of scientific articles about the use of bibliometric approaches",
                                                  "in business and management disciplines."),
                                               br(),
                                               em("Period: 1985 - 2020
                                                    , Source WoS.")
                                      )
                                    ),
                                    conditionalPanel(
                                      condition = "input.load == 'import'",
                                      selectInput(
                                        "dbsource",
                                        label = "Database",
                                        choices = c(
                                          "Web of Science (WoS/WoK)" = "isi",
                                          "Scopus" = "scopus",
                                          "Dimensions" = "dimensions",
                                          "Lens.org" = "lens",
                                          "PubMed" = "pubmed",
                                          "Cochrane Library" = "cochrane"
                                        ),
                                        selected = "isi"
                                      )
                                    ),
                                    conditionalPanel(
                                      condition = "input.load != 'null' & input.load != 'demo'",
                                      conditionalPanel(
                                        condition = "input.load == 'load'",
                                        helpText(em("Load a collection in XLSX or R format previously exported from bibliometrix")
                                        )),
                                      fileInput(
                                        "file1",
                                        "Choose a file",
                                        multiple = FALSE,
                                        accept = c(
                                          ".csv",
                                          ".txt",
                                          ".ciw",
                                          ".bib",
                                          ".xlsx",
                                          ".zip",
                                          ".xls",
                                          ".rdata",
                                          ".rda",
                                          ".rds"
                                        )
                                      )
                                    ),
                                    conditionalPanel(condition = "input.load != 'null'",
                                                     actionButton("applyLoad", strong("START"),
                                                                  style ="border-radius: 10px; border-width: 3px; font-size: 20px;",
                                                                  width = "100%"),
                                                     width = "100%"),
                                    tags$hr(),
                                    uiOutput("textLog"),
                                    tags$hr(),
                                    h3(strong(
                                      "Export collection"
                                    )),
                                    selectInput(
                                      'save_file',
                                      'Save as:',
                                      choices = c(
                                        ' ' = 'null',
                                        'Excel' = 'xlsx',
                                        'R Data Format' = 'RData'
                                      ),
                                      selected = 'null'
                                    ),
                                    conditionalPanel(condition = "input.save_file != 'null'",
                                                     downloadButton("collection.save", strong("Export"),
                                                                    style ="border-radius: 10px; border-width: 3px; font-size: 20px;",
                                                                    width = "100%")
                                    )
                   ),
                   ## Gathering Data ----
                   conditionalPanel(condition = 'input.sidebarmenu == "gathData"',
                                    h3(strong(
                                      "Gather data using APIs "
                                    )),
                                    br(),
                                    selectInput(
                                      "dbapi",
                                      label = "Database",
                                      choices = c("DS Dimensions" = "ds",
                                                  "PubMed" = "pubmed"),
                                      selected = "pubmed"
                                    ),
                                    ## Dimenions API 
                                    conditionalPanel(
                                      condition = "input.dbapi == 'ds'",
                                      br(),
                                      actionButton("dsShow",  h5(strong("1.  Configure API request")),
                                                   style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                   width = "100%"),
                                      h5(tags$b("Your Query")),
                                      verbatimTextOutput("queryLog2", placeholder = FALSE),
                                      h5(tags$b("Documents returned using your query")),
                                      verbatimTextOutput("sampleLog2", placeholder = FALSE),
                                    ),
                                    ### Pubmed API 
                                    conditionalPanel(
                                      condition = "input.dbapi == 'pubmed'",
                                      br(),
                                      actionButton("pmShow", h5(strong("1.  Configure API request")),
                                                   style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                   width = "100%"),
                                      h5(tags$b("Your Query")),
                                      verbatimTextOutput("pmQueryLog2", placeholder = FALSE),
                                      h5(tags$b("Documents returned using your query")),
                                      verbatimTextOutput("pmSampleLog2", placeholder = FALSE),
                                    ),
                                    tags$hr(),
                                    actionButton("apiApply", h5(strong("2.  Download metadata")),
                                                 style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                 width = "100%"),
                                    tags$hr(),
                                    h3(strong(
                                      "Export a bibliometrix file "
                                    )),
                                    br(),
                                    selectInput(
                                      'save_file_api',
                                      'Save as:',
                                      choices = c(
                                        ' ' = 'null',
                                        'Excel' = 'xlsx',
                                        'R Data Format' = 'RData'
                                      ),
                                      selected = 'null'
                                    ),
                                    conditionalPanel(condition = "input.save_file_api != 'null'",
                                                     downloadButton("collection.save_api", strong("Export"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")
                                    )
                   ),
                   ## Filters ----
                   conditionalPanel(condition = 'input.sidebarmenu == "filters"',
                                    h3(strong("Filters")),
                                    br(),
                                    actionButton("applyFilter", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 20px;",
                                                 width = "100%",
                                                 icon = fa_i(name ="play")),
                                    h5(" "),
                                    box(h6(htmlOutput("textDim")),
                                        width = "100%"),
                                    br(),
                                    uiOutput("selectLA"),
                                    uiOutput("sliderPY"),
                                    uiOutput("selectType"),
                                    uiOutput("sliderTCpY"),
                                    selectInput("bradfordSources", 
                                                label = "Source by Bradford Law Zones",
                                                choices = c("Core Sources"="core", 
                                                            "Core + Zone 2 Sources"="zone2",
                                                            "All Sources"="all"),
                                                selected = "all")
                   ),
                   ## Annual Scientific Prod ----
                   conditionalPanel(condition = 'input.sidebarmenu == "annualScPr"',
                                    br(),
                                    h4(strong("Annual Growth Rate")),
                                    br(),
                                    verbatimTextOutput("CAGR", placeholder = TRUE)
                   ),
                   br(),
                   br(),
                   conditionalPanel(condition = 'input.sidebarmenu == "annualScPr"',
                                    selectInput(
                                      'ASPdpi',
                                      label = h4(strong("Export plot")),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    )
                   ),
                   br(),
                   br(),
                   conditionalPanel(condition = 'input.sidebarmenu == "annualScPr" & input.ASPdpi != "null"',
                                    sliderInput(
                                      'ASPh',
                                      label =h4(em(strong("Height (in inches)"))),
                                      value = 7, min = 1, max = 20, step = 1),
                                    downloadButton("ASPplot.save", strong("Export plot as png"),
                                                   style ="border-radius: 10px; border-width: 3px; vertical-align: 'middle';font-size: 20px;",
                                                   width = "100%")
                   ),
                   ## Average Cit Per Year ----
                   conditionalPanel(condition = 'input.sidebarmenu == "averageCitPerYear"',
                                    selectInput(
                                      'ACpYdpi',
                                      h4(strong("Export plot"), align ="center"),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.ACpYdpi != 'null'",
                                                     sliderInput(
                                                       'ACpYh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("ACpYplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px; font-size: 20px;",
                                                                    width = "100%")
                                    )
                   ),
                   ## Three field Plot ----
                   conditionalPanel(condition = 'input.sidebarmenu == "threeFieldPlot"',
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, 
                                        fluidRow(
                                          (column(6, selectInput("CentralField",
                                                                 label = "Middle Field",
                                                                 choices = c("Authors" = "AU",
                                                                             "Affiliations" = "AU_UN",
                                                                             "Countries"="AU_CO",
                                                                             "Keywords" = "DE",
                                                                             "Keywords Plus" = "ID",
                                                                             "Titles" = "TI_TM",
                                                                             "Abstract" = "AB_TM",
                                                                             "Sources" = "SO",
                                                                             "References" = "CR",
                                                                             "Cited Sources" = "CR_SO"),
                                                                 selected = "AU"))),
                                          (column(6,numericInput("CentralFieldn", 
                                                                 label=("Number of items"), 
                                                                 min = 1, max = 50, step = 1, value = 20)))),
                                        fluidRow(
                                          (column(6,selectInput("LeftField",
                                                                label = "Left Field",
                                                                choices = c("Authors" = "AU",
                                                                            "Affiliations" = "AU_UN",
                                                                            "Countries"="AU_CO",
                                                                            "Keywords" = "DE",
                                                                            "Keywords Plus" = "ID",
                                                                            "Titles" = "TI_TM",
                                                                            "Abstract" = "AB_TM",
                                                                            "Sources" = "SO",
                                                                            "References" = "CR",
                                                                            "Cited Sources" = "CR_SO"),
                                                                selected = "CR"))),
                                          (column(6, numericInput("LeftFieldn", 
                                                                  label=("Number of items"), 
                                                                  min = 1, max = 50, step = 1, value = 20)))),
                                        fluidRow(
                                          (column(6,selectInput("RightField",
                                                                label = "Right Field",
                                                                choices = c("Authors" = "AU",
                                                                            "Affiliations" = "AU_UN",
                                                                            "Countries"="AU_CO",
                                                                            "Keywords" = "DE",
                                                                            "Keywords Plus" = "ID",
                                                                            "Titles" = "TI_TM",
                                                                            "Abstract" = "AB_TM",
                                                                            "Sources" = "SO",
                                                                            "References" = "CR",
                                                                            "Cited Sources" = "CR_SO"),
                                                                selected = "DE"))),
                                          (column(6,numericInput("RightFieldn", 
                                                                 label=("Number of items"), 
                                                                 min = 1, max = 50, step = 1, value = 20))))
                                    )),
                   ## Relevant Sources ----
                   conditionalPanel(condition = 'input.sidebarmenu == "relevantSources"',
                                    h4(strong("Parameters: ")),
                                    "  ",
                                    numericInput("MostRelSourcesK", 
                                                 label=("Number of Sources"), 
                                                 value = 10),
                                    selectInput(
                                      'MRSdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.MRSdpi != 'null'",
                                                     sliderInput(
                                                       'MRSh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("MRSplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px; font-size: 20px;",
                                                                    width = "100%")
                                    )
                   ),
                   ## Most Local Cited Sources ----
                   conditionalPanel(condition ='input.sidebarmenu == "localCitedSources"',
                                    h4(strong("Parameters: ")),
                                    "  ",
                                    numericInput("MostRelCitSourcesK", 
                                                 label=("Number of Sources"), 
                                                 value = 10),
                                    selectInput(
                                      'MLCSdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.MLCSdpi != 'null'",
                                                     sliderInput(
                                                       'MLCSh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("MLCSplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px; font-size: 20px;",
                                                                    width = "100%")
                                    )
                   ),
                   ## Bradford Law ----
                   conditionalPanel(condition ='input.sidebarmenu == "bradford"',
                                    selectInput(
                                      'BLdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.BLdpi != 'null'",
                                                     sliderInput(
                                                       'BLh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("BLplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")
                                    )),
                   ## Source Impact ----
                   conditionalPanel(condition ='input.sidebarmenu == "sourceImpact"',
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, 
                                        collapsed = FALSE,
                                        selectInput("HmeasureSources", 
                                                    label = "Impact measure",
                                                    choices = c("H-Index"="h", 
                                                                "G-Index"="g",
                                                                "M-Index"="m",
                                                                "Total Citation"="tc"),
                                                    selected = "h"),
                                        "  ",
                                        numericInput("Hksource", 
                                                     label=("Number of sources"), 
                                                     value = 10)),
                                    selectInput(
                                      'SIdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.SIdpi != 'null'",
                                                     sliderInput(
                                                       'SIh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("SIplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")
                                    )
                   ),
                   ## Source Dynamics ----
                   conditionalPanel(condition ='input.sidebarmenu == "sourceDynamics"',
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, 
                                        collapsed = FALSE,
                                        selectInput("cumSO", "Occurrences",
                                                    choices = c("Cumulate" = "Cum",
                                                                "Per year" = "noCum"),
                                                    selected = "Cum"),
                                        hr(),
                                        sliderInput("topSO", label = "Number of Sources", min = 1, max = 50, step = 1, value = c(1,5))),
                                    selectInput(
                                      'SDdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.SDdpi != 'null'",
                                                     sliderInput(
                                                       'SDh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("SDplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")
                                    )),
                   ## Most relevant Authors ----
                   conditionalPanel(condition = 'input.sidebarmenu == "mostRelAuthors"',
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, 
                                        collapsed = FALSE,
                                        numericInput("MostRelAuthorsK", 
                                                     label=("Number of Authors"), 
                                                     value = 10),
                                        "  ",
                                        selectInput("AuFreqMeasure", 
                                                    label = "Frequency measure",
                                                    choices = c("N. of Documents "="t", 
                                                                "Percentage"="p",
                                                                "Fractionalized Frequency"="f"),
                                                    selected = "t")),
                                    selectInput(
                                      'MRAdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.MRAdpi != 'null'",
                                                     sliderInput(
                                                       'MRAh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("MRAplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")
                                    )
                   ),
                   ## Most Local Cited Authors ----
                   conditionalPanel(condition = 'input.sidebarmenu == "mostLocalCitedAuthors"',
                                    h4(strong("Parameters: ")),
                                    "  ",
                                    numericInput("MostCitAuthorsK", 
                                                 label=("Number of Authors"), 
                                                 value = 10),
                                    selectInput(
                                      'MLCAdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.MLCAdpi != 'null'",
                                                     sliderInput(
                                                       'MLCAh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("MLCAplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")   
                                    )
                   ),
                   ## Authors production over time ----
                   conditionalPanel(condition = 'input.sidebarmenu == "authorsProdOverTime"',
                                    h4(strong("Parameters: ")),
                                    "  ",
                                    numericInput("TopAuthorsProdK", 
                                                 label=("Number of Authors"), 
                                                 value = 10),
                                    selectInput(
                                      'APOTdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.APOTdpi != 'null'",
                                                     sliderInput(
                                                       'APOTh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("APOTplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")   
                                    )
                   ),
                   ## Lotka law ----
                   conditionalPanel(condition = 'input.sidebarmenu == "lotka"',
                                    selectInput(
                                      'LLdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.LLdpi != 'null'",
                                                     sliderInput(
                                                       'LLh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("LLplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")   
                                    )
                   ),
                   ## Author Impact ----
                   conditionalPanel(condition = 'input.sidebarmenu == "authorImpact"',
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, 
                                        collapsed = FALSE,
                                        selectInput("HmeasureAuthors", 
                                                    label = "Impact measure",
                                                    choices = c("H-Index"="h", 
                                                                "G-Index"="g",
                                                                "M-Index"="m",
                                                                "Total Citation"="tc"),
                                                    selected = "h"),
                                        "  ",
                                        numericInput("Hkauthor", 
                                                     label=("Number of authors"), 
                                                     value = 10)),
                                    selectInput(
                                      'AIdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.AIdpi != 'null'",
                                                     sliderInput(
                                                       'AIh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("AIplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")   
                                    )
                   ),
                   ## Most Relevant Affiliations ----
                   conditionalPanel(condition = 'input.sidebarmenu == "mostRelAffiliations"',
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, 
                                        collapsed = FALSE,
                                        selectInput("disAff", 
                                                    label = "Affiliation Name Disambiguation",
                                                    choices = c("Yes"="Y", 
                                                                "No"="N"),
                                                    selected = "Y"),
                                        "  ",
                                        numericInput("MostRelAffiliationsK", 
                                                     label=("Number of Affiliations"), 
                                                     value = 10)),
                                    selectInput(
                                      'AFFdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.AFFdpi != 'null'",
                                                     sliderInput(
                                                       'AFFh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("AFFplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")  
                                    )
                   ),
                   ## Affiliations' Production over Time ----
                   conditionalPanel(condition ='input.sidebarmenu == "AffOverTime"',
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, 
                                        collapsed = FALSE,
                                        sliderInput("topAFF", label = "Number of Affiliations", min = 1, max = 50, step = 1, value = 5)),
                                    selectInput(
                                      'AFFGrowthdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.AFFGrowthdpi != 'null'",
                                                     sliderInput(
                                                       'AFFGrowthh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("AffOverTimeplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")
                                    )),
                   
                   ## Corresponding Author country ----
                   conditionalPanel(condition = 'input.sidebarmenu == "correspAuthorCountry"',
                                    h4(strong("Parameters: ")),
                                    numericInput("MostRelCountriesK", 
                                                 label=("Number of Countries"), 
                                                 value = 20),
                                    selectInput(
                                      'MRCOdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.MRCOdpi != 'null'",
                                                     sliderInput(
                                                       'MRCOh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("MRCOplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")  
                                    )
                   ),
                   ## Country Scientific Production ----
                   conditionalPanel(condition = 'input.sidebarmenu == "countryScientProd"',
                                    selectInput(
                                      'CSPdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.CSPdpi != 'null'",
                                                     sliderInput(
                                                       'CSPh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("CSPplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")  
                                    )
                   ),
                   ## Countries' Production over Time ----
                   conditionalPanel(condition ='input.sidebarmenu == "COOverTime"',
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, 
                                        collapsed = FALSE,
                                        sliderInput("topCO", label = "Number of Countries", min = 1, max = 50, step = 1, value = 5)),
                                    selectInput(
                                      'COGrowthdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.COGrowthdpi != 'null'",
                                                     sliderInput(
                                                       'COGrowthh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("CountryOverTimeplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")
                                    )
                   ),
                   ## Most Cited Countries ----
                   conditionalPanel(condition = 'input.sidebarmenu == "mostCitedCountries"',
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, 
                                        collapsed = FALSE,
                                        selectInput("CitCountriesMeasure", 
                                                    label = "Measure",
                                                    choices = c("Total Citations"="TC", 
                                                                "Average Citations per Year"="TCY"),
                                                    selected = "TC"),
                                        "  ",
                                        numericInput("MostCitCountriesK", 
                                                     label=("Number of Countries"), 
                                                     value = 10)),
                                    selectInput(
                                      'MCCdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.MCCdpi != 'null'",
                                                     sliderInput(
                                                       'MCCh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("MCCplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")  
                                    )
                   ),
                   ## Most Global Cited Documents
                   conditionalPanel(condition = 'input.sidebarmenu == "mostGlobalCitDoc"',
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, 
                                        collapsed = FALSE,
                                        numericInput("MostCitDocsK", 
                                                     label=("Number of Documents"), 
                                                     value = 10),
                                        "  ",
                                        selectInput("CitDocsMeasure", 
                                                    label = "Measure",
                                                    choices = c("Total Citations"="TC", 
                                                                "Total Citations per Year"="TCY"),
                                                    selected = "TC")),
                                    selectInput(
                                      'MGCDdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.MGCDdpi != 'null'",
                                                     sliderInput(
                                                       'MGCDh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("MGCDplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")  
                                    )
                   ),
                   # Most Local Cited Document
                   conditionalPanel(condition = 'input.sidebarmenu == "mostLocalCitDoc"',
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, 
                                        collapsed = FALSE,
                                        numericInput("MostLocCitDocsK", 
                                                     label=("Number of Documents"), 
                                                     value = 10),
                                        "  ",
                                        selectInput(inputId = "LocCitSep", 
                                                    label = "Field separator character", 
                                                    choices = c(";" = ";", 
                                                                ".  " = ".  ",
                                                                "," = ","),
                                                    selected = ";")),
                                    selectInput(
                                      'MLCDdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.MLCDdpi != 'null'",
                                                     sliderInput(
                                                       'MLCDh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("MLCDplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")  
                                    )
                   ),
                   ## Most Local Cited References ----
                   conditionalPanel(condition = 'input.sidebarmenu == "mostLocalCitRef"',
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, 
                                        collapsed = FALSE,
                                        numericInput("MostCitRefsK", 
                                                     label=("Number of Documents"), 
                                                     value = 10),
                                        "  ",
                                        selectInput(inputId = "CitRefsSep", 
                                                    label = "Field separator character", 
                                                    choices = c(";" = ";", 
                                                                ".  " = ".  ",
                                                                "," = ","),
                                                    selected = ";")),
                                    selectInput(
                                      'MLCRdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.MLCRdpi != 'null'",
                                                     sliderInput(
                                                       'MLCRh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("MLCRplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")  
                                    )
                   ),
                   ## References spectroscopy
                   conditionalPanel(condition = 'input.sidebarmenu == "ReferenceSpect"',
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, 
                                        collapsed = FALSE,
                                        selectInput(inputId = "rpysSep", 
                                                    label = "Field separator character", 
                                                    choices = c(";" = ";", 
                                                                ".  " = ".  ",
                                                                "," = ","),
                                                    selected = ";"),
                                        h4(em(strong("Time slice"))),
                                        fluidRow(column(6,
                                                        numericInput(inputId = "rpysMinYear",
                                                                     label = "Starting Year",
                                                                     value = NA,
                                                                     step = 1)),
                                                 column(6,
                                                        numericInput(inputId = "rpysMaxYear",
                                                                     label = "End Year",
                                                                     value = NA,
                                                                     step = 1)
                                                 ))),
                                    selectInput(
                                      'RSdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.RSdpi != 'null'",
                                                     sliderInput(
                                                       'RSh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("RSplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")  
                                                     
                                    )
                   ),
                   ## Most Frequent Words ----
                   conditionalPanel(condition = 'input.sidebarmenu == "mostFreqWords"',
                                    selectInput("MostRelWords", "Field",
                                                choices = c("Keywords Plus" = "ID",
                                                            "Author's keywords" = "DE",
                                                            "Titles" = "TI",
                                                            "Abstracts" = "AB",
                                                            "Subject Categories (WoS)" = "WC"),
                                                selected = "ID"),
                                    conditionalPanel(condition = "input.MostRelWords == 'AB' |input.MostRelWords == 'TI'",
                                                     selectInput("MRWngrams",'N-Grams',
                                                                 choices = c("Unigrams" = "1",
                                                                             "Bigrams" = "2",
                                                                             "Trigrams" = "3"),
                                                                 selected = 1)),
                                    numericInput("MostRelWordsN", label = "Number of words", min = 2, max = 100, step = 1, value = 10),
                                    br(),
                                    box(title = p(strong("Text Editing"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        selectInput("MostRelWordsStopFile", "Load a list of terms to remove",
                                                    choices = c("Yes" = "Y",
                                                                "No" = "N"),
                                                    selected = "N"),
                                        conditionalPanel(condition = "input.MostRelWordsStopFile == 'Y'",
                                                         helpText(h5(strong("Upload a TXT or CSV file containing a list of terms you want to remove from the analysis.")),
                                                                  h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator)."))
                                                         ),
                                                         fileInput("MostRelWordsStop", "",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv",
                                                                              ".txt")),
                                                         
                                                         selectInput("MostRelWordsSep", "File Separator",
                                                                     choices = c('Comma ","' = ",",
                                                                                 'Semicolon ";"' = ";",
                                                                                 'Tab '= "\t"),
                                                                     selected = ","),
                                                         h5(htmlOutput("MostRelWordsStopPreview"))
                                        ),
                                        selectInput("MRWSynFile", "Load a list of synonyms",
                                                    choices = c("Yes" = "Y",
                                                                "No" = "N"),
                                                    selected = "N"),
                                        conditionalPanel(condition = "input.MRWSynFile == 'Y'",
                                                         helpText(h5(strong("Upload a TXT or CSV file containing, in each row, a list of synonyms, that will be merged into a single term (the first word contained in the row)")),
                                                                  h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator). 
                              Rows have to be separated by return separator."))
                                                         ),
                                                         fileInput("MRWSyn", "",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv",
                                                                              ".txt")),
                                                         
                                                         selectInput("MRWSynSep", "File Separator",
                                                                     choices = c('Comma ","' = ",",
                                                                                 'Semicolon ";"' = ";",
                                                                                 'Tab '= "\t"),
                                                                     selected = ","),
                                                         h5(htmlOutput("MRWSynPreview"))
                                        )),
                                    selectInput(
                                      'MRWdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.MRWdpi != 'null'",
                                                     sliderInput(
                                                       'MRWh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("MRWplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%"))
                   ),
                   ## Wordcloud ----
                   conditionalPanel(condition = 'input.sidebarmenu == "wcloud"',
                                    h4(em(strong(" "))),
                                    " ",
                                    selectInput("summaryTerms", "Field",
                                                choices = c("Keywords Plus" = "ID",
                                                            "Author's keywords" = "DE",
                                                            "Titles" = "TI",
                                                            "Abstracts" = "AB",
                                                            "Subject Categories (WoS)" = "WC"),
                                                selected = "ID"),
                                    conditionalPanel(condition = "input.summaryTerms == 'AB' |input.summaryTerms == 'TI'",
                                                     selectInput("summaryTermsngrams",'N-Grams',
                                                                 choices = c("Unigrams" = "1",
                                                                             "Bigrams" = "2",
                                                                             "Trigrams" = "3"),
                                                                 selected = 1)),
                                    numericInput("n_words", label = "Number of words", min = 10, max = 500, step = 1, value = 50),
                                    br(),
                                    box(title = p(strong("Text Editing"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        selectInput("WCStopFile", "Load a list of terms to remove",
                                                    choices = c("Yes" = "Y",
                                                                "No" = "N"),
                                                    selected = "N"),
                                        conditionalPanel(condition = "input.WCStopFile == 'Y'",
                                                         helpText(h5(strong("Upload a TXT or CSV file containing a list of terms you want to remove from the analysis.")),
                                                                  h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator)."))
                                                         ),
                                                         fileInput("WCStop", "",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv",
                                                                              ".txt")),
                                                         selectInput("WCSep", "File Separator",
                                                                     choices = c('Comma ","' = ",",
                                                                                 'Semicolon ";"' = ";",
                                                                                 'Tab '= "\t"),
                                                                     selected = ",")
                                        ),
                                        selectInput("WCSynFile", "Load a list of synonyms",
                                                    choices = c("Yes" = "Y",
                                                                "No" = "N"),
                                                    selected = "N"),
                                        conditionalPanel(condition = "input.WCSynFile == 'Y'",
                                                         helpText(h5(strong("Upload a TXT or CSV file containing, in each row, a list of synonyms, that will be merged into a single term (the first word contained in the row)")),
                                                                  h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator). 
                              Rows have to be separated by return separator."))
                                                         ),
                                                         fileInput("WCSyn", "",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv",
                                                                              ".txt")),
                                                         
                                                         selectInput("WCSynSep", "File Separator",
                                                                     choices = c('Comma ","' = ",",
                                                                                 'Semicolon ";"' = ";",
                                                                                 'Tab '= "\t"),
                                                                     selected = ",")
                                        )),
                                    br(),
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        fluidRow(column(6,
                                                        selectInput("measure", "Word occurrence by",
                                                                    choices = c("Frequency" = "freq",
                                                                                "Square root" = "sqrt",
                                                                                "Log" = "log",
                                                                                "Log10" = "log10"),
                                                                    selected = "freq")
                                        ),
                                        column(6,
                                               selectInput("wcShape", "Shape",
                                                           choices = c("Circle" = "circle",
                                                                       "Cardiod" = "cardioid",
                                                                       "Diamond" = "diamond",
                                                                       "Pentagon" = "pentagon",
                                                                       "Star" = "star",
                                                                       "Triangle-forward" = "triangle-forward"
                                                                       ,"Triangle" = "triangle"),
                                                           selected = "circle")
                                        )),
                                        fluidRow(column(6,
                                                        selectInput("font", label = "Font type",
                                                                    choices = c("Impact", "Comic Sans MS (No plz!)" = "Comic Sans MS",
                                                                                "Arial", "Arial Black", "Tahoma", "Verdana", "Courier New",
                                                                                "Georgia", "Times New Roman", "Andale Mono"))
                                        ),
                                        column(6,
                                               selectInput("wcCol", "Text colors",
                                                           choices = c("Random Dark" = "random-dark",
                                                                       "Random Light" = "random-light"),
                                                           selected = "random-dark")
                                        )),
                                        fluidRow(column(6,
                                                        numericInput("scale", label = "Font size", min=0.2,max=5,step=0.1,value=1)
                                        ),
                                        column(6,
                                               numericInput("ellipticity", label = "Ellipticity", min=0,max=1,step=0.05,value=0.65)
                                        )),
                                        fluidRow(column(6,
                                                        numericInput("padding", label = "Padding", min = 0, max = 5, value = 1, step = 1)
                                        ),
                                        column(6,
                                               numericInput("rotate", label = "Rotate", min = 0, max = 20, value = 0, step = 1)
                                        ))
                                    )
                   ),
                   ## Tree Map ----
                   conditionalPanel(condition = 'input.sidebarmenu == "treemap"',
                                    selectInput("treeTerms", "Field",
                                                choices = c("Keywords Plus" = "ID",
                                                            "Author's keywords" = "DE",
                                                            "Titles" = "TI",
                                                            "Abstracts" = "AB",
                                                            "Subject Categories (WoS)" = "WC"),
                                                selected = "ID"),
                                    conditionalPanel(condition = "input.treeTerms == 'AB' |input.treeTerms == 'TI'",
                                                     selectInput("treeTermsngrams",'N-Grams',
                                                                 choices = c("Unigrams" = "1",
                                                                             "Bigrams" = "2",
                                                                             "Trigrams" = "3"),
                                                                 selected = 1)),
                                    numericInput("treen_words", label = "Number of words", min = 10, max = 200, step = 5, value = 50),
                                    br(),
                                    box(title = p(strong("Text Editing"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        selectInput("TreeMapStopFile", "Load a list of terms to remove",
                                                    choices = c("Yes" = "Y",
                                                                "No" = "N"),
                                                    selected = "N"),
                                        conditionalPanel(condition = "input.TreeMapStopFile == 'Y'",
                                                         helpText(h5(strong("Upload a TXT or CSV file containing a list of terms you want to remove from the analysis.")),
                                                                  h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator)."))
                                                         ),
                                                         fileInput("TreeMapStop", "",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv",
                                                                              ".txt")),
                                                         
                                                         selectInput("TreeMapSep", "File Separator",
                                                                     choices = c('Comma ","' = ",",
                                                                                 'Semicolon ";"' = ";",
                                                                                 'Tab '= "\t"),
                                                                     selected = ","),
                                                         h5(htmlOutput("TreeMapStopPreview"))
                                        ),
                                        selectInput("TreeMapSynFile", "Load a list of synonyms",
                                                    choices = c("Yes" = "Y",
                                                                "No" = "N"),
                                                    selected = "N"),
                                        conditionalPanel(condition = "input.TreeMapSynFile == 'Y'",
                                                         helpText(h5(strong("Upload a TXT or CSV file containing, in each row, a list of synonyms, that will be merged into a single term (the first word contained in the row)")),
                                                                  h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator). 
                              Rows have to be separated by return separator."))
                                                         ),
                                                         fileInput("TreeMapSyn", "",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv",
                                                                              ".txt")),
                                                         
                                                         selectInput("TreeMapSynSep", "File Separator",
                                                                     choices = c('Comma ","' = ",",
                                                                                 'Semicolon ";"' = ";",
                                                                                 'Tab '= "\t"),
                                                                     selected = ","),
                                                         h5(htmlOutput("TreeMapSynPreview"))
                                        )
                                    )
                   ),
                   ## Word dynamics ----
                   conditionalPanel(condition = 'input.sidebarmenu == "wordDynamics"',
                                    selectInput("growthTerms", "Field",
                                                choices = c("Keywords Plus" = "ID",
                                                            "Author's keywords" = "DE",
                                                            "Titles" = "TI",
                                                            "Abstracts" = "AB"),
                                                selected = "ID"),
                                    conditionalPanel(condition = "input.growthTerms == 'AB' |input.growthTerms == 'TI'",
                                                     selectInput("growthTermsngrams",'N-Grams',
                                                                 choices = c("Unigrams" = "1",
                                                                             "Bigrams" = "2",
                                                                             "Trigrams" = "3"),
                                                                 selected = 1)),
                                    br(),
                                    box(title = p(strong("Text Editing"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        selectInput("WDStopFile", "Load a list of terms to remove",
                                                    choices = c("Yes" = "Y",
                                                                "No" = "N"),
                                                    selected = "N"),
                                        conditionalPanel(condition = "input.WDStopFile == 'Y'",
                                                         helpText(h5(strong("Upload a TXT or CSV file containing a list of terms you want to remove from the analysis.")),
                                                                  h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator)."))
                                                         ),
                                                         fileInput("WDStop", "",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv",
                                                                              ".txt")),
                                                         
                                                         selectInput("WDSep", "File Separator",
                                                                     choices = c('Comma ","' = ",",
                                                                                 'Semicolon ";"' = ";",
                                                                                 'Tab '= "\t"),
                                                                     selected = ","),
                                                         h5(htmlOutput("WDStopPreview"))
                                        ),
                                        selectInput("WDSynFile", "Load a list of synonyms",
                                                    choices = c("Yes" = "Y",
                                                                "No" = "N"),
                                                    selected = "N"),
                                        conditionalPanel(condition = "input.WDSynFile == 'Y'",
                                                         helpText(h5(strong("Upload a TXT or CSV file containing, in each row, a list of synonyms, that will be merged into a single term (the first word contained in the row)")),
                                                                  h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator). 
                              Rows have to be separated by return separator."))
                                                         ),
                                                         fileInput("WDSyn", "",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv",
                                                                              ".txt")),
                                                         
                                                         selectInput("WDSynSep", "File Separator",
                                                                     choices = c('Comma ","' = ",",
                                                                                 'Semicolon ";"' = ";",
                                                                                 'Tab '= "\t"),
                                                                     selected = ","),
                                                         h5(htmlOutput("WDSynPreview"))
                                        )),
                                    br(),
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        selectInput("cumTerms", "Occurrences",
                                                    choices = c("Cumulate" = "Cum",
                                                                "Per year" = "noCum"),
                                                    selected = "Cum"),
                                        sliderInput("topkw", label = "Number of words", min = 1, max = 100, step = 1, value = c(1,10))),
                                    selectInput(
                                      'WDdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.WDdpi != 'null'",
                                                     sliderInput(
                                                       'WDh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("WDplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")  
                                                     
                                    )
                   ),
                   ## Trend Topic ----
                   conditionalPanel(condition = 'input.sidebarmenu == "trendTopic"',
                                    selectInput("trendTerms", "Field",
                                                choices = c("Keywords Plus" = "ID",
                                                            "Author's keywords" = "DE",
                                                            "Titles" = "TI",
                                                            "Abstracts" = "AB"),
                                                selected = "ID"),
                                    conditionalPanel(condition = "input.trendTerms == 'TI' | input.trendTerms == 'AB'",
                                                     selectInput("trendTermsngrams",'N-Grams',
                                                                 choices = c("Unigrams" = "1",
                                                                             "Bigrams" = "2",
                                                                             "Trigrams" = "3"),
                                                                 selected = 1)),
                                    conditionalPanel(
                                      condition = "input.trendTerms == 'TI' | input.trendTerms == 'AB'",
                                      selectInput("trendStemming", label="Word Stemming",
                                                  choices = c("Yes" = TRUE,
                                                              "No" = FALSE),
                                                  selected = FALSE)),
                                    uiOutput("trendSliderPY"),
                                    br(),
                                    box(title = p(strong("Text Editing"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        selectInput("TTStopFile", "Load a list of terms to remove",
                                                    choices = c("Yes" = "Y",
                                                                "No" = "N"),
                                                    selected = "N"),
                                        conditionalPanel(condition = "input.TTStopFile == 'Y'",
                                                         helpText(h5(strong("Upload a TXT or CSV file containing a list of terms you want to remove from the analysis.")),
                                                                  h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator)."))
                                                         ),
                                                         fileInput("TTStop", "",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv",
                                                                              ".txt")),
                                                         
                                                         selectInput("TTSep", "File Separator",
                                                                     choices = c('Comma ","' = ",",
                                                                                 'Semicolon ";"' = ";",
                                                                                 'Tab '= "\t"),
                                                                     selected = ","),
                                                         h5(htmlOutput("TTStopPreview"))
                                        ),
                                        selectInput("TTSynFile", "Load a list of synonyms",
                                                    choices = c("Yes" = "Y",
                                                                "No" = "N"),
                                                    selected = "N"),
                                        conditionalPanel(condition = "input.TTSynFile == 'Y'",
                                                         helpText(h5(strong("Upload a TXT or CSV file containing, in each row, a list of synonyms, that will be merged into a single term (the first word contained in the row)")),
                                                                  h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator). 
                              Rows have to be separated by return separator."))
                                                         ),
                                                         fileInput("TTSyn", "",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv",
                                                                              ".txt")),
                                                         
                                                         selectInput("TTSynSep", "File Separator",
                                                                     choices = c('Comma ","' = ",",
                                                                                 'Semicolon ";"' = ";",
                                                                                 'Tab '= "\t"),
                                                                     selected = ","),
                                                         h5(htmlOutput("TTSynPreview"))
                                        )),
                                    br(),
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        fluidRow(column(6,
                                                        numericInput("trendMinFreq", label = "Word Minimum Frequency", min = 0, max = 100, value = 5, step = 1),
                                        ),
                                        column(6,
                                               numericInput("trendNItems", label = "Number of Words per Year", min = 1, max = 20, step = 1, value = 3)
                                        ))),
                                    selectInput(
                                      'TTdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.TTdpi != 'null'",
                                                     sliderInput(
                                                       'TTh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("TTplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")
                                    )
                   ),
                   ## Coupling ----
                   conditionalPanel(condition = 'input.sidebarmenu == "coupling"',
                                    h4(em(strong(" "))),
                                    "  ",
                                    selectInput("CManalysis", 
                                                label = "Unit of Analysis",
                                                choices = c("Documents" = "documents", 
                                                            "Authors" = "authors",
                                                            "Sources" = "sources"),
                                                selected = "documents"),
                                    " ",
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, 
                                        collapsed = FALSE,
                                        selectInput("CMfield", 
                                                    label = "Coupling measured by",
                                                    choices = c("References" ="CR",
                                                                "Keywords Plus" = "ID", 
                                                                "Author's Keywords" = "DE",
                                                                "Titles" = "TI",
                                                                "Abstracts" = "AB"),
                                                    selected = "CR"),
                                        conditionalPanel(condition = "input.CMfield == 'TI' | input.CMfield == 'AB'",
                                                         selectInput("CMstemming", label="Word Stemming",
                                                                     choices = c("Yes" = TRUE,
                                                                                 "No" = FALSE),
                                                                     selected = FALSE)),
                                        selectInput("CMimpact", 
                                                    label = "Impact measure",
                                                    choices = c("Local Citation Score" = "local", 
                                                                "Global Citation Score" = "global"),
                                                    selected = "local"),
                                        selectInput("CMlabeling", 
                                                    label = "Cluster labeling by",
                                                    choices = c("None" = "none", 
                                                                "Keyword Plus" = "ID",
                                                                "Authors' keywords" = "DE",
                                                                "Title terms" = "TI",
                                                                "Abstract terms" = "AB"),
                                                    selected = "ID"),
                                        conditionalPanel(condition = "input.CMlabeling == 'TI' | input.CMlabeling == 'AB'",
                                                         selectInput("CMngrams",'N-Grams',
                                                                     choices = c("Unigrams" = "1",
                                                                                 "Bigrams" = "2",
                                                                                 "Trigrams" = "3"),
                                                                     selected = 1)),
                                        fluidRow(column(6,
                                                        numericInput("CMn", label="Number of Units\n ",value=250,min=50,max=5000,step=1)),
                                                 column(6,
                                                        numericInput("CMfreq", label="Min Cluster Freq. ",value=5,min=1,max=100,step=1))),
                                        fluidRow(column(6,
                                                        numericInput("CMn.labels", label="Labels per cluster",value=3,min=1,max=10,step=1)),
                                                 column(6,
                                                        numericInput("sizeCM", label="Label size",value=0.3,min=0.0,max=1,step=0.05))),
                                        fluidRow(column(6,
                                                        numericInput("CMrepulsion", label="Community Repulsion",value=0,min=0,max=1,step=0.01)),
                                                 column(6,
                                                        selectInput("CMcluster", 
                                                                    label = "Clustering Algorithm",
                                                                    choices = c("None" = "none",
                                                                                "Edge Betweenness" = "edge_betweenness",
                                                                                "Fast Greedy" = "fast_greedy",
                                                                                "InfoMap" = "infomap",
                                                                                "Leading Eigenvalues" = "leading_eigen",
                                                                                "Leiden" = "leiden",
                                                                                "Louvain" = "louvain",
                                                                                "Spinglass" = "spinglass",
                                                                                "Walktrap" = "walktrap"),
                                                                    selected = "walktrap")
                                                 ))
                                    ),
                                    selectInput(
                                      'CMdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "Dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.CMdpi != 'null'",
                                                     sliderInput(
                                                       'CMh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("CMplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")
                                    )
                   ),
                   ## Co-Occurence Network ----
                   conditionalPanel(condition = 'input.sidebarmenu == "coOccurenceNetwork"',
                                    selectInput("field", 
                                                "Field",
                                                choices = c("Keywords Plus" = "ID", 
                                                            "Author's Keywords" = "DE",
                                                            "Titles" = "TI",
                                                            "Abstracts" = "AB",
                                                            "Subject Categories (WoS)" = "WC"),
                                                selected = "ID"),
                                    conditionalPanel(condition = "input.field == 'TI' | input.field == 'AB'",
                                                     selectInput("cocngrams",'N-Grams',
                                                                 choices = c("Unigrams" = "1",
                                                                             "Bigrams" = "2",
                                                                             "Trigrams" = "3"),
                                                                 selected = 1)),
                                    br(),
                                    box(title = p(strong("Text Editing"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        selectInput("COCStopFile", "Load a list of terms to remove",
                                                    choices = c("Yes" = "Y",
                                                                "No" = "N"),
                                                    selected = "N"),
                                        conditionalPanel(condition = "input.COCStopFile == 'Y'",
                                                         helpText(h5(strong("Upload a TXT or CSV file containing a list of terms you want to remove from the analysis.")),
                                                                  h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator)."))
                                                         ),
                                                         fileInput("COCStop", "",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv",
                                                                              ".txt")),
                                                         
                                                         selectInput("COCSep", "File Separator",
                                                                     choices = c('Comma ","' = ",",
                                                                                 'Semicolon ";"' = ";",
                                                                                 'Tab '= "\t"),
                                                                     selected = ","),
                                                         h5(htmlOutput("COCStopPreview"))
                                        ),
                                        selectInput("COCSynFile", "Load a list of synonyms",
                                                    choices = c("Yes" = "Y",
                                                                "No" = "N"),
                                                    selected = "N"),
                                        conditionalPanel(condition = "input.COCSynFile == 'Y'",
                                                         helpText(h5(strong("Upload a TXT or CSV file containing, in each row, a list of synonyms, that will be merged into a single term (the first word contained in the row)")),
                                                                  h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator). 
                              Rows have to be separated by return separator."))
                                                         ),
                                                         fileInput("COCSyn", "",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv",
                                                                              ".txt")),
                                                         
                                                         selectInput("COCSynSep", "File Separator",
                                                                     choices = c('Comma ","' = ",",
                                                                                 'Semicolon ";"' = ";",
                                                                                 'Tab '= "\t"),
                                                                     selected = ","),
                                                         h5(htmlOutput("COCSynPreview"))
                                        )),
                                    br(),
                                    box(title = p(strong("Method Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        fluidRow(column(6,
                                                        selectInput("layout", 
                                                                    label = "Network Layout",
                                                                    choices = c("Automatic layout"="auto", 
                                                                                "Circle"="circle",
                                                                                "Fruchterman & Reingold"="fruchterman",
                                                                                "Kamada & Kawai"="kamada",
                                                                                "MultiDimensional Scaling"="mds",
                                                                                "Sphere"="sphere",
                                                                                "Star"="star"),
                                                                    selected = "auto")
                                        ),
                                        column(6,
                                               selectInput("cocCluster", 
                                                           label = "Clustering Algorithm",
                                                           choices = c("None" = "none",
                                                                       "Edge Betweenness" = "edge_betweenness",
                                                                       "Fast Greedy" = "fast_greedy",
                                                                       "InfoMap" = "infomap",
                                                                       "Leading Eigenvalues" = "leading_eigen",
                                                                       "Leiden" = "leiden",
                                                                       "Louvain" = "louvain",
                                                                       "Spinglass" = "spinglass",
                                                                       "Walktrap" = "walktrap"),
                                                           selected = "walktrap")
                                        )),
                                        fluidRow(column(6,
                                                        selectInput("normalize", 
                                                                    label = "Normalization",
                                                                    choices = c("none", 
                                                                                "association",
                                                                                "jaccard", 
                                                                                "salton",
                                                                                "inclusion",
                                                                                "equivalence"),
                                                                    selected = "association")
                                        ),
                                        column(6,
                                               selectInput("cocyears",
                                                           label = "Node Color by Year",
                                                           choices = c("No" = "No",
                                                                       "Yes"= "Yes"),
                                                           selected = "No")
                                        )
                                        ),
                                        fluidRow(column(6,
                                                        numericInput(inputId = "Nodes",
                                                                     label = "Number of Nodes",
                                                                     min = 5,
                                                                     max = 1000,
                                                                     value = 50,
                                                                     step = 1)
                                        ),
                                        column(6,
                                               numericInput(inputId = "coc.repulsion",
                                                            label = "Repulsion Force",
                                                            min = 0,
                                                            max = 1,
                                                            value = 0.1,
                                                            step = 0.1)
                                        )),
                                        fluidRow(column(6,
                                                        selectInput(inputId ="coc.isolates",
                                                                    label = "Remove Isolated Nodes",
                                                                    choices = c("Yes" = "yes",
                                                                                "No" = "no"),
                                                                    selected = "yes")
                                        ),
                                        column(6,
                                               numericInput("edges.min", 
                                                            label=("Minimum Number of Edges"),
                                                            value = 2,
                                                            step = 1,
                                                            min = 0)
                                        )
                                        )),
                                    br(),
                                    box(title = p(strong("Graphical Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        fluidRow(column(6,
                                                        numericInput(inputId = "cocAlpha",
                                                                     label = "Opacity",
                                                                     min = 0,
                                                                     max = 1,
                                                                     value = 0.7,
                                                                     step=0.05)
                                        ),
                                        column(6,
                                               numericInput(inputId = "Labels",
                                                            label = "Number of labels",
                                                            min = 0,
                                                            max = 1000,
                                                            value = 50,
                                                            step = 1)
                                        )),
                                        fluidRow(column(6,
                                                        selectInput(inputId ="label.cex",
                                                                    label = "Label cex",
                                                                    choices = c("Yes", 
                                                                                "No"),
                                                                    selected = "Yes")
                                        ),
                                        column(6,
                                               selectInput(inputId ="coc.shape",
                                                           label = "Node Shape",
                                                           choices = c(
                                                             "Box"="box",
                                                             "Circle"="circle",
                                                             "Dot"="dot",
                                                             "Ellipse"="ellipse",
                                                             "Square"="square",
                                                             "Text"="text"),
                                                           selected = "dot")
                                        )),
                                        fluidRow(column(6,
                                                        numericInput(inputId = "labelsize",
                                                                     label = "Label size",
                                                                     min = 0.0,
                                                                     max = 20,
                                                                     value = 6,
                                                                     step = 0.10)
                                        ),
                                        column(6,
                                               numericInput(
                                                 inputId = "edgesize",
                                                 label = "Edge size",
                                                 min = 0.5,
                                                 max = 20,
                                                 value = 5,
                                                 step=0.5)
                                        )), 
                                        fluidRow(column(6,
                                                        selectInput(inputId ="coc.shadow",
                                                                    label = "Node shadow",
                                                                    choices = c("Yes",
                                                                                "No"),
                                                                    selected = "No")
                                        ),
                                        column(6,
                                               selectInput(inputId ="coc.curved",
                                                           label = "Curved edges",
                                                           choices = c("Yes",
                                                                       "No"),
                                                           selected = "No")     
                                               
                                        )
                                        )
                                    ),
                                    br(),
                                    fluidRow(column(6,
                                                    downloadButton("network.coc", strong("Save Pajek"),
                                                                   style ="border-radius: 10px; border-width: 3px;font-size: 15px;",
                                                                   width = "100%")
                                    ),
                                    column(6,
                                           downloadButton("networkCoc.fig", strong("Save HTML"),
                                                          style ="border-radius: 10px; border-width: 3px;font-size: 15px;",
                                                          width = "100%")
                                    )
                                  ),
                                  br(),
                                  selectInput("cocRes",
                                              h4(strong("Export plot")),
                                              choices = c(
                                                "Select the image scale" = 0,
                                                "screen resolution x1" = 1,
                                                "screen resolution x2" = 2,
                                                "screen resolution x3" = 3,
                                                "screen resolution x4" = 4,
                                                "screen resolution x5" = 5,
                                                "screen resolution x6" = 6,
                                                "screen resolution x7" = 7,
                                                "screen resolution x8" = 8
                                              ),
                                              selected = 0),
                                  conditionalPanel(condition = "input.cocRes != 0",
                                                   actionButton("cocPlot.save", strong("Export plot as png"),
                                                                style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                width = "100%")
                                  )
                   ),
                   ## Thematic Map ----
                   conditionalPanel(condition = 'input.sidebarmenu == "thematicMap"',
                                    h4(em(strong("    "))),
                                    "  ",
                                    selectInput("TMfield", 
                                                label = "Field",
                                                choices = c("Keywords Plus" = "ID", 
                                                            "Author's Keywords" = "DE",
                                                            "Titles" = "TI",
                                                            "Abstracts" = "AB"),
                                                selected = "ID"),
                                    conditionalPanel(condition = "input.TMfield == 'TI' | input.TMfield == 'AB'",
                                                     selectInput("TMngrams",'N-Grams',
                                                                 choices = c("Unigrams" = "1",
                                                                             "Bigrams" = "2",
                                                                             "Trigrams" = "3"),
                                                                 selected = 1)),
                                    conditionalPanel(
                                      condition = "input.TMfield == 'TI' | input.TMfield == 'AB'",
                                      selectInput("TMstemming", label="Word Stemming",
                                                  choices = c("Yes" = TRUE,
                                                              "No" = FALSE),
                                                  selected = FALSE)),
                                    br(),
                                    box(title = p(strong("Text Editing"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        selectInput("TMStopFile", "Load a list of terms to remove",
                                                    choices = c("Yes" = "Y",
                                                                "No" = "N"),
                                                    selected = "N"),
                                        conditionalPanel(condition = "input.TMStopFile == 'Y'",
                                                         helpText(h5(strong("Upload a TXT or CSV file containing a list of terms you want to remove from the analysis.")),
                                                                  h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator)."))
                                                         ),
                                                         fileInput("TMStop", "",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv",
                                                                              ".txt")),
                                                         
                                                         selectInput("TMSep", "File Separator",
                                                                     choices = c('Comma ","' = ",",
                                                                                 'Semicolon ";"' = ";",
                                                                                 'Tab '= "\t"),
                                                                     selected = ","),
                                                         h5(htmlOutput("TMStopPreview"))
                                        ),
                                        selectInput("TMapSynFile", "Load a list of synonyms",
                                                    choices = c("Yes" = "Y",
                                                                "No" = "N"),
                                                    selected = "N"),
                                        conditionalPanel(condition = "input.TMapSynFile == 'Y'",
                                                         helpText(h5(strong("Upload a TXT or CSV file containing, in each row, a list of synonyms, that will be merged into a single term (the first word contained in the row)")),
                                                                  h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator). 
                              Rows have to be separated by return separator."))
                                                         ),
                                                         fileInput("TMapSyn", "",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv",
                                                                              ".txt")),
                                                         
                                                         selectInput("TMapSynSep", "File Separator",
                                                                     choices = c('Comma ","' = ",",
                                                                                 'Semicolon ";"' = ";",
                                                                                 'Tab '= "\t"),
                                                                     selected = ","),
                                                         h5(htmlOutput("TMapSynPreview"))
                                        )),
                                    br(),
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        fluidRow(column(6,
                                                        numericInput("TMn", label="Number of Words",value=250,min=50,max=5000,step=1)
                                        ),
                                        column(6,
                                               numericInput("TMfreq", label="Min Cluster Frequency (per thousand docs)",value=5,min=1,max=100,step=1)
                                        )),
                                        fluidRow(column(6,
                                                        numericInput("TMn.labels", label="Number of Labels",value=3,min=0,max=10,step=1)
                                        ),
                                        column(6,
                                               numericInput("sizeTM", label="Label size",value=0.3,min=0.0,max=1,step=0.05)
                                        )),
                                        fluidRow(column(6,
                                                        numericInput("TMrepulsion", label="Community Repulsion",value=0,min=0,max=1,step=0.01)),
                                          column(6,
                                                        selectInput("TMCluster", 
                                                                    label = "Clustering Algorithm",
                                                                    choices = c("None" = "none",
                                                                                "Edge Betweenness" = "edge_betweenness",
                                                                                "Fast Greedy" = "fast_greedy",
                                                                                "InfoMap" = "infomap",
                                                                                "Leading Eigenvalues" = "leading_eigen",
                                                                                "Leiden" = "leiden",
                                                                                "Louvain" = "louvain",
                                                                                "Spinglass" = "spinglass",
                                                                                "Walktrap" = "walktrap"),
                                                                    selected = "walktrap")
                                                        )
                                        )
                                    ),
                                    selectInput(
                                      'TMdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.TMdpi != 'null'",
                                                     sliderInput(
                                                       'TMh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("TMplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")
                                    )
                   ),
                   ## Thematic Evolution ----
                   conditionalPanel(condition = 'input.sidebarmenu == "thematicEvolution"',
                                    h4(em(strong("    "))),
                                    "  ",
                                    selectInput("TEfield", 
                                                label = "Field",
                                                choices = c("Keywords Plus" = "ID", 
                                                            "Author's Keywords" = "DE",
                                                            "Titles" = "TI",
                                                            "Abstracts
                                                           " = "AB"),
                                                selected = "ID"),
                                    conditionalPanel(condition = "input.TEfield == 'TI' | input.TEfield == 'AB'",
                                                     selectInput("TEngrams",'N-Grams',
                                                                 choices = c("Unigrams" = "1",
                                                                             "Bigrams" = "2",
                                                                             "Trigrams" = "3"),
                                                                 selected = 1)),
                                    br(),
                                    box(title = p(strong("Text Editing"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        selectInput("TEStopFile", "Load a list of terms to remove",
                                                    choices = c("Yes" = "Y",
                                                                "No" = "N"),
                                                    selected = "N"),
                                        conditionalPanel(condition = "input.TEStopFile == 'Y'",
                                                         helpText(h5(strong("Upload a TXT or CSV file containing a list of terms you want to remove from the analysis.")),
                                                                  h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator)."))
                                                         ),
                                                         fileInput("TEStop", "",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv",
                                                                              ".txt")),
                                                         
                                                         selectInput("TESep", "File Separator",
                                                                     choices = c('Comma ","' = ",",
                                                                                 'Semicolon ";"' = ";",
                                                                                 'Tab '= "\t"),
                                                                     selected = ","),
                                                         h5(htmlOutput("TEStopPreview"))
                                        ),
                                        selectInput("TESynFile", "Load a list of synonyms",
                                                    choices = c("Yes" = "Y",
                                                                "No" = "N"),
                                                    selected = "N"),
                                        conditionalPanel(condition = "input.TESynFile == 'Y'",
                                                         helpText(h5(strong("Upload a TXT or CSV file containing, in each row, a list of synonyms, that will be merged into a single term (the first word contained in the row)")),
                                                                  h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator). 
                              Rows have to be separated by return separator."))
                                                         ),
                                                         fileInput("TESyn", "",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv",
                                                                              ".txt")),
                                                         
                                                         selectInput("TESynSep", "File Separator",
                                                                     choices = c('Comma ","' = ",",
                                                                                 'Semicolon ";"' = ";",
                                                                                 'Tab '= "\t"),
                                                                     selected = ","),
                                                         h5(htmlOutput("TESynPreview"))
                                        )),
                                    br(),
                                    box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        fluidRow(column(6,
                                                        numericInput("nTE", label="Number of Words",value=250,min=50,max=5000,step=1)
                                        ),
                                        column(6,
                                               numericInput("fTE", label="Min Cluster Frequency (per thousand docs)",value=5,min=1,max=100,step=1)
                                        )),
                                        fluidRow(column(6,
                                                        selectInput("TEmeasure", 
                                                                    label = "Weight index",
                                                                    choices = c("Inclusion Index" = "inclusion", 
                                                                                "Inclusion Index weighted by Word-Occurrences" = "weighted",
                                                                                "Stability Index" = "stability"
                                                                    ),
                                                                    selected = "weighted")
                                        ),
                                        column(6,
                                               numericInput("minFlowTE", label="Min Weight Index",value=0.1,min=0.02,max=1,step=0.02)
                                        )),
                                        fluidRow(column(6,
                                                        numericInput("sizeTE", label="Label size",value=0.3,min=0.0,max=1,step=0.05)
                                        ),
                                        column(6,
                                               numericInput("TEn.labels", label="Number of Labels (for each cluster)",value=3,min=1,max=5,step=1)
                                        )),
                                        fluidRow(column(12,
                                                        selectInput("TECluster", 
                                                                    label = "Clustering Algorithm",
                                                                    choices = c("None" = "none",
                                                                                "Edge Betweenness" = "edge_betweenness",
                                                                                "Fast Greedy" = "fast_greedy",
                                                                                "InfoMap" = "infomap",
                                                                                "Leading Eigenvalues" = "leading_eigen",
                                                                                "Leiden" = "leiden",
                                                                                "Louvain" = "louvain",
                                                                                "Spinglass" = "spinglass",
                                                                                "Walktrap" = "walktrap"),
                                                                    selected = "walktrap")
                                        )
                                        
                                        )
                                    ),
                                    br(),
                                    box(title = p(strong("Time Slices"),style='font-size:16px;color:black;'), 
                                        collapsible = FALSE, width = 15,
                                        solidHeader = FALSE, collapsed = FALSE,
                                        numericInput("numSlices", label="Number of Cutting Points",min=1,max=4,value=1),
                                        "Please, write the cutting points (in year) for your collection",
                                        uiOutput("sliders")
                                    ),
                                    selectInput(
                                      'TEdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.TEdpi != 'null'",
                                                     sliderInput(
                                                       'TEh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("TEplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")
                                    )
                   ),
                   ## Factorial Analysis
                   conditionalPanel(condition = 'input.sidebarmenu == "factorialAnalysis"',
                                    selectInput("method", 
                                                label = "Method",
                                                choices = c("Correspondence Analysis" = "CA",
                                                            "Multiple Correspondence Analysis" = "MCA",
                                                            "Multidimensional Scaling"= "MDS"),
                                                selected = "MCA"),
                                    selectInput("CSfield", 
                                                label = "Field",
                                                choices = c("Keywords Plus" = "ID", 
                                                            "Author's Keywords" = "DE",
                                                            "Titles" = "TI",
                                                            "Abstracts" = "AB"),
                                                selected = "ID"),
                                    conditionalPanel(condition = "input.CSfield == 'TI' | input.CSfield == 'AB'",
                                                     selectInput("CSngrams",'N-Grams',
                                                                 choices = c("Unigrams" = "1",
                                                                             "Bigrams" = "2",
                                                                             "Trigrams" = "3"),
                                                                 selected = 1)),
                                    br(),
                                    box(title = p(strong("Text Editing"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        selectInput("CSStopFile", "Load a list of terms to remove",
                                                    choices = c("Yes" = "Y",
                                                                "No" = "N"),
                                                    selected = "N"),
                                        conditionalPanel(condition = "input.CSStopFile == 'Y'",
                                                         helpText(h5(strong("Upload a TXT or CSV file containing a list of terms you want to remove from the analysis.")),
                                                                  h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator)."))
                                                         ),
                                                         fileInput("CSStop", "",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv",
                                                                              ".txt")),
                                                         
                                                         selectInput("CSSep", "File Separator",
                                                                     choices = c('Comma ","' = ",",
                                                                                 'Semicolon ";"' = ";",
                                                                                 'Tab '= "\t"),
                                                                     selected = ","),
                                                         h5(htmlOutput("CSStopPreview"))
                                        ),
                                        selectInput("FASynFile", "Load a list of synonyms",
                                                    choices = c("Yes" = "Y",
                                                                "No" = "N"),
                                                    selected = "N"),
                                        conditionalPanel(condition = "input.FASynFile == 'Y'",
                                                         helpText(h5(strong("Upload a TXT or CSV file containing, in each row, a list of synonyms, that will be merged into a single term (the first word contained in the row)")),
                                                                  h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator). 
                              Rows have to be separated by return separator."))
                                                         ),
                                                         fileInput("FASyn", "",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv",
                                                                              ".txt")),
                                                         
                                                         selectInput("FASynSep", "File Separator",
                                                                     choices = c('Comma ","' = ",",
                                                                                 'Semicolon ";"' = ";",
                                                                                 'Tab '= "\t"),
                                                                     selected = ","),
                                                         h5(htmlOutput("FASynPreview"))
                                        )),
                                    br(),
                                    box(title = p(strong("Method Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        fluidRow(column(6,
                                                        numericInput("CSn", 
                                                                     label=("Number of terms"), 
                                                                     value = 50, step = 1)),
                                                 column(6,
                                                        selectInput("nClustersCS", 
                                                                    label = "N. of Clusters",
                                                                    choices = c("Auto" = "0", 
                                                                                "2" = "2",
                                                                                "3" = "3",
                                                                                "4" = "4",
                                                                                "5" = "5",
                                                                                "6" = "6",
                                                                                "7" = "7",
                                                                                "8" = "8"),
                                                                    selected = "0")))
                                    ),
                                    br(),
                                    box(title = p(strong("Graphical Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        fluidRow(column(6,
                                                        numericInput(
                                                          inputId = "CSlabelsize",
                                                          label = "Label size",
                                                          min = 5,
                                                          max = 30,
                                                          value = 10)),
                                                 column(6,
                                                        numericInput("CSdoc", 
                                                                     label=("Num. of documents"), 
                                                                     value = 5)))
                                    ),
                                    selectInput(
                                      'FAdpi',
                                      h4(strong(
                                        "Export plots as png"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.FAdpi != 'null'",
                                                     sliderInput(
                                                       'FAh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("FA1plot.save", strong("Term Factorial Map "),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%"),
                                                     h4(" "),
                                                     downloadButton("FA2plot.save", strong("Topic Dendrogram "),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%"),
                                                     h4(" "),
                                                     downloadButton("FA3plot.save", strong("Most Contributing Map "),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%"),
                                                     h4(" "),
                                                     downloadButton("FA4plot.save", strong("Most Cited Map "),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")
                                    )
                   ),
                   ## Co-citation Network ----
                   conditionalPanel(condition = 'input.sidebarmenu == "coCitationNetwork"',
                                    selectInput("citField", 
                                                label = "Field",
                                                choices = c("Papers" = "CR", 
                                                            "Authors" = "CR_AU",
                                                            "Sources" = "CR_SO"),
                                                selected = "CR"),
                                    selectInput(inputId = "citSep", 
                                                label = "Field separator character", 
                                                choices = c('";" (Semicolon)' = ";", 
                                                            '".   " (Dot and 3 or more spaces)' = ".   ",
                                                            '"," (Comma)' = ","),
                                                selected = "';'"),
                                    br(),
                                    box(title = p(strong("Method Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        fluidRow(column(6,
                                                        selectInput("citlayout", 
                                                                    label = "Network Layout",
                                                                    choices = c("Automatic layout"="auto", 
                                                                                "Circle"="circle",
                                                                                "Fruchterman & Reingold"="fruchterman",
                                                                                "Kamada & Kawai"="kamada",
                                                                                "MultiDimensional Scaling"="mds",
                                                                                "Sphere"="sphere",
                                                                                "Star"="star"),
                                                                    selected = "auto")
                                        ),
                                        column(6,
                                               selectInput("cocitCluster", 
                                                           label = "Clustering Algorithm",
                                                           choices = c("None" = "none",
                                                                       "Edge Betweenness" = "edge_betweenness",
                                                                       "Fast Greedy" = "fast_greedy",
                                                                       "InfoMap" = "infomap",
                                                                       "Leading Eigenvalues" = "leading_eigen",
                                                                       "Leiden" = "leiden",
                                                                       "Louvain" = "louvain",
                                                                       "Spinglass" = "spinglass",
                                                                       "Walktrap" = "walktrap"),
                                                           selected = "walktrap")
                                        )),
                                        fluidRow(column(6,
                                                        numericInput(inputId = "citNodes",
                                                                     label = "Number of Nodes",
                                                                     min = 5,
                                                                     max = 1000,
                                                                     value = 50,
                                                                     step = 1)
                                        ),
                                        column(6,
                                               numericInput(inputId = "cocit.repulsion",
                                                            label = "Repulsion Force",
                                                            min = 0,
                                                            max = 1,
                                                            value = 0.1,
                                                            step = 0.1)
                                        )),
                                        fluidRow(column(6,
                                                        selectInput(inputId ="cit.isolates",
                                                                    label = "Remove Isolated Nodes",
                                                                    choices = c("Yes" = "yes",
                                                                                "No" = "no"),
                                                                    selected = "yes")
                                        ),
                                        column(6,
                                               numericInput("citedges.min", 
                                                            label=("Minimum Number of Edges"),
                                                            value = 2,
                                                            step = 1,
                                                            min = 0)
                                        )
                                        )),
                                    br(),
                                    box(title = p(strong("Graphical Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        fluidRow(column(6,
                                                        selectInput(inputId ="citShortlabel",
                                                                    label = "Short Label",
                                                                    choices = c("Yes", 
                                                                                "No"),
                                                                    selected = "Yes"),
                                                        
                                        ),
                                        column(6,
                                               numericInput(inputId = "citLabels",
                                                            label = "Number of labels",
                                                            min = 0,
                                                            max = 1000,
                                                            value = 50,
                                                            step = 1)
                                        )),
                                        fluidRow(column(6,
                                                        selectInput(inputId ="citlabel.cex",
                                                                    label = "Label cex",
                                                                    choices = c("Yes", 
                                                                                "No"),
                                                                    selected = "Yes")
                                        ),
                                        column(6,
                                               selectInput(inputId ="cocit.shape",
                                                           label = "Node Shape",
                                                           choices = c(
                                                             "Box"="box",
                                                             "Circle"="circle",
                                                             "Dot"="dot",
                                                             "Ellipse"="ellipse",
                                                             "Square"="square",
                                                             "Text"="text"),
                                                           selected = "dot")
                                        )),
                                        fluidRow(column(6,
                                                        numericInput(inputId = "citlabelsize",
                                                                     label = "Label size",
                                                                     min = 0.0,
                                                                     max = 20,
                                                                     value = 2,
                                                                     step = 0.10)
                                        ),
                                        column(6,
                                               numericInput(
                                                 inputId = "citedgesize",
                                                 label = "Edge size",
                                                 min = 0.5,
                                                 max = 20,
                                                 value = 5,
                                                 step=0.5)
                                        )), 
                                        fluidRow(column(6,
                                                        selectInput(inputId ="cocit.shadow",
                                                                    label = "Node shadow",
                                                                    choices = c("Yes",
                                                                                "No"),
                                                                    selected = "No")
                                        ),
                                        column(6,
                                               selectInput(inputId ="cocit.curved",
                                                           label = "Curved edges",
                                                           choices = c("Yes",
                                                                       "No"),
                                                           selected = "No")     
                                               
                                        )
                                        )
                                    ),
                                    br(),
                                    fluidRow(column(6,
                                                    downloadButton("network.cocit", strong("Save Pajek"),
                                                                   style ="border-radius: 10px; border-width: 3px;font-size: 15px;",
                                                                   width = "100%")
                                    ),
                                    column(6,
                                           downloadButton("networkCocit.fig", strong("Save HTML"),
                                                          style ="border-radius: 10px; border-width: 3px;font-size: 15px;",
                                                          width = "100%")
                                    )),
                                    br(),
                                    selectInput("cocitRes",
                                                h4(strong("Export plot")),
                                                choices = c(
                                                  "Select the image scale" = 0,
                                                  "screen resolution x1" = 1,
                                                  "screen resolution x2" = 2,
                                                  "screen resolution x3" = 3,
                                                  "screen resolution x4" = 4,
                                                  "screen resolution x5" = 5,
                                                  "screen resolution x6" = 6,
                                                  "screen resolution x7" = 7,
                                                  "screen resolution x8" = 8
                                                ),
                                                selected = 0),
                                    conditionalPanel(condition = "input.cocitRes != 0",
                                                     actionButton("cocitPlot.save", strong("Export plot as png"),
                                                                  style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                  width = "100%")
                                    )
                   ),
                   ## Historiograph ----
                   conditionalPanel(condition = 'input.sidebarmenu == "historiograph"',
                                    numericInput(inputId = "histNodes",
                                                 label = "Number of Nodes",
                                                 min = 5,
                                                 max = 100,
                                                 value = 20,
                                                 step = 1),
                                    "  ",
                                    br(),
                                    box(title = p(strong("Graphical Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, 
                                        collapsed = FALSE,
                                        selectInput(inputId = "titlelabel",
                                                    label = "Node label",
                                                    choices = c("Short id (1st Author, Year)" = "short",
                                                                "Document Title" = "title",
                                                                "Authors' Keywords" = "keywords",
                                                                "Keywords Plus" = "keywordsplus"),
                                                    selected = "short"),
                                        fluidRow(column(6,
                                                        numericInput(inputId = "histlabelsize",
                                                                     label = "Label size",
                                                                     min = 0.0,
                                                                     max = 20,
                                                                     value = 3, step = 1)),
                                                 column(6,
                                                        numericInput(inputId = "histsize",
                                                                     label = "Node size",
                                                                     min = 0,
                                                                     max = 20,
                                                                     value = 4, step = 1)))
                                    ),
                                    br(),
                                    selectInput("HGh",
                                                h4(strong("Export plot")),
                                                choices = c(
                                                  "Select the image scale" = 0,
                                                  "screen resolution x1" = 1,
                                                  "screen resolution x2" = 2,
                                                  "screen resolution x3" = 3,
                                                  "screen resolution x4" = 4,
                                                  "screen resolution x5" = 5,
                                                  "screen resolution x6" = 6,
                                                  "screen resolution x7" = 7,
                                                  "screen resolution x8" = 8
                                                ),
                                                selected = 0),
                                    conditionalPanel(condition = "input.HGh != 0",
                                        actionButton("HGplot.save", strong("Export plot as png"),
                                                                 style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                 width = "100%")
                                   )
                   ),
                   ## Collaboration Network ----
                   conditionalPanel(condition = 'input.sidebarmenu == "collabNetwork"',
                                    selectInput("colField", 
                                                label = "Field",
                                                choices = c("Authors" = "COL_AU", 
                                                            "Institutions" = "COL_UN",
                                                            "Countries" = "COL_CO"),
                                                selected = "COL_AU"),
                                    br(),
                                    box(title = p(strong("Method Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        fluidRow(column(6,
                                                        selectInput("collayout", 
                                                                    label = "Network Layout",
                                                                    choices = c("Automatic layout"="auto", 
                                                                                "Circle"="circle",
                                                                                "Fruchterman & Reingold"="fruchterman",
                                                                                "Kamada & Kawai"="kamada",
                                                                                "MultiDimensional Scaling"="mds",
                                                                                "Sphere"="sphere",
                                                                                "Star"="star"),
                                                                    selected = "auto")
                                        ),
                                        column(6,
                                               selectInput("colCluster", 
                                                           label = "Clustering Algorithm",
                                                           choices = c("None" = "none",
                                                                       "Edge Betweenness" = "edge_betweenness",
                                                                       "Fast Greedy" = "fast_greedy",
                                                                       "InfoMap" = "infomap",
                                                                       "Leading Eigenvalues" = "leading_eigen",
                                                                       "Leiden" = "leiden",
                                                                       "Louvain" = "louvain",
                                                                       "Spinglass" = "spinglass",
                                                                       "Walktrap" = "walktrap"),
                                                           selected = "walktrap")
                                        )),
                                        fluidRow(column(6,
                                                        selectInput("colnormalize", 
                                                                    label = "Normalization",
                                                                    choices = c("none", 
                                                                                "association",
                                                                                "jaccard", 
                                                                                "salton",
                                                                                "inclusion",
                                                                                "equivalence"),
                                                                    selected = "association")
                                        )),
                                        fluidRow(column(6,
                                                        numericInput(inputId = "colNodes",
                                                                     label = "Number of Nodes",
                                                                     min = 5,
                                                                     max = 1000,
                                                                     value = 50,
                                                                     step = 1)
                                        ),
                                        column(6,
                                               numericInput(inputId = "col.repulsion",
                                                            label = "Repulsion Force",
                                                            min = 0,
                                                            max = 1,
                                                            value = 0.1,
                                                            step = 0.1)
                                        )),
                                        fluidRow(column(6,
                                                        selectInput(inputId ="col.isolates",
                                                                    label = "Remove Isolated Nodes",
                                                                    choices = c("Yes" = "yes",
                                                                                "No" = "no"),
                                                                    selected = "yes")
                                        ),
                                        column(6,
                                               numericInput("coledges.min", 
                                                            label=("Minimum Number of Edges"),
                                                            value = 1,
                                                            step = 1,
                                                            min = 0)
                                        )
                                        )),
                                    br(),
                                    box(title = p(strong("Graphical Parameters"),style='font-size:16px;color:black;'), 
                                        collapsible = TRUE, width = 15,
                                        solidHeader = FALSE, collapsed = TRUE,
                                        fluidRow(column(6,
                                                        numericInput(inputId = "colAlpha",
                                                                     label = "Opacity",
                                                                     min = 0,
                                                                     max = 1,
                                                                     value = 0.7,
                                                                     step=0.05)
                                        ),
                                        column(6,
                                               numericInput(inputId = "colLabels",
                                                            label = "Number of labels",
                                                            min = 0,
                                                            max = 1000,
                                                            value = 50,
                                                            step = 1)
                                        )),
                                        fluidRow(column(6,
                                                        selectInput(inputId ="collabel.cex",
                                                                    label = "Label cex",
                                                                    choices = c("Yes", 
                                                                                "No"),
                                                                    selected = "Yes")
                                        ),
                                        column(6,
                                               selectInput(inputId ="col.shape",
                                                           label = "Node Shape",
                                                           choices = c(
                                                             "Box"="box",
                                                             "Circle"="circle",
                                                             "Dot"="dot",
                                                             "Ellipse"="ellipse",
                                                             "Square"="square",
                                                             "Text"="text"),
                                                           selected = "dot")
                                        )),
                                        fluidRow(column(6,
                                                        numericInput(inputId = "collabelsize",
                                                                     label = "Label size",
                                                                     min = 0.0,
                                                                     max = 20,
                                                                     value = 2,
                                                                     step = 0.10)
                                        ),
                                        column(6,
                                               numericInput(
                                                 inputId = "coledgesize",
                                                 label = "Edge size",
                                                 min = 0.5,
                                                 max = 20,
                                                 value = 5,
                                                 step=0.5)
                                        )), 
                                        fluidRow(column(6,
                                                        selectInput(inputId ="col.shadow",
                                                                    label = "Node shadow",
                                                                    choices = c("Yes",
                                                                                "No"),
                                                                    selected = "No")
                                        ),
                                        column(6,
                                               selectInput(inputId ="soc.curved",
                                                           label = "Curved edges",
                                                           choices = c("Yes",
                                                                       "No"),
                                                           selected = "No")     
                                               
                                        ))
                                    ),
                                    br(),
                                    fluidRow(column(6,
                                                    downloadButton("network.col", strong("Save Pajek"),
                                                                   style ="border-radius: 10px; border-width: 3px;font-size: 15px;",
                                                                   width = "100%")
                                    ),
                                    column(6,
                                           downloadButton("networkCol.fig", strong("Save HTML"),
                                                          style ="border-radius: 10px; border-width: 3px;font-size: 15px;",
                                                          width = "100%")
                                    )), 
                                    br(),
                                    selectInput("colRes",
                                                h4(strong("Export plot")),
                                                choices = c(
                                                  "Select the image scale" = 0,
                                                  "screen resolution x1" = 1,
                                                  "screen resolution x2" = 2,
                                                  "screen resolution x3" = 3,
                                                  "screen resolution x4" = 4,
                                                  "screen resolution x5" = 5,
                                                  "screen resolution x6" = 6,
                                                  "screen resolution x7" = 7,
                                                  "screen resolution x8" = 8
                                                ),
                                                selected = 0),
                                    conditionalPanel(condition = "input.colRes != 0",
                                                     actionButton("colPlot.save", strong("Export plot as png"),
                                                                  style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                  width = "100%")
                                    )
                   ),
                   ## Collaboration World Map ----
                   conditionalPanel(condition = 'input.sidebarmenu == "collabWorldMap"',
                                    h4(strong("Method Parameters: ")),
                                    "  ",
                                    numericInput("WMedges.min", 
                                                 label=("Min edges"),
                                                 value = 2,
                                                 step = 1),
                                    "  ",
                                    br(),
                                    h4(strong("Graphical Parameters: ")),
                                    "  ",
                                    sliderInput(inputId = "WMedgesize",
                                                label = "Edge size",
                                                min = 0.1,
                                                max = 20,
                                                value = 5),
                                    br(),
                                    selectInput(
                                      'CCdpi',
                                      h4(strong(
                                        "Export plot"
                                      )),
                                      choices=c(
                                        "dpi value" = "null",
                                        "75 dpi" = "75",
                                        "150 dpi" = "150",
                                        "300 dpi" = "300",
                                        "600 dpi" = "600"
                                      ),
                                      selected = "null"
                                    ),
                                    conditionalPanel(condition = "input.CCdpi != 'null'",
                                                     sliderInput(
                                                       'CCh',
                                                       h4(em(strong(
                                                         "Height (in inches)"
                                                       ))),
                                                       value = 7, min = 1, max = 20, step = 1),
                                                     downloadButton("CCplot.save", strong("Export plot as png"),
                                                                    style ="border-radius: 10px; border-width: 3px;font-size: 20px;",
                                                                    width = "100%")  
                                    )
                   )
            ) 
          )
        )
      )
    )
  })
}



# END ####