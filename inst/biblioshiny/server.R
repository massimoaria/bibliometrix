source("utils.R", local=TRUE)
source("libraries.R", local=TRUE)
source("biblioShot.R", local=TRUE)
source("biblioAI.R", local=TRUE)

suppressMessages(libraries())

#### SERVER ####
server <- function(input, output,session){
  session$onSessionEnded(stopApp)
  
  ## suppress warnings
  options(warn = -1)
  
  if(inherits(try(pagedown::find_chrome(), silent=T), "try-error")) {
    Chrome_url <- NULL
  }else{
    Chrome_url <- pagedown::find_chrome()
  }

  #  Sys.setenv (CHROMOTE_CHROME = Chrome_url)
  
  ## chrome configuration for shinyapps server

  if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
    chromote::set_default_chromote_object(
      chromote::Chromote$new(chromote::Chrome$new(
        args = c("--disable-gpu",
                 "--no-sandbox",
                 "--disable-dev-shm-usage", # required bc the target easily crashes
                 c("--force-color-profile", "srgb"))
      ))
    )
  }
  ## end configuration
  
  ## Check if Chrome browser is installed on the computer
  if(is.null(Chrome_url)){
    showModal(modalDialog(
      title = strong("Warning message!"),
      HTML("Chrome or a Chromium-based browser is not installed on your computer.<br>
If you do not have either of these browsers installed, Biblioshiny will be unable to export graphs.<br>
To ensure the functionality of Biblioshiny,
           please download Chrome by <a href='https://www.google.com/chrome/' target='_blank' > <b>clicking here</b></a>."),
      footer = modalButton("Dismiss"),
      easyClose = TRUE
    ))
  } else {
    Sys.setenv (CHROMOTE_CHROME = Chrome_url)
  }
  
  ## file upload max size
  maxUploadSize <- 200 # default value
  maxUploadSize <- getShinyOption("maxUploadSize", maxUploadSize)
  options(shiny.maxRequestSize=maxUploadSize*1024^2)
  
  ## initial values
  data("logo",package="bibliometrix",envir=environment())
  values = reactiveValues()
  values$sidebar <- sidebarMenu()
  values$rest_sidebar <- FALSE
  values$list_file <- data.frame(sheet=NULL,file=NULL,n=NULL) 
  values$wb <-  openxlsx::createWorkbook()
  values$dfLabel <- dfLabel()
  values$myChoices <- "Empty Report"
  values$logo <- logo
  values$logoGrid <- grid::rasterGrob(logo,interpolate = TRUE)
  values$out <- NULL
  values$loadMenu <- NA
  
  ### column to export in TALL
  if (suppressPackageStartupMessages(!require("tall", quietly = TRUE))) {
      values$TALLmissing <- TRUE
    } else {
      values$TALLmissing <- FALSE  
    }
  values$corpusCol <- c("Title" = "TI","Abstract"="AB", "Author's Keywords"="DE")
  values$metadataCol <- c("Publication Year" = "PY","Document Type"="DT", "DOI"="DI", "Open Access"="OA", "Language"="LA", "First Author"= "AU1")
  
  
  ### setting values
  values$dpi <- 300
  values$h <- 7
  #values$w <- 14 
  values$path <- paste(getwd(),"/", sep="")
  ###
  
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
  
  ## gemini api
  home <- homeFolder()
  path_gemini_key <- paste0(home,"/.biblio_gemini_key.txt", collapse="")
  # check if sub directory exists
  values$geminiAPI <- load_api_key(path_gemini_key)
  values$collection_description <- NULL
  values$gemini_additional <- NULL
  
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
  
  output$rest_of_sidebar <- renderMenu({
    if (isTRUE(values$rest_sidebar)){
      sidebarMenu(.list=values$menu)
    } else {
      sidebarMenu()
    }
  })
  
  observeEvent(input$applyLoad, {
    updateTabItems(session, "sidebarmenu", "loadData")
  })
  
  observeEvent(input$apiApply, {
    updateTabItems(session, "sidebarmenu", "gathData")
  })
  
  observeEvent(values$missTags, {
    switch(values$loadMenu,
           "load"={
             updateTabItems(session, "sidebarmenu", "loadData")
           },
           "merge"={
             updateTabItems(session, "sidebarmenu", "mergeData")
           })
    values$loadMenu <- NA
  })

  observeEvent(input$applyMerge, {
    updateTabItems(session, "sidebarmenu", "mergeData")
  })
  
  ## observe Gemini copy2clipboard button
  observeEvent(input$copy_btn, {
    content <- geminiSave(values, input$sidebarmenu)
    copy_to_clipboard(content)
  })
  
  ## observe Gemini Save button
  output$save_btn <- downloadHandler(
    filename = function() {
      paste0("BiblioAI_",input$sidebarmenu,".txt")
    },
    content <- function(file) {
      txtOutput <- geminiSave(values, input$sidebarmenu)
      writeLines(txtOutput, con=file)
    },
    contentType = "txt"
  )
  
  ## observe gemini generate button
  observeEvent(input$gemini_btn, {
    values$gemini_additional <- input$gemini_additional ## additional info to Gemini prompt
    values <- geminiWaitingMessage(values, input$sidebarmenu)
    values <- geminiGenerate(values, input$sidebarmenu, values$gemini_additional,values$gemini_model_parameters, input)
  })
  
  observeEvent(input$applyLoad,
               {
                 output$collection_descriptionUI <- renderUI({
                   textAreaInput(
                     inputId = "collection_description_merge",
                     label = "Brief description about your collection",
                     placeholder = "Please provide a brief description of your bibliographic collection (e.g., type of content, domain, research hypotheses, timespan) to improve prompts for the BIBLIO AI Assistant.\n\nExample: The corpus consists of 150 academic articles from biomedical journals published between 2015 and 2020...",
                     value = NULL,
                     rows = 3,
                     width = "100%"
                   )
                 })
               })
  
  observeEvent(values$M,{
    updateTextAreaInput(
      session = getDefaultReactiveDomain(),
      inputId = "collection_description",
      label = "Brief description about your collection",
      value = values$collection_description
    )
    updateTextAreaInput(
      session = getDefaultReactiveDomain(),
      inputId = "collection_description_merge",
      label = "Brief description about your collection",
      value = values$collection_description
    )
  })
  
  observeEvent(eventExpr = {
    input$collection_description},
    handlerExpr = {
      if (input$collection_description!="" & nchar(input$collection_description)>1){
        values$collection_description <- input$collection_description
      }
    },ignoreNULL = TRUE)
  
  observeEvent(eventExpr = {
    input$collection_description_merge},
    handlerExpr = {
      if (input$collection_description_merge!="" & nchar(input$collection_description_merge)>1){
        values$collection_description <- input$collection_description_merge
      }
    },ignoreNULL = TRUE)
  
  
  ## Load Menu ----
  
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
      values$rest_sidebar <- TRUE
      values$missingdf <- df <- missingData(values$M)$mandatoryTags
      values$missTags <- NULL
      values$menu <- menuList(values)
      values$collection_description <- "Dataset 'Management':\nA collection of scientific articles about the use of bibliometric approaches in business and management disciplines. Period: 1985–2020."
      showModal(missingModal(session))
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
                   D <-  utils::unzip(inFile$datapath)
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(D,
                                                  dbsource = input$dbsource,
                                                  format = formatDB(D))
                                  M <- authorNameFormat(M, input$authorName)
                                })
                 },
                 ### WoS Txt/Bib Files
                 {
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(inFile$datapath,
                                                  dbsource = input$dbsource,
                                                  format = formatDB(inFile$datapath))
                                  M <- authorNameFormat(M, input$authorName)
                                })
                 })
        },
        scopus = {
          switch(ext,
                 ###  Scopus ZIP Files
                 zip = {
                   D <- utils::unzip(inFile$datapath)
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(D,
                                                  dbsource = input$dbsource,
                                                  format = formatDB(D))
                                  M <- authorNameFormat(M, input$authorName)
                                  if (formatDB(D)=="csv" & input$authorName=="AF") M <- AuthorNameMerge(M)
                                })
                 },
                 ### Scopus CSV/Bib Files
                 csv = {
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(inFile$datapath,
                                                  dbsource = input$dbsource,
                                                  format = "csv")
                                  M <- authorNameFormat(M, input$authorName)
                                  if (input$authorName=="AF") M <- AuthorNameMerge(M)
                                })
                 },
                 bib = {
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(inFile$datapath,
                                                  dbsource = input$dbsource,
                                                  format = "bibtex")
                                  M <- authorNameFormat(M, input$authorName)
                                })
                 })
        },
        openalex={
          withProgress(message = 'Conversion in progress',
                         value = 0, {
                           M <- convert2df(inFile$datapath,
                                           dbsource = input$dbsource,
                                           format = "csv")
                         })
        },
        openalex_api = {
          M <- convert2df(inFile$datapath,
            dbsource = input$dbsource,
            format = "api")
        },
        lens = {
          switch(ext,
                 ###  Lens.org ZIP Files
                 zip = {
                   D <-  utils::unzip(inFile$datapath)
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(D,
                                                  dbsource = input$dbsource,
                                                  format = formatDB(D))
                                })
                 },
                 ### Lens.org CSV Files
                 {
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(inFile$datapath,
                                                  dbsource = input$dbsource,
                                                  format = formatDB(inFile$datapath))
                                })
                 })
        },
        cochrane = {
          switch(ext,
                 ###  Cochrane ZIP Files
                 zip = {
                   D <- utils::unzip(inFile$datapath)
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <- convert2df(D,
                                                  dbsource = input$dbsource,
                                                  format = formatDB(D))
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
                   D <- utils::unzip(inFile$datapath)
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
                   D = utils::unzip(inFile$datapath)
                   withProgress(message = 'Conversion in progress',
                                value = 0, {
                                  M <-
                                    convert2df(D,
                                               dbsource = input$dbsource,
                                               format = formatDB(D))
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
               M <- readxl::read_excel(inFile$datapath, col_types = "text") %>% as.data.frame()
               M$PY <- as.numeric(M$PY)
               M$TC <- as.numeric(M$TC)
               class(M) <- c("bibliometrixDB", "data.frame")
               ### M row names
               ### identify duplicated SRs 
               SR <- M$SR
               tab <- table(SR)
               tab2 <- table(tab)
               ind <- as.numeric(names(tab2))
               ind <- ind[which(ind>1)]
               if (length(ind)>0){
                 for (i in ind){
                   indice=names(which(tab==i))
                   for (j in indice){
                     indice2 <- which(SR==j)
                     SR[indice2] <- paste(SR[indice2],as.character(1:length(indice2)),sep=" ")
                   }
                 }
               }
               M$SR <- SR
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
    ## remove not useful columns
    ind <- which(substr(names(M),1,2)=="X.")
    if (length(ind)>0) M <- M[,-ind]
    ##
    
    values$M <- M
    values$Morig = M
    values$Histfield = "NA"
    values$results = list("NA")
    if (ncol(values$M)>1){values$rest_sidebar <- TRUE}
    if (ncol(values$M)>1){
      values$loadMenu <- "load"
      showModal(missingModal(session))}
  })
  
  output$contents <- DT::renderDT({
    DATAloading()   
    MData = as.data.frame(apply(values$M, 2, function(x) {
      substring(x, 1, 150)
    }))
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
    DTformat(MData, nrow=3, filename="Table", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=TRUE, size='70%', filter="top",
             columnShort=NULL, columnSmall=NULL, round=2, title="", button=FALSE, escape=FALSE, selection=FALSE, scrollX=TRUE)
  })
  
  
  ## Merge Menu ----
  DATAmerging<- eventReactive(input$applyMerge,{
    
    inFile <- input$fileMerge
    
    if (!is.null(inFile)){
      #save(inFile,file="prova.rdata")
      M <- merge_files(inFile)
    } else if (is.null(inFile)) {return(NULL)}
    
    values = initial(values)
    ## remove not useful columns
    ind <- which(substr(names(M),1,2)=="X.")
    if (length(ind)>0) M <- M[,-ind]
    ##
    
    values$M <- M
    values$Morig = M
    values$nMerge <- attr(M,"nMerge")
    values$Histfield = "NA"
    values$results = list("NA")
    if (ncol(values$M)>1){values$rest_sidebar <- TRUE}
    if (ncol(values$M)>1){
      values$loadMenu <- "merge"
      showModal(missingModal(session))}
  })
  
  output$contentsMerge <- DT::renderDT({
    DATAmerging()   
    MData = as.data.frame(apply(values$M, 2, function(x) {
      substring(x, 1, 150)
    }))
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
    DTformat(MData, nrow=3, filename="Table", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=TRUE, size='70%', filter="top",
             columnShort=NULL, columnSmall=NULL, round=2, title="", button=FALSE, escape=FALSE, selection=FALSE, scrollX=TRUE)
  })
  
  observeEvent(input$applyMerge,
               {
                 output$collection_description_mergeUI <- renderUI({
                   textAreaInput(
                     inputId = "collection_description",
                     label = "Brief description about your collection",
                     placeholder = "Please provide a brief description of your bibliographic collection (e.g., source, type of content, domain) to improve prompts for the BIBLIO AI Assistant.\n\nExample: The corpus consists of 150 academic articles from biomedical journals published between 2015 and 2020...",
                     value = NULL,
                     rows = 3,
                     width = "100%"
                   )
                 })
               })
  
  ### Missing Data in Metadata ----
  output$missingDataTable <- DT::renderDT({
    values$missingdf <- df <- missingData(values$M)$mandatoryTags
    values$missTags <- df$tag[df$missing_pct>50]
    values$menu <- menuList(values)
    
    names(df) <- c("Metadata", "Description", "Missing Counts", "Missing %", "Status")
    values$missingDataTable <- DT::datatable(df,escape = FALSE,rownames = FALSE, #extensions = c("Buttons"),
                  class = 'cell-border stripe',
                  selection = 'none',
                  options = list(
                    pageLength = nrow(df),
                    info = FALSE,
                    autoWidth = FALSE, scrollX = TRUE, 
                    dom = 'rti',
                    ordering=F,
                    columnDefs = list(
                      list(
                        targets = ncol(df)-1,
                        createdCell = JS(
                          "function(td, cellData, rowData, row, col) {",
                          "  if (cellData === 'Completely missing') {",
                          "    $(td).css('background-color', '#b22222');",
                          "  } else if (cellData === 'Critical') {",
                          "    $(td).css('background-color', '#f08080');",
                          "  } else if (cellData === 'Poor') {",
                          "    $(td).css('background-color', 'lightgrey');",
                          "  } else if (cellData === 'Acceptable') {",
                          "    $(td).css('background-color', '#f0e68c');",
                          "  } else if (cellData === 'Good') {",
                          "    $(td).css('background-color', '#90ee90');",
                          "  } else if (cellData === 'Excellent') {",
                          "    $(td).css('background-color', '#32cd32');",
                          "  }",
                          "}")
                      )
                    )
                  )
    ) %>% 
      formatRound("Missing %", digits=2) %>% 
      formatStyle(
        "Status",
        textAlign = 'center'
      )
    values$missingDataTable
  })
  
  observeEvent(input$missingMessage,{
    tag <- values$missingdf$description[values$missingdf$status %in% c("Critical", "Completely missing")]
    if (length(values$out)>0){
      text <- paste("The following analyses could not be performed: <br><br>",paste("- ","<em>",values$out,"</em>","<br>", collapse=""),
                    "<br>These menu will be hidden in the Biblioshiny dashboard!",collapse="")
      type <- "warning"
    }else{
      text <- "Your metadata have no critical issues"
      type <- "success"
    }
   
    show_alert(
      title = NULL,
      #text = HTML(paste("Analyses that require the following information:<br>",paste("- ",tag,"<br>", collapse=""),"cannot be performed!",collapse="")),
      text =tagList(
        div(
          h4(HTML(text)),
          style="text-align:left")
      ),
      type = type,
      size = "s", 
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      btn_labels = "OK",
      btn_colors = "#1d8fe1",
      timer = NULL,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  output$missingTitle <- renderUI({
    ndocs <- nrow(values$M)
    if ("DB_Original" %in% names(values$M)){
      DB <- paste0(length(unique(values$M$DB_Original))," DBs")
      txt1 <- paste0("Completeness of metadata -- ", strong(ndocs)," docs merged from ", DB)
      txt2 <- paste0("Original size ",strong(values$nMerge), " docs -- Deleted ", strong(values$nMerge-ndocs), " duplicated docs")
    } else {
      DB <- firstup(values$M$DB[1])
      txt1 <- paste0("Completeness of metadata -- ", strong(ndocs)," docs from ", strong(DB))
      txt2 <- ""
    }
    
    
    tagList(
      div(
        h3(HTML(txt1)),
        br(),
        h4(HTML(txt2)),
        style="text-align:center")
    )
  })
  
  missingModal <- function(session) {
    ns <- session$ns
    modalDialog(
      uiOutput("missingTitle"),
      DT::DTOutput(ns("missingDataTable")),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton(label="Advice", inputId = "missingMessage",
                     icon = icon("exclamation-sign", lib = "glyphicon")),
        actionButton(label="Report", inputId = "missingReport",
                     icon = icon("plus", lib = "glyphicon")),
        actionButton(label="Save", inputId = "missingDataTable",
                     icon = icon("camera", lib = "glyphicon")),
        modalButton(label="Close")),
    )
  }
  
  observeEvent(input$missingDataTable,{
    filename = paste("missingDataTable-", "_",  gsub(" |:","",Sys.time()), ".png", sep="")
    screenShot(values$missingDataTable, filename=filename, type="plotly")
  })
  
  observeEvent(input$missingReport,{
    if (!is.null(values$missingDataTable)){
      sheetname <- "MissingData"
      ind <- which(regexpr(sheetname,values$wb$sheet_names)>-1)
      if (length(ind)>0){
        sheetname <- paste(sheetname,length(ind)+1,sep="")
      } 
      addWorksheet(wb=values$wb, sheetName=sheetname, gridLines = FALSE)
      #values$fileTFP <- screenSh(selector = "#ThreeFieldsPlot") ## screenshot
      values$fileMissingData <- screenSh(values$missingDataTable, zoom = 2, type="plotly")
      values$list_file <- rbind(values$list_file, c(sheetname,values$fileMissingData,1))
      popUp(title="Missing Data Table", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  ## export functions ----
  output$collection.save <- downloadHandler(
    filename = function() {
      paste("Bibliometrix-Export-File-", Sys.Date(), ".",input$save_file, sep="")
    },
    content <- function(file) {
      tr <- FALSE
      if ("CR" %in% names(values$M)) tr <- (sum(nchar(values$M$CR)>32767, na.rm=TRUE))>0
      
      if (tr & input$save_file=="xlsx"){
        show_alert(
          text = tags$span(
            tags$h4("Some documents have too long a list of references that cannot be saved in excel (>32767 characters).",
                    style = "color: firebrick;"),
            tags$br(),
            tags$h4("Data in the column CR could be truncated.",
                    style = "color: firebrick;")
          ),
          #text = "Some documents have too long a list of references that cannot be saved in excel (>32767 characters).\nData in the column CR could be truncated",
          title = "Please save the collection using the 'RData' format",
          type = "warning",
          width =  "50%", ##NEW ----
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          html = FALSE,
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          btn_labels = "OK",
          btn_colors = "#913333",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
        suppressWarnings(openxlsx::write.xlsx(values$M, file=file))
      } else {
        switch(input$save_file,
               xlsx={suppressWarnings(openxlsx::write.xlsx(values$M, file=file))},
               RData={
                 M=values$M
                 save(M, file=file)
               })
      }
    },
    contentType = input$save_file
  )
  
  output$collection.saveMerge <- downloadHandler(
    filename = function() {
      paste("Bibliometrix-Export-File-", Sys.Date(), ".",input$save_fileMerge, sep="")
    },
    content <- function(file) {
      tr <- FALSE
      if ("CR" %in% names(values$M)) tr <- (sum(nchar(values$M$CR)>32767, na.rm=TRUE))>0
      
      if (tr & input$save_file=="xlsx"){
        show_alert(
          text = tags$span(
            tags$h4("Some documents have too long a list of references that cannot be saved in excel (>32767 characters).",
                    style = "color: firebrick;"),
            tags$br(),
            tags$h4("Data in the column CR could be truncated.",
                    style = "color: firebrick;")
          ),
          #text = "Some documents have too long a list of references that cannot be saved in excel (>32767 characters).\nData in the column CR could be truncated",
          title = "Please save the collection using the 'RData' format",
          type = "warning",
          width =  "50%", ##NEW ----
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          html = FALSE,
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          btn_labels = "OK",
          btn_colors = "#913333",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
        suppressWarnings(openxlsx::write.xlsx(values$M, file=file))
      } else {
        switch(input$save_fileMerge,
               xlsx={
                 suppressWarnings(openxlsx::write.xlsx(values$M, file=file))
                 },
               RData={
                 M=values$M
                 save(M, file=file)
               })
      }
    },
    contentType = input$save_fileMerge
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
  
  output$textLog2 <- renderUI({  
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
               if (ncol(values$M)>1){values$rest_sidebar <- TRUE}
               if (ncol(values$M)>1){showModal(missingModal(session))}
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
               if (ncol(values$M)>1){values$rest_sidebar <- TRUE}
               if (ncol(values$M)>1){showModal(missingModal(session))}
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
    }))
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
    DTformat(MData, nrow=3, filename="Table", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=TRUE, size='70%', filter="top",
             columnShort=NULL, columnSmall=NULL, round=2, title="", button=FALSE, escape=FALSE, selection=FALSE,scrollX=TRUE)
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
      mutate(Age = Y - PY+1,
             TCpY = round(TC/Age,2)) %>% 
      group_by(PY) %>% 
      mutate(NTC = TC/mean(TC, na.rm=T)) %>% 
      as.data.frame()
    sliderInput("sliderTCpY", "Average Citations per Year", min = floor(min(values$Morig$TCpY, na.rm=T)),
                max = ceiling(max(values$Morig$TCpY,na.rm=T)), step=0.1,
                value = c(floor(min(values$Morig$TCpY, na.rm=T)),ceiling(max(values$Morig$TCpY,na.rm=T))))
  })
  
  ## Update filtered data ----
  observeEvent(input$applyFilter, {
    updateTabItems(session, "sidebarmenu", "filters")
  })
  
  DTfiltered <- eventReactive(input$applyFilter,{
    M <- values$Morig
    B <- bradford(M)$table
    M <- M %>%
      dplyr::filter(PY >= input$sliderPY[1], PY <= input$sliderPY[2]) %>%
      dplyr::filter(TCpY >= input$sliderTCpY[1], TCpY <= input$sliderTCpY[2]) %>%
      dplyr::filter(DT %in% input$selectType) %>%
      dplyr::filter(LA %in% input$selectLA)
    
    switch(input$bradfordSources,
           "core"={
             so <- B$SO[B$Zone %in% "Zone 1"]
           },
           "zone2"={
             so <- B$SO[B$Zone %in% c("Zone 1", "Zone 2")]
           },
           "all"={so <- B$SO})
    M <- M %>%
      filter(SO %in% so)
    
    values<-initial(values)
    row.names(M) <- M$SR
    class(M) <- c("bibliometrixDB", "data.frame")
    values$M <- M
    Mdisp <- values$M %>%
      mutate(across(everything(), ~ substring(., 1, 150))) %>%
      as.data.frame()
    
    if (dim(Mdisp)[1]>0){
      DTformat(Mdisp, nrow=3, filename="Filtered_DataTable", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=TRUE, size='70%', filter="top",
               columnShort=NULL, columnSmall=NULL, round=2, title="", button=FALSE, escape=FALSE, selection=FALSE,scrollX=TRUE)
    }else{Mdisp=data.frame(Message="Empty collection", row.names = " ")}
  })
  
  output$dataFiltered <- DT::renderDT({
    DTfiltered()
  })
  
  # OVERVIEW ----
  ### Main Info ----
  output$MainInfo <- DT::renderDT({
    DTformat(values$TABvb , nrow=50, filename="Main_Information", pagelength=TRUE, left=1, right=2, numeric=NULL, dom=TRUE, size='100%', filter="none",
             columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, selection=FALSE)
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
  
  observeEvent(input$reportMI,{
    if(!is.null(values$TABvb)){
      sheetname <- "MainInfo"
      list_df <- list(values$TABvb)
      res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
      values$wb <- res$wb
      popUp(title="Main Information", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  # gemini button for word network
  output$MainInfoGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(values, input$sidebarmenu, input)
    geminiOutput(title = "", content = values$MainInfoGemini, values)
    
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
    
    g=ggplot2::ggplot(Y, aes(x = Year, y = Freq, text=paste("Year: ",Year,"\nN .of Documents: ",Freq))) +
      geom_line(aes(group="NA")) +
      #geom_area(aes(group="NA"),fill = 'grey90', alpha = .5) +
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
            ,axis.line.x = element_line(color="black",linewidth=0.5)
            ,axis.line.y = element_line(color="black",linewidth=0.5)
      ) +
      annotation_custom(values$logoGrid, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
    values$ASPplot <- g
    
    plot.ly(g,flip=FALSE, side="r", aspectratio=1.7, size=0.10)
  })
  
  observeEvent(input$reportASP,{
    if(!is.null(values$TAB)){
      list_df <- list(values$TAB)
      list_plot <- list(values$ASPplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "AnnualSciProd", wb=values$wb)
      values$wb <- wb
      popUp(title="Annual Scientific Production", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  output$ASPplot.save <- downloadHandler(
    filename = function() {
      
      paste("AnnualScientificProduction-", Sys.Date(), ".png", sep="")
    },
    
    content <- function(file) {
      ggsave(filename = file, plot = values$ASPplot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
    },
    contentType = "png"
  )
  
  output$AnnualProdTable <- DT::renderDT({
    TAB <- values$TAB
    DTformat(TAB , nrow=10, filename="Annual_Production", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=TRUE, size='100%', filter="none",
             columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, selection=FALSE)
  })
  
  ## Annual Citation per Year ----
  output$AnnualTotCitperYearPlot <- renderPlotly({
    current_year = as.numeric(substr(Sys.Date(),1,4))+1
    Table2 <- values$M %>%
      group_by(PY) %>% 
      summarize(MeanTCperArt=round(mean(TC, na.rm=TRUE),2),
                N =n()) %>% 
      mutate(MeanTCperYear = round(MeanTCperArt/(current_year-PY),2),
             CitableYears = current_year-PY) %>% 
      rename(Year = PY) %>% 
      drop_na()
    values$AnnualTotCitperYear=Table2
    Table2$group="A"
    
    x <- c(max(Table2$Year)-0.02-diff(range(Table2$Year))*0.125, max(Table2$Year)-0.02)+1
    y <- c(min(Table2$MeanTCperYear),min(Table2$MeanTCperYear)+diff(range(Table2$MeanTCperYear))*0.125)
    
    g <- ggplot(Table2, aes(x = Year, y =MeanTCperYear,text=paste("Year: ",Year,"\nAverage Citations per Year: ",round(MeanTCperYear,1)))) +
      geom_line(aes(x = Year, y = MeanTCperYear, group=group)) +
      #geom_area(aes(x = Year, y = MeanTCperYear, group=group),fill = 'grey90', alpha = .5) +
      labs(x = 'Year'
           , y = 'Citations'
           , title = "Average Citations per Year")+
      scale_x_continuous(breaks= (Table2$Year[seq(1,length(Table2$Year),by=2)])) +
      theme(text = element_text(color = "#444444")
            ,panel.background = element_rect(fill = '#FFFFFF')
            ,panel.grid.minor = element_line(color = '#EFEFEF')
            ,panel.grid.major = element_line(color = '#EFEFEF')
            ,plot.title = element_text(size = 24)
            ,axis.title = element_text(size = 14, color = '#555555')
            ,axis.title.y = element_text(vjust = 1, angle = 0)
            ,axis.title.x = element_text(hjust = 0)
            ,axis.line.x = element_line(color="black",linewidth=0.5)
            ,axis.line.y = element_line(color="black",linewidth=0.5)
      ) + 
      annotation_custom(values$logoGrid, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
    values$ACpYplot <- g
    plot.ly(g,flip=FALSE, side="r", aspectratio=1.7, size=0.10)
  })
  
  observeEvent(input$reportACpY,{
    if(!is.null(values$AnnualTotCitperYear)){
      list_df <- list(values$AnnualTotCitperYear)
      list_plot <- list(values$ACpYplot)
      wb <- addSheetToReport(list_df, list_plot, sheetname = "AnnualCitPerYear", wb = values$wb)
      values$wb <- wb
      popUp(title="Average Citations per Year", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  output$ACpYplot.save <- downloadHandler(
    filename = function() {
      paste("AverageArticleCitationPerYear-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$ACpYplot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
    },
    contentType = "png"
  )
  
  output$AnnualTotCitperYearTable <- DT::renderDT({
    TAB <- values$AnnualTotCitperYear
    DTformat(TAB , nrow=10, filename="Annual_Total_Citation_per_Year", pagelength=TRUE, left=NULL, right=NULL, numeric=c(2,4), dom=TRUE, size='100%', filter="none",
             columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, selection=FALSE)
  })
  
  ## Three Fields Plot ---- 
  TFP <- eventReactive(input$apply3F,{
    fields=c(input$LeftField, input$CentralField, input$RightField)
    threeFieldsPlot(values$M, fields=fields,n=c(input$LeftFieldn, input$CentralFieldn,input$RightFieldn))
  })
  
  output$ThreeFieldsPlot <- renderPlotly({
    values$TFP <- TFP()  
    
    is.reactive(values$TFP)
    values$TFP
  })
  
  
  # gemini button for Three Fields Plot
  output$TFPGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(values, input$sidebarmenu, input)
    geminiOutput(title = "", content = values$TFPGemini, values)
    
  })
  
  observeEvent(input$reportTFP,{
    if (!is.null(values$TFP)){
      sheetname <- "ThreeFieldsPlot"
      ind <- which(regexpr(sheetname,values$wb$sheet_names)>-1)
      if (length(ind)>0){
        sheetname <- paste(sheetname, "(", length(ind) + 1, ")", sep = "")
      } 
      addWorksheet(wb=values$wb, sheetName=sheetname, gridLines = FALSE)
      values$fileTFP <- screenSh(values$TFP, zoom = 2, type="plotly")
      values$list_file <- rbind(values$list_file, c(sheetname,values$fileTFP,1))
      popUp(title="Three-Field Plot", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
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
      ggsave(filename = file, plot = values$MRSplot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
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
    DTformat(TAB , nrow=10, filename="Most_Relevant_Sources", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportMRS,{
    if(!is.null(values$TABSo)){
      list_df <- list(values$TABSo %>% drop_na())
      list_plot <- list(values$MRSplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "MostRelSources", wb=values$wb)
      values$wb <- wb
      popUp(title="Most Relevant Sources", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  ### Most Local Cited Sources ---- 
  MLCSources <- eventReactive(input$applyMLCSources,{
    values$M=metaTagExtraction(values$M,"CR_SO")
    TAB=tableTag(values$M,"CR_SO")
    TAB=data.frame(Sources=names(TAB),Articles=as.numeric(TAB))
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
      ggsave(filename = file, plot = values$MLCSplot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
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
    DTformat(TAB , nrow=10, filename="Most_Cited_Sources", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportMLS,{
    if(!is.null(values$TABSoCit)){
      list_df <- list(values$TABSoCit)
      list_plot <- list(values$MLCSplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "MostLocCitSources", wb=values$wb)
      values$wb <- wb
      popUp(title="Most Local Cited Sources", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
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
      ggsave(filename = file, plot = values$bradford$graph, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
    },
    contentType = "png"
  )
  
  output$bradfordTable <- DT::renderDT({
    DTformat(values$bradford$table , nrow=10, filename="Bradford_Law", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportBradford,{
    if(!is.null(values$bradford$table)){
      list_df <- list(values$bradford$table)
      list_plot <- list(values$bradford$graph)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "BradfordLaw", wb=values$wb)
      values$wb <- wb
      popUp(title="Core Sources by Bradford's Law", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
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
      ggsave(filename = file, plot = values$SIplot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
    },
    contentType = "png"
  )
  
  output$SourceHindexPlot <- renderPlotly({
    Hsource()
  })
  
  output$SourceHindexTable <- DT::renderDT({
    DTformat(values$H %>% rename(Source = Element) , nrow=10, filename="Source_Impact", pagelength=TRUE, left=NULL, right=NULL, numeric=4, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportSI,{
    if(!is.null(values$H)){
      list_df <- list(values$H %>% rename(Source = Element))
      list_plot <- list(values$SIplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "SourceLocImpact", wb=values$wb)
      values$wb <- wb
      popUp(title="Sources' Local Impact", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
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
    values$SODF=data.frame(Year=rep(values$PYSO$Year,(dim(values$PYSO)[2]-1)),Source=term, Freq=freq)
    
    Text <- paste(values$SODF$Source," (",values$SODF$Year,") ",values$SODF$Freq, sep="")
    
    width_scale <- 1.7 * 26 / length(unique(values$SODF$Source))
    
    x <- c(max(values$SODF$Year)-0.02-diff(range(values$SODF$Year))*0.15, max(values$SODF$Year)-0.02)+1
    y <- c(min(values$SODF$Freq),min(values$SODF$Freq)+diff(range(values$SODF$Freq))*0.15)
    
    g=ggplot(values$SODF, aes(x=values$SODF$Year,y=values$SODF$Freq, group=values$SODF$Source, color=values$SODF$Source, text=Text))+
      geom_line()+
      labs(x = 'Year'
           , y = laby
           , title = "Sources' Production over Time") +
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
            ,axis.line.x = element_line(color="black",linewidth=0.5)
            ,axis.line.y = element_line(color="black",linewidth=0.5)
      ) + annotation_custom(values$logoGrid, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
    
    values$SDplot <- g
    return(g)
  }) 
  
  output$SDplot.save <- downloadHandler(
    filename = function() {
      paste("SourceDynamics-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$SDplot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
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
               'toImage',
               'sendDataToCloud',
               'pan2d', 
               'select2d', 
               'lasso2d',
               'toggleSpikelines',
               'hoverClosestCartesian',
               'hoverCompareCartesian'
             ))  %>%
      layout(hovermode = 'compare')
  })
  
  output$soGrowthtable <- DT::renderDT({
    
    g <- SOGrowth()
    soData=values$PYSO
    DTformat(soData , nrow=10, filename="Source_Prod_over_Time", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportSD,{
    if(!is.null(values$PYSO)){
      list_df <- list(values$PYSO)
      list_plot <- list(values$SDplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "SourceProdOverTime", wb=values$wb)
      values$wb <- wb
      popUp(title="Sources' Production over Time", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
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
      ggsave(filename = file, plot = values$MRAplot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
    },
    contentType = "png"
  )
  
  output$MostRelAuthorsPlot <- renderPlotly({
    
    g <- MRAuthors()
    plot.ly(g,flip=FALSE, side="r", aspectratio=1.3, size=0.10)
  })
  
  output$MostRelAuthorsTable <- DT::renderDT({
    
    TAB <- values$TABAu
    DTformat(TAB , nrow=10, filename="Most_Relevant_Authors", pagelength=TRUE, left=NULL, right=NULL, numeric=3, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportMRA,{
    if(!is.null(values$TABAu)){
      list_df <- list(values$TABAu)
      list_plot <- list(values$MRAplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "MostRelAuthors", wb=values$wb)
      values$wb <- wb
      popUp(title="Most Relevant Authors", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
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
      ggsave(filename = file, plot = values$MLCAplot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
    },
    contentType = "png"
  )
  
  output$MostCitAuthorsPlot <- renderPlotly({
    
    g <- MLCAuthors()
    plot.ly(g,flip=FALSE, side="r", aspectratio=1.3, size=0.10)
  })
  
  output$MostCitAuthorsTable <- DT::renderDT({
    
    TAB <- values$TABAuCit
    DTformat(TAB , nrow=10, filename="Most_Local_Cited_Authors", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportMLCA,{
    if(!is.null(values$TABAuCit)){
      list_df <- list(values$TABAuCit)
      list_plot <- list(values$MLCAplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "MostLocCitAuthors", wb=values$wb)
      values$wb <- wb
      popUp(title="Most Local Cited Authors", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
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
      ggsave(filename = file, plot = values$AIplot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
    },
    contentType = "png"
  )
  
  output$AuthorHindexPlot <- renderPlotly({
    res <- HAuthors()
    plot.ly(res$g,flip=FALSE, side="r", aspectratio=1.3, size=0.10)
  })
  
  output$AuthorHindexTable <- DT::renderDT({
    DTformat(values$H %>% rename(Author = Element), nrow=10, filename="Author_Impact", pagelength=TRUE, left=NULL, right=NULL, numeric=4, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportAI,{
    if(!is.null(values$H)){
      list_df <- list(values$H %>% rename(Author = Element))
      list_plot <- list(values$AIplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "AuthorLocImpact", wb=values$wb)
      values$wb <- wb
      popUp(title="Authors' Local Impact", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  ### Authors Production Over Time ----  
  AUoverTime <- eventReactive(input$applyAUoverTime,{
    values$AUProdOverTime <- authorProdOverTime(values$M, k=input$TopAuthorsProdK, graph=FALSE)
  })
  
  # gemini button for Apot
  output$ApotGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(values, input$sidebarmenu, input)
    geminiOutput(title = "", content = values$ApotGemini, values)
    
  })
  
  output$APOTplot.save <- downloadHandler(
    filename = function() {
      paste("AuthorsProductionOverTime-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$AUProdOverTime$graph, dpi = values$dpi, height = values$h, width = values$h*2.5, bg="white")
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
    DTformat(TAB , nrow=10, filename="Author_Prod_over_Time", pagelength=TRUE, left=NULL, right=NULL, numeric=5, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$TopAuthorsProdTablePapers <- DT::renderDT({
    AUoverTime()
    TAB <- values$AUProdOverTime$dfPapersAU
    TAB$DOI=paste0('<a href=\"https://doi.org/',TAB$DOI,'\" target=\"_blank\">',TAB$DOI,'</a>')
    DTformat(TAB , nrow=10, filename="Author_Prod_over_Time_Docs", pagelength=TRUE, left=NULL, right=NULL, numeric=7, dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
    })
  
  observeEvent(input$reportAPOT,{
    if(!is.null(values$AUProdOverTime$dfPapersAU)){
      list_df <- list(values$AUProdOverTime$dfPapersAU)
      list_plot <- list(values$AUProdOverTime$graph)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "AuthorProdOverTime", wb=values$wb)
      values$wb <- wb
      popUp(title="Authors' Production over Time", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  ### Lotka Law ----  
  output$lotkaPlot <- renderPlotly({
    
    values$lotka <- lotka(values$M)
    
    values$LLplot <- values$lotka$g
    plot.ly(values$lotka$g_shiny,flip=FALSE, side="r", aspectratio=1.4, size=0.10)
  })
  
  output$LLplot.save <- downloadHandler(
    filename = function() {
      paste("LotkaLaw-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$LLplot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
    },
    contentType = "png"
  )
  
  output$lotkaTable <- DT::renderDT({

    DTformat(values$lotka$AuthorProd, nrow=10, filename="Lotka_Law", pagelength=TRUE, left=NULL, right=NULL, numeric=c(3,4), dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportLotka,{
    if(!is.null(values$lotka$AuthorProd)){
      list_df <- list(values$lotka$AuthorProd)
      list_plot <- list(values$LLplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "LotkaLaw", wb=values$wb)
      values$wb <- wb
      popUp(title="Author Productivity through Lotka's Law", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
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
      ggsave(filename = file, plot = values$AFFplot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
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
    DTformat(TAB, nrow=10, filename="Most_Relevant_Affiliations", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportMRAFF,{
    if(!is.null(values$TABAff)){
      list_df <- list(values$TABAff)
      list_plot <- list(values$AFFplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "MostRelAffiliations", wb=values$wb)
      values$wb <- wb
      popUp(title="Most Relevant Affiliations", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
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
      ggsave(filename = file, plot = values$AffOverTimePlot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
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
    afftimeData <- values$AffOverTime
    DTformat(afftimeData, nrow=10, filename="Affiliation_over_Time", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportAFFPOT,{
    if(!is.null(values$AffOverTime)){
      list_df <- list(values$AffOverTime)
      list_plot <- list(values$AffOverTimePlot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "AffOverTime", wb=values$wb)
      values$wb <- wb
      popUp(title="Affiliations' Production over Time", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  # Countries ----
  ### Country by Corresponding Authors ----  
  CAUCountries <- eventReactive(input$applyCAUCountries,{
    res <- descriptive(values,type="tab5")
    values <-res$values
    values$TABCo <- values$TAB
    
    k=input$MostRelCountriesK
    xx <- values$TABCo %>% slice_head(n=k) %>% 
      select(Country,SCP,MCP)
    xx=xx[order(-(xx$SCP+xx$MCP)),]
    xx1=cbind(xx[,1:2],rep("SCP",k))
    names(xx1)=c("Country","Freq","Collaboration")
    xx2=cbind(xx[,c(1,3)],rep("MCP",k))
    names(xx2)=c("Country","Freq","Collaboration")
    xx=rbind(xx2,xx1)
    xx$Country=factor(xx$Country,levels=xx$Country[1:dim(xx2)[1]])
    
    xx2 <- xx %>% dplyr::group_by(Country) %>%
      dplyr::summarize(Freq = sum(Freq))
    
    x <- c(0.5,0.5+length(levels(xx2$Country))*0.125)+1
    y <- c(max(xx2$Freq)-0.02-diff(range(xx2$Freq))*0.125,max(xx2$Freq)-0.02)
    
    g=suppressWarnings(ggplot2::ggplot(data=xx, aes(x=Country, y=Freq,fill=Collaboration, text=paste("Country: ",Country,"\nN.of Documents: ",Freq))) +
                         geom_bar(aes(group="NA"),stat="identity")+
                         scale_x_discrete(limits = rev(levels(xx$Country)))+
                         scale_fill_discrete(name="Collaboration",
                                             breaks=c("SCP","MCP"))+
                         labs(title = "Corresponding Author's Countries", x = "Countries", y = "N. of Documents", 
                              caption = "SCP: Single Country Publications, MCP: Multiple Country Publications")+
                         theme(plot.caption = element_text(size = 9, hjust = 0.5,
                                                           color = "blue", face = "italic")
                               ,panel.background = element_rect(fill = '#FFFFFF')
                               ,panel.grid.major.y  = element_line(color = '#EFEFEF')
                               ,plot.title = element_text(size = 24)
                               ,axis.title = element_text(size = 14, color = '#555555')
                               ,axis.title.y = element_text(vjust = 1, angle = 0)
                               ,axis.title.x = element_text(hjust = 0)
                               ,axis.line.x = element_line(color="black",linewidth=0.5)
                         ) +
                         coord_flip()) + 
      annotation_custom(values$logoGrid, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
    values$TABCo <- values$TABCo %>% 
      mutate(Freq = Freq*100,
             MCP_Ratio = MCP_Ratio*100) %>% 
      rename("Articles %" = Freq,
             "MCP %" = MCP_Ratio) %>% 
      select(Country, "Articles","Articles %", SCP, MCP, "MCP %")
      
    values$MRCOplot <- g
    return(g)
  }) 
  
  # gemini button for word network
  output$MostRelCountriesGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(values, input$sidebarmenu, input)
    geminiOutput(title = "", content = values$MostRelCountriesGemini, values)
    
  })
  
  output$MRCOplot.save <- downloadHandler(
    filename = function() {
      paste("MostRelevantCountries-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MRCOplot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
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
    DTformat(TAB, nrow=10, filename="Most_Relevant_Countries_By_Corresponding_Author", pagelength=TRUE, left=NULL, right=NULL, numeric=c(3,6), dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=1, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportMRCO,{
    if(!is.null(values$TABCo)){
      list_df <- list(values$TABCo)
      list_plot <- list(values$MRCOplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "CorrAuthCountries", wb=values$wb)
      values$wb <- wb
      popUp(title="Corresponding Author's Countries", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
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
      ggsave(filename = file, plot = values$mapworld$g, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
    },
    contentType = "png"
  )
  
  output$countryProdTable <- DT::renderDT({
    
    TAB <- values$mapworld$tab %>% rename(Country = region)
    DTformat(TAB, nrow=10, filename="Country_Production", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportCSP,{
    if(!is.null(values$mapworld$tab)){
      list_df <- list(values$mapworld$tab)
      list_plot <- list(values$mapworld$g)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "CountrySciProd", wb=values$wb)
      values$wb <- wb
      popUp(title="Countries' Scientific Production", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  ### Countries' Production Over Time ----  
  COGrowth <- eventReactive(input$applyCOGrowth,{
    
    values <- CountryOverTime(values,input$topCO)
    
  }) 
  
  output$CountryOverTimeplot.save <- downloadHandler(
    filename = function() {
      paste("CountryOverTime-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$CountryOverTimePlot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
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
               'toImage',
               'sendDataToCloud',
               'pan2d', 
               'select2d', 
               'lasso2d',
               'toggleSpikelines',
               'hoverClosestCartesian',
               'hoverCompareCartesian'
             )) %>%
      layout(hovermode = 'compare')
  })
  
  output$CountryOverTimeTable <- DT::renderDT({
    
    COGrowth()
    cotimeData=values$CountryOverTime
    DTformat(cotimeData, nrow=10, filename="Countries_Production_Over_Time", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportCPOT,{
    if(!is.null(values$CountryOverTime)){
      list_df <- list(values$CountryOverTime)
      list_plot <- list(values$CountryOverTimePlot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "CountryProdOverTime", wb=values$wb)
      values$wb <- wb
      popUp(title="Countries' Production over Time", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
    
    
    
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
      laby="Average Article Citations"
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
      ggsave(filename = file, plot = values$MCCplot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
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
    DTformat(TAB, nrow=10, filename="Most_Cited_Countries", pagelength=TRUE, left=NULL, right=NULL, numeric=3, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportMCCO,{
    if(!is.null(values$TABCitCo)){
      list_df <- list(values$TABCitCo)
      list_plot <- list(values$MCCplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "MostCitCountries", wb=values$wb)
      values$wb <- wb
      popUp(title="Most Cited Countries", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
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
        xx[,2] <- round(xx[,2],1)
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
      ggsave(filename = file, plot = values$MGCDplot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
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
    DTformat(TAB, nrow=10, filename="Most_Global_Cited_Documents", pagelength=TRUE, left=NULL, right=NULL, numeric=4:5, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportMCD,{
    if(!is.null(values$TABGlobDoc)){
      list_df <- list(values$TABGlobDoc)
      list_plot <- list(values$MGCDplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "MostGlobCitDocs", wb=values$wb)
      values$wb <- wb
      popUp(title="Most Global Cited Documents", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  ### Most Local Cited Documents ----  
  MLCDocuments <- eventReactive(input$applyMLCDocuments,{
    withProgress(message = 'Calculation in progress',
                 value = 0, {
                   TAB <-localCitations(values$M, fast.search=FALSE, sep = input$LocCitSep)$Paper
                   TAB <- TAB %>%
                     group_by(Year) %>%
                     mutate(Ratio = LCS/GCS*100,
                            NLCS = LCS/mean(LCS),
                            NGCS = GCS/mean(GCS)) %>%
                     ungroup() %>%
                     as.data.frame()
                 })
    
    xx=data.frame(Document=as.character(TAB[,1]), DOI=as.character(TAB[,2]), Year=TAB[,3], 
                  "Local Citations"=TAB[,4], "Global Citations"=TAB[,5],"LC/GC Ratio"=TAB[6], 
                  "Normalized Local Citations"=TAB[,7],"Normalized Global Citations"=TAB[,8])
    values$TABLocDoc=xx
    
    if (input$MostLocCitDocsK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostLocCitDocsK}
    
    xx=xx[1:k,]
    
    g <- freqPlot(xx,x=4,y=1, textLaby = "Documents", textLabx = "Local Citations", title = "Most Local Cited Documents", values)
    
    values$MLCDplot <- g
    return(g)
  })
  
  # gemini button for word network
  output$MostLocCitDocsGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(values, input$sidebarmenu, input)
    geminiOutput(title = "", content = values$MostLocCitDocsGemini, values)
  })
  
  output$MLCDplot.save <- downloadHandler(
    filename = function() {
      paste("MostLocalCitedDocuments-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MLCDplot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
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
    DTformat(TAB, nrow=10, filename="Most_Local_Cited_Documents", pagelength=TRUE, left=NULL, right=NULL, numeric=6:8, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportMLCD,{
    if(!is.null(values$TABLocDoc)){
      list_df <- list(values$TABLocDoc)
      list_plot <- list(values$MLCDplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "MostLocCitDocs", wb=values$wb)
      values$wb <- wb
      popUp(title="Most Local Cited Documents", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  ## Cited References ----
  ### Most Local Cited References ----
  MLCReferences <- eventReactive(input$applyMLCReferences,{
    CR <- citations(values$M,sep=input$CitRefsSep)$Cited
    TAB <- data.frame(names(CR),as.numeric(CR))
    names(TAB) <- c("Cited References", "Citations")
    values$TABCitRef <- TAB %>% filter(`Cited References`!="ANONYMOUS, NO TITLE CAPTURED")
    
    xx=values$TABCitRef
    if (input$MostCitRefsK>dim(xx)[1]){
      k=dim(xx)[1]
    } else {k=input$MostCitRefsK}
    
    xx=xx[1:k,]
    
    g <- freqPlot(xx,x=2,y=1, textLaby = "References", textLabx = "Local Citations", title = "Most Local Cited References", values, string.max=70)
    
    values$MLCRplot <- g
    return(g)
  })
  
  output$MLCRplot.save <- downloadHandler(
    filename = function() {
      paste("MostLocalCitedReferences-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MLCRplot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
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
    DTformat(TAB, nrow=10, filename="Most_Local_Cited_References", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportMLCR,{
    if(!is.null(values$TABCitRef)){
      list_df <- list(values$TABCitRef)
      list_plot <- list(values$MLCRplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "MostLocCitRefs", wb=values$wb)
      values$wb <- wb
      popUp(title="Most Local Cited References", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
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
      ggsave(filename = file, plot = values$res$spectroscopy, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
    },
    contentType = "png"
  )
  
  # gemini button for rpys
  output$rpysGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(values, input$sidebarmenu, input)
    geminiOutput(title = "", content = values$rpysGemini, values)
    
  })
  
  output$rpysPlot <- renderPlotly({
    RPYS()
    plot.ly(values$res$spectroscopy, side="l", aspectratio = 1.3, size=0.10)
  })
  
  output$rpysTable <- DT::renderDT({
    RPYS()
    rpysData=values$res$rpysTable
    DTformat(rpysData, nrow=10, filename="RPYS", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$crTable <- DT::renderDT({
    RPYS()
    crData=values$res$CR
    crData$link <- paste0('<a href=\"https://scholar.google.it/scholar?hl=en&as_sdt=0%2C5&q=',crData$Reference,'\" target=\"_blank\">','link','</a>')
    
    crData=crData[order(-as.numeric(crData$Year),-crData$Freq),]
    names(crData)=c("Year", "Reference", "Local Citations", "Google link")
    crData <- crData[,c(1,4,2,3)] 
    DTformat(crData, nrow=10, filename="RPYS_Documents", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportRPYS,{
    if(!is.null(values$res$CR)){
      list_df <- list(values$res$CR, values$res$rpysTable)
      list_plot <- list(values$res$spectroscopy)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "RPYS", wb=values$wb)
      values$wb <- wb
      popUp(title="Reference Spectroscopy", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  ## Words ----
  ### Most Frequent Words ----
  
  observeEvent(input$MostRelWordsStop,{
    values$MRWremove.terms <- data.frame(stopword=trimws(readStopwordsFile(file=input$MostRelWordsStop, sep=input$MostRelWordsSep)))
    values$GenericSL <- values$MRWremove.terms
    popUpGeneric(title="Stopword list", 
                 type=NULL, 
                 color=c("#1d8fe1"),
                 subtitle=DTOutput("stopwordList"),
                 btn_labels="OK")
    
  })
  
  observeEvent(input$MRWSyn,{
    synonyms <- trimws(readSynWordsFile(file=input$MRWSyn, sep=input$MRWSynSep))
    term <- unlist(lapply(strsplit(synonyms,";"),function(l){l[1]}))
    synList <- unlist(lapply(strsplit(synonyms,";"),function(l){
      paste0(trimws(l[-1]),collapse=";")
      }))
    values$MRWsyn.terms <- data.frame(term=term, synonyms=synList)
    values$GenericSYN <- values$MRWsyn.terms
    popUpGeneric(title="Synonym List", 
                 type=NULL, 
                 color=c("#1d8fe1"),
                 subtitle=DTOutput("synonymList"),
                 btn_labels="OK")
    
  })
  
  output$stopwordList <- renderDT({
    DTformat(values$GenericSL, nrow=Inf, filename="Stopword_List", pagelength=FALSE, left=1, right=NULL, numeric=NULL, dom="none", 
             size='90%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=FALSE, escape=FALSE, 
             selection=FALSE, scrollY=TRUE)
  })
  
  output$synonymList <- renderDT({
    DTformat(values$GenericSYN, nrow=Inf, filename="Stopword_List", pagelength=FALSE, left=1, right=NULL, numeric=NULL, dom="none", 
             size='90%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=FALSE, escape=FALSE, 
             selection=FALSE, scrollY=TRUE)
  })
  
  MFWords <- eventReactive(input$applyMFWords,{
    if (input$MostRelWords %in% c("TI","AB")){
      ngrams <- as.numeric(input$MRWngrams)
    }else{
      ngrams <- 1
    }
    
    ### load file with terms to remove
    if (input$MostRelWordsStopFile=="Y"){
      remove.terms <- trimws(values$MRWremove.terms$stopword)
    }else{remove.terms <- NULL}
    #values$MRWremove.terms <- remove.terms
    ### end of block
    
    ### load file with synonyms
    if (input$MRWSynFile=="Y"){
      synonyms <- values$MRWsyn.terms %>% group_by(term) %>% mutate(term=paste0(term,";",synonyms)) %>% select(term)
      synonyms <- synonyms$term
    }else{synonyms <- NULL}
    #values$MRWsyn.terms <- synonyms
    ### end of block
    
    WR <- wordlist(values$M,Field=input$MostRelWords,n=Inf,measure="identity", ngrams=ngrams, remove.terms = remove.terms, synonyms = synonyms)$v
    
    TAB <- data.frame(names(WR),as.numeric(WR))
    names(TAB) <- c("Words", "Occurrences")
    values$TABWord <- TAB
    
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
  
  output$MRWplot.save <- downloadHandler(
    filename = function() {
      paste("MostRelevantWords-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$MRWplot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
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
    DTformat(TAB, nrow=10, filename="Most_Frequent_Words", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportMFW,{
    if(!is.null(values$TABWord)){
      list_df <- list(values$TABWord)
      list_plot <- list(values$MRWplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "MostFreqWords", wb=values$wb)
      values$wb <- wb
      popUp(title="Most Frequent Words", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  ### WordCloud ----  
  observeEvent(input$WCStop,{
    values$WCremove.terms <- data.frame(stopword=trimws(readStopwordsFile(file=input$WCStop, sep=input$WCSep)))
    values$GenericSL <- values$WCremove.terms
    popUpGeneric(title="Stopword list", 
                 type=NULL, 
                 color=c("#1d8fe1"),
                 subtitle=DTOutput("stopwordList"),
                 btn_labels="OK")
    
  })

  observeEvent(input$WCSyn,{
    synonyms <- trimws(readSynWordsFile(file=input$WCSyn, sep=input$WCSynSep))
    term <- unlist(lapply(strsplit(synonyms,";"),function(l){l[1]}))
    synList <- unlist(lapply(strsplit(synonyms,";"),function(l){
      paste0(trimws(l[-1]),collapse=";")
    }))
    values$WCsyn.terms <- data.frame(term=term, synonyms=synList)
    values$GenericSYN <- values$WCsyn.terms
    popUpGeneric(title="Synonym List", 
                 type=NULL, 
                 color=c("#1d8fe1"),
                 subtitle=DTOutput("synonymList"),
                 btn_labels="OK")
    
  })

  WordCloud <- eventReactive(input$applyWordCloud,{
    if (input$summaryTerms %in% c("TI","AB")){
      ngrams <- as.numeric(input$summaryTermsngrams)
    }else{
      ngrams <- 1
    }
    
    ### load file with terms to remove
    if (input$WCStopFile=="Y"){
      remove.terms <- trimws(values$WCremove.terms$stopword)
    }else{remove.terms <- NULL}
    #values$WCremove.terms <- remove.terms
    ### end of block
    
    ### load file with synonyms
    if (input$WCSynFile=="Y"){
      synonyms <- values$WCsyn.terms %>% group_by(term) %>% mutate(term=paste0(term,";",synonyms)) %>% select(term)
      synonyms <- synonyms$term
      print(synonyms)
    }else{synonyms <- NULL}
    #values$WCsyn.terms <- synonyms
    ### end of block
    
    resW=wordlist(M=values$M, Field=input$summaryTerms, n=input$n_words, measure=input$measure, ngrams=ngrams, remove.terms = remove.terms, synonyms = synonyms)
    
    W=resW$W
    values$Words <- resW$Words
    
    values$WordCloud <- wordcloud2::wordcloud2(W, size = input$scale, minSize = 0, gridSize =  input$padding,
                           fontFamily = input$font, fontWeight = 'normal',
                           color = input$wcCol, backgroundColor = "white", #input$wcBGCol,
                           minRotation = 0, maxRotation = input$rotate/10, shuffle = TRUE,
                           rotateRatio = 0.7, shape = input$wcShape, ellipticity = input$ellipticity,
                           widgetsize = NULL, figPath = NULL, hoverFunction = NULL)
  })
  
  output$wordcloud <- wordcloud2::renderWordcloud2({
    WordCloud()
    values$WordCloud
  })
  
  observeEvent(input$reportWC,{
    if(!is.null(values$Words)){
      sheetname <- "WordCloud"
      list_df <- list(values$Words)
      res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
      values$wb <- res$wb
      #values$fileTFP <- screenSh(selector = "#wordcloud") ## screenshot
      values$fileWC <- screenSh(values$WordCloud, zoom = 2, type="plotly")
      values$list_file <- rbind(values$list_file, c(sheetname=res$sheetname,values$fileWC,res$col))
      popUp(title="WordCloud", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  ### TreeMap ----  
  observeEvent(input$TreeMapStop,{
    values$TreeMapremove.terms <- data.frame(stopword=trimws(readStopwordsFile(file=input$TreeMapStop, sep=input$TreeMapSep)))
    values$GenericSL <- values$TreeMapremove.terms
    popUpGeneric(title="Stopword list", 
                 type=NULL, 
                 color=c("#1d8fe1"),
                 subtitle=DTOutput("stopwordList"),
                 btn_labels="OK")
    
  })
  
  observeEvent(input$TreeMapSyn,{
    synonyms <- trimws(readSynWordsFile(file=input$TreeMapSyn, sep=input$TreeMapSynSep))
    term <- unlist(lapply(strsplit(synonyms,";"),function(l){l[1]}))
    synList <- unlist(lapply(strsplit(synonyms,";"),function(l){
      paste0(trimws(l[-1]),collapse=";")
    }))
    values$TreeMapsyn.terms <- data.frame(term=term, synonyms=synList)
    values$GenericSYN <- values$TreeMapsyn.terms
    popUpGeneric(title="Synonym List", 
                 type=NULL, 
                 color=c("#1d8fe1"),
                 subtitle=DTOutput("synonymList"),
                 btn_labels="OK")
    
  })
  
  TreeMap <- eventReactive(input$applyTreeMap,{
    if (input$treeTerms %in% c("TI","AB")){
      ngrams <- as.numeric(input$treeTermsngrams)
    }else{
      ngrams <- 1
    }
    ### load file with terms to remove
    if (input$TreeMapStopFile=="Y"){
      remove.terms <- trimws(values$TreeMapremove.terms$stopword)
    }else{remove.terms <- NULL}
    #values$TreeMapremove.terms <- remove.terms
    ### end of block
    ### load file with synonyms
    if (input$TreeMapSynFile=="Y"){
      synonyms <- values$TreeMapsyn.terms %>% group_by(term) %>% mutate(term=paste0(term,";",synonyms)) %>% select(term)
      synonyms <- synonyms$term
    }else{synonyms <- NULL}
    #values$TreeMapsyn.terms <- synonyms
    ### end of block
    
    resW=wordlist(M=values$M, Field=input$treeTerms, n=input$treen_words, measure="identity", ngrams=ngrams, remove.terms=remove.terms, synonyms = synonyms)
    
    W=resW$W
    values$TreeMap <- plot_ly(
      type='treemap',
      labels=W[,1],
      parents="Tree",
      values= W[,2],
      textinfo="label+value+percent entry",
      domain=list(column=0)) %>% 
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
    
    values$WordsT=resW$Words
    return(resW$Words)
  })
  
  output$treemap <- renderPlotly({
    TreeMap()
    values$TreeMap
  })
  
  
  output$wordTable <- DT::renderDT({
    WordCloud()
    DTformat(values$Words, nrow=10, filename="Most_Frequent_Words", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$treeTable <- DT::renderDT({
    WordsT <- TreeMap()
    DTformat(values$WordsT, nrow=10, filename="Most_Frequent_Words", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  },height = 600, width = 900)
  
  observeEvent(input$reportTREEMAP,{
    if(!is.null(values$WordsT)){
      sheetname <- "TreeMap"
      list_df <- list(values$WordsT)
      res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
      values$wb <- res$wb
      #values$fileTFP <- screenSh(selector = "#treemap") ## screenshot
      values$fileTreeMap <- screenSh(values$TreeMap, zoom = 2, type="plotly")
      values$list_file <- rbind(values$list_file, c(sheetname=res$sheetname,values$fileTreeMap,res$col))
      popUp(title="TreeMap", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  ### Words' Frequency over Time ----   
  observeEvent(input$WDStop,{
    values$WDremove.terms <- data.frame(stopword=trimws(readStopwordsFile(file=input$WDStop, sep=input$WDSep)))
    values$GenericSL <- values$WDremove.terms
    popUpGeneric(title="Stopword list", 
                 type=NULL, 
                 color=c("#1d8fe1"),
                 subtitle=DTOutput("stopwordList"),
                 btn_labels="OK")
    
  })
  
  observeEvent(input$WDSyn,{
    synonyms <- trimws(readSynWordsFile(file=input$WDSyn, sep=input$WDSynSep))
    term <- unlist(lapply(strsplit(synonyms,";"),function(l){l[1]}))
    synList <- unlist(lapply(strsplit(synonyms,";"),function(l){
      paste0(trimws(l[-1]),collapse=";")
    }))
    values$WDsyn.terms <- data.frame(term=term, synonyms=synList)
    values$GenericSYN <- values$WDsyn.terms
    popUpGeneric(title="Synonym List", 
                 type=NULL, 
                 color=c("#1d8fe1"),
                 subtitle=DTOutput("synonymList"),
                 btn_labels="OK")
    
  })
  
  WDynamics <- eventReactive(input$applyWD,{
    if (input$cumTerms=="Cum"){
      cdf=TRUE
      laby="Cumulate occurrences"
    }else{
      cdf=FALSE
      laby="Annual occurrences"}
    
    ### load file with terms to remove
    if (input$WDStopFile=="Y"){
      remove.terms <- trimws(values$WDremove.terms$stopword)
    }else{remove.terms <- NULL}
    #values$WDremove.terms <- remove.terms
    ### end of block
    
    ### load file with synonyms
    if (input$WDSynFile=="Y"){
      synonyms <- values$WDsyn.terms %>% group_by(term) %>% mutate(term=paste0(term,";",synonyms)) %>% select(term)
      synonyms <- synonyms$term
    }else{synonyms <- NULL}
    #values$WDsyn.terms <- synonyms
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
    values$DF=data.frame(Year=rep(values$KW$Year,(dim(values$KW)[2]-1)),Term=term, Freq=freq)
    
    width_scale <- 2.5 * 26 / length(unique(values$DF$Term))
    
    Text <- paste(values$DF$Term," (",values$DF$Year,") ",values$DF$Freq, sep="")
    
    x <- c(max(values$DF$Year)-0.02-diff(range(values$DF$Year))*0.20, max(values$DF$Year)-0.02)-1
    y <- c(min(values$DF$Freq),min(values$DF$Freq)+diff(range(values$DF$Freq))*0.20)
    
    g <- ggplot(values$DF, aes(x=Year,y=Freq, group=Term, color=Term, text = Text))+
      geom_line()+
      labs(x = 'Year'
           , y = laby
           , title = "Words' Frequency over Time") +
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
            ,axis.line.x = element_line(color="black",linewidth=0.5)
            ,axis.line.y = element_line(color="black",linewidth=0.5)
      ) + 
      annotation_custom(values$logoGrid, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
    
    values$WDplot <- g
    return(g)
  })
  
  output$WDplot.save <- downloadHandler(
    filename = function() {
      paste("WordsFrequencyOverTime-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$WDplot, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
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
               'toImage',
               'sendDataToCloud',
               'pan2d', 
               'select2d', 
               'lasso2d',
               'toggleSpikelines',
               'hoverClosestCartesian',
               'hoverCompareCartesian'
             ))  %>%
      layout(hovermode = 'compare')
  })
  
  output$kwGrowthtable <- DT::renderDT({
    g <- WDynamics()
    kwData <- values$KW
    DTformat(kwData, nrow=10, filename="Word_Dynamics", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportWD,{
    if(!is.null(values$KW)){
      list_df <- list(values$KW)
      list_plot <- list(values$WDplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "WordFreqOverTime", wb=values$wb)
      values$wb <- wb
      popUp(title="Words' Frequency over Time", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  ### Trend Topics ----
  
  observeEvent(input$TTStop,{
    values$TTremove.terms <- data.frame(stopword=trimws(readStopwordsFile(file=input$TTStop, sep=input$TTSep)))
    values$GenericSL <- values$TTremove.terms
    popUpGeneric(title="Stopword list", 
                 type=NULL, 
                 color=c("#1d8fe1"),
                 subtitle=DTOutput("stopwordList"),
                 btn_labels="OK")
    
  })
  
  observeEvent(input$TTSyn,{
    synonyms <- trimws(readSynWordsFile(file=input$TTSyn, sep=input$TTSynSep))
    term <- unlist(lapply(strsplit(synonyms,";"),function(l){l[1]}))
    synList <- unlist(lapply(strsplit(synonyms,";"),function(l){
      paste0(trimws(l[-1]),collapse=";")
    }))
    values$TTsyn.terms <- data.frame(term=term, synonyms=synList)
    values$GenericSYN <- values$TTsyn.terms
    popUpGeneric(title="Synonym List", 
                 type=NULL, 
                 color=c("#1d8fe1"),
                 subtitle=DTOutput("synonymList"),
                 btn_labels="OK")
    
  })
  
  output$trendSliderPY <- renderUI({
    
    sliderInput("trendSliderPY", "Timespan", min = min(values$M$PY,na.rm=T),sep="",
                max = max(values$M$PY,na.rm=T), value = c(min(values$M$PY,na.rm=T),max(values$M$PY,na.rm=T)))
  })
  
  TrendTopics <- eventReactive(input$applyTrendTopics,{
    
    ### load file with terms to remove
    if (input$TTStopFile=="Y"){
      remove.terms <- trimws(values$TTremove.terms$stopword)
    }else{remove.terms <- NULL}
    #values$TTremove.terms <- remove.terms
    ### end of block
    
    ### load file with synonyms
    if (input$TTSynFile=="Y"){
      synonyms <- values$TTsyn.terms %>% group_by(term) %>% mutate(term=paste0(term,";",synonyms)) %>% select(term)
      synonyms <- synonyms$term
    }else{synonyms <- NULL}
    #values$TTsyn.terms <- synonyms
    ### end of block
    
    if (input$trendTerms %in% c("TI","AB")){
      values$M=termExtraction(values$M, Field = input$trendTerms, stemming = input$trendStemming, verbose = FALSE, ngrams=as.numeric(input$trendTermsngrams))
      field=paste(input$trendTerms,"_TM",sep="")
    } else {field=input$trendTerms}
    values$trendTopics <- fieldByYear(values$M, field = field, timespan = input$trendSliderPY, min.freq = input$trendMinFreq,
                                      n.items = input$trendNItems, remove.terms = remove.terms, synonyms = synonyms, 
                                      dynamic.plot=TRUE, graph = FALSE)
    values$trendTopics$params <- data.frame(description=c("Textual field", "N. of words per Year"),
                                            value=c(field, input$trendNItems))
    return(values$trendTopics$graph)
  })
  
  # gemini button for word network
  output$trendTopicsGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(values, input$sidebarmenu, input)
    geminiOutput(title = "", content = values$trendTopicsGemini, values)
    
  })
  
  output$TTplot.save <- downloadHandler(
    filename = function() {
      paste("TrendTopics-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$trendTopics$graph, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
    },
    contentType = "png"
  )
  
  output$trendTopicsPlot <- renderPlotly({
    g <- TrendTopics()
    plot.ly(g, flip=TRUE, side="r", size=0.1, aspectratio=1.3)
  })
  
  output$trendTopicsTable <- DT::renderDT({
    TrendTopics()
    tpData=values$trendTopics$df_graph %>% 
      rename(Term = item,
             Frequency = freq,
             "Year (Q1)" = year_q1,
             "Year (Median)" = year_med,
             "Year (Q3)" = year_q3)
    DTformat(tpData, nrow=10, filename="TrendTopic", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportTT,{
    if(!is.null(values$trendTopics$df_graph)){
      list_df <- list(values$trendTopics$df_graph %>% 
                        rename(Term = item,
                               Frequency = freq,
                               "Year (Q1)" = year_q1,
                               "Year (Median)" = year_med,
                               "Year (Q3)" = year_q3))
      list_plot <- list(values$trendTopics$graph)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "TrendTopics", wb=values$wb)
      values$wb <- wb
      popUp(title="Trend Topics", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
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
    values$CM$data <- values$CM$data[,c(1,5,2)]
    values$CM$clusters <- values$CM$clusters[,c(7,1:4,6)]
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
                                 shape=input$coc.shape, net=values$CM$net,shadow=TRUE)
    values$networkCM$VIS
  })
  
  output$CMplot.save <- downloadHandler(
    filename = function() {
      paste("CouplingMap-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$CM$map, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
    },
    contentType = "png"
  )
  
  output$CMTable <- DT::renderDT({
    CMMAP()
    #cmData=values$CM$data[,c(2,1,3,5)]
    cmData <- values$CM$data
    DTformat(cmData, nrow=10, filename="CouplingMap", pagelength=TRUE, left=NULL, right=NULL, numeric=2, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$CMTableCluster <- DT::renderDT({
    CMMAP()
    #cmData=values$CM$clusters[,c(7,1:4,6)]
    cmData <- values$CM$clusters
    DTformat(cmData, nrow=10, filename="CouplingMap_Clusters", pagelength=TRUE, left=NULL, right=NULL, numeric=4:5, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportCM,{
    if(!is.null(values$CM$data)){
      popUp(title=NULL, type="waiting")
      list_df <- list(values$CM$params,
                      values$CM$data,
                      values$CM$clusters)
      list_plot <- list(values$CM$map,
                        values$CM$net$graph)
      wb <- addSheetToReport(list_df, list_plot, sheetname="CouplingMap", wb=values$wb)
      values$wb <- wb
      popUp(title="Coupling Map", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  # CONCEPTUAL STRUCTURE ----
  ### Network approach ----
  #### Co-occurrences network ----
  observeEvent(input$COCStop,{
    values$COCremove.terms <- data.frame(stopword=trimws(readStopwordsFile(file=input$COCStop, sep=input$COCSep)))
    values$GenericSL <- values$COCremove.terms
    popUpGeneric(title="Stopword list", 
                 type=NULL, 
                 color=c("#1d8fe1"),
                 subtitle=DTOutput("stopwordList"),
                 btn_labels="OK")
    
  })
  
  observeEvent(input$COCSyn,{
    synonyms <- trimws(readSynWordsFile(file=input$COCSyn, sep=input$COCSynSep))
    term <- unlist(lapply(strsplit(synonyms,";"),function(l){l[1]}))
    synList <- unlist(lapply(strsplit(synonyms,";"),function(l){
      paste0(trimws(l[-1]),collapse=";")
    }))
    values$COCsyn.terms <- data.frame(term=term, synonyms=synList)
    values$GenericSYN <- values$COCsyn.terms
    popUpGeneric(title="Synonym List", 
                 type=NULL, 
                 color=c("#1d8fe1"),
                 subtitle=DTOutput("synonymList"),
                 btn_labels="OK")
    
  })
  
  COCnetwork <- eventReactive(input$applyCoc,{
    
    values <- cocNetwork(input,values)
    values$COCnetwork<-igraph2vis(g=values$cocnet$graph,curved=(input$coc.curved=="Yes"), 
                               labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                               shape=input$coc.shape, net=values$cocnet, shadow=(input$coc.shadow=="Yes"), edgesize=input$edgesize, noOverlap=input$noOverlap)
    values$cocOverlay <- overlayPlotly(values$COCnetwork$VIS)
    values$degreePlot <- degreePlot(values$cocnet)
  })
  
  output$cocPlot <- renderVisNetwork({  
    COCnetwork()
    values$COCnetwork$VIS
  })
  
  output$cocOverlay <- renderPlotly({
    COCnetwork()
    values$cocOverlay
  })
  
  # gemini button for word network
  output$cocGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(values, input$sidebarmenu, input)
    geminiOutput(title = "", content = values$cocGemini, values)
    
  })
  
  output$network.coc <- downloadHandler(
    filename = function() {
      paste("Co_occurrence_network-", Sys.Date(), ".zip", sep="")
    },
    content <- function(file) {
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      # print(tmpdir)
      #igraph::write.graph(values$obj$graph_pajek,file=file, format="pajek")
      myfile <- paste("mynetwork-", Sys.Date(), sep="")
      files <- paste0(myfile, c(".net",".vec",".clu"))
      graph2Pajek(values$cocnet$graph, filename=myfile)
      # print(files)
      # print(dir())
      zip::zip(file,files)
    },
    contentType = "zip"
  )
  
  ##### save coc network image as html ####
  output$networkCoc.fig <- downloadHandler(
    filename = "network.html",
    content <- function(con) {
      savenetwork(con, values$COCnetwork$VIS)
    },
    contentType = "html"
  )
  
  output$cocTable <- DT::renderDT({
    COCnetwork()
    cocData=values$cocnet$cluster_res
    names(cocData)=c("Node", "Cluster", "Betweenness", "Closeness", "PageRank")
    DTformat(cocData, nrow=10, filename="CoWord_Network", pagelength=TRUE, left=NULL, right=NULL, numeric=3:5, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  ### Degree Plot Co-word analysis ----
  output$cocDegree <- renderPlotly({
    COCnetwork()
    #values$degreePlot <- degreePlot(values$cocnet)
    plot.ly(values$degreePlot)
  })
  
  observeEvent(input$reportCOC,{
    if(!is.null(values$cocnet$cluster_res)){
      names(values$cocnet$cluster_res)=c("Node", "Cluster", "Betweenness", "Closeness", "PageRank")
      sheetname <- "CoWordNet"
      list_df <- list(values$cocnet$cluster_res)
      list_plot <- list(values$degreePlot)
      res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
      #values$wb <- res$wb
      values$wb <- addGgplotsWb(list_plot, wb=res$wb, res$sheetname, col=res$col+16, width=10, height=7, dpi=75)
      #values$fileTFP <- screenSh(selector = "#cocPlot") ## screenshot
      values$fileCOC <- screenSh(values$COCnetwork$VIS, zoom = 2, type="vis")
      values$list_file <- rbind(values$list_file, c(sheetname=res$sheetname,values$fileCOC,res$col))
      popUp(title="Co-occurrence Network", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  ### Correspondence Analysis ----
  observeEvent(input$CSStop,{
    values$CSremove.terms <- data.frame(stopword=trimws(readStopwordsFile(file=input$CSStop, sep=input$CSSep)))
    values$GenericSL <- values$CSremove.terms
    popUpGeneric(title="Stopword list", 
                 type=NULL, 
                 color=c("#1d8fe1"),
                 subtitle=DTOutput("stopwordList"),
                 btn_labels="OK")
    
  })
  
  observeEvent(input$FASyn,{
    synonyms <- trimws(readSynWordsFile(file=input$FASyn, sep=input$FASynSep))
    term <- unlist(lapply(strsplit(synonyms,";"),function(l){l[1]}))
    synList <- unlist(lapply(strsplit(synonyms,";"),function(l){
      paste0(trimws(l[-1]),collapse=";")
    }))
    values$FAsyn.terms <- data.frame(term=term, synonyms=synList)
    values$GenericSYN <- values$FAsyn.terms
    popUpGeneric(title="Synonym List", 
                 type=NULL, 
                 color=c("#1d8fe1"),
                 subtitle=DTOutput("synonymList"),
                 btn_labels="OK")
    
  })
  
  CSfactorial <- eventReactive(input$applyCA,{
    values <- CAmap(input,values)
    values$plotCS <- ca2plotly(values$CS, method=input$method ,dimX = 1, dimY = 2, topWordPlot = Inf, threshold=0.05, labelsize = input$CSlabelsize*2, size=input$CSlabelsize*1.5)
    values$dendCS <- dend2vis(values$CS$km.res, labelsize=input$CSlabelsize, nclusters=as.numeric(input$nClustersCS), community=FALSE)
  })
  
  # gemini button for correspondence analysis
  output$CSGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(values, input$sidebarmenu, input)
    geminiOutput(title = "", content = values$CSGemini, values)
    
  })
  
  output$FAplot.save <- downloadHandler(
    filename = function() {
      #
      paste("FactorialAnalysis_", Sys.Date(), ".zip", sep="")
    },
    content <- function(file) {
      #go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- c(paste("FactorialMap_", Sys.Date(), ".png", sep=""),
                 paste("Dendrogram_", Sys.Date(), ".png", sep="")
                 #paste("MostContribDocuments_", Sys.Date(), ".png", sep=""),
                 #paste("MostCitedDocuments_", Sys.Date(), ".png", sep="")
      )
      ggsave(filename = files[1], plot = values$CS$graph_terms, dpi = values$dpi, height = values$h, width = values$h*1.5, bg="white")
      png(filename = files[2],  height = values$h, width = values$h*2, units="in", res = values$dpi)
          plot(values$CS$graph_dendogram)
      dev.off()
      zip::zip(file,files)
    },
    contentType = "zip"
  )

  output$CSPlot1 <- renderPlotly({
    CSfactorial()
    #CS=values$CS
    #save(CS,file="provaCS.rdata")
    values$plotCS #<- ca2plotly(values$CS, method=input$method ,dimX = 1, dimY = 2, topWordPlot = Inf, threshold=0.05, labelsize = input$CSlabelsize*2, size=input$CSlabelsize*1.5)
  })
  

  output$CSPlot4 <- renderVisNetwork({
    CSfactorial()
      #dend2vis(values$CS$km.res, labelsize=input$CSlabelsize, nclusters=as.numeric(input$nClustersCS), community=FALSE)
      values$dendCS
      #values$CS$graph_dendogram)
  })
  
  output$CSTableW <- DT::renderDT({
    CSfactorial()
    WData <- values$CS$WData
    DTformat(WData, nrow=10, filename="CoWord_Factorial_Analysis_Words_By_Cluster", pagelength=TRUE, left=NULL, right=NULL, numeric=2:3, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$CSTableD <- DT::renderDT({
    CSfactorial()
    CSData <- values$CS$CSData
    DTformat(CSData, nrow=10, filename="CoWord_Factorial_Analysis_Articles_By_Cluster", pagelength=TRUE, left=NULL, right=NULL, numeric=2:4, dom=FALSE, 
             size='100%', filter="none", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  # add to report
  observeEvent(input$reportFA,{
    if(!is.null(values$CS$params)){
      list_df <- list(values$CS$params, values$CS$WData, values$CS$CSData)
      list_plot <- list(values$CS$graph_terms, values$CS$graph_dendogram)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "FactorialAnalysis", wb=values$wb)
      values$wb <- wb
      popUp(title="Factorial Analysis", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  ### Thematic Map ----
  observeEvent(input$TMStop,{
    values$TMremove.terms <- data.frame(stopword=trimws(readStopwordsFile(file=input$TMStop, sep=input$TMSep)))
    values$GenericSL <- values$TMremove.terms
    popUpGeneric(title="Stopword list", 
                 type=NULL, 
                 color=c("#1d8fe1"),
                 subtitle=DTOutput("stopwordList"),
                 btn_labels="OK")
    
  })
  
  observeEvent(input$TMapSyn,{
    synonyms <- trimws(readSynWordsFile(file=input$TMapSyn, sep=input$TMapSynSep))
    term <- unlist(lapply(strsplit(synonyms,";"),function(l){l[1]}))
    synList <- unlist(lapply(strsplit(synonyms,";"),function(l){
      paste0(trimws(l[-1]),collapse=";")
    }))
    values$TMapsyn.terms <- data.frame(term=term, synonyms=synList)
    values$GenericSYN <- values$TMapsyn.terms
    popUpGeneric(title="Synonym List", 
                 type=NULL, 
                 color=c("#1d8fe1"),
                 subtitle=DTOutput("synonymList"),
                 btn_labels="OK")
    
  })
  
  TMAP <- eventReactive(input$applyTM,{
    if (input$TMfield %in% c("TI","AB")){
      ngrams <- as.numeric(input$TMngrams)
    }else{
      ngrams <- 1
    }
    
    ### load file with terms to remove
    if (input$TMStopFile=="Y"){
      remove.terms <- trimws(values$TMremove.terms$stopword)
    }else{remove.terms <- NULL}
    #values$TMremove.terms <- remove.terms
    ### end of block
    
    ### load file with synonyms
    if (input$TMapSynFile=="Y"){
      synonyms <- values$TMapsyn.terms %>% group_by(term) %>% mutate(term=paste0(term,";",synonyms)) %>% select(term)
      synonyms <- synonyms$term
    }else{synonyms <- NULL}
    #values$TMapsyn.terms <- synonyms
    ### end of block
    
    values$TM <- thematicMap(values$M, field=input$TMfield, 
                             n=input$TMn, minfreq=input$TMfreq, ngrams=ngrams,
                             community.repulsion = input$TMrepulsion,
                             stemming=input$TMstemming, size=input$sizeTM, cluster=input$TMCluster,
                             n.labels=input$TMn.labels, repel=FALSE, remove.terms=remove.terms, synonyms=synonyms,
                             subgraphs=TRUE)
    values$TM$doc2clust <- values$TM$documentToClusters %>% select(Assigned_cluster, SR, pagerank) 
    
    values$TM$documentToClusters$DI<- paste0('<a href=\"https://doi.org/',values$TM$documentToClusters$DI,'\" target=\"_blank\">',values$TM$documentToClusters$DI,'</a>')
    names(values$TM$documentToClusters)[1:9] <- c("DOI", "Authors","Title","Source","Year","TotalCitation","TCperYear","NTC","SR") 
    
    values$TM$words <- values$TM$words[,-c(4,6)]
    values$TM$clusters_orig <- values$TM$clusters
    values$TM$clusters <- values$TM$clusters[,c(9,5:8,11)]
    names(values$TM$clusters) <- c("Cluster", "CallonCentrality","CallonDensity","RankCentrality","RankDensity","ClusterFrequency") 
    values$TMmap <- plot.ly(values$TM$map, size=0.07, aspectratio = 1.3, customdata=values$TM$clusters$color)
    validate(
      need(values$TM$nclust > 0, "\n\nNo topics in one or more periods. Please select a different set of parameters.")
    )
  })
  output$TMPlot <- renderPlotly({
    TMAP()
    values$TMmap
  })
  
  # gemini button for Thematic Map
  output$TMGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(values, input$sidebarmenu, input)
    geminiOutput(title = "", content = values$TMGemini, values)
    
  })
  
  ### click cluster networks
  
  plotModal <- function(session) {
    ns <- session$ns
    modalDialog(
      h3(strong(("Cluster Network"))),
      visNetworkOutput(ns("cocPlotClust")),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton(label="Save", inputId = "cocPlotClust",
                     icon = icon("camera", lib = "glyphicon")),
        modalButton("Close")),
    )
  }
  
  observeEvent(input$cocPlotClust,{
    #Time <- format(Sys.time(),'%H%M%S')
    filename = paste("TMClusterGraph-", "_",  gsub(" |:","",Sys.time()), ".png", sep="")
    screenShot(values$plotClust , filename=filename, type="vis")
  })
  
  observeEvent(event_data("plotly_click"), {
    if (input$sidebarmenu=="thematicMap"){
      showModal(plotModal(session))
    }
  })
  
  output$cocPlotClust <- renderVisNetwork({
    values$d <- event_data("plotly_click")
    coord <- values$d[c("x","y")]
    color <- values$TM$clusters_orig %>% 
      filter(rcentrality==coord$x,rdensity==coord$y) %>% 
      select(color) %>% as.character()
    g <- values$TM$subgraphs[[color]]
    values$plotClust <- igraph2visClust(g,curved=F,labelsize=4,opacity=0.5,shape="dot", shadow=TRUE, edgesize=5)$VIS
    values$plotClust 
  })
  
  ### end click cluster subgraphs
  
  output$NetPlot <- renderVisNetwork({
    TMAP()
    values$networkTM<-igraph2vis(g=values$TM$net$graph,curved=(input$coc.curved=="Yes"), 
                                 labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                 shape=input$coc.shape, net=values$TM$net, noOverlap=input$noOverlapTM)
    values$networkTM$VIS
  })
  
  
  output$TMplot.save <- downloadHandler(
    filename = function() {
      paste("ThematicMap-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      ggsave(filename = file, plot = values$TM$map, dpi = values$dpi, height = values$h, width = values$h*1.5, bg="white")
    },
    contentType = "png"
  )
  
  output$TMTable <- DT::renderDT({
    TMAP()
    tmData=values$TM$words
    DTformat(tmData, nrow=10, filename="Thematic_Map_Terms", pagelength=TRUE, left=NULL, right=NULL, numeric=5:7, dom=FALSE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$TMTableCluster <- DT::renderDT({
    TMAP()
    tmData <- values$TM$clusters
    DTformat(tmData, nrow=10, filename="Thematic_Map_Clusters", pagelength=TRUE, left=NULL, right=NULL, numeric=2:3, dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$TMTableDocument <- DT::renderDT({
    TMAP()
    tmDataDoc <- values$TM$documentToClusters
    DTformat(tmDataDoc, nrow=10, filename="Thematic_Map_Documents", pagelength=TRUE, left=NULL, right=NULL, numeric=c(7:8,10:(ncol(tmDataDoc)-2),ncol(tmDataDoc)), dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportTM,{
    if(!is.null(values$TM$words)){
      popUp(title=NULL, type="waiting")
      list_df <- list(values$TM$params,
                      values$TM$words,
                      values$TM$clusters,
                      values$TM$documentToClusters)
      list_plot <- list(values$TM$map,
                        values$TM$net$graph)
      wb <- addSheetToReport(list_df, list_plot, sheetname="ThematicMap", wb=values$wb)
      values$wb <- wb
      popUp(title="Thematic Map", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  ### Thematic Evolution ----
  observeEvent(input$TEStop,{
    values$TEremove.terms <- data.frame(stopword=trimws(readStopwordsFile(file=input$TEStop, sep=input$TESep)))
    values$GenericSL <- values$TEremove.terms
    popUpGeneric(title="Stopword list", 
                 type=NULL, 
                 color=c("#1d8fe1"),
                 subtitle=DTOutput("stopwordList"),
                 btn_labels="OK")
    
  })
  
  observeEvent(input$TESyn,{
    synonyms <- trimws(readSynWordsFile(file=input$TESyn, sep=input$TESynSep))
    term <- unlist(lapply(strsplit(synonyms,";"),function(l){l[1]}))
    synList <- unlist(lapply(strsplit(synonyms,";"),function(l){
      paste0(trimws(l[-1]),collapse=";")
    }))
    values$TEsyn.terms <- data.frame(term=term, synonyms=synList)
    values$GenericSYN <- values$TEsyn.terms
    popUpGeneric(title="Synonym List", 
                 type=NULL, 
                 color=c("#1d8fe1"),
                 subtitle=DTOutput("synonymList"),
                 btn_labels="OK")
    
  })
  
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
      remove.terms <- trimws(values$TEremove.terms$stopword)
    }else{remove.terms <- NULL}
    #values$TEremove.terms <- remove.terms
    ### end of block
    
    ### load file with synonyms
    if (input$TESynFile=="Y"){
      synonyms <- values$TEsyn.terms %>% group_by(term) %>% mutate(term=paste0(term,";",synonyms)) %>% select(term)
      synonyms <- synonyms$term
    }else{synonyms <- NULL}
    #values$TEsyn.terms <- synonyms
    ### end of block
    
    values$yearSlices <- as.numeric()
    if (is.null(input$numSlices)){
      values$yearSlices <- median(values$M$PY, na.rm=TRUE)
    }else{
      for (i in 1:as.integer(input$numSlices)){
        if (length(input[[paste0("Slice", i)]])>0){values$yearSlices <- c(values$yearSlices,input[[paste0("Slice", i)]])}
      }
    }
    
    if (length(values$yearSlices)>0){
      values$nexus <- thematicEvolution(values$M, field=input$TEfield, values$yearSlices, n = input$nTE, minFreq = input$fTE, size = input$sizeTE, 
                                        cluster=input$TECluster,
                                        n.labels=input$TEn.labels, repel=FALSE, ngrams=ngrams, remove.terms = remove.terms, synonyms = synonyms)
      validate(
        need(values$nexus$check != FALSE, "\n\nNo topics in one or more periods. Please select a different set of parameters.")
      )
      for (i in 1:(length(values$yearSlices)+1)){
        values$nexus$TM[[i]]$words <- values$nexus$TM[[i]]$words[,-c(4,6)]
        values$nexus$TM[[i]]$clusters <- values$nexus$TM[[i]]$clusters[,c(9,5:8,11)]
        names(values$nexus$TM[[i]]$clusters) <- c("Cluster", "CallonCentrality","CallonDensity","RankCentrality","RankDensity","ClusterFrequency")
        
        values$nexus$TM[[i]]$documentToClusters$DI<- paste0('<a href=\"https://doi.org/',values$nexus$TM[[i]]$documentToClusters$DI,'\" target=\"_blank\">',values$nexus$TM[[i]]$documentToClusters$DI,'</a>')
        names(values$nexus$TM[[i]]$documentToClusters)[1:9] <- c("DOI", "Authors","Title","Source","Year","TotalCitation","TCperYear","NTC","SR")
      }
      values$nexus$Data <- values$nexus$Data[values$nexus$Data$Inc_index>0,-c(4,8)]
      values$TEplot <- plotThematicEvolution(Nodes = values$nexus$Nodes,Edges = values$nexus$Edges, measure = input$TEmeasure, min.flow = input$minFlowTE, label_size = input$sizeTE*20)
    }
  })
  
  output$TEPlot <- visNetwork::renderVisNetwork({
    TEMAP()
    values$TEplot
  })
  
  # gemini button for word network
  output$TEGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(values, input$sidebarmenu, input)
    geminiOutput(title = "", content = values$TEGemini, values)
    
  })
  
  session$onFlushed(function() {
    shinyjs::runjs("$('#TEPlot').trigger('resize');")
  })
  
  output$TEplot.save <- downloadHandler(
    filename = function() {
      #
      paste("ThematicEvolution-", Sys.Date(), ".zip", sep="")
    },
    content <- function(file) {
      #go to a temp dir to avoid permission issues
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      files <- filenameTE <- paste("ThematicEvolution_", Sys.Date(), ".png", sep="")
      
      for (i in 1:length(values$nexus$TM)){
        fileName <- paste("ThematicEvolution-Map_",i,"_",Sys.Date(), ".png", sep="")
        ggsave(filename = fileName, plot = values$nexus$TM[[i]]$map, dpi = values$dpi, height = values$h, width = values$h*1.5, bg="white")
        files <- c(fileName,files)
      }
      plot2png(values$TEplot, filename= filenameTE, 
               zoom = 2, type="vis", tmpdir=tmpdir)
      zip::zip(file,files)
    },
    contentType = "zip"
  )
  
  output$TETable <- DT::renderDT({
    TEMAP()
    TEData=values$nexus$Data
    names(TEData)=c("From", "To", "Words", "Weighted Inclusion Index", "Inclusion Index", "Occurrences", "Stability Index")
    DTformat(TEData, nrow=10, filename="Thematic_Evolution", pagelength=TRUE, left=NULL, right=NULL, numeric=c(4,5,7), dom=TRUE, 
             size='85%', filter="top", columnShort=NULL, columnSmall=NULL, round=2, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
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
                                shape=input$coc.shape, net=values$nexus$Net[[k]], noOverlap=input$noOverlapTE)
    values$network1$VIS
  })
  
  output$NetPlot2 <- renderVisNetwork({
    TEMAP()
    k=2
    values$network2<-igraph2vis(g=values$nexus$Net[[k]]$graph,curved=(input$coc.curved=="Yes"), 
                                labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                shape=input$coc.shape, net=values$nexus$Net[[k]], noOverlap=input$noOverlapTE)
    values$network2$VIS
  })
  
  output$NetPlot3 <- renderVisNetwork({
    TEMAP()
    k=3
    values$network3<-igraph2vis(g=values$nexus$Net[[k]]$graph,curved=(input$coc.curved=="Yes"), 
                                labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                shape=input$coc.shape, net=values$nexus$Net[[k]], noOverlap=input$noOverlapTE)
    values$network3$VIS
  })
  
  output$NetPlot4 <- renderVisNetwork({
    TEMAP()
    k=4
    values$network4<-igraph2vis(g=values$nexus$Net[[k]]$graph,curved=(input$coc.curved=="Yes"), 
                                labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                shape=input$coc.shape, net=values$nexus$Net[[k]], noOverlap=input$noOverlapTE)
    values$network4$VIS
  })
  
  output$NetPlot5 <- renderVisNetwork({
    TEMAP()
    k=5
    values$network5<-igraph2vis(g=values$nexus$Net[[k]]$graph,curved=(input$coc.curved=="Yes"), 
                                labelsize=input$labelsize, opacity=input$cocAlpha,type=input$layout,
                                shape=input$coc.shape, net=values$nexus$Net[[k]], noOverlap=input$noOverlapTE)
    values$network5$VIS
  })
  
  output$TMTable1 <- DT::renderDT({
    TEMAP()
    tmData=values$nexus$TM[[1]]$words
    DTformat(tmData, nrow=10, filename="Thematic_Map_Period_1_Terms", pagelength=TRUE, left=NULL, right=NULL, numeric=5:7, dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$TMTable2 <- DT::renderDT({
    TEMAP()
    tmData=values$nexus$TM[[2]]$words
    DTformat(tmData, nrow=10, filename="Thematic_Map_Period_2_Terms", pagelength=TRUE, left=NULL, right=NULL, numeric=5:7, dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$TMTable3 <- DT::renderDT({
    TEMAP()
    tmData=values$nexus$TM[[3]]$words
    DTformat(tmData, nrow=10, filename="Thematic_Map_Period_3_Terms", pagelength=TRUE, left=NULL, right=NULL, numeric=5:7, dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$TMTable4 <- DT::renderDT({
    TEMAP()
    tmData=values$nexus$TM[[4]]$words
    DTformat(tmData, nrow=10, filename="Thematic_Map_Period_4_Terms", pagelength=TRUE, left=NULL, right=NULL, numeric=5:7, dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$TMTable5 <- DT::renderDT({
    TEMAP()
    tmData=values$nexus$TM[[5]]$words
    DTformat(tmData, nrow=10, filename="Thematic_Map_Period_5_Terms", pagelength=TRUE, left=NULL, right=NULL, numeric=5:7, dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$TMTableCluster1 <- DT::renderDT({
    TEMAP()
    tmData <- values$nexus$TM[[1]]$clusters
    DTformat(tmData, nrow=10, filename="Thematic_Map_Period_1_Clusters", pagelength=TRUE, left=NULL, right=NULL, numeric=2:3, dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$TMTableCluster2 <- DT::renderDT({
    TEMAP()
    tmData <- values$nexus$TM[[2]]$clusters
    DTformat(tmData, nrow=10, filename="Thematic_Map_Period_2_Clusters", pagelength=TRUE, left=NULL, right=NULL, numeric=2:3, dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$TMTableCluster3 <- DT::renderDT({
    TEMAP()
    tmData <- values$nexus$TM[[3]]$clusters
    DTformat(tmData, nrow=10, filename="Thematic_Map_Period_3_Clusters", pagelength=TRUE, left=NULL, right=NULL, numeric=2:3, dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$TMTableCluster4 <- DT::renderDT({
    TEMAP()
    tmData <- values$nexus$TM[[4]]$clusters
    DTformat(tmData, nrow=10, filename="Thematic_Map_Period_4_Clusters", pagelength=TRUE, left=NULL, right=NULL, numeric=2:3, dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE) 
  })
  
  output$TMTableCluster5 <- DT::renderDT({
    TEMAP()
    tmData <- values$nexus$TM[[5]]$clusters
    DTformat(tmData, nrow=10, filename="Thematic_Map_Period_5_Clusters", pagelength=TRUE, left=NULL, right=NULL, numeric=2:3, dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE) 
  })
  
  output$TMTableDocument1 <- DT::renderDT(server=TRUE,{
    TEMAP()
    tmDataDoc <- values$nexus$TM[[1]]$documentToClusters
    DTformat(tmDataDoc, nrow=10, filename="Thematic_Map_Period_1_Documents", pagelength=TRUE, left=NULL, right=NULL, numeric=c(7:8,10:(ncol(tmDataDoc)-2),ncol(tmDataDoc)), dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$TMTableDocument2 <- DT::renderDT({
    TEMAP()
    tmDataDoc <- values$nexus$TM[[2]]$documentToClusters
    tmDataDoc$DI<- paste0('<a href=\"https://doi.org/',tmDataDoc$DI,'\" target=\"_blank\">',tmDataDoc$DI,'</a>')
    names(tmDataDoc)[1:9] <- c("DOI", "Authors","Title","Source","Year","TotalCitation","TCperYear","NTC","SR") 
    DTformat(tmDataDoc, nrow=10, filename="Thematic_Map_Period_2_Documents", pagelength=TRUE, left=NULL, right=NULL, numeric=c(7:8,10:(ncol(tmDataDoc)-2),ncol(tmDataDoc)), dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$TMTableDocument3 <- DT::renderDT({
    TEMAP()
    tmDataDoc <- values$nexus$TM[[3]]$documentToClusters
    tmDataDoc$DI<- paste0('<a href=\"https://doi.org/',tmDataDoc$DI,'\" target=\"_blank\">',tmDataDoc$DI,'</a>')
    names(tmDataDoc)[1:9] <- c("DOI", "Authors","Title","Source","Year","TotalCitation","TCperYear","NTC","SR") 
    
    DTformat(tmDataDoc, nrow=10, filename="Thematic_Map_Period_3_Documents", pagelength=TRUE, left=NULL, right=NULL, numeric=c(7:8,10:(ncol(tmDataDoc)-2),ncol(tmDataDoc)), dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$TMTableDocument4 <- DT::renderDT({
    TEMAP()
    tmDataDoc <- values$nexus$TM[[4]]$documentToClusters
    tmDataDoc$DI<- paste0('<a href=\"https://doi.org/',tmDataDoc$DI,'\" target=\"_blank\">',tmDataDoc$DI,'</a>')
    names(tmDataDoc)[1:9] <- c("DOI", "Authors","Title","Source","Year","TotalCitation","TCperYear","NTC","SR") 
    
    DTformat(tmDataDoc, nrow=10, filename="Thematic_Map_Period_4_Documents", pagelength=TRUE, left=NULL, right=NULL, numeric=c(7:8,10:(ncol(tmDataDoc)-2),ncol(tmDataDoc)), dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  output$TMTableDocument5 <- DT::renderDT({
    TEMAP()
    tmDataDoc <- values$nexus$TM[[5]]$documentToClusters
    tmDataDoc$DI<- paste0('<a href=\"https://doi.org/',tmDataDoc$DI,'\" target=\"_blank\">',tmDataDoc$DI,'</a>')
    names(tmDataDoc)[1:9] <- c("DOI", "Authors","Title","Source","Year","TotalCitation","TCperYear","NTC","SR") 
    
    DTformat(tmDataDoc, nrow=10, filename="Thematic_Map_Period_5_Documents", pagelength=TRUE, left=NULL, right=NULL, numeric=c(7:8,10:(ncol(tmDataDoc)-2),ncol(tmDataDoc)), dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  observeEvent(input$reportTE,{
    if(!is.null(values$nexus$Data)){
      popUp(title=NULL, type="waiting")
      sheetname <- "ThematicEvolution"
      list_df <- list(values$nexus$params, values$nexus$Data)
      res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
      #values$wb <- res$wb
      #values$fileTFP <- screenSh(selector = "#TEPlot") ## screenshot
      values$fileTEplot <- screenSh(values$TEplot, zoom = 2, type="plotly")
      values$list_file <- rbind(values$list_file, c(sheetname=res$sheetname,values$fileTEplot,res$col))
      
      ## Periods
      L <- length(values$nexus$TM)
      wb <- res$wb
      for (l in 1:L){
        if(!is.null(values$nexus$TM[[l]]$words)){
          
          list_df <- list(values$nexus$TM[[l]]$params,
                          values$nexus$TM[[l]]$words,
                          values$nexus$TM[[l]]$clusters,
                          values$nexus$TM[[l]]$documentToClusters)
          list_plot <- list(values$nexus$TM[[l]]$map,
                            values$nexus$TM[[l]]$net$graph)
          wb <- addSheetToReport(list_df, list_plot, sheetname=paste("TE_Period_",l,sep=""), wb=wb)
          #
        }
      }
      values$wb <- wb
      popUp(title="Thematic Evolution", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  # INTELLECTUAL STRUCTURE ####
  ### Co-citation network ----
  COCITnetwork <- eventReactive(input$applyCocit,{
    values <- intellectualStructure(input,values)
    values$COCITnetwork<-igraph2vis(g=values$cocitnet$graph,curved=(input$cocit.curved=="Yes"), 
                               labelsize=input$citlabelsize, opacity=0.7,type=input$citlayout,
                               shape=input$cocit.shape, net=values$cocitnet, shadow=(input$cocit.shadow=="Yes"),
                               noOverlap=input$citNoOverlap)
    values$cocitOverlay <- overlayPlotly(values$COCITnetwork$VIS)
    values$degreePlot <- degreePlot(values$cocitnet)
  })
  
  output$cocitPlot <- renderVisNetwork({  
    COCITnetwork()
    isolate(values$COCITnetwork$VIS)
  })
  
  output$cocitOverlay <- renderPlotly({
    COCITnetwork()
    values$cocitOverlay
  })
  
  # gemini button for co-citation network
  output$cocitGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(values, input$sidebarmenu, input)
    geminiOutput(title = "", content = values$cocitGemini, values)
    
  })
  
  output$network.cocit <- downloadHandler(
    filename = function() {
      paste("Co_citation_network-", Sys.Date(), ".zip", sep="")
    },
    content <- function(file) {
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      # print(tmpdir)
      #igraph::write.graph(values$obj$graph_pajek,file=file, format="pajek")
      myfile <- paste("mynetwork-", Sys.Date(), sep="")
      files <- paste0(myfile, c(".net",".vec",".clu"))
      graph2Pajek(values$cocitnet$graph, filename=myfile)
      # print(files)
      # print(dir())
      zip::zip(file,files)
    },
    contentType = "zip"
  )
  
  output$cocitTable <- DT::renderDT({
    COCITnetwork()
    cocitData=values$cocitnet$cluster_res
    names(cocitData)=c("Node", "Cluster", "Betweenness", "Closeness", "PageRank")
    DTformat(cocitData, nrow=10, filename="CoCitation_Network", pagelength=TRUE, left=NULL, right=NULL, numeric=3:5, dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  #### save coc network image as html ----
  output$networkCocit.fig <- downloadHandler(
    filename = "network.html",
    content <- function(con) {
      savenetwork(con, values$COCITnetwork$VIS)
    },
    contentType = "html"
  )
  
  ### Degree Plot Co-citation analysis ####
  output$cocitDegree <- renderPlotly({
    COCITnetwork()
    #p <- degreePlot(values$cocitnet)
    plot.ly(values$degreePlot)
  })
  
  observeEvent(input$reportCOCIT,{
    
    if(!is.null(values$cocitnet$cluster_res)){
      names(values$cocitnet$cluster_res) <- c("Node", "Cluster", "Betweenness", "Closeness", "PageRank")
      sheetname <- "CoCitNet"
      list_df <- list(values$cocitnet$cluster_res)
      list_plot <- list(values$degreePlot)
      res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
      #values$wb <- res$wb
      values$wb <- addGgplotsWb(list_plot, wb=res$wb, res$sheetname, col=res$col+15, width=12, height=8, dpi=75)
      #values$fileTFP <- screenSh(selector = "#cocitPlot") ## screenshot
      values$fileCOCIT <- screenSh(values$COCITnetwork$VIS, zoom = 2, type="vis")
      values$list_file <- rbind(values$list_file, c(sheetname=res$sheetname,values$fileCOCIT,res$col))
      popUp(title="Co-citation Network", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  ### Historiograph ----
  Hist <- eventReactive(input$applyHist,{
    withProgress(message = 'Calculation in progress',
                 value = 0, {
                   values <- historiograph(input,values)
                 })
  })
  
  output$histPlotVis <- renderVisNetwork({  
    g <- Hist()
    values$histPlotVis<-hist2vis(values$histPlot,curved=TRUE, 
                                 labelsize=input$histlabelsize, 
                                 nodesize=input$histsize,
                                 opacity=0.7,
                                 shape="dot",
                                 labeltype=input$titlelabel,
                                 timeline=FALSE)
    values$histPlotVis$VIS
  })
  
  output$histTable <- DT::renderDT({
    g <- Hist()
    Data <- values$histResults$histData
    DTformat(Data, nrow=10, filename="Historiograph_Network", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })
  
  # gemini button for word network
  output$histGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(values, input$sidebarmenu, input)
    geminiOutput(title = "", content = values$histGemini, values)
    
  })
  
  observeEvent(input$reportHIST,{
    if(!is.null(values$histResults$histData)){
      sheetname <- "Historiograph"
      list_df <- list(values$histResults$histData)
      res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
      #values$fileTFP <- screenSh(selector = "#histPlotVis") ## screenshot
      values$fileHIST <- screenSh(values$histPlotVis$VIS, zoom = 2, type="vis")
      values$list_file <- rbind(values$list_file, c(sheetname=res$sheetname,values$fileHIST,res$col))
      popUp(title="Historiograph", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  # SOCIAL STRUCTURE ####
  ### Collaboration network ----
  COLnetwork <- eventReactive(input$applyCol,{
    values <- socialStructure(input,values)
    values$COLnetwork<-igraph2vis(g=values$colnet$graph,curved=(input$soc.curved=="Yes"), 
                               labelsize=input$collabelsize, opacity=input$colAlpha,type=input$collayout,
                               shape=input$col.shape, net=values$colnet, shadow=(input$col.shadow=="Yes"),
                               noOverlap=input$colNoOverlap)
    values$colOverlay <- overlayPlotly(values$COLnetwork$VIS)
    values$degreePlot <-degreePlot(values$colnet)
    if (is.null(dim(values$colnet$cluster_res))){
      values$colnet$cluster_res <- data.frame(Node=NA, Cluster=NA, Betweenness=NA, 
                                                                             Closeness = NA, PageRank = NA)
    }else{
      names(values$colnet$cluster_res) <- c("Node", "Cluster", "Betweenness", "Closeness", "PageRank")
    }
    
  })
  output$colPlot <- renderVisNetwork({  
    COLnetwork()
    values$COLnetwork$VIS
  })
  
  output$colOverlay <- renderPlotly({
    COLnetwork()
    values$colOverlay
  })
  
  # gemini button for word network
  output$colGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(values, input$sidebarmenu, input)
    geminiOutput(title = "", content = values$colGemini, values)
    
  })
  
  output$network.col <- downloadHandler(
    filename = function() {
      paste("Collaboration_network-", Sys.Date(), ".zip", sep="")
    },
    content <- function(file) {
      tmpdir <- tempdir()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      # print(tmpdir)
      #igraph::write.graph(values$obj$graph_pajek,file=file, format="pajek")
      myfile <- paste("mynetwork-", Sys.Date(), sep="")
      files <- paste0(myfile, c(".net",".vec",".clu"))
      graph2Pajek(values$colnet$graph, filename=myfile)
      # print(files)
      # print(dir())
      zip::zip(file,files)
    },
    contentType = "zip"
  )
  
  output$colTable <- DT::renderDT({
    COLnetwork()
    colData=values$colnet$cluster_res
    DTformat(colData, nrow=10, filename="Collaboration_Network", pagelength=TRUE, left=NULL, right=NULL, numeric=3:5, dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  })   
  
  #### save coc network image as html ####
  output$networkCol.fig <- downloadHandler(
    filename = "network.html",
    content <- function(con) {
      savenetwork(con, values$COLnetwork$VIS)
    },
    contentType = "html"
  )
  
  ### Degree Plot Collaboration analysis ####
  output$colDegree <- renderPlotly({
    COLnetwork()
    p <- degreePlot(values$colnet)
    plot.ly(p)
  })
  
  observeEvent(input$reportCOL,{
    if(!is.null(values$colnet$cluster_res)){
      sheetname <- "CollabNet"
      list_df <- list(values$colnet$params, values$colnet$cluster_res)
      list_plot <- list(values$degreePlot)
      res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
      values$wb <- addGgplotsWb(list_plot, wb=res$wb, res$sheetname, col=res$col+15, width=12, height=8, dpi=75)
      #values$fileTFP <- screenSh(selector = "#colPlot") ## screenshot
      values$fileCOL <- screenSh(values$COLnetwork$VIS, zoom = 2, type="vis")
      values$list_file <- rbind(values$list_file, c(sheetname=res$sheetname,values$fileCOL,res$col))
      popUp(title="Collaboration Network", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  ### WPPlot ----
  WMnetwork<- eventReactive(input$applyWM,{
    values$WMmap <- countrycollaboration(values$M,label=FALSE,edgesize=input$WMedgesize/2,min.edges=input$WMedges.min, values)
    values$WMmap$tab <- values$WMmap$tab[,c(1,2,9)]
    names(values$WMmap$tab)=c("From","To","Frequency")
    
  })
  
  # gemini button for World Map
  output$WMGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(values, input$sidebarmenu, input)
    geminiOutput(title = "", content = values$WMGemini, values)
    
  })
  
  output$CCplot.save <- downloadHandler(
    filename = function() {
      paste("CountryCollaborationMap-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      g <- values$WMmap$g + labs(title = "Country Collaboration Map")
      ggsave(filename = file, plot = g, dpi = values$dpi, height = values$h, width = values$h*2, bg="white")
    },
    contentType = "png"
  )
  
  output$WMPlot<- renderPlotly({
    WMnetwork()  
    plot.ly(values$WMmap$g,flip=FALSE, side="r", aspectratio=1.7, size=0.07, data.type=1,height=15)
    #plot(values$WMmap$g)
  })
  
  output$WMTable <- DT::renderDT({
    WMnetwork()  
    colData=values$WMmap$tab
    DTformat(colData, nrow=10, filename="Collaboration_WorldMap", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=TRUE, 
             size='100%', filter="top", columnShort=NULL, columnSmall=NULL, round=3, title="", button=TRUE, escape=FALSE, 
             selection=FALSE)
  }) 
  
  observeEvent(input$reportCOLW,{
    if(!is.null(values$WMmap$tab)){
      list_df <- list(values$WMmap$tab)
      list_plot <- list(values$WMmap$g)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "CollabWorldMap", wb=values$wb)
      values$wb <- wb
      popUp(title="Countries' Collaboration World Map", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })
  
  # REPORT ----
  ### Report Save xlsx ----
  output$report.save <- downloadHandler(
    filename = function() {
      paste("BiblioshinyReport-", Sys.Date(), ".xlsx", sep="")
    },
    content <- function(file) {
      wb_export <- copyWorkbook(values$wb)
      if (nrow(values$list_file)>0){
        wb_export <- addScreenWb(df=values$list_file, wb=wb_export)#, width=10, height=7, dpi=300)
      }
      sheetToRemove <- setdiff(sheets(wb_export),input$reportSheets)
      if (length(sheetToRemove)>0) for (i in sheetToRemove) removeWorksheet(wb_export,i)
      sheetToAdd <- sheets(wb_export)
      for (i in sheetToAdd) setColWidths(wb_export,sheet=i,cols=1,widths = 30, hidden = FALSE)
      openxlsx::saveWorkbook(wb_export, file = file)
    },
    contentType = "xlsx"
  )
  
  ### Report UI elements
  observe({
    output$reportSheets <- renderUI({
      prettyCheckboxGroup(
        inputId = "reportSheets",
        label = NULL, #short2long(df=values$dfLabel, myC=values$myChoices),
        choices = short2long(df=values$dfLabel, myC=values$myChoices),
        selected = values$myChoices,
        icon = icon("check"),
        animation = "pulse",
        status = "primary",
        bigger = T,
        fill = TRUE
      )
    })
  })
  
  observe({
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(), 
      inputId = "reportSheets",
      #label = short2long(df=values$dfLabel, myC=values$myChoices),
      choices = short2long(df=values$dfLabel, myC=values$myChoices),
      selected = if(!input$noSheets) values$myChoices,
      prettyOptions = list(
        animation = "pulse",
        status = "info",
        bigger = T
      )
    )
  })
  
  observe({
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(), 
      inputId = "reportSheets",
      choices = short2long(df=values$dfLabel, myC=values$myChoices),
      selected = if(input$allSheets) values$myChoices,
      prettyOptions = list(
        animation = "pulse",
        status = "info",
        bigger = T
      )
    )
  })
  
  observeEvent(input$deleteAll, {
    ask_confirmation(
      inputId = "delete_confirmation",
      title = "Want to confirm?",
      text = "All the results will be removed from the report",
      type = "warning",
      btn_labels = c("CANCEL", "CONFIRM"),
    )
  })
  
  observeEvent(input$delete_confirmation, {
    if (isTRUE(input$delete_confirmation)) {
      values$myChoices <- "Empty Report"
      values$list_file <- data.frame(sheet=NULL,file=NULL,n=NULL) 
      values$wb <-  openxlsx::createWorkbook()
    }
  }, ignoreNULL = TRUE
  )
  
  ### screenshot buttons ----
  observeEvent(input$screenTFP,{
    filename = paste("ThreeFieldPlot-", "_",  gsub(" |:","",Sys.time()), ".png", sep="")
    screenShot(values$TFP, filename=filename, type="plotly")
  })
  
  observeEvent(input$screenWC,{
    filename = paste("WordCloud-", "_",  gsub(" |:","",Sys.time()), ".png", sep="")
    screenShot(values$WordCloud, filename=filename, type="plotly")
  })
  
  observeEvent(input$screenTREEMAP,{
    filename = paste("TreeMap-", "_",  gsub(" |:","",Sys.time()), ".png", sep="")
    screenShot(values$TreeMap, filename=filename, type="plotly")
  })
  
  observeEvent(input$screenCOC,{
    filename = paste("Co_occurrenceNetwork-", "_",  gsub(" |:","",Sys.time()), ".png", sep="")
    screenShot(values$COCnetwork$VIS, filename=filename, type="vis")
  })
  
  observeEvent(input$screenCOCIT,{
    filename = paste("Co_citationNetwork-", "_",  gsub(" |:","",Sys.time()), ".png", sep="")
    screenShot(values$COCITnetwork$VIS, filename=filename, type="vis")
  })
  
  observeEvent(input$screenHIST,{
    filename = paste("Historiograph-", "_",  gsub(" |:","",Sys.time()), ".png", sep="")
    screenShot(values$histPlotVis$VIS, filename=filename, type="vis")
  })
  
  observeEvent(input$screenCOL,{
    filename = paste("Collaboration_Network-", "_",  gsub(" |:","",Sys.time()), ".png", sep="")
    screenShot(values$COLnetwork$VIS, filename=filename, type="vis")
  })
  
  ## TALL EXPORT ----
  observe({
    req(values$M)
    ind <- which(values$corpusCol %in% names(values$M))
    corpusCol <- values$corpusCol[ind]
    updateMultiInput(session, 'tallFields',
                     selected = character(0),
                     choices = names(corpusCol))
  })
  
  observe({
    req(values$M)
    ind <- which(values$metadataCol %in% names(values$M))
    metadataCol <- values$metadataCol[ind]
    updateMultiInput(session, 'tallMetadata',
                     selected = character(0),
                     choices = names(metadataCol))
  })
  
  observeEvent(eventExpr = {input$tallRun},
               handlerExpr = {
                 req(input$tallFields)
    values$tallDf <- tallExport(values$M, input$tallFields, input$tallMetadata, values$metadataCol)
    
  })
  
  output$tallTable <- renderDT({
    req(values$tallDf)
      DTformat(values$tallDf, nrow=3, filename="tallDf", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=FALSE, size='70%', filter="none",
               columnShort=NULL, columnSmall=NULL, round=2, title="", button=FALSE, escape=FALSE, selection=FALSE,scrollX=TRUE)
  })
  
  output$tall.save <- downloadHandler(
    filename = function() {
      paste("tallFile-", Sys.Date(), ".csv", sep="")
    },
    content <- function(file) {
      write.csv(values$tallDf, file=file, row.names = FALSE)
    },
    contentType = "csv"
  )
  
  observeEvent(eventExpr = {values$TALLmissing},
               handlerExpr = {
                 if (values$TALLmissing){
                   output$tallBttn1 <- renderUI({
                     div(style ="border-radius: 10px; border-width: 3px; font-size: 15px;",
                         align = "center",
                         width="100%",
                         actionBttn(inputId = "installTall", label = strong("Install TALL"),
                                    width = "100%", style = "pill", color = "danger",
                                    icon = icon(name ="cloud-download", lib="glyphicon"))
                     )
                   })
                   output$tallBttn2 <- renderUI("")
                 } else {
                   output$tallBttn1 <- renderUI("")
                   output$tallBttn2 <- renderUI({
                     if (require("tall", quietly = TRUE)){
                       div(style ="border-radius: 10px; border-width: 3px; font-size: 15px;",
                           align = "center",
                           width="100%",
                           actionBttn(inputId = "launchTall", label = strong("Launch TALL"),
                                      width = "100%", style = "pill", color = "primary",
                                      icon = icon(name ="play", lib="glyphicon")))
                     }
                   })
                 }
  })
  
  
  observeEvent(eventExpr = {input$installTall},
               handlerExpr = {
                 pak::pkg_install("tall")
                 popUpGeneric(title=NULL, type="success", color=c("#1d8fe1"),
                              subtitle="TALL has been successfully installed",
                              btn_labels="OK", size="40%")
                 if (suppressMessages(require("tall", quietly = TRUE))) values$TALLmissing <- FALSE
               })

  observeEvent(eventExpr = {input$launchTall},
               handlerExpr = {
                 system('Rscript -e "tall::tall()"')
               })
  
  ## SETTING ----
  observeEvent(input$dpi, {
    values$dpi <- as.numeric(input$dpi)
  })
  
  observeEvent(input$h,{
    values$h <- as.numeric(input$h)
  })
  
  output$apiStatus <- renderUI({
    if (values$geminiAPI){
      last <- showGeminiAPI()
      output$status <- renderText(paste0("✅ API key has been set: ",last))
    }
  })
  
  observeEvent(input$set_key, {
    key <- input$api_key
    last <- setGeminiAPI(key)
    
    if (is.na(last)){
      output$apiStatus <- renderUI({
        output$status <- renderText(paste0("❌ API key seems tto be not valid"))
      })
      values$geminiAPI <- FALSE
    } else {
      output$apiStatus <- renderUI({
        output$status <- renderText(paste0("✅ API key has been set: ",last))
      })
      values$geminiAPI <- TRUE
      home <- homeFolder()
      path_gemini_key <- paste0(home,"/.biblio_gemini_key.txt", collapse="")
      writeLines(Sys.getenv("GEMINI_API_KEY"), path_gemini_key)
    }
    
  })
  
  observeEvent(input$remove_key, {
    if (values$geminiAPI){
      home <- homeFolder()
      path_gemini_key <- paste0(home,"/.biblio_gemini_key.txt", collapse="")
      file.remove(path_gemini_key)
      values$geminiAPI <- FALSE
    }
  })
  
}



# END ####
