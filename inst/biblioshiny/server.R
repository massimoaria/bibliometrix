# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  ## stop the R session
  session$onSessionEnded(stopApp)
  ##
  
  ## file upload max size
  options(shiny.maxRequestSize=30*1024^2) 
  
  ## initial values
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

  
  
  
  ### caricamento file
  output$contents <- DT::renderDT({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile)) {return(NULL)}
    
    ### excel format
    if (grepl(".*\\.xlsx",inFile$name)){
      M <- rio::import(inFile$datapath)
      #values$log<-"Data imported from XLSX file"
      
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
      
      
    }
    
    ### zip folder
    if (grepl(".*\\.zip",inFile$name)) {
        files=unzip(inFile$datapath)
        #files = list.files(pattern = ".txt")
        D = unlist(lapply(files, function(l){
          Dpar=readFiles(l)
          return(Dpar)
          }))
        capture.output(M <- convert2df(D, dbsource=input$dbsource,format=input$format))
    }
    
    ### txt or bib formats  
    if (grepl(".*\\.txt",inFile$name) | grepl(".*\\.bib",inFile$name)){
        D=readFiles(inFile$datapath)
        capture.output(M <- convert2df(D, dbsource=input$dbsource,format=input$format))
    }
      
    ### RData format
    if (grepl(".*\\.RData",inFile$name)) {
      
      load(inFile$datapath)
    }
    
    values$M <- M
    values$Morig=M
    values$Histfield="NA"
    values$results=list("NA")
    
    MData=as.data.frame(apply(values$M,2,function(x){substring(x,1,150)}),stringsAsFactors = FALSE)
    MData$DOI<- paste0('<a href=\"http://doi.org/',MData$DI,'\" target=\"_blank\">',MData$DI,'</a>')
    nome=c("DOI",names(MData)[-length(names(MData))])
    MData=MData[nome]
    
    DT::datatable(MData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),  
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = c('pageLength','copy', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(MData))-1))))  
                  ,class = 'cell-border compact stripe')  %>% 
                  
                  formatStyle(names(MData),  backgroundColor = 'white', textAlign = 'center',fontSize = '70%') 
    
    
    })

  output$collection.save <- downloadHandler(
    filename = function() {
      
      paste("data-", Sys.Date(), ".",input$save_file, sep="")
    },
    content <- function(file) {
      switch(input$save_file,
             xlsx={rio::export(values$M, file=file)})
      
    },
    contentType = input$save_file
  )
  
  output$textLog <- renderUI({
    #log=gsub("  Art","\\\nArt",values$log)
    #log=gsub("Done!   ","Done!\\\n",log)
    log=paste("Number of Documents ",dim(values$M)[1])
    textInput("textLog", "Conversion results", 
              value=log)
  })
  
  ### Filters uiOutput
  output$textDim <- renderUI({
    dimMatrix=paste("Number of Documents ",dim(values$M)[1])
    textInput("textDim", "", 
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
    sliderInput("sliderPY", "Publication Year", min = min(values$Morig$PY),sep="",
                max = max(values$Morig$PY), value = c(min(values$Morig$PY),max(values$Morig$PY)))
  })
  
  output$selectSource <- renderUI({
    SO=sort(unique(values$Morig$SO))
    selectInput("selectSource", "Source", 
                choices = SO,
                selected = SO,
                multiple = TRUE)
  })
  
  output$sliderTC <- renderUI({
    sliderInput("sliderTC", "Total Citation", min = min(values$Morig$TC),
                max = max(values$Morig$TC), value = c(min(values$Morig$TC),max(values$Morig$TC)))
  })
  ### End Filters uiOutput
  
  output$dataFiltered <- DT::renderDT({
    
    M=values$Morig
    
    M=(M[(M$PY>=input$sliderPY[1] & M$PY<=input$sliderPY[2]),])
    M=(M[(M$TC>=input$sliderTC[1] & M$TC<=input$sliderTC[2]),])
    indexDT=(ifelse(M$DT %in% input$selectType,TRUE,FALSE))
    M=(M[indexDT,])
    indexSO=(ifelse(M$SO %in% input$selectSource,TRUE,FALSE))
    M=((M[M$SO %in% input$selectSource,]))
    values$M=M
    values$results=list("NA") ## to reset summary statistics
    Mdisp=as.data.frame(apply(M,2,function(x){substring(x,1,150)}),stringsAsFactors = FALSE)
    
    DT::datatable(Mdisp, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(Mdisp))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(Mdisp),  backgroundColor = 'white',textAlign = 'center', fontSize = '70%')
  })
  
  ### Descriptive Analysis
  output$summary <- DT::renderDT({
    
    ### aggiungere il pulsante apply
    #if (values$results[[1]]=="NA"){values$results=biblioAnalysis(values$M)}
    values$results=biblioAnalysis(values$M)
    S=summary(object=values$results,k=input$kk,verbose=FALSE)
    
    
    
    switch(input$summary_type,
           "tab1"={
             #TAB=data.frame(Information=gsub("[[:digit:]]", "", S$MainInformation), Data=gsub("[^0-9]", "", S$MainInformation))
             TAB=data.frame(S$MainInformationDF)
             #cat(S$MainInformation)
             },
           "tab2"={
             
             TAB=S$AnnualProduction
             names(TAB)=c("Year","Articles")
             #print(S$AnnualProduction)
             #cat("\n\n")
             #cat("Annual Growth Rate ",round(S$AnnualGrowthRate, digits=2),"%")
             },
           "tab3"={
             TAB=S$MostProdAuthors
             names(TAB)=c("Authors","Articles","Authors-Frac","Articles Fractionalized")
             #print(S$MostProdAuthors)
             },
           "tab4"={
             TAB=S$MostCitedPapers
             names(TAB)=c("Paper", "Total Citations","TC per Year")
             #print(S$MostCitedPapers)
             },
           "tab5"={
             TAB=S$MostProdCountries
             #print(S$MostProdCountries)
             },
           "tab6"={
             TAB=S$TCperCountries
             #print(S$TCperCountries)
             },
           "tab7"={
             TAB=S$MostRelSources
             #print(S$MostRelSources)
             },
           "tab8"={
             TAB=S$MostRelKeywords
             names(TAB)=c("Author Keywords (DE)", "Articles (DE)","Keywords-Plus (ID)","Articles (ID)")
             #print(S$MostRelKeywords)
             },
           "tab9"={
             TAB=as.data.frame(citations(values$M,sep=";")$Cited)
             names(TAB)=c("Cited References", "Citations")
             #print(S$MostRelKeywords)
           },
           "tab10"={
             TAB<-mapworld(values$M)$tab
           }
          )
    
    DT::datatable(TAB, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TAB))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(TAB),  backgroundColor = 'white',textAlign = 'center', fontSize = '110%')
  })
    #

  output$results.txt <- downloadHandler(
    
    filename = function() {
      paste("results-", Sys.Date(), ".txt", sep="")
    },
    content <- function(file) {
      values$S_text=capture.output(summary(object=values$results,k=input$kk))
      write(values$S_text, file, sep="\n")
    },
    
    contentType = "txt"
  )
  output$summaryPlots <- renderPlot({
    if (values$results[[1]]=="NA"){
      values$results=biblioAnalysis(values$M)}
    k=input$k
    switch(input$plot_type,
           authors={
             xx=as.data.frame(values$results$Authors[1:k])
             g=ggplot2::ggplot(data=xx, aes(x=xx$AU, y=xx$Freq)) +
               geom_bar(stat="identity", fill="steelblue")+
               scale_x_discrete(limits = rev(levels(xx$AU)))+
               labs(title="Most productive Authors", x = "Authors")+
               labs(y = "N. of Documents")+
               theme_minimal() +
               coord_flip()
             
           },
           countries={
             xx=values$results$CountryCollaboration[1:k,]
             xx=xx[order(-(xx$SCP+xx$MCP)),]
             xx1=cbind(xx[,1:2],rep("SCP",k))
             names(xx1)=c("Country","Freq","Collaboration")
             xx2=cbind(xx[,c(1,3)],rep("MCP",k))
             names(xx2)=c("Country","Freq","Collaboration")
             xx=rbind(xx2,xx1)
             xx$Country=factor(xx$Country,levels=xx$Country[1:dim(xx2)[1]])
             g=suppressWarnings(ggplot2::ggplot(data=xx, aes(x=xx$Country, y=xx$Freq,fill=xx$Collaboration)) +
                                  geom_bar(stat="identity")+
                                  scale_x_discrete(limits = rev(levels(xx$Country)))+
                                  scale_fill_discrete(name="Collaboration",
                                                      breaks=c("SCP","MCP"))+
                                  labs(title = "Corresponding Author's Country", x = "Countries", y = "N. of Documents", 
                                       caption = "SCP: Single Country Publications, MCP: Multiple Country Publications")+
                                  theme_minimal() +
                                  theme(plot.caption = element_text(size = 9, hjust = 0.5,
                                                                    color = "blue", face = "italic"))+
                                  coord_flip())
           },
           mapworld={
             g<-mapworld(values$M)$g
           },
           production={
             Tab=table(values$results$Years)
             
             ## inserting missing years
             YY=setdiff(seq(min(values$results$Years),max(values$results$Years)),names(Tab))
             Y=data.frame(Year=as.numeric(c(names(Tab),YY)),Freq=c(as.numeric(Tab),rep(0,length(YY))))
             Y=Y[order(Y$Year),]
             
             names(Y)=c("Year","Freq")
             
             g=ggplot2::ggplot(Y, aes(x = Y$Year, y = Y$Freq)) +
               geom_line() +
               geom_area(fill = '#002F80', alpha = .5) +
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
           },
           articleTC={
             Table2=aggregate(values$results$TotalCitation,by=list(values$results$Years),length)
             Table2$xx=aggregate(values$results$TotalCitation,by=list(values$results$Years),mean)$x
             Table2$Annual=NA
             d=date()
             d=as.numeric(substring(d,nchar(d)-3,nchar(d)))
             Table2$Years=d-Table2$Group.1
             Table2$Annual=Table2$xx/Table2$Years
             names(Table2)=c("Year","N","MeanTCperArt","MeanTCperYear","CitableYears")
             
             ## inserting missing years
             YY=setdiff(seq(min(values$results$Years),max(values$results$Years)),Table2$Year)
             if (length(YY>0)){
               YY=data.frame(YY,0,0,0,0)
               names(YY)=c("Year","N","MeanTCperArt","MeanTCperYear","CitableYears")
               Table2=rbind(Table2,YY)
               Table2=Table2[order(Table2$Year),]
               row.names(Table2)=Table2$Year}
             
             
             g=ggplot2::ggplot(Table2, aes(x = Table2$Year, y = Table2$MeanTCperYear)) +
               geom_line() +
               geom_area(fill = '#002F80', alpha = .5) +
               labs(x = 'Publication Year'
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
           },
           annualTC={
             Table2=aggregate(values$results$TotalCitation,by=list(values$results$Years),length)
             Table2$xx=aggregate(values$results$TotalCitation,by=list(values$results$Years),mean)$x
             Table2$Annual=NA
             d=date()
             d=as.numeric(substring(d,nchar(d)-3,nchar(d)))
             Table2$Years=d-Table2$Group.1
             Table2$Annual=Table2$xx/Table2$Years
             names(Table2)=c("Year","N","MeanTCperArt","MeanTCperYear","CitableYears")
             
             ## inserting missing years
             YY=setdiff(seq(min(values$results$Years),max(values$results$Years)),Table2$Year)
             if (length(YY>0)){
               YY=data.frame(YY,0,0,0,0)
               names(YY)=c("Year","N","MeanTCperArt","MeanTCperYear","CitableYears")
               Table2=rbind(Table2,YY)
               Table2=Table2[order(Table2$Year),]
               row.names(Table2)=Table2$Year}
             
             g=ggplot2::ggplot(Table2, aes(x = Table2$Year, y = Table2$MeanTCperArt)) +
               geom_line() +
               geom_area(fill = '#002F80', alpha = .5) +
               labs(x = 'Publication Year'
                    , y = 'Citations'
                    , title = "Average Total Citations per Year")+
               scale_x_continuous(breaks= (Table2$Year[seq(1,length(Table2$Year),by=2)])) +
               theme(text = element_text(color = "#444444")
                     ,panel.background = element_rect(fill = '#EFEFEF')
                     ,panel.grid.minor = element_line(color = '#FFFFFF')
                     ,panel.grid.major = element_line(color = '#FFFFFF')
                     ,plot.title = element_text(size = 24)
                     ,axis.title = element_text(size = 14, color = '#555555')
                     ,axis.title.y = element_text(vjust = 1, angle = 0)
                     ,axis.title.x = element_text(hjust = 0, angle = 0)
               )
           }
           ) 
    plot(g)
    #plot(results)
    }, height = 500, width =900)
  
  output$bradfordPlot <- renderPlot({
    
    values$bradford=bradford(values$M)
    values$bradford$graph
    
  },height = 600)
  
  output$bradfordTable <- DT::renderDT({
    
    DT::datatable(values$bradford$table, rownames = FALSE,
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = c('pageLength','copy','excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(values$bradford$table))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(values$bradford$table),  backgroundColor = 'white',textAlign = 'center')
  })
  
  output$lotkaPlot <- renderPlot({
    
      values$lotka=lotka(biblioAnalysis(values$M))
      AuProd=values$lotka$AuthorProd
      AuProd$Theoretical=10^(log10(values$lotka$C)-2*log10(AuProd[,1]))
      
      g=ggplot2::ggplot(AuProd, aes(x = AuProd$N.Articles, y = AuProd$Freq*100)) +
        geom_line() +
        geom_area(fill = '#002F80', alpha = .5) +
        labs(x = 'N. Articles'
             , y = '% of total scientific production'
             , title = "Author Scientific Productivity") +
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
      plot(g)
      
  },height = 600)
  
  output$lotkaTable <- DT::renderDT({
    
    DT::datatable(values$lotka$AuthorProd, rownames = FALSE,
                  options = list(pageLength = 20, dom = 'Bfrtip',
                                 buttons = c('pageLength','copy','excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(values$lotka$AuthorProd))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(values$lotka$AuthorProd),  backgroundColor = 'white',textAlign = 'center')
  })
  
  output$wordcloud <- wordcloud2::renderWordcloud2({
    
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
  
  output$treemap <- renderPlot({
    
    resW=wordlist(M=values$M, Field=input$treeTerms, n=input$treen_words, measure=input$treemeasure)
    
    W=resW$W
    values$Words=resW$Words
    
    treemap::treemap(W, #Your data frame object
            index=c("Terms"),  #A list of your categorical variables
            vSize = "Frequency",  #This is your quantitative variable
            type="index", #Type sets the organization and color scheme of your treemap
            palette = input$treeCol,  #Select your color palette from the RColorBrewer presets or make your own.
            title="Word TreeMap", #Customize your title
            fontsize.title = 14, #Change the font size of the title
            fontsize.labels = input$treeFont
    )
    
    
  })
  
  output$wordTable <- DT::renderDT({

    DT::datatable(values$Words, rownames = FALSE,
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = c('pageLength','copy','excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(values$Words))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(values$Words),  backgroundColor = 'white',textAlign = 'center')
  })
  
  output$treeTable <- DT::renderDT({
    
    DT::datatable(values$Words, rownames = FALSE,
                  options = list(pageLength = 10, dom = 'Bfrtip',
                                 buttons = c('pageLength','copy','excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(values$Words))-1)))), 
                  class = 'cell-border compact stripe') %>%
      formatStyle(names(values$Words),  backgroundColor = 'white',textAlign = 'center')
  },height = 600, width = 900)
  
  output$kwGrowthPlot <- renderPlot({
    
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
    values$DF=data.frame(Year=rep(values$KW$Year,(dim(values$KW)[2]-1)),Term=term, Freq=freq)
    
    g=ggplot(values$DF)+
      geom_smooth(aes(x=values$DF$Year,y=values$DF$Freq, group=values$DF$Term, color=values$DF$Term),se = se,method = "loess",formula ='y ~ x')+
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
      ggrepel::geom_text_repel(data = DF2, aes(label = DF2$group, colour = DF2$group, x =DF2$x, y = DF2$y), hjust = -.1)
    suppressWarnings(plot(g))
    
  },height = 600, width = 900)
  
  output$kwGrowthtable <- DT::renderDT({
    
    kwData=values$KW
    
    DT::datatable(kwData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = c('pageLength','copy','excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(kwData))-1))))) %>%
      formatStyle(names(kwData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$soGrowthPlot <- renderPlot({
    
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
    values$SODF=data.frame(Year=rep(values$PYSO$Year,(dim(values$PYSO)[2]-1)),Source=term, Freq=freq)
    
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
    suppressWarnings(plot(g))
    
    
    # maximum=sort(unique(values$SODF$Year),decreasing=TRUE)[2]
    # DF2=subset(values$SODF, Year == maximum)
    # g=g+
    #   ggrepel::geom_text_repel(data = DF2, aes(label = DF2$Source, colour = DF2$Source, x =DF2$Year, y = DF2$Freq), hjust = -.1)
    # 
    # plot(g)
    
  },height = 600, width = 900)
  
  output$soGrowthtable <- DT::renderDT({
    
    soData=values$PYSO
    
    DT::datatable(soData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = c('pageLength','copy','excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(soData))-1))))) %>%
      formatStyle(names(soData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$rpysPlot <- renderPlot({
    values$res <- rpys(values$M, sep=input$rpysSep, timespan=input$sliderYears ,graph=FALSE)
    #values$res <- rpys(values$M, sep=input$rpysSep, timespan=input$sliderYears ,graph=FALSE)
    plot(values$res$spectroscopy)
    
  },height = 600, width = 900)
  
  output$rpysTable <- DT::renderDT({
    
    rpysData=values$res$rpysTable
    
    DT::datatable(rpysData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = c('pageLength','copy','excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(rpysData))-1))))) %>%
      formatStyle(names(rpysData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$crTable <- DT::renderDT({
    
    crData=values$res$CR
    crData=crData[order(-as.numeric(crData$Year),-crData$Freq),]
    names(crData)=c("Year", "Reference", "Local Citations")
    DT::datatable(crData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = c('pageLength','copy','excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(crData))-1))))) %>%
      formatStyle(names(crData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  ### Conceptual Structure
  output$cocPlot <- renderPlot({
    
  ## Keyword co-occurrences network
    input$applyCoc
   
    values <- isolate(cocNetwork(input,values))
    
  }, height = 750, width = 900)
  
  output$network.coc <- downloadHandler(
    filename = "Co_occurrence_network.net",
    content <- function(file) {
      igraph::write.graph(values$cocnet$graph_pajek,file=file, format="pajek")
      
    },
    contentType = "net"
  )
  
  output$cocTable <- DT::renderDT({
    
    cocData=values$cocnet$cluster_res
    names(cocData)=c("Term", "Cluster", "Btw Centrality")
    DT::datatable(cocData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"), filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = c('pageLength','copy','excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(cocData))-1))))) %>%
      formatStyle(names(cocData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$CSPlot1 <- renderPlot({
    
    input$applyCA
    
    values <- isolate(CAmap(input,values))
    

  }, height = 650, width = 800)
  
  output$CSPlot2 <- renderPlot({
    
    if (input$method!="MDS"){
    
      if (values$CS[[1]][1]!="NA"){
        plot(values$CS$graph_documents_Contrib)
      }else{
        emptyPlot("Selected field is not included in your data collection")
      }
    }else{
      emptyPlot("This plot is available only for CA or MCA analyses")
    }
    
  }, height = 650, width = 800)
  
  output$CSPlot3 <- renderPlot({
    
    if (input$method!="MDS"){
      if (values$CS[[1]][1]!="NA"){
        plot(values$CS$graph_documents_TC)
      }else{
        emptyPlot("Selected field is not included in your data collection")
      }
    }else{
      emptyPlot("This plot is available only for CA or MCA analyses")
    }
    
    
  }, height = 650, width = 800)
  
  output$TMPlot <- renderPlot({
    
    input$applyTM
    
    values <- isolate(TMmap(input,values))
    
    validate(
      need(values$TM$nclust > 0, "\n\nNo topics in one or more periods. Please select a different set of parameters.")
    )
    
    plot(values$TM$map)

  }, height = 650, width = 800)
  
  output$TMTable <- DT::renderDT({
    
    tmData=values$TM$words[,-4]
    
    
    
    DT::datatable(tmData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = c('pageLength','copy','excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(tmData))-1))))) %>%
      formatStyle(names(tmData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$sliders <- renderUI({
    numSlices <- as.integer(input$numSlices)
    lapply(1:numSlices, function(i) {
      # sliderInput(inputId = paste0("Slice", i), label = paste("Slice", i),
      #             min=1990,max=2018,value=1990)
      numericInput(inputId = paste0("Slice", i), label = paste("Slice", i),value=median(values$M$PY),min=min(values$M$PY)+1,max=max(values$M$PY)-1, step=1)
    })
  })
  
  output$TEPlot <- networkD3::renderSankeyNetwork({
    
    input$applyTE
    
    values$yearSlices=isolate(as.numeric())
    isolate(for (i in 1:as.integer(input$numSlices)){
      if (length(input[[paste0("Slice", i)]])>0){values$yearSlices=c(values$yearSlices,input[[paste0("Slice", i)]])}
    })
    
    if (length(values$yearSlices)>0){
    values$nexus <- isolate(thematicEvolution(values$M, field=input$TEfield, values$yearSlices,n=input$nTE,minFreq=input$fTE))
    
    validate(
      need(values$nexus$check != FALSE, "\n\nNo topics in one or more periods. Please select a different set of parameters.")
    )
    
    isolate(plotThematicEvolution(values$nexus$Nodes,values$nexus$Edges))
    }
      
    
  })
  
  output$TETable <- DT::renderDT({
    
    TEData=values$nexus$Data
    TEData=TEData[TEData$Inc_index>0,]
    names(TEData)=c("From", "To", "Inclusion Index", "Words", "Occurrences", "Total", "Weighted Inclusion Index")
    DT::datatable(TEData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = c('pageLength','copy','excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(TEData))-1))))) %>%
      formatStyle(names(TEData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$cocitPlot <- renderPlot({
    
    ## Co-citation network
    
    input$applyCocit
    
    values <- isolate(intellectualStructure(input,values))
    
    
    
  }, height = 750, width = 900)
  
  output$network.cocit <- downloadHandler(
    filename = "Co_citation_network.net",
    content <- function(file) {
      igraph::write.graph(values$cocitnet$graph_pajek,file=file, format="pajek")
      #rio::export(values$M, file=file)
    },
    contentType = "net"
  )
  
  output$cocitTable <- DT::renderDT({
    
    cocitData=values$cocitnet$cluster_res
    names(cocitData)=c("Node", "Cluster", "Btw Centrality")
    DT::datatable(cocitData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = c('pageLength','copy','excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(cocitData))-1))))) %>%
      formatStyle(names(cocitData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  
  output$histPlot <- renderPlot({
    
    ## Historiograph
    input$applyHist
    
    values <- isolate(historiograph(input,values))
    

    }, height = 500, width = 900)
  
  output$histTable <- DT::renderDT({
    
    if (values$Histfield=="NA"){
      values$histResults <- histNetwork(values$M, min.citations=quantile(values$M$TC,0.75), sep = ";")
      values$Histfield="done"
    }
    LCS=values$histResults$LCS
    s=sort(LCS,decreasing = TRUE)[input$histNodes]
    ind=which(LCS>=s)
    Data=values$histResults$histData
    Data=Data[ind,]
    Data$DOI<- paste0('<a href=\"http://doi.org/',Data$DOI,'\" target=\"_blank\">',Data$DOI,'</a>')
    DT::datatable(Data, escape = FALSE, rownames = FALSE, extensions = c("Buttons"),
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = c('pageLength','copy', 'pdf', 'print'),
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
  
  output$colPlot <- renderPlot({
    
    ## Collaboration network
    input$applyCol

    isolate(
      if (input$colField=="COL_CO" & input$collayout=="worldmap"){
        values$colmap=countrycollaboration(values$M,label=FALSE,edgesize=input$coledgesize/2,min.edges=input$coledges.min)
        plot(values$colmap$g)

      }else{values <- socialStructure(input,values)}
    )
    
  }, height = 750)#, width = 750
  
  output$network.col <- downloadHandler(
    filename = "Collaboration_network.net",
    content <- function(file) {
      igraph::write.graph(values$colnet$graph_pajek,file=file, format="pajek")
      #rio::export(values$M, file=file)
    },
    contentType = "net"
  )
  
  output$colTable <- DT::renderDT({
    
    if (input$collayout!="worldmap"){
      colData=values$colnet$cluster_res
      names(colData)=c("Node", "Cluster", "Btw Centrality")
    }else{
      colData=values$colmap$tab
      colData=colData[,c(1,2,9)]
      names(colData)=c("From","To","Frequency")
      }
    DT::datatable(colData, escape = FALSE, rownames = FALSE, extensions = c("Buttons"), filter = 'top',
                  options = list(pageLength = 50, dom = 'Bfrtip',
                                 buttons = c('pageLength','copy','excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),c('10 rows', '25 rows', '50 rows','Show all')),
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(length(names(colData))-1))))) %>%
      formatStyle(names(colData),  backgroundColor = 'white') 
    #return(Data)
    
  })
  

  # common functions
  
  emptyPlot<-function(errortext){
    g=ggplot()+
      theme_void() + theme(legend.position="none")+
      annotate("text", x = 4, y = 25, label = errortext)
    plot(g)
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
    Words=data.frame(Terms=names(v)[1:n], Frequency=(as.numeric(v)[1:n]))
    W=Words
    switch(measure,
           sqrt={W$Frequency=sqrt(W$Frequency)},
           log={W$Frequency=log(W$Frequency+1)},
           log10={W$Frequency=log10(W$Frequency+1)}
    )
    
    results=list(v=v,W=W, Words=Words)
    return(results)
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
      if (input$size.cex=="Yes"){size.cex=TRUE}else{size.cex=FALSE}
      if (input$coc.curved=="Yes"){curved=TRUE}else{curved=FALSE}
      
      #par(bg="grey92", mar=c(0,0,0,0))
      values$cocnet=networkPlot(values$NetWords, normalize=normalize,n = n, Title = values$Title, type = input$layout, 
                                size.cex=size.cex, size=input$size , remove.multiple=F, edgesize = input$edgesize, labelsize=input$labelsize,label.cex=label.cex,
                                label.n=label.n,edges.min=input$edges.min,label.color = F, curved=curved,alpha=input$cocAlpha)
    }else{
      emptyPlot("Selected field is not included in your data collection")
    }
  }
  
  mapworld <- function(M){
    M=metaTagExtraction(M,"AU_CO")
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
   
    g=ggplot(country.prod, aes( x = long, y = lat, group = group )) +
      geom_polygon(aes(fill = log(Freq))) +
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
  
  CAmap <- function(input, values){
    if ((input$CSfield %in% names(values$M))){
      
      tab=tableTag(values$M,input$CSfield)
      if (length(tab>=2)){
        
        minDegree=as.numeric(tab[input$CSn])
        
        values$CS <- conceptualStructure(values$M, method=input$method , field=input$CSfield, minDegree=minDegree, k.max = 8, stemming=F, labelsize=input$CSlabelsize,documents=input$CSdoc,graph=FALSE)
        plot(values$CS$graph_terms)
        
      }else{emptyPlot("Selected field is not included in your data collection")
        values$CS=list("NA")}
      
    }else{
      emptyPlot("Selected field is not included in your data collection")
      values$CS=list("NA")
      
    }
  }
  
  TMmap <- function(input,values){
    
    switch(input$TMfield,
           ID={
             NetMatrix <- biblioNetwork(values$M, analysis = "co-occurrences", network = "keywords", sep = ";")
             
           },
           DE={
             NetMatrix <- biblioNetwork(values$M, analysis = "co-occurrences", network = "author_keywords", sep = ";")
             
           },
           TI={
             if(!("TI_TM" %in% names(values$M))){values$M=termExtraction(values$M,Field="TI",verbose=FALSE)}
             NetMatrix <- biblioNetwork(values$M, analysis = "co-occurrences", network = "titles", sep = ";")
             
           },
           AB={
             if(!("AB_TM" %in% names(values$M))){values$M=termExtraction(values$M,Field="AB",verbose=FALSE)}
             NetMatrix <- biblioNetwork(values$M, analysis = "co-occurrences", network = "abstracts", sep = ";")
             
           })
    
    S <- normalizeSimilarity(NetMatrix, type = "association")
    capture.output(net1 <- networkPlot(S, n=500, Title = "Keyword co-occurrences",type="fruchterman",
                                       labelsize = 2, halo = F, cluster = "walktrap",remove.isolates=FALSE,
                                       remove.multiple=FALSE, noloops=TRUE, weighted=TRUE,label.cex=T,edgesize=5, size=1,edges.min = 2))
    Map=thematicMap(net1, NetMatrix, S = S, minfreq=input$TMn)
    #plot(Map$map)
    values$TM=Map
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
    if (input$citsize.cex=="Yes"){size.cex=TRUE}else{size.cex=FALSE}
    if (input$cocit.curved=="Yes"){curved=TRUE}else{curved=FALSE}
    
    values$cocitnet=networkPlot(values$NetRefs, normalize=NULL, n = n, Title = values$Title, type = input$citlayout, 
                                size.cex=size.cex, size=input$citsize , remove.multiple=F, edgesize = input$citedgesize, 
                                labelsize=input$citlabelsize,label.cex=label.cex, curved=curved,
                                label.n=label.n,edges.min=input$citedges.min,label.color = F,remove.isolates = FALSE,alpha=input$cocitAlpha)
    return(values)
  }
  
  historiograph <- function(input,values){
    
    if (input$histsearch=="FAST"){
      min.cit=quantile(values$M$TC,0.75)
    }else{min.cit=1}
    
    if (values$Histfield=="NA" | values$histsearch!=input$histsearch){
      values$histResults <- histNetwork(values$M, min.citations=min.cit, sep = ";")
      values$Histfield="done"
      values$histsearch=input$histsearch
    }
    
    values$histlog<- capture.output(values$histPlot <- histPlot(values$histResults, n=input$histNodes, size.cex=TRUE , size =input$histsize, labelsize = input$histlabelsize, arrowsize = 0.5))
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
    if (input$colsize.cex=="Yes"){size.cex=TRUE}else{size.cex=FALSE}
    if (input$soc.curved=="Yes"){curved=TRUE}else{curved=FALSE}
    
    type=input$collayout
    if (input$collayout=="worldmap"){type="auto"}
    
    values$colnet=networkPlot(values$ColNetRefs, normalize=normalize, n = n, Title = values$Title, type = type, 
                              size.cex=size.cex, size=input$colsize , remove.multiple=F, edgesize = input$coledgesize, 
                              labelsize=input$collabelsize,label.cex=label.cex, curved=curved,
                              label.n=label.n,edges.min=input$coledges.min,label.color = F,alpha=input$colAlpha,remove.isolates = T)
    
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
    
    #tab=data.frame(country.prod %>%
    #                 dplyr::group_by(region) %>%
    #                 dplyr::summarise(Freq=mean(Freq)))
    
    #tab=tab[!is.na(tab$Freq),]
    
    #tab=tab[order(-tab$Freq),]
    
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
    
    g=ggplot(country.prod, aes( x = country.prod$long, y = country.prod$lat, group = country.prod$group )) +
      geom_polygon(aes(fill = log(Freq))) +
      scale_fill_continuous(low='dodgerblue', high='dodgerblue4',breaks=breaks)+
      #guides(fill = guide_legend(reverse = T)) +
      guides(colour=FALSE, fill=FALSE)+
      geom_curve(data=COedges, aes(x = COedges$Longitude.x , y = COedges$Latitude.x, xend = COedges$Longitude.y, yend = COedges$Latitude.y,     # draw edges as arcs
                                   color = "firebrick4", size = COedges$count, group=COedges$continent.x),
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
  
  count.duplicates <- function(DF){
    x <- do.call('paste', c(DF, sep = '\r'))
    ox <- order(x)
    rl <- rle(x[ox])
    cbind(DF[ox[cumsum(rl$lengths)],,drop=FALSE],count = rl$lengths)
    
  }
  

} ## End of Server