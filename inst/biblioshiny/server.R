# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  options(shiny.maxRequestSize=30*1024^2) 
  
  values = reactiveValues()
  values$results="NA"
  values$log="working..."
  values$load="FALSE"
  values$field="NA"
  values$citField=values$colField="NA"
  values$NetWords=values$NetRefs=values$ColNetRefs=matrix(NA,1,1)
  values$Title="Network"
  values$Histfield="NA"
  values$histlog="working..."
  
  
  
  ### caricamento file
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile)) {return(NULL)}
    
    if (grepl(".*\\.xlsx",inFile$name)){
      M <- rio::import(inFile$datapath)
      values$log<-"Data imported from XLSX file"
      
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
      
      
    }else{
      
      if (grepl(".*\\.zip",inFile$name)) {
        files=unzip(inFile$datapath)
        #files = list.files(pattern = ".txt")
        D = unlist(lapply(files, function(l){
          Dpar=readFiles(l)
          return(Dpar)
          }))
      
      }else{
        D=readFiles(inFile$datapath)
          }
    
    values$log<- capture.output(M <- convert2df(D, dbsource=input$dbsource,format=input$format))
    }
    
    #M <- convert2df(D<-readFiles(inFile$datapath), dbsource="isi",format="plaintext")
    values$M <- M
    values$Histfield="NA"
    #return(values$M)
    return(as.data.frame(apply(M,2,function(x){substring(x,1,50)}),stringsAsFactors = FALSE))
    
  }, striped = TRUE,spacing="xs")

  output$collection.xlsx <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content <- function(file) {
      rio::export(values$M, file=file)
    },
    contentType = "xlsx"
  )
  
  output$log <- renderText({
    log=gsub("  Art","\\\nArt",values$log)
    log=gsub("Done!   ","Done!\\\n",log)
    return(log)
    
  })
  

  output$summary <- renderPrint({
    if (values$results=="NA"){
      values$results=biblioAnalysis(values$M)}
    S=summary(values$results,k=input$kk,verbose=FALSE)
    
    
    switch(input$summary_type,
           "tab1"={cat(S$MainInformation)},
           "tab2"={print(S$AnnualProduction)
             cat("\n\n")
             cat("Annual Growth Rate ",round(S$AnnualGrowthRate, digits=2),"%")},
           "tab3"={print(S$MostProdAuthors)},
           "tab4"={print(S$MostCitedPapers)},
           "tab5"={print(S$MostProdCountries)},
           "tab6"={print(S$TCperCountries)},
           "tab7"={print(S$MostRelSources)},
           "tab8"={print(S$MostRelKeywords)},
           "all"={cat(S$MainInformation)
                  print(S$AnnualProduction)
                  cat("\n\n")
                  print(S$AnnualGrowthRate);cat("\n\n")
                  print(S$MostProdAuthors);cat("\n\n")
                  print(S$MostCitedPapers);cat("\n\n")
                  print(S$MostProdCountries);cat("\n\n")
                  print(S$TCperCountries);cat("\n\n")
                  print(S$MostRelSources);cat("\n\n")
                  print(S$MostRelKeywords);cat("\n\n")
           }
           )
    
    
    #
  })
  
  output$summaryPlots <- renderPlot({
    if (values$results=="NA"){
      values$results=biblioAnalysis(values$M)}
    k=input$k
    switch(input$plot_type,
           authors={
             xx=as.data.frame(values$results$Authors[1:k])
             g=ggplot2::ggplot(data=xx, aes(x=xx$AU, y=xx$Freq)) +
               geom_bar(stat="identity", fill="steelblue")+
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
                                  labs(title = "Most Productive Countries", x = "Countries", y = "N. of Documents", 
                                       caption = "SCP: Single Country Publications, MCP: Multiple Country Publications")+
                                  theme_minimal() +
                                  theme(plot.caption = element_text(size = 9, hjust = 0.5,
                                                                    color = "blue", face = "italic"))+
                                  coord_flip())
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
               labs(x = 'Year'
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
  
  output$cocPlot <- renderPlot({
    
  ## Keyword co-occurrences network
    
    n = input$Nodes
    label.n = input$Labels
    
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
    
    net=networkPlot(values$NetWords, normalize=normalize,n = n, Title = values$Title, type = input$layout, 
                    size.cex=size.cex, size=input$size , remove.multiple=F, edgesize = input$edgesize, labelsize=input$labelsize,label.cex=label.cex,
                    label.n=label.n,edges.min=input$edges.min,label.color = F)
    
    
  }, height = 750, width = 900)
  
  output$CSPlot1 <- renderPlot({
    
    values$CS <- conceptualStructure(values$M, method=input$method , field=input$CSfield, minDegree=input$CSn, k.max = 8, stemming=F, labelsize=input$CSlabelsize,documents=input$CSdoc,graph=FALSE)
    plot(values$CS$graph_terms)
  
  }, height = 650, width = 800)
  
  output$CSPlot2 <- renderPlot({
    
    #values$CS <- conceptualStructure(values$M, method=input$method , field="ID", minDegree=5, k.max = 8, stemming=F, labelsize=8,documents=20,graph=FALSE)
    plot(values$CS$graph_documents_Contrib)
    
  }, height = 650, width = 800)
  
  output$CSPlot3 <- renderPlot({
    
    #values$CS <- conceptualStructure(values$M, method=input$method , field="ID", minDegree=5, k.max = 8, stemming=F, labelsize=8,documents=20,graph=FALSE)
    plot(values$CS$graph_documents_TC)
    
  }, height = 650, width = 800)
  
  output$TMPlot <- renderPlot({
    
    NetMatrix <- biblioNetwork(values$M, analysis = "co-occurrences",network = "keywords", sep = ";")
    S <- normalizeSimilarity(NetMatrix, type = "association")
    capture.output(net1 <- networkPlot(S, n=500, Title = "Keyword co-occurrences",type="fruchterman",
                        labelsize = 2, halo = F, cluster = "walktrap",remove.isolates=FALSE,
                        remove.multiple=FALSE, noloops=TRUE, weighted=TRUE,label.cex=T,edgesize=5, size=1,edges.min = 2))
    Map=thematicMap(net1, NetMatrix, S = S, minfreq=input$TMn)
    plot(Map$map)
    
  }, height = 650, width = 800)
  
  output$cocitPlot <- renderPlot({
    
    ## Co-citation network
    
    n = input$citNodes
    label.n = input$citLabels
    
    if ((dim(values$NetRefs)[1])==1 | !(input$citField==values$citField)){
     
      values$citField=input$citField
      
      switch(input$citField,
             CR={
               values$NetRefs <- biblioNetwork(values$M, analysis = "co-citation", network = "references", sep = input$citSep)
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
    
    net=networkPlot(values$NetRefs, normalize=NULL, n = n, Title = values$Title, type = input$citlayout, 
                    size.cex=size.cex, size=input$citsize , remove.multiple=F, edgesize = input$citedgesize, 
                    labelsize=input$citlabelsize,label.cex=label.cex,
                    label.n=label.n,edges.min=input$citedges.min,label.color = F)
    
    
  }, height = 750, width = 900)
  
  output$histPlot <- renderPlot({
    
    ## Historiograph
    
    if (values$Histfield=="NA"){
      values$histResults <- histNetwork(values$M, min.citations=quantile(values$M$TC,0.75), sep = ";")
      values$Histfield="done"
      
    }
    
    values$histlog<- capture.output(values$histPlot <- histPlot(values$histResults, n=input$histNodes, size.cex=TRUE , size =input$histsize, labelsize = input$histlabelsize, arrowsize = 0.5))
    
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
    DT::datatable(Data, escape = FALSE, rownames = FALSE, 
                  options = list(pageLength = input$histNodes, dom = 'tip')) %>%
      formatStyle(names(Data),  backgroundColor = 'gray') %>%
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
               values$cluster="none"
             })
      
    }
    
    if (n>dim(values$ColNetRefs)[1]){n=dim(values$ColNetRefs)[1]}
    if (label.n>n){label.n=n}
    if (input$colnormalize=="none"){normalize=NULL}else{normalize=input$colnormalize}
    if (input$collabel.cex=="Yes"){label.cex=TRUE}else{label.cex=FALSE}
    if (input$colsize.cex=="Yes"){size.cex=TRUE}else{size.cex=FALSE}
    
    net=networkPlot(values$ColNetRefs, normalize=normalize, n = n, Title = values$Title, type = input$collayout, 
                    size.cex=size.cex, size=input$colsize , remove.multiple=F, edgesize = input$coledgesize, 
                    labelsize=input$collabelsize,label.cex=label.cex,
                    label.n=label.n,edges.min=input$coledges.min,label.color = F, cluster=values$cluster)
    
    
  }, height = 750, width = 900)
  
  observe({
    if (length(input$navbar) >0) 
      stopApp()
  })
  
  #session$onSessionEnded(stopApp)
  
  
} ## End of Server