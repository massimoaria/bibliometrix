if (!(require(bibliometrix))){install.packages("bibliometrix"); require(bibliometrix, quietly=TRUE)}
if (!(require(shiny))){install.packages("shiny"); require(shiny, quietly=TRUE)} 
if (!(require(rio))){install.packages("rio")} 
if (!(require(DT))){install.packages("DT")};require(DT, quietly =TRUE)
if (!(require(ggplot2))){install.packages("ggplot2"); require(ggplot2, quietly=TRUE)} 
if (!(require(shinycssloaders))){install.packages("shinycssloaders")} 
if (!(require(wordcloud2))){install.packages("wordcloud2")} 
if (!require(ggmap)){install.packages("ggmap"); require(ggmap, quietly=TRUE)}
if (!require(maps)){install.packages("maps"); require(maps, quietly=TRUE)}
if (!require(visNetwork)){install.packages("visNetwork"); require(visNetwork, quietly=TRUE)}
if (!require(plotly)){install.packages("plotly"); require(plotly, quietly=TRUE)}
if (!require(fontawesome)){install.packages("fontawesome"); require(fontawesome, quietly=TRUE)}
if (!require(dashboardthemes)){install.packages("dashboardthemes"); require(dashboardthemes, quietly=TRUE)}
if (!require(shinydashboardPlus)){install.packages("shinydashboardPlus"); require(shinydashboardPlus, quietly=TRUE)}
if (!require(shinydashboard)){install.packages("shinydashboard"); require(shinydashboard, quietly=TRUE)}
require(Matrix, quietly = TRUE)
require(dimensionsR, quietly = TRUE)
require(pubmedR, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidyr, quietly = TRUE)

#### INTERFACCIA UTENTE #### 
## Title ----
mytitle <- tags$link(tags$a(href = 'https://www.bibliometrix.org/',target="_blank",
                            tags$img(src="logo2.jpg", height = '35',width='35')),
                     strong("bibliometrix")
                     )
                     #style = "font-weight: jost"
                       

intro <- "javascript:void(window.open('https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html', '_blank'))"
importData <- "javascript:void(window.open('https://www.bibliometrix.org/vignettes/Data-Importing-and-Converting.html', '_blank'))"
slides <- "javascript:void(window.open('https://bibliometrix.org/biblioshiny/assets/player/KeynoteDHTMLPlayer.html#0', '_blank'))"
donation <- "javascript:void(window.open('https://www.bibliometrix.org/donate.html', '_blank'))"
bibliometrixWeb <- "javascript:void(window.open('https://www.bibliometrix.org/', '_blank'))" 
k_synth <- "javascript:void(window.open('https://www.k-synth.unina.it', '_blank'))"
github_aria <- "javascript:void(window.open('https://github.com/massimoaria/bibliometrix', '_blank'))"

## Header ----
header <- shinydashboardPlus::dashboardHeader(title = mytitle,
                                              titleWidth = 300,
                                              dropdownMenuOutput("notificationMenu"),
                                              dropdownMenu(
                                                type = "messages",
                                                icon = icon("question"),
                                                badgeStatus = NULL,
                                                headerText = strong("Help Menu"),
                                                messageItem(
                                                  from = "Package Tutorial",
                                                  message = "",
                                                  href = intro,
                                                  icon = icon("play-circle", lib = "glyphicon")
                                                ),
                                                messageItem(
                                                  from = "Convert and Import Data",
                                                  message = "",
                                                  icon = icon("info-sign", lib = "glyphicon"),
                                                  href = importData
                                                ),
                                                messageItem(
                                                  icon = icon("play", lib = "glyphicon"),
                                                  from = "biblioshiny Tutorial",
                                                  message = "",
                                                  href = slides
                                                )
                                              ),
                                              dropdownMenu(type = "messages",
                                                           icon = icon("comment-dollar", lib = "font-awesome"),
                                                           badgeStatus = NULL,
                                                           headerText = strong("Donate"),
                                                           messageItem(
                                                             from = "Donation",
                                                             message = "",
                                                             href = donation,
                                                             icon = icon("share-alt", lib = "glyphicon")
                                                           )
                                              ),
                                              dropdownMenu(
                                                type = "messages",
                                                icon = fa_i(name="cube"),
                                                badgeStatus = NULL,
                                                headerText = strong("Credits"),
                                                messageItem(
                                                  from = "Bibliometrix",
                                                  message = "",
                                                  href = bibliometrixWeb,
                                                  icon = fa_i(name = "globe")
                                                ),
                                                messageItem(
                                                  from = "K-Synth",
                                                  message = "",
                                                  href = k_synth,
                                                  icon = fa_i(name = "watchman-monitoring")
                                                  ),
                                                messageItem(
                                                  from = "Github",
                                                  message = "",
                                                  href = github_aria,
                                                  icon = fa_i(name = "github")
                                                 )
                                                )
                                              )

## Side Bar ----
sidebar <- shinydashboardPlus::dashboardSidebar(
  sidebarMenu(
    menuItem("biblioshiny",tabName = "biblioshinyy",icon = fa_i(name="house-user")),
    menuItem("Data",tabName = "uploadData",icon = fa_i(name = "file-import"),
             menuSubItem("Load Data", tabName = "loadData",icon = icon("chevron-right",lib = "glyphicon")),
             menuSubItem("Gathering Data", tabName = "gathData",icon = icon("chevron-right",lib = "glyphicon"))),
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
             "Countries",
             menuSubItem("Corresponding Author's Country",tabName = "correspAuthorCountry",icon = icon("chevron-right", lib = "glyphicon")),
             menuSubItem("Country Scientific Production",tabName = "countryScientProd",icon = icon("chevron-right", lib = "glyphicon")),
             menuSubItem("Most Cited Countries",tabName = "mostCitedCountries",icon = icon("chevron-right", lib = "glyphicon"))
    ),
    menuItem("Documents", tabName = "documents",icon = fa_i(name="layer-group"), startExpanded = FALSE,
             "Documents",
             menuSubItem("Most Global Cited Documents",tabName = "mostGlobalCitDoc",icon = icon("chevron-right", lib = "glyphicon")),
             menuSubItem("Most Local Cited Documents",tabName = "mostLocalCitDoc",icon = icon("chevron-right", lib = "glyphicon")),
             "Cited Refereces",
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
    
    # menuItem("Quit", tabName = "quit",icon=icon("off",lib = "glyphicon")),
    # menuItem("K-Synth", icon = icon("globe",lib = "glyphicon"), badgeLabel = "Link", badgeColor = "blue",
    #         href = "https://www.k-synth.unina.it")
  ),
  textOutput("res"), width = 300
)

### Body ####

### Theme ----
customTheme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Helvetica" # Carattere di tutto
  # Didot, Verdana, Arial, Helvetica, Trebuchet MS, Optima,Georgia, Courier,
  # Arial Narrow
  ,appFontColor = "rgb(0,0,0)" # Nero
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(255,255,255)" # Bianco
  
  ### header
  ,logoBackColor = "rgb(88,101,185)" # Viola, inizio biblioshiny
  
  ,headerButtonBackColor = "rgb(88,101,185)" # Viola punta del logo
  ,headerButtonIconColor = "rgb(248,248,248)"
  ,headerButtonBackColorHover = "rgb(75,90,179)"
  #,headerButtonBackColorHover = "rgb(75,90,179)" # Viola Scuro
  ,headerButtonIconColorHover = "rgb(248,248,248)" 
  
  ,headerBackColor = "rgb(88,101,185)" # Header orizzontale
  ,headerBoxShadowColor = "rgb(210,210,210)" # Grigio
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
     direction = "down"
    ,colorStart = "rgb(88,101,185)" #Viola punta logo
    ,colorMiddle = "rgb(29,143,225)"
    ,colorEnd = "rgb(34,220,253)"
    ,colorStartPos = 0 # Posizione dei colori
    ,colorMiddlePos = 55
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0 # spazio prima della sidebar
   
   # Menu, colori e bordi
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "0px 0px 0px"
  ,sidebarShadowColor = "#aaaaaa"
  
  # Colore testo in User
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  # Input Search in sidebar
  ,sidebarSearchBackColor = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(34,220,253)"
    ,colorMiddle = "rgb(29,143,225)"
    ,colorEnd = "rgb(88,101,185)"
    ,colorStartPos = 0 # Posizione dei colori
    ,colorMiddlePos = 75
    ,colorEndPos = 100)
  ,sidebarSearchIconColor = "rgb(255,255,255)"
  ,sidebarSearchBorderColor = "rgb(29,143,225)"
  
  # Testo del menu a sinistra
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 15
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  # Gradiente barra dopo aver selezionato una parte del menu
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
     direction = "right"
    ,colorStart = "rgb(34,220,253)"
    ,colorMiddle = "rgb(29,143,225)"
    ,colorEnd = "rgb(88,101,185)"
    ,colorStartPos = 0 # Posizione dei colori
    ,colorMiddlePos = 75
    ,colorEndPos = 100)
  
  # Colore icona della freccetta 
  ,sidebarTabTextColorSelected = "rgb(255,255,255)"
  ,sidebarTabRadiusSelected = "0px 0px 0px 0px"
  
  # Gradiente barra scorrimento
  ,sidebarTabBackColorHover = "rgb(255,255,255)"
    # cssGradientThreeColors(
    # direction = "right"
    # ,colorStart = "rgb(34,220,253)"
    # ,colorMiddle = "rgb(29,143,225)"
    # ,colorEnd = "rgb(88,101,185)"
    # ,colorStartPos = 0 # Posizione dei colori
    # ,colorMiddlePos = 80
    # ,colorEndPos = 100)
  
  # Colore testo, bordo, colore bordo, grandezza bordo dei tab panel
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 0px 0px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgb(88,101,185)"  #success
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(34,220,253)"
    ,colorMiddle = "rgb(29,143,225)"
    ,colorEnd = "rgb(88,101,185)"
    ,colorStartPos = 0 # Posizione dei colori
    ,colorMiddlePos = 55
    ,colorEndPos = 100)
  
  ,buttonTextColor = "rgb(255,255,255)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(255,255,255)"
  ,buttonTextColorHover = "rgb(0,0,0)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(255,255,255)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)


   body <- dashboardBody(
     customTheme,
     ### aggiunto da Massimo il 25/02/2022
     ### codice per risize automatico grafici ggplot statici
     tags$head(
       tags$style(".fa-envelope {color:#FF0000; font-size: 20px}"), # colore icone nel header
       tags$style(".fa-envelope-open {font-size: 20px}"), 
       tags$style(".fa-cube {font-size: 20px}"),
       tags$style(".fa-question {font-size: 20px}"),
       tags$style(".fa-comment-dollar {font-size: 20px}"),
       tags$script(
       'var dimension = [0, 0];
              $(document).on("shiny:connected", function(e) {
                  dimension[0] = window.innerWidth;
                  dimension[1] = window.innerHeight;
                  Shiny.onInputChange("dimension", dimension);
              });
              $(window).resize(function(e) {
                  dimension[0] = window.innerWidth;
                  dimension[1] = window.innerHeight;
                  Shiny.onInputChange("dimension", dimension);
              });'
     )),
     ### fine codice
   tabItems(
#### HOMEPAGE ####
## Home ----    
    tabItem("biblioshinyy",
            fluidPage(
            fluidRow(
              column(12,
            div(h1("biblioshiny: the shiny app for bibliometrix", 
                   style="text-align:center; font-size:50px;")),
            br(),
            div(h6("Aria, M., & Cuccurullo, C. (2017).", strong(" bibliometrix: An R-tool for comprehensive"), 
               style="text-align:center; font-size:20px;")),
            div(h6(strong("science mapping analysis."), 
                   em("Journal of Informetrics"),", 11(4), 959-975.", 
                   style="text-align:center; font-size:20px;")),
            ),
            column(12,
            div(img(src = "logo.jpg", height = "35%",width = "35%"), style="text-align: center;")
            ),
            #div(img(src = "hexagon.png",height = 600, width = 600), style="text-align: center;"),
            column(12,
            div(p("For an introduction and live examples, visit the ",
              em(a("bibliometrix website.", 
                   href = "https://www.bibliometrix.org", target="_blank")), 
              style="text-align:center; font-size:20px;")),
               )
              )
            )
    ),
#### DATA ####
  ## Load Data ----
    tabItem("loadData",
            fluidPage(
              fluidRow(
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    h3(strong("Import or Load ")),
                    br(),
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
                               em("Period: 1985 - 2018, Source WoS.")
                      )
                    ),
                    #br(),
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
                    #h6("Here accept single .txt/.bib/.csv/.xslx/.RData files, or multiple .txt/.bib/.csv files compressed in a single .zip archive."),
                    conditionalPanel(condition = "input.load != 'null'",
                                     actionButton("applyLoad", strong("START"),
                                     style ="border-radius: 10px; border-width: 3px;",
                                     width = "100%")),
                    tags$hr(),
                    
                    uiOutput("textLog"),
                    #shinycssloaders::withSpinner(verbatimTextOutput("log")),
                    
                    tags$hr(),
                    
                    h3(strong(
                      "Export collection"
                    )),
                    br(),
                    
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
                                                    style ="border-radius: 10px; border-width: 3px;",
                                                    width = "100%")
                                     )
                    ),
                  mainPanel(
                    width = 9,
                    ## color of datatable
                    tags$head(tags$style(
                      HTML(
                        "table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                                  background-color: #9c4242 !important;
                                  }
                                  "
                      )
                    )),
                    tags$style(
                      HTML(
                        ".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                  color: #000000 !important;
                  }"
                      )
                    ),
                    shinycssloaders::withSpinner(DT::DTOutput("contents")
                    )
                  )
                )
              )
            )
      ),
## Gather Data ---- 
    tabItem("gathData",
            fluidPage(
              fluidRow(
                #titlePanel("Gather data using APIs"),
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
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
                    ### Dimenions API ####
                    conditionalPanel(
                      condition = "input.dbapi == 'ds'",
                      br(),
                      actionButton("dsShow",  h5(strong("1.  Configure API request")),
                                   style ="border-radius: 10px; border-width: 3px;",
                                   width = "100%"),
                      h5(tags$b("Your Query")),
                      verbatimTextOutput("queryLog2", placeholder = FALSE),
                      h5(tags$b("Documents returned using your query")),
                      verbatimTextOutput("sampleLog2", placeholder = FALSE),
                      # 
                      # 
                      # uiOutput("sliderLimit"),
                      
                      
                    ),
                    ### Pubmed API ####
                    conditionalPanel(
                      condition = "input.dbapi == 'pubmed'",
                      br(),
                      actionButton("pmShow", h5(strong("1.  Configure API request")),
                                                style ="border-radius: 10px; border-width: 3px;",
                                                width = "100%"),
                      h5(tags$b("Your Query")),
                      verbatimTextOutput("pmQueryLog2", placeholder = FALSE),
                      h5(tags$b("Documents returned using your query")),
                      verbatimTextOutput("pmSampleLog2", placeholder = FALSE),
                      
                    ),
                    tags$hr(),
                    #h4(em(strong("Gather metadata"))),
                    actionButton("apiApply", h5(strong("2.  Download metadata")),
                                 style ="border-radius: 10px; border-width: 3px;",
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
                                     style ="border-radius: 10px; border-width: 3px;",
                                     width = "100%")
                                     )
                    ),
                  mainPanel(width = 9,
                            ## color of datatable
                            tags$head(tags$style(
                              HTML(
                                "table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                                  background-color: #9c4242 !important;
                                  }
                                  "
                              )
                            )),
                            tags$style(
                              HTML(
                                ".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                  color: #000000 !important;
                  }"
                              )
                            ),
                            #shinycssloaders::withSpinner(tableOutput("contents"))
                  shinycssloaders::withSpinner(DT::DTOutput("apiContents"))
                  )
                ))
            )
    ),

## FILTERS ----
    tabItem("filters",
            sidebarLayout(
              sidebarPanel(width=3,
                           h3(strong("Filters")),
                           br(),
                           actionButton("applyFilter", strong("Apply!"),style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           h5(" "),
                           #br(),
                           #span(h5((strong("Dataset Overview "))),style="color:CornflowerBlue"),
                           box(h6(htmlOutput("textDim")),
                               width = "100%"),
                           # box(h5(htmlOutput("textDim")),
                           #     width = "100%"),
                           #htmlOutput("textDim"),
                           br(),
                           #uiOutput("textDim"),
                           uiOutput("selectLA"),
                           uiOutput("sliderPY"),
                           uiOutput("selectType"),
                           uiOutput("sliderTCpY"),
                           #uiOutput("selectSource"),
                           selectInput("bradfordSources", 
                                       label = "Source by Bradford Law Zones",
                                       choices = c("Core Sources"="core", 
                                                   "Core + Zone 2 Sources"="zone2",
                                                   "All Sources"="all"),
                                       selected = "all")
              ),
              mainPanel(width=9,
                        DT::DTOutput("dataFiltered"))
            )
    ),
#### OVERVIEW ####
## Main information ----

tabItem("mainInfo",
                 fluidPage(
                   fluidRow(titlePanel(
                    h2(strong("Main Information"), align = "center"))),
                   fluidRow(
                    # sidebarLayout(
                    # sidebarPanel(width = 6),
                    # mainPanel(width=6,
                               tabsetPanel(type = "tabs",
                                           tabPanel("Plot",
                                                    fluidRow(
                                                      br(),
                                                      column(3,
                                                             valueBoxOutput("Timespan", width = "33vh"),
                                                             valueBoxOutput("au", width = "33vh"),
                                                             valueBoxOutput("kw", width = "33vh")),
                                                      column(3,
                                                             valueBoxOutput("so", width = "33vh"),
                                                             valueBoxOutput("auS1", width = "33vh"),
                                                             valueBoxOutput("cr", width = "33vh")),
                                                      column(3,
                                                             valueBoxOutput("doc", width = "33vh"),
                                                             valueBoxOutput("col", width = "33vh"),
                                                             valueBoxOutput("agePerDoc", width = "33vh")),
                                                    column(3,
                                                           valueBoxOutput("cagr", width = "33vh"),
                                                           valueBoxOutput("coAuPerDoc", width = "33vh"),
                                                           valueBoxOutput("tc", width = "33vh")),
                                                           #shinycssloaders::withSpinner(plotlyOutput(outputId = "RadarPlot", height = "105vh")))
                                                       )
                                                    ),
                                    tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "MainInfo", width = 700)
                                                                 ), align ="center")
                                    )
                               )
                   )
        ),

 #tabItem("mainInfo",
  #          fluidPage(
   #           fluidRow(titlePanel(
    #            h1(strong("Main information about the collection"), align = "center"))),
     #         fluidRow(
      #          column(6,
       #                  tabPanel("Table",
        #                             shinycssloaders::withSpinner(DT::DTOutput(outputId = "MainInfo", width = 700)
         #                                                         )
          #                  )),
          #      column(6,tabPanel("Radar Plot",
          #                           shinycssloaders::withSpinner(plotlyOutput(outputId = "RadarPlot", height = "105vh")) #height = 700))
          #                  ))
          #     )
          #    )
       #),
## Annual Scientific Production ----
    tabItem("annualScPr",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Annual Scientific Production"), align = "center")
              ),
              fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
                           h4(strong("Annual Growth Rate")),
                           br(),
                           verbatimTextOutput("CAGR"),
                           selectInput(
                             'ASPdpi',
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
                           conditionalPanel(condition = "input.ASPdpi != 'null'",
                                            sliderInput(
                                              'ASPh',
                                              h4(em(strong(
                                                "Height (in inches)"
                                              ))),
                                              value = 7, min = 1, max = 20, step = 1),
                                            downloadButton("ASPplot.save", strong("Export plot as png"),
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")
                                            )
                           ),
              mainPanel(width = 10,
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "AnnualProdPlot", height = "75vh")) #height = 700))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("AnnualProdTable"))
                                    ))
              )
            )
    )))),
## Average Citation per Year ----
    tabItem("averageCitPerYear",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Average Citation Per Year"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
                           selectInput(
                             'ACpYdpi',
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
                           conditionalPanel(condition = "input.ACpYdpi != 'null'",
                                            sliderInput(
                                              'ACpYh',
                                              h4(em(strong(
                                                "Height (in inches)"
                                              ))),
                                              value = 7, min = 1, max = 20, step = 1),
                                            downloadButton("ACpYplot.save", strong("Export plot as png"),
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")
                                            )),
              mainPanel(width = 10,
                         tabsetPanel(type = "tabs",
                                     tabPanel("Plot",
                                              shinycssloaders::withSpinner(plotlyOutput(outputId = "AnnualTotCitperYearPlot", height = "75vh")) #height = 700))
                                     ),
                                     tabPanel("Table",
                                              shinycssloaders::withSpinner(DT::DTOutput("AnnualTotCitperYearTable"))
                                     ))
              )
            )
    )))),
## Three fields plot ----
    tabItem("threeFieldPlot",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Three-Field Plot"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=3,
                           actionButton("apply3F", strong("Apply!"),style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
                           box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                               collapsible = TRUE, width = 15,
                               solidHeader = FALSE, collapsed = TRUE,
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
              mainPanel(width=9,
                        #tabPanel("Plot",
                        shinycssloaders::withSpinner(plotlyOutput(outputId = "ThreeFieldsPlot", height = "90vh"))
                        #shinycssloaders::withSpinner(networkD3::sankeyNetworkOutput(outputId = "ThreeFielsPlot",height = "75vh")) #height = "600px"))
                        #            )
              )
            )
    )))),
#### SOURCES ####
## Relevant Sources ----
    tabItem("relevantSources",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Most Relevant Sources"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
                           actionButton("applyMRSources", strong("Apply!"),style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")
                                            )
                           ),
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelSourcesPlot", height = "75vh",width ="98.9%")) #height = 700))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostRelSourcesTable"))
                            ))
              )
            )
    )))),
## Local Cited Sources ----
    tabItem("localCitedSources",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Most Local Cited Sources (from Reference Lists)"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
                           actionButton("applyMLCSources", strong("Apply!"),style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")
                                            )
                           ),
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelCitSourcesPlot", height = "75vh", width = "98.9%")) #height = 700))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostRelCitSourcesTable"))
                            ))
              )
            )
    )))),
## Bradford Law ----
    tabItem("bradford",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Source clustering through Bradford's Law"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")
                                            )
                           ),
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "bradfordPlot", height = "75vh")) #height = 700))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("bradfordTable"))
                            ))
              )
            )
    )))),
## Source Impact ----
    tabItem("sourceImpact",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Source Local Impact"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
                           actionButton("applyHsource", strong("Apply!"),style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
                           box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                               collapsible = TRUE, width = 15,
                               solidHeader = FALSE, collapsed = TRUE,
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")
                                            )
                           ),
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "SourceHindexPlot", height = "75vh",width ="98.9%")) #height = 700))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "SourceHindexTable"))
                            ))
              )
            )
    )))),
## Source Dynamics ----
    tabItem("sourceDynamics",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Source Dynamics"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              # Sidebar with a slider and selection inputs
              sidebarPanel(width=3,
                           actionButton("applySOGrowth", strong("Apply!"),style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
                           box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                               collapsible = TRUE, width = 15,
                               solidHeader = FALSE, collapsed = TRUE,
                           selectInput("cumSO", "Occurrences",
                                       choices = c("Cumulate" = "Cum",
                                                   "Per year" = "noCum"),
                                       selected = "Cum"),
                           # selectInput("SOse", "Confidence Interval",
                           #             choices = c("Yes" = "Yes",
                           #                         "No" = "No"),
                           #             selected = "No"),
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")
                                            )
                           ),
              # 
              mainPanel(width=9,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     #shinycssloaders::withSpinner(plotOutput(outputId = "soGrowthPlot"))
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "soGrowthPlot", height = "90vh"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "soGrowthtable"))
                            ))
              )
            ))))
            ),
#### AUTHORS ####
## Most Relevant Authors ----
    tabItem("mostRelAuthors",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Most Relevant Authors"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
                           actionButton("applyMRAuthors", strong("Apply!"),style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
                           box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                               collapsible = TRUE, width = 15,
                               solidHeader = FALSE, collapsed = TRUE,
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")
                                            )
                           ),
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelAuthorsPlot", height = "75vh",width ="98.9%")) #height = 700))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostRelAuthorsTable"))
                            ))
              )
            )
          )))),
## Most Local Cited Authors ----
    tabItem("mostLocalCitedAuthors",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Most Local Cited Authors"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
                           actionButton("applyMLCAuthors", strong("Apply!"),style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")   
              )),
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostCitAuthorsPlot", height = "75vh",width ="98.9%")) #height = 700))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostCitAuthorsTable"))
                            ))
              )
            )
    )))
    ),
## Authors Production Over Time ----
    tabItem("authorsProdOverTime",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Authors' Production over Time"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
                           actionButton("applyAUoverTime", strong("Apply!"),
                                        style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")   
              )),
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "TopAuthorsProdPlot", height = "75vh",width ="98.9%"))
                            ),
                            tabPanel("Table - Authors' Production per Year",
                                     shinycssloaders::withSpinner(DT::DTOutput("TopAuthorsProdTable"))
                            ),
                            tabPanel("Table - Author's Documents",
                                     shinycssloaders::withSpinner(DT::DTOutput("TopAuthorsProdTablePapers"))
                            ))
              )
            )
    )))
    ),
## Lotka Law ----
    tabItem("lotka",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Author Productivity through Lotka's Law"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")   
              )),
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "lotkaPlot", height = "75vh")) #height = 700))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("lotkaTable"))
                            ))
              )
            )
    )))
    ),
## Author Impact ----
    tabItem("authorImpact",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Author Local Impact"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
                           actionButton("applyHAuthors", strong("Apply!"),style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
                           box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                               collapsible = TRUE, width = 15,
                               solidHeader = FALSE, collapsed = TRUE,
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
                                            downloadButton("AIplot.save", strong("Export plot as png"),style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")   
              )),
              mainPanel(width=10,
                
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "AuthorHindexPlot", height = "75vh",width ="98.9%")) #height = 700))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "AuthorHindexTable"))
                            ))
              )
            )
    )))
    ),
## Most Relevant Affiliations ----
    tabItem("mostRelAffiliations",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Most Relevant Affiliations"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
                           actionButton("applyMRAffiliations", strong("Apply!"),
                                        style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
                           box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                               collapsible = TRUE, width = 15,
                               solidHeader = FALSE, collapsed = TRUE,
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")  
              )),
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelAffiliationsPlot", height = "75vh",width ="98.9%")) #height = 700))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostRelAffiliationsTable"))
                            ))
              )
            )
          )))
          ),
## Corresponding Author Country ----
    tabItem("correspAuthorCountry",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Corresponding Author's Country"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
                           #h3(strong("Corresponding Author's Country")),
                           br(),
                           actionButton("applyCAUCountries", strong("Apply!"),style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")  
              )),
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelCountriesPlot", height = "75vh",width ="98.9%")) #height = 700))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostRelCountriesTable"))
                            ))
              )
            )
          )))
          ),
## Country Scientific Production ----
    tabItem("countryScientProd",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Country Scientific Production"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")  
              )),
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "countryProdPlot", height = "75vh"))  #height = 700))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("countryProdTable"))
                            ))
                )
              )
             )))
            ),
## Most Cited Countries ----
    tabItem("mostCitedCountries",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Most Cited Countries"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
                           actionButton("applyMCCountries", strong("Apply!"),style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
                           box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                               collapsible = TRUE, width = 15,
                               solidHeader = FALSE, collapsed = TRUE,
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")  
              )),
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostCitCountriesPlot", height = "75vh",width ="98.9%")) #height = 700))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostCitCountriesTable"))
                            ))
              )
            )
    )))
    ),

#### DOCUMENTS ####
## Most Global cited Documents ----
    tabItem("mostGlobalCitDoc",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Most Global Cited Documents"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
                           actionButton("applyMGCDocuments", strong("Apply!"),
                                        style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
                           box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                               collapsible = TRUE, width = 15,
                               solidHeader = FALSE, collapsed = TRUE,
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")  
              )),
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostCitDocsPlot", height = "75vh",width ="98.9%")) #height = 700))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostCitDocsTable"))
                            ))
              )
            )
    )))
    ),
## Most Local Cited Documents ----
    tabItem("mostLocalCitDoc",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Most Local Cited Documents"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
                           actionButton("applyMLCDocuments", strong("Apply!"),
                                        style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
                           box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                               collapsible = TRUE, width = 15,
                               solidHeader = FALSE, collapsed = TRUE,
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")  
              )),
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostLocCitDocsPlot", height = "75vh",width ="98.9%")) #height = 700))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostLocCitDocsTable"))
                            ))
              )
             )
            )))
            ),
## Most Local Cited References ----
    tabItem("mostLocalCitRef",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Most Local Cited References"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
                           actionButton("applyMLCReferences", strong("Apply!"),
                                        style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
                           box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                               collapsible = TRUE, width = 15,
                               solidHeader = FALSE, collapsed = TRUE,
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")  
              )),
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostCitRefsPlot", height = "75vh",width ="98.9%")) #height = 700))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostCitRefsTable"))
                            ))
              )
            )
    )))
    ),
## Reference Spectroscopy ----
    tabItem("ReferenceSpect",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Reference Spectroscopy"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=2,
                           actionButton("applyRPYS", strong("Apply!"),style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
                           box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                               collapsible = TRUE, width = 15,
                               solidHeader = FALSE, collapsed = TRUE,
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")  
                           
              )),
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot", 
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "rpysPlot", height = "75vh"))), #height = 700))),
                            tabPanel("Table - RPY", 
                                     shinycssloaders::withSpinner(DT::DTOutput(
                                       outputId = "rpysTable"))),
                            tabPanel("Table - Cited References", 
                                     shinycssloaders::withSpinner(DT::DTOutput(
                                       outputId = "crTable")))
                )
               )
              )
            )))
            ),
## Most Frequent Words ----
    tabItem("mostFreqWords",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Most Frequent Words"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              # Sidebar with a slider and selection inputs
              sidebarPanel(width=2,
                           actionButton("applyMFWords", strong("Apply!"),
                                        style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
                           selectInput("MostRelWords", "Field",
                                       choices = c("Keywords Plus" = "ID",
                                                   "Author's keywords" = "DE",
                                                   "Titles" = "TI",
                                                   "Abstracts" = "AB"),
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
                              Rows have to be separated by return separator (\\n)."))
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%"))  
                           
              ),
              # Show Word Cloud
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelWordsPlot", height = "75vh",width ="98.9%"))  #height = 700))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostRelWordsTable"))
                            ))
                
               )
              )
             )))
            ),
## World Cloud ----
    tabItem("wcloud",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("WordCloud"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              # Sidebar with a slider and selection inputs
              sidebarPanel(width=2,
                           actionButton("applyWordCloud", strong("Apply!"),
                                        style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
                           h4(em(strong(" "))),
                           " ",
                           selectInput("summaryTerms", "Field",
                                       choices = c("Keywords Plus" = "ID",
                                                   "Author's keywords" = "DE",
                                                   "Titles" = "TI",
                                                   "Abstracts" = "AB"),
                                       selected = "ID"),
                           conditionalPanel(condition = "input.summaryTerms == 'AB' |input.summaryTerms == 'TI'",
                                            selectInput("summaryTermsngrams",'N-Grams',
                                                        choices = c("Unigrams" = "1",
                                                                    "Bigrams" = "2",
                                                                    "Trigrams" = "3"),
                                                        selected = 1)),
                           #hr(),
                           numericInput("n_words", label = "Number of words", min = 10, max = 500, step = 1, value = 50),
                           #hr(),
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
                                            #,h5(htmlOutput("WCStopPreview"))
                           ),
                           selectInput("WCSynFile", "Load a list of synonyms",
                                       choices = c("Yes" = "Y",
                                                   "No" = "N"),
                                       selected = "N"),
                           conditionalPanel(condition = "input.WCSynFile == 'Y'",
                                            helpText(h5(strong("Upload a TXT or CSV file containing, in each row, a list of synonyms, that will be merged into a single term (the first word contained in the row)")),
                                                     h5(("Terms have to be separated by a standard separator (comma, semicolon or tabulator). 
                              Rows have to be separated by return separator (\\n)."))
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
                                            #,h5(htmlOutput("WCSynPreview"))
                           )),
                           hr(),
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
              
              # Show Word Cloud
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     wordcloud2::wordcloud2Output("wordcloud", height = "75vh") #height = "600px")
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("wordTable"))
                            ))
                
               )
              )
            )))),
## Tree Map ----
    tabItem("treemap",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("TreeMap"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              # Sidebar with a slider and selection inputs
              sidebarPanel(width=2,
                           actionButton("applyTreeMap", strong("Apply!"),
                                        style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
                           selectInput("treeTerms", "Field",
                                       choices = c("Keywords Plus" = "ID",
                                                   "Author's keywords" = "DE",
                                                   "Titles" = "TI",
                                                   "Abstracts" = "AB"),
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
                              Rows have to be separated by return separator (\\n)."))
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
              
              # Show TreeMap
              mainPanel(width=9,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "treemap", height = "75vh")) #height = 700))
                                     #shinycssloaders::withSpinner(plotOutput("treemap"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("treeTable"))
                            ))
                
               )
              ))
             ))),
## Word Dynamics ----
    tabItem("wordDynamics",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Word Dynamics"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              # Sidebar with a slider and selection inputs
              sidebarPanel(width=3,
                           actionButton("applyWD", strong("Apply!"),
                                        style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
                           selectInput("growthTerms", "Field",
                                       choices = c("Keywords Plus" = "ID",
                                                   "Author's keywords" = "DE",
                                                   "Titles" = "TI",
                                                   "Abstracts" = "AB"),
                                       selected = "ID"),
                           box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                               collapsible = TRUE, width = 15,
                               solidHeader = FALSE, collapsed = TRUE,
                           selectInput("cumTerms", "Occurrences",
                                       choices = c("Cumulate" = "Cum",
                                                   "Per year" = "noCum"),
                                       selected = "Cum"),
                           conditionalPanel(condition = "input.growthTerms == 'AB' |input.growthTerms == 'TI'",
                                            selectInput("growthTermsngrams",'N-Grams',
                                                        choices = c("Unigrams" = "1",
                                                                    "Bigrams" = "2",
                                                                    "Trigrams" = "3"),
                                                        selected = 1)),
                           sliderInput("topkw", label = "Number of words", min = 1, max = 100, step = 1, value = c(1,10))),                                   
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
                              Rows have to be separated by return separator (\\n)."))
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
                           hr(),
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")  
                           
              )),
              
              # 
              mainPanel(width=9,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     #shinycssloaders::withSpinner(plotOutput(outputId = "kwGrowthPlot"))
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "kwGrowthPlot", height = "90vh")) #height = 700))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "kwGrowthtable"))
                            ))
                
                )
               )
             )))
            ),
## Trend Topic ----
    tabItem("trendTopic",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Trend Topics"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              # Sidebar with a slider and selection inputs
              sidebarPanel(width=2,
                           actionButton("applyTrendTopics", strong("Apply!"),
                                        style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           #h4(em(strong("Method Parameters:"))),
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
                              Rows have to be separated by return separator (\\n)."))
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
                           hr(),
                           box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                               collapsible = TRUE, width = 15,
                               solidHeader = FALSE, collapsed = TRUE,
                           #uiOutput("trendMinFreq"),
                           fluidRow(column(6,
                                           numericInput("trendMinFreq", label = "Word Minimum Frequency", min = 0, max = 100, value = 5, step = 1),
                           ),
                           column(6,
                                  numericInput("trendNItems", label = "Number of Words per Year", min = 1, max = 20, step = 1, value = 5)
                           ))),
                           #sliderInput("trendSize", label = "Word label size", min = 0, max = 20, step = 1, value = 5),
                           selectInput(
                             'TTdpi',
                             h4(strong(
                               "Export plot"
                             )),
                             choices=c(
                               "Please select a dpi value" = "null",
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")  
                           
                           
              )),
              
              # 
              mainPanel(width=10,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "trendTopicsPlot", height = "90vh",width ="98.9%")) #height = 700))
                                     #shinycssloaders::withSpinner(plotOutput(outputId = "trendTopicsPlot"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "trendTopicsTable"))
                            ))
                
               )
             )
            )))
            ),
#### COUPLING ####
## Clustering by Coupling ----
    tabItem("coupling",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Clustering by Coupling"), align = "center")
                ),
                fluidRow(
            tabPanel("Clustering by Coupling",
                     sidebarLayout(
                       sidebarPanel(width=2,
                                    actionButton("applyCM", strong("Apply!"),
                                                 style ="border-radius: 10px; border-width: 3px;",
                                                 width = "100%"),
                                    br(),
                                    br(),
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
                                      solidHeader = FALSE, collapsed = TRUE,
                                    #selectInput("cmNP", 
                                    #            label = h4(em(strong("Parameters: "))),
                                    #            choices = c("Hide Parameters" = "hide", 
                                    #                        "Show Parameters" = "show"),
                                    #            selected = "hide"),
                                    #conditionalPanel(condition = "input.cmNP == 'show'", 
                                                     #"  ",
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
                                                                     numericInput("sizeCM", label="Label size",value=0.3,min=0.0,max=1,step=0.05)))
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
                                                                    style ="border-radius: 10px; border-width: 3px;",
                                                                    width = "100%")
                       )),
                       mainPanel(width=10,
                         #"Clustering by Coupling",
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Map",
                                                      shinycssloaders::withSpinner(plotlyOutput(outputId = "CMPlot", height = "75vh")) #height = 700))
                                             ),
                                             tabPanel("Network",
                                                      shinycssloaders::withSpinner(visNetworkOutput("CMNetPlot", height = "75vh"))), #height = "750px",width = "1100px"))),
                                             tabPanel("Table",
                                                      shinycssloaders::withSpinner(DT::DTOutput(outputId = "CMTable"))
                                             ),
                                             tabPanel("Clusters",
                                                      shinycssloaders::withSpinner(DT::DTOutput(outputId = "CMTableCluster"))
                                             )
                                 )
                       )
                     )
                   )
                 )))
            ),
#### CONCEPTUAL STRUCTURE ####
## Co-occurence Network ----
    tabItem("coOccurenceNetwork",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Co-occurrence Network"), align = "center")
                ),
                fluidRow(
            tabPanel("Co-occurrence Network",
                     
                     sidebarLayout(
                       
                       sidebarPanel(width=3,
                                    actionButton("applyCoc", strong("Apply!"),
                                                 style ="border-radius: 10px; border-width: 3px;",
                                                 width = "100%"),
                                    br(),
                                    br(),
                                    #"  ",
                                    #h4(em(strong("Network Parameters: "))),
                                    selectInput("field", 
                                                "Field",
                                                choices = c("Keywords Plus" = "ID", 
                                                            "Author's Keywords" = "DE",
                                                            "Titles" = "TI",
                                                            "Abstracts" = "AB"),
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
                              Rows have to be separated by return separator (\\n)."))
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
                                                                                    "InfoMap" = "infomap",
                                                                                    "Leading Eigenvalues" = "leading_eigen",
                                                                                    "Louvain" = "louvain",
                                                                                    "Spinglass" = "spinglass",
                                                                                    "Walktrap" = "walktrap"),
                                                                        selected = "louvain")
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
                                                     )),
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
                                    #uiOutput("Focus"),
                                    br(),
                                    #h4(em(strong("Graphical Parameters: "))),
                                    #br(),
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
                                                                   style ="border-radius: 10px; border-width: 3px;",
                                                                   width = "100%")
                                    ),
                                    column(6,
                                           downloadButton("networkCoc.fig", strong("Save HTML"),
                                                          style ="border-radius: 10px; border-width: 3px;",
                                                          width = "100%")
                                    )
                       )),
                       
                       mainPanel(width=9,
                         tabsetPanel(type = "tabs",
                                     tabPanel("Network", 
                                              shinycssloaders::withSpinner(visNetworkOutput("cocPlot", height = "75vh"))), #height = "750px",width = "1100px"))),
                                     # tabPanel("Communities", 
                                     #          shinycssloaders::withSpinner(visNetworkOutput("cocPlotComm", height = "75vh"))), #height = "750px",width = "1100px"))),
                                     tabPanel("Table", 
                                              shinycssloaders::withSpinner(DT::DTOutput(
                                                outputId = "cocTable"))),
                                     tabPanel("Degree Plot", 
                                              shinycssloaders::withSpinner(plotlyOutput(outputId = "cocDegree", height = "75vh"))) #height=700)))
                         )
                        )
                       )
                      )
                     )))
            ),
## Thematic Map ----
    tabItem("thematicMap",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Thematic Map"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=3,
                           actionButton("applyTM", strong("Apply!"),
                                        style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
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
                              Rows have to be separated by return separator (\\n)."))
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
                                            ))
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")
              )),
              mainPanel(width=9,
                #"Thematic Map",
                        tabsetPanel(type = "tabs",
                                    tabPanel("Map",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot", height = "75vh")) #height = 700))
                                    ),
                                    tabPanel("Network",
                                             shinycssloaders::withSpinner(visNetworkOutput("NetPlot", height = "75vh"))), #height = "750px",width = "1100px"))),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable"))
                                    ),
                                    tabPanel("Clusters",
                                             shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTableCluster"))
                                    )
                        )
                      )
                    )
              )))
            ),
## Thematic Evolution ----
    tabItem("thematicEvolution",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Thematic Evolution"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=3,
                           actionButton("applyTE", strong("Apply!"),
                                        style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
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
                              Rows have to be separated by return separator (\\n)."))
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
                                                   numericInput("TEn.labels", label="Number of Labels (for each cluster)",value=1,min=1,max=5,step=1)
                                            ))
                           ),
                           br(),
                           box(title = p(strong("Time Slices"),style='font-size:16px;color:black;'), 
                               collapsible = FALSE, width = 15,
                               solidHeader = FALSE, collapsed = FALSE,
                           numericInput("numSlices", label="Number of Cutting Points",min=1,max=4,value=1),
                           "Please, write the cutting points (in year) for your collection",
                           uiOutput("sliders")
              )),
              mainPanel(width=9,
                #"Thematic Evolution",
                        tabsetPanel(type = "tabs",
                                    tabPanel("Thematic Evolution", tabsetPanel(type="tabs",
                                                                               tabPanel("Map",
                                                                                        #shinycssloaders::withSpinner(networkD3::sankeyNetworkOutput(outputId = "TEPlot", height = "75vh"))  #height = "600px"))
                                                                                        shinycssloaders::withSpinner(plotlyOutput(outputId = "TEPlot", height = "75vh"))
                                                                               ),
                                                                               tabPanel("Table",
                                                                                        shinycssloaders::withSpinner(DT::DTOutput(outputId = "TETable"))
                                                                               ))
                                    ),
                                    tabPanel("Time Slice 1", tabsetPanel(type="tabs",
                                                                         tabPanel("Map",
                                                                                  shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot1", height = "75vh")) #height = 700))
                                                                         ),
                                                                         tabPanel("Network",
                                                                                  shinycssloaders::withSpinner(visNetworkOutput("NetPlot1", height = "75vh")) # height = "750px",width = "1100px"))
                                                                         ),
                                                                         tabPanel("Table",
                                                                                  shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable1"))
                                                                         ),
                                                                         tabPanel("Clusters",
                                                                                  shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTableCluster1"))
                                                                         )
                                    )      
                                    ),
                                    tabPanel("Time Slice 2", tabsetPanel(type="tabs",
                                                                         tabPanel("Map",
                                                                                  shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot2", height = "75vh"))  #height = 700))
                                                                         ),
                                                                         tabPanel("Network",
                                                                                  shinycssloaders::withSpinner(visNetworkOutput("NetPlot2", height = "75vh")) #height = "750px",width = "1100px"))
                                                                         ),
                                                                         tabPanel("Table",
                                                                                  shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable2"))
                                                                         ),
                                                                         tabPanel("Clusters",
                                                                                  shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTableCluster2"))
                                                                         )
                                    ) 
                                    ),
                                    tabPanel("Time Slice 3", tabsetPanel(type="tabs",
                                                                         tabPanel("Map",
                                                                                  shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot3", height = "75vh")) #height = 700))
                                                                         ),
                                                                         tabPanel("Network",
                                                                                  shinycssloaders::withSpinner(visNetworkOutput("NetPlot3", height = "75vh")) #height = "750px",width = "1100px"))
                                                                         ),
                                                                         tabPanel("Table",
                                                                                  shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable3"))
                                                                         ),
                                                                         tabPanel("Clusters",
                                                                                  shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTableCluster3"))
                                                                         )
                                    )
                                    ),
                                    tabPanel("Time Slice 4", tabsetPanel(type="tabs",
                                                                         tabPanel("Map",
                                                                                  shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot4", height = "75vh")) #height = 700))
                                                                         ),
                                                                         tabPanel("Network",
                                                                                  shinycssloaders::withSpinner(visNetworkOutput("NetPlot4", height = "75vh")) #height = "750px",width = "1100px"))
                                                                         ),
                                                                         tabPanel("Table",
                                                                                  shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable4"))
                                                                         ),
                                                                         tabPanel("Clusters",
                                                                                  shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTableCluster4"))
                                                                         )
                                    ) 
                                    ),
                                    tabPanel("Time Slice 5", tabsetPanel(type="tabs",
                                                                         tabPanel("Map",
                                                                                  shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot5", height = "75vh")) #height = 700))
                                                                         ),
                                                                         tabPanel("Network",
                                                                                  shinycssloaders::withSpinner(visNetworkOutput("NetPlot5", height = "75vh")) #height = "750px",width = "1100px"))
                                                                         ),
                                                                         tabPanel("Table",
                                                                                  shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable5"))
                                                                         ),
                                                                         tabPanel("Clusters",
                                                                                  shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTableCluster5"))
                                                                         )
                                     )
                                    )
                        )
              )
             )
            )))),
## Factorial Analysis ----
    tabItem("factorialAnalysis",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Factorial Analysis"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=3,
                           actionButton("applyCA", strong("Apply!"),
                                        style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
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
                              Rows have to be separated by return separator (\\n)."))
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
                           #h4(em(strong("FA Parameters: "))),
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
                                            #h4(em(strong("Graphical Parameters: "))),
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%"),
                                            h4(" "),
                                            downloadButton("FA2plot.save", strong("Topic Dendrogram "),
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%"),
                                            h4(" "),
                                            downloadButton("FA3plot.save", strong("Most Contributing Map "),
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%"),
                                            h4(" "),
                                            downloadButton("FA4plot.save", strong("Most Cited Map "),
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")
                           )
                           ),
              mainPanel(width=9,
                #"Factorial Analysis",
                        tabsetPanel(type = "tabs",
                                    tabPanel("Word Map", 
                                             shinycssloaders::withSpinner(plotOutput(
                                               outputId = "CSPlot1"))),
                                    tabPanel("Topic Dendrogram", 
                                             shinycssloaders::withSpinner(plotOutput(
                                               outputId = "CSPlot4"))),
                                    tabPanel("Table - Most Contributing Papers", 
                                             shinycssloaders::withSpinner(plotOutput(
                                               outputId = "CSPlot2"))),
                                    tabPanel("Table - Most Cited Papers", 
                                             shinycssloaders::withSpinner(plotOutput(
                                               outputId = "CSPlot3"))),
                                    tabPanel("Table - Words by Cluster",
                                             shinycssloaders::withSpinner(DT::DTOutput(outputId = "CSTableW"))),
                                    tabPanel("Table - Articles by Cluster",
                                             shinycssloaders::withSpinner(DT::DTOutput(outputId = "CSTableD")))
                                    
                        )
               )
              )
             )))),
#### INTELLECTUAL STRUCTURE ####
## Co-Citation Network ----
    tabItem("coCitationNetwork",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Co-citation Network"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=3,
                           actionButton("applyCocit", strong("Apply!"),
                                        style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
                           #"  ",
                           #h4(em(strong("Network Parameters: "))),
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
                                                                           "InfoMap" = "infomap",
                                                                           "Leading Eigenvalues" = "leading_eigen",
                                                                           "Louvain" = "louvain",
                                                                           "Spinglass" = "spinglass",
                                                                           "Walktrap" = "walktrap"),
                                                               selected = "louvain")
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
                           #uiOutput("Focus"),
                           #br(),
                           #h4(em(strong("Graphical Parameters: "))),
                           #br(),
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
                                                          style ="border-radius: 10px; border-width: 3px;",
                                                          width = "100%")
                           ),
                           column(6,
                                  downloadButton("networkCocit.fig", strong("Save HTML"),
                                                 style ="border-radius: 10px; border-width: 3px;",
                                                 width = "100%")
                           )
              )),
              mainPanel(width=9,
                tabsetPanel(type = "tabs",
                            tabPanel("Newtwork", 
                                     shinycssloaders::withSpinner(visNetworkOutput("cocitPlot", height = "75vh"))), #height = "750px", width = "1100px"))),         
                            tabPanel("Table", 
                                     shinycssloaders::withSpinner(DT::DTOutput(
                                       outputId = "cocitTable"))),
                            tabPanel("Degree Plot", 
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "cocitDegree", height=700)))
                )
                #shinycssloaders::withSpinner(plotOutput(outputId = "cocitPlot"))
              )
             )
            )))
            ),
## Historiograph ----
    tabItem("historiograph",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Historiograph"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              sidebarPanel(
                width=3,
                actionButton("applyHist", strong("Apply!"),style ="border-radius: 10px; border-width: 3px;",
                             width = "100%"),
                #selectInput('save_colnet', 'Save network as:', choices = c('No, thanks!' = 'no_thanks', 'Pajek format' = 'pajek')),
                #conditionalPanel(condition = "input.save_colnet == 'pajek'"
                br(),
                br(),
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
                    solidHeader = FALSE, collapsed = TRUE,
                selectInput(inputId = "titlelabel",
                            label = "Node label",
                            choices = c("Short id (1st Author, Year)" = "FALSE",
                                        "Document Title" = "TRUE"),
                            selected = "FALSE"),
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
                selectInput(
                  'HGdpi',
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
                conditionalPanel(condition = "input.HGdpi != 'null'",
                                 sliderInput(
                                   'HGh',
                                   h4(em(strong(
                                     "Height (in inches)"
                                   ))),
                                   value = 7, min = 1, max = 20, step = 1),
                                 downloadButton("HGplot.save", strong("Export plot as png"),
                                                style ="border-radius: 10px; border-width: 3px;",
                                                width = "100%")  
                
              )
              ),
              mainPanel(width=9,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot", 
                                     #shinycssloaders::withSpinner(plotOutput(outputId = "histPlot"))),
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "histPlot", height = "75vh"))),
                            tabPanel("Table", 
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "histTable")))
                )
              )
              #plotOutput(outputId = "histPlot"))
            )
           )))
           ),
#### SOCIAL STRUCTURE ####
## Collaboration Network ----
    tabItem("collabNetwork",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Collaboration Network"), align = "center")
                ),
                fluidRow(
            sidebarLayout(
              
              sidebarPanel(width=3,
                           actionButton("applyCol", strong("Apply!"),style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
                           #"  ",
                           #h4(em(strong("Network Parameters: "))),
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
                                                                           "InfoMap" = "infomap",
                                                                           "Leading Eigenvalues" = "leading_eigen",
                                                                           "Louvain" = "louvain",
                                                                           "Spinglass" = "spinglass",
                                                                           "Walktrap" = "walktrap"),
                                                               selected = "louvain")
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
                           #uiOutput("Focus"),
                           #h4(em(strong("Graphical Parameters: "))),
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
                                                          style ="border-radius: 10px; border-width: 3px;",
                                                          width = "100%")
                           ),
                           column(6,
                                  downloadButton("networkCol.fig", strong("Save HTML"),
                                                 style ="border-radius: 10px; border-width: 3px;",
                                                 width = "100%")
                           )
                           )),
              mainPanel(width=9,
                tabsetPanel(type = "tabs",
                            tabPanel("Network", 
                                     shinycssloaders::withSpinner(visNetworkOutput("colPlot", height = "75vh"))), #,width = "1100px"))), 
                            #shinycssloaders::withSpinner(plotOutput(outputId = "colPlot"))),
                            tabPanel("Table", 
                                     shinycssloaders::withSpinner(DT::DTOutput(
                                       outputId = "colTable"))),
                            tabPanel("Degree Plot", 
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "colDegree", height = "75vh"))) #height=700)))
                )
                #shinycssloaders::withSpinner(plotOutput(outputId = "colPlot"))
              )
             )
            )))
            ),
## Collaboration World Map ----
    tabItem("collabWorldMap",
            fluidPage(
              fluidRow(
                titlePanel(
                  h2(strong("Collaboration WorldMap"), align = "center")),
                fluidRow(
            sidebarLayout(
              sidebarPanel(width=3,
                           actionButton("applyWM", strong("Apply!"),
                                        style ="border-radius: 10px; border-width: 3px;",
                                        width = "100%"),
                           br(),
                           br(),
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
                                                           style ="border-radius: 10px; border-width: 3px;",
                                                           width = "100%")  
              )),
              mainPanel(width=9,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot", 
                                     shinycssloaders::withSpinner(plotOutput(outputId = "WMPlot"))),
                            tabPanel("Table", 
                                     shinycssloaders::withSpinner(DT::DTOutput(
                                       outputId = "WMTable")))
                )
                #shinycssloaders::withSpinner(plotOutput(outputId = "colPlot"))
              )
            ))))
          ))
)

#### UI ####
   
ui <- shinydashboardPlus::dashboardPage(header, sidebar, body,
                                        options = list(sidebarExpandOnHover = TRUE),
                                        scrollToTop =TRUE,
                                        controlbar = NULL)
                                        
   
