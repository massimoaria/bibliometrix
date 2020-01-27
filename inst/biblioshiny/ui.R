## BIBLIOSHINY: A SHINY APP FOR BIBLIOMETRIX R-PACKAGE
if (!(require(bibliometrix))){install.packages("bibliometrix"); require(bibliometrix, quietly=TRUE)}
if (!(require(shiny))){install.packages("shiny"); require(shiny, quietly=TRUE)} 
#if (!(require(shinyFiles))){install.packages("shiny"); require(shinyFiles, quietly=TRUE)} 
#if (!(require(fs))){install.packages("shiny"); require(fs, quietly=TRUE)} 
if (!(require(rio))){install.packages("rio")} 
if (!(require(DT))){install.packages("DT")} 
if (!(require(ggplot2))){install.packages("ggplot2"); require(ggplot2, quietly=TRUE)} 
if (!(require(shinycssloaders))){install.packages("shinycssloaders")} 
if (!(require(shinythemes))){install.packages("shinythemes")} 
if (!(require(wordcloud2))){install.packages("wordcloud2")} 
if (!require(colourpicker)){install.packages("colourpicker")}
if (!require(treemap)){install.packages("treemap")}
if (!require(ggmap)){install.packages("ggmap"); require(ggmap, quietly=TRUE)}
if (!require(maps)){install.packages("maps"); require(maps, quietly=TRUE)}
if (!require(visNetwork)){install.packages("visNetwork"); require(visNetwork, quietly=TRUE)}
if (!require(plotly)){install.packages("plotly"); require(plotly, quietly=TRUE)}
require(Matrix, quietly = TRUE)

# Main NavBar ----
options(spinner.size=1, spinner.type=5)

ui <-  navbarPage("biblioshiny for bibliometrix",
                  theme=shinythemes::shinytheme("flatly"),
                  
### WELCOME PAGE ----
                  tabPanel("Welcome",
                           fluidRow(
                             column(9,
                                    wellPanel(
                                          
                                      #shinythemes::themeSelector(),
                                      
                               h1("biblioshiny: The shiny app for bibliometrix",align = "center"),
                               br(),
                               h4(em(strong("bibliometrix: An R-tool for comprehensive science mapping analysis")),align = "center"),
                               br(),
                               h6("Aria, M., & Cuccurullo, C. (2017).", strong(" bibliometrix: An R-tool for comprehensive science mapping analysis."), em(" Journal of Informetrics"),", 11(4), 959-975.", align="center"),
                               br(),
                               div(img(src = "logo.jpg", height = 536, width = 463), style="text-align: center;"),
                               br(),
                               br(),
                               h2("Features"),
                               p(em("bibliometrix")," is an open-source tool for executing a comprehensive science mapping analysis of scientific literature."),
                               br(),
                               p("It was programmed in R language to be flexible and facilitate integration with other statistical and graphical packages.
                                 Indeed, bibliometrics is a constantly changing science and bibliometrix has the flexibility to be quickly upgraded and integrated. 
                                 Its development can address a large and active community of developers formed by prominent researchers."),
                               br(),
                               p(em("bibliometrix"),"provides various routines for importing bibliographic data from SCOPUS, 
                                 Clarivate Analytics' Web of Science, Dimensions, PubMed and Cochrane databases, performing bibliometric 
                                 analysis and building data matrices for co-citation, coupling, scientific collaboration analysis and co-word analysis."),
                               br(),
                               p("For an introduction and live examples, visit the ",
                                 a("bibliometrix website.", 
                                   href = "https://www.bibliometrix.org")),
                               br(),
                               
                               h2("Workflow"),
                               br(),
                               h4(em("bibliometrix")," supports the main stages of the recommended science mapping workflow:"),
                               br(),
                               div(img(src = "workflow.jpg", height=346, width=800), style="text-align: center;"),
                               br(),
                               
                               h2("Example"),
                               br(),
                               p("Step 1 - Download an example at the following", a("link",href = "https://www.bibliometrix.org/datasets/joi.zip", target="_blank"),
                               ". It includes all articles published by the", em("Journal of Informetrics"), "from 2007 to 2017."),  
                               p("Step 2 - In the ",strong("Load ") ,"menu, select ",strong("'Web of Knowledge'")," as database and ",strong("'Plaintext'")," as file format."),
                               p("Step 3 - Choose and load the file", strong("joi.zip")," using the ",strong("browse")," button."),
                               p("Step 4 - ", strong(em("Then, enjoy working with the app!"))),
                               br()
                              )
                               
                             )
                           )
),

### Loading page ----

tabPanel(
  "Data", 
  sidebarLayout(
    sidebarPanel(width=3,
                 h3(em(strong("Import or Load "))),
                 br(),
                 selectInput("load", 
                             label = "Please, choose what to do",
                             choices = c(" "="null",
                                         "Import raw file(s)"="import", 
                                         "Load bibliometrix file(s)"="load"),
                             selected = "null"),
                 #br(),
     conditionalPanel(condition = "input.load == 'import'",
                  selectInput("dbsource", 
                  label = "Database",
                  choices = c("Web of Science (WoS/WoK)"="isi", 
                              "Scopus"="scopus",
                              "Dimensions"="dimensions"),
                  selected = "isi")),
      conditionalPanel(condition = "input.dbsource == 'isi' & input.load == 'import'",
                       selectInput("format", 
                                   label = "File format",
                                   choices = c("Plain Text"="plaintext", 
                                               "BibTeX"="bibtex"),
                                   selected = "plaintext")),
      conditionalPanel(condition = "input.dbsource == 'dimensions' & input.load == 'import'",
                       selectInput("format", 
                                   label = "File format",
                                   choices = c("Excel (Topic Analysis)"="excel",
                                               "CSV (bibliometric mapping)"="csv"),
                                   selected = "excel")),
      
     
     conditionalPanel(condition = "input.load != 'null'",
     fileInput("file1", "Choose a file",
                multiple = FALSE,
                accept = c(
                  ".csv",
                  ".txt",
                  ".bib",
                  ".xlsx",
                  ".zip",
                  ".xls",
                  ".rdata",
                  ".rda",
                  ".rds")
      )),
      #h6("Here accept single .txt/.bib/.csv/.xslx/.RData files, or multiple .txt/.bib/.csv files compressed in a single .zip archive."),
     conditionalPanel(condition = "input.load != 'null'",
                      actionButton("applyLoad", "Start ")),
      tags$hr(),
      
      uiOutput("textLog"),
      #shinycssloaders::withSpinner(verbatimTextOutput("log")),
      
      tags$hr(),
      
      h3(em(strong("Export a bibliometrix file "))),
      br(),
     
      ### download xlsx
     #  selectInput('save_file', 'Save as:', choices = c(' ' ='null',
     #                                                   'Excel/R format' = 'xlsx'),
     #              selected = 'null'),
     # ### prova
     # conditionalPanel(condition = "input.save_file != 'null'",
     # shinySaveButton("save", "Save file", "Save file as ...", filetype=list(xlsx="xlsx", RData="RData")))#,
     # 
     # ###FINE PROVA
     ### download xlsx
     selectInput('save_file', 'Save as:', choices = c(' ' ='null',
                                                      'Excel' = 'xlsx',
                                                      'R Data Format' = 'RData'),
                 selected = 'null'),
     conditionalPanel(condition = "input.save_file != 'null'",
                      downloadButton("collection.save", "Export"))
     ),
     mainPanel(
      ## color of datatable
      tags$head(tags$style(HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                                  background-color: #9c4242 !important;
                                  }
                                  "))),
      tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                  color: #000000 !important;
                  }")),
      #shinycssloaders::withSpinner(tableOutput("contents"))
      shinycssloaders::withSpinner(DT::DTOutput("contents"))
    )
  )
),


### Filters page ----
           tabPanel("Filter",
                    sidebarLayout(
                      
                      sidebarPanel(width=3,
                                   h3(em(strong("Filter "))),
                                   br(),
                                   uiOutput("textDim"),
                                   uiOutput("selectType"),
                                   uiOutput("sliderPY"),
                                   uiOutput("sliderTC"),
                                   #uiOutput("selectSource"),
                                   selectInput("bradfordSources", 
                                               label = "Source by Bradford Law Zones",
                                               choices = c("Core Sources"="core", 
                                                           "Core + Zone 2 Sources"="zone2",
                                                           "All Sources"="all"),
                                               selected = "all")
                      ),
                      mainPanel(DT::DTOutput("dataFiltered"))
                    )
                    
           ),


### DATASET MENU ----
navbarMenu("Dataset",
           "  ",
           "  ",
           tabPanel("Main Information",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Main Information about the collection "))),
                                   br()),
                      mainPanel(
                        shinycssloaders::withSpinner(DT::DTOutput(outputId = "MainInfo"))
                    )
           )),
           tabPanel("Annual Scientific Production",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   "  ",
                                   "  ",
                                   h3(em(strong("Annual Scientific Production "))),
                                   "  ",
                                   br(),
                                   verbatimTextOutput("CAGR")
                                   #uiOutput("textCAGR")
                                   ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "AnnualProdPlot", height = 700))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("AnnualProdTable"))
                                    ))
                      )
                    )
           ),
           tabPanel("Average Citations per Year",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   "  ",
                                   "  ",
                                   h3(em(strong("Average Citations per Year "))),
                                   "  "
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "AnnualTotCitperYearPlot", height = 700))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("AnnualTotCitperYearTable"))
                                    ))
                      )
                    )
           ),
           tabPanel("Three-Fields Plot",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   "  ",
                                   "  ",
                                   h3(em(strong("Three-Fields Plot "))),
                                   "  ",
                                   actionButton("apply3F", "Apply!"),
                                   br(),
                                   selectInput("CentralField",
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
                                               selected = "AU"),
                                  sliderInput("CentralFieldn", 
                                                label=("Middle Field: Number of items"), 
                                                min = 1, max = 50, step = 1, value = 20),
                                   selectInput("LeftField",
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
                                                           selected = "CR"),
                                  sliderInput("LeftFieldn", 
                                              label=("Left Field: Number of items"), 
                                              min = 1, max = 50, step = 1, value = 20),
                                   selectInput("RightField",
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
                                                           selected = "DE"),
                                  sliderInput("RightFieldn", 
                                              label=("Right Field: Number of items"), 
                                              min = 1, max = 50, step = 1, value = 20)
                                   ),
                      mainPanel(
                        #tabPanel("Plot",
                                 shinycssloaders::withSpinner(networkD3::sankeyNetworkOutput(outputId = "ThreeFielsPlot",height = "600px"))
                        #            )
                      )
                    )
           )
),

### SOURCES MENU ----
navbarMenu("Sources",
           "  ",
           "  ",
           #### MOST RELEVANT SOURCES ----
           tabPanel("Most Relevant Sources",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Most Relevant Sources "))),
                                   br(),
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   numericInput("MostRelSourcesK", 
                                                label=("Number of Sources"), 
                                                value = 20)
                                   ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelSourcesPlot",height = 700))
                                    ),
                                    tabPanel("Table",
                                              shinycssloaders::withSpinner(DT::DTOutput("MostRelSourcesTable"))
                                    ))
                      )
                      )
                    ),
           #### MOST RELEVANT CITED SOURCES ----
           tabPanel("Most Local Cited Sources",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Most Local Cited Sources (from Reference Lists)"))),
                                   br(),
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   numericInput("MostRelCitSourcesK", 
                                                label=("Number of Sources"), 
                                                value = 20)
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelCitSourcesPlot",height = 700))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("MostRelCitSourcesTable"))
                                    ))
                      )
                    )
           ),
           ### BRADFORD LAW ----
           tabPanel("Bradford's law",
                    
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Source clustering"))),
                                   h4(em(strong("through Bradford's Law "))),
                                   br()            
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "bradfordPlot", height = 700))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("bradfordTable"))
                                    ))
                      )
                      
                    )
           ),
           ### SOURCE IMPACT MENU ----
           tabPanel("Source Impact",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Source Impact"))),
                                   br(),            
                                   actionButton("applyHsource", "Apply!"),
                                   "  ",
                                   "  ",
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
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
                                                value = 20)
                      ),
                      mainPanel(
                        
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "SourceHindexPlot", height = 700))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput(outputId = "SourceHindexTable"))
                                    ))
                      )
                      
                    )
           ),
           ### SOURCE DYNAMICS MENU ----
           tabPanel("Source Dynamics",
                    
                    sidebarLayout(
                      # Sidebar with a slider and selection inputs
                      sidebarPanel(width=3,
                                   h3(em(strong("Source Dynamics"))),
                                   br(),   
                                   selectInput("cumSO", "Occurrences",
                                               choices = c("Cumulate" = "Cum",
                                                           "Per year" = "noCum"),
                                               selected = "noCum"),
                                   selectInput("SOse", "Confidence Interval",
                                               choices = c("Yes" = "Yes",
                                                           "No" = "No"),
                                               selected = "No"),
                                   hr(),
                                   sliderInput("topSO", label = "Number of Sources", min = 1, max = 50, step = 1, value = 5)
                                   
                                   #uiOutput("sliderKwYears")
                      ),
                      
                      # 
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotOutput(outputId = "soGrowthPlot"))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput(outputId = "soGrowthtable"))
                                    ))
                        
                      )
                    ))
           
),
### AUTHORS MENU ----
navbarMenu("Authors",
           "  ",
           "  ",
           ### MOST RELEVANT AUTHORS ----
           "Authors",
           tabPanel("Most Relevant Authors",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Most Relevant Authors"))),
                                   br(),
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   numericInput("MostRelAuthorsK", 
                                                label=("Number of Authors"), 
                                                value = 20),
                                   "  ",
                                   selectInput("AuFreqMeasure", 
                                               label = "Frequency measure",
                                               choices = c("N. of Documents "="t", 
                                                           "Percentage"="p",
                                                           "Fractionalized Frequency"="f"),
                                               selected = "t")
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelAuthorsPlot", height = 700))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("MostRelAuthorsTable"))
                                    ))
                      )
                    )
           ),
           
           ### MOST LOCAL CITED AUTHORS
           tabPanel("Most Local Cited Authors",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Most Local Cited Authors"))),
                                   br(),
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   numericInput("MostCitAuthorsK", 
                                                label=("Number of Authors"), 
                                                value = 20)
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "MostCitAuthorsPlot", height = 700))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("MostCitAuthorsTable"))
                                    ))
                      )
                    )
           ),
           
           ### AUTHORS' PRODUCTION OVER TIME  ----
           tabPanel("Authors' Production over Time",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Authors' Production over Time"))),
                                   br(),
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   numericInput("TopAuthorsProdK", 
                                                label=("Number of Authors"), 
                                                value = 20)
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "TopAuthorsProdPlot", height=700))
                                    ),
                                    tabPanel("Table - Top Authors' Production per Year",
                                             shinycssloaders::withSpinner(DT::DTOutput("TopAuthorsProdTable"))
                                    ),
                                    tabPanel("Table - Top Author's Documents",
                                             shinycssloaders::withSpinner(DT::DTOutput("TopAuthorsProdTablePapers"))
                                    ))
                      )
                    )
           ),
           

           ### LOTKA LAW ----
           tabPanel("Lotka's law",
                    
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Author Productivity"))),
                                   h4(em(strong("through Lotka's Law"))),
                                   br()         
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "lotkaPlot", height = 700))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("lotkaTable"))
                                    ))
                      )
                      
                    )
           ),
           ### AUTHOR IMPACT ----
           tabPanel("Author Impact",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Author Impact"))),
                                   br(),
                                   actionButton("applyHauthor", "Apply!"),
                                   "  ",
                                   "  ",
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
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
                                                value = 20)
                      ),
                      mainPanel(
                        
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "AuthorHindexPlot", height = 700))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput(outputId = "AuthorHindexTable"))
                                    ))
                      )
                      
                    )
           ),
           "  ",
           "  ",
           "Affiliations",
           ### MOST RELEVANT AFFILIATION ----
           tabPanel("Most Relevant Affiliations",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Most Relevant Affiliations"))),
                                   br(),
                                   "  ",
                                   selectInput("disAff", 
                                               label = "Affiliation Name Disambiguation",
                                               choices = c("Yes"="Y", 
                                                           "No"="N"),
                                               selected = "Y"),
                                   "  ",
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   numericInput("MostRelAffiliationsK", 
                                                label=("Number of Affiliations"), 
                                                value = 20)
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelAffiliationsPlot", height = 700))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("MostRelAffiliationsTable"))
                                    ))
                      )
                    )
           ),
           "  ",
           "  ",
           "Countries",
           ### CORRESP AUTHOR'S COUNTRY ----
           tabPanel("Corresponding Author's Country",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Corresponding Author's Country"))),
                                   br(),
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   numericInput("MostRelCountriesK", 
                                                label=("Number of Countries"), 
                                                value = 20)
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelCountriesPlot", height = 700))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("MostRelCountriesTable"))
                                    ))
                      )
                    )
           ),
           ### COUNTRY SCIENTIFIC PRODUCTION ----
           tabPanel("Country Scientific Production",
                    
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Country Scientific Production"))),
                                   br()
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "countryProdPlot", height = 700))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("countryProdTable"))
                                    ))
                      )
                      
                    )
           ),
           ### MOST CITED COUNTRIES ----
           tabPanel("Most Cited Countries",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Most Cited Countries"))),
                                   br(),
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   selectInput("CitCountriesMeasure", 
                                               label = "Measure",
                                               choices = c("Total Citations"="TC", 
                                                           "Average Citations per Year"="TCY"),
                                               selected = "TC"),
                                   "  ",
                                   numericInput("MostCitCountriesK", 
                                                label=("Number of Countries"), 
                                                value = 20)
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "MostCitCountriesPlot", height = 700))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("MostCitCountriesTable"))
                                    ))
                      )
                    )
           )
),
### DOCS MENU ----           
navbarMenu("Documents",
           "  ",
           "  ",
           "Documents",
           ### CITED DOCS ----
           tabPanel("Most Global Cited Documents",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Most Global Cited Documents"))),
                                   br(),
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   numericInput("MostCitDocsK", 
                                                label=("Number of Documents"), 
                                                value = 20),
                                   "  ",
                                   selectInput("CitDocsMeasure", 
                                               label = "Measure",
                                               choices = c("Total Citations"="TC", 
                                                           "Total Citations per Year"="TCY"),
                                               selected = "TC"),
                                   "  "
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "MostCitDocsPlot", height = 700))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("MostCitDocsTable"))
                                    ))
                      )
                    )
           ),
           tabPanel("Most Local Cited Documents",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Most Local Cited Documents"))),
                                   br(),
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   numericInput("MostLocCitDocsK", 
                                                label=("Number of Documents"), 
                                                value = 20),
                                   "  ",
                                   selectInput(inputId = "LocCitSep", 
                                               label = "Field separator character", 
                                               choices = c(";" = ";", 
                                                           ".  " = ".  ",
                                                           "," = ","),
                                               selected = ";"),
                                   "  "
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "MostLocCitDocsPlot",height = 700))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("MostLocCitDocsTable"))
                                    ))
                      )
                    )
           ),
           ### CITED REFERENCES ----
           "  ",
           "  ",
           "Cited References",
           
           tabPanel("Most Local Cited References",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Most Local Cited References"))),
                                   br(),
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   numericInput("MostCitRefsK", 
                                                label=("Number of Documents"), 
                                                value = 20),
                                   "  ",
                                   selectInput(inputId = "CitRefsSep", 
                                               label = "Field separator character", 
                                               choices = c(";" = ";", 
                                                           ".  " = ".  ",
                                                           "," = ","),
                                               selected = ";")
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotlyOutput(outputId = "MostCitRefsPlot", height = 700))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("MostCitRefsTable"))
                                    ))
                      )
                    )
           ),
           ### Reference Spectroscopy ----
           tabPanel("Reference Spectroscopy",
                    sidebarLayout(
                      
                      sidebarPanel(width=3,
                                   h3(em(strong("Reference Spectroscopy"))),
                                   br(),
                                   h4(em(strong("Parameters: "))),
                                   "  ",
                                   # sliderInput("sliderYears",
                                   #             label = "Timespan",
                                   #             min = 1700,
                                   #             max = as.numeric(substr(Sys.Date(),1,4)),
                                   #             step = 10, sep="",
                                   #             value = c(1700, as.numeric(substr(Sys.Date(),1,4)))
                                   # ),
                                   
                                   selectInput(inputId = "rpysSep", 
                                               label = "Field separator character", 
                                               choices = c(";" = ";", 
                                                           ".  " = ".  ",
                                                           "," = ","),
                                               selected = ";")
                                   
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Graph", 
                                             withSpinner(plotlyOutput(outputId = "rpysPlot", height = 700))),
                                    tabPanel("RPY Table", 
                                             shinycssloaders::withSpinner(DT::DTOutput(
                                               outputId = "rpysTable"))),
                                    tabPanel("Cited References Table", 
                                             shinycssloaders::withSpinner(DT::DTOutput(
                                               outputId = "crTable")))
                        )
                      ))),
           ### WORDS MENU ----
           "  ",
           "  ",
           "Words",
           
           ### MOST FREQ WORDS ----
           tabPanel("Most Frequent Words",
                    
                    sidebarLayout(
                      # Sidebar with a slider and selection inputs
                      sidebarPanel(width=3,
                                   h3(em(strong("Most Frequent Words"))),
                                   br(),
                                   h4(em(strong("Parameters:"))),
                                   " ",
                                   selectInput("MostRelWords", "Field",
                                               choices = c("Keywords Plus" = "ID",
                                                           "Author's keywords" = "DE",
                                                           "Titles" = "TI",
                                                           "Abstracts" = "AB"),
                                               selected = "ID"),
                                   hr(),
                                   sliderInput("MostRelWordsN", label = "Number of words", min = 2, max = 50, step = 1, value = 10)
                                   
                      ),
                      
                      # Show Word Cloud
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             withSpinner(plotlyOutput(outputId = "MostRelWordsPlot", height = 700))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("MostRelWordsTable"))
                                    ))
                        
                      )
                    )),
           
           ### WORDCLOUD ----
           tabPanel("WordCloud",
                    
                    sidebarLayout(
                      # Sidebar with a slider and selection inputs
                      sidebarPanel(width=3,
                                   h3(em(strong("WordCloud"))),
                                   br(),
                                   h4(em(strong("Graphical Parameters:"))),
                                   " ",
                                   selectInput("summaryTerms", "Field",
                                               choices = c("Keywords Plus" = "ID",
                                                           "Author's keywords" = "DE",
                                                           "Titles" = "TI",
                                                           "Abstracts" = "AB"),
                                               selected = "ID"),
                                   hr(),
                                   sliderInput("n_words", label = "Number of words", min = 10, max = 200, step = 5, value = 50),
                                   selectInput("measure", "Word occurrence measure",
                                               choices = c("Frequency" = "freq",
                                                           "Square root" = "sqrt",
                                                           "Log" = "log",
                                                           "Log10" = "log10"),
                                               selected = "freq"),
                                   selectInput("wcShape", "Shape",
                                               choices = c("Circle" = "circle",
                                                           "Cardiod" = "cardioid",
                                                           "Diamond" = "diamond",
                                                           "Pentagon" = "pentagon",
                                                           "Star" = "star",
                                                           "Triangle-forward" = "triangle-forward"
                                                           ,"Triangle" = "triangle"),
                                               selected = "circle"),
                                   selectInput("font", label = "Font type",
                                               choices = c("Impact", "Comic Sans MS (No plz!)" = "Comic Sans MS",
                                                           "Arial", "Arial Black", "Tahoma", "Verdana", "Courier New",
                                                           "Georgia", "Times New Roman", "Andale Mono")),
                                   selectInput("wcCol", "Text colors",
                                               choices = c("Random Dark" = "random-dark",
                                                           "Random Light" = "random-light"),
                                               selected = "random-dark"),
                                   colourpicker::colourInput("wcBGCol", label= "Backgroud color",value="white", showColour = "background", returnName=TRUE),
                                   sliderInput("scale", label = "Font size", min=0.2,max=5,step=0.1,value=1),
                                   sliderInput("ellipticity", label = "Ellipticity", min=0,max=1,step=0.05,value=0.65),
                                   sliderInput("padding", label = "Padding", min = 0, max = 5, value = 1, step = 1),
                                   sliderInput("rotate", label = "Rotate", min = 0, max = 20, value = 0, step = 1)
                      ),
                      
                      # Show Word Cloud
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             wordcloud2::wordcloud2Output("wordcloud", height = "600px")
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("wordTable"))
                                    ))
                        
                      )
                    )),
           
           ### TREEMAP ----
           tabPanel("TreeMap",
                    
                    sidebarLayout(
                      # Sidebar with a slider and selection inputs
                      sidebarPanel(width=3,
                                   h3(em(strong("TreeMap "))),
                                   br(),
                                   h4(em(strong("Graphical Parameters:"))),
                                   " ",
                                   selectInput("treeTerms", "Field",
                                               choices = c("Keywords Plus" = "ID",
                                                           "Author's keywords" = "DE",
                                                           "Titles" = "TI",
                                                           "Abstracts" = "AB"),
                                               selected = "ID"),
                                   hr(),
                                   sliderInput("treen_words", label = "Number of words", min = 10, max = 200, step = 5, value = 50),
                                   selectInput("treemeasure", "Word occurrence measure",
                                               choices = c("Frequency" = "freq",
                                                           "Square root" = "sqrt",
                                                           "Log" = "log",
                                                           "Log10" = "log10"),
                                               selected = "freq"),
                                   selectInput("treeCol", "Text colors",
                                               choices = c("Accent" = "Accent",
                                                           "Dark" = "Dark2",
                                                           "Paired"= "Paired",
                                                           "Pastel1"="Pastel1",
                                                           "Pastel2"="Pastel2",
                                                           "Set1"="Set1",
                                                           "Set2"="Set2",
                                                           "Set3"="Set3"),
                                               selected = "Pastel2"),
                                   sliderInput("treeFont", label = "Font size", min=6,max=20,step=1,value=10)
                      ),
                      
                      # Show TreeMap
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotOutput("treemap"))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("treeTable"))
                                    ))
                        
                      )
                    )),
           
           ### WORD DYNAMICS ----
           tabPanel("Word Dynamics",
                    
                    sidebarLayout(
                      # Sidebar with a slider and selection inputs
                      sidebarPanel(width=3,
                                   h3(em(strong("Word Dynamics"))),
                                   br(),
                                   h4(em(strong("Graphical Parameters:"))),
                                   " ",
                                   selectInput("growthTerms", "Field",
                                               choices = c("Keywords Plus" = "ID",
                                                           "Author's keywords" = "DE",
                                                           "Titles" = "TI",
                                                           "Abstracts" = "AB"),
                                               selected = "ID"),
                                   selectInput("cumTerms", "Occurrences",
                                               choices = c("Cumulate" = "Cum",
                                                           "Per year" = "noCum"),
                                               selected = "noCum"),
                                   selectInput("se", "Confidence Interval",
                                               choices = c("Yes" = "Yes",
                                                           "No" = "No"),
                                               selected = "No"),
                                   hr(),
                                   sliderInput("topkw", label = "Number of words", min = 1, max = 100, step = 1, value = c(1,10))
                                   
                                   #uiOutput("sliderKwYears")
                      ),
                      
                      # 
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotOutput(outputId = "kwGrowthPlot"))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput(outputId = "kwGrowthtable"))
                                    ))
                        
                      )
                    )),
           ### TREND TOPICS ----
           tabPanel("Trend Topics",
                    
                    sidebarLayout(
                      # Sidebar with a slider and selection inputs
                      sidebarPanel(width=3,
                                   h3(em(strong("Trend Topics"))),
                                   br(),
                                   
                                   actionButton("applyTrendTopics", "Apply!"),
                                   h4(em(strong("Method Parameters:"))),
                                   " ",
                                   selectInput("trendTerms", "Field",
                                               choices = c("Keywords Plus" = "ID",
                                                           "Author's keywords" = "DE",
                                                           "Titles" = "TI",
                                                           "Abstracts" = "AB"),
                                               selected = "ID"),
                                   conditionalPanel(
                                     condition = "input.trendTerms == 'TI' | input.trendTerms == 'AB'",
                                     selectInput("trendStemming", label="Word Stemming",
                                                 choices = c("Yes" = TRUE,
                                                             "No" = FALSE),
                                                 selected = FALSE)),
                                   uiOutput("trendSliderPY"),
                                   hr(),
                                   h4(em(strong("Graphical Parameters:"))),
                                   " ",
                                   #uiOutput("trendMinFreq"),
                                   sliderInput("trendMinFreq", label = "Word Minimum Frequency", min = 0, max = 100, value = 5, step = 1),
                                   sliderInput("trendNItems", label = "N. of Words per Year", min = 1, max = 20, step = 1, value = 5),
                                   sliderInput("trendSize", label = "Word label size", min = 0, max = 20, step = 1, value = 5)
                                   
                                   
                      ),
                      
                      # 
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotOutput(outputId = "trendTopicsPlot"))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput(outputId = "trendTopicsTable"))
                                    ))
                        
                      )
                    ))
),


### CONCEPTUAL STRUCTURE ----
navbarMenu("Conceptual Structure",
           
           #### Co-occurrence Network ----
           "  ",
           "  ",
           "Network Approach",
           tabPanel("Co-occurrence Network",
                    
                    sidebarLayout(
                      
                      sidebarPanel(width=3,
                                   h3(em(strong("Co-occurrence Network"))),
                                   br(),
                         
                                   actionButton("applyCoc", "Apply!"),
                                   downloadButton("network.coc", "Save Pajek"),
                                   downloadButton("networkCoc.fig", "Save Fig"),
                                   "  ",
                                   "  ",
                                   h4(em(strong("Network Parameters: "))),
                                  "  ",
                                  selectInput("field", 
                                      label = "Field",
                                      choices = c("Keywords Plus" = "ID", 
                                                "Author's Keywords" = "DE",
                                                "Titles" = "TI",
                                                "Abstracts" = "AB"),
                                      selected = "ID"),
                        
                        selectInput("layout", 
                                    label = "Network Layout",
                                    choices = c("Automatic layout"="auto", 
                                                "Circle"="circle",
                                                "Fruchterman & Reingold"="fruchterman",
                                                "Kamada & Kawai"="kamada",
                                                "MultiDimensional Scaling"="mds",
                                                "Sphere"="sphere",
                                                "Star"="star"),
                                    selected = "auto"),
                        
                        selectInput("normalize", 
                                    label = "Normalization",
                                    choices = c("none", 
                                                "association",
                                                "jaccard", 
                                                "salton",
                                                "inclusion",
                                                "equivalence"),
                                    selected = "association"),
                        selectInput("cocyears",
                                    label = "Node Color by Year",
                                    choices = c("No" = "No",
                                                "Yes"= "Yes"),
                                    selected = "No"),
                        selectInput("cocCluster", 
                                    label = "Clustering Algorithm",
                                    choices = c("None" = "none",
                                                "Edge Betweenness" = "edge_betweenness",
                                                "InfoMap" = "infomap",
                                                "Leading Eigenvalues" = "leading_eigen",
                                                "Louvain" = "louvain",
                                                "Spinglass" = "spinglass",
                                                "Walktrap" = "walktrap"),
                                    selected = "louvain"),
                        
                        sliderInput(inputId = "Nodes",
                                    label = "Number of Nodes",
                                    min = 5,
                                    max = 1000,
                                    value = 50),
                        
                        selectInput(inputId ="coc.isolates",
                                    label = "Remove Isolated Nodes",
                                    choices = c("Yes" = "yes",
                                                "No" = "no"),
                                    selected = "no"),
                        
                        numericInput("edges.min", 
                                     label=("Min edges"),
                                     value = 2,
                                     step = 1,
                                     min = 0),
                        #uiOutput("Focus"),
                        "  ",
                        h4(em(strong("Graphical Parameters: "))),
                        "  ",
                        sliderInput(inputId = "cocAlpha",
                                    label = "Opacity",
                                    min = 0,
                                    max = 1,
                                    value = 0.7,
                                    step=0.05),
                        sliderInput(inputId = "Labels",
                                    label = "Number of labels",
                                    min = 0,
                                    max = 1000,
                                    value = 50),
                        selectInput(inputId ="label.cex",
                                    label = "Label cex",
                                    choices = c("Yes", 
                                                "No"),
                                    selected = "Yes"),
                        
                        sliderInput(inputId = "labelsize",
                                    label = "Label size",
                                    min = 0.0,
                                    max = 20,
                                    value = 6,
                                    step = 0.10),
                        
                        selectInput(inputId ="coc.shape",
                                    label = "Node Shape",
                                    choices = c("Box"="box",
                                                "Circle"="circle",
                                                "Database"="database",
                                                "Ellipse"="ellipse",
                                                "Text"="text"),
                                    selected = "box"),
                        sliderInput(
                          inputId = "edgesize",
                          label = "Edge size",
                          min = 0.1,
                          max = 20,
                          value = 5), 
                        
                        selectInput(inputId ="coc.curved",
                                    label = "Curved edges",
                                    choices = c("Yes",
                                                "No"),
                                    selected = "No")
                        
                      
                      ),
                    
                    mainPanel(
                      tabsetPanel(type = "tabs",
                                  tabPanel("Map", 
                                           shinycssloaders::withSpinner(visNetworkOutput("cocPlot", height = "750px",width = "1100px"))),
                                  tabPanel("Table", 
                                           shinycssloaders::withSpinner(DT::DTOutput(
                                             outputId = "cocTable")))
                      )
                      
                      )
                    )
           ), ## End of tabPanel ("CS network")
           
           ### Thematic Map ----
           #"  ",
           #"  ",
           #"Thematic Analysis",
           tabPanel("Thematic Map",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Thematic Map"))),
                                   br(),
                                   
                                   actionButton("applyTM", "Apply!"),
                                   "  ",
                                   "  ",
                                   h4(em(strong("TM Parameters: "))),
                                   "  ",
                                   selectInput("TMfield", 
                                               label = "Field",
                                               choices = c("Keywords Plus" = "ID", 
                                                           "Author's Keywords" = "DE",
                                                           "Titles" = "TI",
                                                           "Abstracts" = "AB"),
                                               selected = "ID"),
                                   conditionalPanel(
                                     condition = "input.TMfield == 'TI' | input.TMfield == 'AB'",
                                     selectInput("TMstemming", label="Word Stemming",
                                                 choices = c("Yes" = TRUE,
                                                             "No" = FALSE),
                                                 selected = FALSE)),
                                   sliderInput("TMn", label="Number of Words",value=250,min=50,max=500,step=10),
                                   sliderInput("TMfreq", label="Min Cluster Frequency",value=5,min=1,max=100,step=1),
                                   sliderInput("TMn.labels", label="Number of Labels (for each cluster)",value=1,min=1,max=5,step=1),
                                   sliderInput("sizeTM", label="Label size",value=0.3,min=0.0,max=1,step=0.05)
                      ),
                      mainPanel("Thematic Map",
                                tabsetPanel(type = "tabs",
                                            tabPanel("Map",
                                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot", height = 700))
                                            ),
                                            tabPanel("Network",
                                                     shinycssloaders::withSpinner(visNetworkOutput("NetPlot", height = "750px",width = "1100px"))),
                                            tabPanel("Table",
                                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable"))
                                            )
                                )
                                
                      )
                      
                    )
           ), ## End of tabPanel ("Thematic Map")
           
           ### Thematic Evolution ----
           tabPanel("Thematic Evolution",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   h3(em(strong("Thematic Evolution"))),
                                   br(),
                                   actionButton("applyTE", "Apply!"),
                                   br(),
                                   h4(em(strong("TE Parameters: "))),
                                   "  ",
                                   selectInput("TEfield", 
                                               label = "Field",
                                               choices = c("Keywords Plus" = "ID", 
                                                           "Author's Keywords" = "DE",
                                                           "Titles" = "TI",
                                                           "Abstracts" = "AB"),
                                               selected = "ID"),
                                   
                                   sliderInput("nTE", label="Number of Words",value=250,min=50,max=500,step=10),
                                   sliderInput("fTE", label="Min Cluster Frequency",value=5,min=1,max=100,step=1),
                                   selectInput("TEmeasure", 
                                               label = "Weight index",
                                               choices = c("Inclusion Index" = "inclusion", 
                                                           "Inclusion Index weighted by Word-Occurrences" = "weighted",
                                                           "Stability Index" = "stability"
                                               ),
                                               selected = "weighted"),
                                   sliderInput("minFlowTE", label="Min Weight Index",value=0.1,min=0.02,max=1,step=0.02),
                                   sliderInput("sizeTE", label="Label size",value=0.3,min=0.0,max=1,step=0.05),
                                   sliderInput("TEn.labels", label="Number of Labels (for each cluster)",value=1,min=1,max=5,step=1),
                                   br(),
                                   h4(em(strong("Time Slices: "))),
                                   numericInput("numSlices", label="Number of Cutting Points",min=1,max=4,value=1),
                                   "Please, write the cutting points (in year) for your collection",
                                   uiOutput("sliders")
                                   
                                   
                      ),
                      mainPanel("Thematic Evolution",
                                
                                tabsetPanel(type = "tabs",
                                            tabPanel("Thematic Evolution", tabsetPanel(type="tabs",
                                              tabPanel("Map",
                                                       shinycssloaders::withSpinner(networkD3::sankeyNetworkOutput(outputId = "TEPlot",height = "600px"))
                                                      ),
                                              tabPanel("Table",
                                                       shinycssloaders::withSpinner(DT::DTOutput(outputId = "TETable"))
                                                      ))
                                            ),
                                            tabPanel("Time Slice 1", tabsetPanel(type="tabs",
                                                                                          tabPanel("Thematic Map",
                                                                                                    shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot1", height = 700))
                                                                                            ),
                                                                                          tabPanel("Network",
                                                                                                    shinycssloaders::withSpinner(visNetworkOutput("NetPlot1", height = "750px",width = "1100px"))
                                                                                            ),
                                                                                          tabPanel("Table",
                                                                                                    shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable1"))
                                                                                          ))      
                                            ),
                                            tabPanel("Time Slice 2", tabsetPanel(type="tabs",
                                                                                            tabPanel("Thematic Map",
                                                                                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot2", height = 700))
                                                                                            ),
                                                                                            tabPanel("Network",
                                                                                                     shinycssloaders::withSpinner(visNetworkOutput("NetPlot2", height = "750px",width = "1100px"))
                                                                                            ),
                                                                                            tabPanel("Table",
                                                                                                    shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable2"))
                                                                                            )) 
                                            ),
                                            tabPanel("Time Slice 3", tabsetPanel(type="tabs",
                                                                                            tabPanel("Thematic Map",
                                                                                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot3", height = 700))
                                                                                            ),
                                                                                            tabPanel("Network",
                                                                                                     shinycssloaders::withSpinner(visNetworkOutput("NetPlot3", height = "750px",width = "1100px"))
                                                                                            ),
                                                                                            tabPanel("Table",
                                                                                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable3"))
                                                                                            )) 
                                            ),
                                            tabPanel("Time Slice 4", tabsetPanel(type="tabs",
                                                                                            tabPanel("Thematic Map",
                                                                                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot4", height = 700))
                                                                                            ),
                                                                                            tabPanel("Network",
                                                                                                     shinycssloaders::withSpinner(visNetworkOutput("NetPlot4", height = "750px",width = "1100px"))
                                                                                            ),
                                                                                            tabPanel("Table",
                                                                                                      shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable4"))
                                                                                            )) 
                                            ),
                                            tabPanel("Time Slice 5", tabsetPanel(type="tabs",
                                                                                            tabPanel("Thematic Map",
                                                                                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot5", height = 700))
                                                                                            ),
                                                                                            tabPanel("Network",
                                                                                                     shinycssloaders::withSpinner(visNetworkOutput("NetPlot5", height = "750px",width = "1100px"))
                                                                                            ),
                                                                                            tabPanel("Table",
                                                                                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable5"))
                                                                                            ))
                                            )
                                )
                                
                      )
                    )
                    
           ), ## End of tabPanel ("Thematic Map")
           ### Factorial Analysis ----
           "  ",
           "  ",
           "Factorial Approach",
           tabPanel("Factorial Analysis",
                    
                    sidebarLayout(
                      
                      sidebarPanel(width=3,
                                   h3(em(strong("Factorial Analysis"))),
                                   br(),
                                   actionButton("applyCA", "Apply!"),
                                   
                        
                                   "  ",
                                   "  ",
                                   h4(em(strong("FA Parameters: "))),
                                   "  ",
                        
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
                        
                        numericInput("CSn", 
                                     label=("Number of terms"), 
                                     value = 50),
                        selectInput("nClustersCS", 
                                    label = "N. of Clusters",
                                    choices = c("Auto" = "0", 
                                                "1" = "1",
                                                "2" = "2",
                                                "3" = "3",
                                                "4" = "4",
                                                "5" = "5",
                                                "6" = "6",
                                                "7" = "7",
                                                "8" = "8"),
                                    selected = "0"),
                        
                        "  ",
                        "  ",
                        h4(em(strong("Graphical Parameters: "))),
                        "  ",
                        
                        sliderInput(
                          inputId = "CSlabelsize",
                          label = "Label size",
                          min = 5,
                          max = 30,
                          value = 10),
                        numericInput("CSdoc", 
                                     label=("Num. of documents"), 
                                     value = 20)
                        
                      ),
                      
                      mainPanel("Factorial Analysis",
                      
                          tabsetPanel(type = "tabs",
                                  tabPanel("Word Map", 
                                           shinycssloaders::withSpinner(plotOutput(
                                    outputId = "CSPlot1"))),
                                  tabPanel("Topic Dendrogram", 
                                           shinycssloaders::withSpinner(plotOutput(
                                             outputId = "CSPlot4"))),
                                  tabPanel("Most Contributing Papers", 
                                           shinycssloaders::withSpinner(plotOutput(
                                    outputId = "CSPlot2"))),
                                  tabPanel("Most Cited Papers", 
                                           shinycssloaders::withSpinner(plotOutput(
                                    outputId = "CSPlot3"))),
                                  tabPanel("Words by Cluster",
                                           shinycssloaders::withSpinner(DT::DTOutput(outputId = "CSTableW"))),
                                  tabPanel("Articles by Cluster",
                                           shinycssloaders::withSpinner(DT::DTOutput(outputId = "CSTableD")))
                                  
                          )
                      )
                    )
           ) ## End of tabPanel ("Correspondence Analysis")
           

           
          
),

### Intellectual Structure ----
navbarMenu("Intellectual Structure",
           ### Co.Citation Network ----
           tabPanel(title="Co-citation Network",
                    sidebarLayout(
                      
                      sidebarPanel(width=3,
                                   h3(em(strong("Co-citation Network"))),
                                   br(),
                                   actionButton("applyCocit", "Apply!"),
                                   downloadButton("network.cocit", "Save Pajek"),
                                   downloadButton("networkCocit.fig", "Save Fig"),
                                   "  ",
                                   "  ",
                                   h4(em(strong("Network Parameters: "))),
                                   "  ",
                        
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
                        
                        selectInput("citlayout", 
                                    label = "Network Layout",
                                    choices = c("Automatic layout"="auto", 
                                                "Circle"="circle",
                                                "Fruchterman & Reingold"="fruchterman",
                                                "Kamada & Kawai"="kamada",
                                                "MultiDimensional Scaling"="mds",
                                                "Sphere"="sphere",
                                                "Star"="star"),
                                    selected = "auto"),
                        
                        selectInput("cocitCluster", 
                                    label = "Clustering Algorithm",
                                    choices = c("None" = "none", 
                                                "Edge Betweenness" = "edge_betweenness",
                                                "InfoMap" = "infomap",
                                                "Leading Eigenvalues" = "leading_eigen",
                                                "Louvain" = "louvain",
                                                "Spinglass" = "spinglass",
                                                "Walktrap" = "walktrap"),
                                    selected = "louvain"),
                        
                        sliderInput(inputId = "citNodes",
                                    label = "Number of Nodes",
                                    min = 5,
                                    max = 1000,
                                    value = 50),
                        
                        selectInput(inputId ="cit.isolates",
                                    label = "Remove Isolated Nodes",
                                    choices = c("Yes" = "yes",
                                                "No" = "no"),
                                    selected = "no"),
                        
                        numericInput("citedges.min", 
                                     label=("Min edges"),
                                     value = 2,
                                     step = 1),
                        "  ",
                        "  ",
                        h4(em(strong("Graphical Parameters: "))),
                        "  ",
                        sliderInput(inputId = "cocitAlpha",
                                    label = "Opacity",
                                    min = 0,
                                    max = 1,
                                    value = 0.7,
                                    step=0.05),
                        selectInput(inputId ="citShortlabel",
                                    label = "Short Label",
                                    choices = c("Yes", 
                                                "No"),
                                    selected = "Yes"),
                        
                        sliderInput(inputId = "citLabels",
                                    label = "Number of labels",
                                    min = 5,
                                    max = 250,
                                    value = 50),
                        
                        selectInput(inputId ="citlabel.cex",
                                    label = "Label cex",
                                    choices = c("Yes", 
                                                "No"),
                                    selected = "Yes"),
                        
                        sliderInput(inputId = "citlabelsize",
                                    label = "Label size",
                                    min = 0.0,
                                    max = 20,
                                    value = 6,
                                    step = 0.10),
                        
                        selectInput(inputId ="cocit.shape",
                                    label = "Node Shape",
                                    choices = c("Box"="box",
                                                "Circle"="circle",
                                                "Database"="database",
                                                "Ellipse"="ellipse",
                                                "Text"="text"),
                                    selected = "box"),
                        sliderInput(inputId = "citedgesize",
                          label = "Edge size",
                          min = 0.1,
                          max = 20,
                          value = 5), 
                        
                        selectInput(inputId ="cocit.curved",
                                    label = "Curved edges",
                                    choices = c("Yes",
                                                "No"),
                                    selected = "No")
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    
                                    tabPanel("Graph", 
                                             shinycssloaders::withSpinner(visNetworkOutput("cocitPlot", height = "750px",width = "1100px"))),         
                                    tabPanel("Table", 
                                             shinycssloaders::withSpinner(DT::DTOutput(
                                                outputId = "cocitTable")))
                                            
                        )
                        #shinycssloaders::withSpinner(plotOutput(outputId = "cocitPlot"))
                      )
                      
                    )
                    
                    ), ## End of tabPanel "Co-citations"
           ### Historiograph ----
           tabPanel(title="Historiograph",
                    sidebarLayout(
                      
                      sidebarPanel(width=3,
                                   h3(em(strong("Historiograph "))),
                                   br(),
                                   actionButton("applyHist", "Apply!"),
                                   #selectInput('save_colnet', 'Save network as:', choices = c('No, thanks!' = 'no_thanks', 'Pajek format' = 'pajek')),
                                   #conditionalPanel(condition = "input.save_colnet == 'pajek'",
                                   
                                   
                                   "  ",
                                   "  ",
                                   h4(em(strong("Historiograph Parameters: "))),
                                   "  ",
                                   
                                   sliderInput(inputId = "histNodes",
                                               label = "Number of Nodes",
                                               min = 5,
                                               max = 50,
                                               value = 20),
                                   selectInput(inputId = "histsearch",
                                               label = "Search algorithm",
                                               choices = c("Fast search" = "FAST", 
                                                           "Full search" = "FULL"),
                                               selected = "FAST"),
                                   "  ",
                                   "  ",
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   sliderInput(inputId = "histlabelsize",
                                               label = "Label size",
                                               min = 0.0,
                                               max = 20,
                                               value = 10),
                                   
                                   sliderInput(inputId = "histsize",
                                               label = "Node size",
                                               min = 0.1,
                                               max = 20,
                                               value = 10)
                                  ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Graph", 
                                             shinycssloaders::withSpinner(plotOutput(
                                      outputId = "histPlot"))),
                                    tabPanel("Table", 
                                             shinycssloaders::withSpinner(DT::DTOutput(
                                      outputId = "histTable")))
                        )
                      )
                        
                                    #plotOutput(outputId = "histPlot"))
                    )
                ) ## End of tabPanel "Historiograph"
           ),


### Social Structure ####
           ### Collaboration ----
navbarMenu("Social Structure",
           
           tabPanel(title="Collaboration Network",
                    sidebarLayout(
                      
                      sidebarPanel(width=3,
                                   h3(em(strong("Collaboration Network"))),
                                   br(),
                                   actionButton("applyCol", "Apply!"),
                                   downloadButton("network.col", "Save Pajek"),
                                   downloadButton("networkCol.fig", "Save Fig"),
                                                
                                   "  ",
                                   "  ",
                                   h4(em(strong("Network Parameters: "))),
                                   "  ",
                                   
                                   
                                   selectInput("colField", 
                                               label = "Field",
                                               choices = c("Authors" = "COL_AU", 
                                                           "Institutions" = "COL_UN",
                                                           "Countries" = "COL_CO"),
                                               selected = "COL_AU"),
                                   selectInput("colnormalize", 
                                               label = "Normalization",
                                               choices = c("none", 
                                                           "association",
                                                           "jaccard", 
                                                           "salton",
                                                           "inclusion",
                                                           "equivalence"),
                                               selected = "none"),
                                   
                                   selectInput("collayout", 
                                               label = "Network Layout",
                                               choices = c("Automatic layout"="auto", 
                                                           "Circle"="circle",
                                                           "Fruchterman & Reingold"="fruchterman",
                                                           "Kamada & Kawai"="kamada",
                                                           "MultiDimensional Scaling"="mds",
                                                           "Sphere"="sphere",
                                                           "Star"="star"),
                                               selected = "auto"),
                                   
                                   selectInput("colCluster", 
                                               label = "Clustering Algorithm",
                                               choices = c("None" = "none", 
                                                           "Edge Betweenness" = "edge_betweenness",
                                                           "InfoMap" = "infomap",
                                                           "Leading Eigenvalues" = "leading_eigen",
                                                           "Louvain" = "louvain",
                                                           "Spinglass" = "spinglass",
                                                           "Walktrap" = "walktrap"),
                                               selected = "louvain"),
                                   
                                   sliderInput(inputId = "colNodes",
                                               label = "Number of Nodes",
                                               min = 5,
                                               max = 1000,
                                               value = 50),
                                   
                                   selectInput(inputId ="col.isolates",
                                               label = "Remove Isolated Nodes",
                                               choices = c("Yes" = "yes",
                                                           "No" = "no"),
                                               selected = "no"),
                                   
                                   numericInput("coledges.min", 
                                                label=("Min edges"),
                                                value = 2,
                                                step = 1),
                                   "  ",
                                   "  ",
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   sliderInput(inputId = "colAlpha",
                                               label = "Opacity",
                                               min = 0,
                                               max = 1,
                                               value = 0.7,
                                               step=0.05),
                                   sliderInput(inputId = "colLabels",
                                               label = "Number of labels",
                                               min = 5,
                                               max = 250,
                                               value = 50),
                                   
                                   selectInput(inputId ="collabel.cex",
                                               label = "Label cex",
                                               choices = c("Yes", 
                                                           "No"),
                                               selected = "Yes"),
                                   
                                   sliderInput(inputId = "collabelsize",
                                               label = "Label size",
                                               min = 0.0,
                                               max = 20,
                                               value = 6,
                                               step = 0.10),
                                   
                                   selectInput(inputId ="col.shape",
                                               label = "Node Shape",
                                               choices = c("Box"="box",
                                                           "Circle"="circle",
                                                           "Database"="database",
                                                           "Ellipse"="ellipse",
                                                           "Text"="text"),
                                               selected = "box"),

                                   sliderInput(inputId = "coledgesize",
                                               label = "Edge size",
                                               min = 0.1,
                                               max = 20,
                                               value = 5), 
                                   
                                   selectInput(inputId ="soc.curved",
                                               label = "Curved edges",
                                               choices = c("Yes",
                                                           "No"),
                                               selected = "No")
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Graph", 
                                             shinycssloaders::withSpinner(visNetworkOutput("colPlot", height = "750px",width = "1100px"))), 
                                             #shinycssloaders::withSpinner(plotOutput(outputId = "colPlot"))),
                                    tabPanel("Table", 
                                             shinycssloaders::withSpinner(DT::DTOutput(
                                               outputId = "colTable")))
                        )
                        
                        #shinycssloaders::withSpinner(plotOutput(outputId = "colPlot"))
                      )
                      
                    )
                    
           ), ## End of tabPanel "Social Structure" 
           ### Collaboration WorldMap ----
           tabPanel(title="Collaboration WorldMap",
                    sidebarLayout(
                      
                      sidebarPanel(width=3,
                                   h3(em(strong("Collaboration WorldMap"))),
                                   br(),
                                   actionButton("applyWM", "Apply!"),
                                   "  ",
                                   "  ",
                                   h4(em(strong("Map Parameters: "))),
                                   "  ",
                                   numericInput("WMedges.min", 
                                                label=("Min edges"),
                                                value = 2,
                                                step = 1),
                                   "  ",
                                   "  ",
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   sliderInput(inputId = "WMedgesize",
                                               label = "Edge size",
                                               min = 0.1,
                                               max = 20,
                                               value = 5)
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Graph", 
                                             shinycssloaders::withSpinner(plotOutput(outputId = "WMPlot"))),
                                    tabPanel("Table", 
                                             shinycssloaders::withSpinner(DT::DTOutput(
                                               outputId = "WMTable")))
                        )
                        
                        #shinycssloaders::withSpinner(plotOutput(outputId = "colPlot"))
                      )
                      
                    )
                    
           ) ## End of tabPanel "Social Structure" 
), 


# navbarMenu(("About"),
#            tabPanel(title = "Help", 
#                     includeHTML("bibliometrix-vignette.html"))
#            ),

### Quit button ----
navbarMenu("Quit",
           tabPanel(actionLink("stop_radiant", "Stop", icon = icon("power-off"), 
                               onclick = "setTimeout(function(){window.close();}, 100); ")
           )

  )

## End of UI           
)


