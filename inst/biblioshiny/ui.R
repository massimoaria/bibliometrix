## BIBLIOSHINY: A SHINY APP FOR BIBLIOMETRIX R-PACKAGE

if (!(require(shiny))){install.packages("shiny"); require(shiny, quietly=TRUE)} 
if (!(require(rio))){install.packages("rio")} 
if (!(require(DT))){install.packages("DT")} 
if (!(require(ggplot2))){install.packages("ggplot2"); require(ggplot2, quietly=TRUE)} 
if (!(require(shinycssloaders))){install.packages("shinycssloaders")} 
if (!(require(shinythemes))){install.packages("shinythemes")} 
if (!(require(wordcloud2))){install.packages("wordcloud2")} 
if (!require(colourpicker)){install.packages("colourpicker")}
if (!require(treemap)){install.packages("treemap")}
if (!require(ggmap)){install.packages("ggmap"); require(ggmap, quietly=TRUE)}
if (!require(visNetwork)){install.packages("visNetwork"); require(visNetwork, quietly=TRUE)}
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
                               div(img(src = "logo.jpg", height = 400, width = 800), style="text-align: center;"),
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
                                 Clarivate Analytics' Web of Science, PubMed and Cochrane databases, performing bibliometric 
                                 analysis and building data matrices for co-citation, coupling, scientific collaboration analysis and co-word analysis."),
                               br(),
                               p("For an introduction and live examples, visit the ",
                                 a("bibliometrix website.", 
                                   href = "http://www.bibliometrix.org")),
                               br(),
                               
                               h2("Workflow"),
                               br(),
                               h4(em("bibliometrix")," supports the main stages of the recommended science mapping workflow:"),
                               br(),
                               div(img(src = "workflow.jpg", height=346, width=800), style="text-align: center;"),
                               br(),
                               
                               h2("Example"),
                               br(),
                               p("Step 1 - Download an example at the following", a("link",href = "http://www.bibliometrix.org/datasets/joi.zip"),
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
  "Load", 
  sidebarLayout(
    sidebarPanel(width=3,
      selectInput("dbsource", 
                  label = "Database",
                  choices = c("Web of Knowledge"="isi", 
                              "Scopus"="scopus"),
                  selected = "isi"),
      conditionalPanel(condition = "input.dbsource == 'isi'",
                       selectInput("format", 
                                   label = "File format",
                                   choices = c("Plain Text"="plaintext", 
                                               "BibTeX"="bibtex"),
                                   selected = "plaintext")),
      fileInput("file1", "Choose a file",
                multiple = FALSE,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".txt",
                  ".bib",
                  ".RData",
                  ".xlsx",
                  ".xls",
                  ".zip")
      ),
      p("Here accept single .txt/.bib/.xslx/.RData files, or multiple .txt/.bib files compressed in a single .zip archive."),
      tags$hr(),
      
      uiOutput("textLog"),
      #shinycssloaders::withSpinner(verbatimTextOutput("log")),
      
      
      ### download xlsx
      selectInput('save_file', 'Save as:', choices = c('No, thanks!' = 'no_thanks', 'Excel' = 'xlsx')),
      conditionalPanel(condition = "input.save_file == 'xlsx'",
              downloadButton("collection.save", "Save"))
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
                                   uiOutput("textDim"),
                                   uiOutput("selectType"),
                                   uiOutput("sliderPY"),
                                   uiOutput("sliderTC"),
                                   uiOutput("selectSource")
                                   
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
                      sidebarPanel(width=3),
                      mainPanel(
                        shinycssloaders::withSpinner(DT::DTOutput(outputId = "MainInfo"))
                    )
           )),
           tabPanel("Annual Scientific Production",
                    sidebarLayout(
                      sidebarPanel(width=3),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotOutput(outputId = "AnnualProdPlot"))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("AnnualProdTable"))
                                    ))
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
                                   "  ",
                                   "  ",
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   numericInput("MostRelSourcesK", 
                                                label=("Number of Sources"), 
                                                value = 20)
                                   ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotOutput(outputId = "MostRelSourcesPlot"))
                                    ),
                                    tabPanel("Table",
                                              shinycssloaders::withSpinner(DT::DTOutput("MostRelSourcesTable"))
                                    ))
                      )
                      )
                    ),
           ### BRADFORD LAW ----
           tabPanel("Bradford's law",
                    
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   helpText("")            
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotOutput(outputId = "bradfordPlot"))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("bradfordTable"))
                                    ))
                      )
                      
                    )
           ),
           ### SOURCE HINDEX MENU ----
           tabPanel("Source's H-index",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   "  ",
                                   "  ",
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
                                             shinycssloaders::withSpinner(plotOutput(outputId = "SourceHindexPlot"))
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
                                   "  ",
                                   "  ",
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   numericInput("MostRelAuthorsK", 
                                                label=("Number of Authors"), 
                                                value = 20)
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotOutput(outputId = "MostRelAuthorsPlot"))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("MostRelAuthorsTable"))
                                    ))
                      )
                    )
           ),
           
           ### AUTHOR'S PRODUCTION OVER TIME  ----
           tabPanel("Author's Production over Time",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   "  ",
                                   "  ",
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   numericInput("TopAuthorsProdK", 
                                                label=("Number of Authors"), 
                                                value = 20)
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotOutput(outputId = "TopAuthorsProdPlot"))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("TopAuthorsProdTable"))
                                    ))
                      )
                    )
           ),

           ### LOTKA LAW ----
           tabPanel("Lotka's law",
                    
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   helpText("")            
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotOutput(outputId = "lotkaPlot"))
                                    ),
                                    tabPanel("Table",
                                             shinycssloaders::withSpinner(DT::DTOutput("lotkaTable"))
                                    ))
                      )
                      
                    )
           ),
           ### AUTHOR HINDEX ----
           tabPanel("Author's H-index",
                    sidebarLayout(
                      sidebarPanel(width=3,
                                   "  ",
                                   "  ",
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
                                             shinycssloaders::withSpinner(plotOutput(outputId = "AuthorHindexPlot"))
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
                                   "  ",
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
                                             shinycssloaders::withSpinner(plotOutput(outputId = "MostRelAffiliationsPlot"))
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
                                   "  ",
                                   "  ",
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   numericInput("MostRelCountriesK", 
                                                label=("Number of Countries"), 
                                                value = 20)
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotOutput(outputId = "MostRelCountriesPlot"))
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
                      sidebarPanel(width=3
                                   ,helpText("")            
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plot",
                                             shinycssloaders::withSpinner(plotOutput(outputId = "countryProdPlot"))
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
                                   "  ",
                                   "  ",
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
                                             shinycssloaders::withSpinner(plotOutput(outputId = "MostCitCountriesPlot"))
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
                                   "  ",
                                   "  ",
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
                                             shinycssloaders::withSpinner(plotOutput(outputId = "MostCitDocsPlot"))
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
                                   "  ",
                                   "  ",
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
                                             shinycssloaders::withSpinner(plotOutput(outputId = "MostLocCitDocsPlot"))
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
                                   "  ",
                                   "  ",
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
                                             shinycssloaders::withSpinner(plotOutput(outputId = "MostCitRefsPlot"))
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
                                   "  ",
                                   "  ",
                                   h4(em(strong("Graphical Parameters: "))),
                                   "  ",
                                   sliderInput("sliderYears",
                                               label = "Timespan",
                                               min = 1700,
                                               max = as.numeric(substr(Sys.Date(),1,4)),
                                               step = 10, sep="",
                                               value = c(1700, as.numeric(substr(Sys.Date(),1,4)))
                                   ),
                                   
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
                                             withSpinner(plotOutput(outputId = "rpysPlot"))),
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
                                             withSpinner(plotOutput(outputId = "MostRelWordsPlot"))
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
                    ))
),


### CONCEPTUAL STRUCTURE ----
navbarMenu("Conceptual Structure",
           
           #### Co-occurrence Network ----
           "  ",
           "  ",
           "Co-Word Analysis",
           tabPanel("Co-occurrence Network",
                    
                    sidebarLayout(
                      
                      sidebarPanel(width=3,
                         
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
                                    label = "Layout",
                                    choices = c("auto", 
                                                "circle",
                                                "fruchterman",
                                                "kamada",
                                                "mds",
                                                "sphere",
                                                "star"),
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
                        
                        sliderInput(inputId = "Nodes",
                                    label = "Number of Nodes",
                                    min = 5,
                                    max = 1000,
                                    value = 30),
                        
                        numericInput("edges.min", 
                                     label=("Min edges"),
                                     value = 2,
                                     step = 1),
                        #uiOutput("Focus"),
                        "  ",
                        h4(em(strong("Graphical Parameters: "))),
                        "  ",
                        sliderInput(inputId = "cocAlpha",
                                    label = "Opacity",
                                    min = 0,
                                    max = 1,
                                    value = 0.5,
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
                                    max = 10,
                                    value = 2,
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
                                  tabPanel("Graph", 
                                           shinycssloaders::withSpinner(visNetworkOutput("cocPlot", height = "750px",width = "1100px"))),
                                  tabPanel("Table", 
                                           shinycssloaders::withSpinner(DT::DTOutput(
                                             outputId = "cocTable")))
                      )
                      
                      )
                    )
           ), ## End of tabPanel ("CS network")
           
           ### Factorial Analysis ----
           tabPanel("Factorial Analysis",
                    
                    sidebarLayout(
                      
                      sidebarPanel(width=3,
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
                                    selected = "CA"),
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
                                     value = 10)
                        
                      ),
                      
                      mainPanel("Factorial Analysis",
                      
                          tabsetPanel(type = "tabs",
                                  tabPanel("Word Map", 
                                           shinycssloaders::withSpinner(plotOutput(
                                    outputId = "CSPlot1"))),
                                  tabPanel("Most Contributing Papers", 
                                           shinycssloaders::withSpinner(plotOutput(
                                    outputId = "CSPlot2"))),
                                  tabPanel("Most Cited Papers", 
                                           shinycssloaders::withSpinner(plotOutput(
                                    outputId = "CSPlot3")))
                          )
                      )
                    )
           ), ## End of tabPanel ("Correspondence Analysis")
           
           ### Thematic Map ----
           "  ",
           "  ",
           "Thematic Analysis",
           tabPanel("Thematic Map",
                    sidebarLayout(
                      sidebarPanel(width=3,
                        
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
                                   sliderInput("TMn", label="Number of Words",value=250,min=50,max=500,step=10),
                                   sliderInput("TMfreq", label="Min Cluster Frequency",value=5,min=1,max=100,step=1)
                                   ),
                    mainPanel("Thematic Map",
                              tabsetPanel(type = "tabs",
                                tabPanel("Map",
                                  shinycssloaders::withSpinner(plotOutput(outputId = "TMPlot"))
                                ),
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
                                   
                                   "  ",
                                   "  ",
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
                                                           "Stability Index" = "stability"
                                                           ),
                                               selected = "inclusion"),
                                   numericInput("numSlices", label="Number of Cutting Points",min=1,max=4,value=1),
                                   "Please, write the cutting points (in year) for your collection",
                                   uiOutput("sliders"),
                                   actionButton("applyTE", "Apply!")
                                   
                      ),
                      mainPanel("Thematic Evolution",
                                
                                tabsetPanel(type = "tabs",
                                            tabPanel("Map",
                                                     shinycssloaders::withSpinner(networkD3::sankeyNetworkOutput(outputId = "TEPlot",height = "600px"))
                                            ),
                                            tabPanel("Table",
                                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "TETable"))
                                            )
                                )
                                
                                )
                    )
                    
           ) ## End of tabPanel ("Thematic Map")
           
),

### Intellectual Structure ----
navbarMenu("Intellectual Structure",
           ### Co.Citation Network ----
           tabPanel(title="Co-citation Network",
                    sidebarLayout(
                      
                      sidebarPanel(width=3,
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
                                    label = "Layout",
                                    choices = c("auto", 
                                                "circle",
                                                "fruchterman",
                                                "kamada",
                                                "mds",
                                                "sphere",
                                                "star"),
                                    selected = "auto"),
                        
                        sliderInput(inputId = "citNodes",
                                    label = "Number of Nodes",
                                    min = 5,
                                    max = 250,
                                    value = 30),
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
                                    value = 0.5,
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
                                    max = 10,
                                    value = 2,
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
                                   actionButton("applyHist", "Apply!"),
                                   #selectInput('save_colnet', 'Save network as:', choices = c('No, thanks!' = 'no_thanks', 'Pajek format' = 'pajek')),
                                   #conditionalPanel(condition = "input.save_colnet == 'pajek'",
                                   
                                   
                                   "  ",
                                   "  ",
                                   h4(em(strong("Network Parameters: "))),
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


### Collaboration ----
navbarMenu("Social Structure",
           
           tabPanel(title="Collaboration Network",
                    sidebarLayout(
                      
                      sidebarPanel(width=3,
                                   actionButton("applyCol", "Apply!"),
                                   downloadButton("network.col", "Save Pakek"),
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
                                                           "Star"="star",
                                                           "World Map (only for Field Country)"="worldmap"),
                                               selected = "auto"),
                                   
                                   sliderInput(inputId = "colNodes",
                                               label = "Number of Nodes",
                                               min = 5,
                                               max = 250,
                                               value = 30),
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
                                               value = 0.5,
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
                                               max = 10,
                                               value = 2,
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
           tabPanel(title="WorldMap Collaboration",
                    sidebarLayout(
                      
                      sidebarPanel(width=3,
                                   actionButton("applyWM", "Apply!"),
                                   "  ",
                                   "  ",
                                   h4(em(strong("Network Parameters: "))),
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


