## BIBLIOSHINY: A SHINY APP FOR BIBLIOMETRIX R-PACKAGE

if (!(require(shiny))){install.packages("shiny"); require(shiny, quietly=TRUE)} 
if (!(require(rio))){install.packages("rio")} 
if (!(require(DT))){install.packages("DT")} 
if (!(require(ggplot2))){install.packages("ggplot2"); require(ggplot2, quietly=TRUE)} 
if (!(require(shinycssloaders))){install.packages("shinycssloaders")} 
if (!(require(shinythemes))){install.packages("shinythemes")} 
if (!(require(wordcloud2))){install.packages("wordcloud2")} 

# Main NavBar ----
options(spinner.size=1, spinner.type=5)

ui <-  navbarPage("biblioshiny: The shiny app for bibliometrix R-package",
                  theme=shinythemes::shinytheme("slate"),
                  
### WELCOME PAGE ----
                  tabPanel("Welcome",
                           fluidRow(
                             
                             column(9,
                                    wellPanel(
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
                  ".xlsx",
                  ".xls",
                  ".zip")
      ),
      p("Here accept single .txt/.bib/.xslx, or multiple .txt/.bib files compressed in a single .zip archive."),
      tags$hr(),
      
      uiOutput("textLog"),
      #shinycssloaders::withSpinner(verbatimTextOutput("log")),
      
      
      ### download xlsx
      selectInput('save_file', 'Save as:', choices = c('No, thanks!' = 'no_thanks', 'xlsx' = 'xlsx')),
      conditionalPanel(condition = "input.save_file == 'xlsx'",
              downloadButton("collection.xlsx", "Save"))
    ),
    mainPanel(
      ## color of datatable
      tags$head(tags$style(HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                                  background-color: #9c4242 !important;
                                  }
                                  "))),
      tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                  color: #ffffff !important;
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

### Descriptive Analysis PAGE ----
navbarMenu("Descriptive Analysis",
           
           tabPanel("Tables",
                    sidebarLayout(
                      
                      sidebarPanel(width=3,
                        selectInput("summary_type", 
                                    label = "Result:",
                                    choices = c("Main Information about data"="tab1", 
                                                "Annual Scientific Production"="tab2",
                                                "Most Productive Authors"="tab3",
                                                "Most Cited Papers"="tab4",
                                                "Most Productive Countries"="tab5",
                                                "Most Cited Countries"="tab6",
                                                "Most Relevant Sources"="tab7",
                                                "Most Relevant Keywords"="tab8",
                                                "All results"="all"),
                                    selected = "tab1"),
                        
                        conditionalPanel(condition = "input.summary_type != 'tab1'",
                                    numericInput("kk", 
                                      label=("Number of results"), 
                                      value = 20)),
                        selectInput('save_summary', 'Save as:', choices = c('No, thanks!' = 'no_thanks', 'txt file' = 'txt')),
                        conditionalPanel(condition = "input.save_summary == 'txt'",
                                         downloadButton("results.txt", "Save"))
                      ),
                      mainPanel(
                        shinycssloaders::withSpinner(verbatimTextOutput("summary"))
                      )
                    )
           ),
           
           tabPanel("Plots",
                    sidebarLayout(
                      
                      
                      sidebarPanel(width=3,
                        
                        selectInput("plot_type", 
                                    label = "Choose the plot",
                                    choices = c("Annual Scientific Production"= "production",
                                                "Most Productive Authors"="authors", 
                                                "Most Productive Countries"="countries",
                                                "Average Article Citations per Year"="articleTC",
                                                "Average Total Citation per Year"="annualTC"),
                                    selected = "production"),
                        
                        conditionalPanel(condition = "input.plot_type == 'authors'",
                                         numericInput("k", 
                                                      label=("Number of results"), 
                                                      value = 20)
                        ),
                        conditionalPanel(condition = "input.plot_type == 'countries'",
                                         numericInput("k", 
                                                      h3("Number of results"), 
                                                      value = 20)
                        )
                        
                      ),
                      mainPanel(
                        shinycssloaders::withSpinner(plotOutput(outputId = "summaryPlots"))
                      )
                      
                    )
           ),
           tabPanel("Word Cloud",
           
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
               sliderInput("n_words", label = "Number of words:", min = 10, max = 200, step = 5, value = 50),
               sliderInput("scale", label = "Font size:", min=1,max=10,step=1,value=1),
               #selectInput("spiral", label = "Spiral:", choices = c("archimedean", "rectangular")),
               selectInput("font", label = "Font type:",
                           choices = c("Impact", "Comic Sans MS (No plz!)" = "Comic Sans MS",
                                       "Arial", "Arial Black", "Tahoma", "Verdana", "Courier New",
                                       "Georgia", "Times New Roman", "Andale Mono")),
               sliderInput("padding", label = "Padding:", min = 0, max = 5, value = 1, step = 1),
               sliderInput("rotate", label = "Rotate:", min = 0, max = 20, value = 0, step = 1)
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
           ))
      ),

### Temporal Analysis ----
navbarMenu("Temporal Analysis",
           
           tabPanel("Word Analysis",

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
                                               selected = "Cum"),
                                   selectInput("se", "Confidence Interval",
                                               choices = c("Yes" = "Yes",
                                                           "No" = "No"),
                                               selected = "Yes"),
                                   hr(),
                                   sliderInput("topkw", label = "Number of words", min = 1, max = 100, step = 1, value = 10)
                                   
                                   #uiOutput("sliderKwYears")
                      ),

                      # Show Word Cloud
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
           
           tabPanel("Reference Spectroscopy",
                    sidebarLayout(
                      
                      sidebarPanel(width=3,
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
                    )))),

### CONCEPTUAL STRUCTURE ----
navbarMenu("Conceptual Structure",
           
           #### Co-occurrence Network ----
           tabPanel("Co-occurrence Network",
                    
                    sidebarLayout(
                      
                      sidebarPanel(width=3,
                        
                        helpText("Parameters: "),
                        
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
                                    max = 100,
                                    value = 30),
                        
                        sliderInput(inputId = "Labels",
                                    label = "Number of labels",
                                    min = 5,
                                    max = 100,
                                    value = 30),
                        
                        selectInput(inputId ="label.cex",
                                    label = "Label cex",
                                    choices = c("Yes", 
                                                "No"),
                                    selected = "Yes"),
                        
                        sliderInput(inputId = "labelsize",
                                    label = "Label size",
                                    min = 0.0,
                                    max = 10,
                                    value = 5,
                                    step = 0.25),
                        
                        selectInput(inputId ="size.cex",
                                    label = "Size cex",
                                    choices = c("Yes",
                                                "No"),
                                    selected = "Yes"),
                        
                        sliderInput(inputId = "size",
                                    label = "Node size",
                                    min = 0.1,
                                    max = 20,
                                    value = 5),
                        sliderInput(
                          inputId = "edgesize",
                          label = "Edge size",
                          min = 0.1,
                          max = 20,
                          value = 1), 
                        
                        numericInput("edges.min", 
                                     label=("Min edges"),
                                     value = 1,
                                     step = 1),
                        
                        selectInput(inputId ="coc.curved",
                                    label = "Curved edges",
                                    choices = c("Yes",
                                                "No"),
                                    selected = "No")
                        
                      ),
                    
                    mainPanel(
                      tabsetPanel(type = "tabs",
                                  tabPanel("Graph", 
                                           withSpinner(plotOutput(outputId = "cocPlot"))),
                                  tabPanel("Table", 
                                           shinycssloaders::withSpinner(DT::DTOutput(
                                             outputId = "cocTable")))
                      )
                      
                      )
                    )
           ), ## End of tabPanel ("CS network")
           
           ### Factorial Analysis ----
           tabPanel("Correspondence Analysis",
                    
                    sidebarLayout(
                      
                      sidebarPanel(width=3,
                        
                        helpText("Parameters: "),
                        
                        selectInput("method", 
                                    label = "Method",
                                    choices = c("Correspondence Analysis" = "CA",
                                                "Multiple Correspondence Analysis" = "MCA"),
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
                      
                      mainPanel("Correspondence Analysis",
                      
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
           tabPanel("Thematic Map",
                    sidebarLayout(
                      sidebarPanel(width=3,
                        
                        helpText("Parameters: "),
                        
                        numericInput("TMn", 
                                     label=("Min. word frequency"), 
                                     value = 5)
                    ),
                    mainPanel("Thematic Map",
                              shinycssloaders::withSpinner(plotOutput(outputId = "TMPlot"))
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
                        
                        helpText("Parameters: "),
                        
                        selectInput("citField", 
                                    label = "Field",
                                    choices = c("Papers" = "CR", 
                                                "Authors" = "CR_AU",
                                                "Sources" = "CR_SO"),
                                    selected = "CR"),
                        
                        selectInput(inputId = "citSep", 
                                  label = "Field separator character", 
                                  choices = c(";" = ";", 
                                              ".  " = ".  ",
                                              "," = ","),
                                  selected = ";"),
                        
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
                                    max = 100,
                                    value = 30),
                        
                        sliderInput(inputId = "citLabels",
                                    label = "Number of labels",
                                    min = 5,
                                    max = 100,
                                    value = 20),
                        
                        selectInput(inputId ="citlabel.cex",
                                    label = "Label cex",
                                    choices = c("Yes", 
                                                "No"),
                                    selected = "Yes"),
                        
                        sliderInput(inputId = "citlabelsize",
                                    label = "Label size",
                                    min = 0.0,
                                    max = 10,
                                    value = 5,
                                    step = 0.25),
                        
                        selectInput(inputId ="citsize.cex",
                                    label = "Size cex",
                                    choices = c("Yes",
                                                "No"),
                                    selected = "Yes"),
                        
                        sliderInput(inputId = "citsize",
                                    label = "Node size",
                                    min = 0.1,
                                    max = 20,
                                    value = 5),
                        sliderInput(inputId = "citedgesize",
                          label = "Edge size",
                          min = 0.1,
                          max = 20,
                          value = 1), 
                        
                        numericInput("citedges.min", 
                                     label=("Min edges"),
                                     value = 1,
                                     step = 1),
                        selectInput(inputId ="cocit.curved",
                                    label = "Curved edges",
                                    choices = c("Yes",
                                                "No"),
                                    selected = "No")
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Graph", 
                                             shinycssloaders::withSpinner(plotOutput(outputId = "cocitPlot"))),
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
                                   
                                   helpText("Parameters: "),
                                   
                                   sliderInput(inputId = "histNodes",
                                               label = "Number of Nodes",
                                               min = 5,
                                               max = 50,
                                               value = 20),
                                   
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
                                   
                                   helpText("Parameters: "),
                                   
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
                                               label = "Layout",
                                               choices = c("auto", 
                                                           "circle",
                                                           "fruchterman",
                                                           "kamada",
                                                           "mds",
                                                           "sphere",
                                                           "star"),
                                               selected = "auto"),
                                   
                                   sliderInput(inputId = "colNodes",
                                               label = "Number of Nodes",
                                               min = 5,
                                               max = 100,
                                               value = 30),
                                   
                                   sliderInput(inputId = "colLabels",
                                               label = "Number of labels",
                                               min = 5,
                                               max = 100,
                                               value = 20),
                                   
                                   selectInput(inputId ="collabel.cex",
                                               label = "Label cex",
                                               choices = c("Yes", 
                                                           "No"),
                                               selected = "Yes"),
                                   
                                   sliderInput(inputId = "collabelsize",
                                               label = "Label size",
                                               min = 0.0,
                                               max = 10,
                                               value = 5,
                                               step = 0.25),
                                   
                                   selectInput(inputId ="colsize.cex",
                                               label = "Size cex",
                                               choices = c("Yes",
                                                           "No"),
                                               selected = "Yes"),
                                   
                                   sliderInput(inputId = "colsize",
                                               label = "Node size",
                                               min = 0.1,
                                               max = 20,
                                               value = 5),
                                   sliderInput(inputId = "coledgesize",
                                               label = "Edge size",
                                               min = 0.1,
                                               max = 20,
                                               value = 1), 
                                   
                                   numericInput("coledges.min", 
                                                label=("Min edges"),
                                                value = 1,
                                                step = 1),
                                   selectInput(inputId ="soc.curved",
                                               label = "Curved edges",
                                               choices = c("Yes",
                                                           "No"),
                                               selected = "No")
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Graph", 
                                             shinycssloaders::withSpinner(plotOutput(outputId = "colPlot"))),
                                    tabPanel("Table", 
                                             shinycssloaders::withSpinner(DT::DTOutput(
                                               outputId = "colTable")))
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


