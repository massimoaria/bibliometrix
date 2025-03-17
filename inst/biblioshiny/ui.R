source("libraries.R", local=TRUE)
source("helpContent.R", local=TRUE)
suppressMessages(libraries())

# UI components ----
## Title ----
mytitle <- tags$link(tags$a(href = 'https://www.bibliometrix.org/',target="_blank",
                            tags$img(src="logo2.jpg", height = '40',width='40')),
                     strong("bibliometrix")
)

intro <- 'https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html'
importData <- 'https://www.bibliometrix.org/vignettes/Data-Importing-and-Converting.html'
slides <- 'https://bibliometrix.org/biblioshiny/assets/player/KeynoteDHTMLPlayer.html#0'
donation <- 'https://www.bibliometrix.org/home/index.php/donation'
bibliometrixWeb <- 'https://www.bibliometrix.org/' 
k_synth <- 'https://www.k-synth.unina.it'
github_aria <- 'https://github.com/massimoaria/bibliometrix'

style_opt <-  "border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;" # (option button)
style_bttn <- "border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;" # (action buttons)
t_report  <-  "Add Results to the Report"
t_export  <-  "Export Plot as PNG"
t_run <- "Run the Analysis"
run_bttn <- list(
  label = NULL,
  style = "material-circle",
  color = "primary",
  icon = icon(name ="play", lib="glyphicon")
)
report_bttn <- list(
  label = NULL,
  style = "material-circle",
  color = "primary",
  icon = icon(name ="plus", lib="glyphicon")
)
export_bttn <- list(
  label=NULL,
  style = "material-circle",
  color = "primary",
  icon = icon(name ="download", lib="glyphicon")
)

## load content of Info page
info <- helpContent()$info
pubs <- helpContent()$publications
## Header ----
header <- shinydashboardPlus::dashboardHeader(title = mytitle,
                                              titleWidth = 300,
                                              #controlbarIcon = fa_i(name ="bars"),
                                              dropdownMenuOutput("notificationMenu"),
                                              dropdownMenu(
                                                type = "messages",
                                                icon = icon("question"),
                                                badgeStatus = NULL,
                                                headerText = strong("Help Menu"),
                                                messageItem2(
                                                  from = "Package Tutorial",
                                                  message = "",
                                                  href = intro,
                                                  icon = icon("play-circle", lib = "glyphicon")
                                                ),
                                                messageItem2(
                                                  from = "Convert and Import Data",
                                                  message = "",
                                                  icon = icon("info-sign", lib = "glyphicon"),
                                                  href = importData
                                                ),
                                                messageItem2(
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
                                                           messageItem2(
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
                                                messageItem2(
                                                  from = "Bibliometrix",
                                                  message = "",
                                                  href = bibliometrixWeb,
                                                  icon = fa_i(name = "globe")
                                                ),
                                                messageItem2(
                                                  from = "K-Synth",
                                                  message = "",
                                                  href = k_synth,
                                                  icon = fa_i(name = "watchman-monitoring")
                                                ),
                                                messageItem2(
                                                  from = "Github",
                                                  message = "",
                                                  href = github_aria,
                                                  icon = fa_i(name = "github")
                                                )
                                              )
                                              ,tags$li(class = "dropdown",
                                                      tags$style(".main-header .logo {height: 53px}")
                                              )
)

## Side Bar ----
sidebar <- shinydashboardPlus::dashboardSidebar(
  useShinyjs(),
  sidebarMenu(
    id="sidebarmenu",
    menuItem("biblioshiny",tabName = "biblioshinyy",icon = fa_i(name="house-user")),
    menuItem("Info", tabName ="info", icon = fa_i(name="circle-info"),
             menuSubItem("Supported Files", tabName = "supFiles",icon = fa_i(name="circle-info")),
             menuSubItem("Team's Publications", tabName = "pubs",icon = fa_i(name="circle-info"))),
    menuItem("Data",tabName = "uploadData",icon = fa_i(name = "file-import"),
             menuSubItem("Import or Load", tabName = "loadData",icon = icon("chevron-right",lib = "glyphicon")),
             menuSubItem("API", tabName = "gathData",icon = icon("chevron-right",lib = "glyphicon")),
             menuSubItem("Merge Collections", tabName = "mergeData",icon = icon("chevron-right",lib = "glyphicon"))
             ),
    menuItemOutput ("rest_of_sidebar")
  ),
  textOutput("res"), width = 300
)

## Body ####

### Theme ----
data("customTheme",envir=environment())
### Body Content ----
body <- dashboardBody(
  customTheme,
  ## workaround to solve visualization issues in Data Table
  tags$head(tags$style(HTML( '.has-feedback .form-control { padding-right: 0px;}' ))),
  ###
  tags$head(
    tags$style(".fa-envelope {color:#FF0000; font-size: 20px}"),
    tags$style(".fa-envelope-open {font-size: 20px}"), 
    tags$style(".fa-cube {font-size: 20px}"),
    tags$style(".fa-question {font-size: 20px}"),
    tags$style(".fa-comment-dollar {font-size: 20px}"),
    tags$style(".fa-bars {font-size: 20px}"),
    tags$style(".sidebar-toggle {font-size: 15px}"),
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
              });
              $(document).ready(function(){
                  $("a[data-toggle=tab]").on("show.bs.tab", function(e){
                    Shiny.setInputValue("activeTab", $(this).attr("data-value"));
                   });
            });
      '
    )),
  tabItems(
    #### Homepage ----
    ##### home ----
    tabItem("biblioshinyy",
            fluidPage(
              fluidRow(
                column(12,
                       div(h1("biblioshiny: the shiny app for bibliometrix", 
                              style="text-align:center; font-size:50px;")),
                       br()
                ),
                column(12,
                       div(img(src = "logo.jpg", height = "35%",width = "35%"), style="text-align: center;")
                ),
                column(12,
                       div(p("For an introduction and live examples, visit the ",
                             em(a("bibliometrix website.", 
                                  href = "https://www.bibliometrix.org", target="_blank")), 
                             style="text-align:center; font-size:18px;")),
                       br(),
                       hr()
                ),
                column(12,
                       div(h6("biblioshiny and bibliometrix are open-source and freely available for use, distributed under the MIT license.", 
                              style="text-align:center; font-size:19px;")),
                       div(h6("When they are used in a publication, we ask that authors to cite the following reference:", 
                              style="text-align:center; font-size:19px;")),
                       br(),
                       div(h6("Aria, M., & Cuccurullo, C. (2017).", strong(" bibliometrix: An R-tool for comprehensive"), 
                              style="text-align:center; font-size:19px;")),
                       div(h6(strong("science mapping analysis."), 
                              em("Journal of Informetrics"),", 11(4), 959-975.", 
                              style="text-align:center; font-size:19px;")),
                       br(),
                       div(h6("Failure to properly cite the software is considered a violation of the license.", 
                              style="text-align:center; font-size:19px;"))
                       )
                
              )
            )
    ),
    tabItem("supFiles",
            fluidPage(
              fluidRow(
                column(1),
                column(10,
                       HTML(info),
                       div(img(src = "table_DBformats.jpg", height = "70%",width = "70%"), style="text-align: center;")
                ),
                column(1)
              )
            )),
    tabItem("pubs",
            fluidPage(
              fluidRow(
                column(1),
                column(10,
                       HTML(pubs)
                ),
                column(1)
              )
            )),
    #### Data ----
    ##### load Data ----
    tabItem("loadData",
            fluidPage(
              fluidRow(
                div(
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
                  column(9,
                         shinycssloaders::withSpinner(DT::DTOutput("contents"))),
                  column(3, 
                         fluidRow(
                           box(
                             width = "100%",
                             h3(strong("Import or Load ")),
                             selectInput("load", label = "Please, choose what to do",
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
                                                    , Source WoS."))
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
                                   "Openalex" ="openalex",
                                   "OpenAlex API (via openalexR)" = "openalex_api",
                                   "Lens.org" = "lens",
                                   "PubMed" = "pubmed",
                                   "Cochrane Library" = "cochrane"
                                 ),
                                 selected = "isi"
                               ),
                               selectInput(
                                 "authorName",
                                 label = "Author Name format",
                                 choices = c(
                                   "Fullname (if available)" = "AF",
                                   "Surname and Initials" = "AU"
                                 ),
                                 selected = "AU"
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
                                              fluidRow(column(12,
                                                              div(style ="border-radius: 10px; border-width: 3px; font-size: 15px;",
                                                                  align = "center",
                                                                  width="100%",
                                                                  actionBttn(inputId = "applyLoad", label = strong("Start"),
                                                                             width = "100%", style = "pill", color = "primary",
                                                                             icon = icon(name ="play", lib="glyphicon")))))
                             ),
                             tags$hr(),
                             uiOutput("textLog2"),
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
                                              fluidRow(column(12,
                                                              div(style ="border-radius: 10px; border-width: 3px; font-size: 15px;",
                                                                  align = "center",
                                                                  width="100%",
                                                                  downloadBttn(outputId = "collection.save", label = strong("Export"),
                                                                               #width = "100%", 
                                                                               style = "pill", color = "primary"))))
                             ) 
                           )
                         )
                  )
                )
              )
            )
    ),
    ##### gather Data ----
    tabItem("gathData",
            fluidPage(
              fluidRow(
                tags$head(tags$style(
                  HTML(
                    "table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                                  background-color: #9c4242 !important;
                                  }
                                  "
                  ))),
                tags$style(
                  HTML(
                    ".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                  color: #000000 !important;
                  }"
                  )),
                column(9,shinycssloaders::withSpinner(DT::DTOutput("apiContents"))),
                column(3,
                       box(
                         width = "100%",
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
                           fluidRow(column(12,
                                           div(style ="border-radius: 10px; border-width: 3px; font-size: 10px;",
                                               align = "center",
                                               width="100%",
                                               actionBttn(inputId = "dsShow", label ="1. Configure API",
                                                          style = "pill", color = "primary",
                                                          icon = icon(name ="sliders"))))),
                                    #h5(tags$b("Your Query")),
                                    verbatimTextOutput("queryLog2", placeholder = FALSE),
                                    h5(tags$b("Documents returned using your query")),
                                    verbatimTextOutput("sampleLog2", placeholder = FALSE)
                           ),
                           ### Pubmed API 
                           conditionalPanel(
                             condition = "input.dbapi == 'pubmed'",
                             br(),
                             fluidRow(column(12,
                                             div(style ="border-radius: 10px; border-width: 3px; font-size: 10px;",
                                                 align = "center",
                                                 width="100%",
                                                 actionBttn(inputId = "pmShow", label ="1. Configure API",
                                                            style = "pill", color = "primary",
                                                            icon = icon(name ="sliders"))))),
                             #h5(tags$b("Your Query")),
                             verbatimTextOutput("pmQueryLog2", placeholder = FALSE),
                             h5(tags$b("Documents returned using your query")),
                             verbatimTextOutput("pmSampleLog2", placeholder = FALSE),
                           ),
                           tags$hr(),
                           fluidRow(column(12,
                                           div(style ="border-radius: 10px; border-width: 3px; font-size: 10px;",
                                               align = "center",
                                               width="100%",
                                               actionBttn(inputId = "apiApply", label = "2. Download",
                                                          style = "pill", color = "primary",
                                                          icon(name ="download"))))),
                           tags$hr(),
                           h3(strong("Export a bibliometrix file ")),
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
                                            fluidRow(column(12,
                                                            div(style ="border-radius: 10px; border-width: 3px; font-size: 15px;",
                                                                align = "center",
                                                                width="100%",
                                                                downloadBttn(outputId = "collection.save_api", label = strong("Export"),
                                                                             style = "pill", color = "primary"))))
                           )
                         )
                       
                )
              )
            )
    ),
    ##### merge Data ----
    tabItem("mergeData",
            fluidPage(
              fluidRow(
                div(
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
                  column(9,
                         shinycssloaders::withSpinner(DT::DTOutput("contentsMerge"))),
                  column(3, 
                         fluidRow(
                           box(
                             width = "100%",
                             h3(strong("Load Collections")),
                             helpText(em("Merge collections in Excel or R format coming from different DBs")
                                 ),
                               fileInput(
                                 "fileMerge",
                                 "Select collection files",
                                 multiple = TRUE,
                                 accept = c(
                                   ".xlsx",
                                   ".rdata"
                                 )
                               )
                             ),
                             fluidRow(column(12,
                                             div(style ="border-radius: 10px; border-width: 3px; font-size: 15px;",
                                                                  align = "center",
                                                                  width="100%",
                                                                  actionBttn(inputId = "applyMerge", label = strong("Start"),
                                                                             width = "100%", style = "pill", color = "primary",
                                                                             icon = icon(name ="play", lib="glyphicon")))))
                             ),
                             tags$hr(),
                             h3(strong(
                               "Export collection"
                             )),
                             selectInput(
                               'save_fileMerge',
                               'Save as:',
                               choices = c(
                                 ' ' = 'null',
                                 'Excel' = 'xlsx',
                                 'R Data Format' = 'RData'
                               ),
                               selected = 'null'
                             ),
                             conditionalPanel(condition = "input.save_fileMerge != 'null'",
                                              fluidRow(column(12,
                                                              div(style ="border-radius: 10px; border-width: 3px; font-size: 15px;",
                                                                  align = "center",
                                                                  width="100%",
                                                                  downloadBttn(outputId = "collection.saveMerge", label = strong("Export"),
                                                                               #width = "100%", 
                                                                               style = "pill", color = "primary"))))
                             ) 
                           )
                         )
                  )
                )
    ),
    #### Filters ----
    tabItem("filters",
            fluidRow(
              column(9,DT::DTOutput("dataFiltered")),
              column(3,
                     box(
                       width = "100%",
                       h3(strong("Filters")),
                       br(),
                       fluidRow(column(12,
                                       div(style ="border-radius: 10px; border-width: 3px; font-size: 15px;",
                                           align = "center",
                                           width="100%",
                                           actionBttn(inputId = "applyFilter", label = strong("Apply"),
                                                      width = "100%", style = "pill", color = "primary",
                                                      icon = icon(name ="play", lib="glyphicon"))))),
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
                     )
              )
            )
    ),
    #### Overview ----
    ##### main information ----
    tabItem("mainInfo",
            fluidPage(
              fluidRow(column(11,
                h3(strong("Main Information"), align = "center")),
                div(style=style_bttn,
                    title = t_report,
                  column(1, 
                       do.call("actionBttn", c(report_bttn, list(
                                  inputId = "reportMI")
                                  ))
                ))
                ),
              fluidRow(
                tabsetPanel(type = "tabs", id = "maininfo",
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
                                     )
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "MainInfo", width = 700)
                                     ), align ="center")
                )
              )
            )
    ),
    ##### annual scientific production ----
    tabItem("annualScPr",
            fluidPage(
              fluidRow(column(10,
                h3(strong("Annual Scientific Production"), align = "center")),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportASP")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "ASPplot.save")
                           ))
                    ))
              ),
              fluidRow(
                tabsetPanel(id ="tabsASP",
                            type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "AnnualProdPlot", height = "75vh"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("AnnualProdTable"))
                            )
                ))
              
            )
    ),
    ##### average citation per year ----
    tabItem("averageCitPerYear",
            fluidPage(
              fluidRow(column(10,
                h3(strong("Average Citations Per Year"), align = "center")),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportACpY")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "ACpYplot.save")
                           ))
                    ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "AnnualTotCitperYearPlot", height = "75vh"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("AnnualTotCitperYearTable"))
                            )
                )
              )
            )
    ),
    ##### three fields plot ----
    tabItem("threeFieldPlot",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Three-Field Plot"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "apply3F")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportTFP")
                           ))
                    )),
                div(style = style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("actionBttn", c(export_bttn, list(
                             inputId = "screenTFP")
                           ))
                    )),
                div(style = style_opt,
                  column(1,
                         dropdown(
                           box(title = h4(strong("Parameters")), 
                               collapsible = FALSE, 
                               width = 15,
                               solidHeader = FALSE, 
                               fluidRow(
                                 column(6, selectInput("CentralField",
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
                                                       selected = "AU")),
                                 column(6,numericInput("CentralFieldn", 
                                                       label=("Number of items"), 
                                                       min = 1, max = 50, step = 1, value = 20))),
                               fluidRow(
                                 column(6,selectInput("LeftField",
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
                                                      selected = "CR")),
                                 column(6, numericInput("LeftFieldn", 
                                                        label=("Number of items"), 
                                                        min = 1, max = 50, step = 1, value = 20))),
                               fluidRow(
                                 column(6,selectInput("RightField",
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
                                                      selected = "DE")),
                                 column(6,numericInput("RightFieldn", 
                                                       label=("Number of items"), 
                                                       min = 1, max = 50, step = 1, value = 20)))
                           ),
                           right = TRUE, animate = TRUE, circle = TRUE,
                           style = "gradient",
                           tooltip = tooltipOptions(title = "Options"),
                           color = "primary",
                           icon = icon("sliders"),
                           width = "300px"
                         ))
                )
              ),
              fluidRow(
                shinycssloaders::withSpinner(plotlyOutput(outputId = "ThreeFieldsPlot", height = "90vh"))
              )
            )
    ),
    #### Sources ----
    ##### relevant sources ----
    tabItem("relevantSources",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Most Relevant Sources"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyMRSources")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportMRS")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "MRSplot.save")
                           ))
                    )),
                
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             numericInput("MostRelSourcesK", 
                                          label=("Number of Sources"), 
                                          value = 10),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ),
                style = style_opt
                )
                
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelSourcesPlot", height = "75vh",width ="98.9%"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostRelSourcesTable"))
                            )
                )
              )
            )
    ),
    ##### local cited sources ----
    tabItem("localCitedSources",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Most Local Cited Sources"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyMLCSources")
                           ))
                    )),
                
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportMLS")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "MLCSplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             numericInput("MostRelCitSourcesK", 
                                          label=("Number of Sources"), 
                                          value = 10),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelCitSourcesPlot", height = "75vh", width = "98.9%"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostRelCitSourcesTable"))
                            )
                )
              )
            )
    ),
    ##### bradford law ----
    tabItem("bradford",
            fluidPage(
              fluidRow(column(10,
                #titlePanel(
                  h3(strong("Core Sources by Bradford's Law"), align = "center")
                ),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportBradford")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "BLplot.save")
                           ))
                    ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "bradfordPlot", height = "75vh"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("bradfordTable"))
                            )
                )
              )
            )
    ),
    ##### source local impact ----
    tabItem("sourceImpact",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Sources' Local Impact"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyHsource")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportSI")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "SIplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             selectInput("HmeasureSources", 
                                         label = "Impact measure",
                                         choices = c("H-Index"="h", 
                                                     "G-Index"="g",
                                                     "M-Index"="m",
                                                     "Total Citation"="tc"),
                                         selected = "h"),
                             br(),
                             numericInput("Hksource", 
                                          label=("Number of sources"), 
                                          value = 10),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "SourceHindexPlot", height = "75vh",width ="98.9%"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "SourceHindexTable"))
                            )
                )
              )
            )
    ),
    ##### sources prod over time ----
    tabItem("sourceDynamics",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Sources' Production over Time"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applySOGrowth")
                           ))
                    )),
                
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportSD")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "SDplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             selectInput("cumSO", "Occurrences",
                                         choices = c("Cumulate" = "Cum",
                                                     "Per year" = "noCum"),
                                         selected = "Cum"),
                             sliderInput("topSO", label = "Number of Sources", 
                                         min = 1, max = 50, step = 1, value = c(1,5)),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "soGrowthPlot", height = "90vh"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "soGrowthtable"))
                            )
                )
              )
            )
    ),
    #### Authors ----
    ##### most relevant authors ----
    tabItem("mostRelAuthors",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Most Relevant Authors"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyMRAuthors")
                           ))
                    )),
                
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportMRA")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "MRAplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             numericInput("MostRelAuthorsK", 
                                          label=("Number of Authors"), 
                                          value = 10),
                             selectInput("AuFreqMeasure", 
                                         label = "Frequency measure",
                                         choices = c("N. of Documents "="t", 
                                                     "Percentage"="p",
                                                     "Fractionalized Frequency"="f"),
                                         selected = "t"),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelAuthorsPlot", height = "75vh",width ="98.9%"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostRelAuthorsTable"))
                            )
                )
              )
            )
    ),
    ##### most local cited authors ----
    tabItem("mostLocalCitedAuthors",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Most Local Cited Authors"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyMLCAuthors")
                           ))
                    )),
                
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportMLCA")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "MLCAplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             numericInput("MostCitAuthorsK", 
                                          label=("Number of Authors"), 
                                          value = 10),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostCitAuthorsPlot", height = "75vh",width ="98.9%"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostCitAuthorsTable"))
                            )
                )
              )
            )
    ),
    ##### authors production over time ----
    tabItem("authorsProdOverTime",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Authors' Production over Time"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyAUoverTime")
                           ))
                    )),div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportAPOT")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "APOTplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             numericInput("TopAuthorsProdK", 
                                          label=("Number of Authors"), 
                                          value = 10),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "TopAuthorsProdPlot", height = "75vh",width ="98.9%"))
                            ),
                            tabPanel("Table - Production per Year",
                                     shinycssloaders::withSpinner(DT::DTOutput("TopAuthorsProdTable"))
                            ),
                            tabPanel("Table - Documents",
                                     shinycssloaders::withSpinner(DT::DTOutput("TopAuthorsProdTablePapers"))
                            )
                )
              )
            )
    ),
    ##### lotka law ----
    tabItem("lotka",
            fluidPage(
              fluidRow(
                column(10,
                  h3(strong("Author Productivity through Lotka's Law"), align = "center")
                ),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportLotka")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "LLplot.save")
                           ))
                    ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "lotkaPlot", height = "75vh"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("lotkaTable"))
                            )
                )
              )
            )
    ),
    ##### authors local impact ----
    tabItem("authorImpact",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Authors' Local Impact"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyHAuthors")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportAI")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "AIplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             selectInput("HmeasureAuthors", 
                                         label = "Impact measure",
                                         choices = c("H-Index"="h", 
                                                     "G-Index"="g",
                                                     "M-Index"="m",
                                                     "Total Citation"="tc"),
                                         selected = "h"),
                             numericInput("Hkauthor", 
                                          label=("Number of authors"), 
                                          value = 10),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "AuthorHindexPlot", height = "75vh",width ="98.9%"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "AuthorHindexTable"))
                            )
                )
              )
            )
    ),
    ##### most relevant affiliations ----
    tabItem("mostRelAffiliations",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Most Relevant Affiliations"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyMRAffiliations")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportMRAFF")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "AFFplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             selectInput("disAff", 
                                         label = "Affiliation Name Disambiguation",
                                         choices = c("Yes"="Y", 
                                                     "No"="N"),
                                         selected = "Y"),
                             numericInput("MostRelAffiliationsK", 
                                          label=("Number of Affiliations"), 
                                          value = 10),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelAffiliationsPlot", height = "75vh",width ="98.9%"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostRelAffiliationsTable"))
                            )
                )
              )
            )
    ),
    ##### Affiliation over Time ----
    tabItem("AffOverTime",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Affiliations' Production over Time"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyAFFGrowth")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportAFFPOT")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "AffOverTimeplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             numericInput("topAFF", label = "Number of Affiliations", 
                                          min = 1, max = 50, step = 1, value = 5),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "AffOverTimePlot", height = "90vh"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "AffOverTimeTable"))
                            )
                )
              )
            )
    ),
    ##### corresponding author country ----
    tabItem("correspAuthorCountry",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Corresponding Author's Countries"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyCAUCountries")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportMRCO")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "MRCOplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             numericInput("MostRelCountriesK", 
                                          label=("Number of Countries"), 
                                          value = 20, min = 1, max = 50),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelCountriesPlot", height = "75vh",width ="98.9%"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostRelCountriesTable"))
                            )
                )
              )
            )
    ),
    ##### country scientific production ----
    tabItem("countryScientProd",
            fluidPage(
              fluidRow(column(10,
                h3(strong("Countries' Scientific Production"), align = "center")),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportCSP")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "CSPplot.save")
                           ))
                    ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "countryProdPlot", height = "75vh"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("countryProdTable"))
                            )
                )
              )
            )
    ),
    ##### Country over Time ----
    tabItem("COOverTime",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Countries' Production over Time"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyCOGrowth")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportCPOT")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "CountryOverTimeplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             sliderInput("topCO", label = "Number of Countries", 
                                         min = 1, max = 50, step = 1, value = 5),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "CountryOverTimePlot", height = "90vh"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "CountryOverTimeTable"))
                            )
                )
              )
            )
    ),
    ##### most cited countries ----
    tabItem("mostCitedCountries",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Most Cited Countries"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyMCCountries")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportMCCO")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "MCCplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             selectInput("CitCountriesMeasure", 
                                         label = "Measure",
                                         choices = c("Total Citations"="TC", 
                                                     "Average Article Citations"="TCY"),
                                         selected = "TC"),
                             numericInput("MostCitCountriesK", 
                                          label=("Number of Countries"), 
                                          value = 10),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostCitCountriesPlot", height = "75vh",width ="98.9%")) 
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostCitCountriesTable"))
                            )
                )
              )
            )
    ),
    #### Documents ----
    ##### most global cited documents ----
    tabItem("mostGlobalCitDoc",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Most Global Cited Documents"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyMGCDocuments")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportMCD")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "MGCDplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             numericInput("MostCitDocsK", 
                                          label=("Number of Documents"), 
                                          value = 10),
                             selectInput("CitDocsMeasure", 
                                         label = "Measure",
                                         choices = c("Total Citations"="TC", 
                                                     "Total Citations per Year"="TCY"),
                                         selected = "TC"), 
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostCitDocsPlot", height = "75vh",width ="98.9%"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostCitDocsTable"))
                            )
                )
              )
            )
    ),
    ##### most local cited documents ----
    tabItem("mostLocalCitDoc",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Most Local Cited Documents"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyMLCDocuments")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportMLCD")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "MLCDplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             numericInput("MostLocCitDocsK", 
                                          label=("Number of Documents"), 
                                          value = 10),
                             selectInput(inputId = "LocCitSep", 
                                         label = "Field separator character", 
                                         choices = c(";" = ";", 
                                                     ".  " = ".  ",
                                                     "," = ","),
                                         selected = ";"), 
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostLocCitDocsPlot", height = "75vh",width ="98.9%"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostLocCitDocsTable"))
                            )
                )
              )
            )
    ),
    ##### most local cited references ----
    tabItem("mostLocalCitRef",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Most Local Cited References"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyMLCReferences")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportMLCR")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "MLCRplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             numericInput("MostCitRefsK", 
                                         label=("Number of Documents"), 
                                         value = 10),
                             selectInput(inputId = "CitRefsSep", 
                                         label = "Field separator character", 
                                         choices = c(";" = ";", 
                                                     ".  " = ".  ",
                                                     "," = ","),
                                         selected = ";"), 
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostCitRefsPlot", height = "75vh",width ="98.9%"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostCitRefsTable"))
                            )
                )
              )
            )
    ),
    ##### references spectroscopy ----
    tabItem("ReferenceSpect",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Reference Spectroscopy"), align = "center")
                ),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyRPYS")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportRPYS")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "RSplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
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
                                      )), 
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot", 
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "rpysPlot", height = "75vh"))),
                            tabPanel("Table - RPY", 
                                     shinycssloaders::withSpinner(DT::DTOutput(
                                       outputId = "rpysTable"))),
                            tabPanel("Table - Cited References", 
                                     shinycssloaders::withSpinner(DT::DTOutput(
                                       outputId = "crTable")))
                )
              )
            )
    ),
    ##### most frequent words ----
    tabItem("mostFreqWords",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Most Frequent Words"), align = "center")
                ),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyMFWords")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportMFW")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "MRWplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
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
                                                              selected = ",")
                                                  #,h5(htmlOutput("MostRelWordsStopPreview"))
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
                                                              selected = ",")
                                                  #,h5(htmlOutput("MRWSynPreview"))
                                 )),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "MostRelWordsPlot", height = "75vh",width ="98.9%"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("MostRelWordsTable"))
                            )
                )
                
              )
            )
    ),
    ##### word cloud ----
    tabItem("wcloud",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("WordCloud"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyWordCloud")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportWC")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("actionBttn", c(export_bttn, list(
                             inputId = "screenWC")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
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
                                                 numericInput("scale", label = "Font size", min=0.1,max=5,step=0.1,value=0.5)
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
                             ),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     wordcloud2::wordcloud2Output("wordcloud", height = "75vh")
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("wordTable"))
                            )
                )
                
              )
            )
    ),
    ##### tree map ----
    tabItem("treemap",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("TreeMap"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyTreeMap")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportTREEMAP")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("actionBttn", c(export_bttn, list(
                             inputId = "screenTREEMAP")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
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
                                                  #h5(htmlOutput("TreeMapStopPreview"))
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
                                                  #h5(htmlOutput("TreeMapSynPreview"))
                                 )
                             ),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "treemap", height = "75vh"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("treeTable"))
                            )
                )
              )
            )
    ),
    ##### word dynamics ----
    tabItem("wordDynamics",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Words' Frequency over Time"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyWD")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportWD")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "WDplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
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
                                                  #h5(htmlOutput("WDStopPreview"))
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
                                                  #h5(htmlOutput("WDSynPreview"))
                                 )),
                             br(),
                             box(title = p(strong("Parameters"),style='font-size:16px;color:black;'), 
                                 collapsible = TRUE, width = 15,
                                 solidHeader = FALSE, collapsed = TRUE,
                                 selectInput("cumTerms", "Occurrences",
                                             choices = c("Cumulate" = "Cum",
                                                         "Per year" = "noCum"),
                                             selected = "Cum"),
                                 sliderInput("topkw", label = "Number of words", 
                                             min = 1, max = 100, step = 1, value = c(1,10))),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "kwGrowthPlot", height = "90vh"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "kwGrowthtable"))
                            )
                )
              )
            )
    ),
    ##### trend topic ----
    tabItem("trendTopic",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Trend Topics"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyTrendTopics")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportTT")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "TTplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
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
                                                  #h5(htmlOutput("TTStopPreview"))
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
                                                 # h5(htmlOutput("TTSynPreview"))
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
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "trendTopicsPlot", height = "90vh",width ="98.9%"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "trendTopicsTable"))
                            )
                )
                
              )
            )
    ),
    #### Clustering by Coupling ----
    tabItem("coupling",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Clustering by Coupling"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyCM")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportCM")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "CMplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
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
                                                                         #"Fast Greedy" = "fast_greedy",
                                                                         "InfoMap" = "infomap",
                                                                         "Leading Eigenvalues" = "leading_eigen",
                                                                         "Leiden" = "leiden",
                                                                         "Louvain" = "louvain",
                                                                         "Spinglass" = "spinglass",
                                                                         "Walktrap" = "walktrap"),
                                                             selected = "walktrap")
                                          ))
                             ),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Map",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "CMPlot", height = "75vh"))
                            ),
                            tabPanel("Network",
                                     shinycssloaders::withSpinner(visNetworkOutput("CMNetPlot", height = "75vh"))),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "CMTable"))
                            ),
                            tabPanel("Clusters",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "CMTableCluster"))
                            )
                )
              )
            )
    ),
    #### Conceptual Structure ----
    ##### co-occurrence network ----
    tabItem("coOccurenceNetwork",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Co-occurrence Network"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyCoc")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportCOC")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("actionBttn", c(export_bttn, list(
                             inputId = "screenCOC")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
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
                             materialSwitch(
                               inputId = "noOverlap",
                               label = "Avoid Label Overlap",
                               value = TRUE,
                               status = "primary"
                             ),
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
                                                  #h5(htmlOutput("COCStopPreview"))
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
                                                  #h5(htmlOutput("COCSynPreview"))
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
                                                                #"Fast Greedy" = "fast_greedy",
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
                                                     value = 1000,
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
                                                              value = 3,
                                                              step = 0.10)
                                 ),
                                 column(6,
                                        numericInput(
                                          inputId = "edgesize",
                                          label = "Edge size",
                                          min = 0.0,
                                          max = 20,
                                          value = 5,
                                          step=0.5)
                                 )), 
                                 fluidRow(column(6,
                                                 selectInput(inputId ="coc.shadow",
                                                             label = "Node shadow",
                                                             choices = c("Yes",
                                                                         "No"),
                                                             selected = "Yes")
                                 ),
                                 column(6,
                                        selectInput(inputId ="coc.curved",
                                                    label = "Edit Nodes",
                                                    choices = c("Yes",
                                                                "No"),
                                                    selected = "No")     
                                        
                                 )
                                 )
                             ),
                             br(),
                             fluidRow(column(6,
                                             div(style ="border-radius: 10px; border-width: 3px; font-size: 10px;",
                                                 align = "center",
                                                 width="100%",
                                                 downloadBttn(outputId = "network.coc", label = ("Pajek"),
                                                              style = "pill", color = "primary"))),
                                      column(6,
                                             div(style ="border-radius: 10px; border-width: 3px; font-size: 10px;",
                                                 align = "center",
                                                 width="100%",
                                                 downloadBttn(outputId = "networkCoc.fig", label = ("HTML"),
                                                              style = "pill", color = "primary")))
                             ),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Network", 
                                     shinycssloaders::withSpinner(visNetworkOutput("cocPlot", height = "75vh"))),
                            tabPanel("Density", 
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "cocOverlay", height = "75vh"))),
                            tabPanel("Table", 
                                     shinycssloaders::withSpinner(DT::DTOutput(
                                       outputId = "cocTable"))),
                            tabPanel("Degree Plot", 
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "cocDegree", height = "75vh")))
                )
              )
            )
    ),
    ##### thematic map ----
    tabItem("thematicMap",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Thematic Map"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyTM")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportTM")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "TMplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
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
                             materialSwitch(
                               inputId = "noOverlapTM",
                               label = "Avoid Label Overlap",
                               value = TRUE,
                               status = "primary"
                             ),
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
                                                  #h5(htmlOutput("TMStopPreview"))
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
                                                 # h5(htmlOutput("TMapSynPreview"))
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
                                                                         #"Fast Greedy" = "fast_greedy",
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
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Map",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot", height = "75vh"))
                            ),
                            tabPanel("Network",
                                     shinycssloaders::withSpinner(visNetworkOutput("NetPlot", height = "75vh"))),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable"))
                            ),
                            tabPanel("Clusters",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTableCluster"))
                            ),
                            tabPanel("Documents",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTableDocument"))
                            )
                )
              )
            )
    ),
    ##### thematic evolution ----
    tabItem("thematicEvolution",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Thematic Evolution"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyTE")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportTE")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "TEplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
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
                             materialSwitch(
                               inputId = "noOverlapTE",
                               label = "Avoid Label Overlap",
                               value = TRUE,
                               status = "primary"
                             ),
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
                                                  #h5(htmlOutput("TEStopPreview"))
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
                                                  #h5(htmlOutput("TESynPreview"))
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
                                                                         #"Fast Greedy" = "fast_greedy",
                                                                         "InfoMap" = "infomap",
                                                                         "Leading Eigenvalues" = "leading_eigen",
                                                                         "Leiden" = "leiden",
                                                                         "Louvain" = "louvain",
                                                                         "Spinglass" = "spinglass",
                                                                         "Walktrap" = "walktrap"),
                                                             selected = "walktrap"))
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
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Thematic Evolution", 
                                     tabsetPanel(type="tabs",
                                                 tabPanel("Map",
                                                          shinycssloaders::withSpinner(visNetworkOutput(outputId = "TEPlot", height = "75vh", width = "100%"))
                                                 ),
                                                 tabPanel("Table",
                                                          shinycssloaders::withSpinner(DT::DTOutput(outputId = "TETable"))
                                                 )
                                     )
                            ),
                            tabPanel("Time Slice 1", tabsetPanel(type="tabs",
                                                                 tabPanel("Map",
                                                                          shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot1", height = "75vh"))
                                                                 ),
                                                                 tabPanel("Network",
                                                                          shinycssloaders::withSpinner(visNetworkOutput("NetPlot1", height = "75vh"))
                                                                 ),
                                                                 tabPanel("Table",
                                                                          shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable1"))
                                                                 ),
                                                                 tabPanel("Clusters",
                                                                          shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTableCluster1"))
                                                                 ),
                                                                 tabPanel("Documents",
                                                                          shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTableDocument1"))
                                                                 )
                            )      
                            ),
                            tabPanel("Time Slice 2", tabsetPanel(type="tabs",
                                                                 tabPanel("Map",
                                                                          shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot2", height = "75vh"))
                                                                 ),
                                                                 tabPanel("Network",
                                                                          shinycssloaders::withSpinner(visNetworkOutput("NetPlot2", height = "75vh"))
                                                                 ),
                                                                 tabPanel("Table",
                                                                          shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable2"))
                                                                 ),
                                                                 tabPanel("Clusters",
                                                                          shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTableCluster2"))
                                                                 ),
                                                                 tabPanel("Documents",
                                                                          shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTableDocument2"))
                                                                 )
                            ) 
                            ),
                            tabPanel("Time Slice 3", tabsetPanel(type="tabs",
                                                                 tabPanel("Map",
                                                                          shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot3", height = "75vh"))
                                                                 ),
                                                                 tabPanel("Network",
                                                                          shinycssloaders::withSpinner(visNetworkOutput("NetPlot3", height = "75vh"))
                                                                 ),
                                                                 tabPanel("Table",
                                                                          shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable3"))
                                                                 ),
                                                                 tabPanel("Clusters",
                                                                          shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTableCluster3"))
                                                                 ),
                                                                 tabPanel("Documents",
                                                                          shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTableDocument3"))
                                                                 )
                            )
                            ),
                            tabPanel("Time Slice 4", tabsetPanel(type="tabs",
                                                                 tabPanel("Map",
                                                                          shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot4", height = "75vh"))
                                                                 ),
                                                                 tabPanel("Network",
                                                                          shinycssloaders::withSpinner(visNetworkOutput("NetPlot4", height = "75vh"))
                                                                 ),
                                                                 tabPanel("Table",
                                                                          shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable4"))
                                                                 ),
                                                                 tabPanel("Clusters",
                                                                          shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTableCluster4"))
                                                                 ),
                                                                 tabPanel("Documents",
                                                                          shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTableDocument4"))
                                                                 )
                            ) 
                            ),
                            tabPanel("Time Slice 5", tabsetPanel(type="tabs",
                                                                 tabPanel("Map",
                                                                          shinycssloaders::withSpinner(plotlyOutput(outputId = "TMPlot5", height = "75vh"))
                                                                 ),
                                                                 tabPanel("Network",
                                                                          shinycssloaders::withSpinner(visNetworkOutput("NetPlot5", height = "75vh"))
                                                                 ),
                                                                 tabPanel("Table",
                                                                          shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable5"))
                                                                 ),
                                                                 tabPanel("Clusters",
                                                                          shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTableCluster5"))
                                                                 ),
                                                                 tabPanel("Documents",
                                                                          shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTableDocument5"))
                                                                 )
                            )
                            )
                )
              )
            )
    ),
    ##### factorial analysis ----
    tabItem("factorialAnalysis",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Factorial Analysis"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyCA")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportFA")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "FAplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
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
                                                  #h5(htmlOutput("CSStopPreview"))
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
                                                  #h5(htmlOutput("FASynPreview"))
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
                                                             choices = c("1" = "1",
                                                                         "2" = "2",
                                                                         "3" = "3",
                                                                         "4" = "4",
                                                                         "5" = "5",
                                                                         "6" = "6",
                                                                         "7" = "7",
                                                                         "8" = "8"),
                                                             selected = "1")))
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
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Word Map", 
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "CSPlot1", height = "75vh",width ="98.9%"))),
                            tabPanel("Topic Dendrogram", 
                                     shinycssloaders::withSpinner(visNetworkOutput("CSPlot4", width="auto", height = "75vh"))),
                            # tabPanel("Most Contributing Papers", 
                            #          shinycssloaders::withSpinner(plotOutput(
                            #            outputId = "CSPlot2"))),
                            # tabPanel("Most Cited Papers", 
                            #          shinycssloaders::withSpinner(plotOutput(
                            #            outputId = "CSPlot3"))),
                            tabPanel("Words by Cluster",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "CSTableW"))),
                            tabPanel("Articles by Cluster",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "CSTableD")))
                            
                )
              )
            )
    ),
    #### Intellectual Structure ----
    ##### co-citation network ----
    tabItem("coCitationNetwork",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Co-citation Network"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyCocit")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportCOCIT")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("actionBttn", c(export_bttn, list(
                             inputId = "screenCOCIT")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
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
                                                                #"Fast Greedy" = "fast_greedy",
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
                                                     value = 1000,
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
                                          value = 2,
                                          step=0.5)
                                 )),
                                 fluidRow(column(6,
                                                 selectInput(inputId ="cocit.shadow",
                                                             label = "Node shadow",
                                                             choices = c("Yes",
                                                                         "No"),
                                                             selected = "Yes")
                                 ),
                                 column(6,
                                        selectInput(inputId ="cocit.curved",
                                                    label = "Edit Nodes",
                                                    choices = c("Yes",
                                                                "No"),
                                                    selected = "No")
                                        
                                 )
                                 )
                             ),
                             br(),
                             fluidRow(column(6,
                                             div(style ="border-radius: 10px; border-width: 3px; font-size: 10px;",
                                                 align = "center",
                                                 width="100%",
                                                 downloadBttn(outputId = "network.cocit", label = ("Pajek"),
                                                              style = "pill", color = "primary"))),
                                      column(6,
                                             div(style ="border-radius: 10px; border-width: 3px; font-size: 10px;",
                                                 align = "center",
                                                 width="100%",
                                                 downloadBttn(outputId = "networkCocit.fig", label = ("HTML"),
                                                              style = "pill", color = "primary")))
                             ),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Network", 
                                     shinycssloaders::withSpinner(visNetworkOutput("cocitPlot", height = "75vh"))),       
                            tabPanel("Density", 
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "cocitOverlay", height = "75vh"))),
                            tabPanel("Table", 
                                     shinycssloaders::withSpinner(DT::DTOutput(
                                       outputId = "cocitTable"))),
                            tabPanel("Degree Plot", 
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "cocitDegree", height=700)))
                )
              )
            )
    ),
    ##### historiograph ----
    tabItem("historiograph",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Historiograph"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyHist")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportHIST")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("actionBttn", c(export_bttn, list(
                             inputId = "screenHIST")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
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
                                 solidHeader = FALSE,
                                 collapsed = FALSE,
                                 selectInput(inputId = "titlelabel",
                                             label = "Node label",
                                             choices = c("Short id (1st Author, Year)" = "short",
                                                         "Document Title" = "title",
                                                         "Authors' Keywords" = "keywords",
                                                         "Keywords Plus" = "keywordsplus"),
                                             selected = "short"),
                                 selectInput(inputId ="hist.isolates",
                                             label = "Remove Isolated Nodes",
                                             choices = c("Yes" = "yes",
                                                         "No" = "no"),
                                             selected = "yes"),
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
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Network", 
                                     shinycssloaders::withSpinner(visNetworkOutput("histPlotVis", height = "80vh"))),
                            tabPanel("Table", 
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "histTable")))
                )
              )
            )
    ),
    #### Social Structure ----
    ##### collaboration network ----
    tabItem("collabNetwork",
            fluidPage(
              fluidRow(
                column(8,h3(strong("Collaboration Network"), align = "center")),
                div(style=style_bttn,
                     title = t_run,
                     column(1, 
                            do.call("actionBttn", c(run_bttn, list(
                              inputId = "applyCol")
                            ))
                     )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportCOL")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("actionBttn", c(export_bttn, list(
                             inputId = "screenCOL")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
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
                                                                #"Fast Greedy" = "fast_greedy",
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
                                                     value = 1000,
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
                                                             selected = "Yes")
                                 ),
                                 column(6,
                                        selectInput(inputId ="soc.curved",
                                                    label = "Edit Nodes",
                                                    choices = c("Yes",
                                                                "No"),
                                                    selected = "No")
                                        
                                 ))
                             ),
                             br(),
                             fluidRow(column(6,
                                             div(style ="border-radius: 10px; border-width: 3px; font-size: 10px;",
                                                 align = "center",
                                                 width="100%",
                                                 downloadBttn(outputId = "network.col", label = ("Pajek"),
                                                              style = "pill", color = "primary"))),
                                      column(6,
                                             div(style ="border-radius: 10px; border-width: 3px; font-size: 10px;",
                                                 align = "center",
                                                 width="100%",
                                                 downloadBttn(outputId = "networkCol.fig", label = ("HTML"),
                                                              style = "pill", color = "primary")))
                             ),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Network", 
                                     shinycssloaders::withSpinner(visNetworkOutput("colPlot", height = "75vh"))),
                            tabPanel("Density", 
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "colOverlay", height = "75vh"))),
                            tabPanel("Table", 
                                     shinycssloaders::withSpinner(DT::DTOutput(
                                       outputId = "colTable"))),
                            tabPanel("Degree Plot", 
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "colDegree", height = "75vh")))
                )
              )
            )
    ),
    ##### collaboration world map ----
    tabItem("collabWorldMap",
            fluidPage(
              fluidRow(
                column(8,h3(strong("Countries' Collaboration World Map"), align = "center")),
                div(style=style_bttn,
                    title = t_run,
                    column(1, 
                           do.call("actionBttn", c(run_bttn, list(
                             inputId = "applyWM")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_report,
                    column(1, 
                           do.call("actionBttn", c(report_bttn, list(
                             inputId = "reportCOLW")
                           ))
                    )),
                div(style=style_bttn,
                    title = t_export,
                    column(1, 
                           do.call("downloadBttn", c(export_bttn, list(
                             outputId = "CCplot.save")
                           ))
                    )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
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
                             numericInput(inputId = "WMedgesize",
                                          label = "Edge size",
                                          min = 0.1,
                                          max = 20,
                                          value = 5),
                             right = TRUE, animate = TRUE, circle = TRUE,
                             style = "gradient",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "primary",
                             icon = icon("sliders"),
                             width = "300px"
                           )
                ), 
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot", 
                                     #shinycssloaders::withSpinner(plotOutput(outputId = "WMPlot"))
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "WMPlot", height = "75vh"))
                                     ),
                            tabPanel("Table", 
                                     shinycssloaders::withSpinner(DT::DTOutput(
                                       outputId = "WMTable")))
                )
              )
            )
    ),
    #### Report ----
    tabItem("report",
            fluidPage(
              fluidRow(
                h3(strong("Report"), align="center"),
                br(),
              ),
              fluidRow(
                  column(6,offset = 1,
                       box(title = strong("Select results to include in the Report",
                                    style='font-size:20px;color:white;'), 
                        status = "primary", width = 11, solidHeader = TRUE,
                         tags$style(HTML("
                         .box.box-solid.box-primary>.box-header {
                         background:#4379cd;
                         }
                         .box.box-solid.box-primary{
                         border-bottom-color:black;
                         border-left-color:black;
                         border-right-color:black;
                         border-top-color:black;
                         border-width:2px;
                                         }")),
                         uiOutput('reportSheets'),
                         tags$style("#reportSheets {font-size:18px;}")
                       )
                ),#column(1),
                column(2, 
                       div(style ="border-radius: 10px; border-width: 3px; font-size: 10px;",
                          align = "center",
                          #width="100%",
                       actionBttn(
                         inputId = 'allSheets',
                         label = strong('Select All'),
                         icon = icon("ok-circle", lib="glyphicon"),
                         style = "pill", color = "primary",
                         block = TRUE
                       ),
                       #tags$style("#allSheets {font-size:20px; color:#363636; background-color:white; text-align:center; border-width: 3px;}"),
                       br(),
                       actionBttn(
                         inputId = 'noSheets',
                         label = strong('Deselect All'),
                         icon = icon("remove-circle", lib="glyphicon"),
                         style = "pill", color = "primary",
                         block = TRUE
                       ),
                       #tags$style("#noSheets {font-size:20px; color:#363636; background-color:white; text-align:center; border-width: 3px;}"),
                       
                       #tags$style("#deleteAll {border-width: 3px;}"),
                       br(),
                       hr(),
                       downloadBttn(
                         outputId="report.save",
                         label = strong("Export Report"),
                         style = "pill", color = "success",
                         size = "md",
                         block = TRUE,
                         no_outline = TRUE,
                         icon = icon(name ="download-alt", lib="glyphicon")
                       ),#, tags$style("#report.save {border-width: 3px;}")
                       br(),
                       hr(),
                       actionBttn(
                         inputId = 'deleteAll',
                         label = strong('Delete Report'),
                         icon = icon("exclamation-sign", lib="glyphicon"),
                         style = "pill", color = "danger",
                         block = TRUE
                       )
                       
                       )   
                )
              )
              
              
            )
    ),
    #### Settings ----
    tabItem("settings",
            fluidPage(
              fluidRow(
                h3(strong("Settings"), align="center"),
                br()
              ),
              fluidRow(column(6,
                              h3("Plot settings:"),
                              br(),
                              sliderTextInput(
                                inputId = "dpi",
                                label = "Please select the desired DPI", 
                                grid = TRUE,
                                force_edges = TRUE,
                                choices = c("75", "150", "300", "600"),
                                width = "70%",
                                selected = "300"
                              ),
                              br(),
                              sliderTextInput(
                                inputId = "h",
                                label = "Please select the desired heigth in inches", 
                                grid = TRUE,
                                force_edges = TRUE,
                                width = "70%",
                                choices = seq(5,15),
                                selected = "7"
                              )
              ), column(6
                        ### To insert settings for default path, etc.
                        )
              )
            )
    )
  )
)

## Control Bar ####
# controlbar <- shinydashboardPlus::dashboardControlbar(id = "controlbar2",
#                                                       uiOutput("controlbar"),
#                                                       skin = "light",
#                                                       width = 350,
#                                                       overlay = FALSE,
#                                                       collapsed = TRUE,
#                                                       shinyjs::useShinyjs()
# )
## UI ####
ui <- shinydashboardPlus::dashboardPage(
                                        #shinyjs::useShinyjs(),
                                        header = header, 
                                        sidebar = sidebar, 
                                        body = body,
                                        #controlbar = controlbar,
                                        footer = NULL,
                                        options = list(sidebarExpandOnHover = TRUE),
                                        scrollToTop =TRUE
)
# END ----
