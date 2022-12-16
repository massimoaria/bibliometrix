source("libraries.R", local=TRUE)
suppressMessages(libraries())

# UI components ----
## Title ----
mytitle <- tags$link(tags$a(href = 'https://www.bibliometrix.org/',target="_blank",
                            tags$img(src="logo2.jpg", height = '40',width='40')),
                     strong("bibliometrix")
)

intro <- "javascript:void(window.open('https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html', '_blank'))"
importData <- "javascript:void(window.open('https://www.bibliometrix.org/vignettes/Data-Importing-and-Converting.html', '_blank'))"
slides <- "javascript:void(window.open('https://bibliometrix.org/biblioshiny/assets/player/KeynoteDHTMLPlayer.html#0', '_blank'))"
donation <- "javascript:void(window.open('https://www.bibliometrix.org/home/index.php/donation', '_blank'))"
bibliometrixWeb <- "javascript:void(window.open('https://www.bibliometrix.org/', '_blank'))" 
k_synth <- "javascript:void(window.open('https://www.k-synth.unina.it', '_blank'))"
github_aria <- "javascript:void(window.open('https://github.com/massimoaria/bibliometrix', '_blank'))"

## Header ----
header <- shinydashboardPlus::dashboardHeader(title = mytitle,
                                              titleWidth = 300,
                                              controlbarIcon = fa_i(name ="bars"),
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
                                              ),
                                              tags$li(class = "dropdown",
                                                      tags$style(".main-header .logo {height: 53px}")
                                              )
)

## Side Bar ----
sidebar <- shinydashboardPlus::dashboardSidebar(
  useShinyjs(),
  sidebarMenu(
    id="sidebarmenu",
    menuItem("biblioshiny",tabName = "biblioshinyy",icon = fa_i(name="house-user")),
    menuItem("Data",tabName = "uploadData",icon = fa_i(name = "file-import"),
             menuSubItem("Load Data", tabName = "loadData",icon = icon("chevron-right",lib = "glyphicon")),
             menuSubItem("Gathering Data", tabName = "gathData",icon = icon("chevron-right",lib = "glyphicon"))),
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
                column(12,
                       div(p("For an introduction and live examples, visit the ",
                             em(a("bibliometrix website.", 
                                  href = "https://www.bibliometrix.org", target="_blank")), 
                             style="text-align:center; font-size:20px;")),
                )
              )
            )
    ),
    #### Data ----
    ##### load Data ----
    tabItem("loadData",
            fluidPage(
              fluidRow(
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
                  )
                )
                ),
                tags$style(
                  HTML(
                    ".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                  color: #000000 !important;
                  }"
                  )
                ),
                shinycssloaders::withSpinner(DT::DTOutput("apiContents"))
              )
            )
    ),
    #### Filters ----
    tabItem("filters",
            fluidRow(
              DT::DTOutput("dataFiltered"))
    ),
    #### Overview ----
    ##### main information ----
    tabItem("mainInfo",
            fluidPage(
              fluidRow(column(10,
                h2(strong("Main Information"), align = "center")),
                column(2, 
                       actionButton("reportMI", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
                )
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
              fluidRow(column(9,
                h2(strong("Annual Scientific Production"), align = "center")),
                column(2, 
                       actionButton("reportASP", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
                ),
                column(1,
                br(),
                ## dropdown options
                dropdown(
                  h4(strong("Annual Growth Rate")),
                  br(),
                  verbatimTextOutput("CAGR", placeholder = TRUE),
                  br(),
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
                  ),
                  br(),
                  conditionalPanel(condition = 'input.ASPdpi != "null"',
                                   sliderInput(
                                     'ASPh',
                                     label =h4(em(strong("Height (in inches)"))),
                                     value = 7, min = 1, max = 20, step = 1),
                                   downloadButton("ASPplot.save", strong("Export plot as png"),
                                                  style ="border-radius: 10px; border-width: 3px; vertical-align: 'middle';font-size: 20px;",
                                                  width = "100%")
                  ),
                  right = TRUE,
                  animate = TRUE,
                  style = "unite", icon = icon("cog",lib="glyphicon"),
                  width = "300px", 
                  tooltip = "Options"#,
                  #showOnCreate = TRUE
                )
                # End Dropdown options
                )
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
                h2(strong("Average Citations Per Year"), align = "center")),
                column(2, 
                       actionButton("reportACpY", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
                )
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
                column(6,
                       h2(strong("Three-Field Plot"), align = "center")),
                column(2,
                       actionButton("apply3F", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                      width = "80%",
                                      icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportTFP", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
                       ),
                column(2,
                       screenshotButton(label=strong("Export"), id = "ThreeFieldsPlot",
                                        style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                        scale = 2, width = "80%",
                                        file=paste("ThreeFieldPlot-", Sys.Date(), ".png", sep=""))
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
                       h2(strong("Most Relevant Sources"), align = "center")),
                column(2,actionButton("applyMRSources", strong("Run"),style ="border-radius: 10px; border-width: 3px;font-size: 15px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportMRS", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("Most Local Cited Sources"), align = "center")),
                column(2,
                       actionButton("applyMLCSources", strong("Run"),style ="border-radius: 10px; border-width: 3px;font-size: 15px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportMLS", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                  h2(strong("Core Sources by Bradford's Law"), align = "center")
                ),
                column(2, 
                         actionButton("reportBradford", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                      width = "80%",
                                      icon = icon(name ="plus", lib="glyphicon"))
                )
                
                #)
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
                       h2(strong("Sources' Local Impact"), align = "center")),
                column(2,
                       actionButton("applyHsource", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportSI", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("Sources' Production over Time"), align = "center")),
                column(2,actionButton("applySOGrowth", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                      width = "80%",
                                      icon = fa_i(name ="play"))
                ),
                column(2, 
                       actionButton("reportSD", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("Most Relevant Authors"), align = "center")),
                column(2,
                       actionButton("applyMRAuthors", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportMRA", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("Most Local Cited Authors"), align = "center")),
                column(2,actionButton("applyMLCAuthors", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportMLCA", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("Authors' Production over Time"), align = "center")),
                column(2,
                       actionButton("applyAUoverTime", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportAPOT", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                  h2(strong("Author Productivity through Lotka's Law"), align = "center")
                ),
                column(2, 
                       actionButton("reportLotka", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
                )
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
                       h2(strong("Authors' Local Impact"), align = "center")),
                column(2,
                       actionButton("applyHAuthors", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play"))
                ),
                column(2, 
                       actionButton("reportAI", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("Most Relevant Affiliations"), align = "center")),
                column(2,
                       actionButton("applyMRAffiliations", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportMRAFF", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("Affiliations' Production over Time"), align = "center")),
                column(2,actionButton("applyAFFGrowth", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                      width = "80%",
                                      icon = fa_i(name ="play"))
                ),
                column(2, 
                       actionButton("reportAFFPOT", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("Corresponding Author's Countries"), align = "center")),
                column(2,
                       actionButton("applyCAUCountries", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportMRCO", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                h2(strong("Countries' Scientific Production"), align = "center")),
                column(2, 
                       actionButton("reportCSP", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
                )
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
                       h2(strong("Countries' Production over Time"), align = "center")),
                column(2,actionButton("applyCOGrowth", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                      width = "80%",
                                      icon = fa_i(name ="play"))
                ),
                column(2, 
                       actionButton("reportCPOT", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("Most Cited Countries"), align = "center")),
                column(2,actionButton("applyMCCountries", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportMCCO", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("Most Global Cited Documents"), align = "center")),
                column(2,
                       actionButton("applyMGCDocuments", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportMCD", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("Most Local Cited Documents"), align = "center")),
                column(2, actionButton("applyMLCDocuments", strong("Run"),
                                       style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                       width = "80%",
                                       icon = fa_i(name="play"))),
                column(2, 
                       actionButton("reportMLCD", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("Most Local Cited References"), align = "center")),
                column(2,
                       actionButton("applyMLCReferences", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportMLCR", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("Reference Spectroscopy"), align = "center")
                ),
                column(2,
                       actionButton("applyRPYS", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportRPYS", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("Most Frequent Words"), align = "center")
                ),
                column(2,
                       actionButton("applyMFWords", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play"))
                ),
                column(2, 
                       actionButton("reportMFW", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("WordCloud"), align = "center")),
                column(2,actionButton("applyWordCloud", strong("Run"),
                                      style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportWC", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("TreeMap"), align = "center")),
                column(2,
                       actionButton("applyTreeMap", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportTREEMAP", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("Words' Frequency over Time"), align = "center")),
                column(2,
                       actionButton("applyWD", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportWD", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("Trend Topics"), align = "center")),
                column(2,
                       actionButton("applyTrendTopics", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportTT", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("Clustering by Coupling"), align = "center")),
                column(2,
                       actionButton("applyCM", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportCC", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
    ##### co-occurence network ----
    tabItem("coOccurenceNetwork",
            fluidPage(
              fluidRow(
                column(8,
                       h2(strong("Co-occurrence Network"), align = "center")),
                column(2,
                       actionButton("applyCoc", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportCOC", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Network", 
                                     shinycssloaders::withSpinner(visNetworkOutput("cocPlot", height = "75vh"))),
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
                       h2(strong("Thematic Map"), align = "center")),
                column(2,actionButton("applyTM", strong("Run"),
                                      style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportTM", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
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
                       h2(strong("Thematic Evolution"), align = "center")),
                column(2,actionButton("applyTE", strong("Run"),
                                      style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play"))
                ),
                column(2, 
                       actionButton("reportTE", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Thematic Evolution", 
                                     tabsetPanel(type="tabs",
                                                 tabPanel("Map",
                                                          shinycssloaders::withSpinner(plotlyOutput(outputId = "TEPlot", height = "75vh"))
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
                       h2(strong("Factorial Analysis"), align = "center")),
                column(2,actionButton("applyCA", strong("Run"),
                                      style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportFA", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
                )
              ),
              fluidRow(
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
    ),
    #### Intellectual Structure ----
    ##### co-citation network ----
    tabItem("coCitationNetwork",
            fluidPage(
              fluidRow(
                column(8,
                       h2(strong("Co-citation Network"), align = "center")),
                column(2,actionButton("applyCocit", strong("Run"),
                                      style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportCOCIT", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Network", 
                                     shinycssloaders::withSpinner(visNetworkOutput("cocitPlot", height = "75vh"))),        
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
                       h2(strong("Historiograph"), align = "center")),
                column(2,actionButton("applyHist", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportHIST", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Network", 
                                     shinycssloaders::withSpinner(visNetworkOutput("histPlotVis", height = "80vh"))),
                            # tabPanel("Plot",
                            #          shinycssloaders::withSpinner(plotlyOutput(outputId = "histPlot", height = "75vh"))),
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
                column(8,h2(strong("Collaboration Network"), align = "center")),
                column(2,actionButton("applyCol", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportCOL", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Network", 
                                     shinycssloaders::withSpinner(visNetworkOutput("colPlot", height = "75vh"))), 
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
                column(8,h2(strong("Countries' Collaboration World Map"), align = "center")),
                column(2,actionButton("applyWM", strong("Run"),
                                      style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play"))),
                column(2, 
                       actionButton("reportCOLW", strong("Report"),style ="border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;",
                                    width = "80%",
                                    icon = icon(name ="plus", lib="glyphicon"))
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot", 
                                     shinycssloaders::withSpinner(plotOutput(outputId = "WMPlot"))),
                            tabPanel("Table", 
                                     shinycssloaders::withSpinner(DT::DTOutput(
                                       outputId = "WMTable")))
                )
              )
            )
    ),
    #### Report ----
    ## NEW ----
    tabItem("report",
            fluidPage(
              fluidRow(
                h2(strong("Report"), align="center"),
                br(),
              ),
              fluidRow(
                column(7,offset = 1,
                       box(title = strong("Select results to include in the Report",
                                    style='font-size:25px;color:white;'), 
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
                         tags$style("#reportSheets {font-size:23px;}")
                       )
                ),
                column(3, 
                       actionBttn(
                         inputId = 'allSheets',
                         label = strong('SELECT ALL'),
                         icon = icon("ok-circle", lib="glyphicon"),
                         style = "unite", #float
                         color = "default",
                         block = TRUE
                       ),
                       tags$style("#allSheets {font-size:20px; color:#363636; background-color:white; text-align:center; border-width: 3px;}"),
                       br(),
                       actionBttn(
                         inputId = 'noSheets',
                         label = strong('DESELECT ALL'),
                         icon = icon("remove-circle", lib="glyphicon"),
                         style = "unite",
                         color = "default",
                         block = TRUE
                       ),
                       tags$style("#noSheets {font-size:20px; color:#363636; background-color:white; text-align:center; border-width: 3px;}"),
                       br(),
                       actionBttn(
                         inputId = 'deleteAll',
                         label = strong('DELETE REPORT'),
                         icon = icon("exclamation-sign", lib="glyphicon"),
                         style = "unite",
                         color = "danger", 
                         block = TRUE
                       ),
                       tags$style("#deleteAll {border-width: 3px;}"),
                       br(),
                       downloadBttn(
                         outputId="report.save",
                         label = strong("SAVE REPORT"),
                         style = "unite",
                         color = "primary",
                         size = "md",
                         block = TRUE,
                         no_outline = TRUE,
                         icon = icon(name ="download-alt", lib="glyphicon")
                       ), tags$style("#report.save {border-width: 3px;}")
                       
                       
                )
              )
              
              
            )
    )
  )
)

## Control Bar ####
controlbar <- shinydashboardPlus::dashboardControlbar(id = "controlbar2",
                                                      uiOutput("controlbar"),
                                                      skin = "light",
                                                      width = 350,
                                                      overlay = FALSE,
                                                      collapsed = TRUE,
                                                      shinyjs::useShinyjs()
)
## UI ####
ui <- shinydashboardPlus::dashboardPage(shinyjs::useShinyjs(),
                                        header = header, 
                                        sidebar = sidebar, 
                                        body = body,
                                        controlbar = controlbar,
                                        footer = NULL,
                                        options = list(sidebarExpandOnHover = TRUE),
                                        scrollToTop =TRUE
)
# END ----
