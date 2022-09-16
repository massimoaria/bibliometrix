if (!(require(bibliometrix))){install.packages("bibliometrix"); require(bibliometrix, quietly=TRUE)}
if (!(require(shiny))){install.packages("shiny"); require(shiny, quietly=TRUE)} 
#if (!(require(rio))){install.packages("rio")} 
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
if (!require(shinyjs)){install.packages("shinyjs"); require(shinyjs, quietly=TRUE)}
if (!require(shinyscreenshot)){install.packages("shinyscreenshot"); require(shinyscreenshot, quietly=TRUE)}
require(Matrix, quietly = TRUE)
require(dimensionsR, quietly = TRUE)
require(pubmedR, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidyr, quietly = TRUE)

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
customTheme <- shinyDashboardThemeDIY(
  appFontFamily = "Helvetica"
  ,appFontColor = "rgb(0,0,0)" 
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(255,255,255)"
  ,logoBackColor = "rgb(88,101,185)"
  
  ,headerButtonBackColor = "rgb(88,101,185)"
  ,headerButtonIconColor = "rgb(248,248,248)"
  ,headerButtonBackColorHover = "rgb(75,90,179)"
  ,headerButtonIconColorHover = "rgb(248,248,248)" 
  ,headerBackColor = "rgb(88,101,185)"
  ,headerBoxShadowColor = "rgb(210,210,210)"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(88,101,185)"
    ,colorMiddle = "rgb(29,143,225)"
    ,colorEnd = "rgb(34,220,253)"
    ,colorStartPos = 0
    ,colorMiddlePos = 55
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  ,sidebarShadowRadius = "0px 0px 0px"
  ,sidebarShadowColor = "#aaaaaa"
  ,sidebarUserTextColor = "rgb(255,255,255)"
  ,sidebarSearchBackColor = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(34,220,253)"
    ,colorMiddle = "rgb(29,143,225)"
    ,colorEnd = "rgb(88,101,185)"
    ,colorStartPos = 0
    ,colorMiddlePos = 75
    ,colorEndPos = 100)
  ,sidebarSearchIconColor = "rgb(255,255,255)"
  ,sidebarSearchBorderColor = "rgb(29,143,225)"
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 15
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(34,220,253)"
    ,colorMiddle = "rgb(29,143,225)"
    ,colorEnd = "rgb(88,101,185)"
    ,colorStartPos = 0
    ,colorMiddlePos = 75
    ,colorEndPos = 100)
  ,sidebarTabTextColorSelected = "rgb(255,255,255)"
  ,sidebarTabRadiusSelected = "0px 0px 0px 0px"
  ,sidebarTabBackColorHover = "rgb(255,255,255)"
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 0px 0px 0px"
  
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgb(88,101,185)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ,buttonBackColor = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(34,220,253)"
    ,colorMiddle = "rgb(29,143,225)"
    ,colorEnd = "rgb(88,101,185)"
    ,colorStartPos = 0
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
  
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  )

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
              });'
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
              fluidRow(
                h2(strong("Main Information"), align = "center")),
              fluidRow(
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
              fluidRow(
                h2(strong("Annual Scientific Production"), align = "center")
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
                            )
                )
              )
            ),
    ##### average citation per year ----
    tabItem("averageCitPerYear",
            fluidPage(
              fluidRow(
                h2(strong("Average Citation Per Year"), align = "center")
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
                column(10,
                       h2(strong("Three-Field Plot"), align = "center")),
                column(2,actionButton("apply3F", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                      width = "80%",
                                      icon = fa_i(name ="play")))
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
                column(10,
                       h2(strong("Most Relevant Sources"), align = "center")),
                column(2,actionButton("applyMRSources", strong("Run"),style ="border-radius: 10px; border-width: 3px;font-size: 20px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play")))
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
                column(10,
                       h2(strong("Most Local Cited Sources (from Reference Lists)"), align = "center")),
                column(2,
                       actionButton("applyMLCSources", strong("Run"),style ="border-radius: 10px; border-width: 3px;font-size: 20px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play")))
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
              fluidRow(
                titlePanel(
                  h2(strong("Source clustering through Bradford's Law"), align = "center")
                )
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
    ##### source impact ----
    tabItem("sourceImpact",
            fluidPage(
              fluidRow(
                column(10,
                       h2(strong("Source Local Impact"), align = "center")),
                column(2,
                       actionButton("applyHsource", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                    width = "80%",
                                    icon = fa_i(name ="play")))
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
    ##### source dynamics ----
    tabItem("sourceDynamics",
            fluidPage(
              fluidRow(
                column(10,
                       h2(strong("Source Dynamics"), align = "center")),
                column(2,actionButton("applySOGrowth", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                      width = "80%",
                                      icon = fa_i(name ="play"))
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
                column(10,
                       h2(strong("Most Relevant Authors"), align = "center")),
                column(2,
                       actionButton("applyMRAuthors", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play")))
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
                column(10,
                       h2(strong("Most Local Cited Authors"), align = "center")),
                column(2,actionButton("applyMLCAuthors", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play")))
                
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
                column(10,
                       h2(strong("Authors' Production over Time"), align = "center")),
                column(2,
                       actionButton("applyAUoverTime", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play")))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "TopAuthorsProdPlot", height = "75vh",width ="98.9%"))
                            ),
                            tabPanel("Table - Authors' Production per Year",
                                     shinycssloaders::withSpinner(DT::DTOutput("TopAuthorsProdTable"))
                            ),
                            tabPanel("Table - Author's Documents",
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
                titlePanel(
                  h2(strong("Author Productivity through Lotka's Law"), align = "center")
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
    ##### author impact ----
    tabItem("authorImpact",
            fluidPage(
              fluidRow(
                column(10,
                       h2(strong("Author Local Impact"), align = "center")),
                column(2,
                       actionButton("applyHAuthors", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play"))
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
                column(10,
                       h2(strong("Most Relevant Affiliations"), align = "center")),
                column(2,
                       actionButton("applyMRAffiliations", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play")))
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
                column(10,
                       h2(strong("Affiliations' Production over Time"), align = "center")),
                column(2,actionButton("applyAFFGrowth", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                      width = "80%",
                                      icon = fa_i(name ="play"))
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
                column(10,
                       h2(strong("Corresponding Author's Country"), align = "center")),
                column(2,
                       actionButton("applyCAUCountries", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play")))
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
              fluidRow(
                h2(strong("Country Scientific Production"), align = "center")
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
                column(10,
                       h2(strong("Countries' Production over Time"), align = "center")),
                column(2,actionButton("applyCOGrowth", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                      width = "80%",
                                      icon = fa_i(name ="play"))
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
                column(10,
                       h2(strong("Most Cited Countries"), align = "center")),
                column(2,actionButton("applyMCCountries", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play")))
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
                column(10,
                       h2(strong("Most Global Cited Documents"), align = "center")),
                column(2,
                       actionButton("applyMGCDocuments", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play")))
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
                column(10,
                       h2(strong("Most Local Cited Documents"), align = "center")),
                column(2, actionButton("applyMLCDocuments", strong("Run"),
                                       style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                       width = "80%",
                                       icon = fa_i(name="play")))
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
                column(10,
                       h2(strong("Most Local Cited References"), align = "center")),
                column(2,
                       actionButton("applyMLCReferences", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play")))
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
                column(10,
                       h2(strong("References Spectroscopy"), align = "center")
                ),
                column(2,
                       actionButton("applyRPYS", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play")))
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
                column(10,
                       h2(strong("Most Frequent Words"), align = "center")
                ),
                column(2,
                       actionButton("applyMFWords", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play"))
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
                column(10,
                       h2(strong("WordCloud"), align = "center")),
                column(2,actionButton("applyWordCloud", strong("Run"),
                                      style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play")))
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
                column(10,
                       h2(strong("TreeMap"), align = "center")),
                column(2,
                       actionButton("applyTreeMap", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play")))
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
                column(10,
                       h2(strong("Word Dynamics"), align = "center")),
                column(2,
                       actionButton("applyWD", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play")))
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
                column(10,
                       h2(strong("Trend Topics"), align = "center")),
                column(2,
                       actionButton("applyTrendTopics", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play")))
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
                column(10,
                       h2(strong("Clustering by Coupling"), align = "center")),
                column(2,
                       actionButton("applyCM", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play")))
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
                column(10,
                       h2(strong("Co-occurrence Network"), align = "center")),
                column(2,
                       actionButton("applyCoc", strong("Run"),
                                    style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                    width = "80%",icon = fa_i(name ="play")))
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
                column(10,
                       h2(strong("Thematic Map"), align = "center")),
                column(2,actionButton("applyTM", strong("Run"),
                                      style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play")))
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
                column(10,
                       h2(strong("Thematic Evolution"), align = "center")),
                column(2,actionButton("applyTE", strong("Run"),
                                      style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play"))
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
                column(10,
                       h2(strong("Factorial Analysis"), align = "center")),
                column(2,actionButton("applyCA", strong("Run"),
                                      style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play")))
              ),
              fluidRow(
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
            ),
    #### Intellectual Structure ----
    ##### co-citation network ----
    tabItem("coCitationNetwork",
            fluidPage(
              fluidRow(
                column(10,
                       h2(strong("Co-citation Network"), align = "center")),
                column(2,actionButton("applyCocit", strong("Run"),
                                      style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play")))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Newtwork", 
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
                column(10,
                       h2(strong("Historiograph"), align = "center")),
                column(2,actionButton("applyHist", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play")))
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
                column(10,h2(strong("Collaboration Network"), align = "center")),
                column(2,actionButton("applyCol", strong("Run"),style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play")))
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
                column(10,h2(strong("Collaboration WorldMap"), align = "center")),
                column(2,actionButton("applyWM", strong("Run"),
                                      style ="border-radius: 10px; border-width: 3px; font-size: 20px; margin-top: 15px;",
                                      width = "80%",icon = fa_i(name ="play")))
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
