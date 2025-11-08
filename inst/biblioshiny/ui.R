source("libraries.R", local = TRUE)
source("helpContent.R", local = TRUE)
source("utils.R", local = TRUE)
source("contentAnalysisUI.R", local = TRUE)
source("cssTags.R", local = TRUE)
source("openalex_api.R", local = TRUE)
source("pubmed_api.R", local = TRUE)

suppressMessages(libraries())

# UI components ----
## Title ----
mytitle <- tags$link(
  tags$a(
    href = "https://www.bibliometrix.org/",
    target = "_blank",
    tags$img(src = "logo2.jpg", height = "40", width = "40")
  ),
  strong("bibliometrix")
)

intro <- "https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html"
importData <- "https://www.bibliometrix.org/vignettes/Data-Importing-and-Converting.html"
slides <- "https://bibliometrix.org/biblioshiny/assets/player/KeynoteDHTMLPlayer.html#0"
donation <- "https://www.bibliometrix.org/home/index.php/donation"
bibliometrixWeb <- "https://www.bibliometrix.org/"
k_synth <- "https://www.k-synth.unina.it"
github_aria <- "https://github.com/massimoaria/bibliometrix"

biblioshinyVersion <- substr(packageVersion("bibliometrix"), 1, 7)

style_opt <- "border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;" # (option button)
style_bttn <- "border-radius: 10px; border-width: 3px; font-size: 15px; margin-top: 15px;" # (action buttons)
t_report <- "Add Results to the Report"
t_export <- "Export Plot as PNG"
t_run <- "Run the Analysis"
run_bttn <- list(
  label = NULL,
  style = "material-circle",
  color = "primary",
  icon = icon(name = "play", lib = "glyphicon")
)
report_bttn <- list(
  label = NULL,
  style = "material-circle",
  color = "primary",
  icon = icon(name = "plus", lib = "glyphicon")
)
export_bttn <- list(
  label = NULL,
  style = "material-circle",
  color = "primary",
  icon = icon(name = "download", lib = "glyphicon")
)

## load content of Info page
biblioAI <- helpContent()$biblioAI
info <- helpContent()$info
pubs <- helpContent()$publications
filters <- helpContent()$filters
authorProfile <- helpContent()$authorProfile
referenceMatching_help <- helpContent()$referenceMatching

## Header ----
header <- shinydashboardPlus::dashboardHeader(
  title = mytitle,
  titleWidth = 300,
  tags$li(
    class = "dropdown",
    tags$a(
      title = "Total downloads from CRAN", # ← Tooltip
      icon("cloud-arrow-down", lib = "font-awesome"),
      tags$span(
        HTML(suppressWarnings(format_abbreviated(total_downloads(
          "bibliometrix"
        )))),
        style = "margin-left: 5px; font-weight: bold;"
      )
    )
  ),
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
    ),
    messageItem(
      icon = icon("info-circle"),
      from = paste0("Version ", biblioshinyVersion),
      message = "",
      href = NULL
    )
  ),
  dropdownMenu(
    type = "messages",
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
    icon = fa_i(name = "cube"),
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
  ),
  # Settings Button - uses actionLink to trigger server-side tab change
  tags$li(
    class = "dropdown",
    actionButton(
      inputId = "go_to_settings",
      label = NULL,
      icon = icon("gear", lib = "font-awesome"),
      style = "background: transparent; 
               border: none; 
               color: #fff; 
               font-size: 20px;  /* Increased from 16px to 20px */
               margin-top: 7px; 
               cursor: pointer;",
      title = "Settings"
    )
  ),
  tags$li(
    class = "dropdown",
    tags$style(".main-header .logo {height: 53px}")
  )
)

## New Sidebar ----
sidebar <- shinydashboardPlus::dashboardSidebar(
  width = 300,
  useShinyjs(),

  # JavaScript to add IDs and hide menu items on load
  tags$script(HTML(
    "
    $(document).ready(function() {
      // Wait for Shiny to finish rendering
      setTimeout(function() {
        // Function to find and add ID to menu item by text
        function addIdToMenuItem(text, id) {
          $('.sidebar-menu > li.treeview').each(function() {
            var menuText = $(this).find('> a > span').first().text().trim();
            if (menuText === text) {
              $(this).attr('id', id);
              $(this).hide(); // Hide initially
            }
          });
        }
        
        // Function to add ID to simple menu items (non-treeview)
        function addIdToSimpleMenuItem(text, id) {
          $('.sidebar-menu > li:not(.treeview)').each(function() {
            var menuText = $(this).find('> a > span').text().trim();
            if (menuText === text) {
              $(this).attr('id', id);
              $(this).hide(); // Hide initially
            }
          });
        }
        
        // Add IDs to menu items and hide them
        addIdToSimpleMenuItem('Filters', 'menu-filters');
        addIdToMenuItem('Overview', 'menu-overview');
        addIdToMenuItem('Sources', 'menu-sources');
        addIdToMenuItem('Authors', 'menu-authors');
        addIdToMenuItem('Documents', 'menu-documents');
        addIdToMenuItem('Clustering', 'menu-clustering');
        addIdToMenuItem('Conceptual Structure', 'menu-conceptual');
        addIdToMenuItem('Intellectual Structure', 'menu-intellectual');
        addIdToMenuItem('Social Structure', 'menu-social');
        addIdToSimpleMenuItem('Report', 'menu-report');
        addIdToSimpleMenuItem('TALL Export', 'menu-tall');
        
        // NOTE: Section headers are NOT hidden - they stay visible
        
        console.log('Menu items hidden on initialization');
      }, 100);
    });
  "
  )),

  sidebarMenu(
    id = "sidebarmenu",

    # Always visible menu items
    menuItem(
      "biblioshiny",
      tabName = "biblioshinyy",
      icon = fa_i(name = "house-user")
    ),
    menuItem(
      "Info",
      tabName = "info",
      icon = fa_i(name = "circle-info"),
      menuSubItem(
        "Biblio AI",
        tabName = "biblioAI",
        icon = fa_i(name = "microchip")
      ),
      menuSubItem(
        "Supported Files",
        tabName = "supFiles",
        icon = fa_i(name = "database")
      ),
      menuSubItem(
        "Team's Publications",
        tabName = "pubs",
        icon = fa_i(name = "book")
      )
    ),

    ### SEARCH SECTION ----
    tags$div(
      style = "display: flex;
          align-items: center;
          font-size: 14px;
          font-weight: 600;
          color: #FFFFFF;
          background: rgba(255,255,255,0.1);
          padding: 10px 10px;
          margin: 15px 8px 8px 8px;
          border-radius: 6px;
          border-left: 3px solid #4FC3F7;
          letter-spacing: 0.8px;",
      tags$span(
        style = "background: #4FC3F7;
            padding: 4px 8px;
            border-radius: 4px;
            margin-right: 10px;
            font-size: 12px;",
        icon("magnifying-glass")
      ),
      "SEARCH"
    ),

    menuItem(
      "Data",
      tabName = "uploadData",
      icon = fa_i(name = "file-import"),
      menuSubItem(
        "Import or Load",
        tabName = "loadData",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuItem(
        "API",
        startExpanded = TRUE,
        menuSubItem(
          "OpenAlex",
          tabName = "openalexMenu",
          icon = icon("chevron-right", lib = "glyphicon")
        ),
        menuSubItem(
          "Pubmed",
          tabName = "pubmedMenu",
          icon = icon("chevron-right", lib = "glyphicon")
        )
      ),
      menuSubItem(
        "Merge Collections",
        tabName = "mergeData",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Reference Matching",
        tabName = "refMatching",
        icon = icon("chevron-right", lib = "glyphicon")
      )
    ),

    # === SECTION HEADERS (VISIBLE) + MENU ITEMS (HIDDEN) === #

    # APPRAISAL SECTION HEADER (always visible) ----
    tags$div(
      id = "appraisal-header",
      style = "display: flex;
          align-items: center;
          justify-content: space-between;
          font-size: 14px; 
          font-weight: 600; 
          color: #FFFFFF; 
          background: rgba(255,255,255,0.1);
          padding: 10px 10px; 
          margin: 15px 8px 8px 8px;
          border-radius: 6px;
          border-left: 3px solid #66BB6A;
          letter-spacing: 0.8px;",
      tags$div(
        style = "display: flex; align-items: center;",
        tags$span(
          style = "background: #66BB6A; 
              padding: 4px 8px; 
              border-radius: 4px; 
              margin-right: 10px;
              font-size: 12px;",
          icon("filter")
        ),
        "APPRAISAL"
      )
    ),

    # Filters - will get ID 'menu-filters' via JavaScript (hidden initially)
    menuItem(
      "Filters",
      tabName = "filters",
      icon = fa_i(name = "filter")
    ),

    # ANALYSIS SECTION HEADER (always visible) ----
    tags$div(
      id = "analysis-header",
      style = "display: flex;
          align-items: center;
          font-size: 14px;
          font-weight: 600;
          color: #FFFFFF;
          background: rgba(255,255,255,0.1);
          padding: 10px 10px;
          margin: 15px 8px 8px 8px;
          border-radius: 6px;
          border-left: 3px solid #FFA726;
          letter-spacing: 0.8px;",
      tags$span(
        style = "background: #FFA726;
            padding: 4px 8px;
            border-radius: 4px;
            margin-right: 10px;
            font-size: 12px;",
        icon("chart-line")
      ),
      "ANALYSIS"
    ),

    # Overview - will get ID 'menu-overview' via JavaScript (hidden initially)
    menuItem(
      "Overview",
      tabName = "overview",
      icon = fa_i(name = "table"),
      startExpanded = FALSE,
      menuSubItem(
        "Main Information",
        tabName = "mainInfo",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Annual Scientific Production",
        tabName = "annualScPr",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Average Citations per Year",
        tabName = "averageCitPerYear",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Life Cycle",
        tabName = "lifeCycle",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Three-Field Plot",
        tabName = "threeFieldPlot",
        icon = icon("chevron-right", lib = "glyphicon")
      )
    ),

    # Sources - will get ID 'menu-sources' via JavaScript (hidden initially)
    menuItem(
      "Sources",
      tabName = "sources",
      icon = fa_i(name = "book"),
      startExpanded = FALSE,
      menuSubItem(
        "Most Relevant Sources",
        tabName = "relevantSources",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Most Local Cited Sources",
        tabName = "localCitedSources",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Bradford's Law",
        tabName = "bradford",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Sources' Local Impact",
        tabName = "sourceImpact",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Sources' Production over Time",
        tabName = "sourceDynamics",
        icon = icon("chevron-right", lib = "glyphicon")
      )
    ),

    # Authors - will get ID 'menu-authors' via JavaScript (hidden initially)
    menuItem(
      "Authors",
      tabName = "authors",
      icon = fa_i(name = "user"),
      startExpanded = FALSE,
      "Authors",
      menuSubItem(
        "Author Profile",
        tabName = "AuthorPage",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Most Relevant Authors",
        tabName = "mostRelAuthors",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Most Local Cited Authors",
        tabName = "mostLocalCitedAuthors",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Authors' Production over Time",
        tabName = "authorsProdOverTime",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Lotka's Law",
        tabName = "lotka",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Authors' Local Impact",
        tabName = "authorImpact",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      "Affiliations",
      menuSubItem(
        "Most Relevant Affiliations",
        tabName = "mostRelAffiliations",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Affiliations' Production over Time",
        tabName = "AffOverTime",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      "Countries",
      menuSubItem(
        "Corresponding Author's Countries",
        tabName = "correspAuthorCountry",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Countries' Scientific Production",
        tabName = "countryScientProd",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Countries' Production over Time",
        tabName = "COOverTime",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Most Cited Countries",
        tabName = "mostCitedCountries",
        icon = icon("chevron-right", lib = "glyphicon")
      )
    ),

    # Documents - will get ID 'menu-documents' via JavaScript (hidden initially)
    menuItem(
      "Documents",
      tabName = "documents",
      icon = fa_i(name = "layer-group"),
      startExpanded = FALSE,
      "Documents",
      menuSubItem(
        "Most Global Cited Documents",
        tabName = "mostGlobalCitDoc",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Most Local Cited Documents",
        tabName = "mostLocalCitDoc",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      "Cited References",
      menuSubItem(
        "Most Local Cited References",
        tabName = "mostLocalCitRef",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "References Spectroscopy",
        tabName = "ReferenceSpect",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      "Words",
      menuSubItem(
        "Most Frequent Words",
        tabName = "mostFreqWords",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "WordCloud",
        tabName = "wcloud",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "TreeMap",
        tabName = "treemap",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Words' Frequency over Time",
        tabName = "wordDynamics",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Trend Topics",
        tabName = "trendTopic",
        icon = icon("chevron-right", lib = "glyphicon")
      )
    ),

    # SYNTHESIS SECTION HEADER (always visible) ----
    tags$div(
      id = "synthesis-header",
      style = "display: flex;
          align-items: center;
          font-size: 14px;
          font-weight: 600;
          color: #FFFFFF;
          background: rgba(255,255,255,0.1);
          padding: 10px 10px;
          margin: 15px 8px 8px 8px;
          border-radius: 6px;
          border-left: 3px solid #EC407A;
          letter-spacing: 0.8px;",
      tags$span(
        style = "background: #EC407A;
            padding: 4px 8px;
            border-radius: 4px;
            margin-right: 10px;
            font-size: 12px;",
        icon("project-diagram")
      ),
      "SYNTHESIS"
    ),

    # Clustering - will get ID 'menu-clustering' via JavaScript (hidden initially)
    menuItem(
      "Clustering",
      tabName = "clustering",
      icon = fa_i(name = "spinner"),
      startExpanded = FALSE,
      menuSubItem(
        "Clustering by Coupling",
        tabName = "coupling",
        icon = icon("chevron-right", lib = "glyphicon")
      )
    ),

    # Conceptual Structure - will get ID 'menu-conceptual' via JavaScript (hidden initially)
    menuItem(
      "Conceptual Structure",
      tabName = "concepStructure",
      icon = fa_i(name = "spell-check"),
      startExpanded = FALSE,
      "Network Approach",
      menuSubItem(
        "Co-occurence Network",
        tabName = "coOccurenceNetwork",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Thematic Map",
        tabName = "thematicMap",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Thematic Evolution",
        tabName = "thematicEvolution",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      "Factorial Approach",
      menuSubItem(
        "Factorial Analysis",
        tabName = "factorialAnalysis",
        icon = icon("chevron-right", lib = "glyphicon")
      )
    ),

    # Intellectual Structure - will get ID 'menu-intellectual' via JavaScript (hidden initially)
    menuItem(
      "Intellectual Structure",
      tabName = "intStruct",
      icon = fa_i(name = "gem"),
      startExpanded = FALSE,
      menuSubItem(
        "Co-citation Network",
        tabName = "coCitationNetwork",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Historiograph",
        tabName = "historiograph",
        icon = icon("chevron-right", lib = "glyphicon")
      )
    ),

    # Social Structure - will get ID 'menu-social' via JavaScript (hidden initially)
    menuItem(
      "Social Structure",
      tabName = "socialStruct",
      icon = fa_i("users"),
      startExpanded = FALSE,
      menuSubItem(
        "Collaboration Network",
        tabName = "collabNetwork",
        icon = icon("chevron-right", lib = "glyphicon")
      ),
      menuSubItem(
        "Countries' Collaboration World Map",
        tabName = "collabWorldMap",
        icon = icon("chevron-right", lib = "glyphicon")
      )
    ),

    tags$div(style = "margin-top: 20px;"),

    # Report - will get ID 'menu-report' via JavaScript (hidden initially)
    menuItem(
      "Report",
      tabName = "report",
      icon = fa_i(name = "list-alt")
    ),

    # TALL Export - will get ID 'menu-tall' via JavaScript (hidden initially)
    menuItem(
      "TALL Export",
      tabName = "tall",
      icon = icon("text-size", lib = "glyphicon")
    ),

    # CONTENT ANALYSIS (always visible) ----
    tags$div(
      style = "display: flex;
              align-items: center;
              font-size: 14px;
              font-weight: 600;
              color: #FFFFFF;
              background: rgba(255,255,255,0.1);
              padding: 10px 10px;
              margin: 15px 8px 8px 8px;
              border-radius: 6px;
              border-left: 3px solid #AB47BC;
              letter-spacing: 0.8px;",
      tags$span(
        style = "background: #AB47BC;
                padding: 4px 8px;
                border-radius: 4px;
                margin-right: 10px;
                font-size: 12px;",
        icon("file-lines")
      ),
      "CONTENT ANALYSIS"
    ),
    menuItem(
      "Content Analysis",
      tabName = "content_analysis",
      icon = icon("quote-right")
    ),

    # SETTINGS menu item (hidden) ----
    tags$div(
      style = "display: none;",
      menuItem("Settings", tabName = "settings", icon = icon("gear"))
    )
  )
)

## Body ####

### Theme ----
data("customTheme", envir = environment())
### Body Content ----
body <- dashboardBody(
  customTheme,
  cssTags(), # function containing all the css tags
  tabItems(
    #### Homepage ----
    ##### home ----
    tabItem(
      "biblioshinyy",
      fluidPage(
        fluidRow(
          column(
            12,
            div(h1(
              "biblioshiny: the shiny app for bibliometrix",
              style = "text-align:center; font-size:50px;"
            )),
            br()
          ),
          column(
            12,
            div(
              img(src = "logoAI.jpg", height = "35%", width = "35%"),
              style = "text-align: center;"
            )
          ),
          column(
            12,
            div(h3(
              em(
                "Biblioshiny 5.0 now includes Biblio AI – a powerful AI assistant for your science mapping analyses.",
                #   em(a("bibliometrix website.",
                #     href = "https://www.bibliometrix.org", target = "_blank"
                #   )),
              ),
              style = "text-align:center; font-size:24px;"
            )),
            br(),
            hr()
          ),
          column(
            12,
            div(h6(
              "biblioshiny and bibliometrix are open-source and freely available for use, distributed under the MIT license.",
              style = "text-align:center; font-size:19px;"
            )),
            div(h6(
              "When they are used in a publication, we ask that authors to cite the following reference:",
              style = "text-align:center; font-size:19px;"
            )),
            div(h6(
              "Aria, M., & Cuccurullo, C. (2017).",
              strong(" bibliometrix: An R-tool for comprehensive"),
              style = "text-align:center; font-size:22px;"
            )),
            div(h6(
              strong("science mapping analysis."),
              em("Journal of Informetrics"),
              ", 11(4), 959-975.",
              style = "text-align:center; font-size:22px;"
            )),
            br(),
            div(h6(
              "Failure to properly cite the software is considered a violation of the license.",
              style = "text-align:center; font-size:19px;"
            )),
            br(),
            div(p(
              "For an introduction and live examples, visit the ",
              em(a(
                "bibliometrix website.",
                href = "https://www.bibliometrix.org",
                target = "_blank"
              )),
              style = "text-align:center; font-size:18px;"
            )),
          )
        )
      )
    ),
    tabItem(
      "biblioAI",
      fluidPage(
        fluidRow(
          column(1),
          column(
            10,
            HTML(biblioAI)
          ),
          column(1)
        )
      )
    ),
    tabItem(
      "supFiles",
      fluidPage(
        fluidRow(
          column(1),
          column(
            10,
            HTML(info),
            div(
              img(src = "table_DBformats.jpg", height = "70%", width = "70%"),
              style = "text-align: center;"
            )
          ),
          column(1)
        )
      )
    ),
    tabItem(
      "pubs",
      fluidPage(
        fluidRow(
          column(1),
          column(
            10,
            HTML(pubs)
          ),
          column(1)
        )
      )
    ),
    #### Data ----
    ##### load Data ----
    tabItem(
      "loadData",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Import or Load",
          icon = fa_i(name = "file-import"),
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
                column(
                  9,
                  uiOutput("collection_descriptionUI"),
                  shinycssloaders::withSpinner(
                    DT::DTOutput("contents"),
                    caption = HTML(
                      "<br><strong>Converting data to Bibliometrix format</strong>"
                    )
                  )
                ),
                # Improved layout for "Import or Load" parameter options column
                column(
                  3,
                  fluidRow(
                    box(
                      width = "100%",
                      status = "primary",
                      solidHeader = TRUE,

                      # ============================================
                      # SECTION 1: IMPORT OR LOAD OPTIONS
                      # ============================================
                      div(
                        style = "margin-bottom: 20px;",
                        h3(
                          icon("file-import", style = "margin-right: 8px;"),
                          strong("Import or Load"),
                          style = "color: #3c8dbc; margin-bottom: 15px;"
                        ),
                        selectInput(
                          "load",
                          label = tags$strong("Please, choose what to do"),
                          choices = c(
                            " " = "null",
                            "Import raw file(s)" = "import",
                            "Load bibliometrix file(s)" = "load",
                            "Use a sample collection" = "demo"
                          ),
                          selected = "null",
                          width = "100%"
                        )
                      ),

                      # ============================================
                      # SECTION 2: DEMO COLLECTION INFO
                      # ============================================
                      conditionalPanel(
                        condition = "input.load == 'demo'",
                        div(
                          style = "background-color: #f0f8ff; padding: 15px; border-radius: 8px; border-left: 4px solid #3c8dbc; margin-bottom: 20px;",
                          h4(
                            strong(
                              "The use of bibliometric approaches in business and management disciplines."
                            ),
                            style = "color: #2c3e50; margin-top: 0;"
                          ),
                          h5(
                            strong("Dataset 'Management'"),
                            style = "color: #34495e; margin-top: 10px;"
                          ),
                          div(
                            style = "color: #555; font-size: 14px; line-height: 1.6;",
                            em(
                              "A collection of scientific articles about the use of bibliometric approaches ",
                              "in business and management disciplines."
                            ),
                            br(),
                            br(),
                            em("Period: 1985 - 2020, Source WoS.")
                          )
                        )
                      ),

                      # ============================================
                      # SECTION 3: DATABASE AND FORMAT OPTIONS
                      # ============================================
                      conditionalPanel(
                        condition = "input.load == 'import'",
                        div(
                          style = "margin-bottom: 15px;",
                          tags$label(
                            "Database",
                            style = "font-weight: 600; color: #3c8dbc; margin-bottom: 5px; display: block;"
                          ),
                          selectInput(
                            "dbsource",
                            label = NULL,
                            choices = c(
                              "Web of Science (WoS/WoK)" = "isi",
                              "Scopus" = "scopus",
                              "Dimensions" = "dimensions",
                              "Openalex" = "openalex",
                              "OpenAlex API (via openalexR)" = "openalex_api",
                              "Lens.org" = "lens",
                              "PubMed" = "pubmed",
                              "Cochrane Library" = "cochrane"
                            ),
                            selected = "isi",
                            width = "100%"
                          )
                        ),
                        div(
                          style = "margin-bottom: 20px;",
                          tags$label(
                            "Author Name Format",
                            style = "font-weight: 600; color: #3c8dbc; margin-bottom: 5px; display: block;"
                          ),
                          selectInput(
                            "authorName",
                            label = NULL,
                            choices = c(
                              "Fullname (if available)" = "AF",
                              "Surname and Initials" = "AU"
                            ),
                            selected = "AU",
                            width = "100%"
                          )
                        )
                      ),

                      # ============================================
                      # SECTION 4: FILE UPLOAD
                      # ============================================
                      conditionalPanel(
                        condition = "input.load != 'null' & input.load != 'demo'",
                        div(
                          style = "margin-bottom: 20px;",
                          conditionalPanel(
                            condition = "input.load == 'load'",
                            div(
                              style = "background-color: #ffffcc; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
                              icon(
                                "info-circle",
                                style = "margin-right: 5px; color: #856404;"
                              ),
                              tags$span(
                                em(
                                  "Load a collection in XLSX or R format previously exported from bibliometrix"
                                ),
                                style = "color: #856404; font-size: 13px;"
                              )
                            )
                          ),
                          fileInput(
                            "file1",
                            tags$strong("Choose a file"),
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
                            ),
                            width = "100%"
                          )
                        )
                      ),

                      # ============================================
                      # SECTION 5: START BUTTON
                      # ============================================
                      conditionalPanel(
                        condition = "input.load != 'null'",
                        div(
                          style = "margin: 20px 0;text-align: center;",
                          actionBttn(
                            inputId = "applyLoad",
                            label = strong("Start"),
                            width = "100%",
                            style = "pill",
                            color = "primary",
                            size = "lg",
                            icon = icon(name = "play", lib = "glyphicon")
                          )
                        )
                      ),

                      # ============================================
                      # SECTION 6: LOG OUTPUT
                      # ============================================
                      div(
                        style = "margin: 20px 0;",
                        tags$hr(style = "border-color: #ddd;"),
                        uiOutput("textLog2"),
                        tags$hr(style = "border-color: #ddd;")
                      ),

                      # ============================================
                      # SECTION 7: EXPORT OPTIONS
                      # ============================================
                      div(
                        style = "margin-top: 20px;",
                        h3(
                          icon("download", style = "margin-right: 8px;"),
                          strong("Export Collection"),
                          style = "color: #3c8dbc; margin-bottom: 15px;"
                        ),
                        selectInput(
                          "save_file",
                          label = tags$strong("Save as:"),
                          choices = c(
                            " " = "null",
                            "Excel" = "xlsx",
                            "R Data Format" = "RData"
                          ),
                          selected = "null",
                          width = "100%"
                        ),
                        conditionalPanel(
                          condition = "input.save_file != 'null'",
                          div(
                            style = "margin-top: 15px;text-align: center;",
                            downloadBttn(
                              outputId = "collection.save",
                              label = strong("Export"),
                              style = "pill",
                              color = "primary",
                              size = "lg"
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        tabPanel(
          "Info & References",
          icon = fa_i(name = "info-circle"),
          br(),
          fluidRow(
            column(1),
            column(
              10,
              HTML(helpContent()$importOrLoad)
            ),
            column(1)
          )
        )
      )
    ),
    #### OpenAlex API via openalexR ----
    tabItem(
      "openalexMenu",
      tabPanel("OpenAlex Collection", value = "openalex", openAlexUI())
    ),

    #### Pubmed API via pubmedR ----
    tabItem(
      "pubmedMenu",
      tabPanel("Pubmed Collection", value = "pubmed", pubmedUI())
    ),

    # ##### gather Data ----
    # tabItem(
    #   "gathData",
    #   tabsetPanel(
    #     type = "tabs",
    #     tabPanel(
    #       "APIs",
    #       icon = fa_i(name = "cloud-arrow-down"),
    #       fluidPage(
    #         fluidRow(
    #           tags$head(tags$style(
    #             HTML(
    #               "table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
    #                               background-color: #9c4242 !important;
    #                               }
    #                               "
    #             )
    #           )),
    #           tags$style(
    #             HTML(
    #               ".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
    #               color: #000000 !important;
    #               }"
    #             )
    #           ),
    #           column(
    #             9,
    #             shinycssloaders::withSpinner(DT::DTOutput("apiContents"))
    #           ),
    #           column(
    #             3,
    #             box(
    #               width = "100%",
    #               h3(strong(
    #                 "Gather data using APIs "
    #               )),
    #               br(),
    #               selectInput(
    #                 "dbapi",
    #                 label = "Database",
    #                 choices = c(
    #                   "DS Dimensions" = "ds",
    #                   "PubMed" = "pubmed"
    #                 ),
    #                 selected = "pubmed"
    #               ),
    #               ## Dimenions API
    #               conditionalPanel(
    #                 condition = "input.dbapi == 'ds'",
    #                 br(),
    #                 fluidRow(column(
    #                   12,
    #                   div(
    #                     style = "border-radius: 10px; border-width: 3px; font-size: 10px;",
    #                     align = "center",
    #                     width = "100%",
    #                     actionBttn(
    #                       inputId = "dsShow",
    #                       label = "1. Configure API",
    #                       style = "pill",
    #                       color = "primary",
    #                       icon = icon(name = "sliders")
    #                     )
    #                   )
    #                 )),
    #                 # h5(tags$b("Your Query")),
    #                 verbatimTextOutput("queryLog2", placeholder = FALSE),
    #                 h5(tags$b("Documents returned using your query")),
    #                 verbatimTextOutput("sampleLog2", placeholder = FALSE)
    #               ),
    #               ### Pubmed API
    #               conditionalPanel(
    #                 condition = "input.dbapi == 'pubmed'",
    #                 br(),
    #                 fluidRow(column(
    #                   12,
    #                   div(
    #                     style = "border-radius: 10px; border-width: 3px; font-size: 10px;",
    #                     align = "center",
    #                     width = "100%",
    #                     actionBttn(
    #                       inputId = "pmShow",
    #                       label = "1. Configure API",
    #                       style = "pill",
    #                       color = "primary",
    #                       icon = icon(name = "sliders")
    #                     )
    #                   )
    #                 )),
    #                 # h5(tags$b("Your Query")),
    #                 verbatimTextOutput("pmQueryLog2", placeholder = FALSE),
    #                 h5(tags$b("Documents returned using your query")),
    #                 verbatimTextOutput("pmSampleLog2", placeholder = FALSE),
    #               ),
    #               tags$hr(),
    #               fluidRow(column(
    #                 12,
    #                 div(
    #                   style = "border-radius: 10px; border-width: 3px; font-size: 10px;",
    #                   align = "center",
    #                   width = "100%",
    #                   actionBttn(
    #                     inputId = "apiApply",
    #                     label = "2. Download",
    #                     style = "pill",
    #                     color = "primary",
    #                     icon(name = "download")
    #                   )
    #                 )
    #               )),
    #               tags$hr(),
    #               h3(strong("Export a bibliometrix file ")),
    #               br(),
    #               selectInput(
    #                 "save_file_api",
    #                 "Save as:",
    #                 choices = c(
    #                   " " = "null",
    #                   "Excel" = "xlsx",
    #                   "R Data Format" = "RData"
    #                 ),
    #                 selected = "null"
    #               ),
    #               conditionalPanel(
    #                 condition = "input.save_file_api != 'null'",
    #                 fluidRow(column(
    #                   12,
    #                   div(
    #                     style = "border-radius: 10px; border-width: 3px; font-size: 15px;",
    #                     align = "center",
    #                     width = "100%",
    #                     downloadBttn(
    #                       outputId = "collection.save_api",
    #                       label = strong("Export"),
    #                       style = "pill",
    #                       color = "primary"
    #                     )
    #                   )
    #                 ))
    #               )
    #             )
    #           )
    #         )
    #       )
    #     ),
    #     tabPanel(
    #       "Info & References",
    #       icon = fa_i(name = "info-circle"),
    #       br(),
    #       fluidRow(
    #         column(1),
    #         column(
    #           10,
    #           HTML(helpContent()$api)
    #         ),
    #         column(1)
    #       )
    #     )
    #   )
    # ),
    ##### merge Data ----
    tabItem(
      "mergeData",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Merge Collections",
          icon = fa_i(name = "object-group"),
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
                column(
                  9,
                  uiOutput("collection_description_mergeUI"),
                  shinycssloaders::withSpinner(DT::DTOutput("contentsMerge"))
                ),
                column(
                  3,
                  fluidRow(
                    box(
                      width = "100%",
                      h3(strong("Load Collections")),
                      helpText(em(
                        "Merge collections in Excel or R format coming from different DBs"
                      )),
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
                    fluidRow(column(
                      12,
                      div(
                        style = "border-radius: 10px; border-width: 3px; font-size: 15px;",
                        align = "center",
                        width = "100%",
                        actionBttn(
                          inputId = "applyMerge",
                          label = strong("Start"),
                          width = "100%",
                          style = "pill",
                          color = "primary",
                          icon = icon(name = "play", lib = "glyphicon")
                        )
                      )
                    ))
                  ),
                  tags$hr(),
                  h3(strong(
                    "Export collection"
                  )),
                  selectInput(
                    "save_fileMerge",
                    "Save as:",
                    choices = c(
                      " " = "null",
                      "Excel" = "xlsx",
                      "R Data Format" = "RData"
                    ),
                    selected = "null"
                  ),
                  conditionalPanel(
                    condition = "input.save_fileMerge != 'null'",
                    fluidRow(column(
                      12,
                      div(
                        style = "border-radius: 10px; border-width: 3px; font-size: 15px;",
                        align = "center",
                        width = "100%",
                        downloadBttn(
                          outputId = "collection.saveMerge",
                          label = strong("Export"),
                          # width = "100%",
                          style = "pill",
                          color = "primary"
                        )
                      )
                    ))
                  )
                )
              )
            )
          )
        ),
        tabPanel(
          "Info & References",
          icon = fa_i(name = "info-circle"),
          br(),
          fluidRow(
            column(1),
            column(
              10,
              HTML(helpContent()$mergeCollections)
            ),
            column(1)
          )
        )
      )
    ),

    #### Reference matching ----
    ## ============================================================================
    ## UI - tabItem per Reference Matching
    ## ============================================================================

    tabItem(
      tabName = "refMatching",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Reference Matching",
          icon = icon("link"),
          br(),
          fluidRow(
            column(
              12,
              h3(strong("Reference Matching")),
              helpText(
                "This tool helps identify and merge duplicate citations in your bibliographic dataset. ",
                "It uses string similarity algorithms to find variants of the same reference, allowing you to clean and standardize your data for more accurate analysis."
              ),
              hr()
            )
          ),
          fluidRow(
            # LEFT COLUMN - Results
            column(
              width = 9,

              # Summary Statistics Box
              box(
                title = "Matching Statistics",
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,

                fluidRow(
                  column(4, valueBoxOutput("refMatch_original", width = NULL)),
                  column(
                    4,
                    valueBoxOutput("refMatch_normalized", width = NULL)
                  ),
                  column(4, valueBoxOutput("refMatch_duplicates", width = NULL))
                ),

                hr(),

                fluidRow(
                  column(
                    6,
                    plotOutput("refMatch_clusterSizePlot", height = "250px")
                  ),
                  column(
                    6,
                    plotOutput("refMatch_variantsPlot", height = "250px")
                  )
                )
              ),

              # Manual Merge Controls - Above the table
              box(
                title = "Manual Merge",
                width = NULL,
                status = "info",
                solidHeader = TRUE,

                fluidRow(
                  # Buttons column
                  column(
                    width = 7,
                    tags$div(
                      style = "display: flex; gap: 10px; align-items: center;",

                      actionButton(
                        "refMatch_toggleSelection",
                        "Toggle Selection",
                        icon = icon("check-square"),
                        class = "btn-info",
                        style = "flex: 1;"
                      ),

                      actionButton(
                        "refMatch_clearSelection",
                        "Clear Selection",
                        icon = icon("times"),
                        class = "btn-default",
                        style = "flex: 1;"
                      ),

                      actionButton(
                        "refMatch_confirmMerge",
                        "Confirm Merge",
                        icon = icon("compress"),
                        class = "btn-primary",
                        style = "flex: 1;"
                      )
                    )
                  ),

                  # Status column
                  column(
                    width = 5,
                    tags$div(
                      id = "refMatch_selectionStatusInline",
                      style = "padding: 8px 15px; background-color: #f4f4f4; border-radius: 4px; min-height: 42px; display: flex; align-items: center;",
                      uiOutput("refMatch_selectionStatusInline")
                    )
                  )
                ),

                # Help text below buttons
                tags$div(
                  style = "margin-top: 10px; padding-top: 10px; border-top: 1px solid #ddd;",
                  helpText(
                    icon("info-circle"),
                    tags$small(
                      "Click on a row in the table below, then click 'Toggle Selection' to mark it for merging. ",
                      "Repeat for all citations to merge, then click 'Confirm Merge'."
                    )
                  )
                )
              ),

              # Top Citations Box
              box(
                title = "Top Cited References (After Normalization)",
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,

                helpText(
                  icon("mouse-pointer"),
                  "Click on a row to view its citation variants below."
                ),

                DTOutput("refMatch_topCitations")
              ),

              # Variants Example Box
              box(
                title = "Citation Variants Examples",
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,

                p(
                  "The table below shows all variants of the selected citation that were matched together."
                ),
                DTOutput("refMatch_variantsTable")
              )
            ),

            # RIGHT COLUMN - Options and Actions
            column(
              width = 3,

              # Matching Options Box - with ID for collapse control
              box(
                id = "refMatch_optionsBox",
                title = "Matching Options",
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,

                helpText(
                  "Configure the citation matching algorithm parameters."
                ),

                sliderInput(
                  "refMatch_threshold",
                  "Similarity Threshold:",
                  min = 0.70,
                  max = 0.98,
                  value = 0.85,
                  step = 0.01
                ),

                helpText(
                  tags$small(
                    tags$b("Guidelines:"),
                    br(),
                    "• 0.90-0.95: Conservative (fewer false positives)",
                    br(),
                    "• 0.85-0.90: Balanced (recommended)",
                    br(),
                    "• 0.75-0.80: Aggressive (more matching)"
                  )
                ),

                hr(),

                selectInput(
                  "refMatch_method",
                  "Distance Method:",
                  choices = c(
                    "Jaro-Winkler (recommended)" = "jw",
                    "Levenshtein" = "lv",
                    "Optimal String Alignment" = "osa",
                    "Longest Common Substring" = "lcs"
                  ),
                  selected = "jw"
                ),

                hr(),

                actionButton(
                  "refMatch_run",
                  "Run Matching",
                  icon = icon("play"),
                  class = "btn-primary btn-lg btn-block",
                  style = "margin-bottom: 10px;"
                ),

                uiOutput("refMatch_runStatus")
              ),

              # Loading Indicator Box
              shinyjs::hidden(
                div(
                  id = "refMatch_loadingIndicator",
                  box(
                    title = NULL,
                    width = NULL,
                    status = "info",
                    solidHeader = FALSE,

                    tags$div(
                      style = "text-align: center; padding: 20px;",

                      tags$div(
                        icon("sync", class = "fa-spin fa-3x"),
                        style = "color: #3c8dbc; margin-bottom: 15px;"
                      ),

                      tags$h4(
                        "Matching in progress...",
                        style = "color: #3c8dbc; margin: 0;"
                      ),

                      tags$p(
                        "Please wait while citations are being normalized.",
                        style = "color: #666; font-size: 13px; margin-top: 10px;"
                      )
                    )
                  )
                )
              ),

              # Apply/Reset Box
              box(
                title = "Apply to Data",
                width = NULL,
                status = "warning",
                solidHeader = TRUE,

                helpText(
                  icon("exclamation-triangle"),
                  "Apply the normalized citations to your bibliometric data. ",
                  "This will update the CR field in your dataset."
                ),

                actionButton(
                  "refMatch_apply",
                  "Apply Normalized Citations",
                  icon = icon("check"),
                  class = "btn-warning btn-block",
                  style = "margin-bottom: 10px;"
                ),

                uiOutput("refMatch_applyStatus"),

                hr(),

                actionButton(
                  "refMatch_reset",
                  "Reset to Original Data",
                  icon = icon("undo"),
                  class = "btn-danger btn-block"
                ),

                helpText(
                  tags$small(
                    icon("info-circle"),
                    "Reset will restore the original CR field from your initial dataset."
                  )
                )
              ),

              # Export Options Box
              box(
                title = "Export Results",
                width = NULL,
                status = "success",
                solidHeader = TRUE,

                helpText(
                  "Save the bibliographic collection with normalized citations."
                ),

                radioButtons(
                  "refMatch_exportFormat",
                  "Export Format:",
                  choices = c(
                    "Excel (.xlsx)" = "xlsx",
                    "R Data (.RData)" = "rdata",
                    "Both formats" = "both"
                  ),
                  selected = "xlsx"
                ),

                hr(),

                textInput(
                  "refMatch_filename",
                  "Filename (without extension):",
                  value = "M_normalized"
                ),

                hr(),

                downloadButton(
                  "refMatch_download",
                  "Download Normalized Data",
                  class = "btn-success btn-block",
                  icon = icon("download")
                ),

                br(),
                br(),

                helpText(
                  tags$small(
                    icon("info-circle"),
                    "The exported data will contain the bibliometric data with ",
                    "normalized citations in the CR field."
                  )
                )
              ),

              # Advanced Options Box
              box(
                title = "Advanced Options",
                width = NULL,
                status = "info",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,

                checkboxInput(
                  "refMatch_keepOriginal",
                  "Keep original CR field as CR_original",
                  value = TRUE
                ),

                checkboxInput(
                  "refMatch_addStats",
                  "Add matching statistics columns",
                  value = TRUE
                ),

                hr(),

                downloadButton(
                  "refMatch_downloadReport",
                  "Download Detailed Report",
                  class = "btn-info btn-block",
                  icon = icon("file-alt")
                )
              )
            )
          )
        ),
        tabPanel(
          "Info & References",
          icon = icon("info-circle"),
          fluidPage(
            fluidRow(
              column(1),
              column(
                10,
                br(),
                HTML(referenceMatching_help)
              ),
              column(1)
            )
          )
        )
      )
    ),

    #### Filters ----
    tabItem(
      "filters",
      fluidPage(
        tabsetPanel(
          id = "tabsFilters",
          type = "tabs",
          tabPanel(
            "Filter List",
            fluidRow(
              column(4, box(h6(htmlOutput("textDim")), width = "100%")),
              column(
                2,
                div(
                  style = "display: flex; align-items: center; height: 150px;",
                  align = "center",
                  width = "100%",
                  actionBttn(
                    inputId = "applyFilter",
                    label = strong("Apply"),
                    width = "100%",
                    style = "pill",
                    color = "primary",
                    icon = icon(name = "play", lib = "glyphicon")
                  )
                )
              ),
              column(
                2,
                div(
                  style = "display: flex; align-items: center; height: 150px;",
                  align = "center",
                  width = "100%",
                  actionBttn(
                    inputId = "resetFilter",
                    label = strong("Reset"),
                    width = "100%",
                    style = "pill",
                    color = "primary",
                    icon = icon(name = "repeat", lib = "glyphicon")
                  )
                )
              ),
              column(
                2,
                div(
                  style = "display: flex; align-items: center; height: 150px;",
                  align = "center",
                  width = "100%",
                  actionBttn(
                    inputId = "viewDataFilter",
                    label = strong("Data"),
                    width = "100%",
                    style = "pill",
                    color = "primary",
                    icon = icon(name = "table", lib = "font-awesome")
                  )
                )
              )
            ),
            fluidRow(
              column(
                3,
                fluidRow(
                  box(
                    title = "1. General",
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    selectizeInput(
                      "selectType",
                      "Document Type",
                      choices = NULL,
                      multiple = TRUE
                    ),
                    selectizeInput(
                      "selectLA",
                      "Language",
                      choices = NULL,
                      multiple = TRUE
                    ),
                    sliderInput(
                      "sliderPY",
                      "Publication Year",
                      min = 1900,
                      max = 2025,
                      value = c(2000, 2025),
                      sep = ""
                    ),
                    shinyWidgets::multiInput(
                      inputId = "subject_category",
                      label = "Subject Category",
                      choices = character(0),
                      selected = NULL
                    )
                  )
                )
              ),
              column(
                3,
                fluidRow(
                  box(
                    title = "2. (J) Journal",
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    fileInput(
                      "journal_list_upload",
                      "Upload a List of Journals",
                      accept = c(".txt", ".csv", ".xlsx")
                    ),
                    uiOutput("journal_list_ui"),
                    br(),
                    fileInput(
                      "journal_ranking_upload",
                      "Upload a Journal Ranking List",
                      accept = c(".csv", ".xlsx")
                    ),
                    uiOutput("journal_ranking_ui"),
                    uiOutput("journal_ranking_ui_view"),
                    br(),
                    selectInput(
                      "bradfordSources",
                      "Source by Bradford Law Zones",
                      choices = c(
                        "Core Sources" = "core",
                        "Core + Zone 2 Sources" = "zone2",
                        "All Sources" = "all"
                      ),
                      selected = "all"
                    )
                  )
                )
              ),
              column(
                3,
                fluidRow(
                  box(
                    title = "3. (AU) Author's Country",
                    width = 12,
                    solidHeader = TRUE,
                    status = "warning",
                    selectInput(
                      "region",
                      "Region",
                      choices = c(
                        "Africa" = "AFRICA",
                        "Asia" = "ASIA",
                        "Europe" = "EUROPE",
                        "North America" = "NORTH AMERICA",
                        "South America" = "SOUTH AMERICA",
                        "Seven Seas" = "SEVEN SEAS (OPEN OCEAN)",
                        "Oceania" = "OCEANIA",
                        "Unknown" = "Unknown"
                      ),
                      selected = c(
                        "AFRICA",
                        "ASIA",
                        "EUROPE",
                        "NORTH AMERICA",
                        "SOUTH AMERICA",
                        "SEVEN SEAS (OPEN OCEAN)",
                        "OCEANIA",
                        "Unknown"
                      ),
                      multiple = TRUE
                    ),
                    # selectizeInput("country", "Country", choices = NULL, multiple = TRUE)
                    shinyWidgets::multiInput(
                      inputId = "country",
                      label = "Country",
                      choices = character(0),
                      selected = NULL
                    )
                  )
                )
              ),
              column(
                3,
                fluidRow(
                  box(
                    title = "4. (DOC) Documents",
                    width = 12,
                    solidHeader = TRUE,
                    status = "danger",
                    sliderInput(
                      "sliderTC",
                      "Total Citations",
                      min = 0,
                      max = 500,
                      value = c(0, 500)
                    ),
                    sliderInput(
                      "sliderTCpY",
                      "Total Citations per Year",
                      min = 0,
                      max = 100,
                      value = c(0, 100)
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            "Info & References",
            fluidPage(
              fluidRow(
                column(1),
                column(
                  10,
                  br(),
                  HTML(filters)
                ),
                column(1)
              )
            )
          )
        )
      )
    ),
    #### Overview ----
    ##### main information ----
    tabItem(
      "mainInfo",
      fluidPage(
        fluidRow(
          column(
            11,
            h3(strong("Main Information"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportMI"
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          #div(id = "valuebox-container",
          tabsetPanel(
            type = "tabs",
            id = "maininfo",
            tabPanel(
              "Plot",
              fluidRow(
                br(),
                column(
                  3,
                  valueBoxOutput("Timespan", width = "33vh"),
                  valueBoxOutput("au", width = "33vh"),
                  valueBoxOutput("kw", width = "33vh")
                ),
                column(
                  3,
                  valueBoxOutput("so", width = "33vh"),
                  valueBoxOutput("auS1", width = "33vh"),
                  valueBoxOutput("cr", width = "33vh")
                ),
                column(
                  3,
                  valueBoxOutput("doc", width = "33vh"),
                  valueBoxOutput("col", width = "33vh"),
                  valueBoxOutput("agePerDoc", width = "33vh")
                ),
                column(
                  3,
                  valueBoxOutput("cagr", width = "33vh"),
                  valueBoxOutput("coAuPerDoc", width = "33vh"),
                  valueBoxOutput("tc", width = "33vh")
                ),
              )
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(
                outputId = "MainInfo",
                width = 700
              )),
              align = "center"
            ),
            tabPanel(
              title = tagList(
                icon("microchip"),
                tags$span(strong("Biblio AI"), style = "margin-left: 5px;")
              ),
              fluidPage(
                fluidRow(
                  column(
                    12,
                    br(),
                    shinycssloaders::withSpinner(
                      htmlOutput("MainInfoGeminiUI"),
                      caption = HTML("<br><strong>Thinking...</strong>"),
                      image = "ai_small2.gif",
                      color = "#466fc4"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "Info & References",
              icon = icon("info-circle"),
              fluidPage(
                fluidRow(
                  column(1),
                  column(
                    10,
                    br(),
                    HTML(helpContent()$mainInformation)
                  ),
                  column(1)
                )
              )
            )
          )
        )
      )
    ),
    ##### annual scientific production ----
    tabItem(
      "annualScPr",
      fluidPage(
        fluidRow(
          column(
            10,
            h3(strong("Annual Scientific Production"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportASP"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "ASPplot.save"
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          tabsetPanel(
            id = "tabsASP",
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "AnnualProdPlot",
                height = "75vh"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput("AnnualProdTable"))
            )
          )
        )
      )
    ),

    ##### average citation per year ----
    tabItem(
      "averageCitPerYear",
      fluidPage(
        fluidRow(
          column(
            10,
            h3(strong("Average Citations Per Year"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportACpY"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "ACpYplot.save"
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "AnnualTotCitperYearPlot",
                height = "75vh"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(
                "AnnualTotCitperYearTable"
              ))
            )
          )
        )
      )
    ),

    ##### Life Cycle ----

    tabItem(
      "lifeCycle",
      fluidPage(
        fluidRow(
          column(
            9,
            h3(strong("Life Cycle of Scientific Production"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyDLC"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportDLC"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "DLCplot.save"
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          tabsetPanel(
            id = "tabsDLC",
            type = "tabs",
            # === TAB 1: SUMMARY ===
            tabPanel(
              title = tagList(shiny::icon("table"), " Summary"),
              value = "summary",
              br(),
              uiOutput("lifeCycleSummaryUIid"),
            ),

            # === TAB 2: INTERACTIVE PLOTS ===
            tabPanel(
              "Plot",
              fluidRow(
                column(
                  6,
                  shinycssloaders::withSpinner(plotlyOutput(
                    outputId = "DLCPlotYear",
                    height = "75vh"
                  ))
                ),
                column(
                  6,
                  shinycssloaders::withSpinner(plotlyOutput(
                    outputId = "DLCPlotCum",
                    height = "75vh"
                  ))
                )
              )
            ),
            tabPanel(
              title = tagList(
                icon("microchip"),
                tags$span(strong("Biblio AI"), style = "margin-left: 5px;")
              ),
              fluidPage(
                fluidRow(
                  column(
                    12,
                    br(),
                    shinycssloaders::withSpinner(
                      htmlOutput("DLCGeminiUI"),
                      caption = HTML("<br><strong>Thinking...</strong>"),
                      image = "ai_small2.gif",
                      color = "#466fc4"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "Info & References",
              icon = icon("info-circle"),
              fluidPage(
                fluidRow(
                  column(1),
                  column(
                    10,
                    br(),
                    HTML(helpContent()$lifeCycle)
                  ),
                  column(1)
                )
              )
            )
          )
        )
      )
    ),

    ##### three fields plot ----
    tabItem(
      "threeFieldPlot",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Three-Field Plot"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "apply3F"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportTFP"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  export_bttn,
                  list(
                    inputId = "screenTFP"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # ============================================
                # MAIN CONFIGURATION
                # ============================================
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),

                  # Middle Field
                  fluidRow(
                    column(
                      6,
                      div(
                        style = "padding-right: 8px;",
                        selectInput(
                          "CentralField",
                          label = strong("Middle Field"),
                          choices = c(
                            "Authors" = "AU",
                            "Affiliations" = "AU_UN",
                            "Countries" = "AU_CO",
                            "Keywords" = "DE",
                            "Keywords Plus" = "ID",
                            "All Keywords" = "KW_Merged",
                            "Titles" = "TI_TM",
                            "Abstract" = "AB_TM",
                            "Sources" = "SO",
                            "References" = "CR",
                            "Cited Sources" = "CR_SO"
                          ),
                          selected = "AU"
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        style = "padding-left: 8px;",
                        numericInput(
                          "CentralFieldn",
                          label = ("Number of Items"),
                          min = 1,
                          max = 50,
                          step = 1,
                          value = 20
                        )
                      )
                    )
                  ),

                  # Left Field
                  fluidRow(
                    column(
                      6,
                      div(
                        style = "padding-right: 8px; margin-top: 10px;",
                        selectInput(
                          "LeftField",
                          label = strong("Left Field"),
                          choices = c(
                            "Authors" = "AU",
                            "Affiliations" = "AU_UN",
                            "Countries" = "AU_CO",
                            "Keywords" = "DE",
                            "Keywords Plus" = "ID",
                            "All Keywords" = "KW_Merged",
                            "Titles" = "TI_TM",
                            "Abstract" = "AB_TM",
                            "Sources" = "SO",
                            "References" = "CR",
                            "Cited Sources" = "CR_SO"
                          ),
                          selected = "CR"
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        style = "padding-left: 8px; margin-top: 10px;",
                        numericInput(
                          "LeftFieldn",
                          label = ("Number of Items"),
                          min = 1,
                          max = 50,
                          step = 1,
                          value = 20
                        )
                      )
                    )
                  ),

                  # Right Field
                  fluidRow(
                    column(
                      6,
                      div(
                        style = "padding-right: 8px; margin-top: 10px;",
                        selectInput(
                          "RightField",
                          label = strong("Right Field"),
                          choices = c(
                            "Authors" = "AU",
                            "Affiliations" = "AU_UN",
                            "Countries" = "AU_CO",
                            "Keywords" = "DE",
                            "Keywords Plus" = "ID",
                            "All Keywords" = "KW_Merged",
                            "Titles" = "TI_TM",
                            "Abstract" = "AB_TM",
                            "Sources" = "SO",
                            "References" = "CR",
                            "Cited Sources" = "CR_SO"
                          ),
                          selected = "KW_Merged"
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        style = "padding-left: 8px; margin-top: 10px;",
                        numericInput(
                          "RightFieldn",
                          label = ("Number of Items"),
                          min = 1,
                          max = 50,
                          step = 1,
                          value = 20
                        )
                      )
                    )
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "ThreeFieldsPlot",
                height = "90vh"
              ))
            ),
            tabPanel(
              title = tagList(
                icon("microchip"),
                tags$span(strong("Biblio AI"), style = "margin-left: 5px;")
              ),
              fluidPage(
                fluidRow(
                  column(
                    12,
                    br(),
                    shinycssloaders::withSpinner(
                      htmlOutput("TFPGeminiUI"),
                      caption = HTML("<br><strong>Thinking...</strong>"),
                      image = "ai_small2.gif",
                      color = "#466fc4"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "Info & References",
              fluidPage(
                fluidRow(
                  column(1),
                  column(
                    10,
                    br(),
                    HTML(helpContent()$threeFieldPlot)
                  ),
                  column(1)
                )
              )
            )
          )
        )
      )
    ),
    #### Sources ----
    ##### relevant sources ----
    tabItem(
      "relevantSources",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Most Relevant Sources"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyMRSources"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportMRS"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "MRSplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),
                  numericInput(
                    "MostRelSourcesK",
                    label = ("Number of Sources"),
                    value = 10
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "MostRelSourcesPlot",
                height = "75vh",
                width = "98.9%"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput("MostRelSourcesTable"))
            )
          )
        )
      )
    ),
    ##### local cited sources ----
    tabItem(
      "localCitedSources",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Most Local Cited Sources"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyMLCSources"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportMLS"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "MLCSplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),
                  numericInput(
                    "MostRelCitSourcesK",
                    label = ("Number of Sources"),
                    value = 10
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "MostRelCitSourcesPlot",
                height = "75vh",
                width = "98.9%"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(
                "MostRelCitSourcesTable"
              ))
            )
          )
        )
      )
    ),
    ##### bradford law ----
    tabItem(
      "bradford",
      fluidPage(
        fluidRow(
          column(
            10,
            # titlePanel(
            h3(strong("Core Sources by Bradford's Law"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportBradford"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "BLplot.save"
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "bradfordPlot",
                height = "75vh"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput("bradfordTable"))
            )
          )
        )
      )
    ),
    ##### source local impact ----
    tabItem(
      "sourceImpact",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Sources' Local Impact"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyHsource"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportSI"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "SIplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),

                  # Impact Measure Selection
                  selectInput(
                    "HmeasureSources",
                    label = strong("Impact Measure"),
                    choices = c(
                      "H-Index" = "h",
                      "G-Index" = "g",
                      "M-Index" = "m",
                      "Total Citation" = "tc"
                    ),
                    selected = "h"
                  ),

                  # Number of Sources
                  div(
                    style = "margin-top: 10px;",
                    numericInput(
                      "Hksource",
                      label = ("Number of Sources"),
                      value = 10
                    )
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "SourceHindexPlot",
                height = "75vh",
                width = "98.9%"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(
                outputId = "SourceHindexTable"
              ))
            )
          )
        )
      )
    ),
    ##### sources prod over time ----
    tabItem(
      "sourceDynamics",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Sources' Production over Time"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applySOGrowth"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportSD"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "SDplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),

                  # Occurrences Selection
                  selectInput(
                    "cumSO",
                    label = strong("Occurrences"),
                    choices = c(
                      "Cumulate" = "Cum",
                      "Per year" = "noCum"
                    ),
                    selected = "Cum"
                  ),

                  # Number of Sources Slider
                  div(
                    style = "margin-top: 10px;",
                    sliderInput(
                      "topSO",
                      label = "Number of Sources",
                      min = 1,
                      max = 50,
                      step = 1,
                      value = c(1, 5)
                    )
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "soGrowthPlot",
                height = "90vh"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(
                outputId = "soGrowthtable"
              ))
            )
          )
        )
      )
    ),
    #### Authors ----
    ##### most relevant authors ----
    tabItem(
      "mostRelAuthors",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Most Relevant Authors"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyMRAuthors"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportMRA"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "MRAplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # ============================================
                # MAIN CONFIGURATION
                # ============================================
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),

                  # Frequency Measure Selection
                  selectInput(
                    "AuFreqMeasure",
                    label = strong("Frequency Measure"),
                    choices = c(
                      "N. of Documents" = "t",
                      "N. of Documents (%)" = "p",
                      "N. of Documents (Fractionalized)" = "f"
                    ),
                    selected = "t"
                  ),

                  # Number of Authors
                  div(
                    style = "margin-top: 10px;",
                    numericInput(
                      "MostRelAuthorsK",
                      label = "Number of Authors",
                      value = 10,
                      min = 1,
                      step = 1
                    )
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "MostRelAuthorsPlot",
                height = "75vh",
                width = "98.9%"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput("MostRelAuthorsTable"))
            )
          )
        )
      )
    ),
    ##### Author Bio Page ----
    tabItem(
      tabName = "AuthorPage",
      fluidPage(
        fluidRow(
          column(
            12,
            h3(strong("Author Profile"), align = "center")
          )
        )
      ),
      br(),

      fluidRow(
        column(
          9,
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Global Profile",
              div(
                # style = "height: 700px; overflow-y: scroll; border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9;",
                shinycssloaders::withSpinner(uiOutput("AuthorBioPageUI"))
              )
            ),
            tabPanel(
              "Local Profile",
              div(
                # style = "height: 700px; overflow-y: scroll; border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9;",
                shinycssloaders::withSpinner(uiOutput("AuthorLocalProfileUI"))
              )
            ),
            tabPanel(
              "Info & References",
              fluidPage(
                fluidRow(
                  column(1),
                  column(
                    10,
                    br(),
                    HTML(authorProfile)
                  ),
                  column(1)
                )
              )
            )
          )
        ),
        column(
          3,
          div(
            box(
              width = 12,
              # div(h3(strong(em("----"))), style = "margin-top:-57px"),
              # tags$hr(),
              style = "text-align: left; text-color: #989898",
              selectizeInput(
                inputId = "authorSearch",
                label = h4(strong("Search Auhtor")),
                choices = NULL
              ),
              fluidRow(
                column(
                  4,
                  div(
                    align = "center",
                    title = "Apply",
                    do.call(
                      "actionButton",
                      c(list(
                        label = NULL,
                        style = "display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 16px;",
                        icon = icon(name = "play", lib = "glyphicon"),
                        inputId = "authorPageApply"
                      ))
                    )
                  )
                ),
                column(
                  4,
                  div(
                    align = "center",
                    title = "Reset",
                    do.call(
                      "actionButton",
                      c(list(
                        label = NULL,
                        style = "display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 16px;",
                        icon = icon(name = "remove", lib = "glyphicon"),
                        inputId = "authorPageAReset"
                      ))
                    )
                  )
                ),
                column(4)
              ),
              br(),
              div(
                id = "authorFetchingSpinner",
                style = "display: none; text-align: center; margin-top: 15px;",
                icon(
                  "spinner",
                  class = "fa-spin fa-3x",
                  style = "color: #466fc4;"
                ),
                p(
                  "Fetching data...",
                  style = "color: #666; font-size: 14px; margin-top: 10px;"
                )
              )
            ),
            style = "margin-top:40px"
          )
        )
      )
    ),
    ##### most local cited authors ----
    tabItem(
      "mostLocalCitedAuthors",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Most Local Cited Authors"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyMLCAuthors"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportMLCA"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "MLCAplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),
                  numericInput(
                    "MostCitAuthorsK",
                    label = ("Number of Authors"),
                    value = 10
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "MostCitAuthorsPlot",
                height = "75vh",
                width = "98.9%"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput("MostCitAuthorsTable"))
            )
          )
        )
      )
    ),
    ##### authors production over time ----
    tabItem(
      "authorsProdOverTime",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Authors' Production over Time"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyAUoverTime"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportAPOT"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "APOTplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),
                  numericInput(
                    "TopAuthorsProdK",
                    label = ("Number of Authors"),
                    value = 10
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "TopAuthorsProdPlot",
                height = "75vh",
                width = "98.9%"
              ))
            ),
            tabPanel(
              "Table - Production per Year",
              shinycssloaders::withSpinner(DT::DTOutput("TopAuthorsProdTable"))
            ),
            tabPanel(
              "Table - Documents",
              shinycssloaders::withSpinner(DT::DTOutput(
                "TopAuthorsProdTablePapers"
              ))
            ),
            tabPanel(
              title = tagList(
                icon("microchip"),
                tags$span(strong("Biblio AI"), style = "margin-left: 5px;")
              ),
              fluidPage(
                fluidRow(
                  column(
                    12,
                    br(),
                    shinycssloaders::withSpinner(
                      htmlOutput("ApotGeminiUI"),
                      caption = HTML("<br><strong>Thinking...</strong>"),
                      image = "ai_small2.gif",
                      color = "#466fc4"
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    ##### lotka law ----
    tabItem(
      "lotka",
      fluidPage(
        fluidRow(
          column(
            10,
            h3(
              strong("Author Productivity through Lotka's Law"),
              align = "center"
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportLotka"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "LLplot.save"
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "lotkaPlot",
                height = "75vh"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput("lotkaTable"))
            )
          )
        )
      )
    ),
    ##### authors local impact ----
    tabItem(
      "authorImpact",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Authors' Local Impact"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyHAuthors"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportAI"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "AIplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),

                  # Impact Measure Selection
                  selectInput(
                    "HmeasureAuthors",
                    label = strong("Impact Measure"),
                    choices = c(
                      "H-Index" = "h",
                      "G-Index" = "g",
                      "M-Index" = "m",
                      "Total Citation" = "tc"
                    ),
                    selected = "h"
                  ),

                  # Number of Authors
                  div(
                    style = "margin-top: 10px;",
                    numericInput(
                      "Hkauthor",
                      label = ("Number of Authors"),
                      value = 10
                    )
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "AuthorHindexPlot",
                height = "75vh",
                width = "98.9%"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(
                outputId = "AuthorHindexTable"
              ))
            )
          )
        )
      )
    ),
    ##### most relevant affiliations ----
    tabItem(
      "mostRelAffiliations",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Most Relevant Affiliations"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyMRAffiliations"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportMRAFF"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "AFFplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),

                  # Affiliation Name Disambiguation
                  selectInput(
                    "disAff",
                    label = strong("Affiliation Name Disambiguation"),
                    choices = c(
                      "Yes" = "Y",
                      "No" = "N"
                    ),
                    selected = "Y"
                  ),

                  # Number of Affiliations
                  div(
                    style = "margin-top: 10px;",
                    numericInput(
                      "MostRelAffiliationsK",
                      label = ("Number of Affiliations"),
                      value = 10
                    )
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "MostRelAffiliationsPlot",
                height = "75vh",
                width = "98.9%"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(
                "MostRelAffiliationsTable"
              ))
            )
          )
        )
      )
    ),
    ##### Affiliation over Time ----
    tabItem(
      "AffOverTime",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Affiliations' Production over Time"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyAFFGrowth"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportAFFPOT"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "AffOverTimeplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),
                  numericInput(
                    "topAFF",
                    label = "Number of Affiliations",
                    min = 1,
                    max = 50,
                    step = 1,
                    value = 5
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "AffOverTimePlot",
                height = "90vh"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(
                outputId = "AffOverTimeTable"
              ))
            )
          )
        )
      )
    ),
    ##### corresponding author country ----
    tabItem(
      "correspAuthorCountry",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Corresponding Author's Countries"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyCAUCountries"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportMRCO"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "MRCOplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),
                  numericInput(
                    "MostRelCountriesK",
                    label = ("Number of Countries"),
                    value = 20,
                    min = 1,
                    max = 50
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "MostRelCountriesPlot",
                height = "75vh",
                width = "98.9%"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(
                "MostRelCountriesTable"
              ))
            ),
            tabPanel(
              title = tagList(
                icon("microchip"),
                tags$span(strong("Biblio AI"), style = "margin-left: 5px;")
              ),
              fluidPage(
                fluidRow(
                  column(
                    12,
                    br(),
                    shinycssloaders::withSpinner(
                      htmlOutput("MostRelCountriesGeminiUI"),
                      caption = HTML("<br><strong>Thinking...</strong>"),
                      image = "ai_small2.gif",
                      color = "#466fc4"
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    ##### country scientific production ----
    tabItem(
      "countryScientProd",
      fluidPage(
        fluidRow(
          column(
            10,
            h3(strong("Countries' Scientific Production"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportCSP"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "CSPplot.save"
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "countryProdPlot",
                height = "75vh"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput("countryProdTable"))
            )
          )
        )
      )
    ),
    ##### Country over Time ----
    tabItem(
      "COOverTime",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Countries' Production over Time"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyCOGrowth"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportCPOT"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "CountryOverTimeplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),
                  sliderInput(
                    "topCO",
                    label = "Number of Countries",
                    min = 1,
                    max = 50,
                    step = 1,
                    value = 5
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "CountryOverTimePlot",
                height = "90vh"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(
                outputId = "CountryOverTimeTable"
              ))
            )
          )
        )
      )
    ),
    ##### most cited countries ----
    tabItem(
      "mostCitedCountries",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Most Cited Countries"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyMCCountries"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportMCCO"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "MCCplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),

                  # Measure Selection
                  selectInput(
                    "CitCountriesMeasure",
                    label = strong("Measure"),
                    choices = c(
                      "Total Citations" = "TC",
                      "Average Article Citations" = "TCY"
                    ),
                    selected = "TC"
                  ),

                  # Number of Countries
                  div(
                    style = "margin-top: 10px;",
                    numericInput(
                      "MostCitCountriesK",
                      label = ("Number of Countries"),
                      value = 10
                    )
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "MostCitCountriesPlot",
                height = "75vh",
                width = "98.9%"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(
                "MostCitCountriesTable"
              ))
            )
          )
        )
      )
    ),
    #### Documents ----
    ##### most global cited documents ----
    tabItem(
      "mostGlobalCitDoc",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Most Global Cited Documents"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyMGCDocuments"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportMCD"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "MGCDplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),

                  # Number of Documents
                  numericInput(
                    "MostCitDocsK",
                    label = ("Number of Documents"),
                    value = 10
                  ),

                  # Measure Selection
                  div(
                    style = "margin-top: 10px;",
                    selectInput(
                      "CitDocsMeasure",
                      label = strong("Measure"),
                      choices = c(
                        "Total Citations" = "TC",
                        "Total Citations per Year" = "TCY"
                      ),
                      selected = "TC"
                    )
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "MostCitDocsPlot",
                height = "75vh",
                width = "98.9%"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput("MostCitDocsTable"))
            )
          )
        )
      )
    ),
    ##### most local cited documents ----
    tabItem(
      "mostLocalCitDoc",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Most Local Cited Documents"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyMLCDocuments"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportMLCD"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "MLCDplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),

                  # Number of Documents
                  numericInput(
                    "MostLocCitDocsK",
                    label = ("Number of Documents"),
                    value = 10
                  ),

                  # Field Separator
                  div(
                    style = "margin-top: 10px;",
                    selectInput(
                      inputId = "LocCitSep",
                      label = strong("Field Separator Character"),
                      choices = c(
                        ";" = ";",
                        ".  " = ".  ",
                        "," = ","
                      ),
                      selected = ";"
                    )
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "MostLocCitDocsPlot",
                height = "75vh",
                width = "98.9%"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput("MostLocCitDocsTable"))
            ),
            tabPanel(
              title = tagList(
                icon("microchip"),
                tags$span(strong("Biblio AI"), style = "margin-left: 5px;")
              ),
              fluidPage(
                fluidRow(
                  column(
                    12,
                    br(),
                    shinycssloaders::withSpinner(
                      htmlOutput("MostLocCitDocsGeminiUI"),
                      caption = HTML("<br><strong>Thinking...</strong>"),
                      image = "ai_small2.gif",
                      color = "#466fc4"
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    ##### most local cited references ----
    tabItem(
      "mostLocalCitRef",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Most Local Cited References"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyMLCReferences"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportMLCR"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "MLCRplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),
                numericInput(
                  "MostCitRefsK",
                  label = ("Number of Documents"),
                  value = 10
                ),
                selectInput(
                  inputId = "CitRefsSep",
                  label = "Field separator character",
                  choices = c(
                    ";" = ";",
                    ".  " = ".  ",
                    "," = ","
                  ),
                  selected = ";"
                ),
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "MostCitRefsPlot",
                height = "75vh",
                width = "98.9%"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput("MostCitRefsTable"))
            )
          )
        )
      )
    ),
    ##### references spectroscopy ----
    tabItem(
      "ReferenceSpect",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Reference Spectroscopy"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyRPYS"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportRPYS"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "RSplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),

                  # Median Window
                  selectInput(
                    "rpysMedianWindow",
                    label = strong("Median Window"),
                    choices = c(
                      "Centered (Marx et al., 2014)" = "centered",
                      "Backward (bibliometrix)" = "backward"
                    ),
                    selected = "backward"
                  ),

                  # Field Separator
                  div(
                    style = "margin-top: 10px;",
                    selectInput(
                      inputId = "rpysSep",
                      label = strong("Field Separator Character"),
                      choices = c(
                        ";" = ";",
                        ".  " = ".  ",
                        "," = ","
                      ),
                      selected = ";"
                    )
                  )
                ),

                # Time Slice Configuration
                div(
                  style = "background-color: #f0f4f8; padding: 12px; border-radius: 5px; margin-top: 15px;",
                  h6(
                    strong("Time Slice"),
                    style = "color: #34495e; margin-bottom: 10px;"
                  ),
                  fluidRow(
                    column(
                      6,
                      div(
                        style = "padding-right: 8px;",
                        numericInput(
                          inputId = "rpysMinYear",
                          label = "Starting Year",
                          value = NA,
                          step = 1
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        style = "padding-left: 8px;",
                        numericInput(
                          inputId = "rpysMaxYear",
                          label = "End Year",
                          value = NA,
                          step = 1
                        )
                      )
                    )
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "rpysPlot",
                height = "75vh"
              ))
            ),
            tabPanel(
              "Table - RPYS",
              shinycssloaders::withSpinner(DT::DTOutput(
                outputId = "rpysTable"
              ))
            ),
            tabPanel(
              "Table - Cited References",
              shinycssloaders::withSpinner(DT::DTOutput(
                outputId = "crTable"
              ))
            ),
            tabPanel(
              "Table - Influential References",
              fluidRow(
                column(10),
                column(
                  2,
                  selectInput(
                    "rpysInfluential",
                    label = "Type",
                    choices = c(
                      "Constant Performer" = "Constant Performer",
                      "Hot Paper" = "Hot Paper",
                      "Life Cycle" = "Life Cycle",
                      "Sleeping Beauty" = "Sleeping Beauty",
                      "Not Influent" = "Not Influent"
                    ),
                    selected = "Hot Paper"
                  )
                )
              ),
              shinycssloaders::withSpinner(DT::DTOutput(
                outputId = "rpysSequence"
              ))
            ),
            tabPanel(
              "Table - Top 10 Peaks",
              shinycssloaders::withSpinner(DT::DTOutput(
                outputId = "rpysPeaks"
              ))
            ),
            tabPanel(
              title = tagList(
                icon("microchip"),
                tags$span(strong("Biblio AI"), style = "margin-left: 5px;")
              ),
              fluidPage(
                fluidRow(
                  column(
                    12,
                    br(),
                    shinycssloaders::withSpinner(
                      htmlOutput("rpysGeminiUI"),
                      caption = HTML("<br><strong>Thinking...</strong>"),
                      image = "ai_small2.gif",
                      color = "#466fc4"
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    ##### most frequent words ----
    tabItem(
      "mostFreqWords",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Most Frequent Words"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyMFWords"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportMFW"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "MRWplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),

                  # Field Selection
                  selectInput(
                    "MostRelWords",
                    label = strong("Field"),
                    choices = c(
                      "Keywords Plus" = "ID",
                      "Author's keywords" = "DE",
                      "All Keywords" = "KW_Merged",
                      "Titles" = "TI",
                      "Abstracts" = "AB",
                      "Subject Categories (WoS)" = "WC"
                    ),
                    selected = "KW_Merged"
                  ),

                  # N-Grams (conditional)
                  conditionalPanel(
                    condition = "input.MostRelWords == 'AB' |input.MostRelWords == 'TI'",
                    div(
                      style = "margin-top: 10px;",
                      selectInput(
                        "MRWngrams",
                        label = strong("N-Grams"),
                        choices = c(
                          "Unigrams" = "1",
                          "Bigrams" = "2",
                          "Trigrams" = "3"
                        ),
                        selected = 1
                      )
                    )
                  ),

                  # Number of Words
                  div(
                    style = "margin-top: 10px;",
                    numericInput(
                      "MostRelWordsN",
                      label = "Number of Words",
                      min = 2,
                      max = 100,
                      step = 1,
                      value = 10
                    )
                  )
                ),

                # Text Editing Box (collapsed)
                box(
                  title = span(
                    icon("edit", style = "margin-right: 5px;"),
                    strong("Text Editing"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "warning",

                  selectInput(
                    "MostRelWordsStopFile",
                    "Load a list of terms to remove",
                    choices = c(
                      "Yes" = "Y",
                      "No" = "N"
                    ),
                    selected = "N"
                  ),
                  conditionalPanel(
                    condition = "input.MostRelWordsStopFile == 'Y'",
                    helpText(
                      h5(strong(
                        "Upload a TXT or CSV file containing a list of terms you want to remove from the analysis."
                      )),
                      h5(
                        ("Terms have to be separated by a standard separator (comma, semicolon or tabulator).")
                      )
                    ),
                    fileInput(
                      "MostRelWordsStop",
                      "",
                      multiple = FALSE,
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv",
                        ".txt"
                      )
                    ),
                    selectInput(
                      "MostRelWordsSep",
                      "File Separator",
                      choices = c(
                        'Comma ","' = ",",
                        'Semicolon ";"' = ";",
                        "Tab " = "\t"
                      ),
                      selected = ","
                    )
                  ),
                  selectInput(
                    "MRWSynFile",
                    "Load a list of synonyms",
                    choices = c(
                      "Yes" = "Y",
                      "No" = "N"
                    ),
                    selected = "N"
                  ),
                  conditionalPanel(
                    condition = "input.MRWSynFile == 'Y'",
                    helpText(
                      h5(strong(
                        "Upload a TXT or CSV file containing terms and their respective synonyms."
                      )),
                      h5(
                        ("Each row must contain a term and related synonyms, separated by a standard separator (comma, semicolon or tabulator).")
                      )
                    ),
                    fileInput(
                      "MRWSyn",
                      "",
                      multiple = FALSE,
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv",
                        ".txt"
                      )
                    ),
                    selectInput(
                      "MRWSynSep",
                      "File Separator",
                      choices = c(
                        'Comma ","' = ",",
                        'Semicolon ";"' = ";",
                        "Tab " = "\t"
                      ),
                      selected = ","
                    )
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "MostRelWordsPlot",
                height = "75vh",
                width = "98.9%"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput("MostRelWordsTable"))
            )
          )
        )
      )
    ),
    ##### word cloud ----
    tabItem(
      "wcloud",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("WordCloud"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyWordCloud"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportWC"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  export_bttn,
                  list(
                    inputId = "screenWC"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # ============================================
                # MAIN CONFIGURATION
                # ============================================
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),

                  # Field Selection
                  selectInput(
                    "summaryTerms",
                    label = strong("Field"),
                    choices = c(
                      "Keywords Plus" = "ID",
                      "Author's keywords" = "DE",
                      "All Keywords" = "KW_Merged",
                      "Titles" = "TI",
                      "Abstracts" = "AB",
                      "Subject Categories (WoS)" = "WC"
                    ),
                    selected = "KW_Merged"
                  ),

                  # N-Grams (conditional)
                  conditionalPanel(
                    condition = "input.summaryTerms == 'AB' |input.summaryTerms == 'TI'",
                    div(
                      style = "margin-top: 10px;",
                      selectInput(
                        "summaryTermsngrams",
                        label = strong("N-Grams"),
                        choices = c(
                          "Unigrams" = "1",
                          "Bigrams" = "2",
                          "Trigrams" = "3"
                        ),
                        selected = 1
                      )
                    )
                  ),

                  # Number of Words
                  div(
                    style = "margin-top: 10px;",
                    numericInput(
                      "n_words",
                      label = "Number of Words",
                      min = 10,
                      max = 500,
                      step = 1,
                      value = 50
                    )
                  )
                ),

                # ============================================
                # TEXT EDITING BOX
                # ============================================
                box(
                  title = span(
                    icon("edit", style = "margin-right: 5px;"),
                    strong("Text Editing"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "warning",

                  selectInput(
                    "WCStopFile",
                    "Load a list of terms to remove",
                    choices = c(
                      "Yes" = "Y",
                      "No" = "N"
                    ),
                    selected = "N"
                  ),
                  conditionalPanel(
                    condition = "input.WCStopFile == 'Y'",
                    helpText(
                      h5(strong(
                        "Upload a TXT or CSV file containing a list of terms you want to remove from the analysis."
                      )),
                      h5(
                        ("Terms have to be separated by a standard separator (comma, semicolon or tabulator).")
                      )
                    ),
                    fileInput(
                      "WCStop",
                      "",
                      multiple = FALSE,
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv",
                        ".txt"
                      )
                    ),
                    selectInput(
                      "WCSep",
                      "File Separator",
                      choices = c(
                        'Comma ","' = ",",
                        'Semicolon ";"' = ";",
                        "Tab " = "\t"
                      ),
                      selected = ","
                    )
                  ),
                  selectInput(
                    "WCSynFile",
                    "Load a list of synonyms",
                    choices = c(
                      "Yes" = "Y",
                      "No" = "N"
                    ),
                    selected = "N"
                  ),
                  conditionalPanel(
                    condition = "input.WCSynFile == 'Y'",
                    helpText(
                      h5(strong(
                        "Upload a TXT or CSV file containing, in each row, a list of synonyms, that will be merged into a single term (the first word contained in the row)"
                      )),
                      h5(
                        ("Terms have to be separated by a standard separator (comma, semicolon or tabulator). Rows have to be separated by return separator.")
                      )
                    ),
                    fileInput(
                      "WCSyn",
                      "",
                      multiple = FALSE,
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv",
                        ".txt"
                      )
                    ),
                    selectInput(
                      "WCSynSep",
                      "File Separator",
                      choices = c(
                        'Comma ","' = ",",
                        'Semicolon ";"' = ";",
                        "Tab " = "\t"
                      ),
                      selected = ","
                    )
                  )
                ),

                # ============================================
                # PARAMETERS BOX
                # ============================================
                box(
                  title = span(
                    icon("sliders", style = "margin-right: 5px;"),
                    strong("Parameters"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "info",

                  fluidRow(
                    column(
                      6,
                      div(
                        style = "padding-right: 8px;",
                        selectInput(
                          "measure",
                          "Word occurrence by",
                          choices = c(
                            "Frequency" = "freq",
                            "Square root" = "sqrt",
                            "Log" = "log",
                            "Log10" = "log10"
                          ),
                          selected = "freq"
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        style = "padding-left: 8px;",
                        selectInput(
                          "wcShape",
                          "Shape",
                          choices = c(
                            "Circle" = "circle",
                            "Cardiod" = "cardioid",
                            "Diamond" = "diamond",
                            "Pentagon" = "pentagon",
                            "Star" = "star",
                            "Triangle-forward" = "triangle-forward",
                            "Triangle" = "triangle"
                          ),
                          selected = "circle"
                        )
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      6,
                      div(
                        style = "padding-right: 8px;",
                        selectInput(
                          "font",
                          label = "Font type",
                          choices = c(
                            "Impact",
                            "Comic Sans MS (No plz!)" = "Comic Sans MS",
                            "Arial",
                            "Arial Black",
                            "Tahoma",
                            "Verdana",
                            "Courier New",
                            "Georgia",
                            "Times New Roman",
                            "Andale Mono"
                          )
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        style = "padding-left: 8px;",
                        selectInput(
                          "wcCol",
                          "Text colors",
                          choices = c(
                            "Random Dark" = "random-dark",
                            "Random Light" = "random-light"
                          ),
                          selected = "random-dark"
                        )
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      6,
                      div(
                        style = "padding-right: 8px;",
                        numericInput(
                          "scale",
                          label = "Font size",
                          min = 0.1,
                          max = 5,
                          step = 0.1,
                          value = 0.5
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        style = "padding-left: 8px;",
                        numericInput(
                          "ellipticity",
                          label = "Ellipticity",
                          min = 0,
                          max = 1,
                          step = 0.05,
                          value = 0.65
                        )
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      6,
                      div(
                        style = "padding-right: 8px;",
                        numericInput(
                          "padding",
                          label = "Padding",
                          min = 0,
                          max = 5,
                          value = 1,
                          step = 1
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        style = "padding-left: 8px;",
                        numericInput(
                          "rotate",
                          label = "Rotate",
                          min = 0,
                          max = 20,
                          value = 0,
                          step = 1
                        )
                      )
                    )
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              wordcloud2::wordcloud2Output("wordcloud", height = "75vh")
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput("wordTable"))
            )
          )
        )
      )
    ),
    ##### tree map ----
    tabItem(
      "treemap",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("TreeMap"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyTreeMap"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportTREEMAP"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  export_bttn,
                  list(
                    inputId = "screenTREEMAP"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),

                  # Field Selection
                  selectInput(
                    "treeTerms",
                    label = strong("Field"),
                    choices = c(
                      "Keywords Plus" = "ID",
                      "Author's keywords" = "DE",
                      "All Keywords" = "KW_Merged",
                      "Titles" = "TI",
                      "Abstracts" = "AB",
                      "Subject Categories (WoS)" = "WC"
                    ),
                    selected = "KW_Merged"
                  ),

                  # N-Grams (conditional)
                  conditionalPanel(
                    condition = "input.treeTerms == 'AB' |input.treeTerms == 'TI'",
                    div(
                      style = "margin-top: 10px;",
                      selectInput(
                        "treeTermsngrams",
                        label = strong("N-Grams"),
                        choices = c(
                          "Unigrams" = "1",
                          "Bigrams" = "2",
                          "Trigrams" = "3"
                        ),
                        selected = 1
                      )
                    )
                  ),

                  # Number of Words
                  div(
                    style = "margin-top: 10px;",
                    numericInput(
                      "treen_words",
                      label = "Number of Words",
                      min = 10,
                      max = 200,
                      step = 5,
                      value = 50
                    )
                  )
                ),

                # Text Editing Box (collapsed)
                box(
                  title = span(
                    icon("edit", style = "margin-right: 5px;"),
                    strong("Text Editing"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "warning",

                  selectInput(
                    "TreeMapStopFile",
                    "Load a list of terms to remove",
                    choices = c(
                      "Yes" = "Y",
                      "No" = "N"
                    ),
                    selected = "N"
                  ),
                  conditionalPanel(
                    condition = "input.TreeMapStopFile == 'Y'",
                    helpText(
                      h5(strong(
                        "Upload a TXT or CSV file containing a list of terms you want to remove from the analysis."
                      )),
                      h5(
                        ("Terms have to be separated by a standard separator (comma, semicolon or tabulator).")
                      )
                    ),
                    fileInput(
                      "TreeMapStop",
                      "",
                      multiple = FALSE,
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv",
                        ".txt"
                      )
                    ),
                    selectInput(
                      "TreeMapSep",
                      "File Separator",
                      choices = c(
                        'Comma ","' = ",",
                        'Semicolon ";"' = ";",
                        "Tab " = "\t"
                      ),
                      selected = ","
                    )
                  ),
                  selectInput(
                    "TreeMapSynFile",
                    "Load a list of synonyms",
                    choices = c(
                      "Yes" = "Y",
                      "No" = "N"
                    ),
                    selected = "N"
                  ),
                  conditionalPanel(
                    condition = "input.TreeMapSynFile == 'Y'",
                    helpText(
                      h5(strong(
                        "Upload a TXT or CSV file containing terms and their respective synonyms."
                      )),
                      h5(
                        ("Each row must contain a term and related synonyms, separated by a standard separator (comma, semicolon or tabulator).")
                      )
                    ),
                    fileInput(
                      "TreeMapSyn",
                      "",
                      multiple = FALSE,
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv",
                        ".txt"
                      )
                    ),
                    selectInput(
                      "TreeMapSynSep",
                      "File Separator",
                      choices = c(
                        'Comma ","' = ",",
                        'Semicolon ";"' = ";",
                        "Tab " = "\t"
                      ),
                      selected = ","
                    )
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "treemap",
                height = "75vh"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput("treeTable"))
            )
          )
        )
      )
    ),
    ##### word dynamics ----
    tabItem(
      "wordDynamics",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Words' Frequency over Time"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyWD"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportWD"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "WDplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),
                # ============================================
                # MAIN CONFIGURATION
                # ============================================
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),

                  # Field Selection
                  selectInput(
                    "growthTerms",
                    label = strong("Field"),
                    choices = c(
                      "Keywords Plus" = "ID",
                      "Author's keywords" = "DE",
                      "All Keywords" = "KW_Merged",
                      "Titles" = "TI",
                      "Abstracts" = "AB"
                    ),
                    selected = "KW_Merged"
                  ),

                  # N-Grams (conditional)
                  conditionalPanel(
                    condition = "input.growthTerms == 'AB' |input.growthTerms == 'TI'",
                    div(
                      style = "margin-top: 10px;",
                      selectInput(
                        "growthTermsngrams",
                        label = strong("N-Grams"),
                        choices = c(
                          "Unigrams" = "1",
                          "Bigrams" = "2",
                          "Trigrams" = "3"
                        ),
                        selected = 1
                      )
                    )
                  )
                ),

                # ============================================
                # TEXT EDITING BOX
                # ============================================
                box(
                  title = span(
                    icon("edit", style = "margin-right: 5px;"),
                    strong("Text Editing"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "warning",

                  selectInput(
                    "WDStopFile",
                    "Load a list of terms to remove",
                    choices = c(
                      "Yes" = "Y",
                      "No" = "N"
                    ),
                    selected = "N"
                  ),
                  conditionalPanel(
                    condition = "input.WDStopFile == 'Y'",
                    helpText(
                      h5(strong(
                        "Upload a TXT or CSV file containing a list of terms you want to remove from the analysis."
                      )),
                      h5(
                        ("Terms have to be separated by a standard separator (comma, semicolon or tabulator).")
                      )
                    ),
                    fileInput(
                      "WDStop",
                      "",
                      multiple = FALSE,
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv",
                        ".txt"
                      )
                    ),
                    selectInput(
                      "WDSep",
                      "File Separator",
                      choices = c(
                        'Comma ","' = ",",
                        'Semicolon ";"' = ";",
                        "Tab " = "\t"
                      ),
                      selected = ","
                    )
                  ),
                  selectInput(
                    "WDSynFile",
                    "Load a list of synonyms",
                    choices = c(
                      "Yes" = "Y",
                      "No" = "N"
                    ),
                    selected = "N"
                  ),
                  conditionalPanel(
                    condition = "input.WDSynFile == 'Y'",
                    helpText(
                      h5(strong(
                        "Upload a TXT or CSV file containing, in each row, a list of synonyms, that will be merged into a single term (the first word contained in the row)"
                      )),
                      h5(
                        ("Terms have to be separated by a standard separator (comma, semicolon or tabulator). Rows have to be separated by return separator.")
                      )
                    ),
                    fileInput(
                      "WDSyn",
                      "",
                      multiple = FALSE,
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv",
                        ".txt"
                      )
                    ),
                    selectInput(
                      "WDSynSep",
                      "File Separator",
                      choices = c(
                        'Comma ","' = ",",
                        'Semicolon ";"' = ";",
                        "Tab " = "\t"
                      ),
                      selected = ","
                    )
                  )
                ),

                # ============================================
                # PARAMETERS BOX
                # ============================================
                box(
                  title = span(
                    icon("sliders", style = "margin-right: 5px;"),
                    strong("Parameters"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "info",

                  selectInput(
                    "cumTerms",
                    label = strong("Occurrences"),
                    choices = c(
                      "Cumulate" = "Cum",
                      "Per year" = "noCum"
                    ),
                    selected = "Cum"
                  ),
                  div(
                    style = "margin-top: 10px;",
                    sliderInput(
                      "topkw",
                      label = "Number of words",
                      min = 1,
                      max = 100,
                      step = 1,
                      value = c(1, 10)
                    )
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "kwGrowthPlot",
                height = "90vh"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(
                outputId = "kwGrowthtable"
              ))
            )
          )
        )
      )
    ),
    ##### trend topic ----
    tabItem(
      "trendTopic",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Trend Topics"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyTrendTopics"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportTT"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "TTplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # ============================================
                # MAIN CONFIGURATION
                # ============================================
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),

                  # Field Selection
                  selectInput(
                    "trendTerms",
                    label = strong("Field"),
                    choices = c(
                      "Keywords Plus" = "ID",
                      "Author's keywords" = "DE",
                      "All Keywords" = "KW_Merged",
                      "Titles" = "TI",
                      "Abstracts" = "AB"
                    ),
                    selected = "KW_Merged"
                  ),

                  # N-Grams (conditional)
                  conditionalPanel(
                    condition = "input.trendTerms == 'TI' | input.trendTerms == 'AB'",
                    div(
                      style = "margin-top: 10px;",
                      selectInput(
                        "trendTermsngrams",
                        label = strong("N-Grams"),
                        choices = c(
                          "Unigrams" = "1",
                          "Bigrams" = "2",
                          "Trigrams" = "3"
                        ),
                        selected = 1
                      )
                    )
                  ),

                  # Word Stemming (conditional)
                  conditionalPanel(
                    condition = "input.trendTerms == 'TI' | input.trendTerms == 'AB'",
                    div(
                      style = "margin-top: 10px;",
                      selectInput(
                        "trendStemming",
                        label = strong("Word Stemming"),
                        choices = c(
                          "Yes" = TRUE,
                          "No" = FALSE
                        ),
                        selected = FALSE
                      )
                    )
                  ),

                  # Trend Slider (dynamic UI output)
                  div(
                    style = "margin-top: 10px;",
                    uiOutput("trendSliderPY")
                  )
                ),

                # ============================================
                # TEXT EDITING BOX
                # ============================================
                box(
                  title = span(
                    icon("edit", style = "margin-right: 5px;"),
                    strong("Text Editing"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "warning",

                  selectInput(
                    "TTStopFile",
                    "Load a list of terms to remove",
                    choices = c(
                      "Yes" = "Y",
                      "No" = "N"
                    ),
                    selected = "N"
                  ),
                  conditionalPanel(
                    condition = "input.TTStopFile == 'Y'",
                    helpText(
                      h5(strong(
                        "Upload a TXT or CSV file containing a list of terms you want to remove from the analysis."
                      )),
                      h5(
                        ("Terms have to be separated by a standard separator (comma, semicolon or tabulator).")
                      )
                    ),
                    fileInput(
                      "TTStop",
                      "",
                      multiple = FALSE,
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv",
                        ".txt"
                      )
                    ),
                    selectInput(
                      "TTSep",
                      "File Separator",
                      choices = c(
                        'Comma ","' = ",",
                        'Semicolon ";"' = ";",
                        "Tab " = "\t"
                      ),
                      selected = ","
                    )
                  ),
                  selectInput(
                    "TTSynFile",
                    "Load a list of synonyms",
                    choices = c(
                      "Yes" = "Y",
                      "No" = "N"
                    ),
                    selected = "N"
                  ),
                  conditionalPanel(
                    condition = "input.TTSynFile == 'Y'",
                    helpText(
                      h5(strong(
                        "Upload a TXT or CSV file containing, in each row, a list of synonyms, that will be merged into a single term (the first word contained in the row)"
                      )),
                      h5(
                        ("Terms have to be separated by a standard separator (comma, semicolon or tabulator). Rows have to be separated by return separator.")
                      )
                    ),
                    fileInput(
                      "TTSyn",
                      "",
                      multiple = FALSE,
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv",
                        ".txt"
                      )
                    ),
                    selectInput(
                      "TTSynSep",
                      "File Separator",
                      choices = c(
                        'Comma ","' = ",",
                        'Semicolon ";"' = ";",
                        "Tab " = "\t"
                      ),
                      selected = ","
                    )
                  )
                ),

                # ============================================
                # PARAMETERS BOX
                # ============================================
                box(
                  title = span(
                    icon("sliders", style = "margin-right: 5px;"),
                    strong("Parameters"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "info",

                  fluidRow(
                    column(
                      6,
                      div(
                        style = "padding-right: 8px;",
                        numericInput(
                          "trendMinFreq",
                          label = "Word Minimum Frequency",
                          min = 0,
                          max = 100,
                          value = 5,
                          step = 1
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        style = "padding-left: 8px;",
                        numericInput(
                          "trendNItems",
                          label = "Number of Words per Year",
                          min = 1,
                          max = 20,
                          step = 1,
                          value = 3
                        )
                      )
                    )
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "trendTopicsPlot",
                height = "90vh",
                width = "98.9%"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(
                outputId = "trendTopicsTable"
              ))
            ),
            tabPanel(
              title = tagList(
                icon("microchip"),
                tags$span(strong("Biblio AI"), style = "margin-left: 5px;")
              ),
              fluidPage(
                fluidRow(
                  column(
                    12,
                    br(),
                    shinycssloaders::withSpinner(
                      htmlOutput("trendTopicsGeminiUI"),
                      caption = HTML("<br><strong>Thinking...</strong>"),
                      image = "ai_small2.gif",
                      color = "#466fc4"
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    #### Clustering by Coupling ----
    tabItem(
      "coupling",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Clustering by Coupling"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(
                    inputId = "applyCM"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(
                    inputId = "reportCM"
                  )
                )
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(
                  export_bttn,
                  list(
                    outputId = "CMplot.save"
                  )
                )
              )
            )
          ),
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),
                selectInput(
                  "CManalysis",
                  label = "Unit of Analysis",
                  choices = c(
                    "Documents" = "documents",
                    "Authors" = "authors",
                    "Sources" = "sources"
                  ),
                  selected = "documents"
                ),
                " ",
                box(
                  title = p(
                    strong("Parameters"),
                    style = "font-size:16px;color:black;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  selectInput(
                    "CMfield",
                    label = "Coupling measured by",
                    choices = c(
                      "References" = "CR",
                      "Keywords Plus" = "ID",
                      "Author's Keywords" = "DE",
                      "Titles" = "TI",
                      "Abstracts" = "AB"
                    ),
                    selected = "CR"
                  ),
                  conditionalPanel(
                    condition = "input.CMfield == 'TI' | input.CMfield == 'AB'",
                    selectInput(
                      "CMstemming",
                      label = "Word Stemming",
                      choices = c(
                        "Yes" = TRUE,
                        "No" = FALSE
                      ),
                      selected = FALSE
                    )
                  ),
                  selectInput(
                    "CMimpact",
                    label = "Impact measure",
                    choices = c(
                      "Local Citation Score" = "local",
                      "Global Citation Score" = "global"
                    ),
                    selected = "local"
                  ),
                  selectInput(
                    "CMlabeling",
                    label = "Cluster labeling by",
                    choices = c(
                      "None" = "none",
                      "Keyword Plus" = "ID",
                      "Authors' keywords" = "DE",
                      "Title terms" = "TI",
                      "Abstract terms" = "AB"
                    ),
                    selected = "DE"
                  ),
                  conditionalPanel(
                    condition = "input.CMlabeling == 'TI' | input.CMlabeling == 'AB'",
                    selectInput(
                      "CMngrams",
                      "N-Grams",
                      choices = c(
                        "Unigrams" = "1",
                        "Bigrams" = "2",
                        "Trigrams" = "3"
                      ),
                      selected = 1
                    )
                  ),
                  fluidRow(
                    column(
                      6,
                      numericInput(
                        "CMn",
                        label = "Number of Units\n ",
                        value = 250,
                        min = 50,
                        max = 5000,
                        step = 1
                      )
                    ),
                    column(
                      6,
                      numericInput(
                        "CMfreq",
                        label = "Min Cluster Freq. ",
                        value = 5,
                        min = 1,
                        max = 100,
                        step = 1
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      6,
                      numericInput(
                        "CMn.labels",
                        label = "Labels per cluster",
                        value = 3,
                        min = 1,
                        max = 10,
                        step = 1
                      )
                    ),
                    column(
                      6,
                      numericInput(
                        "sizeCM",
                        label = "Label size",
                        value = 0.3,
                        min = 0.0,
                        max = 1,
                        step = 0.05
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      6,
                      numericInput(
                        "CMrepulsion",
                        label = "Community Repulsion",
                        value = 0.5,
                        min = 0,
                        max = 1,
                        step = 0.1
                      )
                    ),
                    column(
                      6,
                      selectInput(
                        "CMcluster",
                        label = "Clustering Algorithm",
                        choices = c(
                          "None" = "none",
                          "Edge Betweenness" = "edge_betweenness",
                          # "Fast Greedy" = "fast_greedy",
                          "InfoMap" = "infomap",
                          "Leading Eigenvalues" = "leading_eigen",
                          "Leiden" = "leiden",
                          "Louvain" = "louvain",
                          "Spinglass" = "spinglass",
                          "Walktrap" = "walktrap"
                        ),
                        selected = "louvain"
                      )
                    )
                  )
                ),
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Map",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "CMPlot",
                height = "75vh"
              ))
            ),
            tabPanel(
              "Network",
              shinycssloaders::withSpinner(visNetworkOutput(
                "CMNetPlot",
                height = "75vh"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(outputId = "CMTable"))
            ),
            tabPanel(
              "Clusters",
              shinycssloaders::withSpinner(DT::DTOutput(
                outputId = "CMTableCluster"
              ))
            )
          )
        )
      )
    ),
    #### Conceptual Structure ----
    ##### co-occurrence network ----
    tabItem(
      "coOccurenceNetwork",
      fluidPage(
        # Header Section
        fluidRow(
          column(8, h3(strong("Co-occurrence Network"), align = "center")),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call("actionBttn", c(run_bttn, list(inputId = "applyCoc")))
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call("actionBttn", c(report_bttn, list(inputId = "reportCOC")))
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call("actionBttn", c(export_bttn, list(inputId = "screenCOC")))
            )
          ),

          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),
                  selectInput(
                    "field",
                    "Field",
                    choices = c(
                      "Keywords Plus" = "ID",
                      "Author's Keywords" = "DE",
                      "All Keywords" = "KW_Merged",
                      "Titles" = "TI",
                      "Abstracts" = "AB",
                      "Subject Categories (WoS)" = "WC"
                    ),
                    selected = "KW_Merged"
                  ),
                  conditionalPanel(
                    condition = "input.field == 'TI' | input.field == 'AB'",
                    div(
                      style = "margin-top: 10px;",
                      selectInput(
                        "cocngrams",
                        "N-Grams",
                        choices = c(
                          "Unigrams" = "1",
                          "Bigrams" = "2",
                          "Trigrams" = "3"
                        ),
                        selected = 1
                      )
                    )
                  ),
                  div(
                    style = "margin-top: 10px;",
                    materialSwitch(
                      inputId = "noOverlap",
                      label = "Avoid Label Overlap",
                      value = TRUE,
                      status = "primary"
                    )
                  )
                ),

                # Text Editing Box
                box(
                  title = span(
                    icon("edit", style = "margin-right: 5px;"),
                    strong("Text Editing"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "warning",
                  div(
                    style = "background-color: #fff8e6; padding: 12px; border-radius: 5px; margin-bottom: 15px; border-left: 3px solid #f39c12;",
                    h6(
                      icon("ban", style = "margin-right: 5px;"),
                      strong("Stop Words"),
                      style = "color: #e67e22; margin-bottom: 10px;"
                    ),
                    selectInput(
                      "COCStopFile",
                      "Load a list of terms to remove",
                      choices = c("Yes" = "Y", "No" = "N"),
                      selected = "N"
                    ),
                    conditionalPanel(
                      condition = "input.COCStopFile == 'Y'",
                      div(
                        style = "margin-top: 10px; padding: 10px; background-color: #fff; border-radius: 4px;",
                        helpText(
                          h5(strong(
                            "Upload a TXT or CSV file containing a list of terms you want to remove from the analysis."
                          )),
                          h5(
                            "Terms have to be separated by a standard separator (comma, semicolon or tabulator)."
                          ),
                          style = "color: #666;"
                        ),
                        fileInput(
                          "COCStop",
                          "",
                          multiple = FALSE,
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv",
                            ".txt"
                          )
                        ),
                        selectInput(
                          "COCSep",
                          "File Separator",
                          choices = c(
                            'Comma ","' = ",",
                            'Semicolon ";"' = ";",
                            "Tab" = "\t"
                          ),
                          selected = ","
                        )
                      )
                    )
                  ),
                  div(
                    style = "background-color: #e8f5e9; padding: 12px; border-radius: 5px; border-left: 3px solid #4caf50;",
                    h6(
                      icon("exchange-alt", style = "margin-right: 5px;"),
                      strong("Synonyms"),
                      style = "color: #2e7d32; margin-bottom: 10px;"
                    ),
                    selectInput(
                      "COCSynFile",
                      "Load a list of synonyms",
                      choices = c("Yes" = "Y", "No" = "N"),
                      selected = "N"
                    ),
                    conditionalPanel(
                      condition = "input.COCSynFile == 'Y'",
                      div(
                        style = "margin-top: 10px; padding: 10px; background-color: #fff; border-radius: 4px;",
                        helpText(
                          h5(strong(
                            "Upload a TXT or CSV file containing, in each row, a list of synonyms that will be merged into a single term."
                          )),
                          h5(
                            "Terms have to be separated by a standard separator. Rows have to be separated by return separator."
                          ),
                          style = "color: #666;"
                        ),
                        fileInput(
                          "COCSyn",
                          "",
                          multiple = FALSE,
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv",
                            ".txt"
                          )
                        ),
                        selectInput(
                          "COCSynSep",
                          "File Separator",
                          choices = c(
                            'Comma ","' = ",",
                            'Semicolon ";"' = ";",
                            "Tab" = "\t"
                          ),
                          selected = ","
                        )
                      )
                    )
                  )
                ),

                # Method Parameters Box
                box(
                  title = span(
                    icon("cogs", style = "margin-right: 5px;"),
                    strong("Method Parameters"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "primary",
                  fluidRow(
                    column(
                      6,
                      div(
                        style = "padding-right: 8px;",
                        selectInput(
                          "layout",
                          label = strong("Network Layout"),
                          choices = c(
                            "Automatic layout" = "auto",
                            "Circle" = "circle",
                            "Fruchterman & Reingold" = "fruchterman",
                            "Kamada & Kawai" = "kamada",
                            "MultiDimensional Scaling" = "mds",
                            "Sphere" = "sphere",
                            "Star" = "star"
                          ),
                          selected = "auto"
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        style = "padding-left: 8px;",
                        selectInput(
                          "cocCluster",
                          label = strong("Clustering Algorithm"),
                          choices = c(
                            "None" = "none",
                            "Edge Betweenness" = "edge_betweenness",
                            "InfoMap" = "infomap",
                            "Leading Eigenvalues" = "leading_eigen",
                            "Leiden" = "leiden",
                            "Louvain" = "louvain",
                            "Spinglass" = "spinglass",
                            "Walktrap" = "walktrap"
                          ),
                          selected = "louvain"
                        )
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      6,
                      div(
                        style = "padding-right: 8px;",
                        selectInput(
                          "normalize",
                          label = strong("Normalization Method"),
                          choices = c(
                            "None" = "none",
                            "Association" = "association",
                            "Jaccard" = "jaccard",
                            "Salton" = "salton",
                            "Inclusion" = "inclusion",
                            "Equivalence" = "equivalence"
                          ),
                          selected = "association"
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        style = "padding-left: 8px;",
                        selectInput(
                          "cocyears",
                          label = strong("Node Color by Year"),
                          choices = c("No" = "No", "Yes" = "Yes"),
                          selected = "No"
                        )
                      )
                    )
                  ),
                  hr(style = "margin: 15px 0; border-top: 1px solid #ddd;"),
                  div(
                    style = "background-color: #f0f4f8; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
                    h6(
                      strong("Network Size"),
                      style = "color: #34495e; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          numericInput(
                            inputId = "Nodes",
                            label = "Number of Nodes",
                            min = 5,
                            max = 1000,
                            value = 50,
                            step = 1
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            inputId = "coc.repulsion",
                            label = "Repulsion Force",
                            min = 0,
                            max = 1,
                            value = 0.5,
                            step = 0.1
                          )
                        )
                      )
                    )
                  ),
                  div(
                    style = "background-color: #fff8e6; padding: 12px; border-radius: 5px; border-left: 3px solid #f39c12;",
                    h6(
                      icon("filter", style = "margin-right: 5px;"),
                      strong("Filtering Options"),
                      style = "color: #e67e22; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          selectInput(
                            inputId = "coc.isolates",
                            label = "Remove Isolated Nodes",
                            choices = c("Yes" = "yes", "No" = "no"),
                            selected = "yes"
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            "edges.min",
                            label = "Minimum Number of Edges",
                            value = 2,
                            step = 1,
                            min = 0
                          )
                        )
                      )
                    )
                  )
                ),

                # Graphical Parameters Box
                box(
                  title = span(
                    icon("palette", style = "margin-right: 5px;"),
                    strong("Graphical Parameters"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "info",
                  div(
                    style = "background-color: #f0f4f8; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
                    h6(
                      strong("Visual Appearance"),
                      style = "color: #34495e; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          numericInput(
                            inputId = "cocAlpha",
                            label = "Opacity",
                            min = 0,
                            max = 1,
                            value = 0.7,
                            step = 0.05
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            inputId = "Labels",
                            label = "Number of Labels",
                            min = 0,
                            max = 1000,
                            value = 1000,
                            step = 1
                          )
                        )
                      )
                    )
                  ),
                  div(
                    style = "background-color: #e8f5e9; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
                    h6(
                      strong("Label Settings"),
                      style = "color: #2e7d32; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          selectInput(
                            inputId = "label.cex",
                            label = "Label Scaling (cex)",
                            choices = c("Yes", "No"),
                            selected = "Yes"
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            inputId = "labelsize",
                            label = "Label Size",
                            min = 0.0,
                            max = 20,
                            value = 3,
                            step = 0.10
                          )
                        )
                      )
                    )
                  ),
                  div(
                    style = "background-color: #fce4ec; padding: 12px; border-radius: 5px;",
                    h6(
                      strong("Node & Edge Settings"),
                      style = "color: #c2185b; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          selectInput(
                            inputId = "coc.shape",
                            label = "Node Shape",
                            choices = c(
                              "Box" = "box",
                              "Circle" = "circle",
                              "Dot" = "dot",
                              "Ellipse" = "ellipse",
                              "Square" = "square",
                              "Text" = "text"
                            ),
                            selected = "dot"
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            inputId = "edgesize",
                            label = "Edge Size",
                            min = 0.0,
                            max = 20,
                            value = 5,
                            step = 0.5
                          )
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px; margin-top: 10px;",
                          selectInput(
                            inputId = "coc.shadow",
                            label = "Node Shadow",
                            choices = c("Yes", "No"),
                            selected = "Yes"
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px; margin-top: 10px;",
                          selectInput(
                            inputId = "coc.curved",
                            label = "Edit Nodes",
                            choices = c("Yes", "No"),
                            selected = "No"
                          )
                        )
                      )
                    )
                  )
                ),

                # Export Options
                br(),
                div(
                  style = "background-color: #e3f2fd; padding: 12px; border-radius: 8px;",
                  h6(
                    icon("download", style = "margin-right: 5px;"),
                    strong("Export Network"),
                    style = "color: #1976d2; margin-bottom: 12px;"
                  ),
                  fluidRow(
                    column(
                      6,
                      div(
                        style = "padding-right: 5px;",
                        downloadBttn(
                          outputId = "network.coc",
                          label = "Pajek",
                          style = "pill",
                          color = "primary",
                          size = "sm",
                          block = TRUE
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        style = "padding-left: 5px;",
                        downloadBttn(
                          outputId = "networkCoc.fig",
                          label = "HTML",
                          style = "pill",
                          color = "primary",
                          size = "sm",
                          block = TRUE
                        )
                      )
                    )
                  )
                ),

                right = TRUE,
                animate = TRUE,
                circle = TRUE,
                style = "gradient",
                tooltip = tooltipOptions(title = "Options"),
                color = "primary",
                icon = icon("sliders"),
                width = "320px"
              )
            ),
            style = style_opt
          )
        ),
        fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Network",
              shinycssloaders::withSpinner(visNetworkOutput(
                "cocPlot",
                height = "75vh"
              ))
            ),
            tabPanel(
              "Diachronic Network",
              br(),
              fluidRow(
                column(
                  2,
                  actionButton(
                    "start_coc",
                    "Start",
                    icon = icon("play"),
                    width = "90%"
                  )
                ),
                column(
                  2,
                  actionButton(
                    "pause_coc",
                    "Pause / Resume",
                    icon = icon("pause"),
                    width = "90%"
                  )
                ),
                column(
                  2,
                  actionButton(
                    "reset_coc",
                    "Reset",
                    icon = icon("rotate-left"),
                    width = "90%"
                  )
                ),
                column(2, uiOutput("export_cocUI")),
                column(1, div(style = "text-align:right;", "Speed (ms)")),
                column(
                  2,
                  selectInput(
                    "speed_coc",
                    label = NULL,
                    seq(250, 2000, by = 250),
                    selected = 500
                  )
                )
              ),
              fluidRow(column(12, uiOutput("year_slider_cocUI"))),
              fluidRow(
                column(1, uiOutput("cocYearUI")),
                column(11, visNetworkOutput("cocOverTime", height = "65vh"))
              )
            ),
            tabPanel(
              "Density",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "cocOverlay",
                height = "75vh"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(outputId = "cocTable"))
            ),
            tabPanel(
              "Degree Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "cocDegree",
                height = "75vh"
              ))
            ),
            tabPanel(
              title = tagList(
                icon("microchip"),
                tags$span(strong("Biblio AI"), style = "margin-left: 5px;")
              ),
              fluidPage(fluidRow(column(
                12,
                br(),
                shinycssloaders::withSpinner(
                  htmlOutput("cocGeminiUI"),
                  caption = HTML("<br><strong>Thinking...</strong>"),
                  image = "ai_small2.gif",
                  color = "#466fc4"
                )
              )))
            )
          )
        )
      )
    ),
    ##### thematic map ----
    tabItem(
      "thematicMap",
      fluidPage(
        fluidRow(
          column(8, h3(strong("Thematic Map"), align = "center")),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call("actionBttn", c(run_bttn, list(inputId = "applyTM")))
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call("actionBttn", c(report_bttn, list(inputId = "reportTM")))
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(export_bttn, list(outputId = "TMplot.save"))
              )
            )
          ),

          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),
                  selectInput(
                    "TMfield",
                    label = "Field",
                    choices = c(
                      "Keywords Plus" = "ID",
                      "Author's Keywords" = "DE",
                      "All Keywords" = "KW_Merged",
                      "Titles" = "TI",
                      "Abstracts" = "AB"
                    ),
                    selected = "KW_Merged"
                  ),
                  conditionalPanel(
                    condition = "input.TMfield == 'TI' | input.TMfield == 'AB'",
                    div(
                      style = "margin-top: 10px;",
                      selectInput(
                        "TMngrams",
                        "N-Grams",
                        choices = c(
                          "Unigrams" = "1",
                          "Bigrams" = "2",
                          "Trigrams" = "3"
                        ),
                        selected = 1
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.TMfield == 'TI' | input.TMfield == 'AB'",
                    div(
                      style = "margin-top: 10px;",
                      selectInput(
                        "TMstemming",
                        label = "Word Stemming",
                        choices = c("Yes" = TRUE, "No" = FALSE),
                        selected = FALSE
                      )
                    )
                  ),
                  div(
                    style = "margin-top: 10px;",
                    materialSwitch(
                      inputId = "noOverlapTM",
                      label = "Avoid Label Overlap",
                      value = TRUE,
                      status = "primary"
                    )
                  )
                ),

                # Text Editing Box
                box(
                  title = span(
                    icon("edit", style = "margin-right: 5px;"),
                    strong("Text Editing"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "warning",
                  div(
                    style = "background-color: #fff8e6; padding: 12px; border-radius: 5px; margin-bottom: 15px; border-left: 3px solid #f39c12;",
                    h6(
                      icon("ban", style = "margin-right: 5px;"),
                      strong("Stop Words"),
                      style = "color: #e67e22; margin-bottom: 10px;"
                    ),
                    selectInput(
                      "TMStopFile",
                      "Load a list of terms to remove",
                      choices = c("Yes" = "Y", "No" = "N"),
                      selected = "N"
                    ),
                    conditionalPanel(
                      condition = "input.TMStopFile == 'Y'",
                      div(
                        style = "margin-top: 10px; padding: 10px; background-color: #fff; border-radius: 4px;",
                        helpText(
                          h5(strong(
                            "Upload a TXT or CSV file containing a list of terms you want to remove from the analysis."
                          )),
                          h5(
                            "Terms have to be separated by a standard separator (comma, semicolon or tabulator)."
                          ),
                          style = "color: #666;"
                        ),
                        fileInput(
                          "TMStop",
                          "",
                          multiple = FALSE,
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv",
                            ".txt"
                          )
                        ),
                        selectInput(
                          "TMSep",
                          "File Separator",
                          choices = c(
                            'Comma ","' = ",",
                            'Semicolon ";"' = ";",
                            "Tab" = "\t"
                          ),
                          selected = ","
                        )
                      )
                    )
                  ),
                  div(
                    style = "background-color: #e8f5e9; padding: 12px; border-radius: 5px; border-left: 3px solid #4caf50;",
                    h6(
                      icon("exchange-alt", style = "margin-right: 5px;"),
                      strong("Synonyms"),
                      style = "color: #2e7d32; margin-bottom: 10px;"
                    ),
                    selectInput(
                      "TMapSynFile",
                      "Load a list of synonyms",
                      choices = c("Yes" = "Y", "No" = "N"),
                      selected = "N"
                    ),
                    conditionalPanel(
                      condition = "input.TMapSynFile == 'Y'",
                      div(
                        style = "margin-top: 10px; padding: 10px; background-color: #fff; border-radius: 4px;",
                        helpText(
                          h5(strong(
                            "Upload a TXT or CSV file containing, in each row, a list of synonyms that will be merged into a single term."
                          )),
                          h5(
                            "Terms have to be separated by a standard separator. Rows have to be separated by return separator."
                          ),
                          style = "color: #666;"
                        ),
                        fileInput(
                          "TMapSyn",
                          "",
                          multiple = FALSE,
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv",
                            ".txt"
                          )
                        ),
                        selectInput(
                          "TMapSynSep",
                          "File Separator",
                          choices = c(
                            'Comma ","' = ",",
                            'Semicolon ";"' = ";",
                            "Tab" = "\t"
                          ),
                          selected = ","
                        )
                      )
                    )
                  )
                ),

                # Parameters Box
                box(
                  title = span(
                    icon("cogs", style = "margin-right: 5px;"),
                    strong("Parameters"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "primary",
                  div(
                    style = "background-color: #f0f4f8; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
                    h6(
                      strong("Data Parameters"),
                      style = "color: #34495e; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          numericInput(
                            "TMn",
                            label = "Number of Words",
                            value = 250,
                            min = 50,
                            max = 5000,
                            step = 1
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            "TMfreq",
                            label = "Min Cluster Frequency (per thousand docs)",
                            value = 5,
                            min = 1,
                            max = 100,
                            step = 1
                          )
                        )
                      )
                    )
                  ),
                  div(
                    style = "background-color: #e8f5e9; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
                    h6(
                      strong("Display Parameters"),
                      style = "color: #2e7d32; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          numericInput(
                            "TMn.labels",
                            label = "Number of Labels",
                            value = 3,
                            min = 0,
                            max = 10,
                            step = 1
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            "sizeTM",
                            label = "Label Size",
                            value = 0.3,
                            min = 0.0,
                            max = 1,
                            step = 0.05
                          )
                        )
                      )
                    )
                  ),
                  div(
                    style = "background-color: #fce4ec; padding: 12px; border-radius: 5px;",
                    h6(
                      strong("Network Parameters"),
                      style = "color: #c2185b; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          numericInput(
                            "TMrepulsion",
                            label = "Community Repulsion",
                            value = 0.5,
                            min = 0,
                            max = 1,
                            step = 0.1
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          selectInput(
                            "TMCluster",
                            label = "Clustering Algorithm",
                            choices = c(
                              "None" = "none",
                              "Edge Betweenness" = "edge_betweenness",
                              "InfoMap" = "infomap",
                              "Leading Eigenvalues" = "leading_eigen",
                              "Leiden" = "leiden",
                              "Louvain" = "louvain",
                              "Spinglass" = "spinglass",
                              "Walktrap" = "walktrap"
                            ),
                            selected = "louvain"
                          )
                        )
                      )
                    )
                  )
                ),

                right = TRUE,
                animate = TRUE,
                circle = TRUE,
                style = "gradient",
                tooltip = tooltipOptions(title = "Options"),
                color = "primary",
                icon = icon("sliders"),
                width = "320px"
              )
            ),
            style = style_opt
          )
        ),
        fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Map",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "TMPlot",
                height = "75vh"
              ))
            ),
            tabPanel(
              "Network",
              shinycssloaders::withSpinner(visNetworkOutput(
                "NetPlot",
                height = "75vh"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(outputId = "TMTable"))
            ),
            tabPanel(
              "Clusters",
              shinycssloaders::withSpinner(DT::DTOutput(
                outputId = "TMTableCluster"
              ))
            ),
            tabPanel(
              "Documents",
              shinycssloaders::withSpinner(DT::DTOutput(
                outputId = "TMTableDocument"
              ))
            ),
            tabPanel(
              title = tagList(
                icon("microchip"),
                tags$span(strong("Biblio AI"), style = "margin-left: 5px;")
              ),
              fluidPage(fluidRow(column(
                12,
                br(),
                shinycssloaders::withSpinner(
                  htmlOutput("TMGeminiUI"),
                  caption = HTML("<br><strong>Thinking...</strong>"),
                  image = "ai_small2.gif",
                  color = "#466fc4"
                )
              )))
            )
          )
        )
      )
    ),
    ##### thematic evolution ----
    tabItem(
      "thematicEvolution",
      fluidPage(
        fluidRow(
          column(8, h3(strong("Thematic Evolution"), align = "center")),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call("actionBttn", c(run_bttn, list(inputId = "applyTE")))
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call("actionBttn", c(report_bttn, list(inputId = "reportTE")))
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(export_bttn, list(outputId = "TEplot.save"))
              )
            )
          ),

          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),
                  selectInput(
                    "TEfield",
                    label = "Field",
                    choices = c(
                      "Keywords Plus" = "ID",
                      "Author's Keywords" = "DE",
                      "All Keywords" = "KW_Merged",
                      "Titles" = "TI",
                      "Abstracts" = "AB"
                    ),
                    selected = "KW_Merged"
                  ),
                  conditionalPanel(
                    condition = "input.TEfield == 'TI' | input.TEfield == 'AB'",
                    div(
                      style = "margin-top: 10px;",
                      selectInput(
                        "TEngrams",
                        "N-Grams",
                        choices = c(
                          "Unigrams" = "1",
                          "Bigrams" = "2",
                          "Trigrams" = "3"
                        ),
                        selected = 1
                      )
                    )
                  ),
                  div(
                    style = "margin-top: 10px;",
                    materialSwitch(
                      inputId = "noOverlapTE",
                      label = "Avoid Label Overlap",
                      value = TRUE,
                      status = "primary"
                    )
                  )
                ),

                # Text Editing Box
                box(
                  title = span(
                    icon("edit", style = "margin-right: 5px;"),
                    strong("Text Editing"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "warning",
                  div(
                    style = "background-color: #fff8e6; padding: 12px; border-radius: 5px; margin-bottom: 15px; border-left: 3px solid #f39c12;",
                    h6(
                      icon("ban", style = "margin-right: 5px;"),
                      strong("Stop Words"),
                      style = "color: #e67e22; margin-bottom: 10px;"
                    ),
                    selectInput(
                      "TEStopFile",
                      "Load a list of terms to remove",
                      choices = c("Yes" = "Y", "No" = "N"),
                      selected = "N"
                    ),
                    conditionalPanel(
                      condition = "input.TEStopFile == 'Y'",
                      div(
                        style = "margin-top: 10px; padding: 10px; background-color: #fff; border-radius: 4px;",
                        helpText(
                          h5(strong(
                            "Upload a TXT or CSV file containing a list of terms you want to remove from the analysis."
                          )),
                          h5(
                            "Terms have to be separated by a standard separator (comma, semicolon or tabulator)."
                          ),
                          style = "color: #666;"
                        ),
                        fileInput(
                          "TEStop",
                          "",
                          multiple = FALSE,
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv",
                            ".txt"
                          )
                        ),
                        selectInput(
                          "TESep",
                          "File Separator",
                          choices = c(
                            'Comma ","' = ",",
                            'Semicolon ";"' = ";",
                            "Tab" = "\t"
                          ),
                          selected = ","
                        )
                      )
                    )
                  ),
                  div(
                    style = "background-color: #e8f5e9; padding: 12px; border-radius: 5px; border-left: 3px solid #4caf50;",
                    h6(
                      icon("exchange-alt", style = "margin-right: 5px;"),
                      strong("Synonyms"),
                      style = "color: #2e7d32; margin-bottom: 10px;"
                    ),
                    selectInput(
                      "TESynFile",
                      "Load a list of synonyms",
                      choices = c("Yes" = "Y", "No" = "N"),
                      selected = "N"
                    ),
                    conditionalPanel(
                      condition = "input.TESynFile == 'Y'",
                      div(
                        style = "margin-top: 10px; padding: 10px; background-color: #fff; border-radius: 4px;",
                        helpText(
                          h5(strong(
                            "Upload a TXT or CSV file containing, in each row, a list of synonyms that will be merged into a single term."
                          )),
                          h5(
                            "Terms have to be separated by a standard separator. Rows have to be separated by return separator."
                          ),
                          style = "color: #666;"
                        ),
                        fileInput(
                          "TESyn",
                          "",
                          multiple = FALSE,
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv",
                            ".txt"
                          )
                        ),
                        selectInput(
                          "TESynSep",
                          "File Separator",
                          choices = c(
                            'Comma ","' = ",",
                            'Semicolon ";"' = ";",
                            "Tab" = "\t"
                          ),
                          selected = ","
                        )
                      )
                    )
                  )
                ),

                # Parameters Box
                box(
                  title = span(
                    icon("cogs", style = "margin-right: 5px;"),
                    strong("Parameters"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "primary",
                  div(
                    style = "background-color: #f0f4f8; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
                    h6(
                      strong("Data Parameters"),
                      style = "color: #34495e; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          numericInput(
                            "nTE",
                            label = "Number of Words",
                            value = 250,
                            min = 50,
                            max = 5000,
                            step = 1
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            "fTE",
                            label = "Min Cluster Frequency (per thousand docs)",
                            value = 5,
                            min = 1,
                            max = 100,
                            step = 1
                          )
                        )
                      )
                    )
                  ),
                  div(
                    style = "background-color: #e8f5e9; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
                    h6(
                      strong("Weight Parameters"),
                      style = "color: #2e7d32; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          selectInput(
                            "TEmeasure",
                            label = "Weight Index",
                            choices = c(
                              "Inclusion Index" = "inclusion",
                              "Inclusion Index weighted by Word-Occurrences" = "weighted",
                              "Stability Index" = "stability"
                            ),
                            selected = "weighted"
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            "minFlowTE",
                            label = "Min Weight Index",
                            value = 0.1,
                            min = 0.02,
                            max = 1,
                            step = 0.02
                          )
                        )
                      )
                    )
                  ),
                  div(
                    style = "background-color: #fce4ec; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
                    h6(
                      strong("Display Parameters"),
                      style = "color: #c2185b; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          numericInput(
                            "sizeTE",
                            label = "Label Size",
                            value = 0.3,
                            min = 0.0,
                            max = 1,
                            step = 0.05
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            "TEn.labels",
                            label = "Number of Labels (for each cluster)",
                            value = 3,
                            min = 1,
                            max = 5,
                            step = 1
                          )
                        )
                      )
                    )
                  ),
                  fluidRow(column(
                    12,
                    selectInput(
                      "TECluster",
                      label = strong("Clustering Algorithm"),
                      choices = c(
                        "None" = "none",
                        "Edge Betweenness" = "edge_betweenness",
                        "InfoMap" = "infomap",
                        "Leading Eigenvalues" = "leading_eigen",
                        "Leiden" = "leiden",
                        "Louvain" = "louvain",
                        "Spinglass" = "spinglass",
                        "Walktrap" = "walktrap"
                      ),
                      selected = "louvain"
                    )
                  ))
                ),

                # Time Slices Box
                box(
                  title = span(
                    icon("clock", style = "margin-right: 5px;"),
                    strong("Time Slices"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = FALSE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  status = "info",
                  numericInput(
                    "numSlices",
                    label = "Number of Cutting Points",
                    min = 1,
                    max = 4,
                    value = 1
                  ),
                  "Please, write the cutting points (in year) for your collection",
                  uiOutput("sliders")
                ),

                right = TRUE,
                animate = TRUE,
                circle = TRUE,
                style = "gradient",
                tooltip = tooltipOptions(title = "Options"),
                color = "primary",
                icon = icon("sliders"),
                width = "320px"
              )
            ),
            style = style_opt
          )
        ),
        fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Thematic Evolution",
              tabsetPanel(
                type = "tabs",
                tabPanel(
                  "Map",
                  shinycssloaders::withSpinner(plotlyOutput(
                    outputId = "TEPlot",
                    height = "75vh"
                  ))
                ),
                tabPanel(
                  "Table",
                  shinycssloaders::withSpinner(DT::DTOutput(
                    outputId = "TETable"
                  ))
                )
              )
            ),
            tabPanel(
              "Time Slice 1",
              tabsetPanel(
                type = "tabs",
                tabPanel(
                  "Map",
                  shinycssloaders::withSpinner(plotlyOutput(
                    outputId = "TMPlot1",
                    height = "75vh"
                  ))
                ),
                tabPanel(
                  "Network",
                  shinycssloaders::withSpinner(visNetworkOutput(
                    "NetPlot1",
                    height = "75vh"
                  ))
                ),
                tabPanel(
                  "Table",
                  shinycssloaders::withSpinner(DT::DTOutput(
                    outputId = "TMTable1"
                  ))
                ),
                tabPanel(
                  "Clusters",
                  shinycssloaders::withSpinner(DT::DTOutput(
                    outputId = "TMTableCluster1"
                  ))
                ),
                tabPanel(
                  "Documents",
                  shinycssloaders::withSpinner(DT::DTOutput(
                    outputId = "TMTableDocument1"
                  ))
                )
              )
            ),
            tabPanel(
              "Time Slice 2",
              tabsetPanel(
                type = "tabs",
                tabPanel(
                  "Map",
                  shinycssloaders::withSpinner(plotlyOutput(
                    outputId = "TMPlot2",
                    height = "75vh"
                  ))
                ),
                tabPanel(
                  "Network",
                  shinycssloaders::withSpinner(visNetworkOutput(
                    "NetPlot2",
                    height = "75vh"
                  ))
                ),
                tabPanel(
                  "Table",
                  shinycssloaders::withSpinner(DT::DTOutput(
                    outputId = "TMTable2"
                  ))
                ),
                tabPanel(
                  "Clusters",
                  shinycssloaders::withSpinner(DT::DTOutput(
                    outputId = "TMTableCluster2"
                  ))
                ),
                tabPanel(
                  "Documents",
                  shinycssloaders::withSpinner(DT::DTOutput(
                    outputId = "TMTableDocument2"
                  ))
                )
              )
            ),
            tabPanel(
              "Time Slice 3",
              tabsetPanel(
                type = "tabs",
                tabPanel(
                  "Map",
                  shinycssloaders::withSpinner(plotlyOutput(
                    outputId = "TMPlot3",
                    height = "75vh"
                  ))
                ),
                tabPanel(
                  "Network",
                  shinycssloaders::withSpinner(visNetworkOutput(
                    "NetPlot3",
                    height = "75vh"
                  ))
                ),
                tabPanel(
                  "Table",
                  shinycssloaders::withSpinner(DT::DTOutput(
                    outputId = "TMTable3"
                  ))
                ),
                tabPanel(
                  "Clusters",
                  shinycssloaders::withSpinner(DT::DTOutput(
                    outputId = "TMTableCluster3"
                  ))
                ),
                tabPanel(
                  "Documents",
                  shinycssloaders::withSpinner(DT::DTOutput(
                    outputId = "TMTableDocument3"
                  ))
                )
              )
            ),
            tabPanel(
              "Time Slice 4",
              tabsetPanel(
                type = "tabs",
                tabPanel(
                  "Map",
                  shinycssloaders::withSpinner(plotlyOutput(
                    outputId = "TMPlot4",
                    height = "75vh"
                  ))
                ),
                tabPanel(
                  "Network",
                  shinycssloaders::withSpinner(visNetworkOutput(
                    "NetPlot4",
                    height = "75vh"
                  ))
                ),
                tabPanel(
                  "Table",
                  shinycssloaders::withSpinner(DT::DTOutput(
                    outputId = "TMTable4"
                  ))
                ),
                tabPanel(
                  "Clusters",
                  shinycssloaders::withSpinner(DT::DTOutput(
                    outputId = "TMTableCluster4"
                  ))
                ),
                tabPanel(
                  "Documents",
                  shinycssloaders::withSpinner(DT::DTOutput(
                    outputId = "TMTableDocument4"
                  ))
                )
              )
            ),
            tabPanel(
              "Time Slice 5",
              tabsetPanel(
                type = "tabs",
                tabPanel(
                  "Map",
                  shinycssloaders::withSpinner(plotlyOutput(
                    outputId = "TMPlot5",
                    height = "75vh"
                  ))
                ),
                tabPanel(
                  "Network",
                  shinycssloaders::withSpinner(visNetworkOutput(
                    "NetPlot5",
                    height = "75vh"
                  ))
                ),
                tabPanel(
                  "Table",
                  shinycssloaders::withSpinner(DT::DTOutput(
                    outputId = "TMTable5"
                  ))
                ),
                tabPanel(
                  "Clusters",
                  shinycssloaders::withSpinner(DT::DTOutput(
                    outputId = "TMTableCluster5"
                  ))
                ),
                tabPanel(
                  "Documents",
                  shinycssloaders::withSpinner(DT::DTOutput(
                    outputId = "TMTableDocument5"
                  ))
                )
              )
            ),
            tabPanel(
              title = tagList(
                icon("microchip"),
                tags$span(strong("Biblio AI"), style = "margin-left: 5px;")
              ),
              fluidPage(fluidRow(column(
                12,
                br(),
                shinycssloaders::withSpinner(
                  htmlOutput("TEGeminiUI"),
                  caption = HTML("<br><strong>Thinking...</strong>"),
                  image = "ai_small2.gif",
                  color = "#466fc4"
                )
              )))
            )
          )
        )
      )
    ),
    ##### factorial analysis ----
    tabItem(
      "factorialAnalysis",
      fluidPage(
        fluidRow(
          column(8, h3(strong("Factorial Analysis"), align = "center")),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call("actionBttn", c(run_bttn, list(inputId = "applyCA")))
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call("actionBttn", c(report_bttn, list(inputId = "reportFA")))
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(export_bttn, list(outputId = "FAplot.save"))
              )
            )
          ),

          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),
                  selectInput(
                    "method",
                    label = "Method",
                    choices = c(
                      "Correspondence Analysis" = "CA",
                      "Multiple Correspondence Analysis" = "MCA",
                      "Multidimensional Scaling" = "MDS"
                    ),
                    selected = "MCA"
                  ),
                  selectInput(
                    "CSfield",
                    label = "Field",
                    choices = c(
                      "Keywords Plus" = "ID",
                      "Author's Keywords" = "DE",
                      "All Keywords" = "KW_Merged",
                      "Titles" = "TI",
                      "Abstracts" = "AB"
                    ),
                    selected = "KW_Merged"
                  ),
                  conditionalPanel(
                    condition = "input.CSfield == 'TI' | input.CSfield == 'AB'",
                    div(
                      style = "margin-top: 10px;",
                      selectInput(
                        "CSngrams",
                        "N-Grams",
                        choices = c(
                          "Unigrams" = "1",
                          "Bigrams" = "2",
                          "Trigrams" = "3"
                        ),
                        selected = 1
                      )
                    )
                  )
                ),

                # Text Editing Box
                box(
                  title = span(
                    icon("edit", style = "margin-right: 5px;"),
                    strong("Text Editing"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "warning",
                  div(
                    style = "background-color: #fff8e6; padding: 12px; border-radius: 5px; margin-bottom: 15px; border-left: 3px solid #f39c12;",
                    h6(
                      icon("ban", style = "margin-right: 5px;"),
                      strong("Stop Words"),
                      style = "color: #e67e22; margin-bottom: 10px;"
                    ),
                    selectInput(
                      "CSStopFile",
                      "Load a list of terms to remove",
                      choices = c("Yes" = "Y", "No" = "N"),
                      selected = "N"
                    ),
                    conditionalPanel(
                      condition = "input.CSStopFile == 'Y'",
                      div(
                        style = "margin-top: 10px; padding: 10px; background-color: #fff; border-radius: 4px;",
                        helpText(
                          h5(strong(
                            "Upload a TXT or CSV file containing a list of terms you want to remove from the analysis."
                          )),
                          h5(
                            "Terms have to be separated by a standard separator (comma, semicolon or tabulator)."
                          ),
                          style = "color: #666;"
                        ),
                        fileInput(
                          "CSStop",
                          "",
                          multiple = FALSE,
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv",
                            ".txt"
                          )
                        ),
                        selectInput(
                          "CSSep",
                          "File Separator",
                          choices = c(
                            'Comma ","' = ",",
                            'Semicolon ";"' = ";",
                            "Tab" = "\t"
                          ),
                          selected = ","
                        )
                      )
                    )
                  ),
                  div(
                    style = "background-color: #e8f5e9; padding: 12px; border-radius: 5px; border-left: 3px solid #4caf50;",
                    h6(
                      icon("exchange-alt", style = "margin-right: 5px;"),
                      strong("Synonyms"),
                      style = "color: #2e7d32; margin-bottom: 10px;"
                    ),
                    selectInput(
                      "FASynFile",
                      "Load a list of synonyms",
                      choices = c("Yes" = "Y", "No" = "N"),
                      selected = "N"
                    ),
                    conditionalPanel(
                      condition = "input.FASynFile == 'Y'",
                      div(
                        style = "margin-top: 10px; padding: 10px; background-color: #fff; border-radius: 4px;",
                        helpText(
                          h5(strong(
                            "Upload a TXT or CSV file containing, in each row, a list of synonyms that will be merged into a single term."
                          )),
                          h5(
                            "Terms have to be separated by a standard separator. Rows have to be separated by return separator."
                          ),
                          style = "color: #666;"
                        ),
                        fileInput(
                          "FASyn",
                          "",
                          multiple = FALSE,
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv",
                            ".txt"
                          )
                        ),
                        selectInput(
                          "FASynSep",
                          "File Separator",
                          choices = c(
                            'Comma ","' = ",",
                            'Semicolon ";"' = ";",
                            "Tab" = "\t"
                          ),
                          selected = ","
                        )
                      )
                    )
                  )
                ),

                # Method Parameters Box
                box(
                  title = span(
                    icon("cogs", style = "margin-right: 5px;"),
                    strong("Method Parameters"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "primary",
                  div(
                    style = "background-color: #f0f4f8; padding: 12px; border-radius: 5px;",
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          numericInput(
                            "CSn",
                            label = "Number of Terms",
                            value = 50,
                            step = 1
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          selectInput(
                            "nClustersCS",
                            label = "N. of Clusters",
                            choices = c(
                              "1" = "1",
                              "2" = "2",
                              "3" = "3",
                              "4" = "4",
                              "5" = "5",
                              "6" = "6",
                              "7" = "7",
                              "8" = "8"
                            ),
                            selected = "1"
                          )
                        )
                      )
                    )
                  )
                ),

                # Graphical Parameters Box
                box(
                  title = span(
                    icon("palette", style = "margin-right: 5px;"),
                    strong("Graphical Parameters"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "info",
                  div(
                    style = "background-color: #f0f4f8; padding: 12px; border-radius: 5px;",
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          numericInput(
                            inputId = "CSlabelsize",
                            label = "Label Size",
                            min = 5,
                            max = 30,
                            value = 10
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            "CSdoc",
                            label = "Num. of Documents",
                            value = 5
                          )
                        )
                      )
                    )
                  )
                ),

                right = TRUE,
                animate = TRUE,
                circle = TRUE,
                style = "gradient",
                tooltip = tooltipOptions(title = "Options"),
                color = "primary",
                icon = icon("sliders"),
                width = "320px"
              )
            ),
            style = style_opt
          )
        ),
        fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Word Map",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "CSPlot1",
                height = "75vh",
                width = "98.9%"
              ))
            ),
            tabPanel(
              "Topic Dendrogram",
              shinycssloaders::withSpinner(visNetworkOutput(
                "CSPlot4",
                width = "auto",
                height = "75vh"
              ))
            ),
            tabPanel(
              "Words by Cluster",
              shinycssloaders::withSpinner(DT::DTOutput(outputId = "CSTableW"))
            ),
            tabPanel(
              "Articles by Cluster",
              shinycssloaders::withSpinner(DT::DTOutput(outputId = "CSTableD"))
            ),
            tabPanel(
              title = tagList(
                icon("microchip"),
                tags$span(strong("Biblio AI"), style = "margin-left: 5px;")
              ),
              fluidPage(fluidRow(column(
                12,
                br(),
                shinycssloaders::withSpinner(
                  htmlOutput("CSGeminiUI"),
                  caption = HTML("<br><strong>Thinking...</strong>"),
                  image = "ai_small2.gif",
                  color = "#466fc4"
                )
              )))
            )
          )
        )
      )
    ),
    #### Intellectual Structure ----
    ##### co-citation network ----
    tabItem(
      "coCitationNetwork",
      fluidPage(
        # Header Section
        fluidRow(
          column(8, h3(strong("Co-citation Network"), align = "center")),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call("actionBttn", c(run_bttn, list(inputId = "applyCocit")))
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(report_bttn, list(inputId = "reportCOCIT"))
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "actionBttn",
                c(export_bttn, list(inputId = "screenCOCIT"))
              )
            )
          ),

          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),
                  selectInput(
                    "citField",
                    label = "Field",
                    choices = c(
                      "Papers" = "CR",
                      "Authors" = "CR_AU",
                      "Sources" = "CR_SO"
                    ),
                    selected = "CR"
                  ),
                  selectInput(
                    inputId = "citSep",
                    label = "Field separator character",
                    choices = c(
                      '";" (Semicolon)' = ";",
                      '".   " (Dot and 3 or more spaces)' = ".   ",
                      '"," (Comma)' = ","
                    ),
                    selected = "';'"
                  ),
                  div(
                    style = "margin-top: 10px;",
                    materialSwitch(
                      inputId = "citNoOverlap",
                      label = "Avoid Label Overlap",
                      value = TRUE,
                      status = "primary"
                    )
                  )
                ),

                # Method Parameters Box
                box(
                  title = span(
                    icon("cogs", style = "margin-right: 5px;"),
                    strong("Method Parameters"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "primary",
                  fluidRow(
                    column(
                      6,
                      div(
                        style = "padding-right: 8px;",
                        selectInput(
                          "citlayout",
                          label = strong("Network Layout"),
                          choices = c(
                            "Automatic layout" = "auto",
                            "Circle" = "circle",
                            "Fruchterman & Reingold" = "fruchterman",
                            "Kamada & Kawai" = "kamada",
                            "MultiDimensional Scaling" = "mds",
                            "Sphere" = "sphere",
                            "Star" = "star"
                          ),
                          selected = "auto"
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        style = "padding-left: 8px;",
                        selectInput(
                          "cocitCluster",
                          label = strong("Clustering Algorithm"),
                          choices = c(
                            "None" = "none",
                            "Edge Betweenness" = "edge_betweenness",
                            "InfoMap" = "infomap",
                            "Leading Eigenvalues" = "leading_eigen",
                            "Leiden" = "leiden",
                            "Louvain" = "louvain",
                            "Spinglass" = "spinglass",
                            "Walktrap" = "walktrap"
                          ),
                          selected = "louvain"
                        )
                      )
                    )
                  ),
                  hr(style = "margin: 15px 0; border-top: 1px solid #ddd;"),
                  div(
                    style = "background-color: #f0f4f8; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
                    h6(
                      strong("Network Size"),
                      style = "color: #34495e; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          numericInput(
                            inputId = "citNodes",
                            label = "Number of Nodes",
                            min = 5,
                            max = 1000,
                            value = 50,
                            step = 1
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            inputId = "cocit.repulsion",
                            label = "Repulsion Force",
                            min = 0,
                            max = 1,
                            value = 0.5,
                            step = 0.1
                          )
                        )
                      )
                    )
                  ),
                  div(
                    style = "background-color: #fff8e6; padding: 12px; border-radius: 5px; border-left: 3px solid #f39c12;",
                    h6(
                      icon("filter", style = "margin-right: 5px;"),
                      strong("Filtering Options"),
                      style = "color: #e67e22; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          selectInput(
                            inputId = "cit.isolates",
                            label = "Remove Isolated Nodes",
                            choices = c("Yes" = "yes", "No" = "no"),
                            selected = "yes"
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            "citedges.min",
                            label = "Minimum Number of Edges",
                            value = 2,
                            step = 1,
                            min = 0
                          )
                        )
                      )
                    )
                  )
                ),

                # Graphical Parameters Box
                box(
                  title = span(
                    icon("palette", style = "margin-right: 5px;"),
                    strong("Graphical Parameters"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "info",
                  div(
                    style = "background-color: #f0f4f8; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
                    h6(
                      strong("Visual Appearance"),
                      style = "color: #34495e; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          selectInput(
                            inputId = "citShortlabel",
                            label = "Short Label",
                            choices = c("Yes", "No"),
                            selected = "Yes"
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            inputId = "citLabels",
                            label = "Number of Labels",
                            min = 0,
                            max = 1000,
                            value = 1000,
                            step = 1
                          )
                        )
                      )
                    )
                  ),
                  div(
                    style = "background-color: #e8f5e9; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
                    h6(
                      strong("Label Settings"),
                      style = "color: #2e7d32; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          selectInput(
                            inputId = "citlabel.cex",
                            label = "Label Scaling (cex)",
                            choices = c("Yes", "No"),
                            selected = "Yes"
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            inputId = "citlabelsize",
                            label = "Label Size",
                            min = 0.0,
                            max = 20,
                            value = 2,
                            step = 0.10
                          )
                        )
                      )
                    )
                  ),
                  div(
                    style = "background-color: #fce4ec; padding: 12px; border-radius: 5px;",
                    h6(
                      strong("Node & Edge Settings"),
                      style = "color: #c2185b; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          selectInput(
                            inputId = "cocit.shape",
                            label = "Node Shape",
                            choices = c(
                              "Box" = "box",
                              "Circle" = "circle",
                              "Dot" = "dot",
                              "Ellipse" = "ellipse",
                              "Square" = "square",
                              "Text" = "text"
                            ),
                            selected = "dot"
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            inputId = "citedgesize",
                            label = "Edge Size",
                            min = 0.5,
                            max = 20,
                            value = 2,
                            step = 0.5
                          )
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px; margin-top: 10px;",
                          selectInput(
                            inputId = "cocit.shadow",
                            label = "Node Shadow",
                            choices = c("Yes", "No"),
                            selected = "Yes"
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px; margin-top: 10px;",
                          selectInput(
                            inputId = "cocit.curved",
                            label = "Edit Nodes",
                            choices = c("Yes", "No"),
                            selected = "No"
                          )
                        )
                      )
                    )
                  )
                ),

                # Export Options
                br(),
                div(
                  style = "background-color: #e3f2fd; padding: 12px; border-radius: 8px;",
                  h6(
                    icon("download", style = "margin-right: 5px;"),
                    strong("Export Network"),
                    style = "color: #1976d2; margin-bottom: 12px;"
                  ),
                  fluidRow(
                    column(
                      6,
                      div(
                        style = "padding-right: 5px;",
                        downloadBttn(
                          outputId = "network.cocit",
                          label = "Pajek",
                          style = "pill",
                          color = "primary",
                          size = "sm",
                          block = TRUE
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        style = "padding-left: 5px;",
                        downloadBttn(
                          outputId = "networkCocit.fig",
                          label = "HTML",
                          style = "pill",
                          color = "primary",
                          size = "sm",
                          block = TRUE
                        )
                      )
                    )
                  )
                ),

                right = TRUE,
                animate = TRUE,
                circle = TRUE,
                style = "gradient",
                tooltip = tooltipOptions(title = "Options"),
                color = "primary",
                icon = icon("sliders"),
                width = "320px"
              )
            ),
            style = style_opt
          )
        ),
        fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Network",
              shinycssloaders::withSpinner(visNetworkOutput(
                "cocitPlot",
                height = "75vh"
              ))
            ),
            tabPanel(
              "Density",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "cocitOverlay",
                height = "75vh"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(
                outputId = "cocitTable"
              ))
            ),
            tabPanel(
              "Degree Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "cocitDegree",
                height = 700
              ))
            ),
            tabPanel(
              title = tagList(
                icon("microchip"),
                tags$span(strong("Biblio AI"), style = "margin-left: 5px;")
              ),
              fluidPage(fluidRow(column(
                12,
                br(),
                shinycssloaders::withSpinner(
                  htmlOutput("cocitGeminiUI"),
                  caption = HTML("<br><strong>Thinking...</strong>"),
                  image = "ai_small2.gif",
                  color = "#466fc4"
                )
              )))
            )
          )
        )
      )
    ),

    ##### historiograph ----
    tabItem(
      "historiograph",
      fluidPage(
        fluidRow(
          column(8, h3(strong("Historiograph"), align = "center")),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call("actionBttn", c(run_bttn, list(inputId = "applyHist")))
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(report_bttn, list(inputId = "reportHIST"))
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "actionBttn",
                c(export_bttn, list(inputId = "screenHIST"))
              )
            )
          ),

          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Main Configuration
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),
                  numericInput(
                    inputId = "histNodes",
                    label = "Number of Nodes",
                    min = 5,
                    max = 100,
                    value = 20,
                    step = 1
                  )
                ),

                # Graphical Parameters Box
                box(
                  title = span(
                    icon("palette", style = "margin-right: 5px;"),
                    strong("Graphical Parameters"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  status = "info",
                  div(
                    style = "background-color: #f0f4f8; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
                    h6(
                      strong("Label Configuration"),
                      style = "color: #34495e; margin-bottom: 10px;"
                    ),
                    selectInput(
                      inputId = "titlelabel",
                      label = "Node Label",
                      choices = c(
                        "Short id (1st Author, Year)" = "short",
                        "Document Title" = "title",
                        "Authors' Keywords" = "keywords",
                        "Keywords Plus" = "keywordsplus"
                      ),
                      selected = "short"
                    )
                  ),
                  div(
                    style = "background-color: #fff8e6; padding: 12px; border-radius: 5px; border-left: 3px solid #f39c12; margin-bottom: 10px;",
                    h6(
                      icon("filter", style = "margin-right: 5px;"),
                      strong("Filtering Options"),
                      style = "color: #e67e22; margin-bottom: 10px;"
                    ),
                    selectInput(
                      inputId = "hist.isolates",
                      label = "Remove Isolated Nodes",
                      choices = c("Yes" = "yes", "No" = "no"),
                      selected = "yes"
                    )
                  ),
                  div(
                    style = "background-color: #e8f5e9; padding: 12px; border-radius: 5px;",
                    h6(
                      strong("Visual Settings"),
                      style = "color: #2e7d32; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          numericInput(
                            inputId = "histlabelsize",
                            label = "Label Size",
                            min = 0.0,
                            max = 20,
                            value = 3,
                            step = 1
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            inputId = "histsize",
                            label = "Node Size",
                            min = 0,
                            max = 20,
                            value = 2,
                            step = 1
                          )
                        )
                      )
                    )
                  )
                ),

                right = TRUE,
                animate = TRUE,
                circle = TRUE,
                style = "gradient",
                tooltip = tooltipOptions(title = "Options"),
                color = "primary",
                icon = icon("sliders"),
                width = "320px"
              )
            ),
            style = style_opt
          )
        ),
        fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Network",
              shinycssloaders::withSpinner(visNetworkOutput(
                "histPlotVis",
                height = "80vh"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(outputId = "histTable"))
            ),
            tabPanel(
              title = tagList(
                icon("microchip"),
                tags$span(strong("Biblio AI"), style = "margin-left: 5px;")
              ),
              fluidPage(fluidRow(column(
                12,
                br(),
                shinycssloaders::withSpinner(
                  htmlOutput("histGeminiUI"),
                  caption = HTML("<br><strong>Thinking...</strong>"),
                  image = "ai_small2.gif",
                  color = "#466fc4"
                )
              )))
            )
          )
        )
      )
    ),
    #### Social Structure ----
    ##### collaboration network ----
    tabItem(
      "collabNetwork",
      fluidPage(
        # ============================================
        # HEADER SECTION WITH ACTION BUTTONS
        # ============================================
        fluidRow(
          column(
            8,
            h3(strong("Collaboration Network"), align = "center")
          ),

          # Run Analysis Button
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  run_bttn,
                  list(inputId = "applyCol")
                )
              )
            )
          ),

          # Add to Report Button
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  report_bttn,
                  list(inputId = "reportCOL")
                )
              )
            )
          ),

          # Export Button
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "actionBttn",
                c(
                  export_bttn,
                  list(inputId = "screenCOL")
                )
              )
            )
          ),

          # ============================================
          # OPTIONS DROPDOWN MENU
          # ============================================
          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # ============================================
                # MAIN CONFIGURATION
                # ============================================
                div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5(
                    strong("Main Configuration"),
                    style = "color: #2c3e50; margin-bottom: 10px;"
                  ),

                  # Field Selection
                  selectInput(
                    "colField",
                    label = "Field",
                    choices = c(
                      "Authors" = "COL_AU",
                      "Institutions" = "COL_UN",
                      "Countries" = "COL_CO"
                    ),
                    selected = "COL_AU"
                  ),

                  # Label Overlap Switch
                  div(
                    style = "margin-top: 10px;",
                    materialSwitch(
                      inputId = "colNoOverlap",
                      label = "Avoid Label Overlap",
                      value = TRUE,
                      status = "primary"
                    )
                  )
                ),

                # ============================================
                # METHOD PARAMETERS BOX
                # ============================================
                box(
                  title = span(
                    icon("cogs", style = "margin-right: 5px;"),
                    strong("Method Parameters"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "primary",

                  # Network Layout and Clustering Algorithm
                  fluidRow(
                    column(
                      6,
                      div(
                        style = "padding-right: 8px;",
                        selectInput(
                          "collayout",
                          label = strong("Network Layout"),
                          choices = c(
                            "Automatic layout" = "auto",
                            "Circle" = "circle",
                            "Fruchterman & Reingold" = "fruchterman",
                            "Kamada & Kawai" = "kamada",
                            "MultiDimensional Scaling" = "mds",
                            "Sphere" = "sphere",
                            "Star" = "star"
                          ),
                          selected = "auto"
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        style = "padding-left: 8px;",
                        selectInput(
                          "colCluster",
                          label = strong("Clustering Algorithm"),
                          choices = c(
                            "None" = "none",
                            "Edge Betweenness" = "edge_betweenness",
                            "InfoMap" = "infomap",
                            "Leading Eigenvalues" = "leading_eigen",
                            "Leiden" = "leiden",
                            "Louvain" = "louvain",
                            "Spinglass" = "spinglass",
                            "Walktrap" = "walktrap"
                          ),
                          selected = "louvain"
                        )
                      )
                    )
                  ),

                  # Normalization Method
                  fluidRow(
                    column(
                      12,
                      selectInput(
                        "colnormalize",
                        label = strong("Normalization Method"),
                        choices = c(
                          "None" = "none",
                          "Association" = "association",
                          "Jaccard" = "jaccard",
                          "Salton" = "salton",
                          "Inclusion" = "inclusion",
                          "Equivalence" = "equivalence"
                        ),
                        selected = "association"
                      )
                    )
                  ),

                  hr(style = "margin: 15px 0; border-top: 1px solid #ddd;"),

                  # Network Size Parameters
                  div(
                    style = "background-color: #f0f4f8; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
                    h6(
                      strong("Network Size"),
                      style = "color: #34495e; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          numericInput(
                            inputId = "colNodes",
                            label = "Number of Nodes",
                            min = 5,
                            max = 1000,
                            value = 50,
                            step = 1
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            inputId = "col.repulsion",
                            label = "Repulsion Force",
                            min = 0,
                            max = 1,
                            value = 0.5,
                            step = 0.1
                          )
                        )
                      )
                    )
                  ),

                  # Filtering Options
                  div(
                    style = "background-color: #fff8e6; padding: 12px; border-radius: 5px; border-left: 3px solid #f39c12;",
                    h6(
                      icon("filter", style = "margin-right: 5px;"),
                      strong("Filtering Options"),
                      style = "color: #e67e22; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          selectInput(
                            inputId = "col.isolates",
                            label = "Remove Isolated Nodes",
                            choices = c(
                              "Yes" = "yes",
                              "No" = "no"
                            ),
                            selected = "yes"
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            "coledges.min",
                            label = "Minimum Number of Edges",
                            value = 1,
                            step = 1,
                            min = 0
                          )
                        )
                      )
                    ),

                    # NEW: Filter articles with more than 20 authors
                    fluidRow(
                      column(
                        12,
                        div(
                          style = "margin-top: 10px;",
                          materialSwitch(
                            inputId = "col.filterMaxAuthors",
                            label = "Exclude articles with > 20 authors",
                            value = FALSE,
                            status = "warning"
                          ),
                          helpText(
                            "Enable this option to filter out hyper-authored papers (>20 authors) from the analysis.",
                            style = "margin-top: 5px; color: #666; font-size: 11px;"
                          )
                        )
                      )
                    )
                  )
                ),

                # ============================================
                # GRAPHICAL PARAMETERS BOX
                # ============================================
                box(
                  title = span(
                    icon("palette", style = "margin-right: 5px;"),
                    strong("Graphical Parameters"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = TRUE,
                  status = "info",

                  # Visual Appearance
                  div(
                    style = "background-color: #f0f4f8; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
                    h6(
                      strong("Visual Appearance"),
                      style = "color: #34495e; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          numericInput(
                            inputId = "colAlpha",
                            label = "Opacity",
                            min = 0,
                            max = 1,
                            value = 0.7,
                            step = 0.05
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            inputId = "colLabels",
                            label = "Number of Labels",
                            min = 0,
                            max = 1000,
                            value = 1000,
                            step = 1
                          )
                        )
                      )
                    )
                  ),

                  # Label Settings
                  div(
                    style = "background-color: #e8f5e9; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
                    h6(
                      strong("Label Settings"),
                      style = "color: #2e7d32; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          selectInput(
                            inputId = "collabel.cex",
                            label = "Label Scaling (cex)",
                            choices = c("Yes", "No"),
                            selected = "Yes"
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            inputId = "collabelsize",
                            label = "Label Size",
                            min = 0.0,
                            max = 20,
                            value = 2,
                            step = 0.10
                          )
                        )
                      )
                    )
                  ),

                  # Node and Edge Settings
                  div(
                    style = "background-color: #fce4ec; padding: 12px; border-radius: 5px;",
                    h6(
                      strong("Node & Edge Settings"),
                      style = "color: #c2185b; margin-bottom: 10px;"
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px;",
                          selectInput(
                            inputId = "col.shape",
                            label = "Node Shape",
                            choices = c(
                              "Box" = "box",
                              "Circle" = "circle",
                              "Dot" = "dot",
                              "Ellipse" = "ellipse",
                              "Square" = "square",
                              "Text" = "text"
                            ),
                            selected = "dot"
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px;",
                          numericInput(
                            inputId = "coledgesize",
                            label = "Edge Size",
                            min = 0.5,
                            max = 20,
                            value = 5,
                            step = 0.5
                          )
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          style = "padding-right: 8px; margin-top: 10px;",
                          selectInput(
                            inputId = "col.shadow",
                            label = "Node Shadow",
                            choices = c("Yes", "No"),
                            selected = "Yes"
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "padding-left: 8px; margin-top: 10px;",
                          selectInput(
                            inputId = "soc.curved",
                            label = "Edit Nodes",
                            choices = c("Yes", "No"),
                            selected = "No"
                          )
                        )
                      )
                    )
                  )
                ),

                # ============================================
                # EXPORT OPTIONS
                # ============================================
                br(),
                div(
                  style = "background-color: #e3f2fd; padding: 12px; border-radius: 8px;",
                  h6(
                    icon("download", style = "margin-right: 5px;"),
                    strong("Export Network"),
                    style = "color: #1976d2; margin-bottom: 12px;"
                  ),
                  fluidRow(
                    column(
                      6,
                      div(
                        style = "padding-right: 5px;",
                        downloadBttn(
                          outputId = "network.col",
                          label = "Pajek",
                          style = "pill",
                          color = "primary",
                          size = "sm",
                          block = TRUE
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        style = "padding-left: 5px;",
                        downloadBttn(
                          outputId = "networkCol.fig",
                          label = "HTML",
                          style = "pill",
                          color = "primary",
                          size = "sm",
                          block = TRUE
                        )
                      )
                    )
                  )
                ),

                right = TRUE,
                animate = TRUE,
                circle = TRUE,
                style = "gradient",
                tooltip = tooltipOptions(title = "Options"),
                color = "primary",
                icon = icon("sliders"),
                width = "320px"
              )
            ),
            style = style_opt
          )
        ),

        # ============================================
        # MAIN CONTENT AREA WITH TABS
        # ============================================
        fluidRow(
          tabsetPanel(
            type = "tabs",

            # Network Tab
            tabPanel(
              "Network",
              shinycssloaders::withSpinner(
                visNetworkOutput("colPlot", height = "75vh")
              )
            ),

            # Diachronic Network Tab
            tabPanel(
              "Diachronic Network",
              br(),
              fluidRow(
                column(
                  2,
                  actionButton(
                    "start_col",
                    "Start",
                    icon = icon("play"),
                    width = "90%"
                  )
                ),
                column(
                  2,
                  actionButton(
                    "pause_col",
                    "Pause / Resume",
                    icon = icon("pause"),
                    width = "90%"
                  )
                ),
                column(
                  2,
                  actionButton(
                    "reset_col",
                    "Reset",
                    icon = icon("rotate-left"),
                    width = "90%"
                  )
                ),
                column(2, uiOutput("export_colUI")),
                column(1, div(style = "text-align:right;", "Speed (ms)")),
                column(
                  2,
                  selectInput(
                    "speed_col",
                    label = NULL,
                    seq(250, 2000, by = 250),
                    selected = 500
                  )
                )
              ),
              fluidRow(
                column(12, uiOutput("year_slider_colUI"))
              ),
              fluidRow(
                column(1, uiOutput("colYearUI")),
                column(11, visNetworkOutput("colOverTime", height = "65vh"))
              )
            ),

            # Density Tab
            tabPanel(
              "Density",
              shinycssloaders::withSpinner(
                plotlyOutput(outputId = "colOverlay", height = "75vh")
              )
            ),

            # Table Tab
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(
                DT::DTOutput(outputId = "colTable")
              )
            ),

            # Degree Plot Tab
            tabPanel(
              "Degree Plot",
              shinycssloaders::withSpinner(
                plotlyOutput(outputId = "colDegree", height = "75vh")
              )
            ),

            # Biblio AI Tab
            tabPanel(
              title = tagList(
                icon("microchip"),
                tags$span(strong("Biblio AI"), style = "margin-left: 5px;")
              ),
              fluidPage(
                fluidRow(
                  column(
                    12,
                    br(),
                    shinycssloaders::withSpinner(
                      htmlOutput("colGeminiUI"),
                      caption = HTML("<br><strong>Thinking...</strong>"),
                      image = "ai_small2.gif",
                      color = "#466fc4"
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    ##### collaboration world map ----
    tabItem(
      "collabWorldMap",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Countries' Collaboration World Map"), align = "center")
          ),
          div(
            style = style_bttn,
            title = t_run,
            column(
              1,
              do.call("actionBttn", c(run_bttn, list(inputId = "applyWM")))
            )
          ),
          div(
            style = style_bttn,
            title = t_report,
            column(
              1,
              do.call(
                "actionBttn",
                c(report_bttn, list(inputId = "reportCOLW"))
              )
            )
          ),
          div(
            style = style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "downloadBttn",
                c(export_bttn, list(outputId = "CCplot.save"))
              )
            )
          ),

          div(
            column(
              1,
              dropdown(
                h4(strong("Options: ")),
                br(),

                # Method Parameters Box
                box(
                  title = span(
                    icon("cogs", style = "margin-right: 5px;"),
                    strong("Method Parameters"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  status = "primary",
                  div(
                    style = "background-color: #fff8e6; padding: 12px; border-radius: 5px; border-left: 3px solid #f39c12;",
                    h6(
                      icon("filter", style = "margin-right: 5px;"),
                      strong("Filtering Options"),
                      style = "color: #e67e22; margin-bottom: 10px;"
                    ),
                    numericInput(
                      "WMedges.min",
                      label = "Min Edges",
                      value = 2,
                      step = 1
                    )
                  )
                ),

                # Graphical Parameters Box
                box(
                  title = span(
                    icon("palette", style = "margin-right: 5px;"),
                    strong("Graphical Parameters"),
                    style = "font-size: 16px; color: #2c3e50;"
                  ),
                  collapsible = TRUE,
                  width = 15,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  status = "info",
                  div(
                    style = "background-color: #fce4ec; padding: 12px; border-radius: 5px;",
                    h6(
                      strong("Edge Settings"),
                      style = "color: #c2185b; margin-bottom: 10px;"
                    ),
                    numericInput(
                      inputId = "WMedgesize",
                      label = "Edge Size",
                      min = 0.1,
                      max = 20,
                      value = 5
                    )
                  )
                ),

                right = TRUE,
                animate = TRUE,
                circle = TRUE,
                style = "gradient",
                tooltip = tooltipOptions(title = "Options"),
                color = "primary",
                icon = icon("sliders"),
                width = "320px"
              )
            ),
            style = style_opt
          )
        ),
        fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(plotlyOutput(
                outputId = "WMPlot",
                height = "75vh"
              ))
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(DT::DTOutput(outputId = "WMTable"))
            ),
            tabPanel(
              title = tagList(
                icon("microchip"),
                tags$span(strong("Biblio AI"), style = "margin-left: 5px;")
              ),
              fluidPage(fluidRow(column(
                12,
                br(),
                shinycssloaders::withSpinner(
                  htmlOutput("WMGeminiUI"),
                  caption = HTML("<br><strong>Thinking...</strong>"),
                  image = "ai_small2.gif",
                  color = "#466fc4"
                )
              )))
            )
          )
        )
      )
    ),
    #### Content Analysis ----
    content_analysis_tab("content_analysis"),

    #### Report ----
    tabItem(
      "report",
      fluidPage(
        fluidRow(
          h3(strong("Report"), align = "center"),
          br(),
        ),
        fluidRow(
          column(
            6,
            offset = 1,
            box(
              title = strong(
                "Select results to include in the Report",
                style = "font-size:20px;color:white;"
              ),
              status = "primary",
              width = 11,
              solidHeader = TRUE,
              tags$style(HTML(
                "
                         .box.box-solid.box-primary>.box-header {
                         background:#4379cd;
                         }
                         .box.box-solid.box-primary{
                         border-bottom-color:black;
                         border-left-color:black;
                         border-right-color:black;
                         border-top-color:black;
                         border-width:2px;
                                         }"
              )),
              uiOutput("reportSheets"),
              tags$style("#reportSheets {font-size:18px;}")
            )
          ), # column(1),
          column(
            2,
            div(
              style = "border-radius: 10px; border-width: 3px; font-size: 10px;",
              align = "center",
              # width="100%",
              actionBttn(
                inputId = "allSheets",
                label = strong("Select All"),
                icon = icon("ok-circle", lib = "glyphicon"),
                style = "pill",
                color = "primary",
                block = TRUE
              ),
              # tags$style("#allSheets {font-size:20px; color:#363636; background-color:white; text-align:center; border-width: 3px;}"),
              br(),
              actionBttn(
                inputId = "noSheets",
                label = strong("Deselect All"),
                icon = icon("remove-circle", lib = "glyphicon"),
                style = "pill",
                color = "primary",
                block = TRUE
              ),
              # tags$style("#noSheets {font-size:20px; color:#363636; background-color:white; text-align:center; border-width: 3px;}"),

              # tags$style("#deleteAll {border-width: 3px;}"),
              br(),
              hr(),
              downloadBttn(
                outputId = "report.save",
                label = strong("Export Report"),
                style = "pill",
                color = "success",
                size = "md",
                block = TRUE,
                no_outline = TRUE,
                icon = icon(name = "download-alt", lib = "glyphicon")
              ), # , tags$style("#report.save {border-width: 3px;}")
              br(),
              hr(),
              actionBttn(
                inputId = "deleteAll",
                label = strong("Delete Report"),
                icon = icon("exclamation-sign", lib = "glyphicon"),
                style = "pill",
                color = "danger",
                block = TRUE
              )
            )
          )
        )
      )
    ),
    #### TALL Export ----
    tabItem(
      "tall",
      fluidPage(
        fluidRow(
          h3(strong("TALL - Text Analysis for All"), align = "center"),
          br()
        ),
        fluidRow(
          column(
            8,
            fluidRow(
              column(
                4,
                div(
                  img(src = "tall_logo.jpg", height = "90%", width = "90%"),
                  style = "text-align: center;"
                )
              ),
              column(
                8,
                HTML(
                  "<div style='text-align: center;'>
    <p style='font-size: 16px;max-width: 1400px; margin: auto; text-align: justify;'>
    <strong>Biblioshiny</strong> now includes a dedicated export tool that allows you to prepare and extract textual data (Titles, Abstracts, and Keywords) from your bibliographic collection in a format ready to be used in TALL.
    <br><br>
      <strong>TALL</strong> is a user-friendly R Shiny application designed to support researchers in performing textual data analysis without requiring advanced programming skills.
    <br><br>
      <strong>TALL</strong> offers a comprehensive workflow for data cleaning, pre-processing, statistical analysis, and visualization of textual data, by combining state-of-the-art text analysis techniques into an R Shiny app.
    <br><br>
      <strong>TALL</strong> includes a wide set of methodologies specifically tailored for various text analysis tasks. It aims to address the needs of researchers without extensive programming skills, providing a versatile and general-purpose tool for analyzing textual data. With TALL, researchers can leverage a wide range of text analysis techniques without the burden of extensive programming knowledge, enabling them to extract valuable insights from textual data in a more efficient and accessible manner.
    <br><br>
      Learn more at: <a href='https://www.tall-app.com' target='_blank'>www.tall-app.com</a>
        </p>
        </div>"
                )
              )
            ),
            fluidRow(
              shinycssloaders::withSpinner(DT::DTOutput(outputId = "tallTable"))
            )
          ),
          column(
            4,
            fluidRow(
              box(
                width = 12,
                div(
                  h3(strong(em("Export a corpus for TALL"))),
                  style = "margin-top:-57px"
                ),
                hr(),
                helpText(h4("Select textual metadata:")),
                multiInput(
                  inputId = "tallFields",
                  label = NULL,
                  choiceNames = "",
                  choiceValues = "",
                  width = "100%"
                ),
                hr(),
                helpText(h4("Select additional metadata:")),
                multiInput(
                  inputId = "tallMetadata",
                  label = NULL,
                  choiceNames = "",
                  choiceValues = "",
                  width = "100%"
                ),
                hr(),
                div(
                  fluidRow(
                    column(
                      6,
                      div(
                        style = style_bttn,
                        align = "center",
                        title = t_run,
                        # column(1,
                        do.call(
                          "actionBttn",
                          c(
                            run_bttn,
                            list(
                              inputId = "tallRun"
                            )
                          )
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        style = style_bttn,
                        align = "center",
                        title = t_export,
                        do.call(
                          "downloadBttn",
                          c(
                            export_bttn,
                            list(
                              outputId = "tall.save"
                            )
                          )
                        )
                      )
                    )
                  ),
                  style = "margin-top:-15px"
                ),
                hr(),
                helpText(
                  "Select at least one textual field to export, click 'Play' to generate the dataset, then save and import it into TALL.",
                  style = "font-size: 16px"
                ),
                uiOutput("tallBttn1"),
                uiOutput("tallBttn2")
                # fluidRow(
                #   # column(6,
                #   #        uiOutput("tallBttn1")),
                #   # column(6,
                #   #        uiOutput("tallBttn2"))
                # )
              )
            )
          )
        )
      )
    ),

    #### Settings ----
    tabItem(
      "settings",
      fluidPage(
        # Page Header
        fluidRow(
          column(
            12,
            div(
              class = "page-header",
              h2(
                icon("cog", style = "margin-right: 10px;"),
                "Settings",
                style = "color: #2E86AB; margin-bottom: 20px; font-weight: 600;"
              ),
              p(
                "Configure global settings for plots, analysis reproducibility, and AI features.",
                style = "color: #666; font-size: 16px;"
              )
            )
          )
        ),

        br(),

        # ============================================
        # SECTION 1: PLOT SETTINGS
        # ============================================
        fluidRow(
          column(
            6,
            div(
              class = "box box-primary",
              div(
                class = "box-header with-border",
                h4(
                  icon("chart-bar", style = "margin-right: 8px;"),
                  "Plot Export Settings",
                  class = "box-title",
                  style = "color: #2E86AB; font-weight: 600;"
                )
              ),
              div(
                class = "box-body",

                # DPI Setting
                div(
                  style = "margin-bottom: 25px;",
                  tags$label(
                    "Resolution (DPI)",
                    style = "font-weight: 600; color: #2E86AB; margin-bottom: 8px; display: block;"
                  ),
                  sliderTextInput(
                    inputId = "dpi",
                    label = NULL,
                    grid = TRUE,
                    force_edges = TRUE,
                    choices = c("75", "150", "300", "600"),
                    width = "100%",
                    selected = "300"
                  ),
                  helpText(
                    "Higher DPI values produce better quality images but larger file sizes.",
                    style = "margin-top: 5px; color: #666; font-size: 12px;"
                  )
                ),

                # Height Setting
                div(
                  style = "margin-bottom: 10px;",
                  tags$label(
                    "Plot Height (inches)",
                    style = "font-weight: 600; color: #2E86AB; margin-bottom: 8px; display: block;"
                  ),
                  sliderTextInput(
                    inputId = "h",
                    label = NULL,
                    grid = TRUE,
                    force_edges = TRUE,
                    width = "100%",
                    choices = seq(5, 15),
                    selected = "7"
                  ),
                  helpText(
                    "Adjust the height of exported plots. Width is automatically calculated to maintain aspect ratio.",
                    style = "margin-top: 5px; color: #666; font-size: 12px;"
                  )
                )
              )
            )
          ),

          # ============================================
          # SECTION 2: REPRODUCIBILITY SETTINGS
          # ============================================
          column(
            6,
            div(
              class = "box box-success",
              div(
                class = "box-header with-border",
                h4(
                  icon("random", style = "margin-right: 8px;"),
                  "Reproducibility Settings",
                  class = "box-title",
                  style = "color: #27ae60; font-weight: 600;"
                )
              ),
              div(
                class = "box-body",

                # Random Seed Setting
                div(
                  style = "margin-bottom: 15px;",
                  tags$label(
                    "Random Seed",
                    style = "font-weight: 600; color: #27ae60; margin-bottom: 8px; display: block;"
                  ),
                  div(
                    style = "display: flex; gap: 10px; align-items: flex-start;",
                    div(
                      style = "flex: 1;",
                      numericInput(
                        "random_seed",
                        label = NULL,
                        value = 1234,
                        min = 1,
                        max = 999999,
                        step = 1,
                        width = "100%"
                      )
                    ),
                    div(
                      style = "padding-top: 5px;",
                      actionButton(
                        "randomize_seed",
                        "Randomize",
                        icon = icon("dice"),
                        class = "btn-default",
                        style = "padding: 6px 12px;"
                      )
                    )
                  ),
                  helpText(
                    "Set a random seed to ensure reproducible results in stochastic algorithms (e.g., community detection, network layouts).",
                    style = "margin-top: 5px; color: #666; font-size: 12px;"
                  )
                ),

                # Info Box
                div(
                  style = "background-color: #e8f5e9; padding: 12px; border-radius: 5px; border-left: 4px solid #27ae60; margin-top: 15px;",
                  icon(
                    "info-circle",
                    style = "color: #27ae60; margin-right: 8px;"
                  ),
                  tags$span(
                    "Using the same seed value ensures that analyses involving randomization will produce identical results when re-run.",
                    style = "color: #555; font-size: 13px;"
                  )
                )
              )
            )
          )
        ),

        # ============================================
        # SECTION 3: BIBLIO AI API SETTINGS
        # ============================================
        fluidRow(
          column(
            12,
            div(
              class = "box box-warning",
              div(
                class = "box-header with-border",
                h4(
                  icon("robot", style = "margin-right: 8px;"),
                  "Biblio AI - Google Gemini Integration",
                  class = "box-title",
                  style = "color: #f39c12; font-weight: 600;"
                )
              ),
              div(
                class = "box-body",

                # Description
                div(
                  style = "margin-bottom: 20px; padding: 15px; background-color: #fff3cd; border-radius: 5px; border-left: 4px solid #f39c12;",
                  icon(
                    "lightbulb",
                    style = "color: #f39c12; margin-right: 8px; font-size: 18px;"
                  ),
                  tags$span(
                    style = "color: #856404; font-size: 14px;",
                    HTML(
                      "Enable advanced AI-powered features by providing your Google Gemini API Key. If you don't have one, you can generate it at "
                    ),
                    tags$a(
                      href = "https://aistudio.google.com/app/apikey",
                      target = "_blank",
                      style = "color: #856404; font-weight: bold; text-decoration: underline;",
                      "AI Studio"
                    ),
                    HTML(".")
                  )
                ),

                # API Key Configuration
                fluidRow(
                  # API Key Input Column
                  column(
                    4,
                    div(
                      style = "padding-right: 15px;",
                      tags$label(
                        "API Key",
                        style = "font-weight: 600; color: #f39c12; margin-bottom: 8px; display: block;"
                      ),
                      passwordInput(
                        "api_key",
                        label = NULL,
                        placeholder = "Enter your Gemini API Key",
                        value = "",
                        width = "100%"
                      ),

                      # Status Display
                      div(
                        style = "margin: 10px 0;",
                        uiOutput("apiStatus")
                      ),

                      # Action Buttons
                      fluidRow(
                        column(
                          6,
                          actionButton(
                            "set_key",
                            "Set API Key",
                            icon = icon("check"),
                            class = "btn-success btn-block",
                            style = "font-weight: 600;"
                          )
                        ),
                        column(
                          6,
                          actionButton(
                            "remove_key",
                            "Remove Key",
                            icon = icon("trash"),
                            class = "btn-danger btn-block",
                            style = "font-weight: 600;"
                          )
                        )
                      )
                    )
                  ),

                  # Model Selection Column
                  column(
                    4,
                    div(
                      style = "padding: 0 15px;",
                      tags$label(
                        "Model Selection",
                        style = "font-weight: 600; color: #f39c12; margin-bottom: 8px; display: block;"
                      ),
                      uiOutput("geminiModelChoice"),
                      helpText(
                        "Select the Gemini model to use for AI operations.",
                        style = "margin-top: 8px; color: #666; font-size: 12px;"
                      )
                    )
                  ),

                  # Output Size Column
                  column(
                    4,
                    div(
                      style = "padding-left: 15px;",
                      tags$label(
                        "Output Size",
                        style = "font-weight: 600; color: #f39c12; margin-bottom: 8px; display: block;"
                      ),
                      uiOutput("geminiOutputSize"),
                      helpText(
                        "Configure the maximum output length for AI responses.",
                        style = "margin-top: 8px; color: #666; font-size: 12px;"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

## UI ####
ui <- shinydashboardPlus::dashboardPage(
  # shinyjs::useShinyjs(),
  header = header,
  sidebar = sidebar,
  body = body,
  # controlbar = controlbar,
  footer = NULL,
  options = list(sidebarExpandOnHover = TRUE),
  scrollToTop = TRUE
)
# END ----
