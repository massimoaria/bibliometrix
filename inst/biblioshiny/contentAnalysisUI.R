## UI and SERVER components for biblioshiny integration ----

#' Content Analysis Tab Item with 3 Tabs for Biblioshiny UI (VERSIONE CORRETTA)
#'
#' This creates a tabItem with 3 separate tabs:
#' 1. Descriptive Statistics
#' 2. In-Context Citation Analysis  
#' 3. Citation Network Analysis
#'
#' @param id Character, the tab item ID (default: "content_analysis")
#' @return A tabItem object for biblioshiny

content_analysis_tab <- function(id = "content_analysis") {
  
  tabItem(
    tabName = id,
    
    # Tab header
    # fluidRow(
    #   column(12,
    #          div(class = "page-header",
    #              h2("Scientific Article Content Analysis", 
    #                 style = "color: #2E86AB; margin-bottom: 10px;"),
    #              p("Upload a PDF file and analyze citation patterns, context, and co-occurrence networks.",
    #                style = "color: #666; font-size: 16px; margin-bottom: 20px;")
    #          )
    #   )
    # ),
    # Tab header
    fluidRow(
      column(8,
             div(class = "page-header",
                 h2("Scientific Article Content Analysis", 
                    style = "color: #2E86AB; margin-bottom: 10px;"),
                 p("Upload a PDF file and analyze citation patterns, context, and co-occurrence networks.",
                   style = "color: #666; font-size: 16px; margin-bottom: 20px;")
             )
      ),
      column(4,
             div(
               style = "text-align: right; padding-top: 15px;",
               conditionalPanel(
                 condition = "output.analysis_completed",
                 downloadButton(
                   "download_all_results",
                   "Export All Results",
                   icon = icon("file-excel"),
                   class = "btn-success",
                   style = "font-weight: bold;"
                 )
               )
             )
      )
    ),
    
    fluidRow(
      
      # ===========================================
      # RIGHT PANEL: TABBED RESULTS (column 10)
      # ===========================================
      column(10,
             
             # NUOVO: Bottone per aprire il modal e campo DOI (visibile solo dopo estrazione)
             conditionalPanel(
               condition = "output.text_extracted && !output.analysis_completed",
               div(
                 style = "margin-bottom: 15px; padding: 15px; background-color: #e8f4f8; border-radius: 5px;",
                 
                 # Prima riga: bottone preview e messaggio
                 fluidRow(
                   column(12,
                          actionButton(
                            "open_preview_btn",
                            "View Extracted Text & PDF Preview",
                            icon = icon("file-alt"),
                            class = "btn-info",
                            onclick = "$('#previewModal').modal('show');"
                          ),
                          span(" Text extracted successfully!",
                               style = "margin-left: 15px; color: #555; font-weight: 500;")
                   )
                 ),
                 
                 # Seconda riga: campo DOI
                 fluidRow(
                   style = "margin-top: 15px;",
                   column(12,
                          div(
                            style = "background-color: white; padding: 10px; border-radius: 4px; border: 1px solid #d1e7f0;",
                            fluidRow(
                              column(2,
                                     tags$label(
                                       "Document DOI:",
                                       style = "margin-top: 8px; font-weight: bold; color: #2E86AB;"
                                     )
                              ),
                              column(7,
                                     textInput(
                                       "pdf_doi_input",
                                       label = NULL,
                                       value = "",
                                       placeholder = "Enter or edit DOI (e.g., 10.1234/example)",
                                       width = "100%"
                                     )
                              ),
                              column(3,
                                     div(
                                       style = "margin-top: 8px;",
                                       uiOutput("doi_status_icon")
                                     )
                              )
                            ),
                            helpText(
                              "The DOI is used to fetch reference details from Crossref. Edit if needed or add manually if not detected.",
                              style = "margin-bottom: 0px; margin-top: -5px; font-size: 11px; color: #666;"
                            )
                          )
                   )
                 )
               )
             ),
             
             conditionalPanel(
               condition = "output.analysis_completed",
               
               # NUOVO: Info dopo l'analisi
               div(
                 style = "margin-bottom: 10px;",
                 fluidRow(
                   column(4,
                          actionButton(
                            "reopen_preview_btn",
                            "View Original Text",
                            icon = icon("file-alt"),
                            class = "btn-default btn-sm",
                            onclick = "$('#previewModal').modal('show');"
                          )
                   ),
                   column(8,
                          div(
                            style = "text-align: right; padding-top: 5px;",
                            tags$strong("DOI: ", style = "color: #2E86AB;"),
                            tags$span(
                              textOutput("doi_display_after_analysis", inline = TRUE),
                              style = "font-family: 'Courier New', monospace; color: #555; background-color: #f8f9fa; padding: 4px 8px; border-radius: 3px; border: 1px solid #dee2e6;"
                            )
                          )
                   )
                 )
               ),
               tabsetPanel(
                 id = "content_tabs",
                 type = "tabs",
                 
                 # ===========================================
                 # TAB 1: DESCRIPTIVE STATISTICS
                 # ===========================================
                 tabPanel(
                   title = "Descriptive Statistics",
                   value = "tab_stats",
                   
                   br(),
                   
                   # Summary Statistics Cards
                   fluidRow(
                     column(3,
                            div(class = "info-box bg-aqua",
                                span(class = "info-box-icon", icon("file-text")),
                                div(class = "info-box-content",
                                    span("Total Words", class = "info-box-text"),
                                    span(textOutput("total_words", inline = TRUE), class = "info-box-number")
                                )
                            )
                     ),
                     column(3,
                            div(class = "info-box bg-green",
                                span(class = "info-box-icon", icon("quote-right")),
                                div(class = "info-box-content",
                                    span("Citations Found", class = "info-box-text"),
                                    span(textOutput("total_citations", inline = TRUE), class = "info-box-number")
                                )
                            )
                     ),
                     column(3,
                            div(class = "info-box bg-yellow",
                                span(class = "info-box-icon", icon("users")),
                                div(class = "info-box-content",
                                    span("Narrative Citations", class = "info-box-text"),
                                    span(textOutput("narrative_citations", inline = TRUE), class = "info-box-number")
                                )
                            )
                     ),
                     column(3,
                            div(class = "info-box bg-red",
                                span(class = "info-box-icon", icon("chart-line")),
                                div(class = "info-box-content",
                                    span("Citation Density", class = "info-box-text"),
                                    span(textOutput("citation_density", inline = TRUE), class = "info-box-number"),
                                    span("/1000 words", class = "info-box-more")
                                )
                            )
                     )
                   ),
                   
                   fluidRow(
                     column(3,
                            div(class = "info-box bg-purple",
                                span(class = "info-box-icon", icon("graduation-cap")),
                                div(class = "info-box-content",
                                    span("Flesch-Kincaid Grade", class = "info-box-text"),
                                    span(textOutput("flesch_kincaid_grade", inline = TRUE), class = "info-box-number")
                                )
                            )
                     ),
                     column(3,
                            div(class = "info-box bg-blue",
                                span(class = "info-box-icon", icon("book-reader")),
                                div(class = "info-box-content",
                                    span("Reading Ease", class = "info-box-text"),
                                    span(textOutput("flesch_reading_ease", inline = TRUE), class = "info-box-number")
                                )
                            )
                     ),
                     column(3,
                            div(class = "info-box bg-teal",
                                span(class = "info-box-icon", icon("robot")),
                                div(class = "info-box-content",
                                    span("ARI Index", class = "info-box-text"),
                                    span(textOutput("ari_index", inline = TRUE), class = "info-box-number")
                                )
                            )
                     ),
                     column(3,
                            div(class = "info-box bg-maroon",
                                span(class = "info-box-icon", icon("cloud")),
                                div(class = "info-box-content",
                                    span("Gunning Fog Index", class = "info-box-text"),
                                    span(textOutput("gunning_fog_index", inline = TRUE), class = "info-box-number")
                                )
                            )
                     )
                   ),
                   # Readability Indices Details
                   fluidRow(
                     column(6,
                            div(class = "box box-info",
                                div(class = "box-header with-border",
                                    h5("Readability Indices", style = "color: #3498db;")
                                ),
                                div(class = "box-body",
                                    conditionalPanel(
                                      condition = "output.analysis_completed",
                                      uiOutput("readability_details_html")
                                    ),
                                    conditionalPanel(
                                      condition = "!output.analysis_completed",
                                      div(
                                        style = "text-align: center; padding: 20px; color: #999;",
                                        p("Readability indices will appear here after analysis.")
                                      )
                                    )
                                )
                            )
                     ),
                     column(6,
                            div(class = "box box-primary",
                                div(class = "box-header with-border",
                                    h5("Text Statistics", style = "color: #2E86AB;")
                                ),
                                div(class = "box-body",
                                    conditionalPanel(
                                      condition = "output.analysis_completed",
                                      uiOutput("text_stats")
                                    ),
                                    conditionalPanel(
                                      condition = "!output.analysis_completed",
                                      div(
                                        style = "text-align: center; padding: 20px; color: #999;",
                                        p("Text statistics will appear here after analysis.")
                                      )
                                    )
                                )
                            )
                     )
                   ),
                   # Detailed Analysis Tables
                   fluidRow(
                     div(class = "box box-primary",
                         div(class = "box-header with-border",
                             h4("N-grams Analysis", class = "box-title", style = "color: #2E86AB;")
                         ),
                         column(4,
                                h5("Top Unigrams", class = "box-title", style = "color: #2E86AB;"),
                                DT::dataTableOutput("frequent_words_table")
                         ),
                         column(4,
                                h5("Top Bigrams", style = "color: #2E86AB;"),
                                DT::dataTableOutput("bigrams_table")
                         ),
                         column(4,
                                h5("Top Trigrams", style = "color: #2E86AB;"),
                                DT::dataTableOutput("trigrams_table")
                         )
                     )
                   ),
                   hr(),
                   # N-grams Analysis
                   fluidRow(
                     column(12,
                            div(class = "box-body",
                                fluidRow(
                                  column(6,
                                         div(class = "box box-primary",
                                             h5("Citation Types Distribution", class = "box-title", style = "color: #2E86AB;"),
                                             div(class = "box-header with-border",
                                                 div(class = "box-body",
                                                     DT::dataTableOutput("citation_types_table")
                                                 )
                                             )
                                         )
                                  ),
                                  column(6,
                                         div(class = "box box-success",
                                             h5("Citations by Section", class = "box-title", style = "color: #27ae60;"),
                                             div(class = "box-header with-border",
                                                 div(class = "box-body",
                                                     DT::dataTableOutput("citation_sections_table")
                                                 )
                                             )
                                         )
                                  )
                                )
                            )
                     )
                   )
                 ),
                 
                 # Aggiungi questo nuovo tabPanel DOPO il tab "Network Analysis" nella funzione content_analysis_tab
                 
                 tabPanel(
                   title = "Word Trends",
                   value = "tab_trends",
                   
                   br(),
                   
                   # Word Selection and Configuration
                   fluidRow(
                     column(12,
                            div(class = "box box-primary",
                                div(class = "box-header with-border",
                                    h4("Word Distribution Analysis", class = "box-title", style = "color: #2E86AB;"),
                                    div(class = "box-tools pull-right",
                                        downloadButton("download_word_trends",
                                                       "Export Data",
                                                       class = "btn btn-primary btn-sm",
                                                       icon = icon("download")
                                        )
                                    )
                                ),
                                div(class = "box-body",
                                    fluidRow(
                                      column(4,
                                             selectizeInput("trend_words",
                                                            "Select words to track:",
                                                            choices = NULL,
                                                            selected = NULL,
                                                            multiple = TRUE,
                                                            options = list(
                                                              placeholder = 'Type or select words...',
                                                              maxItems = 10,
                                                              plugins = list('remove_button')
                                                            )
                                             ),
                                             helpText("Select up to 10 words from the most frequent terms in your document.")
                                      ),
                                      column(3,
                                             radioButtons("segmentation_type",
                                                          "Segmentation:",
                                                          choices = list(
                                                            "Auto (use sections if available)" = "auto",
                                                            "Document sections" = "sections",
                                                            "Equal-length segments" = "segments"
                                                          ),
                                                          selected = "auto"
                                             )
                                      ),
                                      column(2,
                                             conditionalPanel(
                                               condition = "input.segmentation_type == 'segments'",
                                               numericInput("n_segments",
                                                            "Number of segments:",
                                                            value = 10,
                                                            min = 5,
                                                            max = 20,
                                                            step = 1
                                               )
                                             )
                                      ),
                                      column(3,
                                             radioButtons("trend_plot_type",
                                                          "Visualization type:",
                                                          choices = list(
                                                            "Line chart" = "line",
                                                            "Area chart" = "area"
                                                          ),
                                                          selected = "line"
                                             ),
                                             checkboxInput("trend_show_points",
                                                           "Show data points",
                                                           value = TRUE
                                             )
                                             # ,checkboxInput("trend_smooth",
                                             #               "Smooth lines",
                                             #               value = TRUE
                                             # )
                                      )
                                    ),
                                    hr(),
                                    fluidRow(
                                      column(12,
                                             actionButton("update_trends",
                                                          "Update Visualization",
                                                          icon = icon("refresh"),
                                                          class = "btn-info",
                                                          style = "margin-bottom: 10px;"
                                             ),
                                             conditionalPanel(
                                               condition = "input.segmentation_type == 'sections' && !output.sections_available",
                                               div(
                                                 style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #ffc107;",
                                                 icon("exclamation-triangle", style = "color: #856404;"),
                                                 span(" Document sections are not available. The analysis will use equal-length segments instead.",
                                                      style = "color: #856404; margin-left: 8px;")
                                               )
                                             )
                                      )
                                    )
                                )
                            )
                     )
                   ),
                   
                   # Visualization
                   fluidRow(
                     column(12,
                            div(class = "box box-success",
                                div(class = "box-header with-border",
                                    h4("Word Distribution Over Document", class = "box-title", style = "color: #27ae60;")
                                ),
                                div(class = "box-body",
                                    conditionalPanel(
                                      condition = "output.trends_available",
                                      plotlyOutput("word_trends_plot", height = "600px")
                                    ),
                                    conditionalPanel(
                                      condition = "!output.trends_available",
                                      div(
                                        style = "text-align: center; padding: 60px; color: #999;",
                                        icon("chart-line", style = "font-size: 48px; margin-bottom: 20px;"),
                                        h4("No visualization available", style = "color: #666;"),
                                        p("Select words from the list above and click 'Update Visualization' to see their distribution across the document.", 
                                          style = "font-size: 14px;")
                                      )
                                    )
                                )
                            )
                     )
                   ),
                   
                   # Statistics Table
                   fluidRow(
                     column(12,
                            div(class = "box box-info",
                                div(class = "box-header with-border",
                                    h4("Distribution Statistics", class = "box-title", style = "color: #3498db;")
                                ),
                                div(class = "box-body",
                                    conditionalPanel(
                                      condition = "output.trends_available",
                                      DT::dataTableOutput("word_trends_table")
                                    ),
                                    conditionalPanel(
                                      condition = "!output.trends_available",
                                      div(
                                        style = "text-align: center; padding: 20px; color: #999;",
                                        p("Statistics will appear here after visualization is generated.")
                                      )
                                    )
                                )
                            )
                     )
                   )
                 ),
                 
                 # ===========================================
                 # TAB 2: IN-CONTEXT CITATION ANALYSIS
                 # ===========================================
                 tabPanel(
                   title = "In-Context Citations",
                   value = "tab_contexts",
                   
                   br(),
                   
                   # Search and Filter Controls
                   fluidRow(
                     column(12,
                            div(class = "box box-primary",
                                div(class = "box-header with-border",
                                    h4("In-Context Citation Analysis", class = "box-title", style = "color: #2E86AB;"),
                                    div(class = "box-tools pull-right",
                                        downloadButton("download_contexts",
                                                       "Export Contexts",
                                                       class = "btn btn-primary btn-sm",
                                                       icon = icon("download")
                                        )
                                    )
                                ),
                                div(class = "box-body",
                                    # Filter controls
                                    fluidRow(
                                      column(6,
                                             textInput("context_search",
                                                       "Search in contexts:",
                                                       placeholder = "Type to search citations or context...",
                                                       width = "100%"
                                             )
                                      ),
                                      column(3,
                                             selectInput("context_type_filter",
                                                         "Filter by type:",
                                                         choices = NULL,
                                                         selected = NULL,
                                                         width = "100%"
                                             )
                                      ),
                                      column(3,
                                             numericInput("context_min_words",
                                                          "Min context words:",
                                                          value = 10,
                                                          min = 5,
                                                          max = 100,
                                                          width = "100%"
                                             )
                                      )
                                    ),
                                    fluidRow(
                                      column(4,
                                             radioButtons("citation_segmentation",
                                                          "Group citations by:",
                                                          choices = list(
                                                            "Auto (use sections if available)" = "auto",
                                                            "Document sections" = "sections",
                                                            "Equal-length segments" = "segments"
                                                          ),
                                                          selected = "auto",
                                                          inline = FALSE
                                             )
                                      ),
                                      column(2,
                                             conditionalPanel(
                                               condition = "input.citation_segmentation == 'segments'",
                                               numericInput("n_segments_citations",
                                                            "Number of segments:",
                                                            value = 10,
                                                            min = 5,
                                                            max = 20,
                                                            step = 1
                                               )
                                             )
                                      ),
                                      column(2,
                                             br(),
                                             actionButton("update_citation_grouping",
                                                          "Update Grouping",
                                                          icon = icon("refresh"),
                                                          class = "btn-info btn-sm",
                                                          style = "margin-top: 5px;"
                                             )
                                      ),
                                      column(4,
                                             conditionalPanel(
                                               condition = "input.citation_segmentation == 'sections' && !output.sections_available",
                                               div(
                                                 style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 25px; border-left: 4px solid #ffc107;",
                                                 icon("exclamation-triangle", style = "color: #856404;"),
                                                 span(" Sections not available. Will use segments.",
                                                      style = "color: #856404; margin-left: 8px; font-size: 12px;")
                                               )
                                             ),
                                             conditionalPanel(
                                               condition = "$('html').hasClass('shiny-busy')",
                                               div(
                                                 style = "margin-top: 30px;",
                                                 icon("spinner", class = "fa-spin", style = "color: #3498db;"),
                                                 span("Updating...", style = "color: #3498db; font-style: italic; margin-left: 5px;")
                                               )
                                             )
                                      )
                                    )
                                )
                            )
                     )
                   ),
                   
                   # Citation Contexts Display
                   fluidRow(
                     column(12,
                            div(class = "box box-success",
                                div(class = "box-header with-border",
                                    h4("Citation Contexts Visualization", class = "box-title", style = "color: #27ae60;")
                                ),
                                div(class = "box-body",
                                    uiOutput("citation_contexts_html")
                                )
                            )
                     )
                   )
                 ),
                 
                 # ===========================================
                 # TAB 3: NETWORK ANALYSIS
                 # ===========================================
                 tabPanel(
                   title = "Network Analysis",
                   value = "tab_network",
                   
                   br(),
                   
                   # Network Visualization
                   fluidRow(
                     div(class = "box box-primary",
                         div(class = "box-header with-border",
                             h4("Citation Co-occurrence Network", class = "box-title", style = "color: #2E86AB;"),
                             div(class = "box-tools pull-right",
                                 downloadButton("download_network",
                                                "Export Network",
                                                class = "btn btn-primary btn-sm",
                                                icon = icon("download")
                                 )
                             )
                         ),
                         div(class = "box-body",
                             div(
                               style = "margin-bottom: 15px; padding: 10px; background-color: #f0f8ff; border-radius: 5px;",
                               icon("info-circle", style = "color: #3498db;"),
                               span(" Nodes are colored by paper section. Legend visible on the right side of the network.",
                                    style = "color: #555; font-size: 13px; margin-left: 8px;")
                             ),
                             div(
                               style = "height: 75vh; min-height: 700px; border: 1px solid #ddd; border-radius: 5px;",
                               visNetworkOutput("citation_network", width = "100%", height = "100%")
                             )
                         )
                     )
                   ),
                   fluidRow(
                     column(6,
                            div(class = "box box-info",
                                div(class = "box-header with-border",
                                    h4("Network Information", class = "box-title", style = "color: #3498db;")
                                ),
                                div(class = "box-body",
                                    verbatimTextOutput("network_info")
                                )
                            )
                     ),
                     column(6,
                            div(class = "box box-success",
                                div(class = "box-header with-border",
                                    h4("Strongest Connections", class = "box-title", style = "color: #27ae60;")
                                ),
                                div(class = "box-body",
                                    DT::dataTableOutput("strongest_connections")
                                )
                            )
                     )
                   )
                 )
               )
             ),
             
             # Placeholder when no analysis is done
             conditionalPanel(
               condition = "!output.analysis_completed && !output.text_extracted",
               div(
                 style = "text-align: center; margin-top: 100px; color: #999;",
                 icon("file-upload", style = "font-size: 48px; margin-bottom: 20px;"),
                 h3("Upload a PDF file and start the analysis", style = "color: #666;"),
                 p("Select a scientific article in PDF format and configure the analysis parameters to begin.", 
                   style = "font-size: 16px;")
               )
             )
      ),
      
      # ===========================================
      # LEFT PANEL: CONTROLS (column 2)
      # ===========================================
      column(2,
             
             # PDF Import Card
             div(class = "box box-primary",
                 div(class = "box-header with-border",
                     h4("1. Import PDF File", class = "box-title", style = "color: #2E86AB;")
                 ),
                 div(class = "box-body",
                     fileInput("pdf_file",
                               label = "Choose PDF File",
                               accept = c(".pdf"),
                               buttonLabel = "Browse...",
                               placeholder = "No file selected",
                               width = "100%"
                     ),
                     numericInput("Columns",
                                  label = "Number of Columns in PDF",
                                  value = NULL,
                                  min = 1,
                                  max = 3,
                                  step = 1,
                                  width = "100%"
                     ),
                     helpText("Specify if the PDF has multiple columns (e.g., 2 for typical
                     academic articles)."),
                     # File info display
                     conditionalPanel(
                       condition = "output.pdf_uploaded",
                       div(
                         style = "background-color: #f0f8ff; padding: 10px; border-radius: 5px; margin-top: 10px;",
                         icon("file-pdf", style = "color: #e74c3c;"),
                         span(" PDF uploaded successfully!", style = "color: #27ae60; font-weight: bold;"),
                         br(),
                         textOutput("pdf_info", inline = TRUE)
                       )
                     ),
                     
                     br(),
                     actionButton("extract_text",
                                  "Extract Text from PDF",
                                  icon = icon("file-text"),
                                  class = "btn-info btn-block",
                                  style = "margin-top: 10px;"
                     )
                 )
             ),
             
             # Analysis Parameters Card
             div(class = "box box-success",
                 div(class = "box-header with-border",
                     h4("2. Analysis Parameters", class = "box-title", style = "color: #27ae60;")
                 ),
                 div(class = "box-body",
                     
                     # Citation context window
                     numericInput("window_size",
                                  label = "Context Window Size (words)",
                                  value = 20,
                                  min = 5,
                                  max = 50,
                                  step = 1,
                                  width = "100%"
                     ),
                     helpText("Number of words before and after each citation to extract."),
                     
                     # Maximum distance for network
                     numericInput("max_distance",
                                  label = "Max Distance for Network (chars)",
                                  value = 800,
                                  min = 200,
                                  max = 2000,
                                  step = 100,
                                  width = "100%"
                     ),
                     helpText("Maximum character distance between citations to consider as connected."),
                     
                     # Advanced options
                     tags$details(
                       tags$summary("Advanced Options", style = "font-weight: bold; cursor: pointer;"),
                       br(),
                       checkboxInput("parse_multiple",
                                     "Parse complex multiple citations",
                                     value = TRUE
                       ),
                       checkboxInput("remove_stopwords",
                                     "Remove stopwords from analysis",
                                     value = TRUE
                       ),
                       textInput("custom_stopwords",
                                 "Custom stopwords (comma-separated)",
                                 value = "machine, learning, random, forest, model",
                                 width = "100%"
                       )
                     )
                 )
             ),
             
             # Analysis Button
             div(class = "box box-warning",
                 div(class = "box-header with-border",
                     h4("3. Run Analysis", class = "box-title", style = "color: #f39c12;")
                 ),
                 div(class = "box-body",
                     actionButton("run_analysis",
                                  "Start",
                                  icon = icon("chart-line"),
                                  class = "btn-warning btn-block btn-lg",
                                  style = "font-weight: bold; margin-bottom: 10px;"
                     ),
                     
                     # Progress indicator
                     conditionalPanel(
                       condition = "$('html').hasClass('shiny-busy')",
                       div(
                         style = "text-align: center; margin-top: 15px;",
                         icon("spinner", class = "fa-spin", style = "color: #f39c12;"),
                         br(),
                         span("Analyzing content...", style = "color: #f39c12; font-style: italic;")
                       )
                     ),
                     
                     # Reset button
                     actionButton("reset_analysis",
                                  "Reset",
                                  icon = icon("refresh"),
                                  class = "btn-default btn-block",
                                  style = "margin-top: 10px;"
                     )
                 )
             )
      )
    ),
    # Modal per Preview (Bootstrap nativo)
    tags$div(
      class = "modal fade",
      id = "previewModal",
      tabindex = "-1",
      role = "dialog",
      
      tags$div(
        class = "modal-dialog modal-lg",
        role = "document",
        style = "width: 90%; max-width: 1200px;",
        
        tags$div(
          class = "modal-content",
          
          # Header
          tags$div(
            class = "modal-header",
            tags$button(
              type = "button",
              class = "close",
              `data-dismiss` = "modal",
              `aria-label` = "Close",
              tags$span(`aria-hidden` = "true", HTML("&times;"))
            ),
            tags$h4(class = "modal-title", "Text & PDF Preview")
          ),
          
          # Body
          tags$div(
            class = "modal-body",
            style = "max-height: 75vh; overflow-y: auto;",
            
            tabsetPanel(
              id = "modal_preview_tabs",
              
              tabPanel(
                "Extracted Text",
                br(),
                div(
                  style = "margin-bottom: 10px;",
                  span(textOutput("text_length_info", inline = TRUE), 
                       style = "font-size: 12px; color: #666;")
                ),
                div(
                  style = paste0(
                    "background-color: #f8f9fa; padding: 15px; ",
                    "border: 1px solid #e9ecef; border-radius: 4px; ",
                    "font-family: 'Consolas', 'Monaco', monospace; ",
                    "font-size: 13px; line-height: 1.4; ",
                    "white-space: pre-wrap; word-wrap: break-word;"
                  ),
                  textOutput("text_preview")
                )
              ),
              
              tabPanel(
                "PDF Preview",
                br(),
                uiOutput("pdf_viewer")
              )
            )
          ),
          
          # Footer
          tags$div(
            class = "modal-footer",
            tags$button(
              type = "button",
              class = "btn btn-default",
              `data-dismiss` = "modal",
              "Close"
            )
          )
        )
      )
    )
  )
}

create_citation_network_basic <- function(citation_analysis_results,
                                          max_distance = 1000,
                                          min_connections = 1,
                                          show_labels = TRUE) {
  
  network_data <- citation_analysis_results$network_data
  
  if (is.null(network_data) || nrow(network_data) == 0) {
    warning("No citation co-occurrence data found.")
    return(NULL)
  }
  
  # Filter by distance
  network_data_filtered <- network_data %>%
    filter(abs(distance) <= max_distance)
  
  if (nrow(network_data_filtered) == 0) {
    warning("No citation pairs found within the specified maximum distance.")
    return(NULL)
  }
  
  # Get unique citations
  all_citation_texts <- unique(c(network_data_filtered$citation1, network_data_filtered$citation2))
  
  # Ottieni informazioni sulle sezioni - AGGREGA per citazione
  citations_with_sections <- citation_analysis_results$citations %>%
    select(citation_text_clean, section) %>%
    group_by(citation_text_clean) %>%
    summarise(
      sections = paste(unique(section), collapse = ", "),
      n_sections = n_distinct(section),
      primary_section = first(section),
      .groups = "drop"
    )
  
  # Create nodes
  nodes <- data.frame(
    id = 1:length(all_citation_texts),
    citation_text = all_citation_texts,
    label = if (show_labels) str_trunc(all_citation_texts, 25) else "",
    stringsAsFactors = FALSE
  )
  
  # Aggiungi informazioni sulla sezione ai nodi
  nodes <- nodes %>%
    left_join(
      citations_with_sections,
      by = c("citation_text" = "citation_text_clean")
    ) %>%
    mutate(
      sections = replace_na(sections, "Unknown"),
      primary_section = replace_na(primary_section, "Unknown"),
      n_sections = replace_na(n_sections, 1)
    )
  
  # Calculate connections
  node_connections <- rbind(
    data.frame(citation = network_data_filtered$citation1, stringsAsFactors = FALSE),
    data.frame(citation = network_data_filtered$citation2, stringsAsFactors = FALSE)
  ) %>%
    count(citation, name = "connections")
  
  nodes$connections <- sapply(nodes$citation_text, function(cite) {
    conn <- node_connections$connections[node_connections$citation == cite]
    if (length(conn) == 0) return(0)
    return(conn[1])
  })
  
  # Filter by connections
  nodes <- nodes[nodes$connections >= min_connections, ]
  valid_citations <- nodes$citation_text
  network_data_filtered <- network_data_filtered %>%
    filter(citation1 %in% valid_citations & citation2 %in% valid_citations)
  
  if (nrow(network_data_filtered) == 0) {
    warning("No valid connections after filtering.")
    return(NULL)
  }
  
  # Set node properties
  nodes$size <- pmax(15, pmin(40, 15 + nodes$connections * 3))/2
  
  # Assegna colori dinamicamente usando colorlist()
  # unique_sections <- unique(nodes$primary_section)
  # colors <- colorlist()
  # 
  # # Crea mapping sezione -> colore
  # section_colors <- setNames(
  #   colors[((seq_along(unique_sections) - 1) %% length(colors)) + 1],
  #   unique_sections
  # )
  section_colors <- citation_analysis_results$section_colors
  
  # Assicurati che "Unknown" abbia sempre un colore grigio
  if ("Unknown" %in% names(section_colors)) {
    section_colors["Unknown"] <- "#CCCCCC"
  }
  
  nodes$group <- nodes$primary_section
  
  # Aggiungi trasparenza ai colori dei nodi (85% opacità)
  nodes$color <- sapply(section_colors[nodes$primary_section], function(hex_color) {
    # Converti hex in rgba con trasparenza
    rgb_vals <- col2rgb(hex_color)
    sprintf("rgba(%d, %d, %d, 0.85)", rgb_vals[1], rgb_vals[2], rgb_vals[3])
  })
  
  # Aggiungi bordo speciale per nodi multi-sezione
  nodes$borderWidth <- ifelse(nodes$n_sections > 1, 3, 1)
  nodes$borderWidthSelected <- ifelse(nodes$n_sections > 1, 5, 2)
  
  # Crea title con informazioni complete
  nodes$title <- paste0(
    nodes$citation_text,
    "\n<br><b>Section(s):</b> ", nodes$sections,
    ifelse(nodes$n_sections > 1, 
           paste0(" (", nodes$n_sections, " sections)"), 
           ""),
    "\n<br><b>Connections:</b> ", nodes$connections
  )
  
  nodes <- nodes %>% 
    mutate(font.size = size,
           font.vadjust = -0.7 * font.size)
  
  # Create edges
  edges <- data.frame(
    from = sapply(network_data_filtered$citation1, function(cite) {
      nodes$id[nodes$citation_text == cite][1]
    }),
    to = sapply(network_data_filtered$citation2, function(cite) {
      nodes$id[nodes$citation_text == cite][1]
    }),
    distance = abs(network_data_filtered$distance),
    stringsAsFactors = FALSE
  )
  
  edges <- edges[!is.na(edges$from) & !is.na(edges$to), ]
  
  # Riduci lo spessore degli archi e aggiungi trasparenza
  edges$width <- pmax(0.5, 3 - (edges$distance / 200))
  
  # Colori con trasparenza (30% opacità per maggiore trasparenza)
  edges$color <- ifelse(edges$distance <= 300, "rgba(255, 111, 111, 0.3)", 
                        ifelse(edges$distance <= 600, "rgba(127, 179, 213, 0.3)", 
                               "rgba(204, 204, 204, 0.25)"))
  
  edges$title <- paste("Distance:", edges$distance, "characters")
  
  # Create network con layout Fruchterman-Reingold
  network <- visNetwork(nodes, edges, type="full", smooth = TRUE, physics = FALSE) %>%
    visIgraphLayout(layout = "layout_nicely", type = "full")
  
  # Configure options
  network <- network %>%
    visOptions(highlightNearest = TRUE) %>%
    visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE, zoomSpeed = 0.2) %>%
    visPhysics(enabled = FALSE) %>%
    visNodes(
      borderWidth = 1,
      borderWidthSelected = 2
    )
  
  # Add stats
  n_nodes <- nrow(nodes)
  n_edges <- nrow(edges)
  avg_distance <- round(mean(edges$distance), 1)
  
  # Statistiche per sezione e multi-sezione
  section_stats <- nodes %>%
    count(primary_section) %>%
    arrange(desc(n))
  
  multi_section_citations <- nodes %>%
    filter(n_sections > 1) %>%
    select(citation_text, sections, n_sections)
  
  attr(network, "stats") <- list(
    n_nodes = n_nodes,
    n_edges = n_edges,
    avg_distance = avg_distance,
    max_distance = max_distance,
    section_distribution = section_stats,
    multi_section_citations = multi_section_citations,
    section_colors = section_colors
  )
  
  return(network)
}

#' Enhanced Server Logic for Content Analysis Tab with 3 Tabs
#' 
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
#' @param values Reactive values object

#' Enhanced Server Logic for Content Analysis Tab with 3 Tabs
#' 
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
#' @param values Reactive values object

content_analysis_server <- function(input, output, session, values) {
  
  # Helper function for null coalescing
  `%||%` <- function(lhs, rhs) {
    if (!is.null(lhs) && length(lhs) > 0) lhs else rhs
  }
  
  # Cartella temporanea per i PDF
  tmpdir <- tempdir()
  
  # Rendi la cartella temporanea accessibile via HTTP
  addResourcePath("tmpPDF", tmpdir)
  
  # Preview visibility reactive value
  preview_visible <- reactiveVal(TRUE)
  
  # Check if text is extracted
  output$text_extracted <- reactive({
    return(!is.null(values$pdf_text) && nchar(values$pdf_text) > 0)
  })
  outputOptions(output, "text_extracted", suspendWhenHidden = FALSE)
  
  # Preview visibility reactive output
  output$preview_visible <- reactive({
    return(preview_visible())
  })
  outputOptions(output, "preview_visible", suspendWhenHidden = FALSE)
  
  # ===========================================
  # PDF UPLOAD AND TEXT EXTRACTION
  # ===========================================
  
  # Check if PDF is uploaded
  output$pdf_uploaded <- reactive({
    return(!is.null(input$pdf_file))
  })
  outputOptions(output, "pdf_uploaded", suspendWhenHidden = FALSE)
  
  # Display PDF info
  output$pdf_info <- renderText({
    if (!is.null(input$pdf_file)) {
      file_size <- round(input$pdf_file$size / 1024 / 1024, 2)
      paste("File:", input$pdf_file$name, "| Size:", file_size, "MB")
    }
  })
  
  ## PDF PREVIEW
  output$pdf_viewer <- renderUI({
    req(input$pdf_file)
    
    # Verifica che sia un PDF
    if (!grepl("\\.pdf$", input$pdf_file$name, ignore.case = TRUE)) {
      return(
        div(
          style = "color: red; padding: 20px;",
          "Errore: Il file selezionato non è un PDF"
        )
      )
    }
    
    # Copia il file nella cartella temporanea con nome univoco
    pdf_name <- paste0("viewer_", 
                       format(Sys.time(), "%Y%m%d_%H%M%S"), 
                       "_", 
                       input$pdf_file$name)
    pdf_path <- file.path(tmpdir, pdf_name)
    file.copy(input$pdf_file$datapath, pdf_path, overwrite = TRUE)
    
    # Crea l'iframe che punta al file tramite il path accessibile
    tags$iframe(
      src = paste0("tmpPDF/", pdf_name),
      width = "100%",
      height = "800px",
      style = "border: 1px solid #ddd;"
    )
  })
  
  # Pulizia alla chiusura della sessione
  onSessionEnded(function() {
    files <- list.files(tmpdir, pattern = "^viewer_.*\\.pdf$", full.names = TRUE)
    file.remove(files)
  })
  
  ## END PDF PREVIEW
  
  # Extract text from PDF
  observeEvent(input$extract_text, {
    req(input$pdf_file)
    
    tryCatch({
      if (is.na(input$Columns)) {
        n_columns <- NULL
      } else {
        n_columns <- input$Columns
      }
      
      pdf_text <- pdf2txt_auto(input$pdf_file$datapath, n_columns = n_columns)
      values$pdf_text <- pdf_text$Full_text
      values$pdf_sections <- pdf_text[-1]
      pdf_metadata <- unlist(pdftools::pdf_info(input$pdf_file$datapath))
      
      # Extract DOI
      extracted_doi <- tryCatch({
        extract_doi_from_pdf(input$pdf_file$datapath)
      }, error = function(e) {
        NULL
      })
      
      # Store DOI and update input field
      if (!is.null(extracted_doi) && !is.na(extracted_doi) && nzchar(trimws(extracted_doi))) {
        values$pdf_doi <- trimws(extracted_doi)
        updateTextInput(session, "pdf_doi_input", value = trimws(extracted_doi))
      } else {
        values$pdf_doi <- ""
        updateTextInput(session, "pdf_doi_input", value = "")
      }
      
      showNotification(
        if (!is.null(extracted_doi) && nzchar(trimws(extracted_doi))) {
          "PDF text extracted successfully! DOI detected and populated."
        } else {
          "PDF text extracted successfully! Please enter DOI manually if needed."
        },
        type = "message",
        duration = 4
      )
      
      updateActionButton(session, "run_analysis", 
                         label = "Start",
                         icon = icon("play"))
      
    }, error = function(e) {
      showNotification(
        paste("Error extracting PDF text:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # Check if DOI was detected automatically
  output$doi_detected <- reactive({
    if (!is.null(values$pdf_doi) && nzchar(values$pdf_doi)) {
      # Check if it's the same as what was automatically extracted
      extracted_doi <- tryCatch({
        if (!is.null(input$pdf_file)) {
          extract_doi_from_pdf(input$pdf_file$datapath)
        } else {
          NULL
        }
      }, error = function(e) NULL)
      
      return(!is.null(extracted_doi) && 
               nzchar(extracted_doi) && 
               extracted_doi == input$pdf_doi_input)
    }
    return(FALSE)
  })
  
  outputOptions(output, "doi_detected", suspendWhenHidden = FALSE)
  
  # Display DOI after analysis
  output$doi_display_after_analysis <- renderText({
    doi_value <- values$pdf_doi
    
    if (!is.null(doi_value) && !is.na(doi_value) && nzchar(trimws(doi_value))) {
      return(trimws(doi_value))
    } else {
      return("Not available")
    }
  })
  
  # DOI status icon
  output$doi_status_icon <- renderUI({
    
    # Verifica se il campo DOI ha un valore
    doi_value <- input$pdf_doi_input
    
    if (is.null(doi_value) || !nzchar(trimws(doi_value))) {
      # Nessun DOI inserito
      return(
        tagList(
          icon("exclamation-triangle", style = "color: #e74c3c; font-size: 18px; margin-right: 5px;"),
          tags$small("DOI not found", style = "color: #e74c3c; font-weight: bold;")
        )
      )
    }
    
    # C'è un DOI, verifica se è stato auto-rilevato
    auto_detected <- FALSE
    
    if (!is.null(input$pdf_file)) {
      extracted_doi <- tryCatch({
        extract_doi_from_pdf(input$pdf_file$datapath)
      }, error = function(e) {
        NULL
      })
      
      # Verifica se il DOI estratto corrisponde a quello inserito
      if (!is.null(extracted_doi) && 
          !is.na(extracted_doi) && 
          nzchar(trimws(extracted_doi))) {
        auto_detected <- (trimws(extracted_doi) == trimws(doi_value))
      }
    }
    
    # Restituisci l'icona appropriata
    if (auto_detected) {
      return(
        tagList(
          icon("check-circle", style = "color: #27ae60; font-size: 18px; margin-right: 5px;"),
          tags$small("Auto-detected", style = "color: #27ae60; font-weight: bold;")
        )
      )
    } else {
      return(
        tagList(
          icon("edit", style = "color: #f39c12; font-size: 18px; margin-right: 5px;"),
          tags$small("Manually entered", style = "color: #f39c12; font-weight: bold;")
        )
      )
    }
  })
  
  # Update DOI value when user edits the field
  observeEvent(input$pdf_doi_input, {
    if (!is.null(input$pdf_doi_input)) {
      values$pdf_doi <- trimws(input$pdf_doi_input)
    }
  }, ignoreInit = TRUE)
  
  # Text length information
  output$text_length_info <- renderText({
    if (!is.null(values$pdf_text)) {
      char_count <- nchar(values$pdf_text)
      word_count <- length(strsplit(values$pdf_text, "\\s+")[[1]])
      paste("Characters:", format(char_count, big.mark = ","), 
            "| Words:", format(word_count, big.mark = ","))
    }
  })
  
  # Text preview output
  output$text_preview <- renderText({
    if (!is.null(values$pdf_text)) {
      preview_length <- 80000
      full_text <- values$pdf_text
      
      if (nchar(full_text) > preview_length) {
        truncated <- substr(full_text, 1, preview_length)
        last_period <- max(c(
          regexpr("\\. [A-Z]", truncated, perl = TRUE),
          regexpr("\\.\n", truncated, perl = TRUE),
          regexpr("\\?\n", truncated, perl = TRUE),
          regexpr("!\n", truncated, perl = TRUE)
        ))
        
        if (last_period > 100) {
          truncated <- substr(truncated, 1, last_period)
        }
        
        paste0(truncated, "\n\n[...text continues for ", 
               format(nchar(full_text) - nchar(truncated), big.mark = ","), 
               " more characters...]")
      } else {
        full_text
      }
    }
  })
  
  # Toggle preview visibility
  observeEvent(input$toggle_preview, {
    preview_visible(!preview_visible())
    if (preview_visible()) {
      updateActionButton(session, "toggle_preview", "Hide Preview")
    } else {
      updateActionButton(session, "toggle_preview", "Show Preview")
    }
  })
  
  # ===========================================
  # CONTENT ANALYSIS
  # ===========================================
  
  observeEvent(input$run_analysis, {
    req(values$pdf_text)
    
    updateActionButton(session, "run_analysis", 
                       label = "Processing...",
                       icon = icon("spinner", class = "fa-spin"))
    
    tryCatch({
      custom_stops <- NULL
      if (!is.null(input$custom_stopwords) && nzchar(input$custom_stopwords)) {
        custom_stops <- trimws(strsplit(input$custom_stopwords, ",")[[1]])
      }
      
      # Determine citation segmentation
      use_sections_cit <- switch(
        input$citation_segmentation %||% "auto",
        "auto" = "auto",
        "sections" = TRUE,
        "segments" = FALSE,
        "auto"  # default fallback
      )
      
      n_segs_cit <- input$n_segments_citations %||% 10
      
      values$analysis_results <- analyze_scientific_content(
        text = values$pdf_sections,
        doi = values$pdf_doi,
        window_size = input$window_size,
        parse_multiple_citations = input$parse_multiple,
        remove_stopwords = input$remove_stopwords,
        custom_stopwords = custom_stops,
        use_sections_for_citations = use_sections_cit,
        n_segments_citations = n_segs_cit
      )
      
      section_colors <- colorlist()[1:length(unique(values$analysis_results$citation_contexts$section))]
      names(section_colors) <- unique(values$analysis_results$citation_contexts$section)
      values$analysis_results$section_colors <- section_colors
      
      # Calculate readability indices
      values$readability_indices <- calculate_readability_indices(
        text = values$pdf_text,
        detailed = TRUE
      )
      
      # Create network if we have data
      if (!is.null(values$analysis_results$network_data) && 
          nrow(values$analysis_results$network_data) > 0) {
        
        values$network_plot <- create_citation_network_basic(
          values$analysis_results,
          max_distance = input$max_distance,
          show_labels = TRUE
        )
      }
      
      preview_visible(FALSE)
      
      updateActionButton(session, "run_analysis", 
                         label = "Start",
                         icon = icon("play"))
      
      showNotification(
        "Content analysis completed successfully!",
        type = "message",
        duration = 3
      )
      
    }, error = function(e) {
      updateActionButton(session, "run_analysis", 
                         label = "Start",
                         icon = icon("play"))
      
      showNotification(
        paste("Error in content analysis:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # Check if analysis is completed
  output$analysis_completed <- reactive({
    return(!is.null(values$analysis_results))
  })
  outputOptions(output, "analysis_completed", suspendWhenHidden = FALSE)
  
  # ===========================================
  # TAB 1: DESCRIPTIVE STATISTICS OUTPUTS
  # ===========================================
  
  output$total_words <- renderText({
    if (!is.null(values$analysis_results)) {
      format(values$analysis_results$summary$total_words_analyzed, big.mark = ",")
    } else { "0" }
  })
  
  output$total_citations <- renderText({
    if (!is.null(values$analysis_results)) {
      format(values$analysis_results$summary$citations_extracted, big.mark = ",")
    } else { "0" }
  })
  
  output$narrative_citations <- renderText({
    if (!is.null(values$analysis_results)) {
      format(values$analysis_results$summary$narrative_citations, big.mark = ",")
    } else { "0" }
  })
  
  output$citation_density <- renderText({
    if (!is.null(values$analysis_results)) {
      format(values$analysis_results$summary$citation_density_per_1000_words, digits = 1)
    } else { "0.0" }
  })
  
  # Readability indices outputs
  output$flesch_kincaid_grade <- renderText({
    if (!is.null(values$readability_indices)) {
      format(round(values$readability_indices$flesch_kincaid_grade, 1), nsmall = 1)
    } else { "0.0" }
  })
  
  output$flesch_reading_ease <- renderText({
    if (!is.null(values$readability_indices)) {
      format(round(values$readability_indices$flesch_reading_ease, 1), nsmall = 1)
    } else { "0.0" }
  })
  
  output$ari_index <- renderText({
    if (!is.null(values$readability_indices)) {
      format(round(values$readability_indices$automated_readability_index, 1), nsmall = 1)
    } else { "0.0" }
  })
  
  output$gunning_fog_index <- renderText({
    if (!is.null(values$readability_indices)) {
      format(round(values$readability_indices$gunning_fog_index, 1), nsmall = 1)
    } else { "0.0" }
  })
  
  # Text Statistics - Matching height with readability
  # Text Statistics - Matching structure with readability
  output$text_stats <- renderUI({
    if (!is.null(values$analysis_results)) {
      stats <- values$analysis_results$text_analytics$basic_stats
      
      # Get additional stats from readability indices
      syllables_info <- ""
      complex_info <- ""
      
      if (!is.null(values$readability_indices)) {
        syllables_info <- format(values$readability_indices$n_syllables, big.mark = ",")
        complex_info <- sprintf("Complex words:      %s (%.1f%%)",
                                format(values$readability_indices$n_complex_words, big.mark = ","),
                                values$readability_indices$pct_complex_words)
      } else {
        syllables_info <- "N/A"
        complex_info <- "Complex words:      N/A"
      }
      
      text_content <- sprintf(
        "TEXT STATISTICS
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

🔤 Characters:      %s
📄 Words:           %s
✏️ Sentences:       %s
🗣 Syllables:       %s

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

%s
Avg words/sentence: %.1f
Lexical diversity:  %.3f",
        format(stats$total_characters, big.mark = ","),
        format(stats$total_words, big.mark = ","),
        format(stats$total_sentences, big.mark = ","),
        syllables_info,
        complex_info,
        round(stats$avg_words_per_sentence, 1),
        round(values$analysis_results$summary$lexical_diversity, 3)
      )
      
      tags$pre(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 6px; border: 1px solid #dee2e6; font-family: 'Courier New', monospace; font-size: 13px; line-height: 1.5; color: #333; margin: 0; min-height: 280px;",
        text_content
      )
    } else {
      tags$pre(
        style = "background-color: #f8f9fa; padding: 40px; border-radius: 6px; border: 1px solid #dee2e6; text-align: center; color: #999; margin: 0; min-height: 280px; display: flex; align-items: center; justify-content: center;",
        "No analysis data available"
      )
    }
  })
  
  # Readability Indices - Add min-height
  output$readability_details_html <- renderUI({
    if (!is.null(values$readability_indices)) {
      
      indices <- values$readability_indices
      
      # Helper to get interpretation symbol
      get_symbol <- function(index_name, value) {
        switch(index_name,
               "flesch_kincaid" = if (value <= 12) "✓" else if (value <= 16) "◆" else "▲",
               "flesch_ease" = if (value >= 60) "✓" else if (value >= 30) "◆" else "▲",
               "ari" = if (value <= 12) "✓" else if (value <= 16) "◆" else "▲",
               "gunning" = if (value <= 12) "✓" else if (value <= 16) "◆" else "▲"
        )
      }
      
      # Helper to get interpretation text
      get_interp_text <- function(index_name, value) {
        switch(index_name,
               "flesch_kincaid" = paste("Grade", round(value)),
               "flesch_ease" = if (value >= 60) "Easy" else if (value >= 30) "Moderate" else "Difficult",
               "ari" = paste("Grade", round(value)),
               "gunning" = if (value <= 12) "Readable" else if (value <= 16) "Difficult" else "Very difficult"
        )
      }
      
      text_content <- sprintf(
        "READABILITY INDICES
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

📚 Flesch-Kincaid:  %5.1f  %s  %s
📖 Reading Ease:    %5.1f  %s  %s  
🤖 ARI Index:       %5.1f  %s  %s
☁️ Gunning Fog:     %5.1f  %s  %s

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Sentences: %s • Words: %s
Syllables: %s • Complex: %s (%.1f%%)
Avg sentence length: %.1f words",
        indices$flesch_kincaid_grade,
        get_symbol("flesch_kincaid", indices$flesch_kincaid_grade),
        get_interp_text("flesch_kincaid", indices$flesch_kincaid_grade),
        indices$flesch_reading_ease,
        get_symbol("flesch_ease", indices$flesch_reading_ease),
        get_interp_text("flesch_ease", indices$flesch_reading_ease),
        indices$automated_readability_index,
        get_symbol("ari", indices$automated_readability_index),
        get_interp_text("ari", indices$automated_readability_index),
        indices$gunning_fog_index,
        get_symbol("gunning", indices$gunning_fog_index),
        get_interp_text("gunning", indices$gunning_fog_index),
        format(indices$n_sentences, big.mark = ","),
        format(indices$n_words, big.mark = ","),
        format(indices$n_syllables, big.mark = ","),
        format(indices$n_complex_words, big.mark = ","),
        indices$pct_complex_words,
        indices$avg_sentence_length
      )
      
      tags$pre(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 6px; border: 1px solid #dee2e6; font-family: 'Courier New', monospace; font-size: 13px; line-height: 1.5; color: #333; margin: 0; min-height: 280px;",
        text_content
      )
      
    } else {
      tags$pre(
        style = "background-color: #f8f9fa; padding: 40px; border-radius: 6px; border: 1px solid #dee2e6; text-align: center; color: #999; margin: 0; min-height: 280px; display: flex; align-items: center; justify-content: center;",
        "No readability data available"
      )
    }
  })
  
  # Citation types table
  output$citation_types_table <- DT::renderDataTable({
    if (!is.null(values$analysis_results) && 
        !is.null(values$analysis_results$citation_metrics$type_distribution)) {
      values$analysis_results$citation_metrics$type_distribution
    } else {
      data.frame(citation_type = character(0), n = numeric(0), percentage = numeric(0))
    }
  }, options = list(pageLength = 10, dom = 't', ordering = FALSE, searching = FALSE))
  
  # Citation sections table
  output$citation_sections_table <- DT::renderDataTable({
    if (!is.null(values$analysis_results) && 
        !is.null(values$analysis_results$citation_metrics$section_distribution)) {
      values$analysis_results$citation_metrics$section_distribution %>% filter(n > 0)
    } else {
      data.frame(section = character(0), n = numeric(0), percentage = numeric(0))
    }
  }, options = list(pageLength = 10, dom = 't', ordering = FALSE, searching = FALSE))
  
  # Frequent words table
  output$frequent_words_table <- DT::renderDataTable({
    if (!is.null(values$analysis_results)) {
      values$analysis_results$word_frequencies %>% 
        slice_head(n = 15) %>%
        select(word, n)
    } else {
      data.frame(word = character(0), n = numeric(0))
    }
  }, options = list(pageLength = 15, dom = 't', ordering = FALSE, searching = FALSE))
  
  # Bigrams table
  output$bigrams_table <- DT::renderDataTable({
    if (!is.null(values$analysis_results) && 
        "2gram" %in% names(values$analysis_results$ngrams)) {
      values$analysis_results$ngrams$`2gram` %>%
        select(ngram, n) %>%
        slice_head(n = 15)
    } else {
      data.frame(ngram = character(0), n = numeric(0))
    }
  }, options = list(pageLength = 15, dom = 't', ordering = FALSE, searching = FALSE))
  
  # Trigrams table
  output$trigrams_table <- DT::renderDataTable({
    if (!is.null(values$analysis_results) && 
        "3gram" %in% names(values$analysis_results$ngrams)) {
      values$analysis_results$ngrams$`3gram` %>%
        select(ngram, n) %>%
        slice_head(n = 15)
    } else {
      data.frame(ngram = character(0), n = numeric(0))
    }
  }, options = list(pageLength = 15, dom = 't', ordering = FALSE, searching = FALSE))
  
  # Text statistics
  # output$text_stats <- renderText({
  #   if (!is.null(values$analysis_results)) {
  #     stats <- values$analysis_results$text_analytics$basic_stats
  #     paste(
  #       "Characters:", format(stats$total_characters, big.mark = ","), "\n",
  #       "Words:", format(stats$total_words, big.mark = ","), "\n",
  #       "Sentences:", format(stats$total_sentences, big.mark = ","), "\n",
  #       "Avg words/sentence:", round(stats$avg_words_per_sentence, 1), "\n",
  #       "Lexical diversity:", round(values$analysis_results$summary$lexical_diversity, 3)
  #     )
  #   } else {
  #     "No analysis data available"
  #   }
  # })
  
  # ===========================================
  # TAB 2: IN-CONTEXT CITATION ANALYSIS
  # ===========================================
  
  # ===========================================
  # UPDATE CITATION GROUPING (without re-running full analysis)
  # ===========================================
  
  observeEvent(input$update_citation_grouping, {
    
    req(values$pdf_text)
    req(values$analysis_results)
    
    tryCatch({
      
      # Determine citation segmentation
      use_sections_cit <- switch(
        input$citation_segmentation %||% "auto",
        "auto" = "auto",
        "sections" = TRUE,
        "segments" = FALSE,
        "auto"
      )
      
      n_segs_cit <- input$n_segments_citations %||% 10
      
      # Get citations without section column
      citations_for_mapping <- values$analysis_results$citations %>%
        select(-section, -segment_type)
      
      # Re-map citations to new segments/sections
      citations_remapped <- map_citations_to_segments(
        citations_df = citations_for_mapping,
        text = values$pdf_sections,
        use_sections = use_sections_cit,
        n_segments = n_segs_cit
      )
      
      # Update the citations in analysis_results
      values$analysis_results$citations <- citations_remapped %>%
        rename(section = segment)
      
      # Update citation contexts with new section info
      if (!is.null(values$analysis_results$citation_contexts)) {
        # Remove old section column if exists
        if ("section" %in% names(values$analysis_results$citation_contexts)) {
          values$analysis_results$citation_contexts <- values$analysis_results$citation_contexts %>%
            select(-section)
        }
        
        # Add new section mapping
        values$analysis_results$citation_contexts <- values$analysis_results$citation_contexts %>%
          left_join(
            values$analysis_results$citations %>% 
              select(citation_id, section),
            by = "citation_id"
          )
        section_colors <- colorlist()[1:length(unique(values$analysis_results$citation_contexts$section))]
        names(section_colors) <- unique(values$analysis_results$citation_contexts$section)
        values$analysis_results$section_colors <- section_colors
      }
      
      # Update citation metrics - section distribution
      if (!is.null(values$analysis_results$citation_metrics)) {
        
        # Determine expected sections/segments
        sections_to_use <- NULL
        
        if (use_sections_cit == TRUE || 
            (use_sections_cit == "auto" && 
             length(setdiff(names(values$pdf_sections), c("Full_text", "References"))) > 0)) {
          # Using sections
          sections_to_use <- setdiff(names(values$pdf_sections), c("Full_text", "References"))
        } else {
          # Using segments
          sections_to_use <- paste0("Segment ", 1:n_segs_cit)
        }
        
        # Recalculate section distribution
        if (!is.null(sections_to_use)) {
          values$analysis_results$citation_metrics$section_distribution <- 
            values$analysis_results$citations %>%
            mutate(section = factor(section, levels = sections_to_use)) %>%
            count(section, sort = FALSE, .drop = FALSE) %>%
            mutate(percentage = round(n / sum(n) * 100, 2))
        } else {
          values$analysis_results$citation_metrics$section_distribution <- 
            values$analysis_results$citations %>%
            count(section, sort = TRUE) %>%
            mutate(percentage = round(n / sum(n) * 100, 2))
        }
      }
      
      # Recreate network with new grouping
      if (!is.null(values$analysis_results$network_data) && 
          nrow(values$analysis_results$network_data) > 0) {
        
        values$network_plot <- create_citation_network_basic(
          values$analysis_results,
          max_distance = input$max_distance,
          show_labels = TRUE
        )
      }
      
      showNotification(
        "Citation grouping updated successfully!",
        type = "message",
        duration = 2
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error updating citation grouping:", e$message),
        type = "error",
        duration = 5
      )
      
      # Print error to console for debugging
      cat("Error in update_citation_grouping:\n")
      print(e)
    })
  })
  
  # Update citation type filter
  observe({
    if (!is.null(values$analysis_results) && 
        !is.null(values$analysis_results$citation_contexts) &&
        nrow(values$analysis_results$citation_contexts) > 0) {
      
      types <- unique(values$analysis_results$citation_contexts$citation_type)
      updateSelectInput(session, "context_type_filter",
                        choices = c("All" = "", types),
                        selected = "")
    }
  })
  
  # Custom HTML output for citation contexts
  output$citation_contexts_html <- renderUI({
    if (!is.null(values$analysis_results) && 
        !is.null(values$analysis_results$citation_contexts) &&
        nrow(values$analysis_results$citation_contexts) > 0) {
      
      contexts <- values$analysis_results$citation_contexts
      
      # Apply filters
      if (!is.null(input$context_search) && nzchar(input$context_search)) {
        contexts <- contexts %>%
          filter(str_detect(citation_text, regex(input$context_search, ignore_case = TRUE)) |
                   str_detect(full_context, regex(input$context_search, ignore_case = TRUE)))
      }
      
      if (!is.null(input$context_type_filter) && nzchar(input$context_type_filter)) {
        contexts <- contexts %>%
          filter(citation_type == input$context_type_filter)
      }
      
      if (!is.null(input$context_min_words)) {
        contexts <- contexts %>%
          filter(context_word_count >= input$context_min_words)
      }
      
      # Create HTML for each citation context
      if (nrow(contexts) > 0) {
        # citation_boxes <- lapply(1:min(50, nrow(contexts)), function(i) {
        citation_boxes <- lapply(1:nrow(contexts), function(i) {
          context <- contexts[i,]
          
          # section_colors <- colorlist()[1:length(unique(contexts$section))]
          # names(section_colors) <- unique(contexts$section)
          section_colors <- values$analysis_results$section_colors
          
          section_name <- if (!is.null(context$section) && !is.na(context$section)) {
            context$section
          } else {
            "Unknown"
          }
          
          box_color <- section_colors[section_name]
          if (is.na(box_color)) box_color <- "#CCCCCC"
          
          has_reference <- FALSE
          tooltip_text <- "No reference matched for this citation"
          
          if ("ref_full_text" %in% names(context)) {
            ref_value <- context[["ref_full_text"]]
            
            if (length(ref_value) > 0 && 
                !is.na(ref_value) && 
                is.character(ref_value) &&
                nzchar(trimws(ref_value))) {
              
              has_reference <- TRUE
              ref_text <- trimws(ref_value)
              ref_text <- gsub("&", "&amp;", ref_text)
              ref_text <- gsub("<", "&lt;", ref_text)
              ref_text <- gsub(">", "&gt;", ref_text)
              ref_text <- gsub('"', "&quot;", ref_text)
              ref_text <- gsub("'", "&#39;", ref_text)
              ref_text <- gsub("\n", "<br/>", ref_text)
              tooltip_text <- paste0("<strong style='color: #4ECDC4;'>Reference:</strong><br/>", ref_text)
            }
          }
          
          citation_id <- paste0("citation_", i, "_", sample(1:10000, 1))
          
          div(
            style = paste0(
              "margin-bottom: 20px; padding: 15px; border-radius: 8px; ",
              "box-shadow: 0 2px 4px rgba(0,0,0,0.1); ",
              "border-left: 4px solid ", box_color, "; ",
              "background-color: #fafafa;"
            ),
            
            div(
              style = "margin-bottom: 10px; font-size: 12px; color: #666;",
              span(paste("Citation", i, "•"), style = "font-weight: bold;"),
              span(section_name, style = paste0("color: ", box_color, "; font-weight: bold; margin-left: 8px;")),
              span(paste("• Position:", context$citation_position_in_text))
            ),
            
            div(
              style = "display: flex; align-items: center; font-family: 'Courier New', monospace;",
              
              div(
                style = "flex: 1; text-align: right; padding-right: 15px; color: #555; font-size: 14px;",
                if (nzchar(context$words_before)) {
                  paste("...", context$words_before)
                } else {
                  "[start of text]"
                }
              ),
              
              div(
                id = citation_id,
                class = "citation-with-tooltip",
                style = paste0(
                  "background-color: ", box_color, "; color: white; ",
                  "padding: 8px 12px; border-radius: 6px; font-weight: bold; ",
                  "font-size: 14px; white-space: nowrap; max-width: 300px; ",
                  "overflow: hidden; text-overflow: ellipsis; ",
                  "cursor: ", if(has_reference) "pointer" else "default", "; position: relative;"
                ),
                `data-toggle` = if(has_reference) "tooltip" else NULL,
                `data-placement` = "top",
                `data-html` = "true",
                `data-original-title` = tooltip_text,
                context$citation_text
              ),
              
              div(
                style = "flex: 1; text-align: left; padding-left: 15px; color: #555; font-size: 14px;",
                if (nzchar(context$words_after)) {
                  paste(context$words_after, "...")
                } else {
                  "[end of text]"
                }
              )
            ),
            
            div(
              style = "margin-top: 10px; font-size: 11px; color: #888;",
              paste("Context words:", context$context_word_count, "•"),
              if (!is.na(context$is_narrative) && context$is_narrative) {
                span("Narrative citation", style = "color: #e67e22; font-weight: bold;")
              } else {
                span("Parenthetical citation", style = "color: #3498db; font-weight: bold;")
              },
              if (has_reference) {
                span(" • ", 
                     icon("book", style = "color: #27ae60;"),
                     " Reference matched (hover to view)",
                     style = "color: #27ae60; font-weight: bold;")
              } else {
                span(" • ", 
                     icon("times-circle", style = "color: #e74c3c;"),
                     " No reference matched",
                     style = "color: #999; font-style: italic;")
              }
            )
          )
        })
        
        tagList(
          tags$script(HTML("
          $(document).ready(function(){
            $('.citation-with-tooltip').tooltip('dispose');
            
            $('.citation-with-tooltip[data-toggle=\"tooltip\"]').tooltip({
              container: 'body',
              trigger: 'hover focus',
              delay: { show: 200, hide: 100 },
              html: true,
              boundary: 'window'
            });
            
            setTimeout(function() {
              $('.citation-with-tooltip[data-toggle=\"tooltip\"]').tooltip('dispose');
              $('.citation-with-tooltip[data-toggle=\"tooltip\"]').tooltip({
                container: 'body',
                trigger: 'hover focus',
                delay: { show: 200, hide: 100 },
                html: true,
                boundary: 'window'
              });
            }, 800);
          });
        ")),
          
          tags$style(HTML("
          .tooltip {
            font-family: Arial, sans-serif;
            pointer-events: none;
          }
          .tooltip-inner {
            max-width: 500px;
            min-width: 250px;
            text-align: left;
            background-color: rgba(0, 0, 0, 0.95);
            padding: 12px 16px;
            font-size: 13px;
            line-height: 1.6;
            border-radius: 6px;
            box-shadow: 0 4px 12px rgba(0,0,0,0.4);
          }
          .tooltip-inner strong {
            color: #4ECDC4;
            display: block;
            margin-bottom: 6px;
          }
          .tooltip.show {
            opacity: 1;
          }
          .tooltip.top .tooltip-arrow {
            border-top-color: rgba(0, 0, 0, 0.95);
          }
          .citation-with-tooltip[data-toggle]:hover {
            opacity: 0.9;
            transform: scale(1.02);
            transition: all 0.2s ease;
          }
        ")),
          
          citation_boxes
        )
        
      } else {
        div(
          style = "text-align: center; padding: 40px; color: #999;",
          icon("search", style = "font-size: 24px; margin-bottom: 10px;"),
          h4("No citations match your current filters"),
          p("Try adjusting your search terms or filters.")
        )
      }
      
    } else {
      div(
        style = "text-align: center; padding: 40px; color: #999;",
        icon("quote-right", style = "font-size: 24px; margin-bottom: 10px;"),
        h4("No citation contexts available"),
        p("Run the analysis first to see in-context citations.")
      )
    }
  })
  
  # ===========================================
  # TAB 3: NETWORK ANALYSIS
  # ===========================================
  
  # Network visualization
  output$citation_network <- renderVisNetwork({
    if (!is.null(values$network_plot)) {
      values$network_plot
    } else {
      visNetwork(
        nodes = data.frame(id = 1, label = "No citations found", size = 20, color = "#CCCCCC"),
        edges = data.frame(from = numeric(0), to = numeric(0))
      ) %>%
        visOptions(highlightNearest = FALSE) %>%
        visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE)
    }
  })
  
  # Network information
  output$network_info <- renderText({
    if (!is.null(values$network_plot)) {
      stats <- attr(values$network_plot, "stats")
      if (!is.null(stats)) {
        section_info <- ""
        if (!is.null(stats$section_distribution)) {
          section_info <- paste0(
            "\n\nSection Distribution:\n",
            paste(
              stats$section_distribution$section, ": ", 
              stats$section_distribution$n, 
              collapse = "\n"
            )
          )
        }
        
        paste(
          "Nodes:", stats$n_nodes, "\n",
          "Edges:", stats$n_edges, "\n", 
          "Avg. Distance:", stats$avg_distance, "chars\n",
          "Max Distance Filter:", stats$max_distance, "chars\n\n",
          "Network Density:", round(stats$n_edges / (stats$n_nodes * (stats$n_nodes - 1) / 2), 3),
          section_info
        )
      } else {
        "Network statistics not available"
      }
    } else {
      "No network data available"
    }
  })
  
  # Strongest connections table
  output$strongest_connections <- DT::renderDataTable({
    if (!is.null(values$analysis_results) && 
        !is.null(values$analysis_results$network_data) &&
        nrow(values$analysis_results$network_data) > 0) {
      
      values$analysis_results$network_data %>%
        arrange(abs(distance)) %>%
        slice_head(n = 8) %>%
        select(citation1, citation2, distance) %>%
        mutate(
          citation1 = str_trunc(citation1, 20),
          citation2 = str_trunc(citation2, 20),
          distance = paste(abs(distance), "chars")
        )
    } else {
      data.frame(citation1 = character(0), citation2 = character(0), distance = character(0))
    }
  }, options = list(pageLength = 8, dom = 't', ordering = FALSE, searching = FALSE))
  
  # ===========================================
  # DOWNLOAD HANDLERS
  # ===========================================
  
  output$download_network <- downloadHandler(
    filename = function() {
      paste0("citation_network_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(values$analysis_results) && 
          !is.null(values$analysis_results$network_data)) {
        write.csv(values$analysis_results$network_data, file, row.names = FALSE)
      } else {
        write.csv(data.frame(message = "No network data available"), file, row.names = FALSE)
      }
    }
  )
  
  output$download_contexts <- downloadHandler(
    filename = function() {
      paste0("citation_contexts_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(values$analysis_results) && 
          !is.null(values$analysis_results$citation_contexts)) {
        write.csv(values$analysis_results$citation_contexts, file, row.names = FALSE)
      } else {
        write.csv(data.frame(message = "No citation contexts available"), file, row.names = FALSE)
      }
    }
  )
  
  # ===========================================
  # TAB 4: WORD TRENDS ANALYSIS
  # ===========================================
  
  # Populate word choices when analysis is complete
  observe({
    if (!is.null(values$analysis_results) && 
        !is.null(values$analysis_results$word_frequencies) &&
        nrow(values$analysis_results$word_frequencies) > 0) {
      
      # Get top 30 most frequent words
      top_words <- values$analysis_results$word_frequencies %>%
        slice_head(n = 30) %>%
        pull(word)
      
      # Get top bigrams if available
      top_bigrams <- character(0)
      if (!is.null(values$analysis_results$ngrams) && 
          "2gram" %in% names(values$analysis_results$ngrams)) {
        top_bigrams <- values$analysis_results$ngrams$`2gram` %>%
          slice_head(n = 15) %>%
          pull(ngram)
      }
      
      # Get top trigrams if available
      top_trigrams <- character(0)
      if (!is.null(values$analysis_results$ngrams) && 
          "3gram" %in% names(values$analysis_results$ngrams)) {
        top_trigrams <- values$analysis_results$ngrams$`3gram` %>%
          slice_head(n = 10) %>%
          pull(ngram)
      }
      
      # Combine all choices with labels
      all_choices <- c(top_words, top_bigrams, top_trigrams)
      
      # Create named list for optgroups
      choices_list <- list(
        "Top Words (1-gram)" = top_words
      )
      
      if (length(top_bigrams) > 0) {
        choices_list[["Top Bigrams (2-gram)"]] <- top_bigrams
      }
      
      if (length(top_trigrams) > 0) {
        choices_list[["Top Trigrams (3-gram)"]] <- top_trigrams
      }
      
      updateSelectizeInput(session, "trend_words",
                           choices = choices_list,
                           selected = NULL,
                           server = TRUE)
    }
  })
  
  # Check if sections are available
  output$sections_available <- reactive({
    if (!is.null(values$pdf_sections) && length(values$pdf_sections) > 0) {
      section_names <- setdiff(names(values$pdf_sections), c("Full_text", "References"))
      return(length(section_names) > 0)
    }
    return(FALSE)
  })
  outputOptions(output, "sections_available", suspendWhenHidden = FALSE)
  
  # Check if trends are available
  output$trends_available <- reactive({
    return(!is.null(values$word_trends_data) && nrow(values$word_trends_data) > 0)
  })
  outputOptions(output, "trends_available", suspendWhenHidden = FALSE)
  
  # Update word trends when button is clicked
  observeEvent(input$update_trends, {
    
    # Validate inputs
    if (is.null(values$pdf_text) || nchar(values$pdf_text) == 0) {
      showNotification(
        "Please extract text from PDF first.",
        type = "warning",
        duration = 3
      )
      return()
    }
    
    if (is.null(input$trend_words) || length(input$trend_words) == 0) {
      showNotification(
        "Please select at least one word to track.",
        type = "warning",
        duration = 3
      )
      return()
    }
    
    if (length(input$trend_words) > 10) {
      showNotification(
        "Maximum 10 words allowed. Please deselect some words.",
        type = "warning",
        duration = 3
      )
      return()
    }
    
    tryCatch({
      
      # Determine use_sections parameter
      use_sections_param <- switch(input$segmentation_type,
                                   "auto" = "auto",
                                   "sections" = TRUE,
                                   "segments" = FALSE)
      
      # Get number of segments
      n_segs <- if (!is.null(input$n_segments)) input$n_segments else 10
      
      # Determine which text to use
      # If user explicitly chooses segments, use Full_text only
      # Otherwise, use the sections structure
      text_input <- if (input$segmentation_type == "segments") {
        values$pdf_text  # Just the full text as string
      } else {
        values$pdf_sections  # List with sections
      }
      
      # Calculate word distribution
      values$word_trends_data <- calculate_word_distribution(
        text = text_input,
        selected_words = input$trend_words,
        use_sections = use_sections_param,
        n_segments = n_segs,
        remove_stopwords = FALSE  # Don't remove stopwords for selected words
      )
      
      # Check if sections were requested but not available
      sections_used <- attr(values$word_trends_data, "use_sections")
      if (input$segmentation_type == "sections" && !sections_used) {
        showNotification(
          "Sections not available. Using equal-length segments instead.",
          type = "warning",
          duration = 4
        )
      }
      
      showNotification(
        "Word distribution calculated successfully!",
        type = "message",
        duration = 2
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error calculating word distribution:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # Render word trends plot
  output$word_trends_plot <- renderPlotly({
    req(values$word_trends_data)
    
    tryCatch({
      plot_word_distribution(
        word_distribution_data = values$word_trends_data,
        plot_type = input$trend_plot_type,
        smooth = TRUE,
        show_points = input$trend_show_points,
        colors = NULL  # Use automatic colors
      )
    }, error = function(e) {
      showNotification(
        paste("Error creating plot:", e$message),
        type = "error",
        duration = 5
      )
      return(NULL)
    })
  })
  
  # Render word trends statistics table
  output$word_trends_table <- DT::renderDataTable({
    req(values$word_trends_data)
    
    # Create summary statistics
    stats_table <- values$word_trends_data %>%
      group_by(word) %>%
      summarise(
        `Total Occurrences` = sum(count),
        `Avg Frequency` = paste0(round(mean(relative_frequency) * 100, 3), "%"),
        `Min Frequency` = paste0(round(min(relative_frequency) * 100, 3), "%"),
        `Max Frequency` = paste0(round(max(relative_frequency) * 100, 3), "%"),
        `Std Dev` = round(sd(relative_frequency) * 100, 3),
        `Peak Segment` = segment_name[which.max(relative_frequency)],
        .groups = "drop"
      ) %>%
      arrange(desc(`Total Occurrences`))
    
    DT::datatable(
      stats_table,
      options = list(
        pageLength = 10,
        dom = 'ftip',
        ordering = TRUE,
        searching = FALSE,
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      DT::formatStyle(
        'word',
        fontWeight = 'bold',
        color = '#2E86AB'
      )
  })
  
  # Download handler for word trends
  output$download_word_trends <- downloadHandler(
    filename = function() {
      paste0("word_trends_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(values$word_trends_data)) {
        # Prepare data for export
        export_data <- values$word_trends_data %>%
          select(word, segment_id, segment_name, count, 
                 relative_frequency, percentage, total_words) %>%
          arrange(word, segment_id)
        
        write.csv(export_data, file, row.names = FALSE)
      } else {
        write.csv(
          data.frame(message = "No word trends data available"), 
          file, 
          row.names = FALSE
        )
      }
    }
  )
  
  # # Reset word trends when analysis is reset
  # observeEvent(input$reset_analysis, {
  #   values$word_trends_data <- NULL
  #   updateSelectizeInput(session, "trend_words", 
  #                        choices = NULL, 
  #                        selected = NULL)
  # })
  
  # ===========================================
  # RESET FUNCTIONALITY
  # ===========================================
  
  observeEvent(input$reset_analysis, {
    values$pdf_text <- NULL
    values$analysis_results <- NULL
    values$network_plot <- NULL
    values$analysis_running <- FALSE
    values$readability_indices <- NULL
    values$word_trends_data <- NULL
    values$pdf_doi <- NULL
    updateSelectizeInput(session, "trend_words", 
                         choices = NULL, 
                         selected = NULL)
    
    preview_visible(TRUE)
    
    tryCatch({
      shinyjs::reset("pdf_file")
      updateTextInput(session, "pdf_doi_input", value = "")
      updateActionButton(session, "run_analysis", 
                         label = "Start",
                         icon = icon("play"))
      updateActionButton(session, "toggle_preview", "Hide Preview")
      updateTextInput(session, "context_search", value = "")
      updateSelectInput(session, "context_type_filter", selected = "")
    }, error = function(e) {
      cat("Reset UI elements failed:", e$message, "\n")
    })
    
    showNotification(
      "Analysis reset successfully!",
      type = "message",
      duration = 3
    )
  })
  
  # ===========================================
  # EXCEL RESULTS DOWNLOAD
  # ===========================================
  
  # Download all results as Excel file
  output$download_all_results <- downloadHandler(
    filename = function() {
      paste0("content_analysis_results_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      
      # Verifica che ci siano risultati disponibili
      if (is.null(values$analysis_results)) {
        showNotification(
          "No analysis results available. Please run the analysis first.",
          type = "error",
          duration = 5
        )
        return()
      }
      
      tryCatch({
        # Create workbook
        wb_content <- openxlsx::createWorkbook()
        
        # === SHEET 1: SUMMARY STATISTICS ===
        openxlsx::addWorksheet(wb_content, "Summary_Statistics")
        
        summary_data <- data.frame(
          Metric = c(
            "Total Words",
            "Total Citations",
            "Narrative Citations",
            "Parenthetical Citations",
            "Citation Density (per 1000 words)",
            "Lexical Diversity",
            "Total Characters",
            "Total Sentences",
            "Avg Words per Sentence"
          ),
          Value = c(
            values$analysis_results$summary$total_words_analyzed,
            values$analysis_results$summary$citations_extracted,
            values$analysis_results$summary$narrative_citations,
            values$analysis_results$summary$citations_extracted - values$analysis_results$summary$narrative_citations,
            round(values$analysis_results$summary$citation_density_per_1000_words, 2),
            round(values$analysis_results$summary$lexical_diversity, 4),
            values$analysis_results$text_analytics$basic_stats$total_characters,
            values$analysis_results$text_analytics$basic_stats$total_sentences,
            round(values$analysis_results$text_analytics$basic_stats$avg_words_per_sentence, 2)
          ),
          stringsAsFactors = FALSE
        )
        
        openxlsx::writeData(wb_content, "Summary_Statistics", summary_data, startRow = 1)
        openxlsx::addStyle(wb_content, "Summary_Statistics", 
                           style = openxlsx::createStyle(textDecoration = "bold", fgFill = "#2E86AB", fontColour = "#FFFFFF"),
                           rows = 1, cols = 1:2, gridExpand = TRUE)
        openxlsx::setColWidths(wb_content, "Summary_Statistics", cols = 1:2, widths = c(35, 20))
        
        # === SHEET 2: READABILITY INDICES ===
        if (!is.null(values$readability_indices)) {
          openxlsx::addWorksheet(wb_content, "Readability_Indices")
          
          readability_data <- data.frame(
            Index = c(
              "Flesch-Kincaid Grade Level",
              "Flesch Reading Ease",
              "Automated Readability Index (ARI)",
              "Gunning Fog Index",
              "Average Sentence Length (words)",
              "Number of Sentences",
              "Number of Words",
              "Number of Syllables",
              "Number of Complex Words",
              "Percentage Complex Words"
            ),
            Value = c(
              round(values$readability_indices$flesch_kincaid_grade, 2),
              round(values$readability_indices$flesch_reading_ease, 2),
              round(values$readability_indices$automated_readability_index, 2),
              round(values$readability_indices$gunning_fog_index, 2),
              round(values$readability_indices$avg_sentence_length, 2),
              values$readability_indices$n_sentences,
              values$readability_indices$n_words,
              values$readability_indices$n_syllables,
              values$readability_indices$n_complex_words,
              paste0(round(values$readability_indices$pct_complex_words, 2), "%")
            ),
            stringsAsFactors = FALSE
          )
          
          openxlsx::writeData(wb_content, "Readability_Indices", readability_data, startRow = 1)
          openxlsx::addStyle(wb_content, "Readability_Indices", 
                             style = openxlsx::createStyle(textDecoration = "bold", fgFill = "#27ae60", fontColour = "#FFFFFF"),
                             rows = 1, cols = 1:2, gridExpand = TRUE)
          openxlsx::setColWidths(wb_content, "Readability_Indices", cols = 1:2, widths = c(40, 20))
        }
        
        # === SHEET 3: WORD FREQUENCIES ===
        if (!is.null(values$analysis_results$word_frequencies) && 
            nrow(values$analysis_results$word_frequencies) > 0) {
          openxlsx::addWorksheet(wb_content, "Word_Frequencies")
          
          top_words <- values$analysis_results$word_frequencies %>%
            slice_head(n = 50) %>%
            as.data.frame(stringsAsFactors = FALSE)
          
          openxlsx::writeData(wb_content, "Word_Frequencies", top_words, startRow = 1)
          openxlsx::addStyle(wb_content, "Word_Frequencies", 
                             style = openxlsx::createStyle(textDecoration = "bold", fgFill = "#3498db", fontColour = "#FFFFFF"),
                             rows = 1, cols = 1:ncol(top_words), gridExpand = TRUE)
          openxlsx::setColWidths(wb_content, "Word_Frequencies", cols = 1:ncol(top_words), widths = "auto")
        }
        
        # === SHEET 4: BIGRAMS ===
        if (!is.null(values$analysis_results$ngrams) && 
            "2gram" %in% names(values$analysis_results$ngrams)) {
          openxlsx::addWorksheet(wb_content, "Bigrams")
          
          bigrams_data <- values$analysis_results$ngrams$`2gram` %>%
            slice_head(n = 50) %>%
            as.data.frame(stringsAsFactors = FALSE)
          
          openxlsx::writeData(wb_content, "Bigrams", bigrams_data, startRow = 1)
          openxlsx::addStyle(wb_content, "Bigrams", 
                             style = openxlsx::createStyle(textDecoration = "bold", fgFill = "#9b59b6", fontColour = "#FFFFFF"),
                             rows = 1, cols = 1:ncol(bigrams_data), gridExpand = TRUE)
          openxlsx::setColWidths(wb_content, "Bigrams", cols = 1:ncol(bigrams_data), widths = "auto")
        }
        
        # === SHEET 5: TRIGRAMS ===
        if (!is.null(values$analysis_results$ngrams) && 
            "3gram" %in% names(values$analysis_results$ngrams)) {
          openxlsx::addWorksheet(wb_content, "Trigrams")
          
          trigrams_data <- values$analysis_results$ngrams$`3gram` %>%
            slice_head(n = 50) %>%
            as.data.frame(stringsAsFactors = FALSE)
          
          openxlsx::writeData(wb_content, "Trigrams", trigrams_data, startRow = 1)
          openxlsx::addStyle(wb_content, "Trigrams", 
                             style = openxlsx::createStyle(textDecoration = "bold", fgFill = "#e67e22", fontColour = "#FFFFFF"),
                             rows = 1, cols = 1:ncol(trigrams_data), gridExpand = TRUE)
          openxlsx::setColWidths(wb_content, "Trigrams", cols = 1:ncol(trigrams_data), widths = "auto")
        }
        
        # === SHEET 6: CITATION TYPES ===
        if (!is.null(values$analysis_results$citation_metrics$type_distribution)) {
          openxlsx::addWorksheet(wb_content, "Citation_Types")
          
          types_data <- values$analysis_results$citation_metrics$type_distribution %>%
            as.data.frame(stringsAsFactors = FALSE)
          
          openxlsx::writeData(wb_content, "Citation_Types", types_data, startRow = 1)
          openxlsx::addStyle(wb_content, "Citation_Types", 
                             style = openxlsx::createStyle(textDecoration = "bold", fgFill = "#e74c3c", fontColour = "#FFFFFF"),
                             rows = 1, cols = 1:ncol(types_data), gridExpand = TRUE)
          openxlsx::setColWidths(wb_content, "Citation_Types", cols = 1:ncol(types_data), widths = "auto")
        }
        
        # === SHEET 7: CITATIONS BY SECTION ===
        if (!is.null(values$analysis_results$citation_metrics$section_distribution)) {
          openxlsx::addWorksheet(wb_content, "Citations_by_Section")
          
          sections_data <- values$analysis_results$citation_metrics$section_distribution %>%
            filter(n > 0) %>%
            as.data.frame(stringsAsFactors = FALSE)
          
          openxlsx::writeData(wb_content, "Citations_by_Section", sections_data, startRow = 1)
          openxlsx::addStyle(wb_content, "Citations_by_Section", 
                             style = openxlsx::createStyle(textDecoration = "bold", fgFill = "#16a085", fontColour = "#FFFFFF"),
                             rows = 1, cols = 1:ncol(sections_data), gridExpand = TRUE)
          openxlsx::setColWidths(wb_content, "Citations_by_Section", cols = 1:ncol(sections_data), widths = "auto")
        }
        
        # === SHEET 8: CITATION CONTEXTS ===
        if (!is.null(values$analysis_results$citation_contexts) &&
            nrow(values$analysis_results$citation_contexts) > 0) {
          openxlsx::addWorksheet(wb_content, "Citation_Contexts")
          
          # Seleziona le colonne principali se esistono, altrimenti esporta tutte
          contexts_cols <- c("citation_id", "citation_text", "section", "citation_type", 
                             "is_narrative", "words_before", "words_after", "full_context",
                             "context_word_count", "citation_position_in_text")
          
          available_cols <- intersect(contexts_cols, names(values$analysis_results$citation_contexts))
          
          if (length(available_cols) > 0) {
            contexts_export <- values$analysis_results$citation_contexts %>%
              select(all_of(available_cols)) %>%
              as.data.frame(stringsAsFactors = FALSE)
          } else {
            # Se nessuna colonna specificata esiste, esporta tutte
            contexts_export <- values$analysis_results$citation_contexts %>%
              as.data.frame(stringsAsFactors = FALSE)
          }
          
          openxlsx::writeData(wb_content, "Citation_Contexts", contexts_export, startRow = 1)
          openxlsx::addStyle(wb_content, "Citation_Contexts", 
                             style = openxlsx::createStyle(textDecoration = "bold", fgFill = "#2E86AB", fontColour = "#FFFFFF"),
                             rows = 1, cols = 1:ncol(contexts_export), gridExpand = TRUE)
          openxlsx::setColWidths(wb_content, "Citation_Contexts", cols = 1:ncol(contexts_export), widths = "auto")
        }
        
        # === SHEET 9: CITATION NETWORK ===
        if (!is.null(values$analysis_results$network_data) &&
            nrow(values$analysis_results$network_data) > 0) {
          openxlsx::addWorksheet(wb_content, "Citation_Network")
          
          # Esporta tutte le colonne disponibili nel network
          network_export <- values$analysis_results$network_data %>%
            as.data.frame(stringsAsFactors = FALSE)
          
          openxlsx::writeData(wb_content, "Citation_Network", network_export, startRow = 1)
          openxlsx::addStyle(wb_content, "Citation_Network", 
                             style = openxlsx::createStyle(textDecoration = "bold", fgFill = "#8e44ad", fontColour = "#FFFFFF"),
                             rows = 1, cols = 1:ncol(network_export), gridExpand = TRUE)
          openxlsx::setColWidths(wb_content, "Citation_Network", cols = 1:ncol(network_export), widths = "auto")
        }
        
        # === SHEET 10: WORD TRENDS (if available) ===
        if (!is.null(values$word_trends_data) && nrow(values$word_trends_data) > 0) {
          openxlsx::addWorksheet(wb_content, "Word_Trends")
          
          trends_export <- values$word_trends_data %>%
            arrange(word, segment_id) %>%
            as.data.frame(stringsAsFactors = FALSE)
          
          openxlsx::writeData(wb_content, "Word_Trends", trends_export, startRow = 1)
          openxlsx::addStyle(wb_content, "Word_Trends", 
                             style = openxlsx::createStyle(textDecoration = "bold", fgFill = "#27ae60", fontColour = "#FFFFFF"),
                             rows = 1, cols = 1:ncol(trends_export), gridExpand = TRUE)
          openxlsx::setColWidths(wb_content, "Word_Trends", cols = 1:ncol(trends_export), widths = "auto")
          
          # Add summary statistics for word trends
          openxlsx::addWorksheet(wb_content, "Word_Trends_Summary")
          
          trends_summary <- values$word_trends_data %>%
            group_by(word) %>%
            summarise(
              total_occurrences = sum(count),
              avg_frequency = mean(relative_frequency),
              min_frequency = min(relative_frequency),
              max_frequency = max(relative_frequency),
              std_dev = sd(relative_frequency),
              peak_segment = segment_name[which.max(relative_frequency)],
              .groups = "drop"
            ) %>%
            arrange(desc(total_occurrences)) %>%
            as.data.frame(stringsAsFactors = FALSE)
          
          openxlsx::writeData(wb_content, "Word_Trends_Summary", trends_summary, startRow = 1)
          openxlsx::addStyle(wb_content, "Word_Trends_Summary", 
                             style = openxlsx::createStyle(textDecoration = "bold", fgFill = "#16a085", fontColour = "#FFFFFF"),
                             rows = 1, cols = 1:ncol(trends_summary), gridExpand = TRUE)
          openxlsx::setColWidths(wb_content, "Word_Trends_Summary", cols = 1:ncol(trends_summary), widths = "auto")
        }
        
        # Save workbook
        openxlsx::saveWorkbook(wb_content, file, overwrite = TRUE)
        
      }, error = function(e) {
        # Log detailed error for debugging
        cat("Error in download_all_results:\n")
        cat("Error message:", e$message, "\n")
        cat("Stack trace:\n")
        print(e)
        
        showNotification(
          paste("Error exporting results:", e$message),
          type = "error",
          duration = 8
        )
      })
    }
  )
  
  # ===========================================
  # UTILITY FUNCTIONS
  # ===========================================
  
  is_analysis_valid <- reactive({
    !is.null(values$analysis_results) && 
      !is.null(values$analysis_results$citations) &&
      nrow(values$analysis_results$citations) > 0
  })
  
  is_network_valid <- reactive({
    !is.null(values$analysis_results) && 
      !is.null(values$analysis_results$network_data) &&
      nrow(values$analysis_results$network_data) > 0
  })
  
  output$debug_info <- renderText({
    if (!is.null(values$analysis_results)) {
      paste(
        "Debug Info:\n",
        "PDF text length:", nchar(values$pdf_text %||% ""), "\n",
        "Citations found:", nrow(values$analysis_results$citations %||% data.frame()), "\n",
        "Network connections:", nrow(values$analysis_results$network_data %||% data.frame()), "\n",
        "Analysis running:", values$analysis_running %||% FALSE
      )
    } else {
      "No analysis data available"
    }
  })
  
  outputOptions(output, "debug_info", suspendWhenHidden = FALSE)
}