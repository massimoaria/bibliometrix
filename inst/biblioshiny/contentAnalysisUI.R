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
    fluidRow(
      column(12,
             div(class = "page-header",
                 h2("Scientific Article Content Analysis", 
                    style = "color: #2E86AB; margin-bottom: 10px;"),
                 p("Upload a PDF file and analyze citation patterns, context, and co-occurrence networks.",
                   style = "color: #666; font-size: 16px; margin-bottom: 20px;")
             )
      )
    ),
    
    fluidRow(
      
      # ===========================================
      # RIGHT PANEL: TABBED RESULTS (column 10)
      # ===========================================
      column(10,
             
             # Results will be shown conditionally
             # Text Preview Panel (shows after extraction, hides after analysis)
             conditionalPanel(
               condition = "output.text_extracted",# && !output.analysis_completed",
               tabsetPanel(
                 id = "content_tabs",
                 type = "tabs",
                 # ===========================================
                 # TAB: TEXT PREVIEW
                 # ===========================================
                 tabPanel(
                   title = "Extracted Text Preview",
                   value = "tab_text",
                   div(class = "box box-info",
                       div(class = "box-header with-border",
                           # h4("Extracted Text Preview", class = "box-title", style = "color: #3498db;"),
                           div(class = "box-tools pull-right",
                               span(textOutput("text_length_info", inline = TRUE), 
                                    style = "font-size: 12px; color: #666; margin-right: 10px;"),
                               actionButton("toggle_preview", "Hide Preview", 
                                            class = "btn btn-xs btn-default")
                           )
                       ),
                       div(class = "box-body",
                           conditionalPanel(
                             condition = "output.preview_visible",
                             div(
                               style = paste0(
                                 "max-height: 600px; overflow-y: auto; ",
                                 "background-color: #f8f9fa; padding: 15px; ",
                                 "border: 1px solid #e9ecef; border-radius: 4px; ",
                                 "font-family: 'Consolas', 'Monaco', monospace; ",
                                 "font-size: 13px; line-height: 1.4; ",
                                 "white-space: pre-wrap; word-wrap: break-word;"
                               ),
                               textOutput("text_preview")
                             )
                           )#,
                           # ,conditionalPanel(
                           #   condition = "!output.preview_visible",
                           #   div(
                           #     style = "text-align: center; padding: 20px; color: #666;",
                           #     icon("eye-slash", style = "font-size: 24px; margin-bottom: 10px;"),
                           #     br(),
                           #     "Text preview is hidden. Click 'Show Preview' to view the extracted text."
                           #   )
                           # )
                       )
                   )
                 ),
                 tabPanel(
                   title = "PDF Preview",
                   value = "tab_text",
                   uiOutput("pdf_viewer")
                 )
               )
             ),
             conditionalPanel(
               condition = "output.analysis_completed",
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
                                  column(8,
                                         div(class = "box box-primary",
                                             h5("Citation Types Distribution", class = "box-title", style = "color: #2E86AB;"),
                                             div(class = "box-header with-border",
                                                 div(class = "box-body",
                                                     DT::dataTableOutput("citation_types_table")
                                                 )
                                             )
                                         )
                                  ),
                                  column(4,
                                         div(class = "box box-primary",
                                             div(class = "box-header with-border",
                                                 h5("Text Statistics", style = "color: #2E86AB;")
                                             ),
                                             div(class = "box-body",
                                                 verbatimTextOutput("text_stats")
                                             )
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
                                                         choices = NULL,  # Will be populated dynamically
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
                                    # Custom HTML output for citation contexts
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
                   
                   # Network Controls
                   fluidRow(
                     column(12,
                            div(class = "box box-warning",
                                div(class = "box-header with-border",
                                    h4("Citation Network Controls", class = "box-title", style = "color: #f39c12;")
                                ),
                                div(class = "box-body",
                                    fluidRow(
                                      column(3,
                                             selectInput("network_layout",
                                                         "Layout Algorithm:",
                                                         choices = list(
                                                           "Force-directed (FR)" = "layout_with_fr",
                                                           "Kamada-Kawai" = "layout_with_kk", 
                                                           "Nicely" = "layout_nicely"
                                                         ),
                                                         selected = "layout_with_fr",
                                                         width = "100%"
                                             )
                                      ),
                                      column(3,
                                             selectInput("network_color",
                                                         "Color Nodes By:",
                                                         choices = list(
                                                           "Citation Type" = "citation_type",
                                                           "Publication Year" = "year",
                                                           "Connection Frequency" = "frequency"
                                                         ),
                                                         selected = "citation_type",
                                                         width = "100%"
                                             )
                                      ),
                                      column(3,
                                             checkboxInput("network_physics",
                                                           "Enable Physics Simulation",
                                                           value = FALSE
                                             )
                                      ),
                                      column(3,
                                             actionButton("update_network",
                                                          "Update Network",
                                                          class = "btn-warning btn-block",
                                                          icon = icon("refresh")
                                             )
                                      )
                                    )
                                )
                            )
                     )
                   ),
                   
                   # Network Visualization
                   fluidRow(
                     column(8,
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
                                      style = "height: 500px; border: 1px solid #ddd; border-radius: 5px;",
                                      visNetworkOutput("citation_network", height = "490px")
                                    )
                                )
                            )
                     ),
                     column(4,
                            div(class = "box box-info",
                                div(class = "box-header with-border",
                                    h4("Network Information", class = "box-title", style = "color: #3498db;")
                                ),
                                div(class = "box-body",
                                    verbatimTextOutput("network_info")
                                )
                            ),
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
                                  icon = icon("chart-line"), # CORREZIONE: icona valida
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
    )
  )
}

create_citation_network_basic <- function(citation_analysis_results,
                                          max_distance = 1000,
                                          min_connections = 1,
                                          layout = "fr",
                                          show_labels = TRUE,
                                          height = "600px",
                                          width = "100%") {
  
  # require(visNetwork)
  # require(dplyr)
  # require(stringr)
  
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
  
  # Create nodes
  nodes <- data.frame(
    id = 1:length(all_citation_texts),
    label = if (show_labels) str_trunc(all_citation_texts, 25) else "",
    title = all_citation_texts,
    stringsAsFactors = FALSE
  )
  
  # Calculate connections
  node_connections <- rbind(
    data.frame(citation = network_data_filtered$citation1, stringsAsFactors = FALSE),
    data.frame(citation = network_data_filtered$citation2, stringsAsFactors = FALSE)
  ) %>%
    count(citation, name = "connections")
  
  nodes$connections <- sapply(nodes$title, function(cite) {
    conn <- node_connections$connections[node_connections$citation == cite]
    if (length(conn) == 0) return(0)
    return(conn[1])
  })
  
  # Filter by connections
  nodes <- nodes[nodes$connections >= min_connections, ]
  valid_citations <- nodes$title
  network_data_filtered <- network_data_filtered %>%
    filter(citation1 %in% valid_citations & citation2 %in% valid_citations)
  
  if (nrow(network_data_filtered) == 0) {
    warning("No valid connections after filtering.")
    return(NULL)
  }
  
  # Set node properties
  nodes$size <- pmax(15, pmin(40, 15 + nodes$connections * 3))
  
  # Simple type extraction
  get_simple_type <- function(citation_text) {
    if (str_detect(citation_text, "et\\s+al")) return("Et al.")
    if (str_detect(citation_text, "&")) return("Multiple authors")
    if (str_detect(citation_text, "see|e\\.g\\.")) return("Reference")
    if (str_detect(citation_text, "\\[\\d+\\]")) return("Numbered")
    if (str_detect(citation_text, "doi\\.org")) return("DOI")
    return("Standard")
  }
  
  nodes$group <- sapply(nodes$title, get_simple_type)
  
  # Colors
  color_map <- c(
    "Et al." = "#FF6B6B",
    "Multiple authors" = "#4ECDC4", 
    "Reference" = "#45B7D1",
    "Standard" = "#96CEB4",
    "Numbered" = "#FFEAA7",
    "DOI" = "#DDA0DD"
  )
  
  nodes$color <- color_map[nodes$group]
  nodes$color[is.na(nodes$color)] <- "#CCCCCC"
  
  # Create edges
  edges <- data.frame(
    from = sapply(network_data_filtered$citation1, function(cite) {
      nodes$id[nodes$title == cite][1]
    }),
    to = sapply(network_data_filtered$citation2, function(cite) {
      nodes$id[nodes$title == cite][1]
    }),
    distance = abs(network_data_filtered$distance),
    stringsAsFactors = FALSE
  )
  
  edges <- edges[!is.na(edges$from) & !is.na(edges$to), ]
  
  edges$width <- pmax(1, 8 - (edges$distance / 100))
  edges$color <- ifelse(edges$distance <= 300, "#FF6B6B", 
                        ifelse(edges$distance <= 600, "#4ECDC4", "#CCCCCC"))
  edges$title <- paste("Distance:", edges$distance, "characters")
  
  # Create network
  network <- visNetwork(nodes, edges, height = height, width = width)
  
  # Apply layout
  if (layout == "fr") {
    network <- network %>% visIgraphLayout(layout = "layout_with_fr", randomSeed = 123)
  } else if (layout == "kk") {
    network <- network %>% visIgraphLayout(layout = "layout_with_kk", randomSeed = 123)
  } else {
    network <- network %>% visIgraphLayout(layout = "layout_nicely", randomSeed = 123)
  }
  
  # Configure options
  network <- network %>%
    visOptions(highlightNearest = TRUE) %>%
    visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) %>%
    visPhysics(enabled = FALSE)
  
  # Add stats
  n_nodes <- nrow(nodes)
  n_edges <- nrow(edges)
  avg_distance <- round(mean(edges$distance), 1)
  
  attr(network, "stats") <- list(
    n_nodes = n_nodes,
    n_edges = n_edges,
    avg_distance = avg_distance,
    max_distance = max_distance
  )
  
  return(network)
}

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
  
  # Check if text is extracted (new reactive)
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
    # Rimuovi i file PDF temporanei creati in questa sessione
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
      values$pdf_sections <- pdf_text[-1]  # All sections except full text
      
      showNotification(
        "PDF text extracted successfully! Preview available above.",
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
      # Show first 3000 characters with smart truncation
      preview_length <- 80000
      full_text <- values$pdf_text
      
      if (nchar(full_text) > preview_length) {
        # Find a good breaking point (end of sentence or paragraph)
        truncated <- substr(full_text, 1, preview_length)
        last_period <- max(c(
          regexpr("\\. [A-Z]", truncated, perl = TRUE),
          regexpr("\\.\n", truncated, perl = TRUE),
          regexpr("\\?\n", truncated, perl = TRUE),
          regexpr("!\n", truncated, perl = TRUE)
        ))
        
        if (last_period > 100) {  # Only break at sentence if we have enough text
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
      
      values$analysis_results <- analyze_scientific_content_enhanced(
        text = values$pdf_sections,
        window_size = input$window_size,
        parse_multiple_citations = input$parse_multiple,
        remove_stopwords = input$remove_stopwords,
        custom_stopwords = custom_stops
      )
      
      # Create network if we have data
      if (!is.null(values$analysis_results$network_data) && 
          nrow(values$analysis_results$network_data) > 0) {
        
        layout_type <- switch(input$network_layout,
                              "layout_with_fr" = "fr",
                              "layout_with_kk" = "kk", 
                              "layout_nicely" = "nicely",
                              "fr")
        
        values$network_plot <- create_citation_network_basic(
          values$analysis_results,
          max_distance = input$max_distance,
          layout = layout_type,
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
                         label = "Start Content Analysis",
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
  
  # Citation types table
  output$citation_types_table <- DT::renderDataTable({
    if (!is.null(values$analysis_results) && 
        !is.null(values$analysis_results$citation_metrics$type_distribution)) {
      values$analysis_results$citation_metrics$type_distribution
    } else {
      data.frame(citation_type = character(0), n = numeric(0), percentage = numeric(0))
    }
  }, options = list(pageLength = 8, dom = 't', ordering = FALSE, searching = FALSE))
  
  # Frequent words table
  output$frequent_words_table <- DT::renderDataTable({
    if (!is.null(values$analysis_results)) {
      values$analysis_results$word_frequencies %>% 
        slice_head(n = 15) %>%
        select(word, n,)
      # select(word, n, frequency) %>%
      # mutate(frequency = round(frequency*100, 1))
    } else {
      data.frame(word = character(0), n = numeric(0))
    }
  }, options = list(pageLength = 15, dom = 't', ordering = FALSE, searching = FALSE))
  
  # Bigrams table
  output$bigrams_table <- DT::renderDataTable({
    if (!is.null(values$analysis_results) && 
        "2gram" %in% names(values$analysis_results$ngrams)) {
      #N <- sum(values$analysis_results$word_frequencies$n)
      values$analysis_results$ngrams$`2gram` %>%
        select(ngram, n) %>%
        #mutate(frequency = round(n*100 / N, 1)) %>%
        slice_head(n = 15)
    } else {
      data.frame(ngram = character(0), n = numeric(0))
    }
  }, options = list(pageLength = 15, dom = 't', ordering = FALSE, searching = FALSE))
  
  # Trigrams table
  output$trigrams_table <- DT::renderDataTable({
    if (!is.null(values$analysis_results) && 
        "3gram" %in% names(values$analysis_results$ngrams)) {
      N <- sum(values$analysis_results$word_frequencies$n)
      values$analysis_results$ngrams$`3gram` %>%
        select(ngram, n) %>%
        #mutate(frequency = round(n*100 / N, 1)) %>%
        slice_head(n = 15)
    } else {
      data.frame(ngram = character(0), n = numeric(0))
    }
  }, options = list(pageLength = 15, dom = 't', ordering = FALSE, searching = FALSE))
  
  # Text statistics
  output$text_stats <- renderText({
    if (!is.null(values$analysis_results)) {
      stats <- values$analysis_results$text_analytics$basic_stats
      paste(
        "Characters:", format(stats$total_characters, big.mark = ","), "\n",
        "Words:", format(stats$total_words, big.mark = ","), "\n",
        "Sentences:", format(stats$total_sentences, big.mark = ","), "\n",
        "Avg words/sentence:", round(stats$avg_words_per_sentence, 1), "\n",
        "Lexical diversity:", round(values$analysis_results$summary$lexical_diversity, 3)
      )
    } else {
      "No analysis data available"
    }
  })
  
  # ===========================================
  # TAB 2: IN-CONTEXT CITATION ANALYSIS
  # ===========================================
  
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
      save(contexts, file="debug_contexts.RData")
      
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
        citation_boxes <- lapply(1:min(50, nrow(contexts)), function(i) {
          context <- contexts[i,]
          
          # Create color based on citation type
          type_colors <- c(
            "narrative_etal" = "#FF6B6B",
            "author_year_etal" = "#4ECDC4",
            "author_year_basic" = "#45B7D1",
            "author_year_ampersand" = "#96CEB4",
            "parsed_from_multiple" = "#FFEAA7",
            "complex_multiple_citations" = "#DDA0DD",
            "see_citations" = "#A8E6CF",
            "multiple_citations_semicolon" = "#FFB3BA",
            "narrative_single" = "#FFDFBA",
            "narrative_multiple_authors" = "#BAFFC9"
          )
          
          box_color <- type_colors[context$citation_type]
          if (is.na(box_color)) box_color <- "#CCCCCC"
          
          # Prepare tooltip text with reference if available
          has_reference <- FALSE
          tooltip_text <- "No reference matched for this citation"

          # Più robusta verifica della presenza di ref_full_text
          if ("ref_full_text" %in% names(context)) {
            # Estrai il valore
            ref_value <- context[["ref_full_text"]]
            
            # Debug print (rimuovi dopo il test)
            cat("Citation", i, "- ref_value class:", class(ref_value), 
                "- is.na:", is.na(ref_value), 
                "- value:", substr(as.character(ref_value), 1, 50), "\n")
            
            # Controlla se il valore è valido
            if (length(ref_value) > 0 && 
                !is.na(ref_value) && 
                is.character(ref_value) &&
                nzchar(trimws(ref_value))) {
              
              has_reference <- TRUE
              # Escape HTML special characters
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
            
            # Citation info header
            div(
              style = "margin-bottom: 10px; font-size: 12px; color: #666;",
              span(paste("Citation", i, "•"), style = "font-weight: bold;"),
              span(context$citation_type, style = paste0("color: ", box_color, "; font-weight: bold;")),
              span(paste("• Position:", context$citation_position_in_text))
            ),
            
            # Context visualization
            div(
              style = "display: flex; align-items: center; font-family: 'Courier New', monospace;",
              
              # Words before (left context)
              div(
                style = "flex: 1; text-align: right; padding-right: 15px; color: #555; font-size: 14px;",
                if (nzchar(context$words_before)) {
                  paste("...", context$words_before)
                } else {
                  "[start of text]"
                }
              ),
              
              # Citation (center, highlighted) with tooltip
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
              
              # Words after (right context)
              div(
                style = "flex: 1; text-align: left; padding-left: 15px; color: #555; font-size: 14px;",
                if (nzchar(context$words_after)) {
                  paste(context$words_after, "...")
                } else {
                  "[end of text]"
                }
              )
            ),
            
            # Additional info
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
          # JavaScript to initialize Bootstrap tooltips
          tags$script(HTML("
          $(document).ready(function(){
            // Destroy any existing tooltips first
            $('.citation-with-tooltip').tooltip('dispose');
            
            // Initialize all tooltips with data-toggle attribute
            $('.citation-with-tooltip[data-toggle=\"tooltip\"]').tooltip({
              container: 'body',
              trigger: 'hover focus',
              delay: { show: 200, hide: 100 },
              html: true,
              boundary: 'window'
            });
            
            // Reinitialize after a delay to catch dynamically added elements
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
          
          # Custom CSS for tooltip styling
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
  
  # Update network when parameters change
  observeEvent(input$update_network, {
    if (!is.null(values$analysis_results) && 
        !is.null(values$analysis_results$network_data) &&
        nrow(values$analysis_results$network_data) > 0) {
      
      tryCatch({
        layout_type <- switch(input$network_layout,
                              "layout_with_fr" = "fr",
                              "layout_with_kk" = "kk", 
                              "layout_nicely" = "nicely",
                              "fr")
        
        values$network_plot <- create_citation_network_basic(
          values$analysis_results,
          max_distance = input$max_distance,
          layout = layout_type,
          show_labels = TRUE
        )
        
        showNotification(
          "Network updated successfully!",
          type = "message",
          duration = 2
        )
        
      }, error = function(e) {
        showNotification(
          paste("Error updating network:", e$message),
          type = "error",
          duration = 3
        )
      })
    }
  })
  
  # Network information
  output$network_info <- renderText({
    if (!is.null(values$network_plot)) {
      stats <- attr(values$network_plot, "stats")
      if (!is.null(stats)) {
        paste(
          "Nodes:", stats$n_nodes, "\n",
          "Edges:", stats$n_edges, "\n", 
          "Avg. Distance:", stats$avg_distance, "chars\n",
          "Max Distance Filter:", stats$max_distance, "chars\n\n",
          "Network Density:", round(stats$n_edges / (stats$n_nodes * (stats$n_nodes - 1) / 2), 3), "\n",
          "Connected Components:", "1"  # Basic assumption for simplicity
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
  # RESET FUNCTIONALITY
  # ===========================================
  
  observeEvent(input$reset_analysis, {
    values$pdf_text <- NULL
    values$analysis_results <- NULL
    values$network_plot <- NULL
    values$analysis_running <- FALSE
    
    # Reset preview visibility
    preview_visible(TRUE)
    
    tryCatch({
      shinyjs::reset("pdf_file")
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
  # UTILITY FUNCTIONS
  # ===========================================
  
  # Helper function to check if analysis is valid
  is_analysis_valid <- reactive({
    !is.null(values$analysis_results) && 
      !is.null(values$analysis_results$citations) &&
      nrow(values$analysis_results$citations) > 0
  })
  
  # Helper function to check if network is valid
  is_network_valid <- reactive({
    !is.null(values$analysis_results) && 
      !is.null(values$analysis_results$network_data) &&
      nrow(values$analysis_results$network_data) > 0
  })
  
  # ===========================================
  # DEBUGGING OUTPUT (hidden by default)
  # ===========================================
  
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

# ===========================================
# HELPER FUNCTION FOR NULL COALESCING
# ===========================================

# Define null coalescing operator if not available
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs) && length(lhs) > 0) lhs else rhs
}