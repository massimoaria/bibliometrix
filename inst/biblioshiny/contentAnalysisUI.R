## UI components for contentanalysis in Biblioshiny ----

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
      column(
        8,
        div(
          class = "page-header",
          h2(
            "Scientific Article Content Analysis",
            style = "color: #2E86AB; margin-bottom: 10px;"
          ),
          p(
            "Upload a PDF file and analyze citation patterns, context, and co-occurrence networks.",
            style = "color: #666; font-size: 16px; margin-bottom: 20px;"
          )
        )
      ),
      column(
        4,
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
      column(
        10,

        # NUOVO: Bottone per aprire il modal e campo DOI (visibile solo dopo estrazione)
        conditionalPanel(
          condition = "output.text_extracted && !output.analysis_completed",
          div(
            style = "margin-bottom: 15px; padding: 15px; background-color: #e8f4f8; border-radius: 5px;",

            # Prima riga: bottone preview e messaggio
            fluidRow(
              column(
                12,
                actionButton(
                  "open_preview_btn",
                  "View Extracted Text & PDF Preview",
                  icon = icon("file-alt"),
                  class = "btn-info",
                  onclick = "$('#previewModal').modal('show');"
                ),
                span(
                  " Text extracted successfully!",
                  style = "margin-left: 15px; color: #555; font-weight: 500;"
                )
              )
            ),

            # Seconda riga: campo DOI
            fluidRow(
              style = "margin-top: 15px;",
              column(
                12,
                div(
                  style = "background-color: white; padding: 10px; border-radius: 4px; border: 1px solid #d1e7f0;",
                  fluidRow(
                    column(
                      2,
                      tags$label(
                        "Document DOI:",
                        style = "margin-top: 8px; font-weight: bold; color: #2E86AB;"
                      )
                    ),
                    column(
                      4,
                      textInput(
                        "pdf_doi_input",
                        label = NULL,
                        value = "",
                        placeholder = "Enter or edit DOI (e.g., 10.1234/example)",
                        width = "100%"
                      )
                    ),
                    column(
                      6,
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
              column(
                4,
                actionButton(
                  "reopen_preview_btn",
                  "View Original Text",
                  icon = icon("file-alt"),
                  class = "btn-default btn-sm",
                  onclick = "$('#previewModal').modal('show');"
                )
              ),
              column(
                8,
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
                column(
                  3,
                  div(
                    class = "info-box bg-aqua",
                    span(class = "info-box-icon", icon("file-text")),
                    div(
                      class = "info-box-content",
                      span("Total Words", class = "info-box-text"),
                      span(
                        textOutput("total_words", inline = TRUE),
                        class = "info-box-number"
                      )
                    )
                  )
                ),
                column(
                  3,
                  div(
                    class = "info-box bg-green",
                    span(class = "info-box-icon", icon("quote-right")),
                    div(
                      class = "info-box-content",
                      span("Citations Found", class = "info-box-text"),
                      span(
                        textOutput("total_citations", inline = TRUE),
                        class = "info-box-number"
                      )
                    )
                  )
                ),
                column(
                  3,
                  div(
                    class = "info-box bg-yellow",
                    span(class = "info-box-icon", icon("users")),
                    div(
                      class = "info-box-content",
                      span("Narrative Citations", class = "info-box-text"),
                      span(
                        textOutput("narrative_citations", inline = TRUE),
                        class = "info-box-number"
                      )
                    )
                  )
                ),
                column(
                  3,
                  div(
                    class = "info-box bg-red",
                    span(class = "info-box-icon", icon("chart-line")),
                    div(
                      class = "info-box-content",
                      span("Citation Density", class = "info-box-text"),
                      span(
                        textOutput("citation_density", inline = TRUE),
                        class = "info-box-number"
                      ),
                      span("/1000 words", class = "info-box-more")
                    )
                  )
                )
              ),

              fluidRow(
                column(
                  3,
                  div(
                    class = "info-box bg-purple",
                    span(class = "info-box-icon", icon("graduation-cap")),
                    div(
                      class = "info-box-content",
                      span("Flesch-Kincaid Grade", class = "info-box-text"),
                      span(
                        textOutput("flesch_kincaid_grade", inline = TRUE),
                        class = "info-box-number"
                      )
                    )
                  )
                ),
                column(
                  3,
                  div(
                    class = "info-box bg-blue",
                    span(class = "info-box-icon", icon("book-reader")),
                    div(
                      class = "info-box-content",
                      span("Reading Ease", class = "info-box-text"),
                      span(
                        textOutput("flesch_reading_ease", inline = TRUE),
                        class = "info-box-number"
                      )
                    )
                  )
                ),
                column(
                  3,
                  div(
                    class = "info-box bg-teal",
                    span(class = "info-box-icon", icon("robot")),
                    div(
                      class = "info-box-content",
                      span("ARI Index", class = "info-box-text"),
                      span(
                        textOutput("ari_index", inline = TRUE),
                        class = "info-box-number"
                      )
                    )
                  )
                ),
                column(
                  3,
                  div(
                    class = "info-box bg-maroon",
                    span(class = "info-box-icon", icon("cloud")),
                    div(
                      class = "info-box-content",
                      span("Gunning Fog Index", class = "info-box-text"),
                      span(
                        textOutput("gunning_fog_index", inline = TRUE),
                        class = "info-box-number"
                      )
                    )
                  )
                )
              ),
              # Readability Indices Details
              fluidRow(
                column(
                  6,
                  div(
                    class = "box box-info",
                    div(
                      class = "box-header with-border",
                      h5("Readability Indices", style = "color: #3498db;")
                    ),
                    div(
                      class = "box-body",
                      conditionalPanel(
                        condition = "output.analysis_completed",
                        uiOutput("readability_details_html")
                      ),
                      conditionalPanel(
                        condition = "!output.analysis_completed",
                        div(
                          style = "text-align: center; padding: 20px; color: #999;",
                          p(
                            "Readability indices will appear here after analysis."
                          )
                        )
                      )
                    )
                  )
                ),
                column(
                  6,
                  div(
                    class = "box box-primary",
                    div(
                      class = "box-header with-border",
                      h5("Text Statistics", style = "color: #2E86AB;")
                    ),
                    div(
                      class = "box-body",
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
                div(
                  class = "box box-primary",
                  div(
                    class = "box-header with-border",
                    h4(
                      "N-grams Analysis",
                      class = "box-title",
                      style = "color: #2E86AB;"
                    )
                  ),
                  column(
                    4,
                    h5(
                      "Top Unigrams",
                      class = "box-title",
                      style = "color: #2E86AB;"
                    ),
                    DT::dataTableOutput("frequent_words_table")
                  ),
                  column(
                    4,
                    h5("Top Bigrams", style = "color: #2E86AB;"),
                    DT::dataTableOutput("bigrams_table")
                  ),
                  column(
                    4,
                    h5("Top Trigrams", style = "color: #2E86AB;"),
                    DT::dataTableOutput("trigrams_table")
                  )
                )
              ),
              hr(),
              # N-grams Analysis
              fluidRow(
                column(
                  12,
                  div(
                    class = "box-body",
                    fluidRow(
                      column(
                        6,
                        div(
                          class = "box box-primary",
                          h5(
                            "Citation Types Distribution",
                            class = "box-title",
                            style = "color: #2E86AB;"
                          ),
                          div(
                            class = "box-header with-border",
                            div(
                              class = "box-body",
                              DT::dataTableOutput("citation_types_table")
                            )
                          )
                        )
                      ),
                      column(
                        6,
                        div(
                          class = "box box-success",
                          h5(
                            "Citations by Section",
                            class = "box-title",
                            style = "color: #27ae60;"
                          ),
                          div(
                            class = "box-header with-border",
                            div(
                              class = "box-body",
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
                column(
                  12,
                  div(
                    class = "box box-primary",
                    div(
                      class = "box-header with-border",
                      h4(
                        "Word Distribution Analysis",
                        class = "box-title",
                        style = "color: #2E86AB;"
                      ),
                      div(
                        class = "box-tools pull-right",
                        downloadButton(
                          "download_word_trends",
                          "Export Data",
                          class = "btn btn-primary btn-sm",
                          icon = icon("download")
                        )
                      )
                    ),
                    div(
                      class = "box-body",
                      fluidRow(
                        column(
                          4,
                          selectizeInput(
                            "trend_words",
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
                          helpText(
                            "Select up to 10 words from the most frequent terms in your document."
                          )
                        ),
                        column(
                          3,
                          radioButtons(
                            "segmentation_type",
                            "Segmentation:",
                            choices = list(
                              "Auto (use sections if available)" = "auto",
                              "Document sections" = "sections",
                              "Equal-length segments" = "segments"
                            ),
                            selected = "auto"
                          )
                        ),
                        column(
                          2,
                          conditionalPanel(
                            condition = "input.segmentation_type == 'segments'",
                            numericInput(
                              "n_segments",
                              "Number of segments:",
                              value = 10,
                              min = 5,
                              max = 20,
                              step = 1
                            )
                          )
                        ),
                        column(
                          3,
                          radioButtons(
                            "trend_plot_type",
                            "Visualization type:",
                            choices = list(
                              "Line chart" = "line",
                              "Area chart" = "area"
                            ),
                            selected = "line"
                          ),
                          checkboxInput(
                            "trend_show_points",
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
                        column(
                          12,
                          actionButton(
                            "update_trends",
                            "Update Visualization",
                            icon = icon("refresh"),
                            class = "btn-info",
                            style = "margin-bottom: 10px;"
                          ),
                          conditionalPanel(
                            condition = "input.segmentation_type == 'sections' && !output.sections_available",
                            div(
                              style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #ffc107;",
                              icon(
                                "exclamation-triangle",
                                style = "color: #856404;"
                              ),
                              span(
                                " Document sections are not available. The analysis will use equal-length segments instead.",
                                style = "color: #856404; margin-left: 8px;"
                              )
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
                column(
                  12,
                  div(
                    class = "box box-success",
                    div(
                      class = "box-header with-border",
                      h4(
                        "Word Distribution Over Document",
                        class = "box-title",
                        style = "color: #27ae60;"
                      )
                    ),
                    div(
                      class = "box-body",
                      conditionalPanel(
                        condition = "output.trends_available",
                        plotlyOutput("word_trends_plot", height = "600px")
                      ),
                      conditionalPanel(
                        condition = "!output.trends_available",
                        div(
                          style = "text-align: center; padding: 60px; color: #999;",
                          icon(
                            "chart-line",
                            style = "font-size: 48px; margin-bottom: 20px;"
                          ),
                          h4(
                            "No visualization available",
                            style = "color: #666;"
                          ),
                          p(
                            "Select words from the list above and click 'Update Visualization' to see their distribution across the document.",
                            style = "font-size: 14px;"
                          )
                        )
                      )
                    )
                  )
                )
              ),

              # Statistics Table
              fluidRow(
                column(
                  12,
                  div(
                    class = "box box-info",
                    div(
                      class = "box-header with-border",
                      h4(
                        "Distribution Statistics",
                        class = "box-title",
                        style = "color: #3498db;"
                      )
                    ),
                    div(
                      class = "box-body",
                      conditionalPanel(
                        condition = "output.trends_available",
                        DT::dataTableOutput("word_trends_table")
                      ),
                      conditionalPanel(
                        condition = "!output.trends_available",
                        div(
                          style = "text-align: center; padding: 20px; color: #999;",
                          p(
                            "Statistics will appear here after visualization is generated."
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

              # Search and Filter Controls - SINGLE ROW
              fluidRow(
                column(
                  12,
                  div(
                    class = "box box-primary",
                    div(
                      class = "box-header with-border",
                      h4(
                        "In-Context Citation Analysis",
                        class = "box-title",
                        style = "color: #2E86AB;"
                      ),
                      div(
                        class = "box-tools pull-right",
                        downloadButton(
                          "download_contexts",
                          "Export Contexts",
                          class = "btn btn-primary btn-sm",
                          icon = icon("download")
                        )
                      )
                    ),
                    div(
                      class = "box-body",
                      # SINGLE ROW with all controls
                      fluidRow(
                        column(
                          4,
                          textInput(
                            "context_search",
                            "Search in contexts:",
                            placeholder = "Type to search citations or context...",
                            width = "100%"
                          )
                        ),
                        column(
                          2,
                          numericInput(
                            "context_min_words",
                            "Min context words:",
                            value = 10,
                            min = 5,
                            max = 100,
                            width = "100%"
                          )
                        ),
                        column(
                          2,
                          radioButtons(
                            "citation_segmentation",
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
                        column(
                          2,
                          conditionalPanel(
                            condition = "input.citation_segmentation == 'segments'",
                            numericInput(
                              "n_segments_citations",
                              "Number of segments:",
                              value = 10,
                              min = 5,
                              max = 20,
                              step = 1
                            )
                          )
                        ),
                        column(
                          2,
                          br(),
                          actionButton(
                            "update_citation_grouping",
                            "Update Grouping",
                            icon = icon("refresh"),
                            class = "btn-info btn-block",
                            style = "margin-top: 5px;"
                          ),
                          conditionalPanel(
                            condition = "input.citation_segmentation == 'sections' && !output.sections_available",
                            div(
                              style = "background-color: #fff3cd; padding: 5px; border-radius: 3px; margin-top: 8px; border-left: 3px solid #ffc107; font-size: 11px;",
                              icon(
                                "exclamation-triangle",
                                style = "color: #856404; font-size: 10px;"
                              ),
                              span(
                                " Sections not available",
                                style = "color: #856404; margin-left: 5px;"
                              )
                            )
                          ),
                          conditionalPanel(
                            condition = "$('html').hasClass('shiny-busy')",
                            div(
                              style = "margin-top: 8px; text-align: center;",
                              icon(
                                "spinner",
                                class = "fa-spin",
                                style = "color: #3498db; font-size: 12px;"
                              ),
                              span(
                                "Updating...",
                                style = "color: #3498db; font-style: italic; margin-left: 3px; font-size: 11px;"
                              )
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
                column(
                  12,
                  div(
                    class = "box box-success",
                    div(
                      class = "box-header with-border",
                      h4(
                        "Citation Contexts Visualization",
                        class = "box-title",
                        style = "color: #27ae60;"
                      )
                    ),
                    div(class = "box-body", uiOutput("citation_contexts_html"))
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
                div(
                  class = "box box-primary",
                  div(
                    class = "box-header with-border",
                    h4(
                      "Citation Co-occurrence Network",
                      class = "box-title",
                      style = "color: #2E86AB;"
                    ),
                    div(
                      class = "box-tools pull-right",
                      downloadButton(
                        "download_network",
                        "Export Network",
                        class = "btn btn-primary btn-sm",
                        icon = icon("download")
                      )
                    )
                  ),
                  div(
                    class = "box-body",
                    div(
                      style = "margin-bottom: 15px; padding: 10px; background-color: #f0f8ff; border-radius: 5px;",
                      icon("info-circle", style = "color: #3498db;"),
                      span(
                        " Nodes are colored by paper section. Legend visible on the right side of the network.",
                        style = "color: #555; font-size: 13px; margin-left: 8px;"
                      )
                    ),
                    div(
                      style = "height: 75vh; min-height: 700px; border: 1px solid #ddd; border-radius: 5px;",
                      visNetworkOutput(
                        "citation_network",
                        width = "100%",
                        height = "100%"
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  6,
                  div(
                    class = "box box-info",
                    div(
                      class = "box-header with-border",
                      h4(
                        "Network Information",
                        class = "box-title",
                        style = "color: #3498db;"
                      )
                    ),
                    div(class = "box-body", verbatimTextOutput("network_info"))
                  )
                ),
                column(
                  6,
                  div(
                    class = "box box-success",
                    div(
                      class = "box-header with-border",
                      h4(
                        "Strongest Connections",
                        class = "box-title",
                        style = "color: #27ae60;"
                      )
                    ),
                    div(
                      class = "box-body",
                      DT::dataTableOutput("strongest_connections")
                    )
                  )
                )
              )
            ),

            # ===========================================
            # TAB 5: REFERENCES
            # ===========================================
            tabPanel(
              title = "References",
              value = "tab_references",

              br(),

              # Header con info e download
              fluidRow(
                column(
                  12,
                  div(
                    class = "box box-primary",
                    div(
                      class = "box-header with-border",
                      h4(
                        "Bibliography",
                        class = "box-title",
                        style = "color: #2E86AB;"
                      ),
                      div(
                        class = "box-tools pull-right",
                        downloadButton(
                          "download_references",
                          "Export References",
                          class = "btn btn-primary btn-sm",
                          icon = icon("download")
                        )
                      )
                    ),
                    div(
                      class = "box-body",
                      conditionalPanel(
                        condition = "output.references_available",

                        # Summary info
                        div(
                          style = "background-color: #e8f4f8; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                          fluidRow(
                            column(
                              3,
                              div(
                                style = "text-align: center;",
                                icon(
                                  "book",
                                  style = "font-size: 24px; color: #2E86AB;"
                                ),
                                h4(
                                  textOutput("total_refs", inline = TRUE),
                                  style = "color: #2E86AB; margin: 10px 0 5px 0;"
                                ),
                                p(
                                  "Total References",
                                  style = "color: #666; margin: 0;"
                                )
                              )
                            ),
                            column(
                              3,
                              div(
                                style = "text-align: center;",
                                icon(
                                  "file-pdf",
                                  style = "font-size: 24px; color: #e74c3c;"
                                ),
                                h4(
                                  textOutput("pdf_refs", inline = TRUE),
                                  style = "color: #e74c3c; margin: 10px 0 5px 0;"
                                ),
                                p("From PDF", style = "color: #666; margin: 0;")
                              )
                            ),
                            column(
                              3,
                              div(
                                style = "text-align: center;",
                                icon(
                                  "cloud-download-alt",
                                  style = "font-size: 24px; color: #27ae60;"
                                ),
                                h4(
                                  textOutput("crossref_refs", inline = TRUE),
                                  style = "color: #27ae60; margin: 10px 0 5px 0;"
                                ),
                                p(
                                  "From Crossref",
                                  style = "color: #666; margin: 0;"
                                )
                              )
                            ),
                            column(
                              3,
                              div(
                                style = "text-align: center;",
                                icon(
                                  "database",
                                  style = "font-size: 24px; color: #8e44ad;"
                                ),
                                h4(
                                  textOutput("openalex_refs", inline = TRUE),
                                  style = "color: #8e44ad; margin: 10px 0 5px 0;"
                                ),
                                p(
                                  "From OpenAlex",
                                  style = "color: #666; margin: 0;"
                                )
                              )
                            )
                          )
                        ),

                        # Search box
                        fluidRow(
                          column(
                            12,
                            textInput(
                              "reference_search",
                              label = NULL,
                              placeholder = "Search in references (author, title, year, DOI)...",
                              width = "100%"
                            )
                          )
                        ),

                        hr(),

                        # References list
                        uiOutput("references_html")
                      ),

                      conditionalPanel(
                        condition = "!output.references_available",
                        div(
                          style = "text-align: center; padding: 60px; color: #999;",
                          icon(
                            "book-open",
                            style = "font-size: 48px; margin-bottom: 20px;"
                          ),
                          h4("No references available", style = "color: #666;"),
                          p(
                            "References will appear here after the analysis is complete.",
                            style = "font-size: 14px;"
                          ),
                          p(
                            "References can be extracted from the PDF or fetched from Crossref using the document's DOI.",
                            style = "font-size: 12px; color: #999;"
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),

            # ===========================================
            # TAB: BIBLIOAI SUMMARY - UI
            # ===========================================

            tabPanel(
              title = "BiblioAI Summary",
              value = "tab_biblioai",

              br(),

              # Header with API status, Summary Type Selection and Generate Button
              fluidRow(
                column(
                  12,
                  div(
                    class = "box box-primary",
                    div(
                      class = "box-header with-border",
                      h4(
                        "AI-Powered Document Summarization",
                        class = "box-title",
                        style = "color: #2E86AB;"
                      ),
                      div(
                        class = "box-tools pull-right",
                        uiOutput("gemini_api_status")
                      )
                    ),
                    div(
                      class = "box-body",
                      fluidRow(
                        column(
                          6,
                          # Summary type selection with grouped options
                          selectInput(
                            "summary_type",
                            label = div(
                              icon("list-alt", style = "margin-right: 5px;"),
                              tags$strong("Summary Type:")
                            ),
                            choices = list(
                              "Article Summarization" = list(
                                "Short Abstract (250 words)" = "short_abstract",
                                "Narrative Abstract (500-600 words)" = "narrative_abstract",
                                "IMRaD Structure Summary" = "imrad_summary",
                                "Thematic Bibliography" = "thematic_bibliography"
                              ),
                              "Focus on Article Parts" = list(
                                "Research Questions & Context" = "research_questions",
                                "Background & Literature" = "background_literature",
                                "Methods Summary" = "methods_summary",
                                "Implications & Conclusions" = "implications",
                                "Tables & Figures List" = "list_tables_figures"
                              )
                            ),
                            selected = "short_abstract",
                            width = "100%"
                          )
                        ),
                        column(4), # empty space for future options
                        column(
                          2,
                          # Generate button aligned with select input
                          div(
                            style = "margin-top: 25px;",
                            actionButton(
                              "generate_summary",
                              "Generate Summary",
                              icon = icon("magic"),
                              class = "btn-success btn-block",
                              style = "font-weight: bold; padding: 10px;"
                            )
                          )
                        )
                      ),
                      # Progress indicator
                      conditionalPanel(
                        condition = "$('html').hasClass('shiny-busy')",
                        div(
                          style = "text-align: center; margin-top: 10px; padding: 10px; background-color: #f0f8f0; border-radius: 4px;",
                          icon(
                            "spinner",
                            class = "fa-spin",
                            style = "color: #27ae60; font-size: 20px; margin-right: 10px;"
                          ),
                          span(
                            "Generating summary with AI...",
                            style = "color: #27ae60; font-weight: 500;"
                          )
                        )
                      ),
                      # Summary type description
                      div(
                        style = "margin-top: 10px;",
                        uiOutput("summary_type_description")
                      )
                    )
                  )
                )
              ),

              # Summary Results - Full Width
              fluidRow(
                column(
                  12,
                  div(
                    class = "box box-info",
                    div(
                      class = "box-header with-border",
                      h4(
                        "AI-Generated Summary",
                        class = "box-title",
                        style = "color: #3498db;"
                      ),
                      div(
                        class = "box-tools pull-right",
                        conditionalPanel(
                          condition = "output.summary_available",
                          downloadButton(
                            "download_summary",
                            "Export Summary",
                            class = "btn btn-info btn-sm",
                            icon = icon("download")
                          )
                        )
                      )
                    ),
                    div(
                      class = "box-body",
                      style = "min-height: 500px;",

                      conditionalPanel(
                        condition = "output.summary_available",
                        div(
                          style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; border: 1px solid #dee2e6;",

                          # Summary metadata
                          div(
                            style = "margin-bottom: 15px; padding-bottom: 10px; border-bottom: 2px solid #3498db;",
                            fluidRow(
                              column(
                                6,
                                tags$strong(
                                  "Summary Type: ",
                                  style = "color: #3498db;"
                                ),
                                tags$span(
                                  textOutput(
                                    "summary_type_display",
                                    inline = TRUE
                                  ),
                                  style = "font-weight: bold;"
                                )
                              ),
                              column(
                                6,
                                div(
                                  style = "text-align: right;",
                                  tags$strong(
                                    "Generated: ",
                                    style = "color: #3498db;"
                                  ),
                                  textOutput("summary_timestamp", inline = TRUE)
                                )
                              )
                            )
                          ),

                          # Summary content
                          div(
                            style = "line-height: 1.8; color: #333; font-size: 14px; white-space: pre-wrap; word-wrap: break-word;",
                            uiOutput("summary_content_display")
                          )
                        )
                      ),

                      conditionalPanel(
                        condition = "!output.summary_available",
                        div(
                          style = "text-align: center; padding: 80px 20px; color: #999;",
                          icon(
                            "robot",
                            style = "font-size: 64px; margin-bottom: 20px; color: #bbb;"
                          ),
                          h4(
                            "No summary generated yet",
                            style = "color: #666; margin-bottom: 15px;"
                          ),
                          p(
                            "Select your summary type above and click 'Generate Summary' to start.",
                            style = "font-size: 14px; margin-bottom: 10px;"
                          ),
                          p(
                            "Make sure you have uploaded a PDF document first.",
                            style = "font-size: 12px; color: #999;"
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

        # Placeholder when no analysis is done
        conditionalPanel(
          condition = "!output.analysis_completed && !output.text_extracted",
          div(
            style = "text-align: center; margin-top: 100px; color: #999;",
            icon(
              "file-upload",
              style = "font-size: 48px; margin-bottom: 20px;"
            ),
            h3(
              "Upload a PDF file and start the analysis",
              style = "color: #666;"
            ),
            p(
              "Select a scientific article in PDF format and configure the analysis parameters to begin.",
              style = "font-size: 16px;"
            )
          )
        )
      ),

      # ===========================================
      # LEFT PANEL: CONTROLS (column 2)
      # ===========================================
      column(
        2,

        # PDF Import Card
        div(
          class = "box box-primary",
          div(
            class = "box-body",

            tags$details(
              id = "pdf_import_details",
              open = NA, # Aperto di default
              tags$summary(
                "1. Import PDF File ▼",
                style = "font-weight: bold; cursor: pointer; color: #2E86AB; font-size: 16px; padding: 8px 0;"
              ),
              br(),

              fileInput(
                "pdf_file",
                label = "Choose PDF File",
                accept = c(".pdf"),
                buttonLabel = "Browse...",
                placeholder = "No file selected",
                width = "100%"
              ),

              numericInput(
                "Columns",
                label = "Number of Columns in PDF",
                value = NULL,
                min = 1,
                max = 3,
                step = 1,
                width = "100%"
              ),
              helpText(
                "Specify if the PDF has multiple columns (e.g., 2 for typical academic articles)."
              ),

              # MODIFIED: Citation Type Selection - NO DEFAULT
              radioButtons(
                "citation_type_import",
                label = div(
                  tags$span(
                    "Citation Format in PDF:",
                    style = "color: #d9534f; font-weight: bold;"
                  ),
                  tags$span(" *", style = "color: #d9534f; font-size: 16px;"),
                  tags$a(
                    icon("info-circle"),
                    href = "#",
                    onclick = "return false;",
                    style = "margin-left: 5px; color: #3498db; cursor: help;",
                    title = "Choose the citation format used in your document. This helps avoid false positives in citation detection."
                  )
                ),
                choices = list(
                  "Author-year (Smith, 2020)" = "author_year",
                  "Numeric brackets [1]" = "numeric_bracketed",
                  "Numeric superscript¹" = "numeric_superscript",
                  "All formats (may have false positives)" = "all"
                ),
                selected = character(0), # NO DEFAULT SELECTION
                width = "100%"
              ),

              # Warning message when no selection
              conditionalPanel(
                condition = "!input.citation_type_import || input.citation_type_import == ''",
                div(
                  style = "background-color: #fcf8e3; padding: 8px 12px; border-radius: 4px; border-left: 4px solid #f0ad4e; margin-top: -8px; margin-bottom: 10px;",
                  icon("exclamation-triangle", style = "color: #8a6d3b;"),
                  span(
                    " Please select the citation format before extracting text.",
                    style = "color: #8a6d3b; margin-left: 8px; font-size: 12px; font-weight: 500;"
                  )
                )
              ),

              # AI Support checkbox - visible only when Gemini API is available
              conditionalPanel(
                condition = "output.gemini_api_available",
                div(
                  style = "margin-top: 12px; margin-bottom: 12px; background-color: #e8f8f5; padding: 15px; border-radius: 6px; border-left: 4px solid #17a589;",
                  div(
                    style = "display: flex; align-items: center; gap: 10px;",
                    div(
                      style = "flex-shrink: 0;",
                      checkboxInput(
                        "enable_ai_support",
                        label = NULL,
                        value = FALSE,
                        width = "auto"
                      )
                    ),
                    div(
                      style = "display: flex; align-items: center; flex-grow: 1;",
                      icon(
                        "robot",
                        style = "color: #17a589; margin-right: 10px; font-size: 22px;"
                      ),
                      span(
                        "Enable AI-Enhanced Extraction",
                        style = "font-weight: bold; color: #17a589; font-size: 15px;"
                      )
                    )
                  ),
                  helpText(
                    "Use advanced AI to improve text extraction quality and citation detection accuracy.",
                    style = "margin-top: 8px; margin-bottom: 0px; margin-left: 35px; font-size: 11px; color: #555;"
                  )
                )
              ),

              # Processing spinner - visible ONLY during PDF text extraction (not during analysis)
              conditionalPanel(
                condition = "$('html').hasClass('shiny-busy') && (input.extract_text > 0) && !output.text_extracted",
                div(
                  style = "margin-top: 10px; margin-bottom: 10px; background-color: #fff3cd; padding: 15px; border-radius: 5px; border-left: 4px solid #ffc107; text-align: center;",
                  div(
                    icon(
                      "spinner",
                      class = "fa-spin",
                      style = "font-size: 24px; color: #ff9800; margin-bottom: 10px;"
                    ),
                    br(),
                    tags$strong(
                      "Extracting Text from PDF...",
                      style = "color: #856404; font-size: 14px;"
                    ),
                    br(),
                    tags$span(
                      id = "extraction_status_text",
                      "Please wait while we extract the document content.",
                      style = "color: #856404; font-size: 12px; font-style: italic;"
                    )
                  )
                )
              ),

              # File info display
              conditionalPanel(
                condition = "output.pdf_uploaded",
                div(
                  style = "background-color: #f0f8ff; padding: 10px; border-radius: 5px; margin-top: 10px;",
                  icon("file-pdf", style = "color: #e74c3c;"),
                  span(
                    " PDF uploaded successfully!",
                    style = "color: #27ae60; font-weight: bold;"
                  ),
                  br(),
                  textOutput("pdf_info", inline = TRUE)
                )
              ),

              br(),
              actionButton(
                "extract_text",
                "Extract Text from PDF",
                icon = icon("file-text"),
                class = "btn-info btn-block",
                style = "margin-top: 10px;"
              )
            )
          )
        ),

        # Analysis Parameters Card
        div(
          class = "box box-success",
          div(
            class = "box-body",

            tags$details(
              tags$summary(
                "2. Analysis Parameters ▼",
                style = "font-weight: bold; cursor: pointer; color: #27ae60; font-size: 16px; padding: 8px 0;"
              ),
              br(),

              # Citation context window
              numericInput(
                "window_size",
                label = "Context Window Size (words)",
                value = 20,
                min = 5,
                max = 50,
                step = 1,
                width = "100%"
              ),
              helpText(
                "Number of words before and after each citation to extract."
              ),

              # Maximum distance for network
              numericInput(
                "max_distance",
                label = "Max Distance for Network (chars)",
                value = 800,
                min = 200,
                max = 2000,
                step = 100,
                width = "100%"
              ),
              helpText(
                "Maximum character distance between citations to consider as connected."
              ),

              # Advanced options - APERTO DI DEFAULT
              tags$details(
                open = NA,
                tags$summary(
                  "Advanced Options",
                  style = "font-weight: bold; cursor: pointer;"
                ),
                br(),
                checkboxInput(
                  "parse_multiple",
                  "Parse complex multiple citations",
                  value = TRUE
                ),
                checkboxInput(
                  "remove_stopwords",
                  "Remove stopwords from analysis",
                  value = TRUE
                ),
                textInput(
                  "custom_stopwords",
                  "Custom stopwords (comma-separated)",
                  value = "",
                  width = "100%"
                )
              )
            )
          )
        ),

        # Analysis Button
        div(
          class = "box box-warning",
          div(
            class = "box-header with-border",
            h4(
              "3. Run Analysis",
              class = "box-title",
              style = "color: #f39c12;"
            )
          ),
          div(
            class = "box-body",
            actionButton(
              "run_analysis",
              "Start",
              icon = icon("chart-line"),
              class = "btn-warning btn-block btn-lg",
              style = "font-weight: bold; margin-bottom: 10px;"
            ),

            # Progress indicator - ONLY during analysis
            conditionalPanel(
              condition = "$('html').hasClass('shiny-busy') && (input.run_analysis > 0) && !output.analysis_completed",
              div(
                style = "text-align: center; margin-top: 15px;",
                icon("spinner", class = "fa-spin", style = "color: #f39c12;"),
                br(),
                span(
                  "Analyzing content...",
                  style = "color: #f39c12; font-style: italic;"
                )
              )
            ),

            # Reset button
            actionButton(
              "reset_analysis",
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
                  span(
                    textOutput("text_length_info", inline = TRUE),
                    style = "font-size: 12px; color: #666;"
                  )
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
    ),

    # Modal to manage OpenAlex Reference Cards
    tags$div(
      class = "modal fade",
      id = "oaDetailsModal",
      tabindex = "-1",
      role = "dialog",

      tags$div(
        class = "modal-dialog modal-lg",
        role = "document",
        style = "width: 90%; max-width: 1000px;",

        tags$div(
          class = "modal-content",

          # Header
          tags$div(
            class = "modal-header",
            style = "background: linear-gradient(135deg, #1a5f7a 0%, #2874a6 100%); padding: 20px 25px;",
            tags$button(
              type = "button",
              class = "close",
              `data-dismiss` = "modal",
              `aria-label` = "Close",
              tags$span(
                `aria-hidden` = "true",
                style = "color: white !important; font-size: 32px; opacity: 0.8;",
                HTML("&times;")
              )
            ),
            tags$h4(
              class = "modal-title",
              id = "oaModalTitle",
              style = "margin: 0; padding-right: 30px; color: white !important; font-weight: 600;",
              "Document Details"
            )
          ),

          # Body
          tags$div(
            class = "modal-body",
            id = "oaModalBody",
            style = "max-height: 70vh; overflow-y: auto; padding: 20px;",
            div(
              style = "text-align: center; padding: 40px;",
              icon(
                "spinner",
                class = "fa-spin",
                style = "font-size: 32px; color: #3498db;"
              ),
              p(
                "Loading document details...",
                style = "margin-top: 15px; color: #666;"
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
    ),

    tags$style(HTML(
      "
  #previewModal .modal-header,
  #oaDetailsModal .modal-header {
    background: linear-gradient(135deg, #1a5f7a 0%, #2874a6 100%) !important;
  }
  
  #previewModal .modal-header .modal-title,
  #oaDetailsModal .modal-header .modal-title,
  #previewModal .modal-header h4,
  #oaDetailsModal .modal-header h4 {
    color: white !important;
    font-weight: 600 !important;
    font-size: 18px !important;
    line-height: 1.4 !important;
  }
  
  #previewModal .modal-header .close,
  #oaDetailsModal .modal-header .close,
  #previewModal .modal-header .close span,
  #oaDetailsModal .modal-header .close span {
    color: white !important;
    opacity: 0.8 !important;
    text-shadow: none !important;
  }
  
  #previewModal .modal-header .close:hover,
  #oaDetailsModal .modal-header .close:hover {
    opacity: 1 !important;
  }
  
  /* Responsive per schermi piccoli */
  @media (max-width: 768px) {
    #oaDetailsModal .modal-header .modal-title {
      font-size: 16px !important;
    }
  }
"
    )),

    tags$style(HTML(
      "
  /* Layout responsive per le statistics boxes */
  @media (max-width: 992px) {
    .small-box {
      margin-bottom: 15px;
    }
  }
  
  @media (max-width: 768px) {
    .small-box h3 {
      font-size: 28px !important;
    }
    
    .small-box i {
      font-size: 32px !important;
    }
  }
"
    )),

    # JavaScript for OpenAlex  modal
    tags$script(HTML(
      "
  // Funzione per mostrare i dettagli OpenAlex
  function showOADetails(refIndex) {
    // Reset modal content
    $('#oaModalBody').html('<div style=\"text-align: center; padding: 40px;\"><i class=\"fa fa-spinner fa-spin\" style=\"font-size: 32px; color: #3498db;\"></i><p style=\"margin-top: 15px; color: #666;\">Loading document details...</p></div>');
    
    // Invia l'indice a Shiny
    Shiny.setInputValue('selected_oa_ref_index', refIndex, {priority: 'event'});
    
    // Apri il modal
    $('#oaDetailsModal').modal('show');
  }
  
  // Handler personalizzato per aggiornare il contenuto del modal
  Shiny.addCustomMessageHandler('updateOAModal', function(data) {
    $('#oaModalTitle').html(data.title);
    $('#oaModalBody').html(data.content);
  });
"
    )),

    tags$script(HTML(
      "
  // Function to open/close the PDF Import box
  Shiny.addCustomMessageHandler('togglePdfImport', function(message) {
    var details = document.getElementById('pdf_import_details');
    if (details) {
      if (message.action === 'close') {
        details.removeAttribute('open');
      } else if (message.action === 'open') {
        details.setAttribute('open', '');
      }
    }
  });
"
    ))
  )
}
