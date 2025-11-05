# ==============================================================================
# UI Component - Query Builder for PubMed API (Collapsible)
# ==============================================================================

pubmedUI <- function() {
  tagList(
    fluidRow(
      column(
        width = 12,
        # Query Builder Box (Collapsible)
        wellPanel(
          id = "pmQueryPanel",

          # Header with collapse button
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
            h4(
              "PubMed Data Collection",
              style = "font-weight: bold; margin: 0;"
            ),
            actionButton(
              "pmToggleQueryPanel",
              icon("chevron-up"),
              class = "btn-sm btn-outline-secondary",
              style = "padding: 5px 10px;",
              title = "Collapse/Expand"
            )
          ),

          # Query content (collapsible)
          div(
            id = "pmQueryContent",

            # Query Builder Section
            div(
              id = "pm-query-builder-container",
              style = "margin-bottom: 20px;",

              # Initial query row (NO operator column)
              div(
                id = "pm-query-row-1",
                class = "pm-query-row",
                style = "margin-bottom: 10px;",
                fluidRow(
                  column(1),
                  column(
                    width = 2,
                    selectInput(
                      "pmField_1",
                      NULL,
                      choices = c(
                        "All Fields" = "all",
                        "Title" = "title",
                        "Abstract" = "abstract",
                        "Title/Abstract" = "tiab",
                        "Author" = "author",
                        "MeSH Terms" = "mesh",
                        "Affiliation" = "affiliation"
                      ),
                      selected = "tiab",
                      width = "100%"
                    )
                  ),
                  column(
                    width = 8,
                    textInput(
                      "pmQuery_1",
                      NULL,
                      value = "",
                      placeholder = 'Example: "machine learning" OR AI',
                      width = "100%"
                    )
                  ),
                  column(
                    width = 1,
                    div(
                      id = "pm-remove-button-1",
                      style = "visibility: hidden;",
                      actionButton(
                        "pmRemoveRow_1",
                        icon("minus-circle"),
                        class = "btn-danger btn-sm",
                        style = "margin-top: 0px;",
                        title = "Remove row"
                      )
                    )
                  )
                )
              )
            ),

            # Help text for search syntax
            div(
              style = "margin-bottom: 15px; font-size: 12px; color: #666;",
              HTML(
                "<strong>Search tips:</strong> Use quotes for exact phrases (e.g., \"machine learning\"). Boolean operators AND, OR, NOT must be UPPERCASE. Learn more at <a href='https://pubmed.ncbi.nlm.nih.gov/help/' target='_blank'>PubMed Help</a>."
              )
            ),

            # Add row and date range buttons
            div(
              style = "margin-bottom: 20px;",
              actionButton(
                "pmAddRow",
                label = "+ Add row",
                icon = icon("plus"),
                class = "btn-outline-primary btn-sm"
              ),
              actionButton(
                "pmToggleDateRange",
                label = "- Date range",
                icon = icon("calendar"),
                class = "btn-outline-primary btn-sm",
                style = "margin-left: 10px;"
              )
            ),

            # Date Range Section (VISIBLE by default)
            div(
              id = "pmDateRangeSection",
              style = "display: block;",
              wellPanel(
                style = "background-color: #f8f9fa;",
                h5("Date Range Filter", style = "font-weight: bold;"),
                fluidRow(
                  column(
                    width = 6,
                    numericInput(
                      "pmYearFrom",
                      "From Year:",
                      value = 1985,
                      min = 1800,
                      max = as.integer(format(Sys.Date(), "%Y"))
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      "pmYearTo",
                      "To Year:",
                      value = as.integer(format(Sys.Date(), "%Y")),
                      min = 1800,
                      max = as.integer(format(Sys.Date(), "%Y"))
                    )
                  )
                )
              )
            ),

            # Advanced filters toggle
            actionButton(
              "pmToggleAdvanced",
              label = "Hide advanced filters",
              icon = icon("filter"),
              class = "btn-link",
              style = "padding: 5px; margin-bottom: 10px;"
            ),

            # Additional Filters Section (VISIBLE by default)
            div(
              id = "pmAdvancedSection",
              style = "display: block;",
              wellPanel(
                style = "background-color: #f8f9fa;",
                fluidRow(
                  column(
                    width = 4,
                    selectizeInput(
                      "pmDocType",
                      "Document Type:",
                      choices = c(
                        "All" = "",
                        "Journal Article" = "Journal Article",
                        "Review" = "Review",
                        "Clinical Trial" = "Clinical Trial",
                        "Meta-Analysis" = "Meta-Analysis",
                        "Case Reports" = "Case Reports",
                        "Randomized Controlled Trial" = "Randomized Controlled Trial",
                        "Systematic Review" = "Systematic Review",
                        "Letter" = "Letter",
                        "Editorial" = "Editorial"
                      ),
                      selected = "",
                      multiple = TRUE
                    )
                  ),
                  column(
                    width = 4,
                    selectizeInput(
                      "pmLanguage",
                      "Language:",
                      choices = c(
                        "All" = "",
                        "English" = "english",
                        "Spanish" = "spanish",
                        "French" = "french",
                        "German" = "german",
                        "Italian" = "italian",
                        "Portuguese" = "portuguese",
                        "Chinese" = "chinese",
                        "Japanese" = "japanese"
                      ),
                      selected = "",
                      multiple = TRUE
                    )
                  ),
                  column(
                    width = 4,
                    numericInput(
                      "pmMaxRecords",
                      "Max Records:",
                      value = 1000,
                      min = 1,
                      max = 10000,
                      step = 100
                    )
                  )
                )
              )
            ),

            # Action buttons with reactive count
            div(
              style = "margin-top: 10px; display: flex; justify-content: space-between; align-items: center;",

              # Left side: Reactive query count message
              div(
                style = "display: flex; align-items: center;",
                uiOutput("pmQueryCountInfo")
              ),

              # Right side: Clear and Search buttons
              div(
                style = "display: flex; gap: 10px;",
                actionButton(
                  "pmClearQuery",
                  "Clear",
                  icon = icon("times"),
                  class = "btn-outline-secondary"
                ),
                actionButton(
                  "pmFetchData",
                  "Search",
                  icon = icon("search"),
                  class = "btn-primary"
                )
              )
            )
          )
        )
      )
    ),

    # Data Display Section (shown when data is available)
    conditionalPanel(
      condition = "output.pmDataAvailable",
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4("Downloaded Collection", style = "font-weight: bold;"),

            # Progress and results info
            uiOutput("pmProgressInfo"),

            # Export buttons
            div(
              style = "margin-bottom: 15px; margin-top: 15px;",
              downloadButton(
                "pmDownloadExcel",
                "Export to Excel",
                icon = icon("file-excel"),
                class = "btn-success"
              ),
              downloadButton(
                "pmDownloadRData",
                "Export to RData",
                icon = icon("database"),
                class = "btn-info",
                style = "margin-left: 10px;"
              )
            ),

            # Data table output
            div(
              style = "margin-top: 15px;",
              DT::dataTableOutput("pmDataTable")
            )
          )
        )
      )
    ),

    # JavaScript for collapsible functionality
    tags$script(HTML(
      "
      $(document).ready(function() {
        // Initialize collapsed state
        var isCollapsed = false;
        
        // Initialize visibility states as TRUE (open)
        var dateRangeVisible = true;
        var advancedVisible = true;
        
        // Handle toggle button click for main panel
        $('#pmToggleQueryPanel').on('click', function() {
          isCollapsed = !isCollapsed;
          $('#pmQueryContent').slideToggle(300);
          
          // Update button icon
          if (isCollapsed) {
            $(this).html('<i class=\"fa fa-chevron-down\"></i>');
          } else {
            $(this).html('<i class=\"fa fa-chevron-up\"></i>');
          }
        });
        
        // Handle date range toggle
        $('#pmToggleDateRange').on('click', function() {
          dateRangeVisible = !dateRangeVisible;
          $('#pmDateRangeSection').slideToggle(300);
          
          // Update button text
          if (dateRangeVisible) {
            $(this).html('<i class=\"fa fa-calendar\"></i> - Date range');
          } else {
            $(this).html('<i class=\"fa fa-calendar\"></i> + Date range');
          }
          
          // Notify Shiny
          Shiny.setInputValue('pmShowDateRange', dateRangeVisible);
        });
        
        // Handle advanced filters toggle
        $('#pmToggleAdvanced').on('click', function() {
          advancedVisible = !advancedVisible;
          $('#pmAdvancedSection').slideToggle(300);
          
          // Update button text
          if (advancedVisible) {
            $(this).html('<i class=\"fa fa-filter\"></i> Hide advanced filters');
          } else {
            $(this).html('<i class=\"fa fa-filter\"></i> Show advanced filters');
          }
          
          // Notify Shiny
          Shiny.setInputValue('pmShowAdvanced', advancedVisible);
        });
        
        // Set initial values in Shiny
        Shiny.setInputValue('pmShowDateRange', true);
        Shiny.setInputValue('pmShowAdvanced', true);
      });
      
      // Custom message handler to collapse panel when data is loaded
      Shiny.addCustomMessageHandler('pmCollapseQueryPanel', function(message) {
        $('#pmQueryContent').slideUp(300);
        $('#pmToggleQueryPanel').html('<i class=\"fa fa-chevron-down\"></i>');
      });
    "
    ))
  )
}


# ==============================================================================
# Server Component - Query Builder for PubMed
# ==============================================================================

pubmedServer <- function(input, output, session, values) {
  # Reactive values to track query rows and toggle states
  queryRows <- reactiveVal(1)
  showDateRange <- reactiveVal(TRUE) # TRUE by default
  showAdvanced <- reactiveVal(TRUE) # TRUE by default

  # Add new query row
  observeEvent(input$pmAddRow, {
    currentRows <- queryRows()
    newRowId <- currentRows + 1
    queryRows(newRowId)

    # Insert new row in UI (WITH operator column)
    insertUI(
      selector = "#pm-query-builder-container",
      where = "beforeEnd",
      ui = div(
        id = paste0("pm-query-row-", newRowId),
        class = "pm-query-row",
        style = "margin-bottom: 10px;",
        fluidRow(
          column(
            width = 1,
            selectInput(
              paste0("pmOperator_", newRowId),
              NULL,
              choices = c("AND" = "AND", "OR" = "OR", "NOT" = "NOT"),
              selected = "AND",
              width = "100%"
            )
          ),
          column(
            width = 2,
            selectInput(
              paste0("pmField_", newRowId),
              NULL,
              choices = c(
                "All Fields" = "all",
                "Title" = "title",
                "Abstract" = "abstract",
                "Title/Abstract" = "tiab",
                "Author" = "author",
                "MeSH Terms" = "mesh",
                "Affiliation" = "affiliation"
              ),
              selected = "tiab",
              width = "100%"
            )
          ),
          column(
            width = 8,
            textInput(
              paste0("pmQuery_", newRowId),
              NULL,
              value = "",
              placeholder = 'Example: "deep learning"',
              width = "100%"
            )
          ),
          column(
            width = 1,
            actionButton(
              paste0("pmRemoveRow_", newRowId),
              icon("minus-circle"),
              class = "btn-danger btn-sm",
              style = "margin-top: 0px;",
              title = "Remove row"
            )
          )
        )
      )
    )

    # Make remove button visible for first row if adding second row
    if (newRowId == 2) {
      runjs("$('#pm-remove-button-1').css('visibility', 'visible');")
    }

    # Add observer for the remove button of the new row
    local({
      rowId <- newRowId
      observeEvent(input[[paste0("pmRemoveRow_", rowId)]], {
        removeUI(selector = paste0("#pm-query-row-", rowId))

        # Check if only one row remains
        if (queryRows() - 1 == 1) {
          runjs("$('#pm-remove-button-1').css('visibility', 'hidden');")
        }
      })
    })
  })

  # Clear all query fields
  observeEvent(input$pmClearQuery, {
    # Reset all query inputs
    for (i in 1:queryRows()) {
      updateTextInput(session, paste0("pmQuery_", i), value = "")
      updateSelectInput(session, paste0("pmField_", i), selected = "tiab")
      if (i > 1) {
        updateSelectInput(session, paste0("pmOperator_", i), selected = "AND")
      }
    }

    # Reset filters
    updateNumericInput(session, "pmYearFrom", value = 2020)
    updateNumericInput(
      session,
      "pmYearTo",
      value = as.integer(format(Sys.Date(), "%Y"))
    )
    updateSelectizeInput(session, "pmDocType", selected = "")
    updateSelectizeInput(session, "pmLanguage", selected = "")
    updateNumericInput(session, "pmMaxRecords", value = 1000)
  })

  # Helper function to build PubMed query
  build_pubmed_query <- function(queryList) {
    if (length(queryList) == 0) {
      return("")
    }

    query_parts <- c()

    for (i in seq_along(queryList)) {
      item <- queryList[[i]]

      # Wrap query with field tag
      field_tag <- switch(
        item$field,
        "all" = "",
        "title" = "[Title]",
        "abstract" = "[Abstract]",
        "tiab" = "[Title/Abstract]",
        "author" = "[Author]",
        "mesh" = "[MeSH Terms]",
        "affiliation" = "[Affiliation]",
        ""
      )

      wrapped_query <- paste0("(", item$query, ")", field_tag)

      if (i == 1) {
        query_parts <- c(query_parts, wrapped_query)
      } else {
        operator <- item$operator
        query_parts <- c(query_parts, operator, wrapped_query)
      }
    }

    query <- paste(query_parts, collapse = " ")
    return(query)
  }

  # Reactive query count - updates automatically when query changes
  output$pmQueryCountInfo <- renderUI({
    # Make this reactive to query inputs
    lapply(1:queryRows(), function(i) {
      input[[paste0("pmQuery_", i)]]
      input[[paste0("pmField_", i)]]
      if (i > 1) input[[paste0("pmOperator_", i)]]
    })

    # Also react to filters
    input$pmYearFrom
    input$pmYearTo
    input$pmDocType
    input$pmLanguage

    # Collect all query rows
    queryList <- list()
    for (i in 1:queryRows()) {
      queryText <- input[[paste0("pmQuery_", i)]]
      if (!is.null(queryText) && queryText != "") {
        queryList[[i]] <- list(
          query = queryText,
          field = input[[paste0("pmField_", i)]],
          operator = if (i > 1) input[[paste0("pmOperator_", i)]] else NULL
        )
      }
    }

    # If no query, show hint
    if (length(queryList) == 0) {
      return(
        div(
          style = "display: inline-block; padding: 5px 10px; color: #6c757d; font-size: 13px; font-style: italic;",
          icon("info-circle"),
          " Enter search terms to see record count"
        )
      )
    }

    # Build query and fetch count
    tryCatch(
      {
        # Build the PubMed query
        query <- build_pubmed_query(queryList)

        # Add date range
        if (showDateRange()) {
          if (!is.na(input$pmYearFrom) && !is.na(input$pmYearTo)) {
            date_filter <- sprintf(
              " AND (%s:%s[pdat])",
              input$pmYearFrom,
              input$pmYearTo
            )
            query <- paste0(query, date_filter)
          }
        }

        # Add document type filter
        if (showAdvanced()) {
          if (!is.null(input$pmDocType) && length(input$pmDocType) > 0) {
            # Rimuovi eventuali stringhe vuote
            doc_types <- input$pmDocType[input$pmDocType != ""]
            if (length(doc_types) > 0) {
              type_filters <- sapply(doc_types, function(t) {
                paste0("\"", t, "\"[Publication Type]")
              })
              type_filter <- paste0(
                " AND (",
                paste(type_filters, collapse = " OR "),
                ")"
              )
              query <- paste0(query, type_filter)
            }
          }
          # Add language filter
          if (!is.null(input$pmLanguage) && length(input$pmLanguage) > 0) {
            # Rimuovi eventuali stringhe vuote
            languages <- input$pmLanguage[input$pmLanguage != ""]
            if (length(languages) > 0) {
              lang_filters <- sapply(input$pmLanguage, function(l) {
                paste0(l, "[Language]")
              })
              lang_filter <- paste0(
                " AND (",
                paste(lang_filters, collapse = " OR "),
                ")"
              )
              query <- paste0(query, lang_filter)
            }
          }
        }

        # Get count from PubMed
        if (!require("pubmedR", quietly = TRUE)) {
          return(
            div(
              style = "display: inline-block; padding: 5px 10px; background-color: #f8d7da; color: #721c24; border-radius: 4px; font-size: 13px;",
              icon("exclamation-circle"),
              " pubmedR package not installed"
            )
          )
        }

        count_result <- pmQueryTotalCount(query = query, api_key = NULL)
        total_available <- count_result$total_count

        div(
          style = "display: inline-block; padding: 5px 10px; background-color: #d4edda; color: #155724; border-radius: 4px; font-size: 13px; font-weight: 500;",
          icon("check-circle"),
          sprintf(" Found %s records", format(total_available, big.mark = ","))
        )
      },
      error = function(e) {
        div(
          style = "display: inline-block; padding: 5px 10px; background-color: #f8d7da; color: #721c24; border-radius: 4px; font-size: 13px;",
          icon("exclamation-circle"),
          " Error: ",
          e$message
        )
      }
    )
  })

  # Download data from PubMed
  observeEvent(input$pmFetchData, {
    # Collect all query rows
    queryList <- list()
    for (i in 1:queryRows()) {
      queryText <- input[[paste0("pmQuery_", i)]]
      if (!is.null(queryText) && queryText != "") {
        queryList[[i]] <- list(
          query = queryText,
          field = input[[paste0("pmField_", i)]],
          operator = if (i > 1) input[[paste0("pmOperator_", i)]] else NULL
        )
      }
    }

    # Validate that at least one query exists
    if (length(queryList) == 0) {
      showModal(modalDialog(
        title = "Error",
        "Please enter at least one search term.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }

    # Build the PubMed query
    query <- build_pubmed_query(queryList)

    # Add date range
    if (showDateRange()) {
      if (!is.na(input$pmYearFrom) && !is.na(input$pmYearTo)) {
        date_filter <- sprintf(
          " AND (%s:%s[pdat])",
          input$pmYearFrom,
          input$pmYearTo
        )
        query <- paste0(query, date_filter)
      }
    }

    # Add document type filter
    if (showAdvanced()) {
      if (!is.null(input$pmDocType) && length(input$pmDocType) > 0) {
        # Rimuovi eventuali stringhe vuote
        doc_types <- input$pmDocType[input$pmDocType != ""]
        if (length(doc_types) > 0) {
          type_filters <- sapply(doc_types, function(t) {
            paste0("\"", t, "\"[Publication Type]")
          })
          type_filter <- paste0(
            " AND (",
            paste(type_filters, collapse = " OR "),
            ")"
          )
          query <- paste0(query, type_filter)
        }
      }
      # Add language filter
      if (!is.null(input$pmLanguage) && length(input$pmLanguage) > 0) {
        # Rimuovi eventuali stringhe vuote
        languages <- input$pmLanguage[input$pmLanguage != ""]
        if (length(languages) > 0) {
          lang_filters <- sapply(input$pmLanguage, function(l) {
            paste0(l, "[Language]")
          })
          lang_filter <- paste0(
            " AND (",
            paste(lang_filters, collapse = " OR "),
            ")"
          )
          query <- paste0(query, lang_filter)
        }
      }
    }

    max_records <- input$pmMaxRecords

    print("PubMed Query:")
    print(query)

    # Show progress modal with dynamic content
    showModal(modalDialog(
      title = "Downloading from PubMed",
      div(
        div(
          style = "text-align: center; margin: 20px 0;",
          icon("spinner", class = "fa-spin fa-3x")
        ),
        div(
          id = "pm-download-progress-text",
          style = "text-align: center; font-size: 14px;",
          "Initializing search..."
        )
      ),
      footer = NULL,
      easyClose = FALSE
    ))

    # Fetch data
    tryCatch(
      {
        # Update progress
        shinyjs::html(
          "pm-download-progress-text",
          paste("Searching PubMed with query:", query)
        )

        # Load pubmedR library
        if (!require("pubmedR", quietly = TRUE)) {
          stop(
            "pubmedR package is not installed. Please install it using: install.packages('pubmedR')"
          )
        }

        # Get total count first
        shinyjs::html(
          "pm-download-progress-text",
          "Counting available records..."
        )

        count_result <- pmQueryTotalCount(query = query, api_key = NULL)
        total_available <- count_result$total_count
        records_to_download <- min(total_available, max_records)

        # Update progress with count information
        shinyjs::html(
          "pm-download-progress-text",
          sprintf(
            "Found %s records. Downloading %s records...",
            format(total_available, big.mark = ","),
            format(records_to_download, big.mark = ",")
          )
        )

        # Fetch data from PubMed using pmApiRequest with limit parameter
        pm_data <- pmApiRequest(
          query = query,
          limit = records_to_download,
          api_key = NULL
        )

        # Update progress
        shinyjs::html(
          "pm-download-progress-text",
          sprintf(
            "Converting %s records to bibliometric format...",
            format(pm_data$records_downloaded, big.mark = ",")
          )
        )

        # Convert using bibliometrix convert2df function
        M <- convert2df(
          file = pm_data,
          dbsource = "pubmed",
          format = "api"
        )

        # Close progress modal
        removeModal()

        # Collapse query panel after successful download
        session$sendCustomMessage('pmCollapseQueryPanel', TRUE)

        # Show success message
        showModal(modalDialog(
          title = "Success",
          div(
            p(
              sprintf(
                "Successfully downloaded %s records from PubMed.",
                format(nrow(M), big.mark = ",")
              )
            ),
            if (total_available > max_records) {
              p(
                style = "color: #856404; background-color: #fff3cd; padding: 10px; border-radius: 5px;",
                sprintf(
                  "Note: %s total records found, but limited to %s records.",
                  format(total_available, big.mark = ","),
                  format(max_records, big.mark = ",")
                )
              )
            }
          ),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))

        # Initialize values
        values = initial(values)
        M <- M %>% mergeKeywords(force = FALSE)

        # Update values
        values$data_source <- "PubMed"
        values$M <- M
        values$Morig <- M
        values$SCdf <- wcTable(M)
        values$COdf <- countryTable(M)
        values$Histfield <- "NA"
        values$results <- list("NA")

        if (ncol(values$M) > 1) {
          values$rest_sidebar <- TRUE
        }
        if (ncol(values$M) > 1) {
          values$loadMenu <- "pubmed_api"
          showModal(missingModalAPI(session))
        }
      },
      error = function(e) {
        removeModal()
        showModal(modalDialog(
          title = "Error",
          paste("An error occurred:", e$message),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
    )
  })

  # Render data table
  output$pmDataTable <- DT::renderDataTable({
    req(values$M)
    req(values$data_source == "PubMed")

    MData <- as.data.frame(apply(values$M, 2, function(x) {
      substring(x, 1, 150)
    }))

    # Create DOI links if available
    if ("DI" %in% names(MData)) {
      MData$DOI <- paste0(
        '<a href=\"https://doi.org/',
        MData$DI,
        '\" target=\"_blank\">',
        MData$DI,
        '</a>'
      )
      nome <- c("DOI", names(MData)[-length(names(MData))])
      MData <- MData[nome]
    }

    DTformat(
      MData,
      nrow = 3,
      filename = "Table",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = TRUE,
      size = '70%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = FALSE,
      escape = FALSE,
      selection = FALSE,
      scrollX = TRUE
    )
  })

  # Check if data is available for conditional panel
  output$pmDataAvailable <- reactive({
    !is.null(values$M) && values$data_source == "PubMed"
  })
  outputOptions(output, "pmDataAvailable", suspendWhenHidden = FALSE)

  # Display progress information
  output$pmProgressInfo <- renderUI({
    if (!is.null(values$M) && values$data_source == "PubMed") {
      div(
        class = "alert alert-success",
        icon("check-circle"),
        strong(" Data loaded: "),
        sprintf(
          "%s documents from PubMed",
          format(nrow(values$M), big.mark = ",")
        )
      )
    }
  })

  # Download handler for Excel
  output$pmDownloadExcel <- downloadHandler(
    filename = function() {
      paste0("pubmed_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(values$M)
      req(values$data_source == "PubMed")

      # Write data to Excel file
      writexl::write_xlsx(values$M, path = file)
    }
  )

  # Download handler for RData
  output$pmDownloadRData <- downloadHandler(
    filename = function() {
      paste0("pubmed_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".RData")
    },
    content = function(file) {
      req(values$M)
      req(values$data_source == "PubMed")

      # Save data as RData file
      M <- values$M
      save(M, file = file)
    }
  )

  # Modal for missing data
  missingModalAPI <- function(session) {
    ns <- session$ns
    modalDialog(
      uiOutput("missingTitle"),
      DT::DTOutput(ns("missingDataTable")),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton(
          label = "Advice",
          inputId = "missingMessage",
          icon = icon("exclamation-sign", lib = "glyphicon")
        ),
        actionButton(
          label = "Report",
          inputId = "missingReport",
          icon = icon("plus", lib = "glyphicon")
        ),
        actionButton(
          label = "Save",
          inputId = "missingDataTable",
          icon = icon("camera", lib = "glyphicon")
        ),
        modalButton(label = "Close")
      )
    )
  }
}
