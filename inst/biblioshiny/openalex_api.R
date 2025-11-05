# ==============================================================================
# UI Component - WebOfScience-like Query Builder for OpenAlex (Collapsible)
# ==============================================================================

openAlexUI <- function() {
  tagList(
    fluidRow(
      column(
        width = 12,
        # Query Builder Box (Collapsible)
        wellPanel(
          id = "oaQueryPanel",

          # Header with collapse button
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
            h4(
              "OpenAlex Data Collection",
              style = "font-weight: bold; margin: 0;"
            ),
            actionButton(
              "oaToggleQueryPanel",
              icon("chevron-up"),
              class = "btn-sm btn-outline-secondary",
              style = "padding: 5px 10px;",
              title = "Collapse/Expand"
            )
          ),

          # Query content (collapsible)
          div(
            id = "oaQueryContent",

            # Query Builder Section (WebOfScience-like)
            div(
              id = "query-builder-container",
              style = "margin-bottom: 20px;",

              # Initial query row (NO operator column)
              div(
                id = "query-row-1",
                class = "query-row",
                style = "margin-bottom: 10px;",
                fluidRow(
                  column(1),
                  column(
                    width = 2,
                    selectInput(
                      "oaField_1",
                      NULL,
                      choices = c(
                        "All Fields" = "default",
                        "Title" = "title",
                        "Abstract" = "abstract",
                        "Title and Abstract" = "title_abstract",
                        "Author" = "author",
                        "Concept" = "concept"
                      ),
                      selected = "title_abstract",
                      width = "100%"
                    )
                  ),
                  column(
                    width = 8,
                    textInput(
                      "oaQuery_1",
                      NULL,
                      value = "",
                      placeholder = 'Example: "machine learning" OR AI',
                      width = "100%"
                    )
                  ),
                  column(
                    width = 1,
                    div(
                      id = "remove-button-1",
                      style = "visibility: hidden;",
                      actionButton(
                        "oaRemoveRow_1",
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
                "<strong>Search tips:</strong> Use quotes for exact phrases (e.g., \"science mapping\"). Boolean operators AND, OR, NOT must be UPPERCASE."
              )
            ),

            # Add row and date range buttons
            div(
              style = "margin-bottom: 20px;",
              actionButton(
                "oaAddRow",
                label = "+ Add row",
                icon = icon("plus"),
                class = "btn-outline-primary btn-sm"
              ),
              actionButton(
                "oaToggleDateRange",
                label = "+ Date range",
                icon = icon("calendar"),
                class = "btn-outline-primary btn-sm",
                style = "margin-left: 10px;"
              )
            ),

            # Date Range Section (initially visible)
            div(
              id = "oaDateRangeSection",
              style = "display: block;", # Visibile di default
              wellPanel(
                style = "background-color: #f8f9fa;",
                h5("Date Range Filter", style = "font-weight: bold;"),
                fluidRow(
                  column(
                    width = 6,
                    numericInput(
                      "oaYearFrom",
                      "From Year:",
                      value = 1985,
                      min = 1900,
                      max = as.integer(format(Sys.Date(), "%Y"))
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      "oaYearTo",
                      "To Year:",
                      value = as.integer(format(Sys.Date(), "%Y")),
                      min = 1900,
                      max = as.integer(format(Sys.Date(), "%Y"))
                    )
                  )
                )
              )
            ),

            # Advanced filters toggle
            actionButton(
              "oaToggleAdvanced",
              label = "Advanced filters",
              icon = icon("filter"),
              class = "btn-link",
              style = "padding: 5px; margin-bottom: 10px;"
            ),

            # Additional Filters Section (initially visible)
            div(
              id = "oaAdvancedSection",
              style = "display: block;", # Visibile di default
              wellPanel(
                style = "background-color: #f8f9fa;",
                fluidRow(
                  column(
                    width = 4,
                    selectizeInput(
                      "oaDocType",
                      "Document Type:",
                      choices = c(
                        "All" = "",
                        "Article" = "article",
                        "Book Chapter" = "book-chapter",
                        "Book" = "book",
                        "Dataset" = "dataset",
                        "Dissertation" = "dissertation",
                        "Editorial" = "editorial",
                        "Letter" = "letter",
                        "Preprint" = "preprint",
                        "Review" = "review"
                      ),
                      selected = "",
                      multiple = TRUE
                    )
                  ),
                  column(
                    width = 4,
                    selectizeInput(
                      "oaLanguage",
                      "Language:",
                      choices = c(
                        "All" = "",
                        "English" = "en",
                        "Spanish" = "es",
                        "French" = "fr",
                        "German" = "de",
                        "Italian" = "it",
                        "Portuguese" = "pt",
                        "Chinese" = "zh",
                        "Japanese" = "ja"
                      ),
                      selected = "",
                      multiple = TRUE
                    )
                  ),
                  column(
                    width = 4,
                    numericInput(
                      "oaMaxRecords",
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
            # Action buttons section
            div(
              style = "margin-top: 10px; display: flex; justify-content: space-between; align-items: center;",

              # Left side: Reactive query count message
              div(
                style = "display: flex; align-items: center;",
                uiOutput("oaQueryCountInfo")
              ),

              # Right side: Clear and Search buttons
              div(
                style = "display: flex; gap: 10px;",
                actionButton(
                  "oaClearQuery",
                  "Clear",
                  icon = icon("times"),
                  class = "btn-outline-secondary"
                ),
                actionButton(
                  "oaFetchData",
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
      condition = "output.oaDataAvailable",
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4("Downloaded Collection", style = "font-weight: bold;"),

            # Progress and results info
            uiOutput("oaProgressInfo"),

            # Export buttons
            div(
              style = "margin-bottom: 15px; margin-top: 15px;",
              downloadButton(
                "oaDownloadExcel",
                "Export to Excel",
                icon = icon("file-excel"),
                class = "btn-success"
              ),
              downloadButton(
                "oaDownloadRData",
                "Export to RData",
                icon = icon("database"),
                class = "btn-info",
                style = "margin-left: 10px;"
              )
            ),

            # Data table output
            div(
              style = "margin-top: 15px;",
              DT::dataTableOutput("oaDataTable")
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
    
    // Initialize visibility states
    var dateRangeVisible = true;
    var advancedVisible = true;
    
    // Handle toggle button click for main panel
    $('#oaToggleQueryPanel').on('click', function() {
      isCollapsed = !isCollapsed;
      $('#oaQueryContent').slideToggle(300);
      
      // Update button icon
      if (isCollapsed) {
        $(this).html('<i class=\"fa fa-chevron-down\"></i>');
      } else {
        $(this).html('<i class=\"fa fa-chevron-up\"></i>');
      }
    });
    
    // Handle date range toggle
    $('#oaToggleDateRange').on('click', function() {
      dateRangeVisible = !dateRangeVisible;
      $('#oaDateRangeSection').slideToggle(300);
      
      // Update button text
      if (dateRangeVisible) {
        $(this).html('<i class=\"fa fa-calendar\"></i> - Date range');
      } else {
        $(this).html('<i class=\"fa fa-calendar\"></i> + Date range');
      }
      
      // Notify Shiny
      Shiny.setInputValue('oaShowDateRange', dateRangeVisible);
    });
    
    // Handle advanced filters toggle
    $('#oaToggleAdvanced').on('click', function() {
      advancedVisible = !advancedVisible;
      $('#oaAdvancedSection').slideToggle(300);
      
      // Update button text
      if (advancedVisible) {
        $(this).html('<i class=\"fa fa-filter\"></i> Hide advanced filters');
      } else {
        $(this).html('<i class=\"fa fa-filter\"></i> Show advanced filters');
      }
      
      // Notify Shiny
      Shiny.setInputValue('oaShowAdvanced', advancedVisible);
    });
    
    // Set initial button labels
    $('#oaToggleDateRange').html('<i class=\"fa fa-calendar\"></i> - Date range');
    $('#oaToggleAdvanced').html('<i class=\"fa fa-filter\"></i> Hide advanced filters');
  });
  
  // Custom message handler to collapse panel when data is loaded
  Shiny.addCustomMessageHandler('oaCollapseQueryPanel', function(message) {
    $('#oaQueryContent').slideUp(300);
    $('#oaToggleQueryPanel').html('<i class=\"fa fa-chevron-down\"></i>');
  });
"
    ))
  )
}


# ==============================================================================
# Server Component - WebOfScience-like Query Builder
# ==============================================================================

openAlexServer <- function(input, output, session, values) {
  # Reactive values to track query rows and toggle states
  queryRows <- reactiveVal(1)
  showDateRange <- reactiveVal(TRUE)
  showAdvanced <- reactiveVal(TRUE)
  downloadProgress <- reactiveVal(NULL)

  # Toggle date range visibility
  observeEvent(input$oaToggleDateRange, {
    current <- showDateRange()
    showDateRange(!current)
    session$sendCustomMessage('oaToggleDateRange', !current)
  })

  # Toggle advanced filters visibility
  observeEvent(input$oaToggleAdvanced, {
    current <- showAdvanced()
    showAdvanced(!current)
    session$sendCustomMessage('oaToggleAdvanced', !current)
  })

  # Add new query row
  observeEvent(input$oaAddRow, {
    currentRows <- queryRows()
    newRowId <- currentRows + 1
    queryRows(newRowId)

    # Insert new row in UI (WITH operator column)
    insertUI(
      selector = "#query-builder-container",
      where = "beforeEnd",
      ui = div(
        id = paste0("query-row-", newRowId),
        class = "query-row",
        style = "margin-bottom: 10px;",
        fluidRow(
          column(
            width = 1,
            selectInput(
              paste0("oaOperator_", newRowId),
              NULL,
              choices = c("And" = "AND", "Or" = "OR", "Not" = "NOT"),
              selected = "AND",
              width = "100%"
            )
          ),
          column(
            width = 2,
            selectInput(
              paste0("oaField_", newRowId),
              NULL,
              choices = c(
                "All Fields" = "default",
                "Title" = "title",
                "Abstract" = "abstract",
                "Title and Abstract" = "title_abstract",
                "Author" = "author",
                "Concept" = "concept"
              ),
              selected = "default",
              width = "100%"
            )
          ),
          column(
            width = 8,
            textInput(
              paste0("oaQuery_", newRowId),
              NULL,
              value = "",
              placeholder = 'Example: "deep learning"',
              width = "100%"
            )
          ),
          column(
            width = 1,
            actionButton(
              paste0("oaRemoveRow_", newRowId),
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
      runjs("$('#remove-button-1').css('visibility', 'visible');")
    }

    # Add observer for the remove button of the new row
    local({
      rowId <- newRowId
      observeEvent(input[[paste0("oaRemoveRow_", rowId)]], {
        removeUI(selector = paste0("#query-row-", rowId))

        # Check if only one row remains
        remainingRows <- length(
          lapply(1:queryRows(), function(i) {
            input[[paste0("oaQuery_", i)]]
          })
        )

        if (remainingRows == 1) {
          runjs("$('#remove-button-1').css('visibility', 'hidden');")
        }
      })
    })
  })

  # Clear all query fields
  observeEvent(input$oaClearQuery, {
    # Reset all query inputs
    for (i in 1:queryRows()) {
      updateTextInput(session, paste0("oaQuery_", i), value = "")
      updateSelectInput(session, paste0("oaField_", i), selected = "default")
      if (i > 1) {
        updateSelectInput(session, paste0("oaOperator_", i), selected = "AND")
      }
    }

    # Reset filters
    updateNumericInput(session, "oaYearFrom", value = 2020)
    updateNumericInput(
      session,
      "oaYearTo",
      value = as.integer(format(Sys.Date(), "%Y"))
    )
    updateSelectizeInput(session, "oaDocType", selected = "")
    updateSelectizeInput(session, "oaLanguage", selected = "")
    updateNumericInput(session, "oaMaxRecords", value = 1000)
  })

  # Helper function to get field name for filter
  get_filter_name <- function(field) {
    switch(
      field,
      "title" = "title.search",
      "abstract" = "abstract.search",
      "title_abstract" = "title_and_abstract.search",
      "author" = "authorships.author.display_name.search",
      "concept" = "concepts.display_name.search",
      "default" = "default.search"
    )
  }

  # Helper function to build query filters
  build_query_filters <- function(queryList) {
    if (length(queryList) == 0) {
      return(list())
    }

    # Group queries by field
    field_queries <- list()

    for (i in seq_along(queryList)) {
      item <- queryList[[i]]
      field <- item$field
      filter_name <- get_filter_name(field)

      if (is.null(field_queries[[filter_name]])) {
        # First query for this field
        field_queries[[filter_name]] <- list(
          terms = c(item$query),
          operators = c()
        )
      } else {
        # Add operator and term
        field_queries[[filter_name]]$operators <- c(
          field_queries[[filter_name]]$operators,
          item$operator
        )
        field_queries[[filter_name]]$terms <- c(
          field_queries[[filter_name]]$terms,
          item$query
        )
      }
    }

    # Build the filter arguments
    search_filters <- list()

    for (filter_name in names(field_queries)) {
      field_data <- field_queries[[filter_name]]

      if (length(field_data$terms) == 1) {
        # Single term - just use it as is
        search_filters[[filter_name]] <- field_data$terms[1]
      } else {
        # Multiple terms - combine with operators
        query_parts <- c(field_data$terms[1])

        for (j in seq_along(field_data$operators)) {
          operator <- field_data$operators[j]
          term <- field_data$terms[j + 1]
          query_parts <- c(query_parts, operator, term)
        }

        # Combine into single string
        combined_query <- paste(query_parts, collapse = " ")
        search_filters[[filter_name]] <- combined_query
      }
    }

    return(search_filters)
  }

  # Reactive query count - updates automatically when query changes
  output$oaQueryCountInfo <- renderUI({
    # Make this reactive to query inputs
    lapply(1:queryRows(), function(i) {
      input[[paste0("oaQuery_", i)]]
      input[[paste0("oaField_", i)]]
      if (i > 1) input[[paste0("oaOperator_", i)]]
    })

    # Also react to filters
    input$oaYearFrom
    input$oaYearTo
    input$oaDocType
    input$oaLanguage

    # Collect all query rows
    queryList <- list()
    for (i in 1:queryRows()) {
      queryText <- input[[paste0("oaQuery_", i)]]
      if (!is.null(queryText) && queryText != "") {
        queryList[[i]] <- list(
          query = queryText,
          field = input[[paste0("oaField_", i)]],
          operator = if (i > 1) input[[paste0("oaOperator_", i)]] else NULL
        )
      }
    }

    # If no query, show nothing or a hint
    if (length(queryList) == 0) {
      return(
        div(
          style = "display: inline-block; padding: 5px 10px; color: #6c757d; font-size: 13px; font-style: italic;",
          icon("info-circle"),
          " Enter search terms to see record count"
        )
      )
    }

    # Show loading state while fetching
    tryCatch(
      {
        # Build the query filters grouped by field
        search_filters <- build_query_filters(queryList)

        # Add date range filter if visible
        if (showDateRange()) {
          if (!is.na(input$oaYearFrom)) {
            search_filters$from_publication_date <- paste0(
              input$oaYearFrom,
              "-01-01"
            )
          }
          if (!is.na(input$oaYearTo)) {
            search_filters$to_publication_date <- paste0(
              input$oaYearTo,
              "-12-31"
            )
          }
        }

        # Add advanced filters if visible
        if (showAdvanced()) {
          if (!is.null(input$oaDocType) && length(input$oaDocType) > 0) {
            # Rimuovi eventuali stringhe vuote
            doc_types <- input$oaDocType[input$oaDocType != ""]
            if (length(doc_types) > 0) {
              search_filters$type <- doc_types
            }
          }

          if (!is.null(input$oaLanguage) && length(input$oaLanguage) > 0) {
            # Rimuovi eventuali stringhe vuote
            languages <- input$oaLanguage[input$oaLanguage != ""]
            if (length(languages) > 0) {
              search_filters$language <- languages
            }
          }
        }

        # Get the count of available records
        count_result <- do.call(
          openalexR::oa_fetch,
          c(
            list(
              entity = "works",
              count_only = TRUE,
              verbose = FALSE
            ),
            search_filters
          )
        )

        total_available <- as.integer(count_result$count)

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

  # Download data from OpenAlex
  observeEvent(input$oaFetchData, {
    # Collect all query rows
    queryList <- list()
    for (i in 1:queryRows()) {
      queryText <- input[[paste0("oaQuery_", i)]]
      if (!is.null(queryText) && queryText != "") {
        queryList[[i]] <- list(
          query = queryText,
          field = input[[paste0("oaField_", i)]],
          operator = if (i > 1) input[[paste0("oaOperator_", i)]] else NULL
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

    # Show progress modal with dynamic content
    showModal(modalDialog(
      title = "Downloading from OpenAlex",
      div(
        div(
          style = "text-align: center; margin: 20px 0;",
          icon("spinner", class = "fa-spin fa-3x")
        ),
        div(
          id = "download-progress-text",
          style = "text-align: center; font-size: 14px;",
          "Initializing query..."
        )
      ),
      footer = NULL,
      easyClose = FALSE
    ))

    tryCatch(
      {
        # Build the query filters grouped by field
        search_filters <- build_query_filters(queryList)

        # Add date range filter if visible
        if (showDateRange()) {
          if (!is.na(input$oaYearFrom)) {
            search_filters$from_publication_date <- paste0(
              input$oaYearFrom,
              "-01-01"
            )
          }
          if (!is.na(input$oaYearTo)) {
            search_filters$to_publication_date <- paste0(
              input$oaYearTo,
              "-12-31"
            )
          }
        }

        # Add advanced filters if visible
        if (showAdvanced()) {
          if (!is.null(input$oaDocType) && length(input$oaDocType) > 0) {
            # Rimuovi eventuali stringhe vuote
            doc_types <- input$oaDocType[input$oaDocType != ""]
            if (length(doc_types) > 0) {
              search_filters$type <- doc_types
            }
          }

          if (!is.null(input$oaLanguage) && length(input$oaLanguage) > 0) {
            # Rimuovi eventuali stringhe vuote
            languages <- input$oaLanguage[input$oaLanguage != ""]
            if (length(languages) > 0) {
              search_filters$language <- languages
            }
          }
        }

        # Calculate pages needed
        max_records <- min(input$oaMaxRecords, 10000)
        pages_needed <- ceiling(max_records / 200)

        print("Search filters:")
        print(search_filters)

        # First, get the count of available records
        count_result <- do.call(
          openalexR::oa_fetch,
          c(
            list(
              entity = "works",
              count_only = TRUE,
              verbose = FALSE
            ),
            search_filters
          )
        )

        total_available <- as.integer(count_result$count)
        records_to_download <- min(total_available, max_records)

        # Update progress modal with count information
        shinyjs::html(
          "download-progress-text",
          sprintf(
            "Found %s records. Downloading %s records...",
            format(total_available, big.mark = ","),
            format(records_to_download, big.mark = ",")
          )
        )

        # Fetch data from OpenAlex as list
        oa_data <- do.call(
          openalexR::oa_fetch,
          c(
            list(
              entity = "works",
              output = "list",
              abstract = TRUE,
              per_page = 200,
              pages = 1:pages_needed,
              verbose = TRUE
            ),
            search_filters
          )
        )

        # Update progress
        shinyjs::html(
          "download-progress-text",
          sprintf(
            "Converting %s records...",
            format(length(oa_data), big.mark = ",")
          )
        )

        # Create temporary directory if it doesn't exist
        temp_dir <- tempdir()
        temp_file <- file.path(
          temp_dir,
          paste0("openalex_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rdata")
        )

        # Save the list as .rdata file
        save(oa_data, file = temp_file)

        # Convert using bibliometrix convert2df function
        M <- convert2df(
          file = temp_file,
          dbsource = "openalex_api",
          format = "api"
        )

        # Clean up temporary file
        unlink(temp_file)

        # Limit to max records if necessary
        if (nrow(M) > max_records) {
          M <- M[1:max_records, ]
        }

        # Close progress modal
        removeModal()

        # Collapse query panel after successful download
        session$sendCustomMessage('oaCollapseQueryPanel', TRUE)

        # Show success message
        showModal(modalDialog(
          title = "Success",
          div(
            p(
              sprintf(
                "Successfully downloaded %s records from OpenAlex.",
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

        values = initial(values)
        M <- M %>% mergeKeywords(force = F)
        # Update values
        values$data_source <- "OpenAlex"
        values$M <- M
        values$Morig = M
        values$SCdf <- wcTable(M)
        values$COdf <- countryTable(M)
        values$Histfield = "NA"
        values$results = list("NA")
        if (ncol(values$M) > 1) {
          values$rest_sidebar <- TRUE
        }
        if (ncol(values$M) > 1) {
          values$loadMenu <- "openalex_api"
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
  output$oaDataTable <- DT::renderDataTable({
    req(values$M)
    req(values$data_source == "OpenAlex")

    MData = as.data.frame(apply(values$M, 2, function(x) {
      substring(x, 1, 150)
    }))
    MData$DOI <-
      paste0(
        '<a href=\"https://doi.org/',
        MData$DI,
        '\" target=\"_blank\">',
        MData$DI,
        '</a>'
      )
    nome = c("DOI", names(MData)[-length(names(MData))])
    MData = MData[nome]
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
  output$oaDataAvailable <- reactive({
    !is.null(values$M) && values$data_source == "OpenAlex"
  })
  outputOptions(output, "oaDataAvailable", suspendWhenHidden = FALSE)

  # Display progress information
  output$oaProgressInfo <- renderUI({
    if (!is.null(values$M) && values$data_source == "OpenAlex") {
      div(
        class = "alert alert-success",
        icon("check-circle"),
        strong(" Data loaded: "),
        sprintf(
          "%s documents from OpenAlex",
          format(nrow(values$M), big.mark = ",")
        )
      )
    }
  })

  # Download handler for Excel
  output$oaDownloadExcel <- downloadHandler(
    filename = function() {
      paste0("openalex_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(values$M)
      req(values$data_source == "OpenAlex")

      # Write data to Excel file
      writexl::write_xlsx(values$M, path = file)
    }
  )

  # Download handler for RData
  output$oaDownloadRData <- downloadHandler(
    filename = function() {
      paste0("openalex_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".RData")
    },
    content = function(file) {
      req(values$M)
      req(values$data_source == "OpenAlex")

      # Save data as RData file
      M <- values$M
      save(M, file = file)
    }
  )

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
      ),
    )
  }
}
