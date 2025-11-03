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
                label = "+ Date range",
                icon = icon("calendar"),
                class = "btn-outline-primary btn-sm",
                style = "margin-left: 10px;"
              )
            ),

            # Date Range Section (with shinyjs toggle instead of conditionalPanel)
            shinyjs::hidden(
              div(
                id = "pmDateRangePanel",
                wellPanel(
                  style = "background-color: #f8f9fa;",
                  h5("Date Range Filter", style = "font-weight: bold;"),
                  fluidRow(
                    column(
                      width = 6,
                      numericInput(
                        "pmYearFrom",
                        "From Year:",
                        value = 2020,
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
              )
            ),

            # Advanced filters toggle
            actionButton(
              "pmToggleAdvanced",
              label = "Advanced filters",
              icon = icon("filter"),
              class = "btn-link",
              style = "padding: 5px; margin-bottom: 10px;"
            ),

            # Additional Filters Section (with shinyjs toggle instead of conditionalPanel)
            shinyjs::hidden(
              div(
                id = "pmAdvancedPanel",
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
              )
            ),

            # Action buttons
            div(
              style = "margin-top: 10px; text-align: right;",
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
                class = "btn-primary",
                style = "margin-left: 10px;"
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

            # Data preview
            div(
              style = "margin-top: 20px;",
              h5("Data Preview", style = "font-weight: bold;"),
              DT::DTOutput("pmDataTable")
            )
          )
        )
      )
    )
  )
}

# ==============================================================================
# Server Logic for PubMed API
# ==============================================================================

pubmedServer <- function(input, output, session, values) {
  # Reactive values to track query rows and toggle states
  pmQueryRows <- reactiveVal(1)
  pmShowDateRange <- reactiveVal(FALSE)
  pmShowAdvanced <- reactiveVal(FALSE)

  # Toggle query panel collapse
  observeEvent(input$pmToggleQueryPanel, {
    session$sendCustomMessage('pmToggleQueryPanel', TRUE)
  })

  # Toggle date range visibility
  observeEvent(input$pmToggleDateRange, {
    currentState <- pmShowDateRange()
    pmShowDateRange(!currentState)
    updateActionButton(
      session,
      "pmToggleDateRange",
      label = if (!currentState) "- Date range" else "+ Date range"
    )
    shinyjs::toggle("pmDateRangePanel", anim = TRUE) # Usa l'ID corretto
  })

  # Toggle advanced filters visibility
  observeEvent(input$pmToggleAdvanced, {
    currentState <- pmShowAdvanced()
    pmShowAdvanced(!currentState)
    updateActionButton(
      session,
      "pmToggleAdvanced",
      label = if (!currentState) "Hide advanced filters" else "Advanced filters"
    )
    shinyjs::toggle("pmAdvancedPanel", anim = TRUE) # Usa l'ID corretto
  })

  # Add new query row
  observeEvent(input$pmAddRow, {
    currentRows <- pmQueryRows()
    newRowNum <- currentRows + 1
    pmQueryRows(newRowNum)

    insertUI(
      selector = "#pm-query-builder-container",
      where = "beforeEnd",
      ui = div(
        id = paste0("pm-query-row-", newRowNum),
        class = "pm-query-row",
        style = "margin-bottom: 10px;",
        fluidRow(
          column(
            width = 1,
            selectInput(
              paste0("pmOperator_", newRowNum),
              NULL,
              choices = c("AND", "OR", "NOT"),
              selected = "AND",
              width = "100%"
            )
          ),
          column(
            width = 2,
            selectInput(
              paste0("pmField_", newRowNum),
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
              paste0("pmQuery_", newRowNum),
              NULL,
              value = "",
              placeholder = 'Example: "deep learning"',
              width = "100%"
            )
          ),
          column(
            width = 1,
            actionButton(
              paste0("pmRemoveRow_", newRowNum),
              icon("minus-circle"),
              class = "btn-danger btn-sm",
              style = "margin-top: 0px;",
              title = "Remove row",
              onclick = sprintf(
                "Shiny.setInputValue('pmRemoveRow', %d, {priority: 'event'})",
                newRowNum
              )
            )
          )
        )
      )
    )
  })

  # Remove query row
  observeEvent(input$pmRemoveRow, {
    rowNum <- input$pmRemoveRow
    removeUI(selector = paste0("#pm-query-row-", rowNum))
  })

  # Clear all fields
  observeEvent(input$pmClearQuery, {
    currentRows <- pmQueryRows()

    # Reset all query fields
    for (i in 1:currentRows) {
      if (i == 1) {
        updateTextInput(session, "pmQuery_1", value = "")
        updateSelectInput(session, "pmField_1", selected = "tiab")
      } else {
        removeUI(selector = paste0("#pm-query-row-", i))
      }
    }
    pmQueryRows(1)

    # Reset date range
    updateNumericInput(session, "pmYearFrom", value = 2020)
    updateNumericInput(
      session,
      "pmYearTo",
      value = as.integer(format(Sys.Date(), "%Y"))
    )

    # Reset advanced filters
    updateSelectizeInput(session, "pmDocType", selected = "")
    updateSelectizeInput(session, "pmLanguage", selected = "")
    updateNumericInput(session, "pmMaxRecords", value = 1000)
  })

  # Build PubMed query from multiple rows with advanced filters
  buildPubMedQuery <- function() {
    query_parts <- c()
    currentRows <- pmQueryRows()

    # Collect all query rows
    for (i in 1:currentRows) {
      field_id <- paste0("pmField_", i)
      query_id <- paste0("pmQuery_", i)
      operator_id <- paste0("pmOperator_", i)

      field_value <- input[[field_id]]
      query_text <- input[[query_id]]

      if (!is.null(query_text) && nchar(trimws(query_text)) > 0) {
        # Map field to PubMed tag
        field_tag <- switch(
          field_value,
          "all" = "[All Fields]",
          "title" = "[Title]",
          "abstract" = "[Abstract]",
          "tiab" = "[Title/Abstract]",
          "author" = "[Author]",
          "mesh" = "[MeSH Terms]",
          "affiliation" = "[Affiliation]",
          "[All Fields]"
        )

        # Add operator for rows after the first
        if (i > 1) {
          operator <- input[[operator_id]]
          if (!is.null(operator)) {
            query_parts <- c(query_parts, operator)
          }
        }

        # Add query with field tag
        query_parts <- c(query_parts, paste0("(", query_text, ")", field_tag))
      }
    }

    # Combine all parts
    base_query <- paste(query_parts, collapse = " ")

    # Add date range filter if enabled: YYYY:YYYY[DP]
    if (
      pmShowDateRange() &&
        !is.null(input$pmYearFrom) &&
        !is.null(input$pmYearTo)
    ) {
      date_filter <- sprintf("%s:%s[DP]", input$pmYearFrom, input$pmYearTo)
      if (nchar(base_query) > 0) {
        base_query <- paste(base_query, "AND", date_filter)
      } else {
        base_query <- date_filter
      }
    }

    # Add language filter: [LA]
    if (
      pmShowAdvanced() &&
        !is.null(input$pmLanguage) &&
        length(input$pmLanguage) > 0 &&
        input$pmLanguage[1] != ""
    ) {
      languages <- paste(
        sapply(input$pmLanguage, function(x) paste0(x, "[LA]")),
        collapse = " OR "
      )
      if (nchar(base_query) > 0) {
        base_query <- paste(base_query, "AND (", languages, ")")
      } else {
        base_query <- paste("(", languages, ")")
      }
    }

    # Add document type filter: [PT]
    if (
      pmShowAdvanced() &&
        !is.null(input$pmDocType) &&
        length(input$pmDocType) > 0 &&
        input$pmDocType[1] != ""
    ) {
      doc_types <- paste(
        sapply(input$pmDocType, function(x) paste0(x, "[PT]")),
        collapse = " OR "
      )
      if (nchar(base_query) > 0) {
        base_query <- paste(base_query, "AND (", doc_types, ")")
      } else {
        base_query <- paste("(", doc_types, ")")
      }
    }

    return(base_query)
  }

  # Fetch data from PubMed
  observeEvent(input$pmFetchData, {
    # Build query with all filters
    query <- buildPubMedQuery()

    # Validate query
    if (is.null(query) || nchar(trimws(query)) == 0) {
      showModal(modalDialog(
        title = "Invalid Query",
        "Please enter at least one search term.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }

    # Get max records from advanced filters
    max_records <- input$pmMaxRecords
    if (is.null(max_records)) {
      max_records <- 1000
    }

    # Show progress modal
    showModal(modalDialog(
      title = "Downloading from PubMed",
      div(
        id = "pm-download-progress",
        div(
          class = "progress",
          style = "height: 25px;",
          div(
            class = "progress-bar progress-bar-striped progress-bar-animated",
            role = "progressbar",
            style = "width: 100%",
            "Searching PubMed..."
          )
        ),
        div(
          id = "pm-download-progress-text",
          style = "margin-top: 15px; text-align: center;",
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
