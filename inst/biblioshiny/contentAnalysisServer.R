## SERVER components for contentanalysis in Biblioshiny ----

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
    pdf_name <- paste0(
      "viewer_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      "_",
      input$pdf_file$name
    )
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
    files <- list.files(
      tmpdir,
      pattern = "^viewer_.*\\.pdf$",
      full.names = TRUE
    )
    file.remove(files)
  })

  ## END PDF PREVIEW

  # Extract text from PDF
  observeEvent(input$extract_text, {
    req(input$pdf_file)

    # VALIDATION: Check if citation type is selected
    if (
      is.null(input$citation_type_import) ||
        input$citation_type_import == "" ||
        length(input$citation_type_import) == 0
    ) {
      showNotification(
        HTML(
          "<strong>Citation Format Required</strong><br/>Please select the citation format used in your PDF before extracting text."
        ),
        type = "warning",
        duration = 5
      )

      return() # Stop execution
    }

    tryCatch(
      {
        if (is.na(input$Columns)) {
          n_columns <- NULL
        } else {
          n_columns <- input$Columns
        }

        # Get citation type from input
        citation_type <- input$citation_type_import

        # Extract text with citation type parameter
        pdf_text <- pdf2txt_auto(
          input$pdf_file$datapath,
          n_columns = n_columns,
          citation_type = citation_type,
          enable_ai_support = input$enable_ai_support,
          ai_model = values$gemini_api_model,
          api_key = NULL
        )

        values$pdf_text <- pdf_text[[1]]

        # Extract PDF metadata (authors, title, journal, year, doi)
        pdf_metadata_info <- tryCatch(
          {
            contentanalysis::extract_pdf_metadata(
              input$pdf_file$datapath,
              fields = "all"
            )
          },
          error = function(e) {
            NULL
          }
        )

        # Store metadata in values
        values$pdf_metadata <- pdf_metadata_info

        # Check if AI support is enabled
        if (input$enable_ai_support) {
          if (!is.null(pdf_text)) {
            showNotification(
              "Text extracted successfully with AI enhancement!",
              type = "message",
              duration = 3
            )
          }
        }

        values$pdf_sections <- pdf_text

        values$citation_type_used <- citation_type # Store for later use

        pdf_metadata <- unlist(pdftools::pdf_info(input$pdf_file$datapath))

        # Extract DOI
        extracted_doi <- tryCatch(
          {
            extract_doi_from_pdf(input$pdf_file$datapath)
          },
          error = function(e) {
            NULL
          }
        )

        # Store DOI and update input field
        if (
          !is.null(extracted_doi) &&
            !is.na(extracted_doi) &&
            nzchar(trimws(extracted_doi))
        ) {
          values$pdf_doi <- trimws(extracted_doi)
          updateTextInput(
            session,
            "pdf_doi_input",
            value = trimws(extracted_doi)
          )
        } else {
          values$pdf_doi <- ""
          updateTextInput(session, "pdf_doi_input", value = "")
        }

        # Create notification message based on citation type
        notification_msg <- if (
          !is.null(extracted_doi) && nzchar(trimws(extracted_doi))
        ) {
          "PDF text extracted successfully! DOI detected and populated."
        } else {
          "PDF text extracted successfully! Please enter DOI manually if needed."
        }

        # Add citation type info to notification
        citation_type_label <- switch(
          citation_type,
          "author_year" = "Author-year",
          "numeric_bracketed" = "Numeric brackets",
          "numeric_superscript" = "Numeric superscript",
          "all" = "All formats"
        )

        if (citation_type == "numeric_superscript") {
          notification_msg <- paste0(
            notification_msg,
            " Superscript citations converted to [n] format."
          )
        }

        notification_msg <- paste0(
          notification_msg,
          " (Citation format: ",
          citation_type_label,
          ")"
        )

        showNotification(
          notification_msg,
          type = "message",
          duration = 5
        )

        updateActionButton(
          session,
          "run_analysis",
          label = "Start",
          icon = icon("play")
        )
      },
      error = function(e) {
        showNotification(
          paste("Error extracting PDF text:", e$message),
          type = "error",
          duration = 5
        )
      }
    )
  })

  # Disable/Enable extract button based on citation type selection
  observe({
    if (
      !is.null(input$citation_type_import) &&
        input$citation_type_import != "" &&
        length(input$citation_type_import) > 0
    ) {
      # Citation type selected - ensure button is enabled
      shinyjs::enable("extract_text")
      shinyjs::removeCssClass("extract_text", "btn-secondary")
      shinyjs::addCssClass("extract_text", "btn-info")
    } else {
      # No citation type selected - visual feedback but don't disable
      # (We handle this in the observeEvent instead)
      shinyjs::removeCssClass("extract_text", "btn-info")
      shinyjs::addCssClass("extract_text", "btn-secondary")
    }
  })

  # Display PDF metadata (authors, title, journal) - compact version for top-right
  output$pdf_metadata_display <- renderUI({
    if (!is.null(values$pdf_metadata)) {
      metadata <- values$pdf_metadata

      # Extract metadata fields with safe defaults
      authors <- if (
        !is.null(metadata$authors) &&
          nzchar(metadata$authors) &&
          !is.na(metadata$authors)
      ) {
        if (nchar(metadata$authors) > 120) {
          paste0(substr(metadata$authors, 1, 117), "...")
        } else {
          metadata$authors
        }
      } else {
        "Authors not available"
      }

      title <- if (
        !is.null(metadata$title) &&
          nzchar(metadata$title) &&
          !is.na(metadata$title)
      ) {
        # Truncate title if too long
        if (nchar(metadata$title) > 120) {
          paste0(substr(metadata$title, 1, 117), "...")
        } else {
          metadata$title
        }
      } else {
        "Title not available"
      }

      journal <- if (
        !is.null(metadata$journal) &&
          nzchar(metadata$journal) &&
          !is.na(metadata$journal)
      ) {
        metadata$journal
      } else {
        "Journal not available"
      }

      year <- if (
        !is.null(metadata$year) &&
          !is.na(metadata$year) &&
          !is.na(metadata$year)
      ) {
        as.character(metadata$year)
      } else {
        ""
      }

      # Build compact display for top-right corner
      div(
        # style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 12px 15px; border-radius: 6px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); color: white; text-align: left; font-size: 12px;",
        style = "background: linear-gradient(135deg, #1D8FE1 0%, #5765B1 100%); padding: 12px 15px; border-radius: 6px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); color: white; text-align: left; font-size: 12px;",

        # Title (most prominent)
        div(
          style = "margin-bottom: 6px; font-weight: 600; font-size: 13px; line-height: 1.3;",
          title
        ),

        # Authors
        div(
          style = "margin-bottom: 4px; opacity: 0.95;",
          icon("user", style = "margin-right: 5px; font-size: 11px;"),
          tags$span(authors, style = "font-size: 11px;")
        ),

        # Journal and Year
        div(
          style = "opacity: 0.9;",
          icon("book", style = "margin-right: 5px; font-size: 11px;"),
          tags$span(
            paste0(journal, if (nzchar(year)) paste0(" (", year, ")") else ""),
            style = "font-size: 11px;"
          )
        )
      )
    }
  })

  # Check if metadata is available
  output$metadata_available <- reactive({
    return(!is.null(values$pdf_metadata))
  })
  outputOptions(output, "metadata_available", suspendWhenHidden = FALSE)

  # Check if DOI was detected automatically
  output$doi_detected <- reactive({
    if (!is.null(values$pdf_doi) && nzchar(values$pdf_doi)) {
      # Check if it's the same as what was automatically extracted
      extracted_doi <- tryCatch(
        {
          if (!is.null(input$pdf_file)) {
            extract_doi_from_pdf(input$pdf_file$datapath)
          } else {
            NULL
          }
        },
        error = function(e) NULL
      )

      return(
        !is.null(extracted_doi) &&
          nzchar(extracted_doi) &&
          extracted_doi == input$pdf_doi_input
      )
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
          icon(
            "exclamation-triangle",
            style = "color: #e74c3c; font-size: 18px; margin-right: 5px;"
          ),
          tags$small(
            "DOI not found",
            style = "color: #e74c3c; font-weight: bold;"
          )
        )
      )
    }

    # C'è un DOI, verifica se è stato auto-rilevato
    auto_detected <- FALSE

    if (!is.null(input$pdf_file)) {
      extracted_doi <- tryCatch(
        {
          extract_doi_from_pdf(input$pdf_file$datapath)
        },
        error = function(e) {
          NULL
        }
      )

      # Verifica se il DOI estratto corrisponde a quello inserito
      if (
        !is.null(extracted_doi) &&
          !is.na(extracted_doi) &&
          nzchar(trimws(extracted_doi))
      ) {
        auto_detected <- (trimws(extracted_doi) == trimws(doi_value))
      }
    }

    # Restituisci l'icona appropriata
    if (auto_detected) {
      return(
        tagList(
          icon(
            "check-circle",
            style = "color: #27ae60; font-size: 18px; margin-right: 5px;"
          ),
          tags$small(
            "Auto-detected",
            style = "color: #27ae60; font-weight: bold;"
          )
        )
      )
    } else {
      return(
        tagList(
          icon(
            "edit",
            style = "color: #f39c12; font-size: 18px; margin-right: 5px;"
          ),
          tags$small(
            "Manually entered",
            style = "color: #f39c12; font-weight: bold;"
          )
        )
      )
    }
  })

  # Update DOI value when user edits the field
  observeEvent(
    input$pdf_doi_input,
    {
      if (!is.null(input$pdf_doi_input)) {
        values$pdf_doi <- trimws(input$pdf_doi_input)
      }
    },
    ignoreInit = TRUE
  )

  # ===========================================
  # LOAD PREVIOUSLY SAVED TEXT FILE
  # ===========================================

  # Check if text file is loaded
  output$text_file_loaded <- reactive({
    return(!is.null(input$load_text_file))
  })
  outputOptions(output, "text_file_loaded", suspendWhenHidden = FALSE)

  # Display text file info
  output$text_file_info <- renderText({
    if (!is.null(input$load_text_file)) {
      file_size <- round(input$load_text_file$size / 1024, 2)
      paste("File:", input$load_text_file$name, "| Size:", file_size, "KB")
    }
  })

  observeEvent(input$load_text_file, {
    req(input$load_text_file)

    tryCatch(
      {
        # Read the text file
        file_content <- readLines(input$load_text_file$datapath, warn = FALSE)

        if (length(file_content) == 0) {
          showNotification(
            "The text file is empty.",
            type = "error",
            duration = 5
          )
          return()
        }

        # Extract DOI from first line if present (format: "DOI: xxxxx")
        first_line <- file_content[1]
        extracted_doi <- ""
        text_start_line <- 1

        if (grepl("^DOI:\\s*", first_line, ignore.case = TRUE)) {
          extracted_doi <- sub("^DOI:\\s*", "", first_line, ignore.case = TRUE)
          extracted_doi <- trimws(extracted_doi)
          text_start_line <- 2
        }

        # Extract CITATION_TYPE from second line if present (format: "CITATION_TYPE: xxxxx")
        extracted_citation_type <- ""
        if (length(file_content) >= 2) {
          second_line <- file_content[2]
          if (grepl("^CITATION_TYPE:\\s*", second_line, ignore.case = TRUE)) {
            extracted_citation_type <- sub(
              "^CITATION_TYPE:\\s*",
              "",
              second_line,
              ignore.case = TRUE
            )
            extracted_citation_type <- trimws(extracted_citation_type)
            text_start_line <- 3
          }
        }

        # Get the text content (everything after metadata lines)
        if (length(file_content) >= text_start_line) {
          text_content <- reconstruct_from_txt(
            file_content[text_start_line:length(file_content)]
          )
        } else {
          text_content <- ""
        }

        # Store the text
        values$pdf_text <- unlist(text_content[1])
        values$pdf_sections <- text_content

        # Update DOI field
        if (nzchar(extracted_doi)) {
          values$pdf_doi <- extracted_doi
          updateTextInput(session, "pdf_doi_input", value = extracted_doi)
        } else {
          values$pdf_doi <- ""
          updateTextInput(session, "pdf_doi_input", value = "")
        }

        # Update citation_type field if available
        if (nzchar(extracted_citation_type)) {
          values$citation_type_used <- extracted_citation_type
          # Note: citation_type_import is only used during PDF import, not needed here
        }

        # Show notification
        notification_msg <- "Text file loaded successfully!"
        if (nzchar(extracted_doi)) {
          notification_msg <- paste0(notification_msg, " DOI: ", extracted_doi)
        }
        if (nzchar(extracted_citation_type)) {
          citation_type_label <- switch(
            extracted_citation_type,
            "author_year" = "Author-year",
            "numeric_bracketed" = "Numeric brackets",
            "numeric_superscript" = "Numeric superscript",
            "all" = "All formats",
            extracted_citation_type
          )
          notification_msg <- paste0(
            notification_msg,
            " | Citation format: ",
            citation_type_label
          )
        }

        showNotification(
          notification_msg,
          type = "message",
          duration = 7
        )

        # Enable analysis button
        updateActionButton(
          session,
          "run_analysis",
          label = "Start",
          icon = icon("play")
        )
      },
      error = function(e) {
        showNotification(
          paste("Error loading text file:", e$message),
          type = "error",
          duration = 5
        )
      }
    )
  })

  # ===========================================
  # SAVE EXTRACTED TEXT TO FILE
  # ===========================================
  output$save_text_file <- downloadHandler(
    filename = function() {
      # Generate filename with DOI or timestamp
      if (!is.null(values$pdf_doi) && nzchar(values$pdf_doi)) {
        # Clean DOI for filename (replace / with _)
        clean_doi <- gsub("/", "_", values$pdf_doi)
        clean_doi <- gsub("[^a-zA-Z0-9._-]", "", clean_doi)
        paste0("extracted_text_", clean_doi, ".txt")
      } else {
        paste0("extracted_text_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
      }
    },
    content = function(file) {
      # Prepare content with DOI and CITATION_TYPE in first lines
      file_content <- ""

      # Add DOI as first line if available
      if (!is.null(values$pdf_doi) && nzchar(values$pdf_doi)) {
        file_content <- paste0("DOI: ", values$pdf_doi, "\n")
      } else {
        file_content <- "DOI: \n"
      }

      # Add CITATION_TYPE as second line if available
      if (
        !is.null(values$citation_type_used) && nzchar(values$citation_type_used)
      ) {
        file_content <- paste0(
          file_content,
          "CITATION_TYPE: ",
          values$citation_type_used,
          "\n"
        )
      } else {
        file_content <- paste0(file_content, "CITATION_TYPE: \n")
      }

      if (length(values$pdf_sections) > 1) {
        txt <- values$pdf_sections
        section_names <- setdiff(names(txt), "Full_text")
        # save txt in a text file adding a row with \n\nSECTION: section names\n\n before each section except Full_text
        #file_content <- ""
        for (section in section_names) {
          file_content <- paste0(
            file_content,
            "\n\nSECTION LINE SEPARATION: ",
            section,
            "\n\n",
            txt[[section]]
          )
        }
      } else {
        file_content <- paste0(file_content, unlist(values$pdf_sections[1]))
      }

      # # Add the extracted text
      # if (!is.null(values$pdf_text)) {
      #   file_content <- paste0(file_content, values$pdf_text)
      # }

      # Write to file
      writeLines(file_content, file, sep = "")
    }
  )

  # Text length information
  output$text_length_info <- renderText({
    if (!is.null(values$pdf_text)) {
      char_count <- nchar(values$pdf_text)
      word_count <- length(strsplit(values$pdf_text, "\\s+")[[1]])
      paste(
        "Characters:",
        format(char_count, big.mark = ","),
        "| Words:",
        format(word_count, big.mark = ",")
      )
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

        paste0(
          truncated,
          "\n\n[...text continues for ",
          format(nchar(full_text) - nchar(truncated), big.mark = ","),
          " more characters...]"
        )
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

    updateActionButton(
      session,
      "run_analysis",
      label = "Processing...",
      icon = icon("spinner", class = "fa-spin")
    )

    tryCatch(
      {
        custom_stops <- NULL
        if (
          !is.null(input$custom_stopwords) && nzchar(input$custom_stopwords)
        ) {
          custom_stops <- trimws(strsplit(input$custom_stopwords, ",")[[1]])
        }

        # Determine citation segmentation
        use_sections_cit <- switch(
          input$citation_segmentation %||% "auto",
          "auto" = "auto",
          "sections" = TRUE,
          "segments" = FALSE,
          "auto" # default fallback
        )

        n_segs_cit <- input$n_segments_citations %||% 10

        # Get citation type (use stored value from extraction or default)
        citation_type <- values$citation_type_used
        if (is.null(citation_type) || citation_type == "") {
          citation_type <- input$citation_type_import
          if (is.null(citation_type) || citation_type == "") {
            citation_type <- "author_year" # ultimate fallback
          }
        }

        # Run analysis with citation type parameter
        values$analysis_results <- analyze_scientific_content(
          text = values$pdf_sections,
          doi = values$pdf_doi,
          citation_type = citation_type,
          window_size = input$window_size,
          parse_multiple_citations = input$parse_multiple,
          remove_stopwords = input$remove_stopwords,
          custom_stopwords = custom_stops,
          use_sections_for_citations = use_sections_cit,
          n_segments_citations = n_segs_cit
        )

        # remove doi_pattern citations
        values$analysis_results$citation_contexts <- values$analysis_results$citation_contexts %>%
          dplyr::filter(!citation_type %in% c("doi_pattern"))

        values$references_oa <- values$analysis_results$references_oa
        section_colors <- colorlist()[
          1:length(unique(values$analysis_results$citation_contexts$section))
        ]
        names(section_colors) <- unique(
          values$analysis_results$citation_contexts$section
        )
        values$analysis_results$section_colors <- section_colors

        # Calculate readability indices
        values$readability_indices <- calculate_readability_indices(
          text = values$pdf_text,
          detailed = TRUE
        )

        # Create network if we have data
        if (
          !is.null(values$analysis_results$network_data) &&
            nrow(values$analysis_results$network_data) > 0
        ) {
          values$network_plot <- create_citation_network(
            values$analysis_results,
            max_distance = input$max_distance,
            show_labels = TRUE
          )
        }

        preview_visible(FALSE)

        updateActionButton(
          session,
          "run_analysis",
          label = "Start",
          icon = icon("play")
        )

        showNotification(
          paste0(
            "Content analysis completed successfully! Citation type: ",
            switch(
              citation_type,
              "author_year" = "Author-Year",
              "numeric_bracketed" = "Numeric Bracketed",
              "numeric_superscript" = "Numeric Superscript",
              "all" = "All Formats"
            )
          ),
          type = "message",
          duration = 4
        )

        # Close the PDF import box
        session$sendCustomMessage('togglePdfImport', list(action = 'close'))
      },
      error = function(e) {
        updateActionButton(
          session,
          "run_analysis",
          label = "Start",
          icon = icon("play")
        )

        showNotification(
          paste("Error in content analysis:", e$message),
          type = "error",
          duration = 5
        )
      }
    )
  })

  # Check if analysis is completed
  output$analysis_completed <- reactive({
    return(!is.null(values$analysis_results))
  })
  outputOptions(output, "analysis_completed", suspendWhenHidden = FALSE)

  # Check if Gemini API is available
  output$gemini_api_available <- reactive({
    return(!is.null(values$geminiAPI) && values$geminiAPI == TRUE)
  })
  outputOptions(output, "gemini_api_available", suspendWhenHidden = FALSE)

  # ===========================================
  # TAB 1: DESCRIPTIVE STATISTICS OUTPUTS
  # ===========================================

  output$total_words <- renderText({
    if (!is.null(values$analysis_results)) {
      format(
        values$analysis_results$summary$total_words_analyzed,
        big.mark = ","
      )
    } else {
      "0"
    }
  })

  output$total_citations <- renderText({
    if (!is.null(values$analysis_results)) {
      format(
        values$analysis_results$summary$citations_extracted,
        big.mark = ","
      )
    } else {
      "0"
    }
  })

  output$narrative_citations <- renderText({
    if (!is.null(values$analysis_results)) {
      format(
        values$analysis_results$summary$narrative_citations,
        big.mark = ","
      )
    } else {
      "0"
    }
  })

  output$citation_density <- renderText({
    if (!is.null(values$analysis_results)) {
      format(
        values$analysis_results$summary$citation_density_per_1000_words,
        digits = 1
      )
    } else {
      "0.0"
    }
  })

  # Readability indices outputs
  output$flesch_kincaid_grade <- renderText({
    if (!is.null(values$readability_indices)) {
      format(
        round(values$readability_indices$flesch_kincaid_grade, 1),
        nsmall = 1
      )
    } else {
      "0.0"
    }
  })

  output$flesch_reading_ease <- renderText({
    if (!is.null(values$readability_indices)) {
      format(
        round(values$readability_indices$flesch_reading_ease, 1),
        nsmall = 1
      )
    } else {
      "0.0"
    }
  })

  output$ari_index <- renderText({
    if (!is.null(values$readability_indices)) {
      format(
        round(values$readability_indices$automated_readability_index, 1),
        nsmall = 1
      )
    } else {
      "0.0"
    }
  })

  output$gunning_fog_index <- renderText({
    if (!is.null(values$readability_indices)) {
      format(round(values$readability_indices$gunning_fog_index, 1), nsmall = 1)
    } else {
      "0.0"
    }
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
        syllables_info <- format(
          values$readability_indices$n_syllables,
          big.mark = ","
        )
        complex_info <- sprintf(
          "Complex words:      %s (%.1f%%)",
          format(values$readability_indices$n_complex_words, big.mark = ","),
          values$readability_indices$pct_complex_words
        )
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
        switch(
          index_name,
          "flesch_kincaid" = if (value <= 12) {
            "✓"
          } else if (value <= 16) {
            "◆"
          } else {
            "▲"
          },
          "flesch_ease" = if (value >= 60) {
            "✓"
          } else if (value >= 30) {
            "◆"
          } else {
            "▲"
          },
          "ari" = if (value <= 12) {
            "✓"
          } else if (value <= 16) {
            "◆"
          } else {
            "▲"
          },
          "gunning" = if (value <= 12) {
            "✓"
          } else if (value <= 16) {
            "◆"
          } else {
            "▲"
          }
        )
      }

      # Helper to get interpretation text
      get_interp_text <- function(index_name, value) {
        switch(
          index_name,
          "flesch_kincaid" = paste("Grade", round(value)),
          "flesch_ease" = if (value >= 60) {
            "Easy"
          } else if (value >= 30) {
            "Moderate"
          } else {
            "Difficult"
          },
          "ari" = paste("Grade", round(value)),
          "gunning" = if (value <= 12) {
            "Readable"
          } else if (value <= 16) {
            "Difficult"
          } else {
            "Very difficult"
          }
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
  output$citation_types_table <- DT::renderDataTable(
    {
      if (
        !is.null(values$analysis_results) &&
          !is.null(values$analysis_results$citation_metrics$type_distribution)
      ) {
        values$analysis_results$citation_metrics$type_distribution
      } else {
        data.frame(
          citation_type = character(0),
          n = numeric(0),
          percentage = numeric(0)
        )
      }
    },
    options = list(
      pageLength = 10,
      dom = 't',
      ordering = FALSE,
      searching = FALSE
    )
  )

  # Citation sections table
  output$citation_sections_table <- DT::renderDataTable(
    {
      if (
        !is.null(values$analysis_results) &&
          !is.null(
            values$analysis_results$citation_metrics$section_distribution
          )
      ) {
        values$analysis_results$citation_metrics$section_distribution %>%
          filter(n > 0)
      } else {
        data.frame(
          section = character(0),
          n = numeric(0),
          percentage = numeric(0)
        )
      }
    },
    options = list(
      pageLength = 10,
      dom = 't',
      ordering = FALSE,
      searching = FALSE
    )
  )

  # Frequent words table
  output$frequent_words_table <- DT::renderDataTable(
    {
      if (!is.null(values$analysis_results)) {
        values$analysis_results$word_frequencies %>%
          slice_head(n = 15) %>%
          select(word, n)
      } else {
        data.frame(word = character(0), n = numeric(0))
      }
    },
    options = list(
      pageLength = 15,
      dom = 't',
      ordering = FALSE,
      searching = FALSE
    )
  )

  # Bigrams table
  output$bigrams_table <- DT::renderDataTable(
    {
      if (
        !is.null(values$analysis_results) &&
          "2gram" %in% names(values$analysis_results$ngrams)
      ) {
        values$analysis_results$ngrams$`2gram` %>%
          select(ngram, n) %>%
          slice_head(n = 15)
      } else {
        data.frame(ngram = character(0), n = numeric(0))
      }
    },
    options = list(
      pageLength = 15,
      dom = 't',
      ordering = FALSE,
      searching = FALSE
    )
  )

  # Trigrams table
  output$trigrams_table <- DT::renderDataTable(
    {
      if (
        !is.null(values$analysis_results) &&
          "3gram" %in% names(values$analysis_results$ngrams)
      ) {
        values$analysis_results$ngrams$`3gram` %>%
          select(ngram, n) %>%
          slice_head(n = 15)
      } else {
        data.frame(ngram = character(0), n = numeric(0))
      }
    },
    options = list(
      pageLength = 15,
      dom = 't',
      ordering = FALSE,
      searching = FALSE
    )
  )

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

    tryCatch(
      {
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
          section_colors <- colorlist()[
            1:length(unique(values$analysis_results$citation_contexts$section))
          ]
          names(section_colors) <- unique(
            values$analysis_results$citation_contexts$section
          )
          values$analysis_results$section_colors <- section_colors
        }

        # Update citation metrics - section distribution
        if (!is.null(values$analysis_results$citation_metrics)) {
          # Determine expected sections/segments
          sections_to_use <- NULL

          if (
            use_sections_cit == TRUE ||
              (use_sections_cit == "auto" &&
                length(setdiff(
                  names(values$pdf_sections),
                  c("Full_text", "References")
                )) >
                  0)
          ) {
            # Using sections
            sections_to_use <- setdiff(
              names(values$pdf_sections),
              c("Full_text", "References")
            )
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
        if (
          !is.null(values$analysis_results$network_data) &&
            nrow(values$analysis_results$network_data) > 0
        ) {
          values$network_plot <- create_citation_network(
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
      },
      error = function(e) {
        showNotification(
          paste("Error updating citation grouping:", e$message),
          type = "error",
          duration = 5
        )

        # Print error to console for debugging
        cat("Error in update_citation_grouping:\n")
        print(e)
      }
    )
  })

  # Custom HTML output for citation contexts
  output$citation_contexts_html <- renderUI({
    if (
      !is.null(values$analysis_results) &&
        !is.null(values$analysis_results$citation_contexts) &&
        nrow(values$analysis_results$citation_contexts) > 0
    ) {
      contexts <- values$analysis_results$citation_contexts

      # Apply filters (REMOVED context_type_filter)
      if (!is.null(input$context_search) && nzchar(input$context_search)) {
        contexts <- contexts %>%
          filter(
            str_detect(
              citation_text,
              regex(input$context_search, ignore_case = TRUE)
            ) |
              str_detect(
                full_context,
                regex(input$context_search, ignore_case = TRUE)
              )
          )
      }

      # REMOVED: type filter code

      if (!is.null(input$context_min_words)) {
        contexts <- contexts %>%
          filter(context_word_count >= input$context_min_words)
      }

      # Create HTML for each citation context
      if (nrow(contexts) > 0) {
        citation_boxes <- lapply(1:nrow(contexts), function(i) {
          context <- contexts[i, ]

          section_colors <- values$analysis_results$section_colors

          section_name <- if (
            !is.null(context$section) && !is.na(context$section)
          ) {
            context$section
          } else {
            "Unknown"
          }

          box_color <- section_colors[section_name]
          if (is.na(box_color)) {
            box_color <- "#CCCCCC"
          }

          has_reference <- FALSE
          tooltip_text <- "No reference matched for this citation"

          if ("ref_full_text" %in% names(context)) {
            ref_value <- context[["ref_full_text"]]

            if (
              length(ref_value) > 0 &&
                !is.na(ref_value) &&
                is.character(ref_value) &&
                nzchar(trimws(ref_value))
            ) {
              has_reference <- TRUE
              ref_text <- trimws(ref_value)
              ref_text <- gsub("&", "&amp;", ref_text)
              ref_text <- gsub("<", "&lt;", ref_text)
              ref_text <- gsub(">", "&gt;", ref_text)
              ref_text <- gsub('"', "&quot;", ref_text)
              ref_text <- gsub("'", "&#39;", ref_text)
              ref_text <- gsub("\n", "<br/>", ref_text)
              tooltip_text <- paste0(
                "<strong style='color: #4ECDC4;'>Reference:</strong><br/>",
                ref_text
              )
            }
          }

          citation_id <- paste0("citation_", i, "_", sample(1:10000, 1))

          div(
            style = paste0(
              "margin-bottom: 20px; padding: 15px; border-radius: 8px; ",
              "box-shadow: 0 2px 4px rgba(0,0,0,0.1); ",
              "border-left: 4px solid ",
              box_color,
              "; ",
              "background-color: #fafafa;"
            ),

            div(
              style = "margin-bottom: 10px; font-size: 12px; color: #666;",
              span(paste("Citation", i, "•"), style = "font-weight: bold;"),
              span(
                section_name,
                style = paste0(
                  "color: ",
                  box_color,
                  "; font-weight: bold; margin-left: 8px;"
                )
              ),
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
                  "background-color: ",
                  box_color,
                  "; color: white; ",
                  "padding: 8px 12px; border-radius: 6px; font-weight: bold; ",
                  "font-size: 14px; white-space: nowrap; max-width: 300px; ",
                  "overflow: hidden; text-overflow: ellipsis; ",
                  "cursor: ",
                  if (has_reference) "pointer" else "default",
                  "; position: relative;"
                ),
                `data-toggle` = if (has_reference) "tooltip" else NULL,
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
                span(
                  "Narrative citation",
                  style = "color: #e67e22; font-weight: bold;"
                )
              } else {
                span(
                  "Parenthetical citation",
                  style = "color: #3498db; font-weight: bold;"
                )
              },
              if (has_reference) {
                span(
                  " • ",
                  icon("book", style = "color: #27ae60;"),
                  " Reference matched (hover to view)",
                  style = "color: #27ae60; font-weight: bold;"
                )
              } else {
                span(
                  " • ",
                  icon("times-circle", style = "color: #e74c3c;"),
                  " No reference matched",
                  style = "color: #999; font-style: italic;"
                )
              }
            )
          )
        })

        tagList(
          tags$script(HTML(
            "
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
      "
          )),

          tags$style(HTML(
            "
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
      "
          )),

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
        nodes = data.frame(
          id = 1,
          label = "No citations found",
          size = 20,
          color = "#CCCCCC"
        ),
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
              stats$section_distribution$section,
              ": ",
              stats$section_distribution$n,
              collapse = "\n"
            )
          )
        }

        paste(
          "Nodes:",
          stats$n_nodes,
          "\n",
          "Edges:",
          stats$n_edges,
          "\n",
          "Avg. Distance:",
          stats$avg_distance,
          "chars\n",
          "Max Distance Filter:",
          stats$max_distance,
          "chars\n\n",
          "Network Density:",
          round(stats$n_edges / (stats$n_nodes * (stats$n_nodes - 1) / 2), 3),
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
  output$strongest_connections <- DT::renderDataTable(
    {
      if (
        !is.null(values$analysis_results) &&
          !is.null(values$analysis_results$network_data) &&
          nrow(values$analysis_results$network_data) > 0
      ) {
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
        data.frame(
          citation1 = character(0),
          citation2 = character(0),
          distance = character(0)
        )
      }
    },
    options = list(
      pageLength = 8,
      dom = 't',
      ordering = FALSE,
      searching = FALSE
    )
  )

  # ===========================================
  # DOWNLOAD HANDLERS
  # ===========================================

  output$download_network <- downloadHandler(
    filename = function() {
      paste0("citation_network_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (
        !is.null(values$analysis_results) &&
          !is.null(values$analysis_results$network_data)
      ) {
        write.csv(values$analysis_results$network_data, file, row.names = FALSE)
      } else {
        write.csv(
          data.frame(message = "No network data available"),
          file,
          row.names = FALSE
        )
      }
    }
  )

  output$download_contexts <- downloadHandler(
    filename = function() {
      paste0("citation_contexts_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (
        !is.null(values$analysis_results) &&
          !is.null(values$analysis_results$citation_contexts)
      ) {
        write.csv(
          values$analysis_results$citation_contexts,
          file,
          row.names = FALSE
        )
      } else {
        write.csv(
          data.frame(message = "No citation contexts available"),
          file,
          row.names = FALSE
        )
      }
    }
  )

  # ===========================================
  # TAB 4: WORD TRENDS ANALYSIS
  # ===========================================

  # Populate word choices when analysis is complete
  observe({
    if (
      !is.null(values$analysis_results) &&
        !is.null(values$analysis_results$word_frequencies) &&
        nrow(values$analysis_results$word_frequencies) > 0
    ) {
      # Get top 30 most frequent words
      top_words <- values$analysis_results$word_frequencies %>%
        slice_head(n = 30) %>%
        pull(word)

      # Get top bigrams if available
      top_bigrams <- character(0)
      if (
        !is.null(values$analysis_results$ngrams) &&
          "2gram" %in% names(values$analysis_results$ngrams)
      ) {
        top_bigrams <- values$analysis_results$ngrams$`2gram` %>%
          slice_head(n = 15) %>%
          pull(ngram)
      }

      # Get top trigrams if available
      top_trigrams <- character(0)
      if (
        !is.null(values$analysis_results$ngrams) &&
          "3gram" %in% names(values$analysis_results$ngrams)
      ) {
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

      updateSelectizeInput(
        session,
        "trend_words",
        choices = choices_list,
        selected = NULL,
        server = TRUE
      )
    }
  })

  # Check if sections are available
  output$sections_available <- reactive({
    if (!is.null(values$pdf_sections) && length(values$pdf_sections) > 0) {
      section_names <- setdiff(
        names(values$pdf_sections),
        c("Full_text", "References")
      )
      return(length(section_names) > 0)
    }
    return(FALSE)
  })
  outputOptions(output, "sections_available", suspendWhenHidden = FALSE)

  # Check if trends are available
  output$trends_available <- reactive({
    return(
      !is.null(values$word_trends_data) && nrow(values$word_trends_data) > 0
    )
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

    tryCatch(
      {
        # Determine use_sections parameter
        use_sections_param <- switch(
          input$segmentation_type,
          "auto" = "auto",
          "sections" = TRUE,
          "segments" = FALSE
        )

        # Get number of segments
        n_segs <- if (!is.null(input$n_segments)) input$n_segments else 10

        # Determine which text to use
        # If user explicitly chooses segments, use Full_text only
        # Otherwise, use the sections structure
        text_input <- if (input$segmentation_type == "segments") {
          values$pdf_text # Just the full text as string
        } else {
          values$pdf_sections # List with sections
        }

        # Calculate word distribution
        values$word_trends_data <- calculate_word_distribution(
          text = text_input,
          selected_words = input$trend_words,
          use_sections = use_sections_param,
          n_segments = n_segs,
          remove_stopwords = FALSE # Don't remove stopwords for selected words
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
      },
      error = function(e) {
        showNotification(
          paste("Error calculating word distribution:", e$message),
          type = "error",
          duration = 5
        )
      }
    )
  })

  # Render word trends plot
  output$word_trends_plot <- renderPlotly({
    req(values$word_trends_data)

    tryCatch(
      {
        plot_word_distribution(
          word_distribution_data = values$word_trends_data,
          plot_type = input$trend_plot_type,
          smooth = TRUE,
          show_points = input$trend_show_points,
          colors = NULL # Use automatic colors
        )
      },
      error = function(e) {
        showNotification(
          paste("Error creating plot:", e$message),
          type = "error",
          duration = 5
        )
        return(NULL)
      }
    )
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
          select(
            word,
            segment_id,
            segment_name,
            count,
            relative_frequency,
            percentage,
            total_words
          ) %>%
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

  # ===========================================
  # TAB 5: REFERENCES
  # ===========================================

  # Check if references are available
  output$references_available <- reactive({
    if (
      !is.null(values$analysis_results) &&
        !is.null(values$analysis_results$parsed_references)
    ) {
      return(nrow(values$analysis_results$parsed_references) > 0)
    }
    return(FALSE)
  })
  outputOptions(output, "references_available", suspendWhenHidden = FALSE)

  # # Total references count
  output$total_refs <- renderText({
    if (
      !is.null(values$analysis_results) &&
        !is.null(values$analysis_results$parsed_references)
    ) {
      format(nrow(values$analysis_results$parsed_references), big.mark = ",")
    } else {
      "0"
    }
  })

  # # PDF references count
  output$pdf_refs <- renderText({
    if (
      !is.null(values$analysis_results) &&
        !is.null(values$analysis_results$parsed_references)
    ) {
      refs <- values$analysis_results$parsed_references
      pdf_refs <- sum(tolower(refs$ref_source) == "pdf", na.rm = TRUE)
      format(pdf_refs, big.mark = ",")
    } else {
      "0"
    }
  })

  # # Crossref references count
  output$crossref_refs <- renderText({
    if (
      !is.null(values$analysis_results) &&
        !is.null(values$analysis_results$parsed_references)
    ) {
      refs <- values$analysis_results$parsed_references
      crossref_refs <- sum(tolower(refs$ref_source) == "crossref", na.rm = TRUE)
      format(crossref_refs, big.mark = ",")
    } else {
      "0"
    }
  })

  # Openalex references count
  output$openalex_refs <- renderText({
    req(values$references_oa)

    if (is.null(values$references_oa) || nrow(values$references_oa) == 0) {
      return("0")
    }

    nrow(values$references_oa)
  })

  # References HTML display
  output$references_html <- renderUI({
    req(values$analysis_results)
    req(values$analysis_results$parsed_references)

    refs <- values$analysis_results$parsed_references

    # Apply search filter
    if (!is.null(input$reference_search) && nzchar(input$reference_search)) {
      search_term <- tolower(input$reference_search)

      refs <- refs %>%
        filter(
          if_else(
            !is.na(ref_full_text),
            str_detect(tolower(ref_full_text), fixed(search_term)),
            FALSE
          ) |
            if_else(
              !is.na(ref_authors),
              str_detect(tolower(ref_authors), fixed(search_term)),
              FALSE
            ) |
            if_else(
              !is.na(ref_year),
              str_detect(tolower(ref_year), fixed(search_term)),
              FALSE
            ) |
            if_else(
              !is.na(doi),
              str_detect(tolower(doi), fixed(search_term)),
              FALSE
            )
        )
    }

    if (nrow(refs) == 0) {
      return(
        div(
          style = "text-align: center; padding: 40px; color: #999;",
          icon("search", style = "font-size: 24px; margin-bottom: 10px;"),
          h4("No references match your search"),
          p("Try different search terms.")
        )
      )
    }

    # Create HTML for each reference
    reference_items <- lapply(1:nrow(refs), function(i) {
      ref <- refs[i, ]
      # Get reference text
      ref_text <- if (!is.na(ref$ref_full_text) && nzchar(ref$ref_full_text)) {
        ref$ref_full_text
      } else {
        "[Reference text not available]"
      }

      # Check if OpenAlex data is available for this reference
      has_oa_data <- FALSE
      oa_button <- NULL

      if (
        !is.null(values$references_oa) && !is.na(ref$doi) && nzchar(ref$doi)
      ) {
        # Cerca corrispondenza nel dataset OpenAlex
        ref_doi_clean <- tolower(trimws(ref$doi))

        # Prova diverse varianti del DOI
        oa_match_idx <- which(
          tolower(gsub("https://doi.org/", "", values$references_oa$doi)) ==
            ref_doi_clean |
            tolower(values$references_oa$doi) ==
              paste0("https://doi.org/", ref_doi_clean)
        )

        if (length(oa_match_idx) > 0) {
          has_oa_data <- TRUE

          oa_button <- tags$button(
            class = "btn btn-info btn-xs",
            style = "float: right; margin-left: 10px;",
            onclick = sprintf("showOADetails(%d)", i),
            icon("info-circle"),
            " View Details"
          )
        }
      }

      # Determine source badge
      source_badge <- if (
        !is.na(ref$ref_source) && tolower(ref$ref_source) == "crossref"
      ) {
        tags$span(
          class = "label label-success",
          style = "font-size: 11px; margin-left: 10px;",
          icon("cloud-download-alt"),
          " Crossref"
        )
      } else {
        tags$span(
          class = "label label-info",
          style = "font-size: 11px; margin-left: 10px;",
          icon("file-pdf"),
          " PDF"
        )
      }

      # OpenAlex badge if available
      oa_badge <- if (has_oa_data) {
        tags$span(
          class = "label label-primary",
          style = "font-size: 11px; margin-left: 5px;",
          icon("database"),
          " OpenAlex"
        )
      }

      # DOI link if available
      doi_link <- NULL
      if ("doi" %in% names(ref) && !is.na(ref$doi) && nzchar(ref$doi)) {
        doi_link <- tags$div(
          style = "margin-top: 8px; font-size: 12px;",
          icon("link", style = "color: #3498db;"),
          tags$a(
            href = paste0("https://doi.org/", ref$doi),
            target = "_blank",
            ref$doi,
            style = "color: #3498db; margin-left: 5px;"
          )
        )
      }

      # Author and year display
      author_year_info <- NULL
      if (!is.na(ref$ref_first_author) && !is.na(ref$ref_year)) {
        n_auth <- if (!is.na(ref$n_authors) && ref$n_authors > 1) {
          paste0(" et al. (", ref$n_authors, " authors)")
        } else {
          ""
        }

        author_year_info <- tags$div(
          style = "margin-top: 8px; font-size: 12px; color: #666;",
          icon("user", style = "color: #3498db;"),
          tags$span(
            style = "margin-left: 5px;",
            paste0(ref$ref_first_author, n_auth, " • ", ref$ref_year)
          )
        )
      }

      # Citation count if available
      citation_info <- NULL
      if (
        "citation_count" %in%
          names(ref) &&
          !is.na(ref$citation_count) &&
          ref$citation_count > 0
      ) {
        citation_info <- tags$span(
          style = "margin-left: 15px; color: #e67e22; font-size: 12px;",
          icon("quote-right"),
          sprintf(
            " Cited %d time%s in text",
            ref$citation_count,
            ifelse(ref$citation_count > 1, "s", "")
          )
        )
      }

      div(
        class = "reference-item",
        `data-ref-index` = i,
        style = "padding: 15px; margin-bottom: 15px; border-left: 4px solid #2E86AB; background-color: #fafafa; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); position: relative;",

        # Reference number, badges and OA button
        div(
          style = "margin-bottom: 10px;",
          tags$span(
            style = "font-weight: bold; color: #2E86AB; font-size: 16px;",
            paste0("[", i, "]")
          ),
          source_badge,
          oa_badge,
          citation_info,
          oa_button
        ),

        # Full reference text
        div(
          style = "font-family: 'Georgia', serif; font-size: 14px; line-height: 1.6; color: #333; clear: both;",
          ref_text
        ),

        # Author and year info
        author_year_info,

        # DOI link
        doi_link
      )
    })

    tagList(
      tags$style(HTML(
        "
      .reference-item:hover {
        background-color: #f0f8ff !important;
        transition: background-color 0.2s ease;
      }
      .oa-document-summary .info-box {
        display: flex;
        align-items: center;
        border-radius: 4px;
        padding: 10px;
        color: white;
      }
      .oa-document-summary .info-box-icon {
        font-size: 32px;
        flex: 0 0 70px;
        text-align: center;
      }
      .oa-document-summary .info-box-content {
        flex: 1;
      }
      .oa-document-summary .info-box-text {
        display: block;
        font-size: 12px;
        text-transform: uppercase;
        opacity: 0.9;
      }
      .oa-document-summary .info-box-number {
        display: block;
        font-size: 20px;
        font-weight: bold;
        margin-top: 5px;
      }
    "
      )),
      reference_items
    )
  })

  # Download handler for references
  output$download_references <- downloadHandler(
    filename = function() {
      paste0("references_", Sys.Date(), ".txt")
    },
    content = function(file) {
      if (
        !is.null(values$analysis_results) &&
          !is.null(values$analysis_results$parsed_references)
      ) {
        refs <- values$analysis_results$parsed_references

        # Create formatted text
        ref_text <- paste0(
          "BIBLIOGRAPHY\n",
          "============\n",
          "Generated: ",
          Sys.time(),
          "\n",
          "Total references: ",
          nrow(refs),
          "\n",
          "From PDF: ",
          sum(tolower(refs$ref_source) == "pdf", na.rm = TRUE),
          "\n",
          "From Crossref: ",
          sum(tolower(refs$ref_source) == "crossref", na.rm = TRUE),
          "\n\n"
        )

        for (i in 1:nrow(refs)) {
          ref <- refs[i, ]

          source_label <- if (
            !is.na(ref$ref_source) && tolower(ref$ref_source) == "crossref"
          ) {
            "[Crossref]"
          } else {
            "[PDF]"
          }

          ref_text <- paste0(
            ref_text,
            "[",
            i,
            "] ",
            source_label,
            "\n",
            ref$ref_full_text,
            "\n"
          )

          if (!is.na(ref$ref_first_author) && !is.na(ref$ref_year)) {
            n_auth <- if (!is.na(ref$n_authors) && ref$n_authors > 1) {
              paste0(" (", ref$n_authors, " authors)")
            } else {
              ""
            }
            ref_text <- paste0(
              ref_text,
              "Author: ",
              ref$ref_first_author,
              n_auth,
              " • Year: ",
              ref$ref_year,
              "\n"
            )
          }

          if ("doi" %in% names(ref) && !is.na(ref$doi) && nzchar(ref$doi)) {
            ref_text <- paste0(ref_text, "DOI: ", ref$doi, "\n")
          }

          if ("citation_count" %in% names(ref) && !is.na(ref$citation_count)) {
            ref_text <- paste0(
              ref_text,
              "Cited ",
              ref$citation_count,
              " times in text\n"
            )
          }

          ref_text <- paste0(ref_text, "\n")
        }

        writeLines(ref_text, file)
      } else {
        writeLines("No references available", file)
      }
    }
  )

  # ===========================================
  # TAB: BIBLIOAI SUMMARY - SERVER LOGIC
  # ===========================================

  # ===========================================
  # BIBLIOAI SUMMARY INITIALIZATION
  # ===========================================

  # Initialize API key from environment on startup
  observe(
    {
      api_key_env <- Sys.getenv("GEMINI_API_KEY")
      if (nzchar(api_key_env) && is.null(values$gemini_api_key)) {
        values$gemini_api_key <- api_key_env
      }
    },
    priority = 100
  )

  # ===========================================
  # API STATUS DISPLAY
  # ===========================================

  output$gemini_api_status <- renderUI({
    api_key_env <- Sys.getenv("GEMINI_API_KEY")

    if (!nzchar(api_key_env)) {
      tags$span(
        class = "label label-danger",
        style = "font-size: 12px; padding: 6px 12px;",
        icon("exclamation-circle"),
        " API Key Not Configured"
      )
    } else {
      tags$span(
        class = "label label-success",
        style = "font-size: 12px; padding: 6px 12px;",
        icon("check-circle"),
        " API Key Configured"
      )
    }
  })

  # ===========================================
  # SUMMARY TYPE DESCRIPTION
  # ===========================================

  output$summary_type_description <- renderUI({
    req(input$summary_type)

    descriptions <- list(
      short_abstract = "A concise, one-paragraph abstract (around 250 words) covering objectives, methods, findings, and conclusions.",
      narrative_abstract = "An engaging narrative (500-600 words) that tells the story of the research from problem to implications.",
      imrad_summary = "A structured summary following the IMRaD format: Introduction, Methods, Results, and Discussion.",
      thematic_bibliography = "Groups the references by research themes and provides descriptions of each thematic area.",
      research_questions = "Extracts research questions, context, and motivation behind the study.",
      background_literature = "Summarizes the theoretical framework and previous research that grounds this study.",
      methods_summary = "Detailed breakdown of research design, data collection, analysis techniques, and tools used.",
      implications = "Focuses on theoretical contributions and practical applications derived from the findings.",
      list_tables_figures = "Complete list of all tables and figures with their captions as they appear in the document."
    )

    description <- descriptions[[input$summary_type]]

    div(
      style = "background-color: #f0f8ff; padding: 12px; border-radius: 4px; border-left: 3px solid #3498db;",
      tags$small(
        icon("info-circle", style = "color: #3498db; margin-right: 5px;"),
        description,
        style = "color: #555; line-height: 1.6;"
      )
    )
  })

  # ===========================================
  # GENERATE SUMMARY
  # ===========================================

  observeEvent(input$generate_summary, {
    # Validate PDF file
    if (is.null(input$pdf_file)) {
      showNotification(
        "Please upload a PDF file first.",
        type = "warning",
        duration = 4
      )
      return()
    }

    # Validate API key
    api_key <- Sys.getenv("GEMINI_API_KEY")
    if (!nzchar(api_key)) {
      showNotification(
        HTML(
          "<strong>API Key Required</strong><br/>
           Please configure your Gemini API key in Biblioshiny settings.<br/>
           You can get a free API key at: <a href='https://aistudio.google.com/apikey' target='_blank'>Google AI Studio</a>"
        ),
        type = "error",
        duration = 8
      )
      return()
    }

    # Get model from Biblioshiny settings or use default
    model <- values$gemini_api_model
    if (is.null(model) || !nzchar(model)) {
      model <- "2.0-flash-lite"
    }

    # Use "huge" as default output size
    output_size <- "huge"

    tryCatch(
      {
        # Generate prompt
        prompt <- get_article_summary_prompt(input$summary_type)

        # Show progress notification
        progress_id <- showNotification(
          "Sending request to Gemini AI...",
          type = "message",
          duration = NULL,
          closeButton = FALSE
        )

        # Call Gemini AI
        result <- gemini_ai(
          docs = input$pdf_file$datapath,
          prompt = prompt,
          model = model,
          api_key = api_key,
          outputSize = output_size,
          retry_503 = 5
        )

        # Remove progress notification
        removeNotification(progress_id)

        # Check if result is an error message
        if (is.character(result) && length(result) == 1) {
          if (
            grepl("^❌", result) || grepl("Error", result, ignore.case = TRUE)
          ) {
            showNotification(
              HTML(paste0(
                "<strong>AI Request Failed</strong><br/>",
                gsub("❌ ", "", result)
              )),
              type = "error",
              duration = 10
            )
            return()
          }
        }

        # Store results
        values$biblioai_summary <- result
        values$biblioai_summary_type <- input$summary_type
        values$biblioai_summary_timestamp <- Sys.time()

        # Get summary type label
        summary_labels <- list(
          short_abstract = "Short Abstract",
          narrative_abstract = "Narrative Abstract",
          imrad_summary = "IMRaD Summary",
          thematic_bibliography = "Thematic Bibliography",
          research_questions = "Research Questions & Context",
          background_literature = "Background & Literature",
          methods_summary = "Methods Summary",
          implications = "Implications & Conclusions",
          list_tables_figures = "Tables & Figures List"
        )

        summary_label <- summary_labels[[input$summary_type]]

        showNotification(
          HTML(paste0(
            "<strong>Summary Generated!</strong><br/>",
            "Type: ",
            summary_label
          )),
          type = "message",
          duration = 5
        )
      },
      error = function(e) {
        removeNotification(progress_id)

        showNotification(
          HTML(paste0(
            "<strong>Error Generating Summary</strong><br/>",
            e$message
          )),
          type = "error",
          duration = 8
        )

        cat("BiblioAI Summary Error:\n")
        print(e)
      }
    )
  })

  # ===========================================
  # SUMMARY AVAILABILITY
  # ===========================================

  output$summary_available <- reactive({
    return(!is.null(values$biblioai_summary))
  })
  outputOptions(output, "summary_available", suspendWhenHidden = FALSE)

  # ===========================================
  # SUMMARY DISPLAY
  # ===========================================

  output$summary_type_display <- renderText({
    req(values$biblioai_summary_type)

    summary_labels <- list(
      short_abstract = "Short Abstract (250 words)",
      narrative_abstract = "Narrative Abstract (500-600 words)",
      imrad_summary = "IMRaD Structure Summary",
      thematic_bibliography = "Thematic Bibliography",
      research_questions = "Research Questions & Context",
      background_literature = "Background & Literature Review",
      methods_summary = "Methodology Summary",
      implications = "Implications & Conclusions",
      list_tables_figures = "Tables & Figures List"
    )

    summary_labels[[values$biblioai_summary_type]]
  })

  output$summary_timestamp <- renderText({
    req(values$biblioai_summary_timestamp)
    format(values$biblioai_summary_timestamp, "%Y-%m-%d %H:%M:%S")
  })

  output$summary_content_display <- renderUI({
    req(values$biblioai_summary)

    # Combine multiple text elements if result is a vector
    summary_text <- if (
      is.character(values$biblioai_summary) &&
        length(values$biblioai_summary) > 1
    ) {
      paste(values$biblioai_summary, collapse = "\n\n")
    } else {
      values$biblioai_summary
    }

    # Convert markdown-style headers to HTML for better display
    summary_html <- summary_text

    # Convert **text** to bold
    summary_html <- gsub(
      "\\*\\*([^*]+)\\*\\*",
      "<strong>\\1</strong>",
      summary_html
    )

    # Convert *text* to italic
    summary_html <- gsub("\\*([^*]+)\\*", "<em>\\1</em>", summary_html)

    # Convert ### headers to h4
    summary_html <- gsub(
      "^### (.+)$",
      "<h4 style='color: #2E86AB; margin-top: 20px; margin-bottom: 10px;'>\\1</h4>",
      summary_html,
      perl = TRUE
    )

    # Convert ## headers to h3
    summary_html <- gsub(
      "^## (.+)$",
      "<h3 style='color: #2E86AB; margin-top: 25px; margin-bottom: 10px;'>\\1</h3>",
      summary_html,
      perl = TRUE
    )

    # Convert # headers to h2
    summary_html <- gsub(
      "^# (.+)$",
      "<h2 style='color: #2E86AB; margin-top: 30px; margin-bottom: 15px;'>\\1</h2>",
      summary_html,
      perl = TRUE
    )

    # Convert bullet points
    summary_html <- gsub("^- (.+)$", "<li>\\1</li>", summary_html, perl = TRUE)
    summary_html <- gsub(
      "^\\* (.+)$",
      "<li>\\1</li>",
      summary_html,
      perl = TRUE
    )

    # Wrap consecutive <li> in <ul>
    summary_html <- gsub(
      "(<li>.*?</li>)\n(<li>)",
      "\\1\\2",
      summary_html,
      perl = TRUE
    )
    summary_html <- gsub(
      "(<li>.*?</li>)",
      "<ul style='margin-left: 20px;'>\\1</ul>",
      summary_html,
      perl = TRUE
    )

    # Convert line breaks to <br> but not inside lists
    summary_html <- gsub("\n(?!<)", "<br/>", summary_html, perl = TRUE)

    HTML(summary_html)
  })

  # ===========================================
  # DOWNLOAD SUMMARY
  # ===========================================

  output$download_summary <- downloadHandler(
    filename = function() {
      summary_type <- values$biblioai_summary_type
      if (is.null(summary_type)) {
        summary_type <- "summary"
      }
      paste0("biblioai_", summary_type, "_", Sys.Date(), ".txt")
    },
    content = function(file) {
      if (!is.null(values$biblioai_summary)) {
        # Create header
        header <- paste0(
          "====================================\n",
          "BiblioAI Summary\n",
          "====================================\n\n",
          "Summary Type: ",
          values$biblioai_summary_type,
          "\n",
          "Generated: ",
          format(values$biblioai_summary_timestamp, "%Y-%m-%d %H:%M:%S"),
          "\n",
          "Document: ",
          input$pdf_file$name,
          "\n\n",
          "====================================\n\n"
        )

        # Combine multiple text elements if result is a vector
        summary_text <- if (
          is.character(values$biblioai_summary) &&
            length(values$biblioai_summary) > 1
        ) {
          paste(values$biblioai_summary, collapse = "\n\n")
        } else {
          values$biblioai_summary
        }

        # Write to file
        writeLines(paste0(header, summary_text), file, useBytes = TRUE)
      } else {
        writeLines("No summary available", file)
      }
    }
  )

  # ===========================================
  # RESET SUMMARY ON NEW PDF OR RESET
  # ===========================================

  # Clear summary when new PDF is uploaded
  observeEvent(input$pdf_file, {
    values$biblioai_summary <- NULL
    values$biblioai_summary_type <- NULL
    values$biblioai_summary_timestamp <- NULL
  })

  # Clear summary on reset
  observeEvent(input$reset_analysis, {
    values$pdf_metadata <- NULL
    values$biblioai_summary <- NULL
    values$biblioai_summary_type <- NULL
    values$biblioai_summary_timestamp <- NULL

    # Reset UI inputs
    updateSelectInput(session, "summary_type", selected = "short_abstract")
  })

  # Clear metadata when new PDF is uploaded
  observeEvent(input$pdf_file, {
    values$pdf_metadata <- NULL
  })

  # ===========================================
  # OPENALEX MODAL HANDLER
  # ===========================================

  observeEvent(input$selected_oa_ref_index, {
    req(values$analysis_results$parsed_references)
    req(values$references_oa)
    req(input$selected_oa_ref_index)

    tryCatch(
      {
        ref_index <- input$selected_oa_ref_index
        ref <- values$analysis_results$parsed_references[ref_index, ]

        if (is.na(ref$doi) || !nzchar(ref$doi)) {
          session$sendCustomMessage(
            "updateOAModal",
            list(
              title = as.character(span(
                "Reference Details",
                style = "font-weight: 600; font-size: 20px;"
              )),
              content = as.character(div(
                style = "text-align: center; padding: 40px; color: #999;",
                icon(
                  "exclamation-triangle",
                  style = "font-size: 32px; margin-bottom: 15px; color: #f39c12;"
                ),
                h4("No DOI available"),
                p("Cannot fetch OpenAlex data without a DOI.")
              ))
            )
          )
          return()
        }

        # Trova i dati OpenAlex corrispondenti
        ref_doi_clean <- tolower(trimws(ref$doi))

        oa_match_idx <- which(
          tolower(gsub("https://doi.org/", "", values$references_oa$doi)) ==
            ref_doi_clean |
            tolower(values$references_oa$doi) ==
              paste0("https://doi.org/", ref_doi_clean)
        )

        if (length(oa_match_idx) == 0) {
          session$sendCustomMessage(
            "updateOAModal",
            list(
              title = as.character(span(
                "Reference Details",
                style = "font-weight: 600; font-size: 20px;"
              )),
              content = as.character(div(
                style = "text-align: center; padding: 40px; color: #999;",
                icon(
                  "database",
                  style = "font-size: 32px; margin-bottom: 15px; color: #e74c3c;"
                ),
                h4("OpenAlex data not found"),
                p(paste("Could not find OpenAlex data for DOI:", ref$doi))
              ))
            )
          )
          return()
        }

        # Prendi il primo match
        oa_data <- values$references_oa[oa_match_idx[1], ]

        # Estrai il titolo per l'header
        doc_title <- if (!is.na(oa_data$title) && nzchar(oa_data$title)) {
          oa_data$title
        } else if (
          !is.na(oa_data$display_name) && nzchar(oa_data$display_name)
        ) {
          oa_data$display_name
        } else {
          "Reference Details"
        }

        # Tronca il titolo se troppo lungo
        if (nchar(doc_title) > 120) {
          doc_title <- paste0(substr(doc_title, 1, 117), "...")
        }

        # Crea l'HTML per il modal
        modal_content <- create_oa_details_html(oa_data)

        # Aggiorna il modal con il titolo del documento
        session$sendCustomMessage(
          "updateOAModal",
          list(
            title = as.character(span(
              doc_title,
              style = "font-weight: 600; font-size: 20px; line-height: 1.4;"
            )),
            content = as.character(modal_content)
          )
        )
      },
      error = function(e) {
        session$sendCustomMessage(
          "updateOAModal",
          list(
            title = as.character(span(
              "Error",
              style = "font-weight: 600; font-size: 20px;"
            )),
            content = as.character(div(
              style = "text-align: center; padding: 40px; color: #e74c3c;",
              icon(
                "exclamation-circle",
                style = "font-size: 32px; margin-bottom: 15px;"
              ),
              h4("Error loading details"),
              p(paste("Error:", e$message))
            ))
          )
        )

        cat("Error in OA modal handler:", e$message, "\n")
        print(e)
      }
    )
  })

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
    values$citation_type_used <- NULL

    updateSelectizeInput(
      session,
      "trend_words",
      choices = NULL,
      selected = NULL
    )

    preview_visible(TRUE)

    tryCatch(
      {
        shinyjs::reset("pdf_file")
        updateTextInput(session, "pdf_doi_input", value = "")
        # MODIFIED: Reset to no selection
        updateRadioButtons(
          session,
          "citation_type_import",
          selected = character(0)
        )
        updateActionButton(
          session,
          "run_analysis",
          label = "Start",
          icon = icon("play")
        )
        updateActionButton(session, "toggle_preview", "Hide Preview")
        updateTextInput(session, "context_search", value = "")
        updateSelectInput(session, "context_type_filter", selected = "")
      },
      error = function(e) {
        cat("Reset UI elements failed:", e$message, "\n")
      }
    )

    showNotification(
      "Analysis reset successfully!",
      type = "message",
      duration = 3
    )

    # Re-open the PDF import box
    session$sendCustomMessage('togglePdfImport', list(action = 'open'))
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

      tryCatch(
        {
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
              values$analysis_results$summary$citations_extracted -
                values$analysis_results$summary$narrative_citations,
              round(
                values$analysis_results$summary$citation_density_per_1000_words,
                2
              ),
              round(values$analysis_results$summary$lexical_diversity, 4),
              values$analysis_results$text_analytics$basic_stats$total_characters,
              values$analysis_results$text_analytics$basic_stats$total_sentences,
              round(
                values$analysis_results$text_analytics$basic_stats$avg_words_per_sentence,
                2
              )
            ),
            stringsAsFactors = FALSE
          )

          openxlsx::writeData(
            wb_content,
            "Summary_Statistics",
            summary_data,
            startRow = 1
          )
          openxlsx::addStyle(
            wb_content,
            "Summary_Statistics",
            style = openxlsx::createStyle(
              textDecoration = "bold",
              fgFill = "#2E86AB",
              fontColour = "#FFFFFF"
            ),
            rows = 1,
            cols = 1:2,
            gridExpand = TRUE
          )
          openxlsx::setColWidths(
            wb_content,
            "Summary_Statistics",
            cols = 1:2,
            widths = c(35, 20)
          )

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
                round(
                  values$readability_indices$automated_readability_index,
                  2
                ),
                round(values$readability_indices$gunning_fog_index, 2),
                round(values$readability_indices$avg_sentence_length, 2),
                values$readability_indices$n_sentences,
                values$readability_indices$n_words,
                values$readability_indices$n_syllables,
                values$readability_indices$n_complex_words,
                paste0(
                  round(values$readability_indices$pct_complex_words, 2),
                  "%"
                )
              ),
              stringsAsFactors = FALSE
            )

            openxlsx::writeData(
              wb_content,
              "Readability_Indices",
              readability_data,
              startRow = 1
            )
            openxlsx::addStyle(
              wb_content,
              "Readability_Indices",
              style = openxlsx::createStyle(
                textDecoration = "bold",
                fgFill = "#27ae60",
                fontColour = "#FFFFFF"
              ),
              rows = 1,
              cols = 1:2,
              gridExpand = TRUE
            )
            openxlsx::setColWidths(
              wb_content,
              "Readability_Indices",
              cols = 1:2,
              widths = c(40, 20)
            )
          }

          # === SHEET 3: WORD FREQUENCIES ===
          if (
            !is.null(values$analysis_results$word_frequencies) &&
              nrow(values$analysis_results$word_frequencies) > 0
          ) {
            openxlsx::addWorksheet(wb_content, "Word_Frequencies")

            top_words <- values$analysis_results$word_frequencies %>%
              slice_head(n = 50) %>%
              as.data.frame(stringsAsFactors = FALSE)

            openxlsx::writeData(
              wb_content,
              "Word_Frequencies",
              top_words,
              startRow = 1
            )
            openxlsx::addStyle(
              wb_content,
              "Word_Frequencies",
              style = openxlsx::createStyle(
                textDecoration = "bold",
                fgFill = "#3498db",
                fontColour = "#FFFFFF"
              ),
              rows = 1,
              cols = 1:ncol(top_words),
              gridExpand = TRUE
            )
            openxlsx::setColWidths(
              wb_content,
              "Word_Frequencies",
              cols = 1:ncol(top_words),
              widths = "auto"
            )
          }

          # === SHEET 4: BIGRAMS ===
          if (
            !is.null(values$analysis_results$ngrams) &&
              "2gram" %in% names(values$analysis_results$ngrams)
          ) {
            openxlsx::addWorksheet(wb_content, "Bigrams")

            bigrams_data <- values$analysis_results$ngrams$`2gram` %>%
              slice_head(n = 50) %>%
              as.data.frame(stringsAsFactors = FALSE)

            openxlsx::writeData(
              wb_content,
              "Bigrams",
              bigrams_data,
              startRow = 1
            )
            openxlsx::addStyle(
              wb_content,
              "Bigrams",
              style = openxlsx::createStyle(
                textDecoration = "bold",
                fgFill = "#9b59b6",
                fontColour = "#FFFFFF"
              ),
              rows = 1,
              cols = 1:ncol(bigrams_data),
              gridExpand = TRUE
            )
            openxlsx::setColWidths(
              wb_content,
              "Bigrams",
              cols = 1:ncol(bigrams_data),
              widths = "auto"
            )
          }

          # === SHEET 5: TRIGRAMS ===
          if (
            !is.null(values$analysis_results$ngrams) &&
              "3gram" %in% names(values$analysis_results$ngrams)
          ) {
            openxlsx::addWorksheet(wb_content, "Trigrams")

            trigrams_data <- values$analysis_results$ngrams$`3gram` %>%
              slice_head(n = 50) %>%
              as.data.frame(stringsAsFactors = FALSE)

            openxlsx::writeData(
              wb_content,
              "Trigrams",
              trigrams_data,
              startRow = 1
            )
            openxlsx::addStyle(
              wb_content,
              "Trigrams",
              style = openxlsx::createStyle(
                textDecoration = "bold",
                fgFill = "#e67e22",
                fontColour = "#FFFFFF"
              ),
              rows = 1,
              cols = 1:ncol(trigrams_data),
              gridExpand = TRUE
            )
            openxlsx::setColWidths(
              wb_content,
              "Trigrams",
              cols = 1:ncol(trigrams_data),
              widths = "auto"
            )
          }

          # === SHEET 6: CITATION TYPES ===
          if (
            !is.null(values$analysis_results$citation_metrics$type_distribution)
          ) {
            openxlsx::addWorksheet(wb_content, "Citation_Types")

            types_data <- values$analysis_results$citation_metrics$type_distribution %>%
              as.data.frame(stringsAsFactors = FALSE)

            openxlsx::writeData(
              wb_content,
              "Citation_Types",
              types_data,
              startRow = 1
            )
            openxlsx::addStyle(
              wb_content,
              "Citation_Types",
              style = openxlsx::createStyle(
                textDecoration = "bold",
                fgFill = "#e74c3c",
                fontColour = "#FFFFFF"
              ),
              rows = 1,
              cols = 1:ncol(types_data),
              gridExpand = TRUE
            )
            openxlsx::setColWidths(
              wb_content,
              "Citation_Types",
              cols = 1:ncol(types_data),
              widths = "auto"
            )
          }

          # === SHEET 7: CITATIONS BY SECTION ===
          if (
            !is.null(
              values$analysis_results$citation_metrics$section_distribution
            )
          ) {
            openxlsx::addWorksheet(wb_content, "Citations_by_Section")

            sections_data <- values$analysis_results$citation_metrics$section_distribution %>%
              filter(n > 0) %>%
              as.data.frame(stringsAsFactors = FALSE)

            openxlsx::writeData(
              wb_content,
              "Citations_by_Section",
              sections_data,
              startRow = 1
            )
            openxlsx::addStyle(
              wb_content,
              "Citations_by_Section",
              style = openxlsx::createStyle(
                textDecoration = "bold",
                fgFill = "#16a085",
                fontColour = "#FFFFFF"
              ),
              rows = 1,
              cols = 1:ncol(sections_data),
              gridExpand = TRUE
            )
            openxlsx::setColWidths(
              wb_content,
              "Citations_by_Section",
              cols = 1:ncol(sections_data),
              widths = "auto"
            )
          }

          # === SHEET 8: CITATION CONTEXTS ===
          if (
            !is.null(values$analysis_results$citation_contexts) &&
              nrow(values$analysis_results$citation_contexts) > 0
          ) {
            openxlsx::addWorksheet(wb_content, "Citation_Contexts")

            # Seleziona le colonne principali se esistono, altrimenti esporta tutte
            contexts_cols <- c(
              "citation_id",
              "citation_text",
              "section",
              "citation_type",
              "is_narrative",
              "words_before",
              "words_after",
              "full_context",
              "context_word_count",
              "citation_position_in_text"
            )

            available_cols <- intersect(
              contexts_cols,
              names(values$analysis_results$citation_contexts)
            )

            if (length(available_cols) > 0) {
              contexts_export <- values$analysis_results$citation_contexts %>%
                select(all_of(available_cols)) %>%
                as.data.frame(stringsAsFactors = FALSE)
            } else {
              # Se nessuna colonna specificata esiste, esporta tutte
              contexts_export <- values$analysis_results$citation_contexts %>%
                as.data.frame(stringsAsFactors = FALSE)
            }

            openxlsx::writeData(
              wb_content,
              "Citation_Contexts",
              contexts_export,
              startRow = 1
            )
            openxlsx::addStyle(
              wb_content,
              "Citation_Contexts",
              style = openxlsx::createStyle(
                textDecoration = "bold",
                fgFill = "#2E86AB",
                fontColour = "#FFFFFF"
              ),
              rows = 1,
              cols = 1:ncol(contexts_export),
              gridExpand = TRUE
            )
            openxlsx::setColWidths(
              wb_content,
              "Citation_Contexts",
              cols = 1:ncol(contexts_export),
              widths = "auto"
            )
          }

          # === SHEET 9: CITATION NETWORK ===
          if (
            !is.null(values$analysis_results$network_data) &&
              nrow(values$analysis_results$network_data) > 0
          ) {
            openxlsx::addWorksheet(wb_content, "Citation_Network")

            # Esporta tutte le colonne disponibili nel network
            network_export <- values$analysis_results$network_data %>%
              as.data.frame(stringsAsFactors = FALSE)

            openxlsx::writeData(
              wb_content,
              "Citation_Network",
              network_export,
              startRow = 1
            )
            openxlsx::addStyle(
              wb_content,
              "Citation_Network",
              style = openxlsx::createStyle(
                textDecoration = "bold",
                fgFill = "#8e44ad",
                fontColour = "#FFFFFF"
              ),
              rows = 1,
              cols = 1:ncol(network_export),
              gridExpand = TRUE
            )
            openxlsx::setColWidths(
              wb_content,
              "Citation_Network",
              cols = 1:ncol(network_export),
              widths = "auto"
            )
          }

          # === SHEET 10: WORD TRENDS (if available) ===
          if (
            !is.null(values$word_trends_data) &&
              nrow(values$word_trends_data) > 0
          ) {
            openxlsx::addWorksheet(wb_content, "Word_Trends")

            trends_export <- values$word_trends_data %>%
              arrange(word, segment_id) %>%
              as.data.frame(stringsAsFactors = FALSE)

            openxlsx::writeData(
              wb_content,
              "Word_Trends",
              trends_export,
              startRow = 1
            )
            openxlsx::addStyle(
              wb_content,
              "Word_Trends",
              style = openxlsx::createStyle(
                textDecoration = "bold",
                fgFill = "#27ae60",
                fontColour = "#FFFFFF"
              ),
              rows = 1,
              cols = 1:ncol(trends_export),
              gridExpand = TRUE
            )
            openxlsx::setColWidths(
              wb_content,
              "Word_Trends",
              cols = 1:ncol(trends_export),
              widths = "auto"
            )

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

            openxlsx::writeData(
              wb_content,
              "Word_Trends_Summary",
              trends_summary,
              startRow = 1
            )
            openxlsx::addStyle(
              wb_content,
              "Word_Trends_Summary",
              style = openxlsx::createStyle(
                textDecoration = "bold",
                fgFill = "#16a085",
                fontColour = "#FFFFFF"
              ),
              rows = 1,
              cols = 1:ncol(trends_summary),
              gridExpand = TRUE
            )
            openxlsx::setColWidths(
              wb_content,
              "Word_Trends_Summary",
              cols = 1:ncol(trends_summary),
              widths = "auto"
            )
          }

          # Save workbook
          openxlsx::saveWorkbook(wb_content, file, overwrite = TRUE)
        },
        error = function(e) {
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
        }
      )
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
        "PDF text length:",
        nchar(values$pdf_text %||% ""),
        "\n",
        "Citations found:",
        nrow(values$analysis_results$citations %||% data.frame()),
        "\n",
        "Network connections:",
        nrow(values$analysis_results$network_data %||% data.frame()),
        "\n",
        "Analysis running:",
        values$analysis_running %||% FALSE
      )
    } else {
      "No analysis data available"
    }
  })

  outputOptions(output, "debug_info", suspendWhenHidden = FALSE)
}

# Helper function for HTML card OpenAlex ----
create_oa_details_html <- function(oa_data) {
  # Helper per gestire valori NULL/NA
  safe_value <- function(x, default = "Not available") {
    if (
      is.null(x) ||
        length(x) == 0 ||
        all(is.na(x)) ||
        !nzchar(trimws(as.character(x)))
    ) {
      return(default)
    }
    return(x)
  }

  # === ESTRAI AUTORI E AFFILIAZIONI INLINE ===
  authors_html <- NULL
  if (!is.null(oa_data$authorships) && length(oa_data$authorships) > 0) {
    authorships_df <- oa_data$authorships[[1]]

    if (!is.null(authorships_df) && nrow(authorships_df) > 0) {
      authors_items <- lapply(1:min(nrow(authorships_df), 15), function(i) {
        auth <- authorships_df[i, ]

        author_name <- safe_value(auth$display_name, "Unknown Author")
        author_id <- safe_value(auth$id, "")

        # Estrai affiliazioni
        affiliation_text <- ""
        if (!is.null(auth$institutions) && length(auth$institutions) > 0) {
          insts_df <- auth$institutions[[1]]
          if (
            !is.null(insts_df) &&
              nrow(insts_df) > 0 &&
              "display_name" %in% names(insts_df)
          ) {
            affiliation_text <- paste(insts_df$display_name, collapse = ", ")
          }
        }

        # Se non ci sono istituzioni, prova con affiliation_raw
        if (
          !nzchar(affiliation_text) &&
            !is.na(auth$affiliation_raw) &&
            nzchar(auth$affiliation_raw)
        ) {
          affiliation_text <- auth$affiliation_raw
        }

        # Layout inline: nome e affiliazione sulla stessa riga
        div(
          style = "margin-bottom: 8px; padding: 6px 10px; background-color: #f8f9fa; border-radius: 4px; display: flex; align-items: center; flex-wrap: wrap;",
          tags$div(
            style = "font-weight: 500; font-size: 14px; margin-right: 12px;",
            # Link OpenAlex se disponibile
            if (nzchar(author_id) && author_id != "Not available") {
              tags$a(
                href = author_id,
                target = "_blank",
                style = "color: #2E86AB; text-decoration: none;",
                author_name,
                icon(
                  "external-link-alt",
                  style = "margin-left: 4px; font-size: 10px;"
                )
              )
            } else {
              tags$span(style = "color: #2E86AB;", author_name)
            }
          ),
          if (nzchar(affiliation_text)) {
            tags$span(
              style = "color: #666; font-size: 13px; font-style: italic;",
              icon("university", style = "margin-right: 5px; font-size: 11px;"),
              affiliation_text
            )
          }
        )
      })

      authors_html <- tagList(authors_items)

      if (nrow(authorships_df) > 15) {
        authors_html <- tagList(
          authors_html,
          tags$small(
            style = "color: #999; font-style: italic; display: block; margin-top: 8px; padding-left: 10px;",
            paste("... and", nrow(authorships_df) - 15, "more authors")
          )
        )
      }
    }
  }

  # === KEYWORDS - TUTTE ===
  keywords_html <- NULL
  if (!is.null(oa_data$keywords) && length(oa_data$keywords) > 0) {
    keywords_data <- oa_data$keywords[[1]]
    if (
      !is.null(keywords_data) &&
        is.data.frame(keywords_data) &&
        nrow(keywords_data) > 0
    ) {
      if ("display_name" %in% names(keywords_data)) {
        # NESSUN LIMITE - tutte le keywords
        keywords_html <- lapply(keywords_data$display_name, function(kw) {
          tags$span(
            class = "label label-primary",
            style = "margin-right: 5px; margin-bottom: 5px; font-size: 11px; display: inline-block; background-color: #3498db; color: white; padding: 4px 10px; border-radius: 3px;",
            kw
          )
        })
      }
    }
  }

  # === ABSTRACT ===
  abstract_text <- safe_value(oa_data$abstract, "Abstract not available")

  # === TOPICS - SOLO I PRIMI 5 ===
  topics_html <- NULL
  if (!is.null(oa_data$topics) && length(oa_data$topics) > 0) {
    topics_data <- oa_data$topics[[1]]
    if (
      !is.null(topics_data) &&
        is.data.frame(topics_data) &&
        nrow(topics_data) > 0
    ) {
      if ("display_name" %in% names(topics_data)) {
        main_topics <- topics_data
        if ("type" %in% names(topics_data)) {
          main_topics <- topics_data[topics_data$type == "topic", ]
        }

        if (nrow(main_topics) > 0) {
          # LIMITE A 5 TOPICS
          topics_html <- lapply(1:min(5, nrow(main_topics)), function(i) {
            tags$span(
              class = "label label-info",
              style = "margin-right: 5px; margin-bottom: 5px; font-size: 11px; display: inline-block; background-color: #5bc0de; color: white; padding: 4px 10px; border-radius: 3px;",
              main_topics$display_name[i]
            )
          })
        }
      }
    }
  }

  # === JOURNAL INFO ===
  journal_name <- safe_value(
    oa_data$source_display_name,
    "Journal not available"
  )
  issn <- safe_value(oa_data$issn_l, "")

  # === DOCUMENT TYPE ===
  doc_type <- safe_value(oa_data$type, "Unknown")

  # === PUBLICATION INFO ===
  pub_year <- safe_value(oa_data$publication_year, "N/A")
  citations <- safe_value(oa_data$cited_by_count, 0)
  fwci <- safe_value(oa_data$fwci, NA)

  # === OPEN ACCESS INFO ===
  is_oa <- !is.na(oa_data$is_oa) && oa_data$is_oa
  oa_status <- tolower(safe_value(oa_data$oa_status, "closed")) # Normalizza in lowercase

  # === DOI ===
  doi_url <- safe_value(oa_data$doi, "")
  doi_clean <- gsub("https://doi.org/", "", doi_url)

  # === PDF URL ===
  pdf_url <- safe_value(oa_data$pdf_url, "")
  has_pdf <- nzchar(pdf_url) && !is.na(pdf_url) && pdf_url != "Not available"

  # === CREA HTML COMPLETO ===
  tagList(
    div(
      class = "oa-document-summary",

      # Metadata Cards Row
      fluidRow(
        style = "margin-bottom: 20px;",

        # Card 1: Publication Year (invariata)
        column(
          3,
          div(
            style = "background: linear-gradient(135deg, #3498db 0%, #2980b9 100%); border-radius: 8px; padding: 15px; color: white; min-height: 85px; display: flex; flex-direction: column; justify-content: center; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            div(
              style = "display: flex; align-items: center;",
              icon(
                "calendar",
                style = "font-size: 28px; margin-right: 12px; opacity: 0.9;"
              ),
              div(
                tags$div(
                  style = "font-size: 10px; text-transform: uppercase; opacity: 0.85; letter-spacing: 0.5px;",
                  "Publication Year"
                ),
                tags$div(
                  style = "font-size: 24px; font-weight: bold; margin-top: 2px;",
                  pub_year
                )
              )
            )
          )
        ),

        # Card 2: Citations (invariata)
        column(
          3,
          div(
            style = "background: linear-gradient(135deg, #27ae60 0%, #229954 100%); border-radius: 8px; padding: 15px; color: white; min-height: 85px; display: flex; flex-direction: column; justify-content: center; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            div(
              style = "display: flex; align-items: center;",
              icon(
                "quote-right",
                style = "font-size: 28px; margin-right: 12px; opacity: 0.9;"
              ),
              div(
                tags$div(
                  style = "font-size: 10px; text-transform: uppercase; opacity: 0.85; letter-spacing: 0.5px;",
                  "Citations"
                ),
                tags$div(
                  style = "font-size: 24px; font-weight: bold; margin-top: 2px;",
                  format(as.numeric(citations), big.mark = ",")
                )
              )
            )
          )
        ),

        # Card 3: Open Access Status (MODIFICATA)
        column(3, {
          # Determina colore e icona in base a oa_status
          oa_colors <- list(
            closed = list(
              bg = "#95a5a6 0%, #7f8c8d",
              icon = "lock",
              text = "Closed"
            ),
            gold = list(
              bg = "#f39c12 0%, #e67e22",
              icon = "unlock",
              text = "Gold OA"
            ),
            green = list(
              bg = "#27ae60 0%, #229954",
              icon = "unlock",
              text = "Green OA"
            ),
            bronze = list(
              bg = "#cd7f32 0%, #b8732d",
              icon = "unlock",
              text = "Bronze OA"
            ),
            hybrid = list(
              bg = "#16a085 0%, #138d75",
              icon = "unlock",
              text = "Hybrid OA"
            )
          )

          # Se oa_status non è nei valori conosciuti, usa closed come default
          current_status <- if (oa_status %in% names(oa_colors)) {
            oa_status
          } else {
            "closed"
          }
          status_info <- oa_colors[[current_status]]

          div(
            style = paste0(
              "background: linear-gradient(135deg, ",
              status_info$bg,
              " 100%); border-radius: 8px; padding: 15px; color: white; min-height: 85px; display: flex; flex-direction: column; justify-content: center; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
            ),
            div(
              style = "display: flex; align-items: center;",
              icon(
                status_info$icon,
                style = "font-size: 28px; margin-right: 12px; opacity: 0.9;"
              ),
              div(
                tags$div(
                  style = "font-size: 10px; text-transform: uppercase; opacity: 0.85; letter-spacing: 0.5px;",
                  "Access"
                ),
                tags$div(
                  style = "font-size: 18px; font-weight: bold; margin-top: 2px;",
                  status_info$text
                )
              )
            )
          )
        }),

        # Card 4: FWCI (invariata)
        column(
          3,
          div(
            style = "background: linear-gradient(135deg, #f39c12 0%, #e67e22 100%); border-radius: 8px; padding: 15px; color: white; min-height: 85px; display: flex; flex-direction: column; justify-content: center; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            div(
              style = "display: flex; align-items: center;",
              icon(
                "chart-line",
                style = "font-size: 28px; margin-right: 12px; opacity: 0.9;"
              ),
              div(
                tags$div(
                  style = "font-size: 10px; text-transform: uppercase; opacity: 0.85; letter-spacing: 0.5px;",
                  "FWCI"
                ),
                tags$div(
                  style = "font-size: 24px; font-weight: bold; margin-top: 2px;",
                  if (!is.na(fwci)) round(as.numeric(fwci), 2) else "N/A"
                )
              )
            )
          )
        )
      ),

      # Authors Section - PIÙ COMPATTA
      if (!is.null(authors_html)) {
        div(
          class = "box box-primary",
          style = "margin-bottom: 15px;",
          div(
            class = "box-header with-border",
            h5(
              icon("users", style = "margin-right: 8px;"),
              "Authors",
              style = "color: #2E86AB; margin: 0; font-weight: 600;"
            )
          ),
          div(
            class = "box-body",
            style = "max-height: 250px; overflow-y: auto;",
            authors_html
          )
        )
      },

      # Journal Info
      div(
        class = "box box-success",
        style = "margin-bottom: 15px;",
        div(
          class = "box-header with-border",
          h5(
            icon("book", style = "margin-right: 8px;"),
            "Journal",
            style = "color: #27ae60; margin: 0; font-weight: 600;"
          )
        ),
        div(
          class = "box-body",
          style = "padding: 15px;",

          # Container principale
          div(
            style = "display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 20px;",

            # Colonna 1: Nome Journal e ISSN
            div(
              style = "flex: 1 1 300px; min-width: 250px;",
              tags$div(
                style = "font-size: 16px; font-weight: 600; color: #2c3e50; margin-bottom: 5px;",
                journal_name
              ),
              if (nzchar(issn) && issn != "Not available") {
                tags$div(
                  style = "color: #666; font-size: 13px;",
                  tags$strong("ISSN: "),
                  issn
                )
              }
            ),

            # Colonna 2: Document Type
            div(
              style = "flex: 0 0 auto; display: flex; align-items: center; gap: 8px;",
              tags$span(
                style = "color: #666; font-size: 13px; font-weight: 500;",
                "Type:"
              ),
              tags$span(
                class = "label label-info",
                style = "font-size: 12px; padding: 5px 12px; background-color: #3498db; text-transform: capitalize;",
                safe_value(oa_data$type, "Unknown")
              )
            ),

            # Colonna 3: DOI
            if (nzchar(doi_url) && doi_url != "Not available") {
              div(
                style = "flex: 0 0 auto; display: flex; align-items: center; gap: 8px;",
                tags$span(
                  style = "color: #666; font-size: 13px; font-weight: 500;",
                  "DOI:"
                ),
                tags$a(
                  href = doi_url,
                  target = "_blank",
                  style = "color: #3498db; text-decoration: none; font-family: 'Courier New', monospace; font-size: 13px; font-weight: 500;",
                  doi_clean,
                  icon(
                    "external-link-alt",
                    style = "margin-left: 5px; font-size: 11px;"
                  )
                )
              )
            }
          )
        )
      ),

      # Abstract
      div(
        class = "box box-info",
        style = "margin-bottom: 15px;",
        div(
          class = "box-header with-border",
          h5(
            icon("align-left", style = "margin-right: 8px;"),
            "Abstract",
            style = "color: #3498db; margin: 0; font-weight: 600;"
          )
        ),
        div(
          class = "box-body",
          tags$p(
            style = "text-align: justify; line-height: 1.7; color: #333;",
            abstract_text
          )
        )
      ),

      # Keywords - TUTTE
      if (!is.null(keywords_html) && length(keywords_html) > 0) {
        div(
          class = "box box-warning",
          style = "margin-bottom: 15px;",
          div(
            class = "box-header with-border",
            h5(
              icon("tags", style = "margin-right: 8px;"),
              "Keywords",
              tags$small(
                style = "color: #999; font-weight: normal; margin-left: 8px;",
                paste0("(", length(keywords_html), ")")
              ),
              style = "color: #f39c12; margin: 0; font-weight: 600;"
            )
          ),
          div(
            class = "box-body",
            keywords_html
          )
        )
      },

      # Topics - SOLO PRIMI 5
      if (!is.null(topics_html) && length(topics_html) > 0) {
        div(
          class = "box",
          style = "margin-bottom: 15px; border-top: 3px solid #9b59b6;",
          div(
            class = "box-header with-border",
            h5(
              icon("lightbulb", style = "margin-right: 8px;"),
              "Research Topics",
              tags$small(
                style = "color: #999; font-weight: normal; margin-left: 8px;",
                "(Top 5)"
              ),
              style = "color: #9b59b6; margin: 0; font-weight: 600;"
            )
          ),
          div(
            class = "box-body",
            topics_html
          )
        )
      },

      # Action Button - PDF
      if (has_pdf) {
        div(
          style = "margin-top: 20px; padding-top: 15px; border-top: 1px solid #ddd; text-align: center;",
          tags$a(
            class = "btn btn-danger btn-lg",
            href = pdf_url,
            target = "_blank",
            icon("file-pdf"),
            " Download Full Text PDF",
            style = "font-weight: bold; padding: 12px 30px;"
          )
        )
      }
    )
  )
}

reconstruct_from_txt <- function(file_lines) {
  reconstructed_txt <- list()
  current_section <- "Full_text"
  section_content <- ""
  for (line in file_lines) {
    if (grepl("^SECTION LINE SEPARATION: ", line)) {
      # Save the previous section content
      if (current_section != "Full_text") {
        reconstructed_txt[[current_section]] <- section_content
      }
      # Start a new section
      current_section <- sub("^SECTION LINE SEPARATION: ", "", line)
      section_content <- ""
    } else {
      section_content <- paste0(
        section_content,
        ifelse(section_content == "", "", "\n"),
        line
      )
    }
  }
  # Save the last section content
  reconstructed_txt[[current_section]] <- section_content
  if (!"Full_text" %in% names(reconstructed_txt)) {
    Full_text <- paste(reconstructed_txt, collapse = "\n\n")
    reconstructed_txt <- c(Full_text = Full_text, reconstructed_txt)
  }
  return(reconstructed_txt)
}
