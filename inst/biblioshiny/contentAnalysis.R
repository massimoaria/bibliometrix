## PDF to TEXT - Enhanced with manual column specification ----

pdf2txt_multicolumn_safe <- function(file, 
                                     n_columns = NULL, 
                                     column_threshold = NULL, 
                                     preserve_structure = TRUE) {
  
  has_poppler_config <- exists("poppler_config", where = asNamespace("pdftools"), mode = "function")
  
  if (has_poppler_config) {
    if (!pdftools::poppler_config()$has_pdf_data) {
      message("Pdf import feature requires a recent version of libpoppler. Please install it.")
      return(NA)
    }
  }
  
  tryCatch({
    data_list <- pdftools::pdf_data(file)
    all_text <- c()
    
    for (page_num in seq_along(data_list)) {
      page_data <- data_list[[page_num]]
      if (nrow(page_data) == 0) next
      
      # Determina il numero di colonne e i threshold
      if (!is.null(n_columns)) {
        # Modalità manuale: l'utente specifica il numero di colonne
        if (n_columns == 1) {
          # Una sola colonna: non dividere
          page_data <- page_data[order(page_data$y, page_data$x), ]
          page_text <- reconstruct_text_structured(page_data, preserve_structure)
        } else if (n_columns >= 2) {
          # Più colonne: usa k-means per trovare i centri
          x_positions <- page_data$x
          
          tryCatch({
            clusters <- kmeans(x_positions, centers = n_columns, nstart = 20)
            cluster_centers <- sort(clusters$centers[, 1])
            
            # Calcola i threshold tra i centri delle colonne
            thresholds <- numeric(n_columns - 1)
            for (i in 1:(n_columns - 1)) {
              thresholds[i] <- mean(c(cluster_centers[i], cluster_centers[i + 1]))
            }
            
            # Dividi il contenuto in colonne
            columns <- list()
            page_data$column <- cut(page_data$x, 
                                    breaks = c(-Inf, thresholds, Inf), 
                                    labels = FALSE)
            
            for (col in 1:n_columns) {
              col_data <- page_data[page_data$column == col, ]
              if (nrow(col_data) > 0) {
                col_data <- col_data[order(col_data$y, col_data$x), ]
                columns[[col]] <- reconstruct_text_structured(col_data, preserve_structure)
              } else {
                columns[[col]] <- ""
              }
            }
            
            # Combina le colonne
            if (preserve_structure) {
              page_text <- paste(columns, collapse = "\n\n")
            } else {
              page_text <- paste(columns, collapse = " ")
            }
            
          }, error = function(e) {
            message("K-means clustering failed for ", n_columns, " columns: ", e$message)
            # Fallback: dividi uniformemente
            page_width <- max(page_data$x) - min(page_data$x)
            column_width <- page_width / n_columns
            thresholds <- min(page_data$x) + column_width * (1:(n_columns - 1))
            
            columns <- list()
            page_data$column <- cut(page_data$x, 
                                    breaks = c(-Inf, thresholds, Inf), 
                                    labels = FALSE)
            
            for (col in 1:n_columns) {
              col_data <- page_data[page_data$column == col, ]
              if (nrow(col_data) > 0) {
                col_data <- col_data[order(col_data$y, col_data$x), ]
                columns[[col]] <- reconstruct_text_structured(col_data, preserve_structure)
              }
            }
            
            page_text <- paste(columns, collapse = ifelse(preserve_structure, "\n\n", " "))
          })
        }
      } else {
        # Modalità automatica: rileva il numero di colonne
        if (is.null(column_threshold)) {
          x_positions <- page_data$x
          if (length(unique(x_positions)) > 20) {
            tryCatch({
              # Prova prima con 2 colonne
              clusters <- kmeans(x_positions, centers = 2, nstart = 10)
              cluster_centers <- sort(clusters$centers[, 1])
              column_threshold <- mean(cluster_centers)
            }, error = function(e) {
              column_threshold <- (max(page_data$x) + min(page_data$x)) / 2
            })
          } else {
            column_threshold <- (max(page_data$x) + min(page_data$x)) / 2
          }
        }
        
        left_column <- page_data[page_data$x < column_threshold, ]
        right_column <- page_data[page_data$x >= column_threshold, ]
        
        if (nrow(left_column) < 5 || nrow(right_column) < 5) {
          # Probabilmente una sola colonna
          page_data <- page_data[order(page_data$y, page_data$x), ]
          page_text <- reconstruct_text_structured(page_data, preserve_structure)
        } else {
          # Due colonne
          left_column <- left_column[order(left_column$y, left_column$x), ]
          right_column <- right_column[order(right_column$y, right_column$x), ]
          
          left_text <- reconstruct_text_structured(left_column, preserve_structure)
          right_text <- reconstruct_text_structured(right_column, preserve_structure)
          
          if (preserve_structure) {
            page_text <- paste(left_text, right_text, sep = "\n\n")
          } else {
            page_text <- paste(left_text, right_text, sep = " ")
          }
        }
      }
      
      all_text <- c(all_text, page_text)
    }
    
    # Post-processing del testo
    if (preserve_structure) {
      txt <- paste(all_text, collapse = "\n\n")
      txt <- gsub("([0-9]+(?:\\.[0-9]+)*\\.\\s+[A-Z][A-Za-z\\s]{3,50})", "\n\n\\1", txt)
      txt <- gsub("\\s+([A-Z][A-Z\\s]{10,60})\\s+", "\n\n\\1\n\n", txt)
      txt <- gsub("([.!?])\\s+([A-Z][a-z])", "\\1\n\n\\2", txt)
      txt <- gsub("\\n{3,}", "\n\n", txt)
    } else {
      txt <- paste(all_text, collapse = " ")
    }
    
    # Rimuove trattini di sillabazione
    txt <- gsub("-\\s*\n", "", txt)
    txt <- gsub("-\\s+", "", txt)
    
    # Normalizza spazi
    if (preserve_structure) {
      txt <- gsub("[ \t]+", " ", txt)
      txt <- gsub("\\n ", "\n", txt)
    } else {
      txt <- gsub("\\s+", " ", txt)
    }
    
    txt <- trimws(txt)
    return(txt)
    
  }, error = function(e) {
    message("pdf_data failed, falling back to pdf_text method: ", e$message)
    
    pages <- pdftools::pdf_length(file)
    txt <- pdftools::pdf_text(file)
    
    if (preserve_structure) {
      txt <- gsub("([0-9]+(?:\\.[0-9]+)*\\.\\s+[A-Z][A-Za-z\\s]{3,50})", "\n\n\\1", txt)
      txt <- gsub("\\n\\s*\\n", "\n\n", txt)
      txt <- gsub("([.!?])\\s*\n\\s*([A-Z])", "\\1\n\n\\2", txt)
      txt <- gsub("(?<![.!?\\n])\\n(?![A-Z0-9\\n])", " ", txt, perl = TRUE)
      txt <- paste(txt, collapse = "\n\n")
    } else {
      txt <- gsub("(?<![\\s\\.])\\n(?!\\s)", " ", txt, perl = TRUE)
      txt <- gsub("\n  ", "\n\n", txt)
      txt <- paste(txt, collapse = " ")
    }
    
    txt <- gsub("-\\s", "", txt)
    return(txt)
  })
}

reconstruct_text_structured <- function(column_data, preserve_structure = TRUE) {
  if (nrow(column_data) == 0) return("")
  
  tolerance <- 4
  column_data$line <- round(column_data$y / tolerance) * tolerance
  
  available_cols <- names(column_data)
  
  if ("height" %in% available_cols) {
    column_data$font_size <- column_data$height
  } else {
    column_data$font_size <- 12
  }
  
  lines <- split(column_data, column_data$line)
  
  line_results <- lapply(lines, function(line) {
    line <- line[order(line$x), ]
    
    line_text <- paste(line$text, collapse = " ")
    line_text <- trimws(line_text)
    
    avg_font_size <- mean(line$font_size, na.rm = TRUE)
    is_short <- nchar(line_text) < 80
    is_caps <- grepl("^[A-Z\\s\\d\\.\\-]+$", line_text)
    starts_with_number <- grepl("^\\d+\\.", line_text)
    starts_with_section <- grepl("^\\d+\\.\\d+", line_text)
    
    is_title <- (is_short && (is_caps || starts_with_number || starts_with_section))
    
    return(list(
      text = line_text,
      y = min(line$y),
      is_title = is_title,
      font_size = avg_font_size,
      starts_with_number = starts_with_number
    ))
  })
  
  line_results <- line_results[sapply(line_results, function(x) nchar(x$text) > 0)]
  line_results <- line_results[order(sapply(line_results, function(x) x$y))]
  
  if (!preserve_structure) {
    result <- paste(sapply(line_results, function(x) x$text), collapse = " ")
  } else {
    result_parts <- c()
    
    for (i in seq_along(line_results)) {
      current_line <- line_results[[i]]
      line_text <- current_line$text
      
      if (nchar(line_text) == 0) next
      
      if (i == 1) {
        result_parts <- c(result_parts, line_text)
      } else {
        prev_line <- line_results[[i-1]]
        
        if (current_line$is_title) {
          result_parts <- c(result_parts, "\n\n", line_text)
        } else if (prev_line$is_title) {
          result_parts <- c(result_parts, "\n\n", line_text)
        } else if (grepl("[.!?]\\s*$", prev_line$text) &&
                   grepl("^[A-Z]", line_text) &&
                   !grepl("^[A-Z][a-z]+\\s+[a-z]", line_text)) {
          result_parts <- c(result_parts, "\n\n", line_text)
        } else {
          result_parts <- c(result_parts, " ", line_text)
        }
      }
    }
    
    result <- paste(result_parts, collapse = "")
    result <- gsub("\\s+", " ", result)
    result <- gsub("\\n\\s+", "\n", result)
    result <- gsub("\\n{3,}", "\n\n", result)
  }
  
  result <- trimws(result)
  return(result)
}

## SEZIONI - VERSIONE SEMPLICE ----

extract_all_titles <- function(toc_node) {
  titles <- character(0)
  
  if (!is.null(toc_node$title) && nchar(toc_node$title) > 0) {
    titles <- c(titles, toc_node$title)
  }
  
  if (!is.null(toc_node$children) && length(toc_node$children) > 0) {
    for (child in toc_node$children) {
      titles <- c(titles, extract_all_titles(child))
    }
  }
  
  return(titles)
}

split_into_sections <- function(text, file_path = NULL) {
  
  common_sections <- c(
    "Abstract", "Introduction", "Related work", "Related Work",
    "Background", "Literature review", "Literature Review",
    "Methodology", "Methods", "Materials and methods", "Materials and Methods",
    "Experimental design", "Experimental Design", "Analysis",
    "Results", "Results and discussion", "Results and Discussion",
    "Discussion", "Comparison study", "Comparison Study",
    "Conclusion", "Conclusions",
    "Acknowledgment", "Acknowledgments", "Acknowledgement", "Acknowledgements",
    "References", "Bibliography", "Appendix"
  )
  
  section_names <- NULL
  if (!is.null(file_path) && file.exists(file_path)) {
    tryCatch({
      toc <- pdftools::pdf_toc(file_path)
      if (!is.null(toc) && length(toc) > 0) {
        section_names <- extract_all_titles(toc)
        section_names <- section_names[nchar(section_names) > 0]
        if (length(section_names) > 0) {
          message(sprintf("Using %d sections from PDF table of contents", length(section_names)))
        } else {
          section_names <- NULL
        }
      }
    }, error = function(e) {
      message("Could not extract TOC from PDF, using common section names")
    })
  }
  
  if (is.null(section_names) || length(section_names) == 0) {
    section_names <- common_sections
    message("Using common section names for pattern matching")
  }
  
  sections_found <- list()
  
  for (section in section_names) {
    section_escaped <- gsub("([.|?*+^$()\\[\\]{}\\\\])", "\\\\\\1", section, perl = TRUE)
    pattern <- paste0("\\n\\n(?:[0-9]+(?:\\.[0-9]+)*\\.\\n\\n)?", section_escaped, "(?=\\s)")
    match <- regexpr(pattern, text, ignore.case = TRUE, perl = TRUE)
    
    if (match > 0) {
      match_length <- attr(match, "match.length")
      sections_found[[section]] <- list(
        start = as.integer(match),
        length = match_length
      )
    }
  }
  
  if (length(sections_found) == 0) {
    message("No clear sections found. Returning full text as single element.")
    return(list("Full text" = text))
  }
  
  sections_found <- sections_found[order(sapply(sections_found, function(x) x$start))]
  sections_list <- list()
  section_names_ordered <- names(sections_found)
  
  first_start <- sections_found[[1]]$start
  if (first_start > 100) {
    preface <- trimws(substr(text, 1, first_start - 1))
    if (nchar(preface) > 50) {
      sections_list[["Preface"]] <- preface
    }
  }
  
  for (i in seq_along(sections_found)) {
    section_name <- section_names_ordered[i]
    content_start <- sections_found[[i]]$start + sections_found[[i]]$length
    
    content_end <- if (i < length(sections_found)) {
      sections_found[[i + 1]]$start - 1
    } else {
      nchar(text)
    }
    
    content <- substr(text, content_start, content_end)
    content <- trimws(content)
    
    original_name <- section_name
    counter <- 1
    while (section_name %in% names(sections_list)) {
      counter <- counter + 1
      section_name <- paste0(original_name, " (", counter, ")")
    }
    
    if (nchar(content) > 0) {
      sections_list[[section_name]] <- content
    }
  }
  
  message(sprintf("Found %d sections: %s", 
                  length(sections_list), 
                  paste(names(sections_list), collapse = ", ")))
  
  return(sections_list)
}

pdf2txt_auto <- function(file, 
                         n_columns = NULL, 
                         preserve_structure = TRUE, 
                         sections = FALSE) {
  
  # Try multi-column method with column specification
  result <- pdf2txt_multicolumn_safe(file, 
                                     n_columns = n_columns,
                                     preserve_structure = preserve_structure)
  
  if (is.na(result) || nchar(result) < 100) {
    message("Multi-column method failed or returned short text, trying original method...")
    
    tryCatch({
      pages <- pdftools::pdf_length(file)
      txt <- pdftools::pdf_text(file)
      
      if (preserve_structure) {
        txt <- gsub("([0-9]+(?:\\.[0-9]+)*\\.\\s+[A-Za-z][A-Za-z\\s]{3,50})", "\n\n\\1\n\n", txt, perl = TRUE)
        txt <- gsub("\\n\\s*\\n", "\n\n", txt)
        txt <- gsub("([.!?])\\s*\n\\s*([A-Z][a-z])", "\\1\n\n\\2", txt, perl = TRUE)
        txt <- gsub("(?<![.!?\\n])\\n(?![A-Z0-9\\n])", " ", txt, perl = TRUE)
        txt <- paste(txt, collapse = "\n\n")
      } else {
        txt <- gsub("(?<![\\s\\.])\\n(?!\\s)", " ", txt, perl = TRUE)
        txt <- gsub("\n  ", "\n\n", txt)
        txt <- paste(txt, collapse = " ")
      }
      
      txt <- gsub("-\\s", "", txt)
      result <- txt
      
    }, error = function(e) {
      message("All methods failed: ", e$message)
      return(NA)
    })
  }
  
  if (sections && !is.na(result)) {
    result <- split_into_sections(result, file_path = file)
  }
  
  return(result)
}
## CONTENT ANALYSIS ----

#' Enhanced Scientific Article Content Analysis with Comprehensive Citation Extraction
#'
#' This function performs comprehensive content analysis of scientific articles,
#' with enhanced citation extraction that handles narrative citations, complex
#' multiple citations, and various citation formats.
#'
#' @param text A character vector containing the scientific article text
#' @param window_size Integer, number of words before and after citations to extract (default: 10)
#' @param min_word_length Integer, minimum word length for analysis (default: 3)
#' @param remove_stopwords Logical, whether to remove stopwords (default: TRUE)
#' @param language Character, language for stopwords removal (default: "en")
#' @param custom_stopwords Character vector of additional stopwords to remove
#' @param ngram_range Vector of integers, range for n-gram analysis (default: c(1,3))
#' @param parse_multiple_citations Logical, whether to parse complex multiple citations into individual ones (default: TRUE)
#'
#' @return A list containing enhanced citation analysis results
analyze_scientific_content_enhanced <- function(text, 
                                                window_size = 10,
                                                min_word_length = 3,
                                                remove_stopwords = TRUE,
                                                language = "en",
                                                custom_stopwords = NULL,
                                                ngram_range = c(1, 3),
                                                parse_multiple_citations = TRUE) {

  # Initialize results list
  results <- list()
  
  # Clean and prepare text
  clean_text <- text %>%
    stringr::str_replace_all("\\n+", " ") %>%  # Replace newlines with spaces
    stringr::str_replace_all("\\s+", " ") %>%  # Normalize whitespace
    stringr::str_trim()
  
  # Basic text statistics
  text_stats <- data.frame(
    total_characters = nchar(clean_text),
    total_words = length(str_split(clean_text, "\\s+")[[1]]),
    total_sentences = length(str_split(clean_text, "[.!?]+")[[1]]),
    avg_words_per_sentence = length(str_split(clean_text, "\\s+")[[1]]) / 
      length(str_split(clean_text, "[.!?]+")[[1]])
  )
  
  # ===========================================
  # ENHANCED CITATION EXTRACTION
  # ===========================================
  
  # Comprehensive citation patterns (ordered by priority)
  citation_patterns <- list(
    # COMPLEX MULTIPLE CITATIONS (must come first to avoid partial matches)
    complex_multiple_citations = "\\((?:see\\s+)?(?:e\\.g\\.\\s+)?[A-Z][^)]*(?:\\d{4}[a-z]?[;,][^)]*){2,}\\d{4}[a-z]?[^)]*\\)",
    
    # NARRATIVE CITATIONS (Author et al. (Year) format)
    narrative_etal = "[A-Z][A-Za-z'-]+(?:\\s*,\\s*[A-Z][A-Za-z'-]+)*(?:\\s*,\\s*et\\s+al\\.)?\\s*\\(\\d{4}[a-z]?\\)",
    narrative_multiple_authors = "[A-Z][A-Za-z'-]+(?:\\s*,\\s*[A-Z][A-Za-z'-]+)+(?:\\s*,\\s*&\\s*[A-Z][A-Za-z'-]+)?\\s*\\(\\d{4}[a-z]?\\)",
    narrative_single = "[A-Z][A-Za-z'-]+\\s*\\(\\d{4}[a-z]?\\)",
    
    # PARENTHETICAL CITATIONS
    multiple_citations_semicolon = "\\([A-Z][A-Za-z'-]+[^)]*\\d{4}[a-z]?(?:\\s*;\\s*[A-Z][^)]*\\d{4}[a-z]?)+[^)]*\\)",
    see_citations = "\\((?:see|e\\.g\\.|cf\\.|compare)\\s+[A-Z][A-Za-z'-]+[^)]+\\d{4}[a-z]?\\)",
    author_year_etal = "\\([A-Z][A-Za-z'-]+\\s+et\\s+al\\.,\\s*\\d{4}[a-z]?\\)",
    author_year_ampersand = "\\([A-Z][A-Za-z'-]+[^)]*&[^)]*\\d{4}[a-z]?\\)",
    author_year_basic = "\\([A-Z][A-Za-z'-]+(?:\\s+[A-Z][A-Za-z'-]+)*,\\s*\\d{4}[a-z]?\\)",
    
    # OTHER CITATION FORMATS
    numbered_simple = "\\[\\d+\\]",
    numbered_multiple = "\\[\\d+(?:[-,;\\s]+\\d+)*\\]",
    superscript = "[¹²³⁴⁵⁶⁷⁸⁹⁰]+",
    doi_pattern = "https?://doi\\.org/[\\w\\./\\-]+"
  )
  
  # Extract all citations
  all_citations <- tibble()
  
  for (pattern_name in names(citation_patterns)) {
    pattern <- citation_patterns[[pattern_name]]
    
    # Find all matches with positions
    matches <- str_locate_all(clean_text, pattern)[[1]]
    
    if (nrow(matches) > 0) {
      citations_temp <- tibble(
        citation_type = pattern_name,
        citation_text = str_sub(clean_text, matches[,1], matches[,2]),
        start_pos = matches[,1],
        end_pos = matches[,2],
        citation_id = paste0(pattern_name, "_", 1:nrow(matches))
      )
      
      all_citations <- bind_rows(all_citations, citations_temp)
    }
  }
  
  # Remove duplicates and sort by position
  all_citations <- all_citations %>%
    distinct(citation_text, start_pos, .keep_all = TRUE) %>%
    arrange(start_pos)
  
  # ===========================================
  # POST-PROCESSING: PARSE COMPLEX CITATIONS
  # ===========================================
  
  parsed_citations <- tibble()
  
  if (parse_multiple_citations && nrow(all_citations) > 0) {
    for (i in 1:nrow(all_citations)) {
      citation <- all_citations[i,]
      
      # If it's a complex multiple citation, parse it into individual citations
      if (citation$citation_type == "complex_multiple_citations") {
        # Remove outer parentheses and prefixes
        inner_text <- str_replace(citation$citation_text, "^\\((?:see\\s+)?(?:e\\.g\\.\\s+)?", "")
        inner_text <- str_replace(inner_text, "\\)$", "")
        
        # Split by semicolons to get individual citations
        individual_citations <- str_split(inner_text, "\\s*;\\s*")[[1]]
        
        for (j in seq_along(individual_citations)) {
          indiv_cite <- stringr::str_trim(individual_citations[j])
          if (str_detect(indiv_cite, "\\d{4}")) {  # Must contain a year
            parsed_row <- tibble(
              citation_type = "parsed_from_multiple",
              citation_text = paste0("(", indiv_cite, ")"),
              start_pos = citation$start_pos,
              end_pos = citation$end_pos,
              citation_id = paste0("parsed_multiple_", i, "_", j),
              original_complex_citation = citation$citation_text
            )
            parsed_citations <- bind_rows(parsed_citations, parsed_row)
          }
        }
      } else {
        # Keep the original citation
        citation$original_complex_citation <- NA
        parsed_citations <- bind_rows(parsed_citations, citation)
      }
    }
    
    # Update all_citations with parsed results
    all_citations <- parsed_citations
  }
  
  # ===========================================
  # ENHANCED NARRATIVE CITATION PROCESSING
  # ===========================================
  
  if (nrow(all_citations) > 0) {
    # For narrative citations, create standardized format
    narrative_citations <- all_citations %>%
      filter(str_detect(citation_type, "narrative")) %>%
      mutate(
        # Extract author and year separately for narrative citations
        author_part = str_extract(citation_text, "^[^(]+"),
        year_part = str_extract(citation_text, "\\(\\d{4}[a-z]?\\)"),
        # Create a standardized parenthetical format
        standardized_citation = paste0("(", stringr::str_trim(author_part), ", ", stringr::str_replace_all(year_part, "[()]", ""), ")")
      )
    
    # Add standardized format to the data
    if (nrow(narrative_citations) > 0) {
      all_citations <- all_citations %>%
        left_join(
          narrative_citations %>% 
            select(citation_id, author_part, year_part, standardized_citation),
          by = "citation_id"
        ) %>%
        mutate(
          citation_text_clean = ifelse(
            !is.na(standardized_citation), 
            standardized_citation, 
            citation_text
          )
        )
    } else {
      all_citations$citation_text_clean <- all_citations$citation_text
      all_citations$author_part <- NA
      all_citations$year_part <- NA
      all_citations$standardized_citation <- NA
    }
  }
  
  # ===========================================
  # CITATION CONTEXT EXTRACTION
  # ===========================================
  
  citation_contexts <- tibble()
  
  if (nrow(all_citations) > 0) {
    
    for (i in 1:nrow(all_citations)) {
      citation <- all_citations[i,]
      
      # Get citation boundaries in the original text
      citation_start <- citation$start_pos
      citation_end <- citation$start_pos + nchar(citation$citation_text) - 1
      
      # Extract text before and after citation
      text_before_citation <- substr(clean_text, 1, citation_start - 1)
      text_after_citation <- substr(clean_text, citation_end + 1, nchar(clean_text))
      
      # Tokenize the text before citation
      words_before_df <- tibble(text = text_before_citation) %>%
        unnest_tokens(word, text, token = "words", to_lower = FALSE)
      
      # Get last N words before citation
      n_words_before <- nrow(words_before_df)
      if (n_words_before > 0) {
        start_idx <- max(1, n_words_before - window_size + 1)
        words_before <- words_before_df %>%
          slice(start_idx:n_words_before) %>%
          pull(word) %>%
          paste(collapse = " ")
      } else {
        words_before <- ""
      }
      
      # Tokenize the text after citation
      words_after_df <- tibble(text = text_after_citation) %>%
        unnest_tokens(word, text, token = "words", to_lower = FALSE)
      
      # Get first N words after citation
      n_words_after <- nrow(words_after_df)
      if (n_words_after > 0) {
        end_idx <- min(window_size, n_words_after)
        words_after <- words_after_df %>%
          slice(1:end_idx) %>%
          pull(word) %>%
          paste(collapse = " ")
      } else {
        words_after <- ""
      }
      
      # Create full context
      full_context <- paste(
        words_before,
        citation$citation_text,
        words_after
      ) %>% stringr::str_trim()
      
      # Count total words in context
      context_word_count <- n_words_before + n_words_after + 
        length(strsplit(citation$citation_text, "\\s+")[[1]])
      
      context_row <- tibble(
        citation_id = citation$citation_id,
        citation_text = citation$citation_text,
        citation_text_clean = citation$citation_text_clean,
        citation_type = citation$citation_type,
        words_before = words_before,
        words_after = words_after,
        full_context = full_context,
        context_word_count = context_word_count,
        citation_position_in_text = citation$start_pos,
        is_narrative = str_detect(citation$citation_type, "narrative"),
        is_parsed_multiple = citation$citation_type == "parsed_from_multiple"
      )
      
      # Add original complex citation info if available
      if ("original_complex_citation" %in% names(citation) && !is.na(citation$original_complex_citation)) {
        context_row$original_complex_citation <- citation$original_complex_citation
      }
      
      citation_contexts <- bind_rows(citation_contexts, context_row)
    }
  }
  
  # ===========================================
  # WORD FREQUENCY ANALYSIS
  # ===========================================
  
  # Tokenize text for analysis
  tokens <- tibble(text = clean_text) %>%
    unnest_tokens(word, text) %>%
    filter(str_length(word) >= min_word_length) %>%
    filter(!str_detect(word, "^\\d+$"))  # Remove pure numbers
  
  # Remove stopwords if requested
  if (remove_stopwords) {
    data(stop_words)
    tokens <- tokens %>%
      anti_join(stop_words, by = "word")
    
    # Remove custom stopwords if provided
    if (!is.null(custom_stopwords)) {
      custom_stops <- tibble(word = custom_stopwords)
      tokens <- tokens %>%
        anti_join(custom_stops, by = "word")
    }
  }
  
  # Calculate word frequencies
  word_frequencies <- tokens %>%
    count(word, sort = TRUE) %>%
    mutate(
      frequency = n / sum(n),
      rank = row_number()
    )
  
  # ===========================================
  # N-GRAM ANALYSIS
  # ===========================================
  
  ngrams_results <- list()
  
  # Prepare text for n-gram analysis
  
  ## remove numbers and punctuation for n-grams
  clean_text_filtered<- text %>%
    stringr::str_replace_all("\\n+", " ") %>%
    stringr::str_replace_all("\\d+", " ") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim()
  
  if (remove_stopwords){
    word_tokens <- tibble(text = clean_text_filtered) %>% 
      unnest_tokens(word, text, token = "words") %>%
        anti_join(stop_words, by = c("word"))
    clean_text_filtered <- word_tokens %>%
      pull(word) %>%
      paste(collapse = " ")
  } else {
    clean_text_filtered <- clean_text
  }
  
  for (n in ngram_range[1]:ngram_range[2]) {
    if (n == 1) {
      ngrams_results[[paste0(n, "gram")]] <- word_frequencies %>%
        slice_head(n = 15)
    } else {
      ngram_tokens <- tibble(text = clean_text_filtered) %>%
          unnest_tokens(ngram, text, token = "ngrams", n = n) %>%
        filter(!is.na(ngram)) %>%
        count(ngram, sort = TRUE) %>%
        slice_head(n = 15) %>%
        mutate(frequency = n / sum(n))
      
      ngrams_results[[paste0(n, "gram")]] <- ngram_tokens
    }
  }
  
  # ===========================================
  # ENHANCED CITATION ANALYSIS METRICS
  # ===========================================
  
  citation_metrics <- list()
  
  if (nrow(all_citations) > 0) {
    # Citation type distribution
    citation_metrics$type_distribution <- all_citations %>%
      count(citation_type, sort = TRUE) %>%
      mutate(percentage = round(n / sum(n) * 100, 2))
    
    # Narrative vs parenthetical citation ratio
    citation_metrics$narrative_ratio <- all_citations %>%
      summarise(
        total_citations = n(),
        narrative_citations = sum(str_detect(citation_type, "narrative")),
        parenthetical_citations = sum(!str_detect(citation_type, "narrative")),
        narrative_percentage = round(narrative_citations / total_citations * 100, 2)
      )
    
    # Complex citation analysis
    if (parse_multiple_citations) {
      citation_metrics$complex_citations <- all_citations %>%
        filter(citation_type == "parsed_from_multiple") %>%
        count(original_complex_citation, sort = TRUE) %>%
        rename(individual_citations_extracted = n)
    }
    
    # Citation density (citations per 1000 words)
    citation_metrics$density <- list(
      citations_per_1000_words = round((nrow(all_citations) / text_stats$total_words) * 1000, 2),
      avg_words_between_citations = if(nrow(all_citations) > 1) {
        round(text_stats$total_words / nrow(all_citations), 2)
      } else {
        text_stats$total_words
      }
    )
  }
  
  # # ===========================================
  # # SENTIMENT ANALYSIS (if available)
  # # ===========================================
  # 
  # sentiment_scores <- NULL
  # 
  # tryCatch({
  #   afinn_scores <- tokens %>%
  #     inner_join(get_sentiments("afinn"), by = "word") %>%
  #     summarise(
  #       afinn_score = sum(value),
  #       afinn_mean = mean(value),
  #       positive_words = sum(value > 0),
  #       negative_words = sum(value < 0)
  #     )
  #   
  #   bing_scores <- tokens %>%
  #     inner_join(get_sentiments("bing"), by = "word") %>%
  #     count(sentiment) %>%
  #     pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  #     mutate(sentiment_score = positive - negative)
  #   
  #   sentiment_scores <- bind_cols(afinn_scores, bing_scores)
  #   
  # }, error = function(e) {
  #   sentiment_scores <- tibble(note = "Sentiment analysis requires textdata package and internet connection")
  # })
  
  # ===========================================
  # CITATION CO-OCCURRENCE NETWORK DATA
  # ===========================================
  
  citation_cooccurrence <- NULL
  
  if (nrow(all_citations) > 1) {
    citation_pairs <- tibble()
    
    for (i in 1:(nrow(all_citations)-1)) {
      for (j in (i+1):nrow(all_citations)) {
        distance <- all_citations$start_pos[j] - all_citations$end_pos[i]
        if (distance <= 1000) {  # Citations within 1000 characters
          pair <- tibble(
            citation1 = all_citations$citation_text_clean[i],
            citation2 = all_citations$citation_text_clean[j],
            distance = distance,
            type1 = all_citations$citation_type[i],
            type2 = all_citations$citation_type[j]
          )
          citation_pairs <- bind_rows(citation_pairs, pair)
        }
      }
    }
    
    citation_cooccurrence <- citation_pairs
  }
  
  # ===========================================
  # COMPILE ENHANCED RESULTS
  # ===========================================
  
  results$text_analytics <- list(
    basic_stats = text_stats,
    total_citations_found = nrow(all_citations),
    citation_types_found = unique(all_citations$citation_type),
    most_frequent_words = word_frequencies %>% slice_head(n = 20)
  )
  
  results$citations <- all_citations
  results$citation_contexts <- citation_contexts
  results$citation_metrics <- citation_metrics
  results$word_frequencies <- word_frequencies
  results$ngrams <- ngrams_results
  #results$sentiment_analysis <- sentiment_scores
  results$network_data <- citation_cooccurrence
  
  # Enhanced summary
  results$summary <- list(
    total_words_analyzed = nrow(tokens),
    unique_words = nrow(word_frequencies),
    citations_extracted = nrow(all_citations),
    narrative_citations = sum(str_detect(all_citations$citation_type, "narrative"), na.rm = TRUE),
    parenthetical_citations = sum(!str_detect(all_citations$citation_type, "narrative"), na.rm = TRUE),
    complex_citations_parsed = sum(all_citations$citation_type == "parsed_from_multiple", na.rm = TRUE),
    lexical_diversity = nrow(word_frequencies) / nrow(tokens),
    average_citation_context_length = if(nrow(citation_contexts) > 0) mean(citation_contexts$context_word_count) else 0,
    citation_density_per_1000_words = if(nrow(all_citations) > 0) round((nrow(all_citations) / text_stats$total_words) * 1000, 2) else 0
  )
  
  class(results) <- c("enhanced_scientific_content_analysis", "list")
  return(results)
}

# ===========================================
# ENHANCED PRINT METHOD
# ===========================================

#' Print method for enhanced scientific content analysis
#' @param x An enhanced_scientific_content_analysis object
#' @param ... Additional arguments
#' @export
print.enhanced_scientific_content_analysis <- function(x, ...) {
  cat("Enhanced Scientific Article Content Analysis\n")
  cat("===========================================\n\n")
  
  cat("Basic Statistics:\n")
  cat("- Total characters:", x$text_analytics$basic_stats$total_characters, "\n")
  cat("- Total words:", x$text_analytics$basic_stats$total_words, "\n")
  cat("- Total sentences:", x$text_analytics$basic_stats$total_sentences, "\n")
  cat("- Avg words per sentence:", round(x$text_analytics$basic_stats$avg_words_per_sentence, 2), "\n\n")
  
  cat("Enhanced Citation Analysis:\n")
  cat("- Total citations found:", x$summary$citations_extracted, "\n")
  cat("- Narrative citations:", x$summary$narrative_citations, "\n")
  cat("- Parenthetical citations:", x$summary$parenthetical_citations, "\n")
  cat("- Complex citations parsed:", x$summary$complex_citations_parsed, "\n")
  cat("- Citation density:", x$summary$citation_density_per_1000_words, "per 1000 words\n")
  
  if (!is.null(x$citation_metrics$type_distribution)) {
    cat("\nCitation Types Found:\n")
    for (i in 1:nrow(x$citation_metrics$type_distribution)) {
      type_info <- x$citation_metrics$type_distribution[i,]
      cat(sprintf("- %s: %d (%.1f%%)\n", type_info$citation_type, type_info$n, type_info$percentage))
    }
  }
  
  cat("\nLexical Analysis:\n")
  cat("- Total words analyzed:", x$summary$total_words_analyzed, "\n")
  cat("- Unique words:", x$summary$unique_words, "\n")
  cat("- Lexical diversity:", round(x$summary$lexical_diversity, 3), "\n\n")
  
  cat("Top 5 most frequent words:\n")
  print(x$word_frequencies %>% slice_head(n = 5))
  
  if (x$summary$complex_citations_parsed > 0) {
    cat("\nNote: Complex multiple citations were parsed into", x$summary$complex_citations_parsed, "individual citations.\n")
    cat("See $citation_metrics$complex_citations for details.\n")
  }
}

# ===========================================
# EXAMPLE USAGE
# ===========================================

# Example with the provided text (uncomment to run):
# txt <- "Your scientific article text here..."
# results <- analyze_scientific_content_enhanced(
#   text = txt,
#   window_size = 12,
#   parse_multiple_citations = TRUE,
#   custom_stopwords = c("machine", "learning", "random", "forest")
# )
# print(results)
# View(results$citation_contexts)


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
               condition = "output.text_extracted && !output.analysis_completed",
               
               div(class = "box box-info",
                   div(class = "box-header with-border",
                       h4("Extracted Text Preview", class = "box-title", style = "color: #3498db;"),
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
                       ),
                       conditionalPanel(
                         condition = "!output.preview_visible",
                         div(
                           style = "text-align: center; padding: 20px; color: #666;",
                           icon("eye-slash", style = "font-size: 24px; margin-bottom: 10px;"),
                           br(),
                           "Text preview is hidden. Click 'Show Preview' to view the extracted text."
                         )
                       )
                   )
               ),
               
               br()
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
  
  require(visNetwork)
  require(dplyr)
  require(stringr)
  
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
  
  # Extract text from PDF
  observeEvent(input$extract_text, {
    req(input$pdf_file)
    
    tryCatch({
      if (is.na(input$Columns)) {
        n_columns <- NULL
      } else {
        n_columns <- input$Columns
      }
      values$pdf_text <- pdf2txt_auto(input$pdf_file$datapath, n_columns = n_columns)
      
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
        text = values$pdf_text,
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
        citation_boxes <- lapply(1:min(50, nrow(contexts)), function(i) {  # Limit to 50 for performance
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
              
              # Citation (center, highlighted)
              div(
                style = paste0(
                  "background-color: ", box_color, "; color: white; ",
                  "padding: 8px 12px; border-radius: 6px; font-weight: bold; ",
                  "font-size: 14px; white-space: nowrap; max-width: 300px; ",
                  "overflow: hidden; text-overflow: ellipsis;"
                ),
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
              }
            )
          )
        })
        
        do.call(tagList, citation_boxes)
        
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