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
      
      if (!is.null(n_columns)) {
        if (n_columns == 1) {
          page_data <- page_data[order(page_data$y, page_data$x), ]
          page_text <- reconstruct_text_structured(page_data, preserve_structure)
        } else if (n_columns >= 2) {
          x_positions <- page_data$x
          
          tryCatch({
            clusters <- kmeans(x_positions, centers = n_columns, nstart = 20)
            cluster_centers <- sort(clusters$centers[, 1])
            
            thresholds <- numeric(n_columns - 1)
            for (i in 1:(n_columns - 1)) {
              thresholds[i] <- mean(c(cluster_centers[i], cluster_centers[i + 1]))
            }
            
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
            
            if (preserve_structure) {
              page_text <- paste(columns, collapse = "\n\n")
            } else {
              page_text <- paste(columns, collapse = " ")
            }
            
          }, error = function(e) {
            message("K-means clustering failed for ", n_columns, " columns: ", e$message)
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
        if (is.null(column_threshold)) {
          x_positions <- page_data$x
          if (length(unique(x_positions)) > 20) {
            tryCatch({
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
          page_data <- page_data[order(page_data$y, page_data$x), ]
          page_text <- reconstruct_text_structured(page_data, preserve_structure)
        } else {
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
    
    if (preserve_structure) {
      txt <- paste(all_text, collapse = "\n\n")
      txt <- gsub("([0-9]+(?:\\.[0-9]+)*\\.\\s+[A-Z][A-Za-z\\s]{3,50})", "\n\n\\1", txt)
      txt <- gsub("\\s+([A-Z][A-Z\\s]{10,60})\\s+", "\n\n\\1\n\n", txt)
      txt <- gsub("([.!?])\\s+([A-Z][a-z])", "\\1\n\n\\2", txt)
      txt <- gsub("\\n{3,}", "\n\n", txt)
    } else {
      txt <- paste(all_text, collapse = " ")
    }
    
    txt <- gsub("-\\s*\n", "", txt)
    txt <- gsub("-\\s+", "", txt)
    
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
    
    # NUOVO: Rileva inizio reference
    is_reference_start <- grepl("^[A-Z][a-z]+(?:['-][A-Z][a-z]+)?,\\s+[A-Z]\\.", line_text, perl = TRUE)
    
    is_title <- (is_short && (is_caps || starts_with_number || starts_with_section))
    
    return(list(
      text = line_text,
      y = min(line$y),
      is_title = is_title,
      font_size = avg_font_size,
      starts_with_number = starts_with_number,
      is_reference_start = is_reference_start
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
        
        # NUOVO: Se è inizio reference, aggiungi \n\n
        if (current_line$is_reference_start) {
          result_parts <- c(result_parts, "\n\n", line_text)
        }
        else if (current_line$is_title) {
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

## NUOVA FUNZIONE: Normalizza References
normalize_references_section <- function(text_sections) {
  
  if (!"References" %in% names(text_sections)) {
    return(text_sections)
  }
  
  ref_text <- text_sections[["References"]]
  
  # Pattern per identificare l'inizio di ogni reference
  ref_start_pattern <- "([A-Z][a-z]+(?:['-][A-Z][a-z]+)?,\\s+[A-Z]\\.)"
  
  matches <- gregexpr(ref_start_pattern, ref_text, perl = TRUE)[[1]]
  
  if (matches[1] == -1) {
    return(text_sections)
  }
  
  start_positions <- as.integer(matches)
  
  individual_refs <- character()
  
  for (i in seq_along(start_positions)) {
    start_pos <- start_positions[i]
    
    if (i < length(start_positions)) {
      end_pos <- start_positions[i + 1] - 1
    } else {
      end_pos <- nchar(ref_text)
    }
    
    ref_block <- substr(ref_text, start_pos, end_pos)
    
    ref_block <- gsub("\\n+", " ", ref_block)
    ref_block <- gsub("\\s+", " ", ref_block)
    ref_block <- trimws(ref_block)
    
    individual_refs <- c(individual_refs, ref_block)
  }
  
  normalized_refs <- paste(individual_refs, collapse = "\n\n")
  
  text_sections[["References"]] <- normalized_refs
  
  message(sprintf("Normalized %d references with consistent \\n\\n separators", 
                  length(individual_refs)))
  
  return(text_sections)
}

## SEZIONI ----

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
    return(list("Full_text" = text))
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
  
  return(c("Full_text" = text, sections_list))
}

pdf2txt_auto <- function(file, 
                         n_columns = NULL, 
                         preserve_structure = TRUE, 
                         sections = TRUE,
                         normalize_refs = TRUE) {
  
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
    
    # NUOVO: Normalizza References
    if (normalize_refs && is.list(result)) {
      result <- normalize_references_section(result)
    }
  }
  
  return(result)
}