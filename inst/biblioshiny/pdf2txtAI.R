process_large_pdf <- function(pdf_path, api_key, pages_per_chunk = 4) {
  # Wrap entire function in tryCatch
  tryCatch(
    {
      # Validate inputs
      if (is.null(pdf_path) || !file.exists(pdf_path)) {
        warning("PDF file does not exist: ", pdf_path)
        return(NULL)
      }

      if (is.null(api_key) || api_key == "") {
        warning("API key is missing or empty")
        return(NULL)
      }

      if (!is.numeric(pages_per_chunk) || pages_per_chunk < 1) {
        warning("Invalid pages_per_chunk value. Must be a positive integer.")
        return(NULL)
      }

      prompt_first <- "Analyze this PDF document and extract all the text following these instructions:
1. Identify and remove repeated headers on each page
2. Organize the content into logical sections
3. Maintain the hierarchical structure (titles, subtitles)
4. Return the result in markdown format with:
   - # for main titles
   - ## for subtitles
   - Paragraphs separated by blank lines
5. IMPORTANT: Mantain the superscript numbers for references in the text reporting them as [1], [2], etc. 
6. IMPORTANT: This PDF is a chunk of a larger file. The first page might contain part of a section that started in a previous request. Include all sections in the output, even if they are incomplete or start mid-section. Do not skip content from sections that appear to have started earlier.
Do not include page numbers or repetitive header elements."

      prompt_continue <- "Analyze this PDF document and extract all the text following these instructions:
  1. Identify and remove repeated headers on each page
  2. Organize the content into logical sections
  3. Maintain the hierarchical structure (titles, subtitles)
  4. Return the result in markdown format with:
     - # for main titles
     - ## for subtitles
     - Paragraphs separated by blank lines
  5. IMPORTANT: Mantain the superscript numbers for references in the text reporting them as [1], [2], etc. 
  6. IMPORTANT: This PDF is a chunk of a larger file. The first page might contain part of a section that started in a previous request. Include all sections in the output, even if they are incomplete or start mid-section. Do not skip content from sections that appear to have started earlier.
  7. Remove titles including journal name (year), author names, etc. from the page if they appear to be part of a previous section.
  Do not include page numbers or repetitive header elements."

      # Get PDF info with error handling
      pdf_info_result <- tryCatch(
        {
          pdf_info(pdf_path)
        },
        error = function(e) {
          warning("Failed to read PDF info: ", e$message)
          return(NULL)
        }
      )

      if (is.null(pdf_info_result)) {
        return(NULL)
      }

      n_pages <- pdf_info_result$pages

      if (is.null(n_pages) || n_pages < 1) {
        warning("PDF has no pages or invalid page count")
        return(NULL)
      }

      # Divide PDF file in chunks
      chunks <- split(1:n_pages, ceiling(1:n_pages / pages_per_chunk))
      all_text <- list()

      # Process each chunk with error handling
      for (i in seq_along(chunks)) {
        message(sprintf("Processing chunk %d/%d", i, length(chunks)))

        # Extract temp chunk with error handling
        temp_pdf <- tempfile(fileext = ".pdf")

        subset_result <- tryCatch(
          {
            pdf_subset(pdf_path, pages = chunks[[i]], output = temp_pdf)
            TRUE
          },
          error = function(e) {
            warning(
              "Failed to create PDF subset for chunk ",
              i,
              ": ",
              e$message
            )
            return(FALSE)
          }
        )

        if (!subset_result || !file.exists(temp_pdf)) {
          warning("Failed to create temporary PDF for chunk ", i)
          # Clean up and return NULL
          if (file.exists(temp_pdf)) {
            unlink(temp_pdf)
          }
          return(NULL)
        }

        # Select prompt based on chunk position
        if (i > 1) {
          prompt <- prompt_continue
        } else {
          prompt <- prompt_first
        }

        # Process with Gemini AI with error handling
        text <- tryCatch(
          {
            gemini_ai(
              image = NULL,
              docs = temp_pdf,
              prompt = prompt,
              model = "2.0-flash",
              image_type = "png",
              retry_503 = 5,
              api_key = api_key,
              outputSize = "huge"
            )
          },
          error = function(e) {
            warning("Gemini AI failed for chunk ", i, ": ", e$message)
            return(NULL)
          }
        )

        # Check if text extraction was successful
        if (is.null(text)) {
          warning("Text extraction returned NULL for chunk ", i)
          unlink(temp_pdf)
          return(NULL)
        }

        # Check if the result is an error message (starts with ❌)
        if (is.character(text) && length(text) > 0 && grepl("^❌", text[1])) {
          warning("Gemini AI error for chunk ", i, ": ", text[1])
          unlink(temp_pdf)
          return(NULL)
        }

        # Store successful result
        all_text[[i]] <- text

        # Clean up temporary file
        unlink(temp_pdf)

        # Rate limiting
        Sys.sleep(1)
      }

      # Verify we have results for all chunks
      if (length(all_text) != length(chunks)) {
        warning("Not all chunks were processed successfully")
        return(NULL)
      }

      # Return the collected text
      return(all_text)
    },
    error = function(e) {
      # Catch-all error handler
      warning("Unexpected error in process_large_pdf: ", e$message)
      return(NULL)
    }
  )
}


#' Merge Text Chunks into Named Sections
#'
#' @description Takes a list of markdown text chunks and merges them into named sections.
#'   Each section name is extracted from the markdown header (# Title).
#'
#' @param text_chunks A list of character strings with markdown text from sequential PDF chunks
#' @param remove_tables Logical. If TRUE, removes all table content including captions. Default is FALSE.
#' @param remove_figure_captions Logical. If TRUE, removes figure captions. Default is FALSE.
#'
#' @return A named character vector where:
#'   - Names are section titles (without the # symbol)
#'   - Values are complete section contents (including the title line)
#'
#' @export
merge_text_chunks_named <- function(
  text_chunks,
  remove_tables = TRUE,
  remove_figure_captions = TRUE
) {
  # Combine all chunks into a single text
  full_text <- paste(text_chunks, collapse = "\n")

  # Remove markdown code block markers
  full_text <- remove_code_blocks(full_text)

  # Remove tables if requested
  if (remove_tables) {
    full_text <- remove_all_tables(full_text)
  }

  # Remove figure captions if requested
  if (remove_figure_captions) {
    full_text <- remove_figure_caps(full_text)
  }

  # Split into lines
  lines <- strsplit(full_text, "\n", fixed = TRUE)[[1]]

  # # Find section headers (lines starting with "# " followed by non-#)
  # section_starts <- grep("^# [^#]", lines)

  # Find section headers (lines starting with "#" or "##")
  section_starts <- grep("^##? [^#]", lines)

  if (length(section_starts) == 0) {
    warning("No section headers found. Returning full content.")
    return(c("Full Content" = trimws(full_text)))
  }

  # Initialize named vector
  sections <- character()

  # Extract each section
  for (i in seq_along(section_starts)) {
    start_idx <- section_starts[i]

    # Determine end index (before next section starts)
    if (i < length(section_starts)) {
      end_idx <- section_starts[i + 1] - 1
    } else {
      end_idx <- length(lines)
    }

    # Extract title (remove leading "# " and trim)
    title_line <- lines[start_idx]
    title <- gsub("^# ", "", title_line)
    title <- gsub("^## ", "", title)
    title <- trimws(title)

    # Extract full section content
    section_lines <- lines[start_idx:end_idx]
    section_text <- paste(section_lines, collapse = "\n")

    # Clean up excessive blank lines (more than 2 consecutive)
    section_text <- gsub("\n{3,}", "\n\n", section_text)

    # Trim leading/trailing whitespace
    section_text <- trimws(section_text)

    # Add to named vector
    sections[title] <- section_text
  }

  # Handle preamble (content before first section)
  if (section_starts[1] > 1) {
    preamble_lines <- lines[1:(section_starts[1] - 1)]
    preamble <- paste(preamble_lines, collapse = "\n")
    preamble <- trimws(preamble)

    # Only include preamble if it contains substantial content
    if (nchar(preamble) > 10) {
      sections <- c("Preamble" = preamble, sections)
    }
  }

  return(sections)
}


# Helper function to remove code block markers
remove_code_blocks <- function(text) {
  #' Remove Markdown Code Block Markers
  #'
  #' @param text Character string containing markdown text
  #' @return Character string with ```markdown and ``` markers removed

  # Remove ```markdown, ```r, ```python, etc. and closing ```
  text <- gsub("```[a-zA-Z]*\n?", "", text)
  text <- gsub("```", "", text)

  return(text)
}


# Helper function to remove all types of tables
remove_all_tables <- function(text) {
  #' Remove All Types of Tables (Markdown and Plain Text)
  #'
  #' @param text Character string containing text with tables
  #' @return Character string with all tables and table captions removed

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]

  # Identify different types of table indicators
  markdown_table_lines <- grepl("\\|", lines) # Markdown tables with |
  table_separator_lines <- grepl("^\\s*[-|]+\\s*$", lines) # Separator lines like ---|---
  table_caption_lines <- grepl(
    "^\\s*(\\*\\*)?Table\\s+\\d+",
    lines,
    ignore.case = TRUE
  )

  # Detect plain text tables by looking for aligned columns
  # Typically have patterns like "word | word" or "---|---" or multiple spaces for alignment
  potential_table_lines <- grepl("\\s{3,}", lines) | grepl("[-]{3,}", lines)

  lines_to_remove <- logical(length(lines))
  in_table <- FALSE
  empty_line_counter <- 0

  for (i in seq_along(lines)) {
    current_line <- trimws(lines[i])

    # Check if this is a table caption
    if (table_caption_lines[i]) {
      in_table <- TRUE
      lines_to_remove[i] <- TRUE
      empty_line_counter <- 0
      next
    }

    # Check if we're in a markdown table
    if (markdown_table_lines[i] || table_separator_lines[i]) {
      in_table <- TRUE
      lines_to_remove[i] <- TRUE
      empty_line_counter <- 0
      next
    }

    # If we're in a table
    if (in_table) {
      # Empty line might signal end of table, but wait for confirmation
      if (nchar(current_line) == 0) {
        lines_to_remove[i] <- TRUE
        empty_line_counter <- empty_line_counter + 1

        # After 2 empty lines, table is definitely over
        if (empty_line_counter >= 2) {
          in_table <- FALSE
          empty_line_counter <- 0
        }
      } else if (potential_table_lines[i] || markdown_table_lines[i]) {
        # Check if line looks like table content (aligned data)
        lines_to_remove[i] <- TRUE
        empty_line_counter <- 0
      } else if (!grepl("^\\s*$", current_line)) {
        # Regular text line after table content - table has ended
        in_table <- FALSE
        empty_line_counter <- 0
      }
    }
  }

  # Second pass: remove isolated table-like structures
  # (tables without explicit captions)
  for (i in seq_along(lines)) {
    if (lines_to_remove[i]) {
      next
    }

    # If a line has the markdown table separator pattern
    if (
      grepl("^\\s*[-:|]+\\s*$", lines[i]) ||
        grepl("^[\\s]*[|][-:|\\s]+[|][\\s]*$", lines[i])
    ) {
      # Mark previous and next lines if they look like table content
      if (i > 1 && !lines_to_remove[i - 1] && grepl("\\|", lines[i - 1])) {
        lines_to_remove[i - 1] <- TRUE
      }
      lines_to_remove[i] <- TRUE
      if (
        i < length(lines) &&
          !lines_to_remove[i + 1] &&
          grepl("\\|", lines[i + 1])
      ) {
        lines_to_remove[i + 1] <- TRUE
      }
    }
  }

  # Remove identified lines
  cleaned_lines <- lines[!lines_to_remove]

  # Reconstruct text
  cleaned_text <- paste(cleaned_lines, collapse = "\n")

  return(cleaned_text)
}


# Helper function to remove figure captions
remove_figure_caps <- function(text) {
  #' Remove Figure Captions
  #'
  #' @param text Character string containing markdown text
  #' @return Character string with figure captions removed

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]

  lines_to_remove <- logical(length(lines))
  in_caption <- FALSE

  for (i in seq_along(lines)) {
    line <- trimws(lines[i])

    # Check if line starts a figure caption
    # Patterns: "Figure N:", "Fig. N:", "Figure N.", "Fig N."
    if (
      grepl(
        "^\\s*(\\*\\*)?(Fig\\.|Figure)\\s+\\d+[:.\\s]",
        line,
        ignore.case = TRUE
      )
    ) {
      lines_to_remove[i] <- TRUE
      in_caption <- TRUE

      # Check if caption ends on the same line
      if (grepl("[.!]\\s*$", line) || nchar(line) < 100) {
        in_caption <- FALSE
      }
    } else if (in_caption) {
      # Continue removing lines if we're in a multi-line caption
      lines_to_remove[i] <- TRUE

      # Caption ends at sentence end or empty line
      if (grepl("[.!]\\s*$", line) || nchar(line) == 0) {
        in_caption <- FALSE
      }
    }
  }

  # Remove identified lines
  cleaned_lines <- lines[!lines_to_remove]

  # Reconstruct text
  cleaned_text <- paste(cleaned_lines, collapse = "\n")

  return(cleaned_text)
}

text_AI_conversion <- function(x, citation_type = "author_year") {
  convert_superscript_citations <- function(text) {
    # Pattern 1: Single number after punctuation at end of sentence
    # Example: "...as shown before. 15 The next..." -> "...as shown before.[15] The next..."
    text <- gsub(
      "([.!?])\\s+(\\d{1,3})\\s+([A-Z])",
      "\\1[\\2] \\3",
      text,
      perl = TRUE
    )

    # Pattern 2: Single or multiple numbers after a word (no space or minimal space)
    # Example: "study 5" or "study5" -> "study[5]"
    # Handles: single numbers, comma-separated, ranges with dash
    text <- gsub(
      "([a-z])\\s+(\\d{1,3}(?:[,\\-]\\d{1,3})*)(?=\\s|[.,;!?]|$)",
      "\\1[\\2]",
      text,
      perl = TRUE
    )

    # Pattern 3: Multiple citation numbers with various separators
    # Example: "evidence 1, 2, 3" -> "evidence[1,2,3]"
    text <- gsub(
      "([a-z])\\s+(\\d{1,3}(?:\\s*[,;]\\s*\\d{1,3})+)(?=\\s|[.,;!?]|$)",
      "\\1[\\2]",
      text,
      perl = TRUE
    )

    # Pattern 4: Range citations with "e" (common OCR artifact for dash)
    # Example: "studies 3e5" -> "studies[3-5]"
    text <- gsub(
      "([a-z])\\s+(\\d{1,3}e\\d{1,3})(?=\\s|[.,;!?]|$)",
      "\\1[\\2]",
      text,
      perl = TRUE
    )

    # Pattern 5: Numbers immediately after closing parenthesis or quote
    # Example: "(Smith et al.) 15" -> "(Smith et al.)[15]"
    text <- gsub(
      "([)])\\s+(\\d{1,3}(?:[,\\-]\\d{1,3})*)(?=\\s|[.,;!?]|$)",
      "\\1[\\2]",
      text,
      perl = TRUE
    )

    # Clean up: remove extra spaces inside brackets
    # Example: "[1, 2, 3]" -> "[1,2,3]"
    text <- gsub("\\[\\s+", "[", text, perl = TRUE)
    text <- gsub("\\s+\\]", "]", text, perl = TRUE)
    text <- gsub("\\s*,\\s*", ",", text, perl = TRUE)

    # Clean up: convert "e" to "-" inside brackets (OCR artifact)
    text <- gsub("\\[(\\d+)e(\\d+)\\]", "[\\1-\\2]", text, perl = TRUE)

    # Pattern 6: Handle cases where number appears at end of incomplete word
    # This catches cases where PDF extraction merged citation with word
    # Example: "studies15" -> "studies[15]"
    text <- gsub(
      "([a-z]{3,})(\\d{1,3})(?=\\s|[.,;!?]|$)",
      "\\1[\\2]",
      text,
      perl = TRUE
    )

    return(text)
  }
  ind <- names(x)

  for (i in ind) {
    txt <- x[[i]]

    tryCatch({
      txt <- gsub(
        "([0-9]+(?:\\.[0-9]+)*\\.\\s+[A-Za-z][A-Za-z\\s]{3,50})",
        "\n\n\\1\n\n",
        txt,
        perl = TRUE
      )
      txt <- gsub("\\n\\s*\\n", "\n\n", txt)
      txt <- gsub(
        "([.!?])\\s*\n\\s*([A-Z][a-z])",
        "\\1\n\n\\2",
        txt,
        perl = TRUE
      )
      txt <- gsub("(?<![.!?\\n])\\n(?![A-Z0-9\\n])", " ", txt, perl = TRUE)
      txt <- paste(txt, collapse = "\n\n")

      txt <- gsub("-\\s", "", txt)

      # Convert superscript citations ONLY if citation_type is "numeric_superscript"
      if (citation_type == "numeric_superscript") {
        txt <- convert_superscript_citations(txt)
      }
      x[[i]] <- txt
    })
  }

  return(x)
}
