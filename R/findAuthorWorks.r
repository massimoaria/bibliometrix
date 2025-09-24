#' Find Author's Co-authored Works
#'
#' Searches for an author's name in a bibliometric dataframe and returns 
#' the DOIs and author positions of their co-authored works.
#'
#' @param author_name Character. The author's name to search for (case-insensitive)
#' @param data Data.frame. The bibliometric dataframe with AU and DI columns
#' @param partial_match Logical. If TRUE, allows partial name matching (default: TRUE)
#' @param exact_match Logical. If TRUE, requires exact name matching (default: FALSE)
#'
#' @return A data.frame with columns:
#'   \itemize{
#'     \item doi: DOI of the work
#'     \item author_position: Numerical position of the author in the author list
#'     \item total_authors: Total number of authors in the work
#'     \item all_authors: Complete list of authors for reference
#'     \item matched_name: The exact name variant that was matched
#'   }
#'
#' @details 
#' The function searches through the AU column which contains author names 
#' separated by semicolons. It identifies the position of the target author
#' and returns comprehensive information about each matching work.
#'
#' @examples
#' \dontrun{
#' # Find works by "ARIA M"
#' works <- findAuthorWorks("ARIA M", M)
#' 
#' # Find works with exact matching
#' works_exact <- findAuthorWorks("PESTANA MH", M, exact_match = TRUE)
#' 
#' # Find works with partial matching disabled
#' works_full <- findAuthorWorks("MASSIMO ARIA", M, partial_match = FALSE)
#' }
#'
#' @author Your Name
#' @export
findAuthorWorks <- function(author_name, 
                              data, 
                              partial_match = TRUE, 
                              exact_match = FALSE) {
  
  # Input validation
  if (missing(author_name) || !is.character(author_name) || nchar(trimws(author_name)) == 0) {
    stop("The 'author_name' parameter must be a non-empty character string")
  }
  
  if (missing(data) || !is.data.frame(data)) {
    stop("The 'data' parameter must be a data.frame")
  }
  
  if (!all(c("AU", "DI") %in% names(data))) {
    stop("The data.frame must contain 'AU' and 'DI' columns")
  }
  
  # Clean the input author name
  author_name <- trimws(author_name)
  
  # Initialize result dataframe
  result <- data.frame(
    doi = character(0),
    author_position = integer(0),
    total_authors = integer(0),
    all_authors = character(0),
    matched_name = character(0),
    stringsAsFactors = FALSE
  )
  
  # Process each row
  for (i in 1:nrow(data)) {
    au_string <- data$AU[i]
    doi <- data$DI[i]
    
    # Skip rows with missing AU or DI
    if (is.na(au_string) || is.na(doi) || au_string == "" || doi == "") {
      next
    }
    
    # Split authors by semicolon and clean
    authors <- trimws(unlist(strsplit(au_string, ";")))
    authors <- authors[authors != ""]  # Remove empty strings
    
    if (length(authors) == 0) {
      next
    }
    
    # Find matching positions
    matched_positions <- c()
    matched_names <- c()
    
    if (exact_match) {
      # Exact matching (case-insensitive)
      matches <- which(toupper(authors) == toupper(author_name))
      if (length(matches) > 0) {
        matched_positions <- matches
        matched_names <- authors[matches]
      }
    } else if (partial_match) {
      # Partial matching (case-insensitive)
      matches <- which(grepl(toupper(author_name), toupper(authors), fixed = TRUE))
      if (length(matches) > 0) {
        matched_positions <- matches
        matched_names <- authors[matches]
      }
    } else {
      # Full name matching (case-insensitive, but complete match)
      matches <- which(toupper(authors) == toupper(author_name))
      if (length(matches) > 0) {
        matched_positions <- matches
        matched_names <- authors[matches]
      }
    }
    
    # Add matches to result
    if (length(matched_positions) > 0) {
      for (j in seq_along(matched_positions)) {
        new_row <- data.frame(
          doi = doi,
          author_position = matched_positions[j],
          total_authors = length(authors),
          all_authors = paste(authors, collapse = "; "),
          matched_name = matched_names[j],
          stringsAsFactors = FALSE
        )
        result <- rbind(result, new_row)
      }
    }
  }
  
  # Add summary information as attributes
  if (nrow(result) > 0) {
    attr(result, "search_summary") <- list(
      searched_author = author_name,
      total_works_found = nrow(result),
      unique_dois = length(unique(result$doi)),
      search_type = ifelse(exact_match, "exact", 
                           ifelse(partial_match, "partial", "full_name")),
      unique_matched_names = unique(result$matched_name)
    )
    
    # Sort by DOI for consistency
    result <- result[order(result$doi, result$author_position), ]
    rownames(result) <- NULL
  }
  
  return(result)
}

#' Print Summary of Author Works Search
#'
#' Prints a summary of the search results from find_author_works()
#'
#' @param works_df Data.frame. Result from find_author_works()
#' @export
print_author_works_summary <- function(works_df) {
  summary_info <- attr(works_df, "search_summary")
  
  if (is.null(summary_info)) {
    cat("No search summary available. Use find_author_works() to generate results.\n")
    return()
  }
  
  cat("=== Author Works Search Summary ===\n")
  cat("Searched for:", summary_info$searched_author, "\n")
  cat("Search type:", summary_info$search_type, "\n")
  cat("Total works found:", summary_info$total_works_found, "\n")
  cat("Unique DOIs:", summary_info$unique_dois, "\n")
  cat("Matched name variants:\n")
  for (name in summary_info$unique_matched_names) {
    cat("  -", name, "\n")
  }
  cat("\n")
  
  if (nrow(works_df) > 0) {
    cat("Position distribution:\n")
    pos_table <- table(works_df$author_position)
    for (pos in names(pos_table)) {
      cat("  Position", pos, ":", pos_table[pos], "works\n")
    }
  }
}

