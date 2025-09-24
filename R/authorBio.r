#' Retrieve Author Biographical Information from OpenAlex
#'
#' This function downloads comprehensive author information from OpenAlex based on a DOI 
#' and the numerical position of the author in the co-authors list. It provides detailed 
#' biographical data, bibliometric indicators, and affiliation information.
#'
#' @param author_position Integer. The numerical position of the author in the authors list (default: 1)
#' @param doi Character. DOI of the article used to identify the authors
#' @param verbose Logical. Print informative messages during execution (default: FALSE)
#' @param return_all_authors Logical. If TRUE, returns information for all co-authors (default: FALSE)
#'
#' @return If \code{return_all_authors = FALSE}, returns a tibble with comprehensive information 
#'   about the specified author including:
#'   \itemize{
#'     \item Basic information (name, ORCID, OpenAlex ID)
#'     \item Bibliometric indicators (works count, citations, h-index, i10-index)
#'     \item Affiliation details from both the paper and author profile
#'     \item Research topics and areas
#'     \item Paper-specific metadata (corresponding author status, position type)
#'   }
#'   If \code{return_all_authors = TRUE}, returns a list of tibbles, one for each co-author.
#'
#' @details 
#' The function first retrieves the work information using the provided DOI, then extracts
#' author IDs from the authorships data, and finally fetches detailed author profiles from
#' OpenAlex. It enriches the author data with paper-specific information such as authorship
#' position, corresponding author status, and affiliations as listed in the paper.
#' 
#' The function handles various edge cases including missing author IDs, invalid positions,
#' and network errors. It also provides comprehensive error messages to help troubleshoot
#' common issues.
#'
#' @examples
#' \dontrun{
#' # Get information for the first author
#' first_author <- authorBio(doi = "10.1016/j.joi.2017.08.007")
#' 
#' # Get information for the second author with verbose output
#' second_author <- authorBio(
#'   author_position = 2, 
#'   doi = "10.1016/j.joi.2017.08.007", 
#'   verbose = TRUE
#' )
#' 
#' # Get information for all co-authors
#' all_authors <- authorBio(
#'   doi = "10.1016/j.joi.2017.08.007", 
#'   return_all_authors = TRUE
#' )
#' }
#'
#' @export
#'
authorBio <- function(author_position = 1, 
                       doi = "10.1016/j.joi.2017.08.007", 
                       verbose = FALSE,
                       return_all_authors = FALSE) {
  
  # Input validation
  if (is.null(doi) || !is.character(doi) || nchar(trimws(doi)) == 0) {
    stop("The 'doi' parameter must be a non-empty character string")
  }
  
  if (!is.numeric(author_position) || author_position < 1 || author_position != as.integer(author_position)) {
    stop("The 'author_position' parameter must be a positive integer")
  }
  
  # Check library availability
  if (!requireNamespace("openalexR", quietly = TRUE)) {
    stop("The 'openalexR' library is not available. Install it with: install.packages('openalexR')")
  }
  
  if (verbose) cat("Retrieving article information for DOI:", doi, "\n")
  
  # Retrieve article information with error handling
  au_work <- tryCatch({
    openalexR::oa_fetch(
      entity = "works",
      doi = doi,
      output = "tibble"
    )
  }, error = function(e) {
    stop("Error retrieving article: ", e$message, 
         "\nPlease verify that the DOI is correct and OpenAlex is accessible")
  })
  
  # Verify that the article was found
  if (is.null(au_work) || nrow(au_work) == 0) {
    stop("No article found for the provided DOI: ", doi)
  }
  
  # Extract author information from the correct structure
  authorships <- au_work$authorships[[1]]
  
  if (is.null(authorships) || nrow(authorships) == 0) {
    stop("No author information found for this article")
  }
  
  # Verify that the requested position exists
  if (author_position > nrow(authorships)) {
    stop("Author position (", author_position, 
         ") is greater than the total number of authors (", nrow(authorships), ")")
  }
  
  if (verbose) {
    cat("Article found:", au_work$display_name[1], "\n")
    cat("Total number of authors:", nrow(authorships), "\n")
    if (nrow(authorships) > 0) {
      cat("Authors:\n")
      for (i in 1:nrow(authorships)) {
        cat("  ", i, ".", authorships$display_name[i], "\n")
      }
    }
  }
  
  # If requested, return all authors
  if (return_all_authors) {
    if (verbose) cat("Retrieving information for all authors...\n")
    
    all_authors <- list()
    for (i in 1:nrow(authorships)) {
      if (verbose) cat("Processing author", i, "of", nrow(authorships), ":", authorships$display_name[i], "\n")
      
      author_id <- authorships$id[i]
      if (!is.na(author_id) && author_id != "") {
        # Extract only the OpenAlex ID from the full URL
        clean_id <- gsub("https://openalex.org/", "", author_id)
        
        author_info <- tryCatch({
          openalexR::oa_fetch(
            entity = "authors",
            identifier = clean_id,
            output = "tibble"
          )
        }, error = function(e) {
          if (verbose) cat("Error for author", i, ":", e$message, "\n")
          NULL
        })
        
        if (!is.null(author_info) && nrow(author_info) > 0) {
          # Add additional information from the authorships structure
          author_info$author_position_in_paper <- i
          author_info$original_author_name <- authorships$display_name[i]
          author_info$is_corresponding <- authorships$is_corresponding[i]
          author_info$author_position_type <- authorships$author_position[i]
          
          # Add affiliation information if available
          if (!is.null(authorships$affiliations[[i]]) && nrow(authorships$affiliations[[i]]) > 0) {
            author_info$primary_affiliation <- authorships$affiliations[[i]]$display_name[1]
            author_info$primary_affiliation_country <- authorships$affiliations[[i]]$country_code[1]
          } else {
            author_info$primary_affiliation <- NA
            author_info$primary_affiliation_country <- NA
          }
          
          # Add raw affiliation if available
          if (!is.null(authorships$affiliation_raw) && length(authorships$affiliation_raw) >= i) {
            author_info$affiliation_raw <- authorships$affiliation_raw[i]
          } else {
            author_info$affiliation_raw <- NA
          }
          
          all_authors[[i]] <- author_info
        }
      } else {
        if (verbose) cat("Invalid author ID for position", i, "\n")
      }
    }
    
    # Combine all valid results
    valid_authors <- all_authors[!sapply(all_authors, is.null)]
    if (length(valid_authors) > 0) {
      # Add common metadata to all
      for (i in seq_along(valid_authors)) {
        valid_authors[[i]]$source_doi <- doi
        valid_authors[[i]]$source_title <- au_work$display_name[1]
        valid_authors[[i]]$query_timestamp <- Sys.time()
      }
      return(valid_authors)
    } else {
      stop("Unable to retrieve information for any author")
    }
  }
  
  # Retrieve information for the specific author
  author_id <- authorships$id[author_position]
  
  if (is.na(author_id) || author_id == "") {
    stop("Invalid author ID at position ", author_position)
  }
  
  # Extract only the OpenAlex ID from the full URL
  clean_id <- gsub("https://openalex.org/", "", author_id)
  
  if (verbose) {
    cat("Retrieving information for author at position", author_position, "\n")
    cat("Author name:", authorships$display_name[author_position], "\n")
    cat("OpenAlex ID:", clean_id, "\n")
    cat("Position type:", authorships$author_position[author_position], "\n")
    cat("Is corresponding author:", authorships$is_corresponding[author_position], "\n")
  }
  
  # Retrieve author biographical data
  au_info <- tryCatch({
    openalexR::oa_fetch(
      entity = "authors",
      identifier = clean_id,
      output = "tibble"
    )
  }, error = function(e) {
    stop("Error retrieving author information: ", e$message)
  })
  
  if (is.null(au_info) || nrow(au_info) == 0) {
    stop("No biographical information found for the author at position ", author_position)
  }
  
  # Add useful metadata from the authorships structure
  au_info$author_position_in_paper <- author_position
  au_info$original_author_name <- authorships$display_name[author_position]
  au_info$is_corresponding <- authorships$is_corresponding[author_position]
  au_info$author_position_type <- authorships$author_position[author_position]
  
  # Add affiliation information if available
  if (!is.null(authorships$affiliations[[author_position]]) && 
      nrow(authorships$affiliations[[author_position]]) > 0) {
    au_info$primary_affiliation <- authorships$affiliations[[author_position]]$display_name[1]
    au_info$primary_affiliation_country <- authorships$affiliations[[author_position]]$country_code[1]
    au_info$primary_affiliation_ror <- authorships$affiliations[[author_position]]$ror[1]
  } else {
    au_info$primary_affiliation <- NA
    au_info$primary_affiliation_country <- NA
    au_info$primary_affiliation_ror <- NA
  }
  
  # Add raw affiliation if available
  if (!is.null(authorships$affiliation_raw) && length(authorships$affiliation_raw) >= author_position) {
    au_info$affiliation_raw <- authorships$affiliation_raw[author_position]
  } else {
    au_info$affiliation_raw <- NA
  }
  
  # Add query metadata
  au_info$source_doi <- doi
  au_info$source_title <- au_work$display_name[1]
  au_info$query_timestamp <- Sys.time()
  
  if (verbose) {
    cat("Information successfully retrieved for:", au_info$display_name[1], "\n")
    cat("Number of publications:", au_info$works_count[1], "\n")
    cat("Number of citations:", au_info$cited_by_count[1], "\n")
    cat("H-index:", au_info$h_index[1], "\n")
    cat("Primary affiliation:", au_info$primary_affiliation[1], "\n")
  }
  
  return(au_info)
}

# Helper function to analyze all authors of an article
analyze_all_authors <- function(doi, verbose = FALSE) {
  return(author_bio(doi = doi, return_all_authors = TRUE, verbose = verbose))
}

#' Get Authors Summary from OpenAlex
#'
#' Retrieves a quick summary of all authors from a paper without making additional API calls
#' for individual author profiles. Useful for getting an overview of the authorship structure.
#'
#' @param doi Character. DOI of the article
#' @param verbose Logical. Print informative messages during execution (default: FALSE)
#'
#' @return A data frame with summary information for all authors including:
#'   \itemize{
#'     \item position: Author position in the paper
#'     \item display_name: Author name as it appears in the paper
#'     \item author_position_type: Type of position (first, last, middle)
#'     \item is_corresponding: Whether the author is a corresponding author
#'     \item orcid: ORCID identifier if available
#'     \item openalex_id: OpenAlex author identifier
#'     \item primary_affiliation: Main institutional affiliation
#'   }
#'
#' @examples
#' \dontrun{
#' # Get a quick summary of all authors
#' summary <- get_authors_summary(doi = "10.1016/j.joi.2017.08.007")
#' print(summary)
#' }
#'
#' @export
get_authors_summary <- function(doi="10.1016/j.joi.2017.08.007", verbose = FALSE) {
  if (verbose) cat("Retrieving author summary for DOI:", doi, "\n")
  
  au_work <- tryCatch({
    openalexR::oa_fetch(entity = "works", doi = doi, output = "tibble")
  }, error = function(e) {
    stop("Error retrieving article: ", e$message)
  })
  
  
  if (is.null(au_work) || nrow(au_work) == 0) {
    stop("No article found for the provided DOI: ", doi)
  }
  
  authorships <- au_work$authorships[[1]]
  
  # Create a summary without additional API calls
  summary_df <- data.frame(
    position = 1:nrow(authorships),
    display_name = authorships$display_name,
    author_position_type = authorships$author_position,
    is_corresponding = authorships$is_corresponding,
    orcid = authorships$orcid,
    openalex_id = authorships$id,
    stringsAsFactors = FALSE
  )
  
  # Add affiliations if available
  summary_df$primary_affiliation <- sapply(1:nrow(authorships), function(i) {
    if (!is.null(authorships$affiliations[[i]]) && nrow(authorships$affiliations[[i]]) > 0) {
      return(authorships$affiliations[[i]]$display_name[1])
    } else {
      return(NA)
    }
  })
  
  return(summary_df)
}

