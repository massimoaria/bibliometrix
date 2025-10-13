utils::globalVariables(c("CR_canonical", "CR_id", "CR_normalized", "CR_original", 
                         "blocking_key", "cluster_id", "completeness_score", 
                         "first_author", "journal", "merge_key", "n_cluster", 
                         "pages", "temp_cluster", "volume", "doi", "n_exact"))

#' Normalize and match bibliographic citations
#'
#' This function performs advanced normalization and fuzzy matching of bibliographic 
#' citations to identify and group citations that refer to the same work but are 
#' formatted differently. It uses a multi-phase approach combining string normalization, 
#' blocking strategies, hierarchical clustering, and post-processing to achieve both 
#' speed and accuracy on large citation datasets.
#'
#' @param CR_vector Character vector containing bibliographic citations to be normalized and matched.
#' @param threshold Numeric value between 0 and 1 indicating the similarity threshold 
#'   for matching citations. Higher values (e.g., 0.90-0.95) produce more conservative 
#'   matching, while lower values (e.g., 0.75-0.80) produce more aggressive matching. 
#'   Default is 0.85, which provides a good balance between precision and recall.
#' @param method String distance method to use for fuzzy matching. Options include:
#'   \itemize{
#'     \item "jw" (default): Jaro-Winkler distance, optimized for bibliographic strings
#'     \item "lv": Levenshtein distance
#'     \item Other methods supported by \code{\link[stringdist]{stringdistmatrix}}
#'   }
#'
#' @details
#' The function implements a five-phase matching algorithm:
#' 
#' \strong{Phase 1: Normalization and Feature Extraction}
#' \itemize{
#'   \item Converts text to uppercase
#'   \item Removes issue numbers and page numbers (which often contain typos)
#'   \item Removes punctuation and normalizes whitespace
#'   \item Expands common journal abbreviations (e.g., "J. CLEAN. PROD." -> "JOURNAL OF CLEANER PRODUCTION")
#'   \item Extracts key features: first author, year, journal, volume, pages
#' }
#' 
#' \strong{Phase 2: Blocking}
#' Citations are grouped into blocks by first author and year. This dramatically 
#' reduces computational complexity from O(n^2) to approximately O(k*m^2), where k is 
#' the number of blocks and m is the average block size.
#' 
#' \strong{Phase 3: Within-Block Matching}
#' Within each block, citations are compared using string distance metrics and 
#' hierarchical clustering. For blocks larger than 500 citations, exact matching 
#' on normalized strings is used instead to maintain performance.
#' 
#' \strong{Phase 4: Canonical Representative Selection}
#' For each cluster, the most complete citation (prioritizing those with volume 
#' and page information) is selected as the canonical representative.
#' 
#' \strong{Phase 5: Post-Processing}
#' Citations sharing the same first author, year, journal, and volume are merged 
#' into a single cluster, even if they weren't matched in Phase 3. This catches 
#' cases where minor title variations prevented matching.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{CR_original}: Original citation string
#'   \item \code{CR_canonical}: Canonical (representative) citation for the cluster
#'   \item \code{cluster_id}: Unique identifier for each citation cluster
#'   \item \code{n_cluster}: Number of citations in the cluster
#'   \item \code{first_author}: First author surname
#'   \item \code{year}: Publication year
#'   \item \code{journal}: Normalized journal name
#'   \item \code{volume}: Volume number
#'   \item \code{blocking_key}: Internal key used for blocking (author_year)
#' }
#'
#' @examples
#' \dontrun{
#' # Load bibliometrix data
#' data(scientometrics, package = "bibliometrixData")
#' 
#' # Extract and normalize citations
#' CR_vector <- unlist(strsplit(scientometrics$CR, ";"))
#' CR_vector <- trimws(CR_vector)
#' 
#' # Perform normalization with default threshold
#' matched <- normalize_citations(CR_vector)
#' 
#' # View matching statistics
#' table(matched$n_cluster)
#' 
#' # Find all variants of a specific citation
#' subset(matched, cluster_id == matched$cluster_id[1])
#' 
#' # Use more conservative matching
#' matched_conservative <- normalize_citations(CR_vector, threshold = 0.90)
#' }
#'
#' @seealso 
#' \code{\link{applyCitationMatching}} for direct application to bibliometrix data frames
#'
#' @references
#' Aria, M. & Cuccurullo, C. (2017). bibliometrix: An R-tool for comprehensive 
#' science mapping analysis. Journal of Informetrics, 11(4), 959-975.
#'
#' @export

normalize_citations <- function(CR_vector, threshold = 0.90, method = "jw", min_chars = 20) {
  
  # Detect citation format
  detect_format <- function(x) {
    # Scopus format: AUTHOR, TITLE (YEAR) JOURNAL, V, PP
    # WoS format: AUTHOR, YEAR, JOURNAL, V, P, DOI
    if (str_detect(x, "\\(\\d{4}\\)\\s+[A-Z]")) {
      return("scopus")
    } else if (str_detect(x, ",\\s*\\d{4},\\s*[A-Z]")) {
      return("wos")
    } else {
      return("unknown")
    }
  }
  
  # Base normalization function
  normalize_string <- function(x) {
    x %>%
      toupper() %>%
      str_replace_all("\\s*:\\s*", " ") %>%           # Remove colons with surrounding spaces
      str_replace_all("\\s*;\\s*", " ") %>%           # Remove semicolons with surrounding spaces
      str_replace_all("\\s*-\\s*", " ") %>%           # Normalize hyphens
      str_replace_all("\\(\\d+\\)", "") %>%           # Remove issue numbers
      str_replace_all("PP\\.?\\s*\\d+\\s*[-\u2013\u2014]\\s*\\d+", "") %>%  # Remove page numbers
      str_replace_all("P\\.?\\s*\\d+\\s*[-\u2013\u2014]\\s*\\d+", "") %>%
      # Normalize common journal abbreviations
      str_replace_all("J\\.?\\s+CLEAN\\.?\\s+PROD\\.?", "JOURNAL OF CLEANER PRODUCTION") %>%
      str_replace_all("JOURNAL OF CLEANER PRODUCTION", "J CLEAN PROD") %>%
      str_replace_all("LONG\\s+RANGE\\s+PLAN(NING)?", "LONG RANGE PLANNING") %>%
      str_replace_all("ORGAN\\.?\\s+ENVIRON\\.?", "ORGANIZATION AND ENVIRONMENT") %>%
      str_replace_all("J\\.?\\s+MANAG\\.?", "JOURNAL OF MANAGEMENT") %>%
      str_replace_all("[[:punct:]]", " ") %>%         # Remove punctuation
      str_replace_all("\\s+", " ") %>%                # Normalize whitespace
      str_trim()
  }
  
  # Extract DOI (for WoS format)
  extract_doi <- function(x) {
    doi_pattern <- "DOI\\s*:?\\s*(10\\.\\d{4,}/[^,\\s]+)"
    doi <- str_extract(x, regex(doi_pattern, ignore_case = TRUE))
    if (!is.na(doi)) {
      doi <- str_remove(doi, regex("^DOI\\s*:?\\s*", ignore_case = TRUE))
      doi <- str_remove(doi, "[,;\\s]+$")
    }
    return(doi)
  }
  
  # Extract key features - unified for both formats
  extract_key_features <- function(x) {
    format <- detect_format(x)
    
    # First author (always before first comma)
    first_author <- str_extract(x, "^[A-Z][A-Z\\s-\\.]+(?=,)") %>%
      str_remove_all("\\.$") %>%
      str_trim()
    
    # Year
    year <- str_extract(x, "\\(?\\d{4}\\)?") %>% 
      str_remove_all("[()]")
    
    # DOI (only in WoS)
    doi <- extract_doi(x)
    
    if (format == "scopus") {
      # Scopus: AUTHOR, TITLE (YEAR) JOURNAL, VOLUME, PAGES
      # Extract title (between first comma and year)
      title <- str_extract(x, "(?<=,\\s)(.+?)(?=\\s*\\(\\d{4}\\))")
      
      # Extract journal (after year, before volume or pages)
      journal <- str_extract(x, "(?<=\\d{4}\\)\\s)([A-Z][A-Z\\s&\\.-]+?)(?=,\\s*\\d+|,\\s*PP)")
      
      # Volume - more flexible approach without variable-length lookbehind
      # Find text after year, split by comma, get the second numeric part
      after_year <- str_extract(x, "\\d{4}\\)(.+)$")
      if (!is.na(after_year)) {
        # Split by comma and find first standalone number
        parts <- str_split(after_year, ",")[[1]]
        if (length(parts) >= 2) {
          volume <- str_extract(parts[2], "^\\s*(\\d+)") %>%
            str_extract("\\d+")
        } else {
          volume <- NA
        }
      } else {
        volume <- NA
      }
      
      # Pages
      pages <- str_extract(x, "PP\\.?\\s*\\d+[-\u2013\u2014]\\d+")
      
      # First 3 significant words from title
      if (!is.na(title)) {
        title_words <- str_extract_all(title, "\\b[A-Z]{3,}\\b")[[1]] %>%
          head(3) %>%
          paste(collapse = " ")
      } else {
        title_words <- NA
      }
      
    } else {
      # WoS: AUTHOR, YEAR, JOURNAL, V, P, DOI
      journal <- str_extract(x, "(?<=\\d{4},\\s)([A-Z][A-Z\\s&-]+?)(?=,\\s*V\\d|,\\s*P\\d|,\\s*DOI)")
      
      volume <- str_extract(x, "(?:,\\s*|\\s+)V(\\d+)") %>%
        str_extract("\\d+")
      
      pages <- str_extract(x, "P\\d+|PP?\\.?\\s*\\d+[-\u2013\u2014]?\\d*")
      
      # For WoS, extract title-like text after author/year
      temp <- str_remove(x, "^[^,]+,\\s*\\d{4},?\\s*")
      title_words <- str_extract_all(temp, "\\b[A-Z]{4,}\\b")[[1]] %>%
        head(3) %>%
        paste(collapse = " ")
    }
    
    list(
      format = format,
      first_author = first_author,
      year = year,
      doi = doi,
      journal = journal,
      volume = volume,
      pages = pages,
      title_words = title_words,
      full_normalized = normalize_string(x)
    )
  }
  
  cat("Phase 1: Cleaning and feature extraction...\n")
  
  # Remove invalid citations
  df <- tibble(
    CR_original = CR_vector,
    CR_id = seq_along(CR_vector)
  ) %>%
    dplyr::filter(
      !is.na(CR_original), 
      CR_original != "",
      nchar(CR_original) >= min_chars,
      !grepl("^NO TITLE CAPTURED$", CR_original, ignore.case = TRUE),
      !grepl("^ANONYMOUS", CR_original, ignore.case = TRUE),
      !grepl("^\\d+-[A-Z#]+$", CR_original),
      !grepl("^[A-Z\\s]{1,10}$", CR_original)  # Remove very short strings
    )
  
  cat("  Filtered out", length(CR_vector) - nrow(df), "invalid citations\n")
  
  # Extract features
  features <- lapply(df$CR_original, extract_key_features)
  
  df <- df %>%
    mutate(
      format = sapply(features, function(x) x$format %||% NA),
      first_author = sapply(features, function(x) x$first_author %||% NA),
      year = sapply(features, function(x) x$year %||% NA),
      doi = sapply(features, function(x) x$doi %||% NA),
      journal = sapply(features, function(x) x$journal %||% NA),
      volume = sapply(features, function(x) x$volume %||% NA),
      pages = sapply(features, function(x) x$pages %||% NA),
      title_words = sapply(features, function(x) x$title_words %||% NA),
      CR_normalized = sapply(features, function(x) x$full_normalized)
    )
  
  cat("  Detected formats: WoS =", sum(df$format == "wos", na.rm = TRUE), 
      ", Scopus =", sum(df$format == "scopus", na.rm = TRUE),
      ", Unknown =", sum(df$format == "unknown", na.rm = TRUE), "\n")
  
  cat("Phase 2: Exact matching by DOI and normalized string...\n")
  
  # Initialize cluster_id
  df <- df %>%
    mutate(cluster_id = NA_character_)
  
  # DOI-based matching (only for WoS with valid DOI)
  valid_dois <- df %>%
    dplyr::filter(!is.na(doi), 
                  str_detect(doi, "^10\\.\\d{4,}/"),
                  nchar(doi) >= 10)
  
  if (nrow(valid_dois) > 0) {
    doi_clusters <- valid_dois %>%
      group_by(doi) %>%
      mutate(cluster_id = paste0("DOI_", min(CR_id))) %>%
      ungroup()
    
    df$cluster_id[df$CR_id %in% doi_clusters$CR_id] <- doi_clusters$cluster_id
    
    cat("  Matched", nrow(doi_clusters), "citations via", 
        n_distinct(doi_clusters$doi), "unique DOIs\n")
  }
  
  # Exact normalized string matching (fast pre-clustering)
  unmatched_df <- df %>%
    dplyr::filter(is.na(cluster_id))
  
  if (nrow(unmatched_df) > 0) {
    exact_matches <- unmatched_df %>%
      group_by(CR_normalized) %>%
      mutate(
        n_exact = n(),
        cluster_id = if_else(n_exact > 1, paste0("EXACT_", min(CR_id)), NA_character_)
      ) %>%
      ungroup() %>%
      dplyr::filter(!is.na(cluster_id))
    
    if (nrow(exact_matches) > 0) {
      df$cluster_id[df$CR_id %in% exact_matches$CR_id] <- exact_matches$cluster_id
      cat("  Matched", nrow(exact_matches), "citations via exact normalization\n")
    }
  }
  
  cat("Phase 3: Blocking by author + year + journal...\n")
  
  # Restrictive blocking - must have same author, year, and journal
  df <- df %>%
    mutate(
      blocking_key = paste0(
        coalesce(first_author, "UNK"), "_",
        coalesce(year, "0000"), "_",
        coalesce(substr(journal, 1, 15), "UNK")
      )
    )
  
  block_sizes <- table(df$blocking_key)
  cat("  Created", length(block_sizes), "blocks\n")
  cat("  Average block size:", round(mean(block_sizes), 1), "\n")
  
  cat("Phase 4: Fuzzy matching within blocks...\n")
  
  # Function to match within a block
  match_within_block <- function(block_df) {
    # Only work with unmatched citations
    unmatched <- block_df %>% dplyr::filter(is.na(cluster_id))
    
    if (nrow(unmatched) <= 1) {
      if (nrow(unmatched) == 1) {
        block_df$cluster_id[is.na(block_df$cluster_id)] <- as.character(unmatched$CR_id[1])
      }
      return(block_df)
    }
    
    unique_norm <- unique(unmatched$CR_normalized)
    
    if (length(unique_norm) == 1) {
      cluster_id <- as.character(unmatched$CR_id[1])
      block_df$cluster_id[is.na(block_df$cluster_id)] <- cluster_id
      return(block_df)
    }
    
    # Fuzzy matching only for reasonable block sizes
    if (length(unique_norm) > 1 && length(unique_norm) < 100) {
      dist_matrix <- stringdistmatrix(unique_norm, unique_norm, method = method)
      
      max_dist <- max(dist_matrix)
      if (max_dist > 0) {
        sim_matrix <- 1 - (dist_matrix / max_dist)
      } else {
        sim_matrix <- matrix(1, nrow = length(unique_norm), ncol = length(unique_norm))
      }
      
      if (nrow(sim_matrix) > 1) {
        hc <- hclust(as.dist(1 - sim_matrix), method = "complete")
        clusters <- cutree(hc, h = 1 - threshold)
        
        cluster_map <- tibble(
          CR_normalized = unique_norm,
          temp_cluster = clusters
        )
        
        unmatched <- unmatched %>%
          left_join(cluster_map, by = "CR_normalized") %>%
          mutate(new_cluster_id = paste0(unique(block_df$blocking_key)[1], "_C", temp_cluster))
        
        block_df$cluster_id[block_df$CR_id %in% unmatched$CR_id] <- unmatched$new_cluster_id
      }
    } else {
      # For very large blocks, keep as separate
      for (i in 1:nrow(unmatched)) {
        if (is.na(block_df$cluster_id[block_df$CR_id == unmatched$CR_id[i]])) {
          block_df$cluster_id[block_df$CR_id == unmatched$CR_id[i]] <- 
            as.character(unmatched$CR_id[i])
        }
      }
    }
    
    return(block_df)
  }
  
  # Apply matching per block
  df_matched <- df %>%
    group_by(blocking_key) %>%
    group_split() %>%
    lapply(match_within_block) %>%
    bind_rows()
  
  # Ensure all have cluster_id
  df_matched <- df_matched %>%
    mutate(cluster_id = ifelse(is.na(cluster_id), as.character(CR_id), cluster_id))
  
  cat("Phase 5: Selecting canonical representatives...\n")
  
  result <- df_matched %>%
    group_by(cluster_id) %>%
    mutate(
      n_cluster = n(),
      # Scoring: DOI > volume > pages > length
      completeness_score = 
        (!is.na(doi)) * 100 + 
        (!is.na(volume)) * 10 + 
        (!is.na(pages)) * 5 + 
        nchar(CR_original) * 0.01,
      CR_canonical = CR_original[which.max(completeness_score)][1]
    ) %>%
    ungroup() %>%
    arrange(desc(n_cluster), cluster_id) %>%
    select(CR_original, CR_canonical, cluster_id, n_cluster, 
           format, first_author, year, journal, volume, doi, blocking_key)
  
  cat("Completed! Found", length(unique(result$cluster_id)), "unique clusters from", 
      nrow(result), "valid citations.\n")
  cat("  Clusters with >1 citation:", sum(result$n_cluster > 1), "\n")
  cat("  Total variants found:", sum(result$n_cluster) - length(unique(result$cluster_id)), "\n")
  
  # Add back filtered citations
  filtered_citations <- tibble(
    CR_original = CR_vector[!CR_vector %in% result$CR_original]
  ) %>%
    dplyr::filter(CR_original != "", !is.na(CR_original)) %>%
    mutate(
      CR_canonical = CR_original,
      cluster_id = paste0("FILTERED_", row_number()),
      n_cluster = 1L,
      format = NA_character_,
      first_author = NA_character_,
      year = NA_character_,
      journal = NA_character_,
      volume = NA_character_,
      doi = NA_character_,
      blocking_key = "FILTERED"
    )
  
  if (nrow(filtered_citations) > 0) {
    result <- bind_rows(result, filtered_citations)
    cat("Added", nrow(filtered_citations), "filtered citations as separate entries\n")
  }
  
  return(result)
}


#' Apply citation normalization to bibliometrix data frame
#'
#' This is a convenience wrapper function that applies \code{\link{normalize_citations}} 
#' to a bibliometrix data frame (typically loaded with \code{\link{convert2df}}). It 
#' extracts citations from the CR field, performs normalization and matching, and 
#' returns comprehensive results including per-paper citation lists and summary statistics.
#'
#' @param M A bibliometrix data frame, typically created by \code{\link{convert2df}}.
#'   Must contain the columns:
#'   \itemize{
#'     \item \code{SR}: Short reference identifier for each document
#'     \item \code{CR}: Cited references field (citations separated by semicolons)
#'   }
#' @param threshold Numeric value between 0 and 1 indicating the similarity threshold 
#'   for matching citations. Default is 0.85. See \code{\link{normalize_citations}} 
#'   for details on selecting appropriate thresholds.
#' @param method String distance method to use for fuzzy matching. Options include:
#'   \itemize{
#'     \item "jw" (default): Jaro-Winkler distance, optimized for bibliographic strings
#'     \item "lv": Levenshtein distance
#'     \item Other methods supported by \code{\link[stringdist]{stringdistmatrix}}
#'   }
#' @param min_chars Minimum characters for valid citations (default: 20)
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Splits the CR field by semicolons to extract individual citations
#'   \item Trims whitespace from each citation
#'   \item Applies \code{\link{normalize_citations}} to identify duplicate citations
#'   \item Links normalized citations back to source documents (SR)
#'   \item Generates summary statistics and reconstructs normalized CR fields
#' }
#' 
#' The normalized CR field can be used to replace the original CR field in subsequent 
#' bibliometric analyses, ensuring that citation counts and network analyses are not 
#' inflated by duplicate citations with minor formatting differences.
#'
#' @return A list with four elements:
#' \describe{
#'   \item{full_data}{A data frame with columns:
#'     \itemize{
#'       \item \code{SR}: Source document identifier
#'       \item \code{CR}: Original citation string
#'       \item \code{CR_canonical}: Canonical (normalized) citation
#'       \item \code{cluster_id}: Unique cluster identifier
#'       \item \code{n_cluster}: Size of the citation cluster
#'       \item \code{first_author}, \code{year}, \code{journal}, \code{volume}: Extracted metadata
#'     }
#'   }
#'   \item{summary}{A data frame summarizing citation frequencies with columns:
#'     \itemize{
#'       \item \code{CR_canonical}: The canonical citation for each cluster
#'       \item \code{n}: Total number of times this work was cited
#'       \item \code{n_variants}: Number of different formatting variants found
#'       \item \code{variants_example}: Sample of variant formats (up to 3 examples)
#'     }
#'     Sorted by citation frequency (n) in descending order.
#'   }
#'   \item{matched_citations}{Complete output from \code{\link{normalize_citations}}, 
#'     useful for detailed analysis of the matching process.}
#'   \item{CR_normalized}{A data frame with columns:
#'     \itemize{
#'       \item \code{SR}: Source document identifier
#'       \item \code{CR}: Reconstructed CR field with normalized citations (semicolon-separated)
#'       \item \code{n_references}: Number of unique references after normalization
#'     }
#'     This can be merged back with M to replace the original CR field.
#'   }
#' }
#'
#' @examples
#' \dontrun{
#' # Load bibliometric data
#' file <- "https://www.bibliometrix.org/datasets/savedrecs.txt"
#' M <- convert2df(file, dbsource = "wos", format = "plaintext")
#' 
#' # Apply citation normalization
#' results <- applyCitationMatching(M, threshold = 0.85)
#' 
#' # View top cited works (after normalization)
#' head(results$summary, 20)
#' 
#' # See how many variants were found for the top citation
#' top_citation <- results$summary$CR_canonical[1]
#' variants <- subset(results$full_data, CR_canonical == top_citation)
#' unique(variants$CR)
#' 
#' # Replace original CR with normalized CR in the data frame
#' M_normalized <- M %>%
#'   rename(CR_orig = CR) %>%
#'   left_join(results$CR_normalized, by = "SR")
#' 
#' # Compare citation counts before and after normalization
#' original_citations <- strsplit(M$CR, ";") %>% 
#'   unlist() %>% 
#'   trimws() %>% 
#'   table() %>% 
#'   length()
#' 
#' normalized_citations <- nrow(results$summary)
#' 
#' cat("Original unique citations:", original_citations, "\n")
#' cat("After normalization:", normalized_citations, "\n")
#' cat("Duplicates found:", original_citations - normalized_citations, "\n")
#' 
#' # Use normalized data for further analysis
#' CR_analysis <- citations(M_normalized, field = "article", sep = ";")
#' }
#'
#' @seealso 
#' \code{\link{normalize_citations}} for the underlying normalization algorithm
#' \code{\link{citations}} for citation analysis
#' \code{\link{localCitations}} for local citation analysis
#'
#' @references
#' Aria, M. & Cuccurullo, C. (2017). bibliometrix: An R-tool for comprehensive 
#' science mapping analysis. Journal of Informetrics, 11(4), 959-975.
#' 
#' @export

applyCitationMatching <- function(M, threshold = 0.90, method = "jw", min_chars = 20) {
  
  # Extract citations
  CR <- strsplit(M$CR, ";")
  CR_df <- tibble(
    SR = rep(M$SR, lengths(CR)), 
    CR = trimws(unlist(CR))
  )
  
  # Apply normalization
  cat("\n=== CITATION NORMALIZATION ===\n")
  matched <- normalize_citations(CR_df$CR, threshold = threshold, 
                                 method = method, min_chars = min_chars)
  
  # Join with SR
  result <- CR_df %>%
    left_join(matched, by = c("CR" = "CR_original"))#, relationship = "many-to-many")
  
  # Reference citation counts
  citation_count <- result %>% distinct() %>% count(CR_canonical, sort=TRUE)
  
  # Create summary (exclude filtered citations)
  summary <- result %>%
    dplyr::filter(!grepl("^FILTERED_", cluster_id)) %>%
    group_by(CR_canonical, cluster_id) %>%
    summarise(
      #n = n(),
      n_variants = n_distinct(CR),
      format = first(format),
      variants_example = paste(head(unique(CR), 3), collapse = " | "),
      .groups = "drop"
    ) %>%
    left_join(citation_count, by = "CR_canonical")
    #arrange(desc(n))
  
  # Reconstruct CR field
  cat("Reconstructing CR field for each paper...\n")
  CR_by_paper <- result %>%
    group_by(SR) %>%
    summarise(
      CR = paste(unique(CR_canonical), collapse = "; "),
      n_references = n_distinct(CR_canonical),
      .groups = "drop"
    )
  
  # Summary statistics
  cat("\n=== SUMMARY STATISTICS ===\n")
  cat("Total citations processed:", nrow(CR_df), "\n")
  cat("Valid citations analyzed:", length(unique(result$CR)), "\n")
  cat("Unique works identified:", length(unique(result$CR_canonical)), "\n")
  cat("Citations with variants (n>1):", sum(summary$n_variants > 1), "\n")
  cat("Duplicate citations removed:", 
      length(unique(result$CR)) - length(unique(result$CR_canonical)), "\n")
  
  return(list(
    full_data = result,
    summary = summary,
    matched_citations = matched,
    CR_normalized = CR_by_paper
  ))
}