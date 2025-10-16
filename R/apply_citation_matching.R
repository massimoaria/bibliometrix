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
#' @param min_chars Minimum characters for valid citations (default: 20)
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
#' \strong{Phase 1.5: Journal Normalization}
#' The function uses the LTWA (List of Title Word Abbreviations) database from 
#' ISO 4 standards to normalize journal names. This ensures that abbreviated 
#' forms (e.g., "J. Clean. Prod.") and full forms (e.g., "Journal of Cleaner 
#' Production") are recognized as the same journal and matched together.
#' 
#' The LTWA database is included in the bibliometrix package. If not found, 
#' the function attempts to download it from ISSN.org. Journal normalization 
#' can be disabled by ensuring the LTWA database is not available.
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
#'   \item \code{journal_iso4}: Journal name normalized to ISO4 abbreviated form
#'   \item \code{journal_original}: Original journal name as extracted from citation
#'   \item \code{volume}: Volume number
#'   \item \code{doi}: Digital Object Identifier (when available)
#'   \item \code{blocking_key}: Internal key used for blocking (author_year_journal)
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
      # Remove diacritics FIRST (before uppercasing)
      remove_diacritics() %>%
      toupper() %>%
      str_replace_all("\\s*:\\s*", " ") %>%           # Remove colons with surrounding spaces
      str_replace_all("\\s*;\\s*", " ") %>%           # Remove semicolons with surrounding spaces
      str_replace_all("\\s*-\\s*", " ") %>%           # Normalize hyphens
      str_replace_all("\\(\\d+\\)", "") %>%           # Remove issue numbers
      str_replace_all("PP\\.?\\s*\\d+\\s*[-\u2013\u2014]\\s*\\d+", "") %>%  # Remove page numbers
      str_replace_all("P\\.?\\s*\\d+\\s*[-\u2013\u2014]\\s*\\d+", "") %>%
      # remove quotes symbols
      str_replace_all("<e2><80><9c>", " ") %>%
      str_replace_all('"', " ") %>% 
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
    
    # First author - IMPROVED PATTERN for both WoS and Scopus
    # WoS: "Adams CA," or "Atkins J," or "Atkins JF,"
    # Scopus: "ADAMS, C." or similar
    first_author <- str_extract(x, "^[A-Z][A-Za-z\\s\\-']+(?=,)") %>%
      str_trim()
    
    # If extraction failed, try alternative pattern
    if (is.na(first_author)) {
      first_author <- str_extract(x, "^[^,]+") %>%
        str_trim()
    }
    
    # Clean up: remove diacritics, trailing periods, normalize
    if (!is.na(first_author)) {
      first_author <- first_author %>%
        remove_diacritics() %>%
        str_remove_all("\\.$") %>%
        str_squish() %>%
        toupper()
    }
    
    
    # Year
    year <- str_extract(x, "\\d{4}") %>%
      head(1)  # Take first year found
    
    # DOI extraction
    doi <- extract_doi(x)
    
    if (format == "scopus") {
      # Scopus: AUTHOR1, AUTHOR2, ..., TITLE (YEAR) JOURNAL, VOLUME, PAGES
      
      # Extract everything between first comma and year
      temp_title <- str_extract(x, "(?<=,\\s)(.+?)(?=\\s*\\(\\d{4}\\))")
      
      if (!is.na(temp_title)) {
        # Strategy: Find the LAST occurrence of author pattern
        # Pattern needs to match:
        # - SURNAME, I., (with comma after) - for middle authors
        # - SURNAME, I. (without comma after) - for last author
        
        # Try pattern WITH comma after (for all authors except last)
        author_pattern_with_comma <- "[A-Z][A-Za-z\\-']+,\\s*[A-Z][A-Z\\.\\s]*\\.?,\\s*"
        matches_with_comma <- str_locate_all(temp_title, author_pattern_with_comma)[[1]]
        
        # Try pattern WITHOUT comma after (for potentially last author)
        author_pattern_no_comma <- "[A-Z][A-Za-z\\-']+,\\s*[A-Z][A-Z\\.\\s]*\\.?\\s+"
        matches_no_comma <- str_locate_all(temp_title, author_pattern_no_comma)[[1]]
        
        # Combine all matches and find the one that ends last
        last_end <- 0
        
        if (nrow(matches_with_comma) > 0) {
          last_end <- max(last_end, max(matches_with_comma[, "end"]))
        }
        
        if (nrow(matches_no_comma) > 0) {
          last_end <- max(last_end, max(matches_no_comma[, "end"]))
        }
        
        if (last_end > 0) {
          # Extract title starting after last author
          title <- substr(temp_title, last_end + 1, nchar(temp_title))
        } else {
          # No author pattern found - use full temp_title
          title <- temp_title
        }
        
        # Clean up
        title <- str_replace_all(title, '"|<e2><80><9c>', " ")
        title <- str_trim(title)
        
        # Safety: remove any remaining author-like fragments at start
        # (single letter followed by dot and space)
        title <- str_remove(title, "^[A-Z]\\.?,?\\s+")
        title <- str_trim(title)
        
      } else {
        title <- NA
      }
      
      # Rest of Scopus extraction...
      journal <- str_extract(x, "(?<=\\d{4}\\)\\s)([A-Z][A-Z\\s&\\.-]+?)(?=,\\s*\\d+|,\\s*PP)")
      
      # Volume
      after_year <- str_extract(x, "\\d{4}\\)(.+)$")
      if (!is.na(after_year)) {
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
      pages <- str_extract(x, "PP\\.?\\s*\\d+[-\u2013\u2014]\\d+|PP\\.?\\s*\\d+")
      
      # Extract significant words from CLEANED title
      if (!is.na(title) && nchar(title) > 0) {
        title_words <- str_extract_all(title, "\\b[A-Z]{3,}\\b")[[1]]
        
        # Filter stop words
        stop_words <- c("THE", "AND", "FOR", "FROM", "WITH", "ABOUT", "THAT", "THIS",
                        "BETWEEN", "THROUGH", "DURING", "BEFORE", "AFTER", "WHICH", 
                        "THEIR", "THESE", "THOSE", "WHEN", "WHERE", "WHAT")
        title_words <- title_words[!title_words %in% stop_words]
        
        # Take first 4 significant words
        title_words <- head(title_words, 4) %>%
          paste(collapse = " ")
        
        # Safety check
        if (is.na(title_words) || nchar(title_words) < 3) {
          title_words <- NA_character_
        }
      } else {
        title_words <- NA_character_
      }
      
    } else {
      # WoS: AUTHOR, YEAR, JOURNAL, V##, P##, DOI
      
      # Extract journal (after year, before V or P or DOI)
      journal <- str_extract(x, "(?<=\\d{4},\\s)([A-Z][A-Z\\s&\\-]+?)(?=,\\s*V\\d+|,\\s*P\\d+|,\\s*DOI)")
      
      # Volume - look for V followed by digits
      volume <- str_extract(x, "V(\\d+)") %>%
        str_extract("\\d+")
      
      # Pages - look for P followed by digits
      pages <- str_extract(x, "P(\\d+)") %>%
        str_remove("P")
      
      # For WoS, NO title extraction - not available in format
      title_words <- NA_character_
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
    ) %>% # exclude rows with NA in first_author and year
    dplyr::filter(!is.na(first_author), !is.na(year)) %>% 
    # exclude rows with year < 1700 and > current year+1
    dplyr::filter(as.numeric(year) >= 1700 & as.numeric(year) <= as.numeric(format(Sys.Date(), "%Y")) + 1)
  
  cat("  Filtered out", length(CR_vector) - nrow(df), "invalid citations\n")
  
  cat("  Detected formats: WoS =", sum(df$format == "wos", na.rm = TRUE), 
      ", Scopus =", sum(df$format == "scopus", na.rm = TRUE),
      ", Unknown =", sum(df$format == "unknown", na.rm = TRUE), "\n")
  
  cat("Phase 1.5: Normalizing journal names to ISO4 format...\n")
  
  # Load LTWA database
  ltwa <- NULL
  

    # Try to load from bibliometrix package data
    data("ltwa", package = "bibliometrix", envir = environment())
  
    # Apply ISO4 normalization if LTWA is available
    if (!is.null(ltwa) && nrow(ltwa) > 0) {
      
      # Create ISO4 lookup table for efficiency
      journal_iso4_lookup <- create_journal_iso4_lookup(df$journal, ltwa)
      
      # Apply normalization
      df <- df %>%
        left_join(journal_iso4_lookup, by = c("journal" = "journal_original")) %>%
        mutate(
          journal_original = journal,  # Preserve original
          journal = coalesce(journal_iso4, journal)  # Use ISO4, fallback to original
        ) %>%
        select(-journal_iso4)
      
      cat("  Journal ISO4 normalization completed\n")
      
      # *** CRITICAL: Recreate CR_normalized with ISO4 journal names ***
      cat("  Recreating normalized strings with ISO4 journal names...\n")
      df <- df %>%
        mutate(
          # Reconstruct the citation string with ISO4 journal
          temp_citation = case_when(
            format == "scopus" ~ paste0(
              first_author, ", ",
              year, " ",
              journal, " ",
              ifelse(!is.na(volume), paste0("V", volume), ""),
              ifelse(!is.na(pages), paste0(" ", pages), "")
            ),
            format == "wos" ~ paste0(
              first_author, ", ",
              year, ", ",
              journal,
              ifelse(!is.na(volume), paste0(", V", volume), ""),
              ifelse(!is.na(pages), paste0(", P", pages), ""),
              ifelse(!is.na(doi), paste0(", DOI ", doi), "")
            ),
            TRUE ~ CR_original
          ),
          # Use sapply to normalize each string individually
          CR_normalized = sapply(temp_citation, normalize_string, USE.NAMES = FALSE)
        ) %>%
        select(-temp_citation)
      
    } else {
      cat("  Proceeding without journal normalization\n")
      df <- df %>%
        mutate(journal_original = journal)
    }
  
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
    
    # Check if block is predominantly WoS or Scopus
    format_counts <- table(unmatched$format)
    predominant_format <- names(which.max(format_counts))
    
    # For WoS citations, use more deterministic matching based on metadata
    if (predominant_format == "wos") {
      # WoS: Match by first_author + year + journal + volume + page_start
      unmatched <- unmatched %>%
        mutate(
          page_start = str_extract(pages, "\\d+"),
          wos_key = paste0(
            coalesce(first_author, "UNK"), "_",
            coalesce(year, "0000"), "_",
            coalesce(journal, "UNK"), "_",
            coalesce(volume, "NA"), "_",
            coalesce(page_start, "NOPAGE")
          )
        ) %>%
        group_by(wos_key) %>%
        mutate(
          new_cluster_id = paste0(unique(block_df$blocking_key)[1], "_C", cur_group_id())
        ) %>%
        ungroup() %>%
        select(-wos_key, -page_start)
      
      block_df$cluster_id[block_df$CR_id %in% unmatched$CR_id] <- unmatched$new_cluster_id
      return(block_df)
    }
    
    # For Scopus citations, use fuzzy matching as before
    unique_norm <- unique(unmatched$CR_normalized)
    
    if (length(unique_norm) == 1) {
      cluster_id <- as.character(unmatched$CR_id[1])
      block_df$cluster_id[is.na(block_df$cluster_id)] <- cluster_id
      return(block_df)
    }
    
    # Fuzzy matching for Scopus (or mixed blocks)
    if (length(unique_norm) > 1 && length(unique_norm) < 100) {
      dist_matrix <- stringdist::stringdistmatrix(unique_norm, unique_norm, method = method)
      
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
  
  cat("Phase 4.5: Post-processing - Merging clusters with identical metadata...\n")
  
  n_clusters_before_merge <- n_distinct(df_matched$cluster_id)
  
  # Different merge strategies based on format
  df_matched <- df_matched %>%
    mutate(
      # Extract starting page number
      page_start = str_extract(pages, "^\\d+"),
      
      # For WoS: use deterministic key (author + year + journal + volume + page)
      # For Scopus: add title_words to distinguish articles in same issue
      merge_key = case_when(
        # WoS format: DOI is most reliable
        format == "wos" & !is.na(doi) ~ paste0("DOI_", doi),
        
        # WoS format without DOI: use metadata + pages
        format == "wos" ~ paste0(
          coalesce(first_author, "UNK"), "_",
          coalesce(year, "0000"), "_",
          coalesce(journal, "UNK"), "_",
          coalesce(volume, "NA"), "_",
          coalesce(page_start, "NOPAGE")
        ),
        
        # Scopus format: use metadata + pages + title fingerprint
        format == "scopus" & !is.na(page_start) & !is.na(title_words) ~ paste0(
          coalesce(first_author, "UNK"), "_",
          coalesce(year, "0000"), "_",
          coalesce(journal, "UNK"), "_",
          coalesce(volume, "NA"), "_",
          coalesce(page_start, "NOPAGE"), "_",
          title_words
        ),
        
        # Scopus with pages but no title
        format == "scopus" & !is.na(page_start) ~ paste0(
          coalesce(first_author, "UNK"), "_",
          coalesce(year, "0000"), "_",
          coalesce(journal, "UNK"), "_",
          coalesce(volume, "NA"), "_",
          coalesce(page_start, "NOPAGE")
        ),
        
        # Scopus with title but no pages (rare)
        format == "scopus" & !is.na(title_words) ~ paste0(
          coalesce(first_author, "UNK"), "_",
          coalesce(year, "0000"), "_",
          coalesce(journal, "UNK"), "_",
          coalesce(volume, "NA"), "_",
          title_words
        ),
        
        # Fallback: basic metadata only (risky)
        TRUE ~ paste0(
          coalesce(first_author, "UNK"), "_",
          coalesce(year, "0000"), "_",
          coalesce(journal, "UNK"), "_",
          coalesce(volume, "NA")
        )
      )
    ) %>%
    group_by(merge_key) %>%
    mutate(
      cluster_id = min(cluster_id, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(-merge_key, -page_start)
  
  n_clusters_after_merge <- n_distinct(df_matched$cluster_id)
  n_additional_matches <- n_clusters_before_merge - n_clusters_after_merge
  
  cat("  Clusters before metadata merge:", n_clusters_before_merge, "\n")
  cat("  Clusters after metadata merge:", n_clusters_after_merge, "\n")
  cat("  Additional matches found:", n_additional_matches, "\n")
  
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
    select(
      CR_original, 
      CR_canonical, 
      cluster_id, 
      n_cluster, 
      format, 
      first_author, 
      year, 
      journal_iso4 = journal,        # Rename for clarity
      journal_original,              # Keep original
      volume, 
      doi, 
      blocking_key
    )
  
  cat("Completed! Found", length(unique(result$cluster_id)), "unique clusters from", 
      nrow(result), "valid citations.\n")
  cat("  Clusters with >1 citation:", sum(result$n_cluster > 1), "\n")
  cat("  Total variants found:", sum(result$n_cluster) - length(unique(result$cluster_id)), "\n")
  
  # Add back filtered citations with basic feature extraction
  filtered_citations <- tibble(
    CR_original = CR_vector[!CR_vector %in% result$CR_original]
  ) %>%
    dplyr::filter(CR_original != "", !is.na(CR_original))
  
  if (nrow(filtered_citations) > 0) {
    
    # Try to extract basic features even for filtered citations
    filtered_features <- lapply(filtered_citations$CR_original, function(x) {
      tryCatch({
        # Basic extraction without full validation
        first_author <- str_extract(x, "^[A-Z][A-Z\\s-\\.]+(?=,)") %>%
          str_remove_all("\\.$") %>%
          str_trim()
        
        year <- str_extract(x, "\\(?\\d{4}\\)?") %>% 
          str_remove_all("[()]")
        
        # Detect format to extract journal
        if (str_detect(x, "\\(\\d{4}\\)\\s+[A-Z]")) {
          # Scopus format
          journal <- str_extract(x, "(?<=\\d{4}\\)\\s)([A-Z][A-Z\\s&\\.-]+?)(?=,)")
        } else if (str_detect(x, ",\\s*\\d{4},\\s*[A-Z]")) {
          # WoS format
          journal <- str_extract(x, "(?<=\\d{4},\\s)([A-Z][A-Z\\s&-]+?)(?=,)")
        } else {
          journal <- NA_character_
        }
        
        list(first_author = first_author, year = year, journal = journal)
      }, error = function(e) {
        list(first_author = NA_character_, year = NA_character_, journal = NA_character_)
      })
    })
    
    filtered_citations <- filtered_citations %>%
      mutate(
        CR_canonical = CR_original,
        cluster_id = paste0("FILTERED_", row_number()),
        n_cluster = 1L,
        format = NA_character_,
        first_author = sapply(filtered_features, function(x) x$first_author %||% NA),
        year = sapply(filtered_features, function(x) x$year %||% NA),
        journal_iso4 = NA_character_,  # No ISO4 normalization for filtered
        journal_original = sapply(filtered_features, function(x) x$journal %||% NA),
        volume = NA_character_,
        doi = NA_character_,
        blocking_key = "FILTERED"
      )
    
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
    left_join(matched, by = c("CR" = "CR_original"))
  
  # Reference citation counts
  citation_count <- result %>% distinct() %>% count(CR_canonical, sort = TRUE)
  
  # Create summary
  summary <- result %>%
    dplyr::filter(!grepl("^FILTERED_", cluster_id)) %>%
    group_by(CR_canonical, cluster_id) %>%
    summarise(
      n_variants = n_distinct(CR),
      format = first(format),
      first_author = first(first_author),
      year = first(year),
      journal_iso4 = first(journal_iso4),        # Add ISO4 journal
      journal_original = first(journal_original), # Add original journal
      variants_example = paste(head(unique(CR), 3), collapse = " | "),
      .groups = "drop"
    ) %>%
    ungroup() %>% 
    left_join(citation_count, by = "CR_canonical") %>% 
    arrange(desc(n))
  
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
    full_data = result %>% distinct(),
    summary = summary,
    matched_citations = matched,
    CR_normalized = CR_by_paper
  ))
}

#' Normalize journal names to ISO4 abbreviated form
#'
#' Converts all journal names to their ISO4 abbreviated form using LTWA.
#' Only uses English language entries from LTWA to avoid foreign word matches.
#'
#' @param journal_name Character string with journal name
#' @param ltwa_db Data frame with LTWA database
#'
#' @return Normalized journal name in ISO4 abbreviated form
#' @keywords internal

normalize_journal_to_iso4 <- function(journal_name, ltwa_db) {
  
  if (is.na(journal_name) || journal_name == "" || is.null(journal_name)) {
    return(NA_character_)
  }
  
  # Clean and prepare
  journal_upper <- journal_name %>%
    toupper() %>%
    str_replace_all("&", "AND") %>%
    str_trim()
  
  # Split into words
  words_with_punct <- str_split(journal_upper, "\\s+")[[1]]
  words <- str_remove_all(words_with_punct, "[[:punct:]]")
  words <- words[nchar(words) > 0]
  
  if (length(words) == 0) return(NA_character_)
  
  # Prepare LTWA lookup - ONLY English entries
  ltwa_english <- ltwa_db %>%
    dplyr::filter(
      !is.na(WORD), 
      !is.na(ABBREVIATION),
      ABBREVIATION != "",
      ABBREVIATION != "n.a.",
      # Filter for English language
      str_detect(LANGUAGES, regex("english", ignore_case = TRUE)) |
        LANGUAGES == "Multiple Languages"
    ) %>%
    mutate(
      WORD_CLEAN = toupper(str_remove_all(WORD, "[[:punct:]]")) %>% str_squish(),
      ABBR_CLEAN = toupper(str_remove_all(ABBREVIATION, "[[:punct:]]")) %>% str_squish()
    ) %>%
    dplyr::filter(WORD_CLEAN != "", ABBR_CLEAN != "") %>%
    select(WORD_CLEAN, ABBR_CLEAN) %>%
    distinct()
  
  # Create lookup: word -> abbreviation
  word_to_abbr <- setNames(ltwa_english$ABBR_CLEAN, ltwa_english$WORD_CLEAN)
  
  # Also create reverse for words that are already abbreviated
  abbr_set <- unique(ltwa_english$ABBR_CLEAN)
  
  # Process each word
  abbreviated_words <- sapply(words, function(word) {
    
    # Skip very short words (articles, prepositions, conjunctions)
    if (word %in% c("OF", "THE", "A", "AN", "AND", "OR", "FOR", "IN", "ON", "AT", "TO", "BY")) {
      return(word)
    }
    
    # Check if word is already an abbreviation in LTWA
    if (word %in% abbr_set) {
      return(word)
    }
    
    # Try to abbreviate using LTWA
    if (word %in% names(word_to_abbr)) {
      return(word_to_abbr[[word]])
    }
    
    # Word not found in LTWA - check if it's a common single-letter abbreviation
    if (nchar(word) == 1) {
      return(word)
    }
    
    # For words not in LTWA, keep original
    # (might be proper nouns, acronyms, etc.)
    return(word)
    
  }, USE.NAMES = FALSE)
  
  # Reconstruct journal in ISO4 form
  result <- paste(abbreviated_words, collapse = " ")
  
  return(result)
}


#' Prepare LTWA database for efficient lookup
#'
#' Pre-processes LTWA database into optimized lookup tables
#'
#' @param ltwa_db LTWA database data frame
#' @return List with singles, prefix, and phrase lookup tables
#' @keywords internal

prepare_ltwa_lookup <- function(ltwa_db) {
  
  # Filter for English language entries only
  ltwa_english <- ltwa_db %>%
    dplyr::filter(
      !is.na(WORD), 
      !is.na(ABBREVIATION),
      str_detect(LANGUAGES, regex("english", ignore_case = TRUE)) |
        LANGUAGES == "Multiple Languages"
    ) %>%
    mutate(
      WORD_CLEAN = toupper(str_remove_all(WORD, "[[:punct:]]")) %>% str_squish(),
      ABBR_CLEAN = toupper(str_remove_all(ABBREVIATION, "[[:punct:]]")) %>% str_squish(),
      # Mark words that should not be abbreviated
      NO_ABBR = ABBREVIATION == "" | ABBREVIATION == "n.a."
    ) %>%
    dplyr::filter(WORD_CLEAN != "")
  
  # Separate into single words and multi-word phrases
  ltwa_singles <- ltwa_english %>%
    dplyr::filter(!str_detect(WORD_CLEAN, "\\s")) %>%
    select(WORD = WORD_CLEAN, ABBREVIATION = ABBR_CLEAN, NO_ABBR)
  
  ltwa_phrases <- ltwa_english %>%
    dplyr::filter(str_detect(WORD_CLEAN, "\\s")) %>%
    select(WORD = WORD_CLEAN, ABBREVIATION = ABBR_CLEAN, NO_ABBR)
  
  # Create prefix lookup (for partial matching)
  ltwa_prefix <- ltwa_singles %>%
    dplyr::filter(!NO_ABBR, nchar(WORD) >= 4) %>%
    select(WORD, ABBREVIATION)
  
  list(
    singles = ltwa_singles,
    phrases = ltwa_phrases,
    prefix = ltwa_prefix
  )
}


#' Articles, prepositions, and conjunctions to be removed (ISO 4 standard)
#'
#' @keywords internal

get_iso4_stop_words <- function() {
  list(
    # Articles (removed in most positions)
    articles = c("A", "AN", "THE"),
    
    # Prepositions (removed when not at start)
    prepositions = c("OF", "IN", "ON", "AT", "TO", "FOR", "WITH", "FROM", 
                     "BY", "ABOUT", "AS", "INTO", "LIKE", "THROUGH", 
                     "AFTER", "OVER", "BETWEEN", "OUT", "AGAINST", "DURING",
                     "WITHOUT", "BEFORE", "UNDER", "AROUND", "AMONG"),
    
    # Conjunctions (removed)
    conjunctions = c("AND", "OR", "BUT", "NOR", "YET", "SO"),
    
    # Common scientific terms that should be abbreviated
    common_abbr = c(
      "JOURNAL" = "J",
      "JOURNALS" = "J",
      "RESEARCH" = "RES",
      "SCIENCE" = "SCI",
      "SCIENCES" = "SCI",
      "SCIENTIFIC" = "SCI",
      "TECHNOLOGY" = "TECHNOL",
      "TECHNOLOGICAL" = "TECHNOL",
      "INTERNATIONAL" = "INT",
      "NATIONAL" = "NATL",
      "AMERICAN" = "AM",
      "EUROPEAN" = "EUR",
      "MANAGEMENT" = "MANAG",
      "ACADEMY" = "ACAD",
      "SOCIETY" = "SOC",
      "ASSOCIATION" = "ASSOC",
      "ORGANIZATION" = "ORGAN",
      "ENVIRONMENTAL" = "ENV",
      "ENGINEERING" = "ENG",
      "APPLIED" = "APPL",
      "THEORETICAL" = "THEOR",
      "EXPERIMENTAL" = "EXP",
      "CLINICAL" = "CLIN",
      "MEDICAL" = "MED",
      "BIOLOGICAL" = "BIOL",
      "CHEMICAL" = "CHEM",
      "PHYSICAL" = "PHYS",
      "MATHEMATICAL" = "MATH",
      "MATHEMATICS" = "MATH",
      "ECONOMICS" = "ECON",
      "ECONOMIC" = "ECON",
      "POLICY" = "POLICY",
      "POLICIES" = "POLICIES",
      "BUSINESS" = "BUS",
      "STRATEGIC" = "STRATEG",
      "STRATEGY" = "STRATEG",
      "QUARTERLY" = "Q",
      "ANNUAL" = "ANNU",
      "ANNALS" = "ANN",
      "REVIEW" = "REV",
      "REVIEWS" = "REV",
      "STUDIES" = "STUD",
      "ANALYSIS" = "ANAL",
      "DEVELOPMENT" = "DEV",
      "PRODUCTION" = "PROD",
      "MANUFACTURING" = "MANUF",
      "OPERATIONS" = "OPER",
      "OPERATIONAL" = "OPER",
      "SYSTEMS" = "SYST",
      "SYSTEM" = "SYST",
      "INFORMATION" = "INF",
      "COMPUTING" = "COMPUT",
      "COMPUTER" = "COMPUT",
      "COMMUNICATIONS" = "COMMUN",
      "COMMUNICATION" = "COMMUN",
      "MATERIALS" = "MATER",
      "MATERIAL" = "MATER",
      "LETTERS" = "LETT",
      "PROCEEDINGS" = "PROC",
      "TRANSACTIONS" = "TRANS",
      "BULLETIN" = "BULL",
      "ANNALS" = "ANN",
      "ARCHIVES" = "ARCH",
      "REPORT" = "REP",
      "REPORTS" = "REPORTS",
      "ADVANCES" = "ADV",
      "INNOVATION" = "INNOV",
      "INNOVATIONS" = "INNOV",
      "SUSTAINABILITY" = "SUSTAIN",
      "SUSTAINABLE" = "SUSTAIN",
      "CLEANER" = "CLEAN",
      "PLANNING" = "PLAN"
    )
  )
}


#' Abbreviate a single term using LTWA
#'
#' @param word Single word to abbreviate
#' @param ltwa_lookup Pre-processed LTWA lookup tables
#' @param common_abbr Named vector of common abbreviations
#' @param check Logical, whether to check for abbreviation
#'
#' @return Abbreviated form of word
#' @keywords internal

abbreviate_term <- function(word, ltwa_lookup, common_abbr, check = TRUE) {
  
  if (!check || is.na(word) || word == "") {
    return(word)
  }
  
  word_upper <- toupper(word)
  
  # Strategy 1: Check common scientific terms
  if (word_upper %in% names(common_abbr)) {
    return(common_abbr[[word_upper]])
  }
  
  # Strategy 2: Check for exact whole-word match in LTWA
  exact_match <- ltwa_lookup$singles %>%
    dplyr::filter(WORD == word_upper)
  
  if (nrow(exact_match) > 0) {
    if (exact_match$NO_ABBR[1]) {
      return(word_upper)
    } else {
      return(exact_match$ABBREVIATION[1])
    }
  }
  
  # Strategy 3: Check for prefix match (for partial words)
  if (nchar(word_upper) >= 4) {
    prefix_matches <- ltwa_lookup$prefix %>%
      dplyr::filter(str_starts(word_upper, fixed(WORD)))
    
    if (nrow(prefix_matches) > 0) {
      # Choose longest matching prefix
      best_match <- prefix_matches %>%
        mutate(len = nchar(WORD)) %>%
        arrange(desc(len)) %>%
        slice(1)
      
      return(best_match$ABBREVIATION)
    }
  }
  
  # Strategy 4: No match found - return original
  return(word_upper)
}


#' Abbreviate journal title to ISO 4 standard
#'
#' Converts a full journal title to its ISO 4 abbreviated form using LTWA.
#' Removes articles, prepositions, and conjunctions according to ISO 4 rules.
#' Returns result WITHOUT periods (dots).
#'
#' @param title Journal title string
#' @param ltwa_lookup Pre-processed LTWA lookup tables (from prepare_ltwa_lookup)
#'
#' @return Abbreviated journal title in ISO 4 format (without periods)
#' @keywords internal

abbreviate_journal_title <- function(title, ltwa_lookup) {
  
  if (is.na(title) || title == "") {
    return(NA_character_)
  }
  
  # Clean title - REMOVE ALL PUNCTUATION including periods
  title_clean <- title %>%
    toupper() %>%
    str_replace_all("&", "AND") %>%
    # Remove parenthetical content
    str_replace_all("\\s*\\([^)]+\\)", "") %>%
    # Remove ALL punctuation (periods, commas, etc.)
    str_remove_all("[[:punct:]]") %>%
    # Normalize whitespace
    str_squish()
  
  # Get stop words
  stop_words <- get_iso4_stop_words()
  
  # Split into words
  words <- str_split(title_clean, "\\s+")[[1]]
  words <- words[nchar(words) > 0]
  
  if (length(words) == 0) return(NA_character_)
  
  # Single word titles - just abbreviate and return
  if (length(words) == 1) {
    return(abbreviate_term(words[1], ltwa_lookup, stop_words$common_abbr))
  }
  
  # Check for multi-word phrases first
  words_to_keep <- rep(TRUE, length(words))
  
  if (nrow(ltwa_lookup$phrases) > 0) {
    title_lower <- tolower(title_clean)
    
    for (i in 1:nrow(ltwa_lookup$phrases)) {
      phrase <- ltwa_lookup$phrases$WORD[i]
      phrase_lower <- tolower(phrase)
      
      if (str_detect(title_lower, fixed(phrase_lower))) {
        phrase_words <- str_split(phrase, "\\s+")[[1]]
        # Find matching positions
        for (j in 1:(length(words) - length(phrase_words) + 1)) {
          if (all(tolower(words[j:(j + length(phrase_words) - 1)]) == tolower(phrase_words))) {
            # Mark these positions
            words[j] <- ltwa_lookup$phrases$ABBREVIATION[i]
            if (length(phrase_words) > 1) {
              words_to_keep[(j+1):(j + length(phrase_words) - 1)] <- FALSE
            }
            break
          }
        }
      }
    }
    
    words <- words[words_to_keep]
  }
  
  # Remove articles and conjunctions (except first word)
  to_remove <- rep(FALSE, length(words))
  
  for (i in seq_along(words)) {
    # Keep first word always
    if (i == 1) next
    
    # Remove prepositions (not at start)
    if (words[i] %in% stop_words$prepositions) {
      to_remove[i] <- TRUE
    }
    
    # Remove articles
    if (words[i] %in% stop_words$articles) {
      to_remove[i] <- TRUE
    }
    
    # Remove conjunctions
    if (words[i] %in% stop_words$conjunctions) {
      to_remove[i] <- TRUE
    }
  }
  
  words <- words[!to_remove]
  
  # Handle hyphenated words
  words_expanded <- character()
  for (word in words) {
    if (str_detect(word, "-")) {
      # Split hyphenated word and abbreviate each part
      parts <- str_split(word, "-")[[1]]
      parts_abbr <- sapply(parts, function(p) {
        abbreviate_term(p, ltwa_lookup, stop_words$common_abbr)
      })
      words_expanded <- c(words_expanded, paste(parts_abbr, collapse = "-"))
    } else {
      words_expanded <- c(words_expanded, word)
    }
  }
  
  # Abbreviate each word
  abbreviated <- sapply(words_expanded, function(w) {
    # Check if already abbreviated (from multi-word phrase)
    if (str_detect(w, "^[A-Z]+$") && nchar(w) <= 6 && !str_detect(w, "-")) {
      # Might already be abbreviated, but try anyway
      abbreviate_term(w, ltwa_lookup, stop_words$common_abbr)
    } else if (str_detect(w, "-")) {
      # Already processed hyphenated word
      w
    } else {
      abbreviate_term(w, ltwa_lookup, stop_words$common_abbr)
    }
  }, USE.NAMES = FALSE)
  
  # Collapse and return - NO PERIODS
  result <- paste(abbreviated, collapse = " ")
  
  # Final cleanup: ensure no periods in result
  result <- str_remove_all(result, "\\.")
  
  return(result)
}


#' Create ISO4 journal normalization lookup table
#'
#' @param journal_vector Character vector of journal names
#' @param ltwa_db LTWA database data frame
#'
#' @return Data frame with journal_original and journal_iso4 columns
#' @keywords internal

create_journal_iso4_lookup <- function(journal_vector, ltwa_db) {
  
  unique_journals <- unique(journal_vector)
  unique_journals <- unique_journals[!is.na(unique_journals) & unique_journals != ""]
  
  if (length(unique_journals) == 0) {
    return(tibble(
      journal_original = character(0),
      journal_iso4 = character(0)
    ))
  }
  
  cat("  Preparing LTWA lookup tables...\n")
  ltwa_lookup <- prepare_ltwa_lookup(ltwa_db)
  
  cat("  Converting", length(unique_journals), "unique journals to ISO4...\n")
  
  show_progress <- length(unique_journals) > 100
  pb_step <- if (show_progress) max(1, floor(length(unique_journals) / 20)) else Inf
  
  normalized <- tibble(journal_original = unique_journals) %>%
    mutate(
      journal_iso4 = sapply(seq_along(journal_original), function(i) {
        if (show_progress && i %% pb_step == 0) {
          cat("\r    Progress:", round(100 * i / length(journal_original)), "%")
        }
        
        tryCatch({
          iso4 <- abbreviate_journal_title(journal_original[i], ltwa_lookup)
          if (is.na(iso4) || iso4 == "") {
            toupper(str_remove_all(journal_original[i], "[[:punct:]]")) %>% str_squish()
          } else {
            iso4
          }
        }, error = function(e) {
          toupper(str_remove_all(journal_original[i], "[[:punct:]]")) %>% str_squish()
        })
      })
    )
  
  if (show_progress) cat("\n")
  
  n_changed <- sum(normalized$journal_original != normalized$journal_iso4, na.rm = TRUE)
  cat("  Converted", n_changed, "journals to ISO4 (", 
      round(100 * n_changed / nrow(normalized), 1), "%)\n")
  
  # # Show examples
  # examples <- normalized %>%
  #   dplyr::filter(journal_original != journal_iso4) %>%
  #   head(5)
  
  # if (nrow(examples) > 0) {
  #   cat("  Examples:\n")
  #   for (i in 1:min(3, nrow(examples))) {
  #     cat("    ", examples$journal_original[i], "\n")
  #     cat("    -> ", examples$journal_iso4[i], "\n")
  #   }
  # }
  
  return(normalized)
}

#' Remove diacritics from string with robust fallback
#' @keywords internal
remove_diacritics <- function(x) {
  if (is.na(x) || x == "") return(x)
  
  # Try iconv first
  result <- tryCatch({
    temp <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    if (!is.na(temp)) {
      # Clean up artifacts that iconv might add (like " or ')
      temp <- str_remove_all(temp, '[\'\"`^~]')
      temp
    } else {
      NULL
    }
  }, error = function(e) {
    NULL
  })
  
  # If iconv fails or returns NA, use manual replacement
  if (is.null(result) || is.na(result)) {
    result <- x %>%
      # German umlauts
      str_replace_all("Ä", "AE") %>%
      str_replace_all("Ö", "OE") %>%
      str_replace_all("Ü", "UE") %>%
      str_replace_all("ä", "ae") %>%
      str_replace_all("ö", "oe") %>%
      str_replace_all("ü", "ue") %>%
      str_replace_all("ß", "ss") %>%
      # French/Spanish accents - vowels with accents
      str_replace_all("À|Á|Â|Ã|Å", "A") %>%
      str_replace_all("È|É|Ê|Ë", "E") %>%
      str_replace_all("Ì|Í|Î|Ï", "I") %>%
      str_replace_all("Ò|Ó|Ô|Õ", "O") %>%
      str_replace_all("Ù|Ú|Û", "U") %>%
      str_replace_all("Ý", "Y") %>%
      str_replace_all("à|á|â|ã|å", "a") %>%
      str_replace_all("è|é|ê|ë", "e") %>%
      str_replace_all("ì|í|î|ï", "i") %>%
      str_replace_all("ò|ó|ô|õ", "o") %>%
      str_replace_all("ù|ú|û", "u") %>%
      str_replace_all("ý|ÿ", "y") %>%
      # Spanish
      str_replace_all("Ñ", "N") %>%
      str_replace_all("ñ", "n") %>%
      # Czech, Polish, etc.
      str_replace_all("Č|Ć|Ç", "C") %>%
      str_replace_all("č|ć|ç", "c") %>%
      str_replace_all("Š", "S") %>%
      str_replace_all("š", "s") %>%
      str_replace_all("Ž", "Z") %>%
      str_replace_all("ž", "z") %>%
      str_replace_all("Ł", "L") %>%
      str_replace_all("ł", "l") %>%
      # Scandinavian
      str_replace_all("Ø", "O") %>%
      str_replace_all("ø", "o") %>%
      str_replace_all("Æ", "AE") %>%
      str_replace_all("æ", "ae") %>%
      str_replace_all("Å", "A") %>%
      str_replace_all("å", "a") %>%
      # Turkish
      str_replace_all("İ|Ī", "I") %>%
      str_replace_all("ı|ī", "i") %>%
      str_replace_all("Ş", "S") %>%
      str_replace_all("ş", "s") %>%
      str_replace_all("Ğ", "G") %>%
      str_replace_all("ğ", "g")
  }
  
  return(result)
}