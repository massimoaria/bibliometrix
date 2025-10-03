
#' Enhanced Scientific Article Content Analysis with Comprehensive Citation Extraction
#'
#' @param text Either a character string OR a list with named elements
#' @param window_size Integer, number of words before and after citations (default: 10)
#' @param min_word_length Integer, minimum word length for analysis (default: 3)
#' @param remove_stopwords Logical, whether to remove stopwords (default: TRUE)
#' @param language Character, language for stopwords removal (default: "en")
#' @param custom_stopwords Character vector of additional stopwords
#' @param ngram_range Vector of integers, range for n-gram analysis (default: c(1,3))
#' @param parse_multiple_citations Logical, parse complex multiple citations (default: TRUE)
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
  
  results <- list()
  
  # ===========================================
  # HANDLE INPUT TYPE (STRING OR LIST)
  # ===========================================
  
  references_section <- NULL
  
  if (is.list(text)) {
    if ("Full_text" %in% names(text)) {
      clean_text <- text$Full_text
      sections_to_use <- NULL
    } else {
      sections_to_use <- setdiff(names(text), "References")
      clean_text <- paste(text[sections_to_use], collapse = " ")
    }
    
    if ("References" %in% names(text)) {
      references_section <- text$References
    }
  } else {
    clean_text <- text
  }
  
  # Clean and prepare text
  clean_text <- clean_text %>%
    stringr::str_replace_all("\\n+", " ") %>%
    stringr::str_replace_all("\\s+", " ") %>%
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
  
  citation_patterns <- list(
    complex_multiple_citations = "\\((?:see\\s+)?(?:e\\.g\\.\\s+)?[A-Z][^)]*(?:\\d{4}[a-z]?[;,][^)]*){2,}\\d{4}[a-z]?[^)]*\\)",
    
    # NARRATIVE CITATIONS - ordine dal più specifico al più generico
    narrative_four_authors_and = "[A-Z][A-Za-z'-]+,\\s*[A-Z][A-Za-z'-]+,\\s*[A-Z][A-Za-z'-]+,\\s*(?:and|&)\\s*[A-Z][A-Za-z'-]+\\s*\\(\\d{4}[a-z]?\\)",
    narrative_three_authors_and = "[A-Z][A-Za-z'-]+,\\s*[A-Z][A-Za-z'-]+,\\s*(?:and|&)\\s*[A-Z][A-Za-z'-]+\\s*\\(\\d{4}[a-z]?\\)",
    narrative_two_authors_and = "[A-Z][A-Za-z'-]+\\s+(?:and|&)\\s+[A-Z][A-Za-z'-]+\\s*\\(\\d{4}[a-z]?\\)",
    narrative_etal = "[A-Z][A-Za-z'-]+(?:\\s*,\\s*[A-Z][A-Za-z'-]+)*(?:\\s*,?\\s*(?:and|&)?\\s*et\\s+al\\.)?\\s*\\(\\d{4}[a-z]?\\)",
    narrative_multiple_authors = "[A-Z][A-Za-z'-]+(?:\\s*,\\s*[A-Z][A-Za-z'-]+)+(?:\\s*,\\s*&\\s*[A-Z][A-Za-z'-]+)?\\s*\\(\\d{4}[a-z]?\\)",
    narrative_single = "[A-Z][A-Za-z'-]+\\s*\\(\\d{4}[a-z]?\\)",
    
    # PARENTHETICAL CITATIONS
    multiple_citations_semicolon = "\\([A-Z][A-Za-z'-]+[^)]*\\d{4}[a-z]?(?:\\s*;\\s*[A-Z][^)]*\\d{4}[a-z]?)+[^)]*\\)",
    see_citations = "\\((?:see|e\\.g\\.|cf\\.|compare)\\s+[A-Z][A-Za-z'-]+[^)]+\\d{4}[a-z]?\\)",
    author_year_etal = "\\([A-Z][A-Za-z'-]+\\s+et\\s+al\\.,\\s*\\d{4}[a-z]?\\)",
    author_year_and = "\\([A-Z][A-Za-z'-]+(?:,\\s*[A-Z][A-Za-z'-]+)*,?\\s+(?:and|&)\\s+[A-Z][A-Za-z'-]+,\\s*\\d{4}[a-z]?\\)",
    author_year_ampersand = "\\([A-Z][A-Za-z'-]+[^)]*&[^)]*\\d{4}[a-z]?\\)",
    author_year_basic = "\\([A-Z][A-Za-z'-]+(?:\\s+[A-Z][A-Za-z'-]+)*,\\s*\\d{4}[a-z]?\\)",
    
    # OTHER FORMATS
    numbered_simple = "\\[\\d+\\]",
    numbered_multiple = "\\[\\d+(?:[-,;\\s]+\\d+)*\\]",
    superscript = "[¹²³⁴⁵⁶⁷⁸⁹⁰]+",
    doi_pattern = "https?://doi\\.org/[\\w\\./\\-]+"
  )
  
  # Extract all citations
  all_citations <- tibble()
  
  for (pattern_name in names(citation_patterns)) {
    pattern <- citation_patterns[[pattern_name]]
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
  
  # Remove duplicates and overlapping citations - ALGORITMO MIGLIORATO
  all_citations <- all_citations %>%
    arrange(start_pos, desc(nchar(citation_text)))
  
  if (nrow(all_citations) > 1) {
    to_remove <- c()
    
    for (i in 1:(nrow(all_citations) - 1)) {
      if (i %in% to_remove) next
      
      for (j in (i + 1):nrow(all_citations)) {
        if (j %in% to_remove) next
        
        cite_i_start <- all_citations$start_pos[i]
        cite_i_end <- all_citations$end_pos[i]
        cite_j_start <- all_citations$start_pos[j]
        cite_j_end <- all_citations$end_pos[j]
        
        # j contenuta in i
        if (cite_j_start >= cite_i_start && cite_j_end <= cite_i_end) {
          to_remove <- c(to_remove, j)
        }
        # i contenuta in j
        else if (cite_i_start >= cite_j_start && cite_i_end <= cite_j_end) {
          to_remove <- c(to_remove, i)
          break
        }
        # Sovrapposizione parziale
        else if (cite_j_start < cite_i_end && cite_j_start > cite_i_start) {
          len_i <- cite_i_end - cite_i_start
          len_j <- cite_j_end - cite_j_start
          if (len_j < len_i) {
            to_remove <- c(to_remove, j)
          } else {
            to_remove <- c(to_remove, i)
            break
          }
        }
      }
    }
    
    if (length(to_remove) > 0) {
      all_citations <- all_citations[-unique(to_remove), ]
    }
  }
  
  all_citations <- all_citations %>% arrange(start_pos)
  
  # ===========================================
  # POST-PROCESSING: PARSE COMPLEX CITATIONS
  # ===========================================
  
  parsed_citations <- tibble()
  
  if (parse_multiple_citations && nrow(all_citations) > 0) {
    for (i in 1:nrow(all_citations)) {
      citation <- all_citations[i,]
      
      if (citation$citation_type == "complex_multiple_citations") {
        inner_text <- str_replace(citation$citation_text, "^\\((?:see\\s+)?(?:e\\.g\\.\\s+)?", "")
        inner_text <- str_replace(inner_text, "\\)$", "")
        individual_citations <- str_split(inner_text, "\\s*;\\s*")[[1]]
        
        for (j in seq_along(individual_citations)) {
          indiv_cite <- stringr::str_trim(individual_citations[j])
          if (str_detect(indiv_cite, "\\d{4}")) {
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
        citation$original_complex_citation <- NA
        parsed_citations <- bind_rows(parsed_citations, citation)
      }
    }
    all_citations <- parsed_citations
  }
  
  # ===========================================
  # ENHANCED NARRATIVE CITATION PROCESSING
  # ===========================================
  
  if (nrow(all_citations) > 0) {
    narrative_citations <- all_citations %>%
      filter(str_detect(citation_type, "narrative")) %>%
      mutate(
        author_part = str_extract(citation_text, "^[^(]+"),
        year_part = str_extract(citation_text, "\\(\\d{4}[a-z]?\\)"),
        standardized_citation = paste0("(", stringr::str_trim(author_part), ", ", 
                                       stringr::str_replace_all(year_part, "[()]", ""), ")")
      )
    
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
  # WORD FREQUENCY ANALYSIS
  # ===========================================
  
  tokens <- tibble(text = clean_text) %>%
    unnest_tokens(word, text) %>%
    filter(str_length(word) >= min_word_length) %>%
    filter(!str_detect(word, "^\\d+$"))
  
  if (remove_stopwords) {
    data(stop_words)
    tokens <- tokens %>%
      anti_join(stop_words, by = "word")
    
    if (!is.null(custom_stopwords)) {
      custom_stops <- tibble(word = custom_stopwords)
      tokens <- tokens %>%
        anti_join(custom_stops, by = "word")
    }
  }
  
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
  
  clean_text_filtered <- clean_text %>%
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
  # CITATION SECTION MAPPING
  # ===========================================
  
  citation_sections <- tibble()
  
  if (is.list(text) && length(text) > 1) {
    section_names <- setdiff(names(text), c("Full_text", "References"))
    
    if (length(section_names) > 0 && nrow(all_citations) > 0) {
      # Calcola posizioni delle sezioni nel testo completo
      section_positions <- list()
      cumulative_pos <- 1
      
      for (sect_name in section_names) {
        sect_text <- text[[sect_name]]
        sect_length <- nchar(sect_text)
        
        section_positions[[sect_name]] <- list(
          start = cumulative_pos,
          end = cumulative_pos + sect_length - 1,
          name = sect_name
        )
        
        cumulative_pos <- cumulative_pos + sect_length + 1  # +1 per lo spazio
      }
      
      # Mappa ogni citazione alla sua sezione
      for (i in 1:nrow(all_citations)) {
        cite_pos <- all_citations$start_pos[i]
        
        mapped_section <- "Unknown"
        for (sect_name in section_names) {
          sect_info <- section_positions[[sect_name]]
          if (cite_pos >= sect_info$start && cite_pos <= sect_info$end) {
            mapped_section <- sect_name
            break
          }
        }
        
        citation_sections <- bind_rows(
          citation_sections,
          tibble(
            citation_id = all_citations$citation_id[i],
            section = mapped_section
          )
        )
      }
      
      # Aggiungi la sezione ai dati delle citazioni
      all_citations <- all_citations %>%
        left_join(citation_sections, by = "citation_id")
    } else {
      all_citations$section <- "Full_text"
    }
  } else {
    all_citations$section <- "Full_text"
  }
  
  # ===========================================
  # CITATION ANALYSIS METRICS
  # ===========================================
  
  citation_metrics <- list()
  
  if (nrow(all_citations) > 0) {
    citation_metrics$type_distribution <- all_citations %>%
      count(citation_type, sort = TRUE) %>%
      mutate(percentage = round(n / sum(n) * 100, 2))
    
    if (!is.null(sections_to_use)){
      citation_metrics$section_distribution <- all_citations %>%
        mutate(section = factor(section, levels = sections_to_use)) %>%
        count(section, sort = FALSE, .drop = FALSE) %>%
        mutate(percentage = round(n / sum(n) * 100, 2))
    } else {
      citation_metrics$section_distribution <- all_citations %>%
        count(section, sort = TRUE) %>%
        mutate(percentage = round(n / sum(n) * 100, 2))
    }
    
    
    citation_metrics$narrative_ratio <- all_citations %>%
      summarise(
        total_citations = n(),
        narrative_citations = sum(str_detect(citation_type, "narrative")),
        parenthetical_citations = sum(!str_detect(citation_type, "narrative")),
        narrative_percentage = round(narrative_citations / total_citations * 100, 2)
      )
    
    if (parse_multiple_citations) {
      citation_metrics$complex_citations <- all_citations %>%
        filter(citation_type == "parsed_from_multiple") %>%
        count(original_complex_citation, sort = TRUE) %>%
        rename(individual_citations_extracted = n)
    }
    
    citation_metrics$density <- list(
      citations_per_1000_words = round((nrow(all_citations) / text_stats$total_words) * 1000, 2),
      avg_words_between_citations = if(nrow(all_citations) > 1) {
        round(text_stats$total_words / nrow(all_citations), 2)
      } else {
        text_stats$total_words
      }
    )
  }
  
  # ===========================================
  # CITATION CO-OCCURRENCE NETWORK DATA
  # ===========================================
  
  citation_cooccurrence <- NULL
  
  if (nrow(all_citations) > 1) {
    citation_pairs <- tibble()
    
    for (i in 1:(nrow(all_citations)-1)) {
      for (j in (i+1):nrow(all_citations)) {
        distance <- all_citations$start_pos[j] - all_citations$end_pos[i]
        if (distance <= 1000) {
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
  # CITATION CONTEXT EXTRACTION (IMPROVED)
  # ===========================================
  
  citation_contexts <- tibble()
  
  if (nrow(all_citations) > 0) {
    for (i in 1:nrow(all_citations)) {
      citation <- all_citations[i,]
      citation_start <- citation$start_pos
      citation_end <- citation$end_pos
      
      text_before_citation <- substr(clean_text, 1, citation_start - 1)
      text_after_citation <- substr(clean_text, citation_end + 1, nchar(clean_text))
      
      # Extract words BEFORE
      words_before_df <- tibble(text = text_before_citation) %>%
        unnest_tokens(word, text, token = "words", to_lower = FALSE)
      
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
      
      # Extract words AFTER - IMPROVED VERSION
      # Find all citations that start after the current citation ends
      next_citations <- all_citations %>%
        filter(start_pos > citation_end) %>%
        arrange(start_pos)
      
      if (nrow(next_citations) > 0) {
        next_citation_start <- next_citations$start_pos[1]
        max_end_pos <- next_citation_start - citation_end - 1
        
        if (max_end_pos > 0) {
          text_after_limited <- substr(text_after_citation, 1, max_end_pos)
        } else {
          text_after_limited <- ""
        }
      } else {
        text_after_limited <- text_after_citation
      }
      
      words_after_df <- tibble(text = text_after_limited) %>%
        unnest_tokens(word, text, token = "words", to_lower = FALSE)
      
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
      
      full_context <- paste(words_before, citation$citation_text, words_after) %>% 
        stringr::str_trim()
      
      context_word_count <- n_words_before + n_words_after + 
        length(strsplit(citation$citation_text, "\\s+")[[1]])
      
      context_row <- tibble(
        citation_id = citation$citation_id,
        citation_text = citation$citation_text,
        citation_text_clean = citation$citation_text_clean,
        citation_type = citation$citation_type,
        section = citation$section,
        words_before = words_before,
        words_after = words_after,
        full_context = full_context,
        context_word_count = context_word_count,
        citation_position_in_text = citation$start_pos,
        is_narrative = str_detect(citation$citation_type, "narrative"),
        is_parsed_multiple = citation$citation_type == "parsed_from_multiple"
      )
      
      if ("original_complex_citation" %in% names(citation) && 
          !is.na(citation$original_complex_citation)) {
        context_row$original_complex_citation <- citation$original_complex_citation
      }
      
      citation_contexts <- bind_rows(citation_contexts, context_row)
    }
  }
  
  # ===========================================
  # CITATION-REFERENCE MAPPING
  # ===========================================
  
  citation_references_mapping <- NULL
  parsed_references <- NULL
  
  if (!is.null(references_section) && length(references_section) > 0 && 
      !all(is.na(references_section)) && references_section != "") {
    
    parsed_references <- parse_references_section(references_section)
    
    if (nrow(all_citations) > 0 && nrow(parsed_references) > 0) {
      citation_references_mapping <- match_citations_to_references(
        citations_df = all_citations %>%
          select(citation_id, citation_text, citation_text_clean, citation_type),
        references_df = parsed_references
      )
    }
  }
  
  # ===========================================
  # ADD REF_FULL_TEXT TO CITATION_CONTEXTS
  # ===========================================
  
  if (!is.null(citation_references_mapping) && nrow(citation_contexts) > 0) {
    citation_contexts <- citation_contexts %>%
      left_join(
        citation_references_mapping %>%
          select(citation_id, matched_ref_id, ref_full_text, match_confidence),
        by = "citation_id"
      )
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
  results$citation_references_mapping <- citation_references_mapping
  results$parsed_references <- parsed_references
  results$word_frequencies <- word_frequencies
  results$ngrams <- ngrams_results
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
    citation_density_per_1000_words = if(nrow(all_citations) > 0) round((nrow(all_citations) / text_stats$total_words) * 1000, 2) else 0,
    references_parsed = if (!is.null(parsed_references)) nrow(parsed_references) else 0,
    citations_matched_to_refs = if (!is.null(citation_references_mapping)) {
      sum(citation_references_mapping$match_confidence %in% c("high", "medium_multiple_matches", "medium_year_only", "medium_fuzzy"), na.rm = TRUE)
    } else {
      0
    },
    match_quality = if (!is.null(citation_references_mapping)) {
      citation_references_mapping %>%
        count(match_confidence) %>%
        mutate(percentage = round(n / sum(n) * 100, 1))
    } else {
      NULL
    }
  )
  
  class(results) <- c("enhanced_scientific_content_analysis", "list")
  return(results)
}


# ===========================================
# FUNZIONE DI UTILITÀ PER DIAGNOSTICA
# ===========================================

#' Print diagnostic information about citation-reference matching
#'
#' @param results Output from analyze_scientific_content_enhanced
#' @export
print_matching_diagnostics <- function(results) {
  
  cat("\n=== CITATION-REFERENCE MATCHING DIAGNOSTICS ===\n\n")
  
  if (is.null(results$citation_references_mapping)) {
    cat("No citation-reference mapping performed (missing References section)\n")
    return(invisible(NULL))
  }
  
  mapping <- results$citation_references_mapping
  
  cat("Total citations:", nrow(mapping), "\n")
  cat("Total references parsed:", nrow(results$parsed_references), "\n\n")
  
  cat("Match quality distribution:\n")
  match_summary <- mapping %>%
    count(match_confidence) %>%
    arrange(desc(n))
  print(match_summary)
  
  cat("\n")
  cat("Match rate:", 
      round(sum(!is.na(mapping$matched_ref_id)) / nrow(mapping) * 100, 1), 
      "%\n")
  
  cat("\nHigh confidence matches:", 
      sum(mapping$match_confidence == "high", na.rm = TRUE), "\n")
  
  cat("\nCitations without matches:\n")
  unmatched <- mapping %>%
    filter(is.na(matched_ref_id)) %>%
    select(citation_text_clean, cite_author, cite_year, match_confidence)
  
  if (nrow(unmatched) > 0) {
    print(head(unmatched, 10))
    if (nrow(unmatched) > 10) {
      cat("... and", nrow(unmatched) - 10, "more\n")
    }
  } else {
    cat("All citations matched!\n")
  }
  
  invisible(match_summary)
}


#' Calculate word distribution across text segments or sections
#'
#' @param text Character string or list with text sections
#' @param selected_words Character vector of words/ngrams to track
#' @param use_sections Logical or "auto". If TRUE, use document sections (if available).
#'                     If FALSE, create equal-length segments. If "auto", use sections 
#'                     when available, otherwise segments. (default: "auto")
#' @param n_segments Integer, number of segments if use_sections=FALSE (default: 10)
#' @param remove_stopwords Logical, whether to remove stopwords (default: TRUE)
#' @param language Character, language for stopwords (default: "en")
#'
#' @return A tibble with word frequencies by segment/section
calculate_word_distribution <- function(text,
                                        selected_words,
                                        use_sections = "auto",
                                        n_segments = 10,
                                        remove_stopwords = TRUE,
                                        language = "en") {
  
  require(tidyverse)
  require(tidytext)
  
  # Normalize selected words to lowercase
  selected_words <- tolower(selected_words)
  
  # Determine maximum n-gram size needed
  max_ngram <- max(sapply(selected_words, function(w) {
    length(strsplit(w, "\\s+")[[1]])
  }))
  
  # ===========================================
  # CHECK IF SECTIONS ARE AVAILABLE
  # ===========================================
  
  sections_available <- FALSE
  section_names <- character(0)
  
  if (is.list(text)) {
    section_names <- setdiff(names(text), c("Full_text", "References"))
    sections_available <- length(section_names) > 0
  }
  
  # ===========================================
  # DETERMINE WHAT TO USE: SECTIONS OR SEGMENTS
  # ===========================================
  
  use_sections_final <- FALSE
  
  if (use_sections == "auto") {
    use_sections_final <- sections_available
  } else if (is.logical(use_sections)) {
    if (use_sections && !sections_available) {
      warning("Sections requested but not available. Using equal-length segments instead.")
      use_sections_final <- FALSE
    } else {
      use_sections_final <- use_sections
    }
  } else {
    stop("use_sections must be TRUE, FALSE, or 'auto'")
  }
  
  # ===========================================
  # PREPARE SEGMENT TEXTS
  # ===========================================
  
  segment_texts <- list()
  segment_info <- tibble()
  
  if (use_sections_final) {
    # ===== USE DOCUMENT SECTIONS =====
    for (i in seq_along(section_names)) {
      section_name <- section_names[i]
      section_text <- text[[section_name]]
      
      if (!is.null(section_text) && nchar(section_text) > 0) {
        segment_texts[[i]] <- section_text
        segment_info <- bind_rows(
          segment_info,
          tibble(
            segment_id = i,
            segment_name = section_name,
            segment_type = "section"
          )
        )
      }
    }
  } else {
    # ===== CREATE EQUAL-LENGTH SEGMENTS =====
    
    # Extract full text
    if (is.list(text)) {
      full_text <- text$Full_text
    } else {
      full_text <- text
    }
    
    # Clean text
    full_text <- full_text %>%
      str_replace_all("\\n+", " ") %>%
      str_replace_all("\\s+", " ") %>%
      str_trim()
    
    # Calculate segment boundaries based on character positions
    total_chars <- nchar(full_text)
    chars_per_segment <- ceiling(total_chars / n_segments)
    
    # Create segments by character position
    for (i in 1:n_segments) {
      start_pos <- (i - 1) * chars_per_segment + 1
      end_pos <- min(i * chars_per_segment, total_chars)
      segment_texts[[i]] <- substr(full_text, start_pos, end_pos)
      segment_info <- bind_rows(
        segment_info,
        tibble(
          segment_id = i,
          segment_name = paste0("Segment ", i),
          segment_type = "equal_length"
        )
      )
    }
  }
  
  # ===========================================
  # PROCESS EACH SEGMENT: REMOVE STOPWORDS AND CREATE N-GRAMS
  # ===========================================
  
  if (remove_stopwords) {
    data(stop_words)
  }
  
  all_ngrams <- tibble()
  
  for (i in seq_along(segment_texts)) {
    segment_text <- segment_texts[[i]]
    
    # Step 1: Tokenize into words
    tokens <- tibble(text = segment_text) %>%
      unnest_tokens(word, text, token = "words")
    
    # Step 2: Remove stopwords if requested
    if (remove_stopwords) {
      tokens <- tokens %>%
        anti_join(stop_words, by = "word")
    }
    
    # Step 3: Reconstruct text without stopwords
    clean_text <- paste(tokens$word, collapse = " ")
    
    # Step 4: Generate n-grams from 1 to max_ngram
    for (n in 1:max_ngram) {
      ngrams <- tibble(text = clean_text) %>%
        unnest_tokens(word, text, token = "ngrams", n = n) %>%
        filter(!is.na(word)) %>%
        mutate(
          segment_id = segment_info$segment_id[i],
          segment_name = segment_info$segment_name[i],
          segment_type = segment_info$segment_type[i],
          ngram_size = n
        )
      
      all_ngrams <- bind_rows(all_ngrams, ngrams)
    }
  }
  
  # ===========================================
  # CALCULATE FREQUENCIES FOR SELECTED WORDS
  # ===========================================
  
  # Filter only selected words/ngrams
  word_counts <- all_ngrams %>%
    filter(word %in% selected_words) %>%
    count(segment_id, segment_name, segment_type, word, name = "count")
  
  # Calculate total unigrams per segment for relative frequency
  total_per_segment <- all_ngrams %>%
    filter(ngram_size == 1) %>%
    count(segment_id, segment_name, segment_type, name = "total_words")
  
  # Join and calculate relative frequency
  result <- word_counts %>%
    left_join(total_per_segment, by = c("segment_id", "segment_name", "segment_type")) %>%
    mutate(
      relative_frequency = count / total_words,
      percentage = relative_frequency * 100
    ) %>%
    arrange(segment_id, word)
  
  # Add metadata as attributes
  attr(result, "use_sections") <- use_sections_final
  attr(result, "sections_available") <- sections_available
  attr(result, "n_segments") <- length(unique(result$segment_id))
  attr(result, "selected_words") <- selected_words
  attr(result, "segment_type") <- if (use_sections_final) "section" else "equal_length"
  
  return(result)
}

#' Create interactive word distribution plot
#'
#' @param word_distribution_data Tibble returned by calculate_word_distribution()
#' @param plot_type Character, type of plot: "line" or "area" (default: "line")
#' @param smooth Logical, whether to apply smoothing to lines (default: FALSE)
#' @param show_points Logical, whether to show data points on lines (default: TRUE)
#' @param colors Character vector of colors for words (optional)
#'
#' @return A plotly object
plot_word_distribution <- function(word_distribution_data,
                                   plot_type = "line",
                                   smooth = FALSE,
                                   show_points = TRUE,
                                   colors = NULL) {
  
  # Extract metadata
  use_sections <- attr(word_distribution_data, "use_sections")
  segment_type <- attr(word_distribution_data, "segment_type")
  selected_words <- attr(word_distribution_data, "selected_words")
  
  # Prepare data - ensure all words appear in all segments (with 0 if absent)
  all_segments <- unique(word_distribution_data$segment_id)
  all_words <- unique(word_distribution_data$word)
  
  complete_data <- expand.grid(
    segment_id = all_segments,
    word = all_words,
    stringsAsFactors = FALSE
  ) %>%
    left_join(
      word_distribution_data %>% 
        select(segment_id, segment_name, word, relative_frequency, count),
      by = c("segment_id", "word")
    ) %>%
    mutate(
      relative_frequency = replace_na(relative_frequency, 0),
      count = replace_na(count, 0)
    )
  
  # Get segment names for x-axis
  segment_labels <- word_distribution_data %>%
    distinct(segment_id, segment_name) %>%
    arrange(segment_id)
  
  complete_data <- complete_data %>%
    left_join(segment_labels, by = "segment_id") %>%
    mutate(
      segment_name = coalesce(segment_name.x, segment_name.y)
    ) %>%
    select(-segment_name.x, -segment_name.y)
  
  # Set colors
  n_words <- length(all_words)
  if (is.null(colors)) {
    # Use a color palette
    if (n_words <= 8) {
      colors <- RColorBrewer::brewer.pal(max(3, n_words), "Set2")[1:n_words]
    } else {
      # Generate colors
      colors <- scales::hue_pal()(n_words)
    }
  }
  
  # Create color mapping
  color_mapping <- setNames(colors, all_words)
  
  # Create plotly figure
  fig <- plot_ly()
  
  # Add traces for each word
  for (i in seq_along(all_words)) {
    word_i <- all_words[i]
    word_data <- complete_data %>% 
      filter(word == word_i) %>%
      arrange(segment_id)
    
    # Prepare hover text
    hover_text <- paste0(
      "<b>Word:</b> ", word_i, "<br>",
      "<b>", if (use_sections) "Section" else "Segment", ":</b> ", 
      word_data$segment_name, "<br>",
      "<b>Frequency:</b> ", round(word_data$relative_frequency * 100, 3), "%<br>",
      "<b>Count:</b> ", word_data$count
    )
    
    if (plot_type == "area") {
      # Add area trace
      fig <- fig %>%
        add_trace(
          data = word_data,
          x = ~segment_id,
          y = ~relative_frequency,
          type = 'scatter',
          mode = if (show_points) 'lines+markers' else 'lines',
          fill = 'tozeroy',
          fillcolor = paste0(color_mapping[word_i], "4D"),  # Add transparency
          name = word_i,
          line = list(color = color_mapping[word_i], width = 2),
          marker = if (show_points) list(size = 8, color = color_mapping[word_i]) else NULL,
          text = hover_text,
          hovertemplate = "%{text}<extra></extra>"
        )
    } else {
      # Add line trace
      fig <- fig %>%
        add_trace(
          data = word_data,
          x = ~segment_id,
          y = ~relative_frequency,
          type = 'scatter',
          mode = if (show_points) 'lines+markers' else 'lines',
          name = word_i,
          line = list(
            color = color_mapping[word_i], 
            width = 2.5,
            shape = if (smooth) 'spline' else 'linear'
          ),
          marker = if (show_points) list(
            size = 8, 
            color = color_mapping[word_i],
            line = list(color = 'white', width = 1)
          ) else NULL,
          text = hover_text,
          hovertemplate = "%{text}<extra></extra>"
        )
    }
  }
  
  # Prepare x-axis labels
  x_labels <- if (use_sections) {
    str_trunc(segment_labels$segment_name, 20)
  } else {
    as.character(segment_labels$segment_id)
  }
  
  # Configure layout
  fig <- fig %>%
    layout(
      title = list(
        text = paste0(
          "<b>Word Distribution Across Document</b><br>",
          "<sub>", 
          if (use_sections) "By Document Section" else paste0("By Segment (", length(all_segments), " equal parts)"),
          " | Words: ", paste(selected_words, collapse = ", "),
          "</sub>"
        ),
        font = list(size = 16, color = "#2E86AB")
      ),
      xaxis = list(
        title = list(
          text = if (use_sections) "Document Section" else "Document Segments (chronological)",
          font = list(size = 12, family = "Arial, sans-serif", color = "#333")
        ),
        tickmode = "array",
        tickvals = segment_labels$segment_id,
        ticktext = x_labels,
        tickangle = if (use_sections) -45 else 0,
        gridcolor = "#e0e0e0",
        gridwidth = 1,
        showline = TRUE,
        linecolor = "#cccccc"
      ),
      yaxis = list(
        title = list(
          text = "Relative Frequency",
          font = list(size = 12, family = "Arial, sans-serif", color = "#333")
        ),
        tickformat = ".2%",
        gridcolor = "#e0e0e0",
        gridwidth = 1,
        showline = TRUE,
        linecolor = "#cccccc"
      ),
      hovermode = "closest",
      legend = list(
        title = list(text = "<b>Words</b>"),
        orientation = "v",
        x = 1.02,
        y = 1,
        xanchor = "left",
        bgcolor = "rgba(255, 255, 255, 0.9)",
        bordercolor = "#cccccc",
        borderwidth = 1,
        font = list(size = 11)
      ),
      plot_bgcolor = "#fafafa",
      paper_bgcolor = "white",
      margin = list(l = 60, r = 120, t = 80, b = 80)
    ) %>%
    config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d"),
      toImageButtonOptions = list(
        format = "png",
        filename = "word_distribution",
        height = 600,
        width = 1000,
        scale = 2
      )
    )
  
  return(fig)
}