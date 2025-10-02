
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
  # CITATION ANALYSIS METRICS
  # ===========================================
  
  citation_metrics <- list()
  
  if (nrow(all_citations) > 0) {
    citation_metrics$type_distribution <- all_citations %>%
      count(citation_type, sort = TRUE) %>%
      mutate(percentage = round(n / sum(n) * 100, 2))
    
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


# ===========================================
# ESEMPIO DI UTILIZZO
# ===========================================

# Usa la funzione con il tuo oggetto text:
# risultati <- analyze_scientific_content_enhanced(text = text)

# Visualizza diagnostiche di matching:
# print_matching_diagnostics(risultati)

# Accedi ai risultati:
# View(risultati$citation_references_mapping)
# View(risultati$parsed_references)
# View(risultati$citation_contexts)

# Filtra solo match ad alta confidenza:
# high_quality_matches <- risultati$citation_references_mapping %>%
#   filter(match_confidence %in% c("high", "medium_fuzzy"))

# Statistiche di matching:
# risultati$summary$match_quality

# Esporta i risultati:
# write_csv(risultati$citation_references_mapping, "citation_mapping.csv")
# write_csv(risultati$citation_contexts, "citation_contexts.csv")
# write_csv(risultati$parsed_references, "parsed_references.csv")