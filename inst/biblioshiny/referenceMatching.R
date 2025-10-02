#' Normalizza il testo References rimuovendo \n\n tra autori della stessa voce
normalize_references_text <- function(references_text) {
  
  if (is.null(references_text) || length(references_text) == 0 || 
      all(is.na(references_text)) || all(references_text == "")) {
    return("")
  }
  
  normalized <- references_text
  
  # Strategia: rimuovi \n\n SOLO se non è preceduto da un punto seguito da numero/lettera maiuscola
  # (che indicherebbe la fine di una voce bibliografica)
  
  # Pattern 1: \n\n tra autori - rimuovi se segue virgola e precede cognome con iniziale
  # Es: "Breiman, L.,\n\nFriedman, J. H." -> "Breiman, L., Friedman, J. H."
  # Ma NON "...166.\n\nChipman" perché il punto indica fine voce
  normalized <- stringr::str_replace_all(
    normalized,
    "(?<!\\.)\\s*,\\s*\\n\\n(?=[A-Z][a-z]+,\\s*[A-Z])",
    ", "
  )
  
  # Pattern 2: \n\n dopo iniziale puntata, prima di cognome
  # Es: "J. H.,\n\nOlshen" -> "J. H., Olshen"
  normalized <- stringr::str_replace_all(
    normalized,
    "(?<=[A-Z]\\.)\\s*,\\s*\\n\\n(?=[A-Z][a-z]+,)",
    ", "
  )
  
  # Pattern 3: \n\n dopo & prima di cognome (ultimo autore)
  # Es: "& \n\nStone, C." -> "& Stone, C."
  normalized <- stringr::str_replace_all(
    normalized,
    "(?<=&)\\s*\\n\\n(?=[A-Z][a-z]+,)",
    " "
  )
  
  # Pattern 4: \n\n tra iniziali dello stesso autore
  # Es: "Friedman, J.\n\nH." -> "Friedman, J. H."
  normalized <- stringr::str_replace_all(
    normalized,
    "(?<=[A-Z]\\.)\\s*\\n\\n(?=[A-Z]\\.)",
    " "
  )
  
  return(normalized)
}

#' Parse references - VERSIONE STABILE
parse_references_section <- function(references_text) {
  
  if (is.null(references_text) || length(references_text) == 0 || 
      all(is.na(references_text)) || all(references_text == "")) {
    return(tibble(
      ref_id = character(),
      ref_full_text = character(),
      ref_authors = character(),
      ref_year = character(),
      ref_first_author = character(),
      ref_first_author_normalized = character(),
      ref_second_author = character(),
      ref_second_author_normalized = character(),
      n_authors = integer()
    ))
  }
  
  # Normalizza il testo
  normalized_text <- normalize_references_text(references_text)
  
  # Split su \n\n
  individual_refs <- stringr::str_split(normalized_text, "\\n\\n+")[[1]]
  individual_refs <- individual_refs[stringr::str_trim(individual_refs) != ""]
  
  if (length(individual_refs) == 0) {
    return(tibble(
      ref_id = character(),
      ref_full_text = character(),
      ref_authors = character(),
      ref_year = character(),
      ref_first_author = character(),
      ref_first_author_normalized = character(),
      ref_second_author = character(),
      ref_second_author_normalized = character(),
      n_authors = integer()
    ))
  }
  
  # Parse ogni voce
  parsed_refs <- tibble(
    ref_id = paste0("REF_", seq_along(individual_refs)),
    ref_full_text = individual_refs %>%
      stringr::str_replace_all("\\s+", " ") %>%
      stringr::str_trim()
  ) %>%
    mutate(
      # Estrai anno
      ref_year = stringr::str_extract(ref_full_text, "\\(\\d{4}[a-z]?\\)"),
      ref_year = stringr::str_replace_all(ref_year, "[()]", ""),
      
      # Estrai sezione autori (tutto prima dell'anno)
      authors_section = stringr::str_extract(ref_full_text, "^[^(]*?(?=\\(\\d{4})"),
      authors_section = ifelse(
        is.na(authors_section), 
        stringr::str_extract(ref_full_text, "^[^.]{0,200}"), 
        authors_section
      ),
      authors_section = stringr::str_trim(authors_section),
      
      # Conta numero autori (conta virgole tra autori)
      n_authors = stringr::str_count(authors_section, ",\\s*[A-Z]") + 1,
      n_authors = ifelse(stringr::str_detect(authors_section, "et\\s+al"), 99, n_authors),
      
      # Primo autore (tutto prima della prima virgola)
      ref_first_author = stringr::str_extract(authors_section, "^[^,]+"),
      ref_first_author = stringr::str_trim(ref_first_author),
      ref_first_author_normalized = stringr::str_extract(ref_first_author, "[A-Za-z'-]+"),
      ref_first_author_normalized = stringr::str_to_lower(ref_first_author_normalized),
      
      # Secondo autore
      ref_second_author = stringr::str_extract(
        authors_section, 
        "(?<=,\\s)[A-Z][a-z]+(?:-[A-Z][a-z]+)?"
      ),
      ref_second_author_normalized = stringr::str_to_lower(ref_second_author),
      
      # Stringa autori completa
      ref_authors = authors_section
    ) %>%
    select(-authors_section)
  
  return(parsed_refs)
}


#' Match citations to references - VERSIONE STABILE CON PICCOLI MIGLIORAMENTI
match_citations_to_references <- function(citations_df, references_df) {
  
  if (nrow(citations_df) == 0 || nrow(references_df) == 0) {
    return(tibble(
      citation_id = character(),
      citation_text = character(),
      citation_text_clean = character(),
      citation_type = character(),
      matched_ref_id = character(),
      ref_full_text = character(),
      match_confidence = character(),
      ref_authors = character(),
      ref_year = character()
    ))
  }
  
  # Estrai info citazioni
  extract_citation_info <- function(citation_text) {
    clean_cite <- stringr::str_replace_all(citation_text, "^\\(|\\)$", "")
    
    year <- stringr::str_extract(clean_cite, "\\d{4}[a-z]?")
    has_etal <- stringr::str_detect(clean_cite, "et\\s+al\\.")
    has_and <- stringr::str_detect(clean_cite, "\\s+and\\s+|\\s+&\\s+")
    
    # Primo autore
    author <- stringr::str_extract(clean_cite, "^[A-Za-z'-]+")
    author_normalized <- stringr::str_to_lower(author)
    
    # Secondo autore (solo se c'è "and" o "&" e non "et al.")
    second_author <- NA_character_
    if (has_and && !has_etal) {
      second_author <- stringr::str_extract(clean_cite, "(?:and|&)\\s+([A-Za-z'-]+)")
      second_author <- stringr::str_replace(second_author, "^(?:and|&)\\s+", "")
    }
    second_author_normalized <- stringr::str_to_lower(second_author)
    
    # Numero autori approssimativo
    n_cite_authors <- if (has_etal) {
      99
    } else if (has_and) {
      stringr::str_count(clean_cite, ",") + 2
    } else {
      1
    }
    
    list(
      author = author, 
      author_normalized = author_normalized,
      second_author = second_author,
      second_author_normalized = second_author_normalized,
      year = year,
      has_etal = has_etal,
      has_and = has_and,
      n_authors = n_cite_authors
    )
  }
  
  citations_info <- citations_df %>%
    rowwise() %>%
    mutate(
      cite_info = list(extract_citation_info(citation_text_clean)),
      cite_author = cite_info$author,
      cite_author_normalized = cite_info$author_normalized,
      cite_second_author = cite_info$second_author,
      cite_second_author_normalized = cite_info$second_author_normalized,
      cite_year = cite_info$year,
      cite_has_etal = cite_info$has_etal,
      cite_has_and = cite_info$has_and,
      cite_n_authors = cite_info$n_authors
    ) %>%
    select(-cite_info) %>%
    ungroup()
  
  # MATCHING ALGORITHM
  matched_citations <- tibble()
  
  for (i in 1:nrow(citations_info)) {
    cite <- citations_info[i, ]
    
    # Skip se mancano info essenziali
    if (is.na(cite$cite_author_normalized) || is.na(cite$cite_year)) {
      matched_row <- cite %>%
        mutate(
          matched_ref_id = NA_character_,
          ref_full_text = NA_character_,
          match_confidence = "no_match_missing_info",
          ref_authors = NA_character_,
          ref_year = NA_character_
        )
      matched_citations <- bind_rows(matched_citations, matched_row)
      next
    }
    
    # STEP 1: Filtra per anno
    year_matches <- references_df %>%
      filter(!is.na(ref_year), ref_year == cite$cite_year)
    
    if (nrow(year_matches) == 0) {
      matched_row <- cite %>%
        mutate(
          matched_ref_id = NA_character_,
          ref_full_text = NA_character_,
          match_confidence = "no_match_year",
          ref_authors = NA_character_,
          ref_year = NA_character_
        )
      matched_citations <- bind_rows(matched_citations, matched_row)
      next
    }
    
    # STEP 2: Match ESATTO primo autore
    author_matches <- year_matches %>%
      filter(
        !is.na(ref_first_author_normalized),
        ref_first_author_normalized == cite$cite_author_normalized
      )
    
    # STEP 2b: Se non trova match esatto, prova fuzzy SOLO come fallback
    if (nrow(author_matches) == 0) {
      fuzzy_matches <- year_matches %>%
        filter(
          !is.na(ref_first_author_normalized),
          stringr::str_detect(ref_first_author_normalized, cite$cite_author_normalized) |
            stringr::str_detect(cite$cite_author_normalized, ref_first_author_normalized)
        )
      
      if (nrow(fuzzy_matches) > 0) {
        author_matches <- fuzzy_matches
      } else {
        matched_row <- cite %>%
          mutate(
            matched_ref_id = NA_character_,
            ref_full_text = NA_character_,
            match_confidence = "no_match_author",
            ref_authors = NA_character_,
            ref_year = NA_character_
          )
        matched_citations <- bind_rows(matched_citations, matched_row)
        next
      }
    }
    
    # STEP 3: Disambiguazione
    final_match <- NULL
    confidence <- "high"
    
    if (nrow(author_matches) == 1) {
      final_match <- author_matches[1, ]
      
      # Verifica coerenza et al.
      if (cite$cite_has_etal && final_match$n_authors < 3) {
        confidence <- "medium_etal_inconsistent"
      }
      
    } else {
      # Match multipli - usa secondo autore per disambiguare
      if (!is.na(cite$cite_second_author_normalized)) {
        second_match <- author_matches %>%
          filter(
            !is.na(ref_second_author_normalized),
            ref_second_author_normalized == cite$cite_second_author_normalized
          )
        
        if (nrow(second_match) == 1) {
          final_match <- second_match[1, ]
          confidence <- "high_second_author"
        } else if (nrow(second_match) > 1) {
          final_match <- second_match[1, ]
          confidence <- "medium_multiple_with_second"
        }
      }
      
      # Euristica et al.: scegli quello con più autori
      if (is.null(final_match) && cite$cite_has_etal) {
        etal_candidates <- author_matches %>%
          filter(n_authors >= 3) %>%
          arrange(desc(n_authors))
        
        if (nrow(etal_candidates) > 0) {
          final_match <- etal_candidates[1, ]
          confidence <- "medium_etal_heuristic"
        }
      }
      
      # Fallback: prendi il primo
      if (is.null(final_match)) {
        final_match <- author_matches[1, ]
        confidence <- "medium_multiple_matches"
      }
    }
    
    # Costruisci risultato
    matched_row <- cite %>%
      mutate(
        matched_ref_id = final_match$ref_id,
        ref_full_text = final_match$ref_full_text,
        match_confidence = confidence,
        ref_authors = final_match$ref_authors,
        ref_year = final_match$ref_year
      )
    
    matched_citations <- bind_rows(matched_citations, matched_row)
  }
  
  # Output finale
  result <- matched_citations %>%
    select(
      citation_id,
      citation_text,
      citation_text_clean,
      citation_type,
      cite_author,
      cite_second_author,
      cite_year,
      cite_has_etal,
      matched_ref_id,
      ref_full_text,
      ref_authors,
      ref_year,
      match_confidence
    )
  
  return(result)
}