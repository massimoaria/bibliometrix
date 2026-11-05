utils::globalVariables(c(
  "CR_canonical",
  "has_title",
  "CR_id",
  "CR_normalized",
  "CR_original",
  "blocking_key",
  "cluster_id",
  "completeness_score",
  "first_author",
  "journal",
  "merge_key",
  "n_cluster",
  "pages",
  "temp_cluster",
  "volume",
  "doi",
  "n_exact",
  "ABBREVIATION",
  "ABBR_CLEAN",
  "LANGUAGES",
  "NO_ABBR",
  "WORD",
  "WORD_CLEAN",
  "journal_iso4",
  "journal_original",
  "len",
  "page_start",
  "str_starts",
  "CR_stripped",
  "n_stripped",
  "temp_citation",
  "variant_freq",
  "wos_key",
  "author_surname",
  "similarity",
  "title_norm",
  "title_first",
  "has_title",
  "canonical_title",
  "min_similarity"
))

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

  if (length(words) == 0) {
    return(NA_character_)
  }

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
      WORD_CLEAN = toupper(str_remove_all(WORD, "[[:punct:]]")) %>%
        str_squish(),
      ABBR_CLEAN = toupper(str_remove_all(ABBREVIATION, "[[:punct:]]")) %>%
        str_squish()
    ) %>%
    dplyr::filter(WORD_CLEAN != "", ABBR_CLEAN != "") %>%
    select(WORD_CLEAN, ABBR_CLEAN) %>%
    distinct()

  # Create lookup: word -> abbreviation
  word_to_abbr <- setNames(ltwa_english$ABBR_CLEAN, ltwa_english$WORD_CLEAN)

  # Also create reverse for words that are already abbreviated
  abbr_set <- unique(ltwa_english$ABBR_CLEAN)

  # Process each word
  abbreviated_words <- sapply(
    words,
    function(word) {
      # Skip very short words (articles, prepositions, conjunctions)
      if (
        word %in%
          c(
            "OF",
            "THE",
            "A",
            "AN",
            "AND",
            "OR",
            "FOR",
            "IN",
            "ON",
            "AT",
            "TO",
            "BY"
          )
      ) {
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
    },
    USE.NAMES = FALSE
  )

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
      WORD_CLEAN = toupper(str_remove_all(WORD, "[[:punct:]]")) %>%
        str_squish(),
      ABBR_CLEAN = toupper(str_remove_all(ABBREVIATION, "[[:punct:]]")) %>%
        str_squish(),
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
    prepositions = c(
      "OF",
      "IN",
      "ON",
      "AT",
      "TO",
      "FOR",
      "WITH",
      "FROM",
      "BY",
      "ABOUT",
      "AS",
      "INTO",
      "LIKE",
      "THROUGH",
      "AFTER",
      "OVER",
      "BETWEEN",
      "OUT",
      "AGAINST",
      "DURING",
      "WITHOUT",
      "BEFORE",
      "UNDER",
      "AROUND",
      "AMONG"
    ),

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

  if (length(words) == 0) {
    return(NA_character_)
  }

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
          if (
            all(
              tolower(words[j:(j + length(phrase_words) - 1)]) ==
                tolower(phrase_words)
            )
          ) {
            # Mark these positions
            words[j] <- ltwa_lookup$phrases$ABBREVIATION[i]
            if (length(phrase_words) > 1) {
              words_to_keep[(j + 1):(j + length(phrase_words) - 1)] <- FALSE
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
    if (i == 1) {
      next
    }

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
  abbreviated <- sapply(
    words_expanded,
    function(w) {
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
    },
    USE.NAMES = FALSE
  )

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
  unique_journals <- unique_journals[
    !is.na(unique_journals) & unique_journals != ""
  ]

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
  pb_step <- if (show_progress) {
    max(1, floor(length(unique_journals) / 20))
  } else {
    Inf
  }

  normalized <- tibble(journal_original = unique_journals) %>%
    mutate(
      journal_iso4 = sapply(seq_along(journal_original), function(i) {
        if (show_progress && i %% pb_step == 0) {
          cat("\r    Progress:", round(100 * i / length(journal_original)), "%")
        }

        tryCatch(
          {
            iso4 <- abbreviate_journal_title(journal_original[i], ltwa_lookup)
            if (is.na(iso4) || iso4 == "") {
              toupper(str_remove_all(journal_original[i], "[[:punct:]]")) %>%
                str_squish()
            } else {
              iso4
            }
          },
          error = function(e) {
            toupper(str_remove_all(journal_original[i], "[[:punct:]]")) %>%
              str_squish()
          }
        )
      })
    )

  if (show_progress) {
    cat("\n")
  }

  n_changed <- sum(
    normalized$journal_original != normalized$journal_iso4,
    na.rm = TRUE
  )
  cat(
    "  Converted",
    n_changed,
    "journals to ISO4 (",
    round(100 * n_changed / nrow(normalized), 1),
    "%)\n"
  )

  return(normalized)
}

#' Remove diacritics from string with robust fallback
#' @keywords internal
remove_diacritics <- function(x) {
  if (is.na(x) || x == "") {
    return(x)
  }

  # Try iconv first
  result <- tryCatch(
    {
      temp <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
      if (!is.na(temp)) {
        # Clean up artifacts that iconv might add (like " or ')
        temp <- str_remove_all(temp, '[\'\"`^~]')
        temp
      } else {
        NULL
      }
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(result) || is.na(result)) {
    result <- x %>%
      # German umlauts
      str_replace_all("\u00C4", "AE") %>% # Ä
      str_replace_all("\u00D6", "OE") %>% # Ö
      str_replace_all("\u00DC", "UE") %>% # Ü
      str_replace_all("\u00E4", "ae") %>% # ä
      str_replace_all("\u00F6", "oe") %>% # ö
      str_replace_all("\u00FC", "ue") %>% # ü
      str_replace_all("\u00DF", "ss") %>% # ß
      # French/Spanish accents - vowels with accents
      str_replace_all("\u00C0|\u00C1|\u00C2|\u00C3|\u00C5", "A") %>% # À|Á|Â|Ã|Å
      str_replace_all("\u00C8|\u00C9|\u00CA|\u00CB", "E") %>% # È|É|Ê|Ë
      str_replace_all("\u00CC|\u00CD|\u00CE|\u00CF", "I") %>% # Ì|Í|Î|Ï
      str_replace_all("\u00D2|\u00D3|\u00D4|\u00D5", "O") %>% # Ò|Ó|Ô|Õ
      str_replace_all("\u00D9|\u00DA|\u00DB", "U") %>% # Ù|Ú|Û
      str_replace_all("\u00DD", "Y") %>% # Ý
      str_replace_all("\u00E0|\u00E1|\u00E2|\u00E3|\u00E5", "a") %>% # à|á|â|ã|å
      str_replace_all("\u00E8|\u00E9|\u00EA|\u00EB", "e") %>% # è|é|ê|ë
      str_replace_all("\u00EC|\u00ED|\u00EE|\u00EF", "i") %>% # ì|í|î|ï
      str_replace_all("\u00F2|\u00F3|\u00F4|\u00F5", "o") %>% # ò|ó|ô|õ
      str_replace_all("\u00F9|\u00FA|\u00FB", "u") %>% # ù|ú|û
      str_replace_all("\u00FD|\u00FF", "y") %>% # ý|ÿ
      # Spanish
      str_replace_all("\u00D1", "N") %>% # Ñ
      str_replace_all("\u00F1", "n") %>% # ñ
      # Czech, Polish, etc.
      str_replace_all("\u010C|\u0106|\u00C7", "C") %>% # Č|Ć|Ç
      str_replace_all("\u010D|\u0107|\u00E7", "c") %>% # č|ć|ç
      str_replace_all("\u0160", "S") %>% # Š
      str_replace_all("\u0161", "s") %>% # š
      str_replace_all("\u017D", "Z") %>% # Ž
      str_replace_all("\u017E", "z") %>% # ž
      str_replace_all("\u0141", "L") %>% # Ł
      str_replace_all("\u0142", "l") %>% # ł
      # Scandinavian
      str_replace_all("\u00D8", "O") %>% # Ø
      str_replace_all("\u00F8", "o") %>% # ø
      str_replace_all("\u00C6", "AE") %>% # Æ
      str_replace_all("\u00E6", "ae") %>% # æ
      str_replace_all("\u00C5", "A") %>% # Å
      str_replace_all("\u00E5", "a") %>% # å
      # Turkish
      str_replace_all("\u0130|\u012A", "I") %>% # İ|Ī
      str_replace_all("\u0131|\u012B", "i") %>% # ı|ī
      str_replace_all("\u015E", "S") %>% # Ş
      str_replace_all("\u015F", "s") %>% # ş
      str_replace_all("\u011E", "G") %>% # Ğ
      str_replace_all("\u011F", "g") # ğ
  }

  return(result)
}

#' Convert new Scopus citation format to classic format
#'
#' Scopus has introduced a new citation format where the publication year appears
#' at the end in parentheses instead of after the title. This function converts
#' citations from the new format to the classic format by moving the year from
#' the end to after the title.
#'
#' @param citation Character string containing a bibliographic citation
#'
#' @return Character string with citation in classic Scopus format
#'
#' @details
#' New Scopus format: AUTHOR, TITLE, JOURNAL, VOLUME, ISSUE, PAGES, (YEAR)
#' Classic Scopus format: AUTHOR, TITLE (YEAR) JOURNAL, VOLUME, PAGES
#'
#' The function uses a robust approach:
#' \itemize{
#'   \item Extracts year from end (YYYY)
#'   \item Extracts first author from beginning
#'   \item Extracts pages (PP. xxx-xxx or PP. xxx)
#'   \item Extracts volume and issue numbers
#'   \item Extracts journal name (text before volume/issue/pages)
#'   \item Deduces title as remaining text after author
#' }
#'
#' @keywords internal

convert_scopus_new_to_classic <- function(citation) {
  # Papers without references reach here as NA (e.g. CR rebuilt by a join
  # after reference matching, #696); if (NA) would fail below
  if (is.na(citation) || !nzchar(trimws(citation))) {
    return(citation)
  }

  # Check if citation ends with year in parentheses: (YYYY)
  if (!str_detect(citation, "\\(\\d{4}\\)\\s*$")) {
    return(citation) # Not new format, return unchanged
  }

  # Extract and remove year from the end
  year_match <- str_extract(citation, "\\(\\d{4}\\)\\s*$")
  year <- str_extract(year_match, "\\d{4}")
  citation_no_year <- str_remove(citation, "\\s*\\(\\d{4}\\)\\s*$") %>%
    str_trim()

  # Extract first author (everything before first comma)
  first_comma_pos <- str_locate(citation_no_year, ",")[1, "start"]
  if (is.na(first_comma_pos)) {
    return(citation) # Cannot parse without commas
  }

  first_author <- substr(citation_no_year, 1, first_comma_pos - 1) %>%
    str_trim()
  after_author <- substr(
    citation_no_year,
    first_comma_pos + 1,
    nchar(citation_no_year)
  ) %>%
    str_trim()

  # Extract pages (PP. xxx-xxx or PP. xxx) - usually near the end
  pages_pattern <- "PP\\.?\\s*\\d+\\s*[-\u2013\u2014]?\\s*\\d*"
  pages_match <- str_extract(
    after_author,
    regex(pages_pattern, ignore_case = TRUE)
  )

  # Remove pages from string to simplify further parsing
  if (!is.na(pages_match)) {
    after_author_no_pages <- str_remove(
      after_author,
      regex(pages_pattern, ignore_case = TRUE)
    ) %>%
      str_trim() %>%
      str_remove(",\\s*$") %>% # Remove trailing comma if present
      str_trim()
  } else {
    after_author_no_pages <- after_author
    pages_match <- ""
  }

  # Split remaining text by commas
  parts <- str_split(after_author_no_pages, ",")[[1]] %>%
    str_trim()

  if (length(parts) < 2) {
    # Not enough parts to parse properly
    return(citation)
  }

  # Working backwards from the end to identify known components:
  # Last part (or second-to-last if pages removed) should be issue or volume
  # Before that should be volume (if issue present) or journal
  # Everything else at the beginning is the title

  # Try to identify volume and issue
  # Volume: usually a number by itself or with V prefix
  # Issue: usually a number by itself, comes after volume

  # Check last parts for numeric patterns
  volume <- NA_character_
  issue <- NA_character_
  journal_idx <- NA_integer_

  # Search from the end backwards
  n_parts <- length(parts)

  # Strategy: Find numeric values from the end
  # In new Scopus format: ..., JOURNAL, VOLUME, ISSUE, (YEAR)
  # After removing year and pages: ..., JOURNAL, VOLUME, ISSUE
  # So last 1-2 parts are likely volume/issue, before that is journal

  numeric_positions <- c()
  for (i in n_parts:1) {
    part <- parts[i]
    # Check if this part is purely numeric (not a year, which we already removed)
    if (str_detect(part, "^\\d+$") && nchar(part) <= 4) {
      numeric_positions <- c(numeric_positions, i)
    }
  }

  # Identify volume and issue based on position
  if (length(numeric_positions) >= 2) {
    # Last two numeric parts are likely issue and volume (in that order from end)
    issue <- parts[numeric_positions[1]]
    volume <- parts[numeric_positions[2]]
    # Journal is the part before volume
    journal_idx <- numeric_positions[2] - 1
  } else if (length(numeric_positions) == 1) {
    # Only one numeric part - it's the volume
    volume <- parts[numeric_positions[1]]
    issue <- NA_character_
    # Journal is the part before volume
    journal_idx <- numeric_positions[1] - 1
  } else {
    # No clear numeric volume/issue found
    # Assume last part is journal
    journal_idx <- n_parts
  }

  # Journal is the part just before volume/issue
  if (journal_idx >= 1 && journal_idx <= length(parts)) {
    journal <- parts[journal_idx]
  } else {
    journal <- ""
  }

  # Title is everything from start up to (but not including) journal
  if (journal_idx > 1) {
    title <- paste(parts[1:(journal_idx - 1)], collapse = ", ")
  } else {
    title <- ""
  }

  # Reconstruct in classic format: AUTHOR, TITLE (YEAR) JOURNAL, VOLUME, ISSUE, PAGES
  result_parts <- c()

  # Add author
  result_parts <- c(result_parts, first_author)

  # Add title with year: TITLE (YEAR)
  if (nchar(title) > 0) {
    result_parts <- c(result_parts, paste0(title, " (", year, ")"))
  } else {
    result_parts <- c(result_parts, paste0("(", year, ")"))
  }

  # Now add journal and rest WITHOUT commas, space-separated like classic Scopus
  # Classic Scopus: AUTHOR, TITLE (YEAR) JOURNAL, VOLUME, PAGES

  # Build the "after year" part
  after_year_parts <- c()

  if (nchar(journal) > 0) {
    after_year_parts <- c(after_year_parts, journal)
  }

  if (!is.na(volume)) {
    after_year_parts <- c(after_year_parts, volume)
  }

  if (!is.na(issue)) {
    after_year_parts <- c(after_year_parts, issue)
  }

  if (nchar(pages_match) > 0) {
    after_year_parts <- c(after_year_parts, pages_match)
  }

  # Join the parts correctly:
  # AUTHOR, TITLE (YEAR) JOURNAL, VOLUME, ISSUE, PAGES
  if (length(result_parts) > 0 && length(after_year_parts) > 0) {
    classic_format <- paste0(
      paste(result_parts, collapse = ", "),
      " ",
      paste(after_year_parts, collapse = ", ")
    )
  } else if (length(result_parts) > 0) {
    classic_format <- paste(result_parts, collapse = ", ")
  } else {
    classic_format <- citation # Fallback to original
  }

  return(classic_format)
}


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
#'   Default is 0.90, which provides a good balance between precision and recall.
#' @param method String distance method to use for fuzzy matching. Options include:
#'   \itemize{
#'     \item "jw" (default): Jaro-Winkler distance, optimized for bibliographic strings
#'     \item "lv": Levenshtein distance
#'     \item "osa": Optimal String Alignment distance
#'     \item "lcs": Longest Common Subsequence distance
#'     \item Other methods supported by \code{\link[stringdist]{stringdistmatrix}}
#'   }
#' @param min_chars Minimum characters for valid citations (default: 20).
#' @param max_block_size Integer. Blocks with at least this many unique normalized
#'   strings skip within-block fuzzy matching and fall back to exact matching only,
#'   to bound the cost of the pairwise distance matrix (default: 100).
#' @param use_iso4 Logical. If \code{TRUE} (default), normalize journal names to their
#'   ISO 4 abbreviated form via the LTWA database (Phase 1.5). Set to \code{FALSE} to
#'   disable ISO 4 / LTWA journal normalization (used for ablation analyses).
#' @param use_doi Logical. If \code{TRUE} (default), perform exact matching on DOIs
#'   (part of Phase 2). Set to \code{FALSE} to disable DOI-based matching.
#' @param use_exact Logical. If \code{TRUE} (default), perform exact normalized-string
#'   and punctuation-invariant matching (Phase 2). Set to \code{FALSE} to disable them.
#' @param fuzzy Logical. If \code{TRUE} (default), perform within-block matching
#'   (Phase 4: WoS deterministic key matching and Scopus fuzzy clustering). Set to
#'   \code{FALSE} to disable within-block matching, keeping only the exact phases.
#' @param use_postproc Logical. If \code{TRUE} (default), perform Phase 4.5
#'   metadata-based post-processing merge. Set to \code{FALSE} to disable it.
#' @param title_guard Logical. If \code{TRUE}, run an optional Phase 4.6 that
#'   purifies clusters by detecting series part markers in titles: distinct
#'   works that share author, year, journal and volume but differ only in a
#'   series designator (e.g. "... Part I" / "... Part II", "... I." / "... II.")
#'   are split into separate clusters. This step only splits clusters, never
#'   merges; it relies solely on the part marker (not on full-title similarity)
#'   so it does not interfere with robustness to title typos. Default
#'   \code{FALSE} (legacy behaviour).
#' @param use_title Logical. If \code{TRUE} (default), references with their year in
#'   parentheses are compared on their title (see \emph{Details}). \code{FALSE} gives
#'   the earlier behaviour.
#' @param merge_editions Logical. If \code{FALSE} (default), the same title in years
#'   further apart counts as different works, the editions of a book for instance. If
#'   \code{TRUE}, all of them count as one work, unless their volume or first page
#'   differ. It requires \code{use_title = TRUE}.
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
#' hierarchical clustering. For blocks with at least \code{max_block_size} unique
#' normalized strings (default 100), exact matching on normalized strings is used
#' instead to maintain performance.
#'
#' \strong{Phase 4: Canonical Representative Selection}
#' For each cluster, the canonical representative is the most frequent variant
#' among those that keep the title of the work (Scopus citations; WoS citations
#' carry no title), with DOI, volume, pages and length as tiebreakers.
#'
#' \strong{Phase 5: Post-Processing}
#' Citations sharing the same first author, year, journal, and volume are merged
#' into a single cluster, even if they weren't matched in Phase 3. This catches
#' cases where minor title variations prevented matching.
#'
#' \strong{Title comparison}
#' With \code{use_title = TRUE} (the default), a reference with its year in
#' parentheses (the Scopus form) is compared on its title, within the block of its
#' first author's surname and year: co-authors, written in many ways, play no part.
#' Two references are the same work when their whole titles, or their first segments
#' if long enough (a chapter cited with different containers), reach \code{threshold},
#' unless their volume, first page or part number differ. The same long title in the
#' year most cited and the years next to it is also the same work; years further away
#' are other editions, and stay separate unless \code{merge_editions = TRUE}. A book and a chapter in it stay separate, since their
#' titles differ. Set \code{use_title = FALSE} to reproduce the earlier behaviour, in
#' which the comparison had no title and the similarity was relative to each block.
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
#'   \item \code{similarity}: Similarity between the title and that of the canonical
#'     citation (1 for the canonical citation itself; \code{NA} when no title is known)
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
#' \code{\link{applyReferenceMatching}} for direct application to bibliometrix data frames
#'
#' @references
#' Aria, M. & Cuccurullo, C. (2017). bibliometrix: An R-tool for comprehensive
#' science mapping analysis. Journal of Informetrics, 11(4), 959-975.
#'
#' @export

normalize_citations <- function(
  CR_vector,
  threshold = 0.90,
  method = "jw",
  min_chars = 20,
  max_block_size = 100,
  use_iso4 = TRUE,
  use_doi = TRUE,
  use_exact = TRUE,
  fuzzy = TRUE,
  use_postproc = TRUE,
  title_guard = FALSE,
  use_title = TRUE,
  merge_editions = FALSE
) {
  # Detect citation format
  detect_format <- function(x) {
    # New Scopus format: ends with (YEAR)
    if (str_detect(x, "\\(\\d{4}\\)\\s+[A-Z]")) {
      # Classic Scopus format: AUTHOR, TITLE (YEAR) JOURNAL, V, PP
      return("scopus")
    } else if (str_detect(x, ",\\s*\\d{4},\\s*[A-Z]")) {
      # WoS format: AUTHOR, YEAR, JOURNAL, V, P, DOI
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
      str_replace_all("\\s*:\\s*", " ") %>% # Remove colons with surrounding spaces
      str_replace_all("\\s*;\\s*", " ") %>% # Remove semicolons with surrounding spaces
      str_replace_all("\\s*-\\s*", " ") %>% # Normalize hyphens
      str_replace_all("\\(\\d+\\)", "") %>% # Remove issue numbers
      str_replace_all("PP\\.?\\s*\\d+\\s*[-\u2013\u2014]\\s*\\d+", "") %>% # Remove page numbers
      str_replace_all("P\\.?\\s*\\d+\\s*[-\u2013\u2014]\\s*\\d+", "") %>%
      # remove quotes symbols
      str_replace_all("<e2><80><9c>", " ") %>%
      str_replace_all('"', " ") %>%
      str_replace_all("[[:punct:]]", " ") %>% # Remove punctuation
      str_replace_all("\\s+", " ") %>% # Normalize whitespace
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
      head(1) # Take first year found

    # DOI extraction
    doi <- extract_doi(x)

    if (format == "scopus" || format == "scopus_new") {
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
        matches_with_comma <- str_locate_all(
          temp_title,
          author_pattern_with_comma
        )[[1]]

        # Try pattern WITHOUT comma after (for potentially last author)
        author_pattern_no_comma <- "[A-Z][A-Za-z\\-']+,\\s*[A-Z][A-Z\\.\\s]*\\.?\\s+"
        matches_no_comma <- str_locate_all(
          temp_title,
          author_pattern_no_comma
        )[[1]]

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
      journal <- str_extract(
        x,
        "(?<=\\d{4}\\)\\s)([A-Z][A-Z\\s&\\.-]+?)(?=,\\s*\\d+|,\\s*PP)"
      )

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
      pages <- str_extract(
        x,
        "PP\\.?\\s*\\d+[-\u2013\u2014]\\d+|PP\\.?\\s*\\d+"
      )

      # Extract significant words from CLEANED title
      if (!is.na(title) && nchar(title) > 0) {
        title_words <- str_extract_all(title, "\\b[A-Z]{3,}\\b")[[1]]

        # Filter stop words
        stop_words <- c(
          "THE",
          "AND",
          "FOR",
          "FROM",
          "WITH",
          "ABOUT",
          "THAT",
          "THIS",
          "BETWEEN",
          "THROUGH",
          "DURING",
          "BEFORE",
          "AFTER",
          "WHICH",
          "THEIR",
          "THESE",
          "THOSE",
          "WHEN",
          "WHERE",
          "WHAT"
        )
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
      journal <- str_extract(
        x,
        "(?<=\\d{4},\\s)([A-Z][A-Z\\s&\\-]+?)(?=,\\s*V\\d+|,\\s*P\\d+|,\\s*DOI)"
      )

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
      !grepl("^[A-Z\\s]{1,10}$", CR_original) # Remove very short strings
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
      CR_normalized = sapply(features, function(x) x$full_normalized),
      title_norm = NA_character_,
      title_first = NA_character_,
      page_start_ref = NA_character_,
      has_title = FALSE
    )

  # References with their year in parentheses hold a title: compare that, since
  # the first author and the year are already the block. The year is the one in
  # the parentheses, not the first number of four digits ("2030 AGENDA ... (2019)").
  if (isTRUE(use_title) && nrow(df) > 0) {
    parsed <- parse_reference_titles(df$CR_original)
    keep <- !is.na(parsed$title) & df$format != "wos"
    df$title_norm[keep] <- parsed$title[keep]
    df$title_first[keep] <- parsed$title_first[keep]
    df$year[keep] <- parsed$year[keep]
    df$page_start_ref <- str_extract(df$pages, "\\d+")
    df$has_title <- keep
  }

  df <- df %>% # exclude rows with NA in first_author and year
    dplyr::filter(!is.na(first_author), !is.na(year)) %>%
    # exclude rows with year < 1700 and > current year+1
    dplyr::filter(
      as.numeric(year) >= 1700 &
        as.numeric(year) <= as.numeric(format(Sys.Date(), "%Y")) + 1
    )

  cat("  Filtered out", length(CR_vector) - nrow(df), "invalid citations\n")

  cat(
    "  Detected formats: WoS =",
    sum(df$format == "wos", na.rm = TRUE),
    ", Scopus =",
    sum(df$format == "scopus", na.rm = TRUE),
    ", Scopus New =",
    sum(df$format == "scopus_new", na.rm = TRUE),
    ", Unknown =",
    sum(df$format == "unknown", na.rm = TRUE),
    "\n"
  )

  cat("Phase 1.5: Normalizing journal names to ISO4 format...\n")

  # Load LTWA database
  ltwa <- NULL

  # Try to load from bibliometrix package data
  data("ltwa", package = "bibliometrix", envir = environment())

  # Apply ISO4 normalization if LTWA is available (and not disabled via use_iso4)
  if (use_iso4 && !is.null(ltwa) && nrow(ltwa) > 0) {
    # Create ISO4 lookup table for efficiency
    journal_iso4_lookup <- create_journal_iso4_lookup(df$journal, ltwa)

    # Apply normalization
    df <- df %>%
      left_join(journal_iso4_lookup, by = c("journal" = "journal_original")) %>%
      mutate(
        journal_original = journal, # Preserve original
        journal = coalesce(journal_iso4, journal) # Use ISO4, fallback to original
      ) %>%
      select(-journal_iso4)

    cat("  Journal ISO4 normalization completed\n")

    # *** CRITICAL: Recreate CR_normalized with ISO4 journal names ***
    cat("  Recreating normalized strings with ISO4 journal names...\n")
    df <- df %>%
      mutate(
        # Reconstruct the citation string with ISO4 journal
        temp_citation = case_when(
          format %in% c("scopus", "scopus_new") ~
            paste0(
              first_author,
              ", ",
              year,
              " ",
              journal,
              " ",
              ifelse(!is.na(volume), paste0("V", volume), ""),
              ifelse(!is.na(pages), paste0(" ", pages), "")
            ),
          format == "wos" ~
            paste0(
              first_author,
              ", ",
              year,
              ", ",
              journal,
              ifelse(!is.na(volume), paste0(", V", volume), ""),
              ifelse(!is.na(pages), paste0(", P", pages), ""),
              ifelse(!is.na(doi), paste0(", DOI ", doi), "")
            ),
          TRUE ~ CR_original
        ),
        # Use sapply to normalize each string individually
        CR_normalized = sapply(
          temp_citation,
          normalize_string,
          USE.NAMES = FALSE
        )
      ) %>%
      select(-temp_citation)
  } else {
    cat("  Proceeding without journal normalization\n")
    df <- df %>%
      mutate(journal_original = journal)
  }

  # The string rebuilt above holds no title, and for a reference with no journal
  # nothing but the author and the year: all the works of an author in one year
  # would become the same string. Where a title is known, the string is built on it.
  if (any(df$has_title)) {
    title_rows <- df$has_title
    df$CR_normalized[title_rows] <- paste(
      gsub("\\s.*", "", df$first_author[title_rows]),
      df$year[title_rows],
      df$title_norm[title_rows],
      coalesce(df$volume[title_rows], ""),
      coalesce(df$page_start_ref[title_rows], "")
    )
  }

  cat("Phase 2: Exact matching by DOI and normalized string...\n")

  # Initialize cluster_id
  df <- df %>%
    mutate(cluster_id = NA_character_)

  # DOI-based matching (only for WoS with valid DOI)
  valid_dois <- df %>%
    dplyr::filter(
      !is.na(doi),
      str_detect(doi, "^10\\.\\d{4,}/"),
      nchar(doi) >= 10
    )

  if (use_doi && nrow(valid_dois) > 0) {
    doi_clusters <- valid_dois %>%
      group_by(doi) %>%
      mutate(cluster_id = paste0("DOI_", min(CR_id))) %>%
      ungroup()

    df$cluster_id[df$CR_id %in% doi_clusters$CR_id] <- doi_clusters$cluster_id

    cat(
      "  Matched",
      nrow(doi_clusters),
      "citations via",
      n_distinct(doi_clusters$doi),
      "unique DOIs\n"
    )
  }

  # Exact normalized string matching (fast pre-clustering)
  unmatched_df <- df %>%
    dplyr::filter(is.na(cluster_id))

  if (use_exact && nrow(unmatched_df) > 0) {
    exact_matches <- unmatched_df %>%
      group_by(CR_normalized) %>%
      mutate(
        n_exact = n(),
        cluster_id = if_else(
          n_exact > 1,
          paste0("EXACT_", min(CR_id)),
          NA_character_
        )
      ) %>%
      ungroup() %>%
      dplyr::filter(!is.na(cluster_id))

    if (nrow(exact_matches) > 0) {
      df$cluster_id[
        df$CR_id %in% exact_matches$CR_id
      ] <- exact_matches$cluster_id
      cat(
        "  Matched",
        nrow(exact_matches),
        "citations via exact normalization\n"
      )
    }
  }

  # Punctuation-invariant matching: additional exact matching pass using
  # a stripped form of the original string (all non-alphanumeric characters
  # removed, case-folded, whitespace collapsed). This catches citation
  # pairs that differ only in punctuation, spacing, or formatting.
  unmatched_df2 <- df %>%
    dplyr::filter(is.na(cluster_id))

  if (use_exact && nrow(unmatched_df2) > 0) {
    unmatched_df2 <- unmatched_df2 %>%
      mutate(
        CR_stripped = sapply(CR_original, function(x) {
          x %>%
            remove_diacritics() %>%
            toupper() %>%
            str_replace_all("[^A-Z0-9]", "")
        }, USE.NAMES = FALSE)
      )

    stripped_matches <- unmatched_df2 %>%
      group_by(CR_stripped) %>%
      mutate(
        n_stripped = n(),
        cluster_id = if_else(
          n_stripped > 1,
          paste0("STRIP_", min(CR_id)),
          NA_character_
        )
      ) %>%
      ungroup() %>%
      dplyr::filter(!is.na(cluster_id))

    if (nrow(stripped_matches) > 0) {
      df$cluster_id[
        df$CR_id %in% stripped_matches$CR_id
      ] <- stripped_matches$cluster_id
      cat(
        "  Matched",
        nrow(stripped_matches),
        "citations via punctuation-invariant matching\n"
      )
    }
  }

  cat("Phase 3: Blocking by author + year...\n")

  # Block by author surname + year to allow matching across journal/name variants
  # Use only surname (first word) so "ARIA M" and "ARIA MASSIMO" share a block
  df <- df %>%
    mutate(
      blocking_key = paste0(
        coalesce(gsub("\\s.*", "", first_author), "UNK"),
        "_",
        coalesce(year, "0000")
      )
    )

  block_sizes <- table(df$blocking_key)
  cat("  Created", length(block_sizes), "blocks\n")
  cat("  Average block size:", round(mean(block_sizes), 1), "\n")

  cat("Phase 4: Fuzzy matching within blocks...\n")

  # Function to match within a block
  match_within_block <- function(block_df) {
    # References with a title are clustered by title, whatever the exact phases
    # found: variants of one work may already sit in two exact clusters.
    if (fuzzy && any(block_df$has_title)) {
      block_df <- cluster_by_title(block_df, threshold, method, max_block_size)
    }

    # Only work with unmatched citations
    unmatched <- block_df %>% dplyr::filter(is.na(cluster_id))

    if (nrow(unmatched) <= 1) {
      if (nrow(unmatched) == 1) {
        block_df$cluster_id[is.na(
          block_df$cluster_id
        )] <- as.character(unmatched$CR_id[1])
      }
      return(block_df)
    }

    # Ablation: when fuzzy matching is disabled, skip within-block matching
    # entirely (both the WoS deterministic key and the Scopus fuzzy branch);
    # each still-unmatched citation is kept as its own singleton cluster.
    if (!fuzzy) {
      for (i in seq_len(nrow(unmatched))) {
        if (is.na(block_df$cluster_id[block_df$CR_id == unmatched$CR_id[i]])) {
          block_df$cluster_id[block_df$CR_id == unmatched$CR_id[i]] <-
            as.character(unmatched$CR_id[i])
        }
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
            coalesce(gsub("\\s.*", "", first_author), "UNK"),
            "_",
            coalesce(year, "0000"),
            "_",
            coalesce(gsub("[^A-Z0-9]", "", toupper(journal)), "UNK"),
            "_",
            coalesce(volume, "NA"),
            "_",
            coalesce(page_start, "NOPAGE")
          )
        ) %>%
        group_by(wos_key) %>%
        mutate(
          new_cluster_id = paste0(
            unique(block_df$blocking_key)[1],
            "_C",
            dplyr::cur_group_id()
          )
        ) %>%
        ungroup() %>%
        select(-wos_key, -page_start)

      block_df$cluster_id[
        block_df$CR_id %in% unmatched$CR_id
      ] <- unmatched$new_cluster_id
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
    if (length(unique_norm) > 1 && length(unique_norm) < max_block_size) {
      dist_matrix <- stringdist::stringdistmatrix(
        unique_norm,
        unique_norm,
        method = method
      )

      max_dist <- max(dist_matrix)
      if (max_dist > 0) {
        sim_matrix <- 1 - (dist_matrix / max_dist)
      } else {
        sim_matrix <- matrix(
          1,
          nrow = length(unique_norm),
          ncol = length(unique_norm)
        )
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
          mutate(
            new_cluster_id = paste0(
              unique(block_df$blocking_key)[1],
              "_C",
              temp_cluster
            )
          )

        block_df$cluster_id[
          block_df$CR_id %in% unmatched$CR_id
        ] <- unmatched$new_cluster_id
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
    mutate(
      cluster_id = ifelse(is.na(cluster_id), as.character(CR_id), cluster_id)
    )

  cat(
    "Phase 4.5: Post-processing - Merging clusters with identical metadata...\n"
  )

  n_clusters_before_merge <- n_distinct(df_matched$cluster_id)

  # Different merge strategies based on format
  # Journal is excluded from merge keys: author + year + volume + page_start
  # (+ title_words for Scopus) is already highly discriminating
  if (use_postproc) {
  df_matched <- df_matched %>%
    mutate(
      # Extract starting page number
      page_start = str_extract(pages, "^\\d+"),
      # Use surname only for merge keys (handles "ARIA M" vs "ARIA MASSIMO")
      author_surname = coalesce(gsub("\\s.*", "", first_author), "UNK"),

      merge_key = case_when(
        # Already compared by title: a shared author, year and volume is no
        # evidence that two titles are the same work.
        has_title ~ paste0("NOMERGE_", cluster_id),

        # WoS format: DOI is most reliable
        format == "wos" & !is.na(doi) ~ paste0("DOI_", doi),

        # WoS format without DOI: use metadata + pages
        format == "wos" ~
          paste0(
            author_surname,
            "_",
            coalesce(year, "0000"),
            "_",
            coalesce(volume, "NA"),
            "_",
            coalesce(page_start, "NOPAGE")
          ),

        # Scopus format: use metadata + pages + title fingerprint
        format %in%
          c("scopus", "scopus_new") &
          !is.na(page_start) &
          !is.na(title_words) ~
          paste0(
            author_surname,
            "_",
            coalesce(year, "0000"),
            "_",
            coalesce(volume, "NA"),
            "_",
            coalesce(page_start, "NOPAGE"),
            "_",
            title_words
          ),

        # Scopus with pages but no title
        format %in% c("scopus", "scopus_new") & !is.na(page_start) ~
          paste0(
            author_surname,
            "_",
            coalesce(year, "0000"),
            "_",
            coalesce(volume, "NA"),
            "_",
            coalesce(page_start, "NOPAGE")
          ),

        # Scopus with title but no pages (rare)
        format %in% c("scopus", "scopus_new") & !is.na(title_words) ~
          paste0(
            author_surname,
            "_",
            coalesce(year, "0000"),
            "_",
            coalesce(volume, "NA"),
            "_",
            title_words
          ),

        # Fallback with title: use title_words to distinguish
        !is.na(title_words) ~
          paste0(
            author_surname,
            "_",
            coalesce(year, "0000"),
            "_",
            coalesce(volume, "NA"),
            "_",
            title_words
          ),

        # Last resort: no pages, no title - keep original cluster (no merge)
        TRUE ~ paste0("NOMERGE_", cluster_id)
      )
    ) %>%
    group_by(merge_key) %>%
    mutate(
      cluster_id = min(cluster_id, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(-merge_key, -page_start, -author_surname)
  }

  # The same long title in consecutive years is one work, online and in print, or a
  # year mistyped in the reference. Blocks hold one year, so this comes after them.
  if (any(df_matched$has_title)) {
    df_matched <- merge_adjacent_years(df_matched, merge_editions)
  }

  n_clusters_after_merge <- n_distinct(df_matched$cluster_id)
  n_additional_matches <- n_clusters_before_merge - n_clusters_after_merge

  cat("  Clusters before metadata merge:", n_clusters_before_merge, "\n")
  cat("  Clusters after metadata merge:", n_clusters_after_merge, "\n")
  cat("  Additional matches found:", n_additional_matches, "\n")

  # ------------------------------------------------------------------
  # Phase 4.6 (optional): Title-based cluster purification.
  # Prevents over-merging of DISTINCT works that share author + year +
  # journal + volume but differ in title (e.g. multi-part series such as
  # "... Part I" / "... Part II", or several same-author/same-year/same-
  # journal articles whose titles were dropped from the matching string).
  # This step ONLY SPLITS existing clusters; it never merges. Disabled by
  # default to preserve legacy behaviour.
  # ------------------------------------------------------------------
  if (isTRUE(title_guard)) {
    cat("Phase 4.6: Title-based cluster purification...\n")

    # Extract a normalised title from a raw Scopus reference (text between the
    # author block and the year), returning NA for WoS / unparseable strings.
    .get_title <- function(x, fmt) {
      if (is.na(x) || is.na(fmt) || !fmt %in% c("scopus", "scopus_new")) {
        return(NA_character_)
      }
      t <- str_extract(x, "^.*?(?=\\s*\\(\\d{4}\\))")
      if (is.na(t)) {
        return(NA_character_)
      }
      t2 <- str_extract(t, "(?<=,\\s).*$") # drop leading first-author chunk
      if (!is.na(t2)) {
        t <- t2
      }
      t <- toupper(remove_diacritics(t))
      t <- gsub("[^A-Z0-9 ]", " ", t)
      t <- str_trim(gsub("\\s+", " ", t))
      if (nchar(t) < 4) {
        return(NA_character_)
      }
      t
    }

    # Extract a series-part marker (roman numeral or digit, optionally after
    # "PART") and map it to a canonical part number; NA when none is present.
    .get_part <- function(t) {
      if (is.na(t)) {
        return(NA_character_)
      }
      m <- regmatches(
        t,
        regexpr("\\b(PART\\s+)?(VIII|VII|VI|IV|IX|III|II|I|V|X|[1-9])\\b", t)
      )
      if (length(m) == 0 || m == "") {
        return(NA_character_)
      }
      r <- sub("PART\\s+", "", m)
      rmap <- c(
        I = "1", II = "2", III = "3", IV = "4", V = "5",
        VI = "6", VII = "7", VIII = "8", IX = "9", X = "10"
      )
      if (r %in% names(rmap)) {
        return(unname(rmap[r]))
      }
      r
    }

    ti_vec <- mapply(
      .get_title,
      df_matched$CR_original,
      df_matched$format,
      USE.NAMES = FALSE
    )
    pt_vec <- vapply(ti_vec, .get_part, character(1), USE.NAMES = FALSE)
    df_matched$.title_sig <- ti_vec
    df_matched$.part_sig <- pt_vec

    n_split <- 0L

    # Process only multi-member clusters; split into part-marker-compatible
    # sub-clusters via complete linkage on the 0/1 dissimilarity below.
    cl_sizes <- table(df_matched$cluster_id)
    multi_ids <- names(cl_sizes)[cl_sizes > 1]

    for (cid in multi_ids) {
      rows <- which(df_matched$cluster_id == cid)
      ti <- df_matched$.title_sig[rows]
      pt <- df_matched$.part_sig[rows]
      m <- length(rows)
      # Need at least two members carrying a title to consider splitting.
      if (sum(!is.na(ti)) < 2) {
        next
      }

      # Pairwise 0/1 dissimilarity: two members are INCOMPATIBLE (distance 1)
      # only when they carry DIFFERENT series part markers (e.g. "Part I" vs
      # "Part II"). This is the single title signal that distinguishes genuine
      # distinct works without being confounded by title typos (a character
      # typo virtually never alters a short roman-numeral/part token, and an
      # original and its perturbed variant always share the same marker), so it
      # is safe for the typo-robustness the pipeline is designed for. Title-less
      # members and same-marker members impose no constraint. Complete linkage
      # guarantees that no incompatible pair ever shares a sub-cluster.
      D <- matrix(0, m, m)
      for (a in 1:(m - 1)) {
        for (b in (a + 1):m) {
          if (!is.na(pt[a]) && !is.na(pt[b]) && pt[a] != pt[b]) {
            D[a, b] <- 1
            D[b, a] <- 1
          }
        }
      }

      if (any(D == 1)) {
        hc <- hclust(as.dist(D), method = "complete")
        comp <- cutree(hc, h = 0.5)
        if (length(unique(comp)) > 1) {
          # Keep the largest sub-cluster on the original id; suffix the rest.
          ord <- names(sort(table(comp), decreasing = TRUE))
          new_ids <- df_matched$cluster_id[rows]
          for (k in seq_along(ord)) {
            if (k > 1) {
              new_ids[comp == as.integer(ord[k])] <- paste0(cid, "_T", k)
            }
          }
          df_matched$cluster_id[rows] <- new_ids
          n_split <- n_split + (length(ord) - 1L)
        }
      }
    }

    df_matched$.title_sig <- NULL
    df_matched$.part_sig <- NULL
    cat("  Title-guard split operations:", n_split, "\n")
    cat(
      "  Clusters after purification:",
      n_distinct(df_matched$cluster_id),
      "\n"
    )
  }

  cat("Phase 5: Selecting canonical representatives...\n")

  # Scopus variants that keep the title of the work come first, so that the
  # canonical citation is not a variant that lost it (e.g. "AUTHORS (2016)
  # PP. 1-10"); WoS citations carry no title and are not affected
  df_matched$has_title <- df_matched$format %in% c("scopus", "scopus_new") &
    !is.na(mpScopusTitle(df_matched$CR_original))

  result <- df_matched %>%
    # Count frequency of each variant within its cluster
    group_by(cluster_id, CR_original) %>%
    mutate(variant_freq = n()) %>%
    ungroup() %>%
    group_by(cluster_id) %>%
    mutate(
      n_cluster = n(),
      # Scoring: a Scopus variant with the title first, then frequency, then
      # completeness as tiebreaker
      completeness_score = has_title * 1e6 +
        variant_freq * 1000 +
        (!is.na(doi)) * 100 +
        (!is.na(volume)) * 10 +
        (!is.na(pages)) * 5 +
        nchar(CR_original) * 0.01,
      CR_canonical = CR_original[which.max(completeness_score)][1],
      canonical_title = title_norm[which.max(completeness_score)][1]
    ) %>%
    select(-variant_freq, -has_title) %>%
    ungroup() %>%
    mutate(
      similarity = reference_similarity(
        title_norm, canonical_title, CR_original == CR_canonical, method
      )
    ) %>%
    arrange(desc(n_cluster), cluster_id) %>%
    select(
      CR_original,
      CR_canonical,
      cluster_id,
      n_cluster,
      format,
      first_author,
      year,
      journal_iso4 = journal, # Rename for clarity
      journal_original, # Keep original
      volume,
      doi,
      blocking_key,
      similarity
    )

  cat(
    "Completed! Found",
    length(unique(result$cluster_id)),
    "unique clusters from",
    nrow(result),
    "valid citations.\n"
  )
  cat("  Clusters with >1 citation:", sum(result$n_cluster > 1), "\n")
  cat(
    "  Total variants found:",
    sum(result$n_cluster) - length(unique(result$cluster_id)),
    "\n"
  )

  # Add back filtered citations with basic feature extraction
  filtered_citations <- tibble(
    CR_original = CR_vector[!CR_vector %in% result$CR_original]
  ) %>%
    dplyr::filter(CR_original != "", !is.na(CR_original))

  if (nrow(filtered_citations) > 0) {
    # Try to extract basic features even for filtered citations
    filtered_features <- lapply(filtered_citations$CR_original, function(x) {
      tryCatch(
        {
          # Basic extraction without full validation
          first_author <- str_extract(x, "^[A-Z][A-Z\\s-\\.]+(?=,)") %>%
            str_remove_all("\\.$") %>%
            str_trim()

          year <- str_extract(x, "\\(?\\d{4}\\)?") %>%
            str_remove_all("[()]")

          # Detect format to extract journal
          if (str_detect(x, "\\(\\d{4}\\)\\s+[A-Z]")) {
            # Scopus format
            journal <- str_extract(
              x,
              "(?<=\\d{4}\\)\\s)([A-Z][A-Z\\s&\\.-]+?)(?=,)"
            )
          } else if (str_detect(x, ",\\s*\\d{4},\\s*[A-Z]")) {
            # WoS format
            journal <- str_extract(x, "(?<=\\d{4},\\s)([A-Z][A-Z\\s&-]+?)(?=,)")
          } else {
            journal <- NA_character_
          }

          list(first_author = first_author, year = year, journal = journal)
        },
        error = function(e) {
          list(
            first_author = NA_character_,
            year = NA_character_,
            journal = NA_character_
          )
        }
      )
    })

    filtered_citations <- filtered_citations %>%
      mutate(
        CR_canonical = CR_original,
        cluster_id = paste0("FILTERED_", row_number()),
        n_cluster = 1L,
        format = NA_character_,
        first_author = sapply(filtered_features, function(x) {
          x$first_author %||% NA
        }),
        year = sapply(filtered_features, function(x) x$year %||% NA),
        journal_iso4 = NA_character_, # No ISO4 normalization for filtered
        journal_original = sapply(filtered_features, function(x) {
          x$journal %||% NA
        }),
        volume = NA_character_,
        doi = NA_character_,
        blocking_key = "FILTERED",
        similarity = NA_real_
      )

    result <- bind_rows(result, filtered_citations)
    cat(
      "Added",
      nrow(filtered_citations),
      "filtered citations as separate entries\n"
    )
  }

  return(result)
}


#' Apply reference normalization to bibliometrix data frame
#'
#' This is a convenience wrapper function that applies \code{\link{normalize_citations}}
#' to a bibliometrix data frame (typically loaded with \code{\link{convert2df}}). It
#' extracts citations from the CR field, performs normalization and matching, and
#' returns comprehensive results including per-paper citation lists and summary statistics.
#'
#' The function automatically handles the new Scopus citation format (where the year
#' appears at the end in parentheses) by converting it to the classic format before
#' processing.
#'
#' @param M A bibliometrix data frame, typically created by \code{\link{convert2df}}.
#'   Must contain the columns:
#'   \itemize{
#'     \item \code{SR}: Short reference identifier for each document
#'     \item \code{CR}: Cited references field (citations separated by semicolons)
#'     \item \code{DB}: (Optional) Database source identifier for format detection
#'   }
#' @param threshold Numeric value between 0 and 1 indicating the similarity threshold
#'   for matching citations. Default is 0.90. See \code{\link{normalize_citations}}
#'   for details on selecting appropriate thresholds.
#' @param method String distance method to use for fuzzy matching. Options include:
#'   \itemize{
#'     \item "jw" (default): Jaro-Winkler distance, optimized for bibliographic strings
#'     \item "lv": Levenshtein distance
#'     \item "osa": Optimal String Alignment distance
#'     \item "lcs": Longest Common Subsequence distance
#'     \item Other methods supported by \code{\link[stringdist]{stringdistmatrix}}
#'   }
#' @param min_chars Minimum characters for valid citations (default: 20).
#' @param max_block_size Integer. Blocks with at least this many unique normalized
#'   strings skip within-block fuzzy matching and fall back to exact matching only
#'   (default: 100). Passed to \code{\link{normalize_citations}}.
#' @param use_iso4 Logical. Enable ISO 4 / LTWA journal normalization (default \code{TRUE}).
#' @param use_doi Logical. Enable DOI-based exact matching (default \code{TRUE}).
#' @param use_exact Logical. Enable exact normalized-string and punctuation-invariant
#'   matching (default \code{TRUE}).
#' @param fuzzy Logical. Enable within-block (WoS deterministic and Scopus fuzzy)
#'   matching (default \code{TRUE}).
#' @param use_postproc Logical. Enable Phase 4.5 metadata-based post-processing
#'   merge (default \code{TRUE}). These five logical switches are intended primarily
#'   for ablation analyses and are passed through to \code{\link{normalize_citations}}.
#' @param title_guard Logical. Enable the optional Phase 4.6 series part-marker
#'   cluster purification that splits over-merged multi-part works (default
#'   \code{FALSE}). Passed through to \code{\link{normalize_citations}}.
#' @param use_title Logical. Compare references on their title (default \code{TRUE}).
#'   Passed through to \code{\link{normalize_citations}}.
#' @param merge_editions Logical. Count the same title in years further apart, such as
#'   the editions of a book, as one work (default \code{FALSE}). Passed through to
#'   \code{\link{normalize_citations}}.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Splits the CR field by semicolons to extract individual citations
#'   \item Detects and converts new Scopus format citations to classic format
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
#'       \item \code{similarity}: Similarity of the title to that of the canonical citation
#'     }
#'   }
#'   \item{summary}{A data frame summarizing citation frequencies with columns:
#'     \itemize{
#'       \item \code{CR_canonical}: The canonical citation for each cluster
#'       \item \code{n}: Total number of times this work was cited
#'       \item \code{n_variants}: Number of different formatting variants found
#'       \item \code{min_similarity}: The lowest similarity among the variants, to find
#'         the merges worth checking; see \code{\link{splitReferenceVariant}} to undo one
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
#' # Apply reference normalization
#' results <- applyReferenceMatching(M, threshold = 0.85)
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

applyReferenceMatching <- function(
  M,
  threshold = 0.90,
  method = "jw",
  min_chars = 20,
  max_block_size = 100,
  use_iso4 = TRUE,
  use_doi = TRUE,
  use_exact = TRUE,
  fuzzy = TRUE,
  use_postproc = TRUE,
  title_guard = FALSE,
  use_title = TRUE,
  merge_editions = FALSE
) {
  # Extract citations
  CR <- strsplit(M$CR, ";")
  CR_df <- tibble(
    SR = rep(M$SR, lengths(CR)),
    CR = trimws(unlist(CR))
  ) %>%
    # A document without references (CR = NA or "") has nothing to match
    dplyr::filter(!is.na(CR), nzchar(CR))

  # === NEW: Pre-processing for new Scopus format ===
  if ("DB" %in% names(M)) {
    # Identify citations from Scopus
    is_scopus <- M$DB == "SCOPUS"
    scopus_sr <- M$SR[is_scopus]

    if (length(scopus_sr) > 0) {
      # Convert only citations from Scopus database
      CR_df <- CR_df %>%
        mutate(
          CR = if_else(
            SR %in% scopus_sr,
            sapply(CR, convert_scopus_new_to_classic, USE.NAMES = FALSE),
            CR
          )
        )

      # Report conversions
      n_scopus_new <- sum(
        sapply(CR_df$CR[CR_df$SR %in% scopus_sr], function(x) {
          # Count how many were actually converted (now show as "scopus" format)
          str_detect(x, "\\(\\d{4}\\)\\s+[A-Z]")
        })
      )

      if (n_scopus_new > 0) {
        cat(
          "Pre-processing: Converted",
          n_scopus_new,
          "citations from new Scopus format to classic format\n"
        )
      }
    }
  }

  # Apply normalization (rest remains unchanged)
  cat("\n=== CITATION NORMALIZATION ===\n")
  matched <- normalize_citations(
    CR_df$CR,
    threshold = threshold,
    method = method,
    min_chars = min_chars,
    max_block_size = max_block_size,
    use_iso4 = use_iso4,
    use_doi = use_doi,
    use_exact = use_exact,
    fuzzy = fuzzy,
    use_postproc = use_postproc,
    title_guard = title_guard,
    use_title = use_title,
    merge_editions = merge_editions
  )

  # Join with SR. normalize_citations() returns one row per occurrence, so a
  # reference cited k times would join k x k rows. Occurrences of one string
  # share their match (only FILTERED_ ids differ, and those rows are dropped
  # below), so one row per string gives the same result.
  result <- CR_df %>%
    left_join(
      dplyr::distinct(matched, CR_original, .keep_all = TRUE),
      by = c("CR" = "CR_original")
    )

  # Remove invalid entries: filtered citations and short canonical forms
  result <- result %>%
    dplyr::filter(
      !is.na(cluster_id),
      !grepl("^FILTERED_", cluster_id),
      nchar(CR_canonical) >= min_chars
    )

  # Reference citation counts
  citation_count <- result %>% distinct() %>% count(CR_canonical, sort = TRUE)

  # Create summary
  summary <- result %>%
    group_by(CR_canonical, cluster_id) %>%
    summarise(
      n_variants = n_distinct(CR),
      format = first(format),
      first_author = first(first_author),
      year = first(year),
      journal_iso4 = first(journal_iso4), # Add ISO4 journal
      journal_original = first(journal_original), # Add original journal
      min_similarity = if (all(is.na(similarity))) NA_real_ else min(similarity, na.rm = TRUE),
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
  cat(
    "Duplicate citations removed:",
    length(unique(result$CR)) - length(unique(result$CR_canonical)),
    "\n"
  )

  return(list(
    full_data = result %>% distinct(),
    summary = summary,
    matched_citations = matched,
    CR_normalized = CR_by_paper
  ))
}


#' @title applyCitationMatching (Deprecated)
#'
#' @description
#' \strong{Deprecated} in bibliometrix 6.0. This function has been renamed to
#' \code{\link{applyReferenceMatching}}. Please use \code{applyReferenceMatching()} instead.
#'
#' @inheritParams applyReferenceMatching
#'
#' @return Same as \code{\link{applyReferenceMatching}}.
#'
#' @export
applyCitationMatching <- function(
  M,
  threshold = 0.90,
  method = "jw",
  min_chars = 20,
  max_block_size = 100,
  use_iso4 = TRUE,
  use_doi = TRUE,
  use_exact = TRUE,
  fuzzy = TRUE,
  use_postproc = TRUE,
  title_guard = FALSE,
  use_title = TRUE,
  merge_editions = FALSE
) {
  warning(
    "applyCitationMatching() is deprecated as of bibliometrix 6.0. ",
    "Please use applyReferenceMatching() instead.",
    call. = FALSE
  )
  applyReferenceMatching(
    M = M,
    threshold = threshold,
    method = method,
    min_chars = min_chars,
    max_block_size = max_block_size,
    use_iso4 = use_iso4,
    use_doi = use_doi,
    use_exact = use_exact,
    fuzzy = fuzzy,
    use_postproc = use_postproc,
    title_guard = title_guard,
    use_title = use_title,
    merge_editions = merge_editions
  )
}


#' Separate a variant from the citation it was merged into
#'
#' It undoes one merge in the result of \code{\link{applyReferenceMatching}}, without
#' running the matching again: the given reference string becomes a work of its own.
#' Find the merges worth checking with the \code{min_similarity} column of
#' \code{results$summary}, and the variants with the \code{similarity} column of
#' \code{results$full_data}.
#'
#' @param results The list returned by \code{\link{applyReferenceMatching}}.
#' @param variant Character vector. The reference strings to separate, as in the
#'   \code{CR} column of \code{results$full_data}.
#'
#' @return The same list, with \code{full_data}, \code{summary} and \code{CR_normalized}
#'   updated. Each variant keeps its own canonical form, its own cluster and a similarity
#'   of 1. If it was the canonical citation of its cluster, the most frequent remaining
#'   variant takes its place.
#'
#' @examples
#' \dontrun{
#' results <- applyReferenceMatching(M)
#'
#' # The merges with the least similar variants
#' head(results$summary[order(results$summary$min_similarity), ])
#'
#' results <- splitReferenceVariant(results, "GENESEE F., A WRONGLY MERGED WORK (2004)")
#' }
#'
#' @seealso \code{\link{applyReferenceMatching}}
#'
#' @export
splitReferenceVariant <- function(results, variant) {
  full_data <- results$full_data
  unknown <- setdiff(variant, full_data$CR)
  if (length(unknown) > 0) {
    stop(
      "Not among the references of these results: ",
      paste(unknown, collapse = "; "),
      call. = FALSE
    )
  }

  for (v in unique(variant)) {
    rows <- full_data$CR == v
    old_canonical <- full_data$CR_canonical[rows][1]
    old_cluster <- full_data$cluster_id[rows][1]

    # A variant that was the canonical citation hands that role over
    others <- full_data$cluster_id == old_cluster & !rows
    if (identical(old_canonical, v) && any(others)) {
      counts <- sort(table(full_data$CR[others]), decreasing = TRUE)
      full_data$CR_canonical[others] <- names(counts)[1]
      full_data$similarity[others & full_data$CR == names(counts)[1]] <- 1
    }

    full_data$CR_canonical[rows] <- v
    full_data$cluster_id[rows] <- paste0("SPLIT_", old_cluster, "_", match(v, variant))
    full_data$n_cluster[rows] <- sum(rows)
    full_data$similarity[rows] <- 1
    full_data$n_cluster[others] <- sum(others)
  }

  citation_count <- full_data %>% distinct() %>% count(CR_canonical, sort = TRUE)
  results$summary <- full_data %>%
    group_by(CR_canonical, cluster_id) %>%
    summarise(
      n_variants = n_distinct(CR),
      format = first(format),
      first_author = first(first_author),
      year = first(year),
      journal_iso4 = first(journal_iso4),
      journal_original = first(journal_original),
      min_similarity = if (all(is.na(similarity))) NA_real_ else min(similarity, na.rm = TRUE),
      variants_example = paste(head(unique(CR), 3), collapse = " | "),
      .groups = "drop"
    ) %>%
    left_join(citation_count, by = "CR_canonical") %>%
    arrange(desc(n))
  results$CR_normalized <- full_data %>%
    group_by(SR) %>%
    summarise(
      CR = paste(unique(CR_canonical), collapse = "; "),
      n_references = n_distinct(CR_canonical),
      .groups = "drop"
    )
  results$full_data <- full_data
  results
}


# The title of a reference with its year in parentheses, in the form
# "AUTHOR I., AUTHOR J., TITLE, CONTAINER (YEAR) SOURCE, VOLUME, PP. 1-2".
# Returns the year, the normalised text between the authors and the year (title), and
# its first segment (title_first): a chapter's title without its container.
parse_reference_titles <- function(x) {
  words <- "[A-Z][A-Z'\\-]*"
  # "GARCIA O.", "LIN A.F.M.", "JOHNSON S. I.", "IBARRA JOHNSON S.", "TLEMISSOVA ZH"
  author <- paste0(
    "^", words, "( ", words, ")*( ([A-Z]\\.( ?[A-Z]\\.)*|[A-Z]|[B-DF-HJ-NP-TV-Z]{2,3}))$"
  )
  initials <- "^([A-Z]\\. ?)+$|^[A-Z]{1,2}$"
  clean <- function(t) {
    t <- gsub("[^A-Z0-9 ]", " ", gsub("'", "", t))
    trimws(gsub(" +", " ", t))
  }

  parse_one <- function(ref) {
    out <- c(year = NA_character_, title = NA_character_, title_first = NA_character_)
    m <- regexec("^(.*?)\\s*\\((\\d{4})\\)", ref, perl = TRUE)
    hit <- regmatches(ref, m)[[1]]
    if (length(hit) < 3) return(out)
    out["year"] <- hit[3]

    # A note in parentheses is no part of the title: "... (PALGRAVE PIVOT)"
    segments <- trimws(strsplit(gsub("\\([^()]*\\)", " ", hit[2]), ",")[[1]])
    segments <- segments[nzchar(segments)]
    if (length(segments) == 0) return(out)

    # The first segment is always the first author; "SMITH, J." takes two
    i <- 1L
    if (length(segments) > 1 && grepl(initials, segments[2])) i <- 2L
    while (i < length(segments)) {
      nxt <- segments[i + 1L]
      if (grepl(author, nxt)) {
        i <- i + 1L
      } else if (i + 2L <= length(segments) && grepl(initials, segments[i + 2L]) &&
        grepl(paste0("^", words, "( ", words, ")*$"), nxt)) {
        i <- i + 2L
      } else {
        break
      }
    }
    if (i >= length(segments)) return(out)

    rest <- segments[(i + 1L):length(segments)]
    title <- clean(paste(rest, collapse = " "))
    if (nchar(title) < 5) return(out)
    out["title"] <- title
    out["title_first"] <- clean(rest[1])
    out
  }

  unique_refs <- unique(x)
  upper <- toupper(vapply(unique_refs, remove_diacritics, character(1), USE.NAMES = FALSE))
  parsed <- vapply(upper, parse_one, character(3), USE.NAMES = FALSE)
  rows <- match(x, unique_refs)
  list(
    year = parsed[1, rows],
    title = parsed[2, rows],
    title_first = parsed[3, rows]
  )
}


# Words that do not occur in a name: a segment holding one of them is not an author.
reference_stop_words <- c(
  "AND", "OR", "OF", "THE", "IN", "ON", "FOR", "A", "AN", "TO", "WITH", "AT", "FROM",
  "BY", "AS", "VS"
)


# Part numbers in a title: "... PART II" and "... PART III" are different works.
reference_parts <- function(title) {
  tokens <- strsplit(title, " ", fixed = TRUE)[[1]]
  sort(unique(tokens[grepl("^([0-9]+|II|III|IV|VI|VII|VIII|IX)$", tokens)]))
}


# Word overlap between all the pairs of a vector of titles.
reference_overlap <- function(titles) {
  tokens <- strsplit(titles, " ", fixed = TRUE)
  n <- length(titles)
  overlap <- diag(n)
  for (a in seq_len(n - 1L)) {
    for (b in (a + 1L):n) {
      shared <- length(intersect(tokens[[a]], tokens[[b]]))
      overlap[a, b] <- overlap[b, a] <-
        shared / max(1L, length(union(tokens[[a]], tokens[[b]])))
    }
  }
  overlap
}


# Which pairs of titles are close enough. A string distance alone says too much on
# long titles with a common start ("TRANSLANGUAGING AND EDUCATION" and "TRANSLANGUAGING
# IN EDUCATION"), so the words must overlap as well, unless the edit is tiny.
reference_titles_close <- function(titles, threshold, method) {
  close <- stringdist::stringsimmatrix(titles, titles, method = method) >= threshold
  edit <- stringdist::stringsimmatrix(titles, titles, method = "lv")
  close & (reference_overlap(titles) >= 0.6 | edit >= 0.95)
}


# Cluster the references of a block that hold a title. Each distinct combination of
# title, volume and first page is one point; complete linkage keeps apart any pair
# that conflicts. Other references are left to the caller, and DOI clusters stay.
cluster_by_title <- function(block_df, threshold, method, max_block_size) {
  rows <- which(block_df$has_title & !grepl("^DOI_", coalesce(block_df$cluster_id, "")))
  if (length(rows) == 0) return(block_df)

  key <- paste(
    block_df$title_norm[rows],
    coalesce(block_df$volume[rows], ""),
    coalesce(block_df$page_start_ref[rows], ""),
    sep = "\r"
  )
  points <- which(!duplicated(key))
  n <- length(points)
  label <- unique(block_df$blocking_key)[1]

  # Too many to compare: only the same title, volume and page stay together
  if (n < 2 || n >= max_block_size) {
    block_df$cluster_id[rows] <- paste0(label, "_T", match(key, key[points]))
    return(block_df)
  }

  title <- block_df$title_norm[rows][points]
  first <- block_df$title_first[rows][points]
  volume <- block_df$volume[rows][points]
  page <- block_df$page_start_ref[rows][points]

  # A co-author written in full ("WEI LI") is not recognised as a name, and stays at
  # the start of the title. If what follows it is the title of another reference of
  # the same kind, that is the title. Up to two words, none of them common: what comes
  # before the title of a book, in a chapter, is a title and not a name.
  has_page <- !is.na(page)
  for (i in order(nchar(title), decreasing = TRUE)) {
    others <- which(
      nchar(title) >= 20 & nchar(title) < nchar(title[i]) & has_page == has_page[i] &
        endsWith(title[i], paste0(" ", title))
    )
    for (o in others[order(-nchar(title[others]))]) {
      extra <- strsplit(trimws(substr(
        title[i], 1, nchar(title[i]) - nchar(title[o])
      )), " ", fixed = TRUE)[[1]]
      if (length(extra) <= 2 && !any(extra %in% reference_stop_words)) {
        title[i] <- title[o]
        first[i] <- first[o]
        break
      }
    }
  }

  same <- reference_titles_close(title, threshold, method)

  # A chapter cited with different containers: the first segment decides, if long
  long_first <- nchar(first) >= 20
  if (sum(long_first) > 1) {
    same_first <- reference_titles_close(first, threshold, method)
    same_first[!long_first, ] <- FALSE
    same_first[, !long_first] <- FALSE
    same <- same | same_first

    # With the same first page, overlapping words are enough: "LANGUAGE PRACTICES IN
    # KENYA" and "LANGUAGE CLASSROOM PRACTICES IN KENYA", both at PP. 131-152. Page 1
    # says nothing: most reports and articles paged on their own start there.
    same_page <- outer(page, page, "==")
    same_page[is.na(same_page)] <- FALSE
    same_page[page %in% "1", ] <- FALSE
    same <- same | (same_page & reference_overlap(first) >= 0.6)
  }

  # A chapter (pages, no journal after the year) against a book (no pages): the title
  # of one holds the title of the other, so only a tiny edit makes them one work.
  is_chapter <- has_page & is.na(block_df$journal[rows][points])
  mixed <- outer(is_chapter, !has_page, "&")
  mixed <- mixed | t(mixed)
  same[mixed] <- (stringdist::stringsimmatrix(title, title, method = "lv") >= 0.95)[mixed]

  parts <- lapply(first, reference_parts)
  for (a in seq_len(n - 1L)) {
    for (b in (a + 1L):n) {
      conflict <- (!is.na(volume[a]) && !is.na(volume[b]) && volume[a] != volume[b]) ||
        (!is.na(page[a]) && !is.na(page[b]) && page[a] != page[b] && title[a] != title[b]) ||
        !identical(parts[[a]], parts[[b]])

      if (conflict) same[a, b] <- same[b, a] <- FALSE
    }
  }

  groups <- stats::cutree(
    stats::hclust(stats::as.dist(1 - (same * 1)), method = "complete"),
    h = 0.5
  )
  block_df$cluster_id[rows] <- paste0(label, "_T", groups[match(key, key[points])])
  block_df
}


# Merge the clusters holding the same long title in the year most cited and the years
# next to it, unless their volume or first page disagree. Years further away are
# other editions, and stay apart unless merge_editions is TRUE.
merge_adjacent_years <- function(df, merge_editions = FALSE) {
  rows <- which(df$has_title & nchar(coalesce(df$title_norm, "")) >= 25)
  if (length(rows) == 0) return(df)

  key <- paste(gsub("\\s.*", "", df$first_author[rows]), df$title_norm[rows])
  groups <- split(rows, key)
  groups <- groups[lengths(groups) > 1]
  mapping <- character(0)
  for (g in groups) {
    years <- as.numeric(df$year[g])
    if (length(unique(years)) < 2) next
    counts <- table(years)
    main <- as.numeric(names(counts)[which.max(counts)])
    if (!isTRUE(merge_editions)) g <- g[abs(years - main) <= 1]
    if (length(unique(df$year[g])) < 2) next
    if (dplyr::n_distinct(stats::na.omit(df$volume[g])) > 1) next
    if (dplyr::n_distinct(stats::na.omit(df$page_start_ref[g])) > 1) next
    ids <- unique(df$cluster_id[g])
    mapping[ids] <- min(ids)
  }
  if (length(mapping) > 0) {
    found <- df$cluster_id %in% names(mapping)
    df$cluster_id[found] <- mapping[df$cluster_id[found]]
  }
  df
}


# Similarity between the whole title of each reference and that of the canonical one
# of its cluster: 1 for the canonical string itself, NA with no title to compare. It
# stays low on merges resting on less than the whole title, the ones worth checking.
reference_similarity <- function(title, canonical, is_canonical, method) {
  out <- stringdist::stringsim(title, canonical, method = method)
  out[is_canonical] <- 1
  round(out, 3)
}
