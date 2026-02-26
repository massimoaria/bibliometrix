utils::globalVariables(c("all_of", "corr", "DI", "C1", "id_oa", "RP", "UN", "AU_ID", "corresponding_author_ids", "References", "mutate_all", "AU_CORRESPONDING", "AU_CO", "AU"))

csvOA2df <- function(file) {
  options(readr.num_columns = 0)

  ## import all files in a single data frame
  for (i in 1:length(file)) {
    # D <- read.csv(file[i], quote='"', check.names = F, stringsAsFactors = F) #fileEncoding = "UTF-8-BOM")
    # D <- read_csv(file[i], na=character(), quote='"', trim_ws = FALSE, progress = show_progress(), show_col_types = FALSE) %>%
    #   mutate(across(where(is.numeric), as.character)) %>%
    #   mutate(across(where(is.character), \(x) tidyr::replace_na(x,""))) %>%
    #   as.data.frame()
    D <- read_csv(file[i], na = character(), quote = '"', trim_ws = FALSE, progress = show_progress(), show_col_types = FALSE) %>%
      mutate(across(where(is.numeric), as.character)) %>%
      mutate(across(where(is.character), function(x) tidyr::replace_na(x, ""))) %>%
      as.data.frame()

    if (i > 1) {
      l <- intersect(l, names(D))
      DATA <- rbind(DATA[l], D[l])
    } else {
      l <- names(D)
      DATA <- D
    }
  }
  rm(D)

  ## Post-Processing

  # column re-labelling
  DATA <- relabelling_OA(DATA)

  # Save original column names before pipe replacement (for dual-path detection)
  DATA_ORIG_NAMES <- names(DATA)

  DATA <- DATA %>% distinct(id_oa, .keep_all = TRUE)

  # replace | with ;
  DATA <- DATA %>%
    mutate(across(where(is.character), ~ stringi::stri_replace_all_regex(., "\\|", ";")))

  # Defensive checks for columns that may be missing in new format
  missing_cols <- c()
  if (!"CR" %in% names(DATA)) {
    DATA$CR <- NA
    missing_cols <- c(missing_cols, "Cited References (CR)")
  }
  if (!"NR" %in% names(DATA)) DATA$NR <- NA
  if (!"VL" %in% names(DATA)) DATA$VL <- NA
  if (!"IS" %in% names(DATA)) DATA$IS <- NA
  if (!"DE" %in% names(DATA)) DATA$DE <- NA
  if (!"ID" %in% names(DATA)) DATA$ID <- NA
  if (!"TOPICS" %in% names(DATA)) DATA$TOPICS <- NA
  if (!"WC" %in% names(DATA)) DATA$WC <- NA
  if (!"SDG" %in% names(DATA)) DATA$SDG <- NA
  if (!"MESH" %in% names(DATA)) DATA$MESH <- NA
  if (!"SO_ID" %in% names(DATA)) DATA$SO_ID <- NA
  if (!"AU_CO" %in% names(DATA)) DATA$AU_CO <- NA

  if (length(missing_cols) > 0) {
    warning(
      "\nThe OpenAlex CSV does not contain the following fields: ",
      paste(missing_cols, collapse = ", "), ".",
      "\nSome analyses (e.g., citation network, co-citation, bibliographic coupling) may not be available.",
      "\nTo get full metadata, consider using the OpenAlex API import instead.",
      call. = FALSE
    )
  }

  DATA$AF <- DATA$AU
  if (!"AB" %in% names(DATA)) DATA$AB <- ""

  # Protected gsub for OpenAlex URL prefixes
  if ("CR" %in% names(DATA) && !all(is.na(DATA$CR))) {
    DATA$CR <- gsub("https://openalex.org/", "", DATA$CR)
  }
  if ("AU_ID" %in% names(DATA)) {
    DATA$AU_ID <- gsub("https://openalex.org/", "", DATA$AU_ID)
  }
  DATA$id_oa <- gsub("https://openalex.org/", "", DATA$id_oa)
  if (!all(is.na(DATA$SO_ID))) {
    DATA$JI <- DATA$J9 <- gsub("https://openalex.org/", "", DATA$SO_ID)
  } else {
    DATA$JI <- DATA$J9 <- NA
  }
  if ("corresponding_author_ids" %in% names(DATA)) {
    DATA$corresponding_author_ids <- gsub("https://openalex.org/", "", DATA$corresponding_author_ids)
  }
  DATA$DB <- "OPENALEX"

  ## Affiliations — dual path: old vs new format
  if ("authorships.affiliations" %in% DATA_ORIG_NAMES) {
    # Old format: parse JSON-like affiliations string
    DATA <- extract_collapsed_affiliations(DATA$authorships.affiliations, DATA$id_oa) %>%
      right_join(DATA, by = "id_oa")
  } else if ("C1_NAMES" %in% names(DATA)) {
    # New format: pipe-separated values already converted to ; by mutate above
    DATA$C1 <- DATA$C1_NAMES
    DATA$C1_ID <- gsub("https://openalex.org/", "", DATA$C1_IDS)
  } else {
    DATA$C1 <- NA
    DATA$C1_ID <- NA
  }

  ## Corresponding author — dual path: old vs new format
  if ("corresponding_author_ids" %in% DATA_ORIG_NAMES) {
    # Old format: use replace_corresponding_info()
    CORR_INFO <- replace_corresponding_info(DATA[, c("corresponding_author_ids", "corresponding_institution_ids", "AU", "AU_ID", "C1", "C1_ID", "AU_CO")]) %>%
      select("RP", "AU1_CO", "corresponding_author_name")
    DATA <- bind_cols(DATA, CORR_INFO)
  } else if ("AU_CORRESPONDING" %in% names(DATA)) {
    # New format: find corresponding author from is_corresponding flags
    DATA <- DATA %>%
      rowwise() %>%
      mutate(
        corresponding_author_name = {
          corr_flags <- trimws(unlist(strsplit(AU_CORRESPONDING, ";")))
          authors <- trimws(unlist(strsplit(AU, ";")))
          idx <- which(toupper(corr_flags) == "TRUE")
          if (length(idx) > 0 && idx[1] <= length(authors)) authors[idx[1]] else NA_character_
        },
        RP = {
          corr_flags <- trimws(unlist(strsplit(AU_CORRESPONDING, ";")))
          affils <- trimws(unlist(strsplit(ifelse(is.na(C1), "", C1), ";")))
          idx <- which(toupper(corr_flags) == "TRUE")
          if (length(idx) > 0 && idx[1] <= length(affils)) affils[idx[1]] else NA_character_
        },
        AU1_CO = {
          corr_flags <- trimws(unlist(strsplit(AU_CORRESPONDING, ";")))
          countries_list <- trimws(unlist(strsplit(ifelse(is.na(AU_CO), "", AU_CO), ";")))
          idx <- which(toupper(corr_flags) == "TRUE")
          if (length(idx) > 0 && idx[1] <= length(countries_list)) countries_list[idx[1]] else NA_character_
        }
      ) %>%
      ungroup()
  } else {
    DATA$corresponding_author_name <- NA
    DATA$RP <- NA
    DATA$AU1_CO <- NA
  }

  # move all char strings to Upper
  ind <- apply(DATA, 2, function(x) {
    sum(regexpr("https://", x) > -1, na.rm = TRUE) > 0
  })
  label <- names(ind)[ind == FALSE & !is.na(ind)]

  AB <- DATA$AB
  TI <- DATA$TI
  DE <- DATA$DE
  DATA <- DATA %>%
    mutate(across(all_of(label), toupper),
      DI = gsub("https://doi.org/", "", DI),
      DI = ifelse(DI == "null", NA, DI)
    )
  DATA$SO <- toupper(DATA$SO)
  DATA$SO[DATA$SO == "NAN"] <- NA
  if ("C1" %in% names(DATA)) DATA$C1 <- toupper(DATA$C1)
  if ("RP" %in% names(DATA)) DATA$RP <- toupper(DATA$RP)
  if ("AU_CO" %in% names(DATA) && !all(is.na(DATA$AU_CO))) DATA$AU_CO <- toupper(DATA$AU_CO)
  if ("AU1_CO" %in% names(DATA) && !all(is.na(DATA$AU1_CO))) DATA$AU1_CO <- toupper(DATA$AU1_CO)
  data("countries", envir = environment())
  DATA$AU_CO <- convert_iso2_to_country(DATA$AU_CO, countries)
  DATA$AU1_CO <- convert_iso2_to_country(DATA$AU1_CO, countries)
  DATA$AB_raw <- AB
  DATA$TI_raw <- TI
  DATA$DE_raw <- DE

  return(DATA %>% as.data.frame())
}

relabelling_OA <- function(DATA) {
  ## column re-labelling
  label <- names(DATA)
  label[label %in% "id"] <- "id_oa"
  label[label %in% "title"] <- "TI"
  label[label %in% "primary_location.source.display_name"] <- "SO"
  # label[label %in% "locations.source.display_name"] <- "SO"
  label[label %in% "primary_location.source.id"] <- "SO_ID"
  # label[label %in% "locations.source.id"] <- "SO_ID"
  label[label %in% "primary_location.source.host_organization_name"] <- "PU"
  label[label %in% "primary_location.source.issn"] <- "ISSN"
  label[label %in% "primary_location.source.issn_l"] <- "ISSN_I"
  label[label %in% "primary_location.landing_page_url"] <- "URL"
  label[label %in% "primary_location.pdf_url"] <- "URL_PDF"
  # label[label %in% "author_ids"] <- "AU_ID"
  label[label %in% "authorships.author.id"] <- "AU_ID"
  label[label %in% "authorships.countries"] <- "AU_CO"
  # label[label %in% "author_names"] <- "AU"
  label[label %in% "authorships.author.display_name"] <- "AU"
  label[label %in% "authorships.author.orcid"] <- "OI"
  # label[label %in% "author_institution_names"] <- "C3"
  label[label %in% "cited_by_count"] <- "TC"
  label[label %in% "publication_year"] <- "PY"
  label[label %in% "type"] <- "DT"
  label[label %in% "biblio.issue"] <- "IS"
  label[label %in% "biblio.volume"] <- "VL"
  label[label %in% "referenced_works"] <- "CR"
  # label[label %in% "keywords_display_name"] <- "DE"
  label[label %in% "keywords.display_name"] <- "DE"
  label[label %in% "abstract"] <- "AB"
  label[label %in% "concepts.display_name"] <- "ID"
  label[label %in% "topics.display_name"] <- "TOPICS"
  label[label %in% "sustainable_development_goals.display_name"] <- "SDG"
  label[label %in% "topics.field.display_name"] <- "WC"
  label[label %in% "mesh.descriptor_name"] <- "MESH"
  label[label %in% "referenced_works_count"] <- "NR"
  label[label %in% "language"] <- "LA"
  label[label %in% "authorships.author_position"] <- "AU_POSITION"
  # label[label %in% "authorships.countries"] <- "C1"
  label[label %in% "doi"] <- "DI"

  # New OpenAlex CSV format mappings (backward-compatible)
  label[label %in% "display_name"] <- "TI"
  label[label %in% "primary_topic.display_name"] <- "WC"
  label[label %in% "fwci"] <- "FWCI"
  label[label %in% "ids.pmid"] <- "PMID"
  label[label %in% "is_retracted"] <- "RETRACTED"
  label[label %in% "funders.display_name"] <- "FU"
  label[label %in% "publication_date"] <- "PD"
  label[label %in% "authorships.institutions.display_name"] <- "C1_NAMES"
  label[label %in% "authorships.institutions.id"] <- "C1_IDS"
  label[label %in% "authorships.is_corresponding"] <- "AU_CORRESPONDING"
  label[label %in% "best_oa_location.license"] <- "OA_LICENSE"
  label[label %in% "open_access.is_oa"] <- "OA"
  label[label %in% "open_access.oa_status"] <- "OA_STATUS"

  names(DATA) <- label
  return(DATA)
}

TrimMult <- function(x, char = " ") {
  return(gsub(paste0("^", char, "*|(?<=", char, ")", char, "|", char, "*$"),
    "", x,
    perl = T
  ))
}

# Funzione aggiornata per estrarre affiliazioni, includere i paesi e rimuovere i caratteri `"`
extract_collapsed_affiliations <- function(affiliations, id_oa) {
  map2_dfr(affiliations, id_oa, ~ {
    # Rimuovi `{}`, `;`, e `'`, quindi dividi per `raw_affiliation_string: `
    affil_split <- stringr::str_split(stringr::str_replace_all(.x, "\\{|\\}|\\'|;", ""), "raw_affiliation_string: ")[[1]]

    # Rimuovi stringhe vuote e processa ciascun elemento per ottenere nome, ID e paese
    affil_df <- map_df(affil_split[affil_split != ""], function(affil) {
      # Estrai il nome completo dell'affiliazione
      affiliation_name <- stringr::str_trim(stringr::str_extract(affil, "^.*?(?=, institution_ids|$)"))

      # Estrai `institution_ids` e rimuovi il prefisso `https://openalex.org/`
      institution_ids <- stringr::str_extract(affil, "(?<=institution_ids: \\[)[^\\]]+") %>%
        stringr::str_replace_all("https://openalex.org/", "")

      # Estrai il paese come ultima parola dopo l'ultima virgola della stringa
      # country <- stringr::str_trim(stringr::str_extract(affiliation_name, "(?<=, )[^,]+$"))

      tibble(
        article_id = .y, # Assegna l'ID dell'articolo a ogni riga
        affiliation_name = ifelse(is.na(affiliation_name) || affiliation_name == "", NA, affiliation_name),
        institution_ids = ifelse(is.na(institution_ids) || institution_ids == "", NA, institution_ids) # ,
        # countries = ifelse(is.na(country) || country == "", NA, country)
      )
    })

    # Collassa tutte le affiliazioni, gli ID istituzionali e i paesi in singole stringhe separate da `;`
    df <- tibble(
      id_oa = .y, # Includi l'ID dell'articolo
      C1 = ifelse(nrow(affil_df) == 0, NA, paste(affil_df$affiliation_name, collapse = "; ")),
      C1_ID = ifelse(nrow(affil_df) == 0, NA, paste(affil_df$institution_ids, collapse = "; ")) # ,
      # AU_CO = ifelse(nrow(affil_df) == 0, NA, paste(affil_df$countries[!is.na(affil_df$countries)], collapse = "; "))
    )

    # Rimozione dei caratteri `"` da tutte le colonne
    df <- df %>% mutate_all(~ stringr::str_replace_all(., "\"", ""))
  })
}

# Funzione aggiornata per sostituire corresponding_author_ids e corresponding_institution_ids con nome e affiliazione
replace_corresponding_info <- function(data) {
  data$corresponding_author_ids <- unlist(lapply(strsplit(data$corresponding_author_ids, ";"), function(l) l[1]))
  data %>%
    rowwise() %>%
    mutate(
      # Trova il nome dell'autore corrispondente
      corresponding_author_name = ifelse(
        !is.na(corresponding_author_ids) && corresponding_author_ids != "",
        {
          # Ottieni la lista di autori e ID
          author_names <- stringr::str_split(AU, ";")[[1]]
          author_ids <- stringr::str_split(AU_ID, ";")[[1]]
          # Trova l'indice dell'ID autore corrispondente
          matching_index <- which(author_ids == corresponding_author_ids, "")
          if (length(matching_index) > 0) author_names[matching_index] else NA_character_
        },
        NA_character_
      ),

      # Trova l'affiliazione dell'autore corrispondente
      corresponding_author_affiliation = ifelse(
        !is.na(corresponding_institution_ids) && corresponding_institution_ids != "",
        {
          # Ottieni la lista di affiliazioni e ID
          institution_names <- stringr::str_split(C1, ";")[[1]]
          institution_ids <- stringr::str_split(C1_ID, ";")[[1]] %>% stringr::str_trim()
          # Rimuovi il prefisso dell'ID istituzionale per la corrispondenza
          corresponding_ids <- stringr::str_split(corresponding_institution_ids, ";")[[1]] %>%
            stringr::str_replace_all("https://openalex.org/", "")
          # Trova le affiliazioni corrispondenti e uniscile
          matching_affiliations <- institution_names[institution_ids %in% corresponding_ids]
          if (length(matching_affiliations) > 0) paste(matching_affiliations, collapse = "; ") else NA_character_
        },
        NA_character_
      ),

      # Trova la nazione dell'autore corrispondente
      corresponding_author_country = ifelse(
        !is.na(corresponding_author_ids) && corresponding_author_ids != "",
        {
          # Ottieni la lista delle nazioni e degli ID
          author_countries <- stringr::str_split(AU_CO, ";")[[1]]
          author_ids <- stringr::str_split(AU_ID, ";")[[1]]
          # Trova l'indice dell'ID autore corrispondente
          matching_index <- which(author_ids == corresponding_author_ids)
          if (length(matching_index) > 0) author_countries[matching_index] else NA_character_
        },
        NA_character_
      )
    ) %>%
    # Rimuove le colonne originali degli ID e rinomina
    select(-corresponding_author_ids, -corresponding_institution_ids) %>%
    rename(
      corresponding_author_name = corresponding_author_name,
      RP = corresponding_author_affiliation,
      AU1_CO = corresponding_author_country
    ) %>%
    ungroup()
}

convert_iso2_to_country <- function(df_column, countries_df) {
  # Crea una named vector per la conversione
  iso_to_country <- setNames(countries_df$countries, countries_df$iso2)
  
  # Funzione per tradurre un singolo elemento (es: "IT;IT;FR")
  translate_entry <- function(entry) {
    if (is.na(entry) || entry == "" || grepl("^;+\\s*$", entry)) return(NA)
    iso_codes <- unlist(strsplit(entry, ";"))
    country_names <- iso_to_country[iso_codes]
    # Rimuove NA e unifica nomi duplicati
    unique_names <- unique(na.omit(country_names))
    paste(unique_names, collapse = "; ")
  }
  
  # Applica la funzione a tutta la colonna
  sapply(df_column, translate_entry)
}
