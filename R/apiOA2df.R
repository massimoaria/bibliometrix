utils::globalVariables(c("corresponding_institution_ids","corresponding_author_name","corresponding_author_affiliation","corresponding_author_country","C1_ID"))


importOAFiles <- function(file){
  objName <- load(file)
  if (!isTRUE(inherits(eval(parse(text = objName)),c("list")))){
    message("the rdata file does not contain a valid object!\nopenalexR API requests have to be exported as 'list'.\nPlease set argument output='list' when use oafetch.\n

    ## Example ##
  works_from_dois <- oa_fetch(\n
  entity = 'works',\n
  doi = c('10.1016/j.joi.2017.08.007', 'https://doi.org/10.1007/s11192-013-1221-3'),\n
  output = 'list',
  verbose = TRUE\n
  )
    ")
    return(NA)
  }
  if (length(objName)!=1){
    message("the rdata file contains more than an object!")
    return(NA)
  }
  return(eval(parse(text = objName)))
}


# Function to apply the extraction to the whole list of items.
apiOA2df <- function(file){
  
  DATA <- importOAFiles(file)

  if (inherits(DATA,"list")){
    type <- "list"
  } else if (inherits(DATA,"data.frame")){
    type <-  "data.frame"
  }

  switch(type,
         "list"={
           df <- purrr::map_dfr(DATA, extract_all_metadata)
           df <- relabelling_OA_API(df)
         },
         "data.frame"={
           
         }
         )
  
  df$AF <- df$AU
  df$AU_UN <- df$C3 <- df$C1
  
  # move all char strings to Upper
  ind <- apply(df,2,function(x){
    sum(regexpr("https://",x)>-1, na.rm = TRUE)>0
  })
  label <- names(ind)[ind==FALSE & !is.na(ind)]
  
  if ("DI" %in% names(df)){
    df <- df %>% 
      mutate(across(all_of(label), toupper),
             DI = gsub("https://doi.org/","",DI),
             DI = ifelse(DI == "null",NA,DI)) 
  } else {
    df <- df %>% 
      mutate(across(all_of(label), toupper))
  }
  df$DB <- "OPENALEX"

  ## transform Country code in names
  CO <- strsplit(df$AU_CO,";")
  CO <- data.frame(Alpha2 = trimws(unlist(CO)), id_oa=rep(df$id_oa,lengths(CO)))
  CO <- CO %>% 
  left_join(openalexR::countrycode %>% select("Alpha2", "Country"), by="Alpha2") %>% 
  mutate(Country = toupper(Country)) %>% 
  group_by(id_oa) %>% 
  summarize(
    AU_CO = paste0(Country, collapse=";")
  )
  df <- df %>%
  select(-"AU_CO") %>% 
  left_join(CO, by="id_oa")
  
  df$id_oa <- gsub("https://openalex.org/","",df$id_oa)

  df <- df %>% as.data.frame()
  return(df)
}

relabelling_OA_API <- function(DATA){
  ## column re-labelling
  label <- names(DATA)
  label[label %in% "id"] <- "id_oa"
  label[label %in% "doi"] <- "DI"
  label[label %in% "title"] <- "TI"
  label[label %in% "language"] <- "LA"
  label[label %in% "publication_year"] <- "PY"
  label[label %in% "type"] <- "DT"
  label[label %in% "referenced_works_count"] <- "NR"
  label[label %in% "cited_by_count"] <- "TC"
  label[label %in% "author_id"] <- "AU_ID"
  label[label %in% "name"] <- "AU"
  label[label %in% "orcid"] <- "OI"
  label[label %in% "position"] <- "AU_POSITION"
  label[label %in% "institutions"] <- "C1"
  label[label %in% "institution_ids"] <- "C1_ID"
  label[label %in% "countries"] <- "AU_CO"
  label[label %in% "corresponding_institutions"] <- "RP"
  label[label %in% "journal_name"] <- "SO"
  label[label %in% "issn"] <- "IS"
  label[label %in% "abstract"] <- "AB"
  label[label %in% "cited_references" ] <- "CR"
  label[label %in% "keywords"] <- "DE"
  label[label %in% "concepts"] <- "ID"
  label[label %in% "oa_url"] <- "URL"
  label[label %in% "sdg_display_name"] <- "SDG"
  label[label %in% "mesh_terms"] <- "MESH"
 
  names(DATA) <- label
  return(DATA)
}


# Basic metadata extraction
extract_basic_info <- function(article) {
  tibble(
    id = article$id,
    doi = article$doi,
    title = article$title,
    language = article$language,
    display_name = article$display_name,
    publication_year = article$publication_year,
    publication_date = article$publication_date,
    type = article$type,
    countries_distinct_count = article$countries_distinct_count,
    institutions_distinct_count = article$institutions_distinct_count,
    referenced_works_count = article$referenced_works_count,
    cited_by_count = article$cited_by_count
  )
}

# Extraction of author information
extract_authors <- function(article) {
  authors <- article$authorships
  if (length(authors) > 0) {
    map_df(authors, ~ tibble(
      # Remove the prefix from the author ID
      author_id = if (!is.null(.x$author$id)) gsub("https://openalex.org/", "", .x$author$id) else NA,
      
      # Author's name
      name = if (!is.null(.x$author$display_name)) .x$author$display_name else NA,
      
      # ORCID (if available)
      orcid = if (!is.null(.x$author$orcid)) .x$author$orcid else NA,
      
      # Author's position
      position = if (!is.null(.x$author_position)) .x$author_position else NA,
      
      # Affiliations: Names of institutions
      institutions = if (!is.null(.x$institutions) && length(.x$institutions) > 0) {
        paste(map_chr(.x$institutions, "display_name", .default = NA), collapse = "; ")
      } else {
        NA
      },
      
      # Affiliations: Institution IDs (removing prefix)
      institution_ids = if (!is.null(.x$institutions) && length(.x$institutions) > 0) {
        paste(map_chr(.x$institutions, ~ gsub("https://openalex.org/", "", .x$id), .default = NA), collapse = "; ")
      } else {
        NA
      },
      
      # Countries of affiliations
      countries = if (!is.null(.x$countries) && length(.x$countries) > 0) {
        paste(.x$countries, collapse = "; ")
      } else {
        NA
      }
    ))
  } else {
    tibble(
      author_id = NA,
      name = NA,
      orcid = NA,
      position = NA,
      institutions = NA,
      institution_ids = NA,
      countries = NA
    )
  }
}

compress_author_affiliation_info <- function(authors_info) {
  # Helper function to concatenate only non-NA values
  concat_non_na <- function(x) {
    if (!is.null(x) && all(is.na(x))) {
      return(NA)
    } else if (is.null(x)) {
      return(NA)
    } else {
      return(paste(na.omit(x), collapse = "; "))
    }
  }
  
  # Check for the existence of columns before concatenating
  compressed_info <- tibble(
    author_id = if ("author_id" %in% names(authors_info)) concat_non_na(authors_info$author_id) else NA,
    name = if ("name" %in% names(authors_info)) concat_non_na(authors_info$name) else NA,
    orcid = if ("orcid" %in% names(authors_info)) concat_non_na(authors_info$orcid) else NA,
    position = if ("position" %in% names(authors_info)) concat_non_na(authors_info$position) else NA,
    institutions = if ("institutions" %in% names(authors_info)) concat_non_na(authors_info$institutions) else NA,
    institution_ids = if ("institution_ids" %in% names(authors_info)) concat_non_na(authors_info$institution_ids) else NA,
    countries = if ("countries" %in% names(authors_info)) concat_non_na(authors_info$countries) else NA
  )
  
  return(compressed_info)
}

# Extract journal information
extract_journal_info <- function(article) {
  primary_loc <- article$primary_location
  
  if (!is.null(primary_loc$source)) {
    journal_name <- primary_loc$source$display_name
    issn <- primary_loc$source$issn_l
  } else {
    journal_name <- NA
    issn <- NA
  }
  
  tibble(
    journal_name = journal_name,
    issn = issn,
    is_oa = primary_loc$is_oa,
    oa_status = article$open_access$oa_status,
    oa_url = article$open_access$oa_url
  )
}

# Extract abstract
extract_abstracts <- function(article) {
  abstract <- if (!is.null(article$abstract_inverted_index)) {
    abstract_build(article$abstract_inverted_index)
  } else {
    NA
  }
  
  tibble(
    abstract = abstract
  )
}

abstract_build <- function(ab) {
  if (is.null(ab)) {
    return(NA)
  }
  w <- rep(names(ab), lengths(ab))
  ind <- unlist(ab)
  if (is.null(ind)) {
    return("")
  }
  
  paste(w[order(ind)], collapse = " ", sep = "")
}

# Extract referenced citations
extract_cited_references <- function(article) {
  if (!is.null(article$referenced_works) && length(article$referenced_works) > 0) {
    cited_references <- article$referenced_works %>%
      map_chr(~ gsub("https://openalex.org/", "", .x)) %>%
      paste(collapse = "; ")
  } else {
    cited_references <- NA
  }
  
  tibble(cited_references = cited_references)
}

# Extract grants
extract_grants <- function(article) {
  if (!is.null(article$grants) && length(article$grants) > 0) {
    grants_info <- article$grants %>%
      map_chr(~ paste(
        ifelse(!is.null(.x$award_id), .x$award_id, NA),
        ifelse(!is.null(.x$funding_agency), .x$funding_agency, NA),
        sep = ": "
      )) %>%
      paste(collapse = "; ")
  } else {
    grants_info <- NA
  }
  
  tibble(grants = grants_info)
}

# Extract SDGs
extract_sdg <- function(article) {
  if (!is.null(article$sustainable_development_goals) && length(article$sustainable_development_goals) > 0) {
    tibble(
      sdg_display_name = article$sustainable_development_goals %>%
        map_chr(~ ifelse(!is.null(.x$display_name), .x$display_name, NA)) %>%
        paste(collapse = "; "),
      sdg_id = article$sustainable_development_goals %>%
        map_chr(~ ifelse(!is.null(.x$id), .x$id, NA)) %>%
        paste(collapse = "; "),
      sdg_score = article$sustainable_development_goals %>%
        map_chr(~ as.character(ifelse(!is.null(.x$score), .x$score, NA))) %>%
        paste(collapse = "; ")
    )
  } else {
    tibble(
      sdg_display_name = NA,
      sdg_id = NA,
      sdg_score = NA
    )
  }
}

# Extract corresponding author information
extract_corresponding_info <- function(article, authors_info) {
  corresponding_authors <- if (!is.null(article$corresponding_author_ids)) {
    id <- gsub("https://openalex.org/","",article$corresponding_author_ids)
    paste(authors_info$name[authors_info$author_id %in% id], collapse = ";")
  } else {
    NA
  }
  
  corresponding_institutions <- if (!is.null(article$corresponding_institution_ids) & !is.na(corresponding_authors)) {
    paste(authors_info$institutions[authors_info$author_id %in% id], collapse = ";")
  } else {
    NA
  }
  
  tibble(
    corresponding_authors = corresponding_authors,
    corresponding_institutions = corresponding_institutions
  )
}

# Extract Mesh Terms
extract_mesh_terms <- function(article) {
  if (!is.null(article$mesh) && length(article$mesh) > 0) {
    mesh_terms <- article$mesh %>%
      map_chr(~ ifelse(!is.null(.x$descriptor_name), .x$descriptor_name, NA)) %>%
      paste(collapse = "; ")
  } else {
    mesh_terms <- NA
  }
  
  tibble(mesh_terms = mesh_terms)
}

# Extract keywords
extract_keywords <- function(article) {
  if (!is.null(article$keywords) && length(article$keywords) > 0) {
    keywords_info <- article$keywords %>%
      map_chr(~ ifelse(!is.null(.x$display_name), .x$display_name, NA)) %>%
      paste(collapse = "; ")
  } else {
    keywords_info <- NA
  }
  
  tibble(keywords = keywords_info)
}

# Extract concepts
extract_concepts <- function(article) {
  if (!is.null(article$concepts) && length(article$concepts) > 0) {
    concepts_info <- article$concepts %>%
      map_chr(~ paste(ifelse(!is.null(.x$display_name), .x$display_name, NA),
                      "(",
                      ifelse(!is.null(.x$score), .x$score, NA),
                      ")")) %>%
      paste(collapse = "; ")
  } else {
    concepts_info <- NA
  }
  
  tibble(concepts = concepts_info)
}

# Extract topics
extract_topics <- function(article) {
  if (!is.null(article$topics) && length(article$topics) > 0) {
    topics_info <- article$topics %>%
      map_chr(~ ifelse(!is.null(.x$display_name), .x$display_name, NA)) %>%
      paste(collapse = "; ")
  } else {
    topics_info <- NA
  }
  
  tibble(topics = topics_info)
}

# Extract additional information (citations per year, FWCI, etc.)
extract_additional_info <- function(article) {
  yearly_citations <- if (!is.null(article$counts_by_year)) {
    paste(sapply(article$counts_by_year, function(count) {
      paste(count$year, count$cited_by_count, sep = ": ")
    }), collapse = "; ")
  } else {
    NA
  }
  
  tibble(
    yearly_citations = yearly_citations,
    fwci = ifelse(!is.null(article$fwci), article$fwci, NA),
    apc_value = ifelse(!is.null(article$apc_list$value), article$apc_list$value, NA),
    apc_currency = ifelse(!is.null(article$apc_list$currency), article$apc_list$currency, NA),
    has_fulltext = article$has_fulltext
  )
}

# Function to combine all extractions
extract_all_metadata <- function(article) {
  authors_info <- extract_authors(article)
  bind_cols(
    extract_basic_info(article),
    compress_author_affiliation_info(authors_info),
    extract_corresponding_info(article,authors_info),
    extract_journal_info(article),
    extract_abstracts(article),
    extract_cited_references(article),
    extract_grants(article),
    extract_sdg(article),
    extract_mesh_terms(article),
    extract_keywords(article),
    extract_concepts(article),
    extract_topics(article),
    extract_additional_info(article)
  )
}



