utils::globalVariables(c("all_of", "corr", "DI", "C1","id_oa","RP","UN","AU_ID","corresponding_author_ids", "References"))

csvOA2df <- function(file){
  options(readr.num_columns = 0)
  
  ## import all files in a single data frame
  for (i in 1:length(file)){
    #D <- read.csv(file[i], quote='"', check.names = F, stringsAsFactors = F) #fileEncoding = "UTF-8-BOM")
    D <- read_csv(file[i], na=character(), quote='"', trim_ws = FALSE, progress = show_progress(), show_col_types = FALSE) %>%
      mutate(across(where(is.numeric), as.character)) %>% 
      mutate(across(where(is.character), \(x) tidyr::replace_na(x,""))) %>% 
      as.data.frame()
    
    if (i>1){
      l <- intersect(l,names(D))
      DATA <- rbind(DATA[l],D[l])
    }else{
      l <- names(D)
      DATA <- D}
  }
  rm(D)
  
  ## Post-Processing
  
  # column re-labelling
  DATA <- relabelling_OA(DATA)
  
  # recode as numeric
  DATA$TC <- as.numeric(DATA$TC)
  DATA$PY <- as.numeric(DATA$PY)
  #DATA$relevance_score <- as.numeric(DATA$relevance_score)
  
  # replace | with ;
  DATA <- DATA %>% 
    mutate(across(where(is.character), ~ stringi::stri_replace_all_regex(.,"\\|",";")))
  
  DATA$AF <- DATA$AU
  DATA$ID <- DATA$DE
  if (!"AB" %in% names(DATA)) DATA$AB=""
  DATA$CR <- gsub("https://openalex.org/","",DATA$CR)
  DATA$AU_ID <- gsub("https://openalex.org/","",DATA$AU_ID)
  DATA$id_oa <- gsub("https://openalex.org/","",DATA$id_oa)
  DATA$JI <- DATA$J9 <- gsub("https://openalex.org/","",DATA$SO_ID)
  DATA$corresponding_author_ids <- gsub("https://openalex.org/","",DATA$corresponding_author_ids)
  DATA$DB <- "OPENALEX"
  
  # affilitation string
  AFF <- DATA %>% 
    select(id_oa, starts_with("authorships_raw_affiliation_strings_")) 
  
  if(ncol(AFF)>1){
    colId <- c(-1,parse_number(colnames(AFF)[-1]))
    
    DATA <- AFF[order(colId)] %>% 
      unite(., C1, starts_with("authorships_raw_affiliation_strings_"), sep=";") %>% 
      mutate(C1 = gsub("NA","",C1),
             C1 = TrimMult(C1,char=";")) %>% 
      bind_cols(DATA %>% 
                  select(-"id_oa", -starts_with("authorships_raw_affiliation_strings_")))
  } else {
    AFF <- lapply(stri_extract_all_regex(DATA$authorships.raw_affiliation_strings, "\\[([^\\]]+)\\]"), function(l){
      gsub("\\['|'\\]","",l)
    })
    
    AFF <- data.frame(id_oa=rep(DATA$id_oa, lengths(AFF)), C1 = unlist(AFF)) %>% 
      group_by(id_oa) %>% 
      summarize(C1 = paste(C1,collapse=";")) 
    DATA <- DATA %>% 
      left_join(AFF, by = "id_oa")
    DATA$C1[is.na(DATA$C1)] <- ""
  }

  DATA$C1 <- gsub("https://", "", DATA$C1)
  
  # country string
  CO <- DATA %>% 
    select(id_oa, starts_with("authorships_countries_")) 
  
  if(ncol(CO)>1){
  colId <- c(-1,parse_number(colnames(CO)[-1]))
  
  DATA <- CO[order(colId)] %>% 
    unite(., AU_CO, starts_with("authorships_countries_"), sep=";") %>% 
    mutate(AU_CO = gsub("NA","",AU_CO),
           AU_CO = TrimMult(AU_CO,char=";")) %>% 
    bind_cols(DATA %>% 
                select(-"id_oa", -starts_with("authorships_countries_")))
  } else {
    CO <- lapply(stri_extract_all_regex(DATA$authorships.countries, "\\[([^\\]]+)\\]"), function(l){
      gsub("\\['|'\\]","",l)
    })
    
    CO <- data.frame(id_oa=rep(DATA$id_oa, lengths(CO)), AU_CO = unlist(CO)) %>% 
      group_by(id_oa) %>% 
      summarize(AU_CO = gsub("'","",paste(AU_CO,collapse=";")))
    DATA <- DATA %>% 
      left_join(CO, by = "id_oa")
    DATA$AU_CO[is.na(DATA$AU_CO)] <- ""
  }
  
  
  ## corresponding author
  DATA <- DATA %>% 
    mutate(AU1_ID = gsub(";.*", "", corresponding_author_ids))
  UN <- strsplit(DATA$C1,";")
  if ("authorships_is_corresponding" %in% names(DATA)){
    corresp <- strsplit(tolower(DATA$authorships_is_corresponding),";")
  } else {
    corresp <- strsplit(tolower(DATA$authorships.is_corresponding),";")
  }
  
  
  
  df_UN <- data.frame(UN=unlist(UN), id_oa=rep(DATA$id_oa,lengths(UN))) %>% 
    group_by(id_oa) %>% 
    mutate(n=row_number())
  df_COR <- data.frame(corr=unlist(corresp), id_oa=rep(DATA$id_oa,lengths(corresp))) %>% 
    group_by(id_oa) %>% 
    mutate(n=row_number())
  df_UN <- df_UN %>% 
    left_join(df_COR, by=(c("id_oa","n"))) 
  AU <- strsplit(DATA$AU,";")
  AU_ID <- strsplit(DATA$AU_ID,";")
  AU_df <- data.frame(RP = unlist(AU), id_oa=rep(DATA$id_oa,lengths(AU))) %>% 
    group_by(id_oa) %>% 
    mutate(n=row_number())
  AU_ID_df <- data.frame(AU_ID=unlist(AU_ID), id_oa=rep(DATA$id_oa,lengths(AU_ID))) %>% 
    group_by(id_oa) %>% 
    mutate(n=row_number())
  AU_df <- AU_df %>% 
    left_join(AU_ID_df, by=c("id_oa","n")) %>% 
    select(-"n") %>% 
    group_by(id_oa) %>% 
    mutate(n=row_number()) %>% 
    left_join(df_UN %>% select("UN","id_oa", "corr", "n"),
              by = c("id_oa","n")) %>% 
    dplyr::filter(corr == "true") %>% 
    mutate(RP = paste(RP,UN, sep=", ")) %>% 
    ungroup() %>% 
    select("RP", "AU_ID") %>% 
    distinct(AU_ID, .keep_all = TRUE)
  DATA <- DATA %>% 
    mutate(AU1_ID = gsub(";.*", "", corresponding_author_ids)) %>% 
    left_join(AU_df, by = c("AU1_ID" = "AU_ID"))
    
  
  # move all char strings to Upper
  ind <- apply(DATA,2,function(x){
    sum(regexpr("https://",x)>-1, na.rm = TRUE)>0
  })
  label <- names(ind)[ind==FALSE & !is.na(ind)]

  DATA <- DATA %>% 
    mutate(across(all_of(label), toupper),
           DI = gsub("https://doi.org/","",DI),
           DI = ifelse(DI == "null",NA,DI)) 
  DATA$SO <- toupper(DATA$SO)
  
  return(DATA)
}

relabelling_OA <- function(DATA){
  ## column re-labelling
  label <- names(DATA)
  label[label %in% "id"] <- "id_oa"
  label[label %in% "display_name"] <- "TI"
  label[label %in% "primary_location_display_name"] <- "SO"
  label[label %in% "locations.source.display_name"] <- "SO"
  label[label %in% "primary_location_id"] <- "SO_ID"
  label[label %in% "locations.source.id"] <- "SO_ID"
  label[label %in% "primary_location_host_organization"] <- "PU"
  label[label %in% "primary_location_issns"] <- "ISSN"
  label[label %in% "primary_location_issn_l"] <- "ISSN_I"
  label[label %in% "primary_location_landing_page_url"] <- "URL"
  label[label %in% "primary_location_pdf_url"] <- "URL_PDF"
  label[label %in% "author_ids"] <- "AU_ID"
  label[label %in% "authorships.author.id"] <- "AU_ID"
  label[label %in% "author_names"] <- "AU"
  label[label %in% "authorships.author.display_name"] <- "AU"
  label[label %in% "author_orcids"] <- "OI"
  label[label %in% "author_institution_names"] <- "C3"
  label[label %in% "cited_by_count"] <- "TC"
  label[label %in% "publication_year"] <- "PY"
  label[label %in% "type"] <- "DT"
  label[label %in% "biblio_issue"] <- "IS"
  label[label %in% "biblio_volume"] <- "VL"
  label[label %in% "referenced_works" ] <- "CR"
  label[label %in% "keywords_display_name"] <- "DE"
  label[label %in% "keywords.display_name"] <- "DE"
  label[label %in% "abstract"] <- "AB"
  label[label %in% "concepts_display_name"] <- "CONCEPTS"
  label[label %in% "topics_display_name"] <- "TOPICS"
  label[label %in% "sustainable_development_goals_display_name"] <- "SDG"
  label[label %in% "primary_topic_field_display_name"] <- "SC"
  label[label %in% "mesh_descriptor_name"] <- "MESH"
  label[label %in% "referenced_works_count"] <- "NR"
  label[label %in% "language"] <- "LA"
  label[label %in% "authorships_author_position"] <- "AU_POSITION"
  #label[label %in% "authorships_raw_affiliation_string"] <- "C1"
  label[label %in% "doi"] <- "DI"
  names(DATA) <- label
  return(DATA)
}

TrimMult <- function(x, char=" ") {
  return(gsub(paste0("^", char, "*|(?<=", char, ")", char, "|", char, "*$"),
              "", x, perl=T))
}
