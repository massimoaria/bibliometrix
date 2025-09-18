utils::globalVariables(c("X1", "X2", "tag", "orig"))

csvScopus2df <- function(file) {
  options(readr.num_columns = 0)

  ## import all files in a single data frame
  for (i in 1:length(file)) {
    D <- read_csv(file[i],
      na = character(), quote = '"', trim_ws = FALSE, progress = show_progress(),
      col_types = cols(.default = col_character())
    ) %>% # Introduced to remove cols parsing errors
      # mutate(across(!where(is.numeric), as.character)) %>%   # not yet necessary with the inclusion of previuos line
      mutate(across(where(is.character), function(x) tidyr::replace_na(x, ""))) %>% as.data.frame()

    if (i > 1) {
      l <- intersect(l, names(D))
      DATA <- rbind(DATA[l], D[l])
    } else {
      l <- names(D)
      DATA <- D
    }
  }

  ## Post-Processing

  # column re-labelling
  DATA <- labelling(DATA)

  # Authors' names cleaning (surname and initials)
  DATA$AU <- gsub("\\.", "", DATA$AU)
  #DATA$AU <- gsub(",", ";", DATA$AU)
  DATA$AU <- gsub(",", "", DATA$AU)

  ### store raw affiliation format to extract link among authors and affiliations
  DATA$C1raw <- DATA$C1
  ###

  # Affiliation
  if (!("C1" %in% names(DATA))) {
    DATA$C1 <- NA
  } else {
    DATA$C1 <- unlist(lapply(strsplit(DATA$C1, ";"), function(l) {
      l <- paste(gsub(".*\\., ", "", l), collapse = ";", sep = "")
    }))
  }
  # Iso Source Titles
  if ("JI" %in% names(DATA)) {
    DATA$J9 <- gsub("\\.", "", DATA$JI)
  } else {
    DATA$J9 <- DATA$JI <- sapply(DATA$SO, AbbrevTitle, USE.NAMES = FALSE)
  }

  DI <- DATA$DI
  URL <- DATA$URL
  AB <- DATA$AB
  TI <- DATA$TI
  DE <- DATA$DE
  DATA <- data.frame(lapply(DATA, toupper))
  DATA$AB_raw <- AB
  DATA$TI_raw <- TI
  DATA$DE_raw <- DE
  DATA$DI <- DI
  DATA$URL <- URL
  return(DATA)
}




labelling <- function(DATA) {
  ## column re-labelling

  df_tag <- data.frame(
    rbind(
      c("Abbreviated Source Title", "JI"),
      c("Affiliations", "C1"),
      c("Authors with affiliations", "C1_raw"),
      c("Author Addresses", "C1_raw"),
      c("Authors", "AU"),
      c("Author Names", "AU"),
      c("Author full names", "AF"),
      c("Source title", "SO"),
      c("Titles", "TI"),
      c("Title", "TI"),
      c("Publication Year", "PY"),
      c("Year", "PY"),
      c("Volume", "VL"),
      c("Issue", "IS"),
      c("Page count", "PP"),
      c("Cited by", "TC"),
      c("DOI", "DI"),
      c("Link", "URL"),
      c("Abstract", "AB"),
      c("Author Keywords", "DE"),
      c("Indexed Keywords", "ID"),
      c("Index Keywords", "ID"),
      c("Funding Details", "FU"),
      c("Funding Texts", "FX"),
      c("Funding Text 1", "FX"),
      c("References", "CR"),
      c("Correspondence Address", "RP"),
      c("Publisher", "PU"),
      c("Open Access", "OA"),
      c("Language of Original Document", "LA"),
      c("Document Type", "DT"),
      c("Source", "DB"),
      c("EID", "UT")
    )
  ) %>%
    rename(
      orig = X1,
      tag = X2
    )

  label <- data.frame(orig = names(DATA)) %>%
    left_join(df_tag, by = "orig") %>%
    mutate(tag = ifelse(is.na(tag), orig, tag))

  names(DATA) <- label$tag
  
  if (!"C1" %in% names(DATA)) {
    if ("C1_raw" %in% names(DATA)) {
      DATA$C1 <- DATA$C1_raw
    } else {
      DATA$C1 <- NA
    }
  }

  return(DATA)
}
