utils::globalVariables("where")

csvLens2df <- function(file) {
  options(readr.num_columns = 0)

  ## import all files in a single data frame
  for (i in 1:length(file)) {
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
  DATA <- relabelling_lens(DATA)

  if ("TC" %in% names(DATA)) DATA$TC <- as.numeric(DATA$TC)

  # Authors' names cleaning (surname and initials)
  DATA$AF <- DATA$AU

  # Authors' names cleaning (surname and initials)
  # remove ; and 2 or more spaces
  # DATA$AU <- gsub("\\s+", " ", DATA$AU)

  listAU <- strsplit(DATA$AU, split = "; ")

  AU <- lapply(listAU, function(l) {
    lastname <- sub(".*\\s", "", trimws(l))
    firstname <- sub("\\s+[^ ]+$", "", l)
    firstname <- gsub("[^:A-Z:]", "", firstname)
    AU <- paste(lastname, firstname, sep = " ", collapse = ";")
    return(AU)
  })


  DATA$AU <- unlist(AU)
  # DATA$AU=gsub("\\.", "", DATA$AU)
  # DATA$AU=gsub(",", ";", DATA$AU)

  # Affiliation
  # DATA$C1 <- "Unknown"

  # Iso Source Titles
  DATA$SO[DATA$SO == ""] <- DATA$Publisher[DATA$SO == ""]
  # DATA$JI <- sapply(DATA$SO, AbbrevTitle, USE.NAMES = FALSE)
  # DATA$J9 <- gsub("\\.","",DATA$JI)
  DATA$JI <- DATA$J9 <- DATA$SO
  DATA$ID <- DATA$DE
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
  DATA$AU_CO <- "NA"
  DATA$DB <- "LENS"
  return(DATA)
}




relabelling_lens <- function(DATA) {
  ## column re-labelling
  label <- names(DATA)
  label <- gsub("Source Title", "SO", label)
  # label <- gsub("Authors with affiliations","C1",label)
  label <- gsub("Author/s", "AU", label)
  label <- gsub("Publication.Type", "DT", label)
  label <- gsub("Title", "TI", label)
  label <- gsub("Publication Year", "PY", label)
  label <- gsub("Volume", "VL", label)
  label <- gsub("Issue Number", "IS", label)
  label <- gsub("Source Country", "SO_CO", label)
  label <- gsub("Scholarly Citation Count", "TC", label)
  label <- gsub("DOI", "DI", label)
  label <- gsub("Source URLs", "URL", label)
  label <- gsub("Abstract", "AB", label)
  label <- gsub("Keywords", "DE", label)
  label <- gsub("MeSH Terms", "MESH", label)
  label <- gsub("Funding Details", "FU", label)
  label <- gsub("Funding", "FX", label)
  label <- gsub("References", "CR", label)
  # label <- gsub("Correspondence Address","RP",label)
  label <- gsub("Fields of Study", "SC", label)
  label <- gsub("Language of Original Document", "LA", label)
  label <- gsub("Document Type", "DT", label)
  label <- gsub("Source", "DB", label)
  label <- gsub("Lens ID", "UT", label)
  label <- gsub("Citing Works Count", "TC", label)
  names(DATA) <- label

  return(DATA)
}
