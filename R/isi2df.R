utils::globalVariables(c("Paper", "Tag", "content", "cont"))
isi2df <- function(D) {
  # D <- D[nchar(D)>0]  # remove empty rows

  # remove empty rows and strange characters
  res <- try(D <- D[nchar(D) > 1], silent = T)
  if (inherits(res, "try-error")) {
    D <- removeStrangeChar(D)
    # next
  } else {
    D <- res
    rm(res)
  }

  D <- D[!(substr(D, 1, 3) %in% c("FN ", "VR "))]

  for (i in 1:length(D)) {
    if (substr(D[i], 1, 3) == "   ") substr(D[i], 1, 3) <- substr(D[i - 1], 1, 3)
  }
  Papers <- which(substr(D, 1, 3) == "PT ") # first row of each document
  nP <- length(Papers) # number of documents

  rowPapers <- diff(c(Papers, length(D) + 1))

  numPapers <- rep(1:nP, rowPapers)

  DATA <- data.frame(Tag = substr(D, 1, 3), content = substr(D, 4, nchar(D)), Paper = numPapers)
  DATA$Tag <- gsub(" ", "", DATA$Tag)
  df <- DATA %>%
    group_by(Paper, Tag) %>%
    summarise(cont = paste(content, collapse = "---", sep = "")) %>%
    arrange(Tag, Paper) %>%
    pivot_wider(names_from = Tag, values_from = cont) %>%
    ungroup()
  df <- as.data.frame(df)


  df$PY <- as.numeric(df$PY)

  missingTags <- setdiff(c("AU", "DE", "C1", "RP", "CR", "PY", "SO", "TI", "TC"), names(df))
  if (length(missingTags) > 0) {
    cat("\nWarning:\nIn your file, some mandatory metadata are missing. Bibliometrix functions may not work properly!\n
Please, take a look at the vignettes:
- 'Data Importing and Converting' (https://www.bibliometrix.org/vignettes/Data-Importing-and-Converting.html)
- 'A brief introduction to bibliometrix' (https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html)\n\n")
    cat("\nMissing fields: ", missingTags, "\n")
  }

  ### replace "---" with ";"
  tagsComma <- c("AU", "AF", "CR")

  nolab <- setdiff(tagsComma, names(df))

  tagsComma <- tagsComma[(!(tagsComma %in% nolab))]

  df1 <- data.frame(lapply(df[tagsComma], function(x) {
    gsub("---", ";", x)
  }))

  ### replace "---" with " "
  otherTags <- setdiff(names(df), tagsComma)
  df2 <- data.frame(lapply(df[otherTags], function(x) {
    trimES(gsub("---", " ", x))
  }))
  df <- cbind(df1, df2)
  rm(df1, df2)

  ### store raw affiliation format to extract link among authors and affiliations
  df$C1raw <- df$C1
  ###

  df$DB <- "ISI"

  # Authors
  df$AU <- trimES(gsub(",", " ", df$AU))

  # Toupper
  DI <- df$DI
  AB <- df$AB
  TI <- df$TI
  DE <- df$DE
  df <- data.frame(lapply(df, toupper))
  df$DI <- DI
  df$AB_raw <- AB
  df$TI_raw <- TI
  df$DE_raw <- DE

  # Cited references
  if ("CR" %in% names(df)) df$CR <- normalizeCRisi(df$CR)

  # add sep ; to affiliations
  df$C1 <- trim(gsub("\\[.*?\\]", "", df$C1)) # to remove author info in square brackets
  df$C1 <- gsub("\\.", ".;", df$C1)

  df <- df[names(df) != "Paper"]

  return(df)
}

### Normalize the cited references of a WoS export
###
### Two shapes turn up in WoS plaintext exports that the positional parsers
### downstream (histNetwork(), CR_AU(), CR_SO(), ...) cannot read:
###
### 1. The cited author written as "Surname, Initials," where the classic
###    format has "Surname Initials,". The extra comma shifts every field of
###    the reference by one, so the parsers pick up the initials where they
###    expect the year and the year where they expect the source, and no
###    cited item can be matched back to a document of the collection.
###    The two name fields are rejoined only when the year sits where the
###    shift would put it, so references already in the classic format --
###    and entries such as "[ANONYMOUS], 1996, ..." -- are left untouched.
###
### 2. A repeated DOI tag ("DOI DOI 10.1234/x"). Splitting on "DOI" then
###    yields an empty string, so those references lose their DOI entirely.
###    The tag is also repeated inside the bracketed multi-DOI form that WoS
###    uses when it carries the same DOI twice, "DOI [DOI 10.1234/X,
###    10.1234/x]"; the brackets are stripped later in convert2df(), so the
###    duplication has to be undone here or it reappears afterwards.
###
### Input and output are the ";"-separated CR strings, one per document.
normalizeCRisi <- function(CR) {
  # "SURNAME, INITIALS, YEAR" -> "SURNAME INITIALS, YEAR"
  CR <- gsub(
    "(^|;)([^,;]+),[[:space:]]*([^,;]{1,12}),[[:space:]]*((19|20)[0-9]{2})(?=[[:space:]]*([,;]|$))",
    "\\1\\2 \\3, \\4",
    CR,
    perl = TRUE
  )

  # "DOI DOI 10.1234/x"   -> "DOI 10.1234/x"
  # "DOI [DOI 10.1234/X, " -> "DOI [10.1234/X, "
  gsub("DOI[[:space:]]+(\\[?)[[:space:]]*DOI[[:space:]]+", "DOI \\1", CR)
}
