utils::globalVariables(c("Paper", "Tag", "content", "cont"))

bib2df <- function(D, dbsource = "isi") {
  # D <- D[nchar(D)>1]

  # remove empty rows and strange characters
  res <- try(D <- D[nchar(D) > 1], silent = T)
  if (inherits(res, "try-error")) {
    D <- removeStrangeChar(D)
    # next
  } else {
    D <- res
    rm(res)
  }

  D <- gsub("\\{\\[\\}", "[", D)
  D <- gsub("\\{\\]\\}", "]", D)
  Papers <- which(substr(D, 1, 1) == "@") # # first row of each document
  if (Papers[1] > 1) {
    D <- D[-(1:(Papers[1] - 1))]
    Papers <- Papers - (Papers[1] - 1)
  }

  if (dbsource == "isi") D <- gsub(" = \\{", "={", D)

  D <- gsub("\\\t", "", gsub(" = \\{", "=\\{", D)) # to work also with new scopus bib format

  D[Papers] <- paste("Paper={", D[Papers], sep = "")

  ind <- regexpr("=\\{", D) # sep among tags and contents
  ind[Papers] <- 6

  nP <- length(Papers) # number of documents

  for (i in 1:length(D)) {
    if (ind[i] == -1) {
      D[i] <- trimES(paste(substr(D[i - 1], 1, ind[i - 1] + 1), D[i], collapse = " "))
      ind[i] <- ind[i - 1]
    }
  }

  rowPapers <- diff(c(Papers, length(D) + 1))

  numPapers <- rep(1:nP, rowPapers)

  DATA <- data.frame(Tag = substr(D, 1, ind + 1), content = substr(D, ind + 2, nchar(D)), Paper = numPapers)
  DATA$content <- gsub("\\}|\\},|\\{", "", DATA$content)
  # DATA$content <- gsub("\\{","",DATA$content)
  df <- DATA %>%
    group_by(Paper, Tag) %>%
    summarise(cont = paste(content, collapse = "---", sep = "")) %>%
    arrange(Tag, Paper) %>%
    pivot_wider(names_from = Tag, values_from = cont) %>%
    ungroup()

  df <- as.data.frame(df)

  rm(DATA)
  bibtag <- NULL
  data("bibtag", envir = environment())
  bibtag <- as.data.frame(bibtag)

  Tag <- tolower(names(df))
  switch(dbsource,
    scopus = {
      bibtag <- bibtag[(bibtag[, "SCOPUS"] %in% Tag), ]
      for (i in 1:nrow(bibtag)) {
        Tag[Tag == bibtag$SCOPUS[i]] <- bibtag$TAG[i]
      }
    },
    isi = {
      bibtag <- bibtag[(bibtag[, "ISI"] %in% Tag), ]
      for (i in 1:nrow(bibtag)) {
        Tag[Tag == bibtag$ISI[i]] <- bibtag$TAG[i]
      }
    },
    generic = {
      bibtag <- bibtag[(bibtag[, "GENERIC"] %in% Tag), ]
      for (i in 1:nrow(bibtag)) {
        Tag[Tag == bibtag$GENERIC[i]] <- bibtag$TAG[i]
      }
    }
  )



  names(df) <- gsub("=\\{", "", Tag)

  ### replace "---" with ";"
  tagsComma <- c("AU", "DE", "ID", "C1", "CR")
  nolab <- setdiff(tagsComma, names(df))
  if (length(nolab) > 0) {
    cat("\nWarning:\nIn your file, some mandatory metadata are missing. Bibliometrix functions may not work properly!\n
Please, take a look at the vignettes:
- 'Data Importing and Converting' (https://www.bibliometrix.org/vignettes/Data-Importing-and-Converting.html)
- 'A brief introduction to bibliometrix' (https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html)\n\n")
    cat("\nMissing fields: ", nolab, "\n")
  }

  tagsComma <- tagsComma[(!(tagsComma %in% nolab))]
  df1 <- data.frame(lapply(df[tagsComma], function(x) {
    gsub("---", ";", x)
  }))

  df1$AU <- gsub(" and;| and ", ";", df1$AU)

  ### replace "---" with " "
  otherTags <- setdiff(names(df), tagsComma)
  df2 <- data.frame(lapply(df[otherTags], function(x) {
    trimES(gsub("---", " ", x))
  }), stringsAsFactors = FALSE)
  df <- cbind(df1, df2)
  rm(df1, df2)

  # Funding info
  ind <- which(regexpr("funding_text", names(df)) > -1)
  if (!("FX" %in% Tag) & length(ind) > 0) {
    df$FX <- apply(
      (as.data.frame(df[, ind], stringsAsFactors = FALSE)), 1,
      function(x) paste(x, collapse = " ")
    )
    df <- df[, -ind]
  }

  df <- postprocessing(df, dbsource)

  df <- df[!names(df) %in% c("Paper", "paper")]

  return(df)
}

### DATA FRAME postprocessing
postprocessing <- function(DATA, dbsource) {
  # Authors' names cleaning (surname and initials)
  # remove ; and 2 or more spaces
  DATA$AU <- gsub("\\s+", " ", DATA$AU)
  DATA$AF <- gsub("\\.|,", "", DATA$AU)

  listAU <- strsplit(DATA$AU, ";")

  AU <- lapply(listAU, function(l) {
    lastname <- trim(gsub(",.*", "", l))
    firstname <- strsplit(trim(gsub(".*,", "", l)), " ")
    firstname <- gsub("[^:A-Z:]", "", firstname)
    AU <- paste(lastname, unlist(firstname), sep = " ", collapse = ";")
    return(AU)
  })


  DATA$AU <- unlist(AU)

  # TC post-processing
  if ("TC" %in% names(DATA)) {
    DATA$TC <- as.numeric(sub("\\D*(\\d+).*", "\\1", DATA$TC))
  }
  # CR post-processing
  if ("CR" %in% names(DATA)) {
    # reomve dots after DOIs
    DATA$CR <- gsub("\\.;", ";", DATA$CR)
    DATA$CR <- substr(DATA$CR, 1, (nchar(DATA$CR) - 1))
  }
  # Year
  if ("PY" %in% names(DATA)) {
    DATA$PY <- as.numeric(sub("\\D*(\\d+).*", "\\1", DATA$PY))
  }

  if ("UT" %in% names(DATA)) {
    DATA$UT <- gsub(":", "", DATA$UT, fixed = TRUE)
  }

  if (!("RP" %in% names(DATA)) & ("C1" %in% names(DATA))) {
    DATA$RP <- unlist(lapply(strsplit(DATA$C1, "\\."), function(l) l[1]))
  }

  # keywords post-processing (missing ";" in some rows)
  if ("ID" %in% names(DATA)) {
    DATA$ID <- gsub("   |,", ";", DATA$ID)
    # DATA$ID <- gsub(",",";",DATA$ID)
  }

  if ("DE" %in% names(DATA)) {
    DATA$DE <- gsub("   |,", ";", DATA$DE)
    # DATA$DE <- gsub(",",";",DATA$DE)
  }
  # row.names(DATA)=DATA$UT

  ### merge Sources and Proceedings
  if (("SO" %in% names(DATA)) & ("BO" %in% names(DATA))) {
    ind <- which(is.na(DATA$SO))
    DATA$SO[ind] <- DATA$BO[ind]
  }

  if ("PN" %in% names(DATA)) {
    DATA$PN <- as.numeric(gsub("[^0-9]", "", DATA$PN))
  }

  if (dbsource != "generic") {
    DATA$DB <- dbsource
  } else {
    DATA$DB <- "SCOPUS"
  }


  # Toupper
  DI <- DATA$DI
  URL <- DATA$url
  AB <- DATA$AB
  TI <- DATA$TI
  DE <- DATA$DE
  DATA <- data.frame(lapply(DATA, toupper), stringsAsFactors = FALSE)
  if ("JI" %in% names(DATA)) {
    DATA$J9 <- gsub("\\.", "", DATA$JI)
  } else {
    DATA$J9 <- DATA$JI <- sapply(DATA$SO, AbbrevTitle, USE.NAMES = FALSE)
  }
  DATA$DI <- DI
  DATA$url <- URL
  DATA$AB_raw <- AB
  DATA$TI_raw <- TI
  DATA$DE_raw <- DE
  return(DATA)
}
