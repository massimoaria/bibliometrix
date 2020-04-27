dimensions2df <- function(file, format = "csv") {
  
  switch(format,
         csv = {
           DATA=rio::import(file, quote = '"',dec = ".",skip=1)
         },
         excel = {
           DATA <- rio::import(file, skip = 1)
         })
  #Encoding(DATA) <- "UTF-8"
  
  bibtag <- NULL
  data("bibtag", envir = environment())
  bibtag <- as.data.frame(bibtag)
  
  
  names(DATA) <- trimES(gsub("\\.|-", " ", names(DATA)))
  
  i <-
    which(names(DATA) == "Authors Affiliations Name of Research organization")
  if (length(i) > 0) {
    names(DATA)[i] = "Authors Affiliations"
  }
  i <- which(names(DATA) == "Source title/Anthology title")
  if (length(i) > 0) {
    names(DATA)[i] = "Source title"
  }
  
  fields <- names(DATA)
  
  for (i in 1:length(fields)) {
    ind <- which(bibtag$DIMENSIONS == fields[i])
    if (length(ind) > 0) {
      fields[i] = bibtag$TAG[ind]
    }
  }
  names(DATA) = fields
  
  DATA <- postprocessingDim(DATA)
  
  return(DATA)
}



postprocessingDim <- function(DATA) {
  DATA <- data.frame(lapply(DATA, toupper), stringsAsFactors = FALSE)
  
  ## Converting original references in WOS format (AU, PY, SO, VOL, NUM, DOI)
  if ("Cited.references" %in% names(DATA)) {
    aaa <- strsplit(DATA$Cited.references, ";\\[")
    cr <- (unlist(lapply(aaa, function(l) {
      l <- gsub("\\|", "!!!", l)
      l <- strsplit(l, "!!!")
      
      ## first authors (of cited references)
      au <- sapply(l, `[`, 1)
      au <-
        ifelse(nchar(au) > 2, {au <- gsub("\\[", "", unlist(lapply(strsplit(au, ";"), function(x) {
        x = x[1]
      })))
      lastname <- trim(gsub(",.*", "", au))
        firstname <- strsplit(trim(gsub(".*,", "", au)), " ")
        firstname <- lapply(firstname, function(x) {
          if (length(x) > 0) {
            x <- paste(substr(x, 1, 1), sep = "", collapse = "")
          } else {
            x = ""
          }
          return(x)
        })
        au <- paste(lastname,
                    unlist(firstname),
                    sep = " ")},
      "NA")
        
      ## publication year
      py <- sapply(l, `[`, 4)
      so <- sapply(l, `[`, 3)
      vol <-
        ifelse(nchar(sapply(l, `[`, 5)) > 0, paste("V", sapply(l, `[`, 5), sep =
                                                     ""), "")
      num <-
        ifelse(nchar(sapply(l, `[`, 6)) > 0, paste("N", sapply(l, `[`, 6), sep =
                                                     ""), "")
      ## doi
      doi = sapply(l, `[`, 8)
      ref = paste(au,
                  py,
                  so,
                  vol,
                  num,
                  doi,
                  sep = ", ",
                  collapse = ";")
      ref = gsub("^,*|(?<=,),|,*$", "", ref, perl = T)
    })))
    
    DATA$CR=cr
    DATA$CR <- gsub("] ];","",cr)
  } else {
    DATA$CR = "NA,0000,NA"
  }
  
  # Document Type
  if (!("DT" %in% names(DATA))) {
    DATA$DT <- "Article"
  }
  
  # Authors cleaning and converting in WoS format
  DATA$AF <- DATA$AU
  
  DATA$AU <- gsub("\\s+", " ", DATA$AU)
  DATA$AU <- gsub("\\(|\\)","",DATA$AU)
  
  listAU <- strsplit(DATA$AU, "; ")
  
  AU <- lapply(listAU, function(l) {
    lastname <- trim(gsub(",.*", "", l))
    firstname <- strsplit(trim(gsub(".*,", "", l)), " ")
    firstname <- lapply(firstname, function(x) {
      if (length(x) > 0) {
        x <- paste(substr(x, 1, 1), sep = "", collapse = "")
      } else {
        x = ""
      }
      return(x)
    })
    AU <- paste(lastname,
                unlist(firstname),
                sep = " ",
                collapse = ";")
    return(AU)
  })
  
  
  DATA$AU <- unlist(AU)
  
  #keywords
  if (!("DE" %in% names(DATA)) & !("ID" %in% names(DATA))) {
    DATA$DE <- DATA$ID <- "NA"
  }
  if (("DE" %in% names(DATA)) & !("ID" %in% names(DATA))) {
    DATA$ID <- DATA$DE
  }
  if (!("DE" %in% names(DATA)) & ("ID" %in% names(DATA))) {
    DATA$DE <- DATA$ID
  }
  
  # Affiliations
  DATA$RP = NA
  #DATA$C1 <- trimws(gsub("(?:\\(.*?\\)|\\.)(*SKIP)(*F)|[\\w' ,\\\"]+", "", DATA$C1, perl=TRUE))
  #DATA$C1 <- gsub("\\.","",DATA$C1)
  #DATA$C1 <- gsub("\\(","",DATA$C1)
  #DATA$C1 <- gsub("\\)","",DATA$C1)
  #DATA$C1 <- gsub("-","",DATA$C1)
  #DATA$C1 <- gsub("\\'","",DATA$C1)
  DATA <- metaTagExtraction(DATA, Field = "AU_UN")
  

  if (!("AU_CO" %in% names(DATA))) {
    DATA$AU_CO <- "NOT AVAILABLE"
  } else {
    DATA$AU1_CO <- unlist(lapply(strsplit(DATA$AU_CO, ";"), function(l) {
      if (length(l) > 0) {
        l = l[[1]]
      } else {
        l = "NA"
      }
      return(l)
    }))
  }
  
  DATA$AU1_UN <-
    unlist(lapply(strsplit(DATA$AU1_UN, ";"), function(l) {
      if (length(l) > 0)
        l = l[1]
      return(l)
    }))
  
  if (("SO" %in% names(DATA)) & ("BO" %in% names(DATA))) {
    ind <- which(is.na(DATA$SO))
    DATA$SO[ind] <- DATA$BO[ind]
  }
  
  if (!("SO" %in% names(DATA))) {
    DATA$SO <- "NA"
  }
  
  DATA$JI = DATA$SO
  
  DATA$PY <- as.numeric(DATA$PY)
  
  DATA$TC <- as.numeric(DATA$TC)
  
  DATA$DB <- "ISI"
  
  return(DATA)
}
