#' Import and Convert bibliographic export files and API objects.
#'
#' It converts a SCOPUS, Clarivate Analytics WoS, Dimensions, PubMed and COCHRANE Database export files or pubmedR and dimensionsR JSON/XML 
#' objects into a data frame, with cases corresponding to articles and variables to Field Tags as used in WoS.
#'
#' @param file a character array containing a sequence of object names coming from: 
#' \tabular{lll}{
#' a)\tab \tab Clarivate Analytics WoS (in plaintext '.txt', Endnote Desktop '.ciw', or bibtex formats '.bib');\cr
#' b)\tab \tab SCOPUS (exclusively in bibtex format '.bib');\cr
#' c)\tab \tab Digital Science Dimensions (in csv '.csv' or excel '.xlsx' formats);\cr
#' d)\tab \tab an object of the class \code{pubmedR (package pubmedR)} containing a collection obtained from a query performed with pubmedR package;\cr
#' e)\tab \tab an object of the class \code{dimensionsR (package dimensionsR)} containing a collection obtained from a query performed with dimensionsR package.}
#' @param dbsource is a character indicating the bibliographic database. \code{dbsource} can be \code{"isi"}, \code{"wos"}, \code{"scopus"}, \code{"dimensions"} or \code{"pubmed"}. Default is \code{dbsource = "isi"}.
#' @param format is a character indicating the format of the SCOPUS and Clarivate Analytics WoS export file. \code{format} can be \code{"api"}, \code{"bibtex"}, \code{"plaintext"}, \code{"endnote"}, \code{"csv"} or \code{"excel"}. Default is \code{format = "plaintext"}.
#' @return a data frame with cases corresponding to articles and variables to Field Tags in the original export file.
#' 
#' I.e We have three files downlaod from Web of Science in plaintext format, file will be:
#' 
#' file <- c("filename1.txt", "filename2.txt", "filename3.txt") 
#' 
#' data frame columns are named using the standard Clarivate Analytics WoS Field Tag codify. The main field tags are:
#'
#' \tabular{lll}{
#' \code{AU}\tab   \tab Authors\cr
#' \code{TI}\tab   \tab Document Title\cr
#' \code{SO}\tab   \tab Publication Name (or Source)\cr
#' \code{JI}\tab   \tab ISO Source Abbreviation\cr
#' \code{DT}\tab   \tab Document Type\cr
#' \code{DE}\tab   \tab Authors' Keywords\cr
#' \code{ID}\tab   \tab Keywords associated by SCOPUS or WoS database \cr
#' \code{AB}\tab   \tab Abstract\cr
#' \code{C1}\tab   \tab Author Address\cr
#' \code{RP}\tab   \tab Reprint Address\cr
#' \code{CR}\tab   \tab Cited References\cr
#' \code{TC}\tab   \tab Times Cited\cr
#' \code{PY}\tab   \tab Year\cr
#' \code{SC}\tab   \tab Subject Category\cr
#' \code{UT}\tab   \tab Unique Article Identifier\cr
#' \code{DB}\tab   \tab Database\cr}
#'
#' for a complete list of field tags see: \href{https://www.bibliometrix.org/documents/Field_Tags_bibliometrix.pdf}{Field Tags used in bibliometrix}
#' 
#' @examples
#' 
#' # Example:
#' # Import and convert a Web of Science collection form an export file in plaintext format:
#' 
#' \dontrun{
#' files <- 'https://www.bibliometrix.org/datasets/wos_plaintext.txt'
#' 
#' M <- convert2df(file = files, dbsource = 'wos', format = "plaintext")
#' }
#'
#' 
#' @export

convert2df<-function(file,dbsource="wos",format="plaintext"){

  allowed_formats <- c('api', 'bibtex', 'csv', 'endnote','excel','plaintext', 'pubmed') 
  allowed_db <- c('cochrane','dimensions','generic','isi','pubmed','scopus','wos')
  
  cat("\nConverting your",dbsource,"collection into a bibliographic dataframe\n\n")
  if (length(setdiff(dbsource,allowed_db))>0){
    cat("\n 'dbsource' argument is not properly specified")
    cat("\n 'dbsource' argument has to be a character string matching one among:",allowed_db, "\n")
    }
  if (length(setdiff(format,allowed_formats))>0){
    cat("\n 'format' argument is not properly specified")
    cat("\n 'format' argument has to be a character string matching one among:",allowed_formats,"\n")
  }
  
  ### da controllare
  if (length(setdiff(format,c("api","plaintext","bibtex","csv","excel", "endnote")))>0){
    D <- importFiles(file)
    D <- iconv(D, "latin1", "ASCII", sub="")}
  
  if (dbsource=="wos") dbsource <- "isi"
  if (format=="endnote") format <- "plaintext"
  
  
  switch(
    dbsource,
    ## db WOS
    isi = {
      switch(format,
             bibtex = {
               D <- importFiles(file)
               M <- bib2df(D, dbsource = "isi")
             },
             plaintext = {
               D <- importFiles(file)
               M <- isi2df(D)
             })
    },
    ## db SCOPUS
    scopus = {
      switch(format,
             bibtex = {
               D <- importFiles(file)
               M <- bib2df(D, dbsource = "scopus")
             },
             csv = {
               M <- csvScopus2df(file)
             })
    },
    ## db GENERIC BIBTEX
    generic = {
      D <- importFiles(file)
      M <- bib2df(D, dbsource = "generic")
    },
    ## db PUBMED
    pubmed = {
      switch(format,
             api = {
               M <- pmApi2df(file)
               M$DB <- "PUBMED"
             },
             {
               D <- importFiles(file)
               M <- pubmed2df(D) 
             })
    },
    ## db COCHRANE
    cochrane = {
      D <- importFiles(file)
      M <- cochrane2df(D)
    },
    ## db DIMENSIONS
    dimensions = {
      switch(format,
             api = {
               M <- dsApi2df(file)
               M$DB <- "DIMENSIONS"
             },
             {
               M <- dimensions2df(file, format = format)
             })
      
    }
  )
  if ("PY" %in% names(M)){M$PY=as.numeric(M$PY)} else {M$PY <- NA}
  
  if ("TC" %in% names(M)){
    M$TC=as.numeric(M$TC)
    M$TC[is.na(M$TC)] <- 0
  } else {M$TC <- 0}
  
  if (!("CR" %in% names(M))){M$CR="none"}
  if (dbsource!="cochrane"){M$AU=gsub(intToUtf8(8217),intToUtf8(39),M$AU)}
  
  cat("Done!\n\n")
  
  if (!(dbsource %in% c("dimensions", "pubmed"))) {
    ## AU_UN field creation
    if ("C1" %in% names(M)) {
      cat("\nGenerating affiliation field tag AU_UN from C1:  ")
      
      M <- metaTagExtraction(M, Field = "AU_UN")
      cat("Done!\n\n")
    } else{
      M$C1 = NA
      M$AU_UN = NA
    }
    
    ## AU normalization
    M$AU = unlist(lapply(strsplit(M$AU, ";"), function(x) {
      x = trimws(trimES(gsub("[^[:alnum:][-]']", " ", x)))
      x = paste(x, collapse = ";")
    }))
  }
  
  if ((dbsource == "pubmed") & (format == "pubmed")) {
    if ("C1" %in% names(M)) {
      cat("\nGenerating affiliation field tag AU_UN from C1:  ")
      
      M <- metaTagExtraction(M, Field = "AU_UN")
      cat("Done!\n\n")
    } else{
      M$C1 = NA
      M$AU_UN = NA
    }
  }
  
  ### SR field creation
  suppressWarnings(M <- metaTagExtraction(M, Field="SR"))
  d <- duplicated(M$SR)
  if (sum(d)>0) cat("\nRemoved ",sum(d),"duplicated documents\n")
  M <- M[!d,]
  row.names(M) <- M$SR
  
  ### bibliometrix>DB class
  class(M) <- c("bibliometrixDB", "data.frame")
  
  return(M)

}


importFiles <- function(...){
  
  arguments <- unlist(list(...))
  k=length(arguments)
  D=list()
  enc="UTF-8"
  origEnc=getOption("encoding")
  if (origEnc=="UTF-8"){options(encoding = "native.enc")}
  for (i in 1:k){
    D[[i]]=suppressWarnings(
      iconv(readLines(arguments[i],encoding = "UTF-8"),"latin1", "ASCII", sub="")
      #conv(readLines(arguments[[i]]))
    )
  }
  D=unlist(D)
  options(encoding = origEnc)
  Encoding(D) <- "UTF-8"
  return(D)
  
}
