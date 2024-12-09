#' Import and Convert bibliographic export files and API objects.
#'
#' It converts a SCOPUS, Clarivate Analytics WoS, Dimensions, Lens.org, PubMed and COCHRANE Database export files or pubmedR and dimensionsR JSON/XML 
#' objects into a data frame, with cases corresponding to articles and variables to Field Tags as used in WoS.
#'
#' @param file a character array containing a sequence of filenames coming from WoS, Scopus, Dimensions, Lens.org, OpenAlex and Pubmed. Alternatively, \code{file} can be 
#' an object resulting from an API query fetched from Dimensions, and PubMed databases: 
#' \tabular{lll}{
#' a)\tab 'wos' \tab Clarivate Analytics WoS (in plaintext '.txt', Endnote Desktop '.ciw', or bibtex formats '.bib');\cr
#' b)\tab 'scopus' \tab SCOPUS (exclusively in bibtex format '.bib');\cr
#' c)\tab 'dimensions' \tab Digital Science Dimensions (in csv '.csv' or excel '.xlsx' formats);\cr
#' d)\tab 'lens' \tab Lens.org (in csv '.csv');\cr
#' e)\tab 'pubmed' \tab an object of the class \code{pubmedR (package pubmedR)} containing a collection obtained from a query performed with pubmedR package;\cr
#' f)\tab 'dimensions' \tab an object of the class \code{dimensionsR (package dimensionsR)} containing a collection obtained from a query performed with dimensionsR package;\cr
#' g)\tab 'openalex' \tab OpenAlex .csv file;\cr
#' h)\tab 'openalex_api' \tab the filename and path to a list object returned by openalexR package, containing a collection of works resulting from a query fetched from OpenAlex database.}
#' @param dbsource is a character indicating the bibliographic database. \code{dbsource} can be \code{dbsource = c('cochrane','dimensions','generic','isi','openalex', 'pubmed','scopus','wos', 'lens')} . Default is \code{dbsource = "isi"}.
#' @param format is a character indicating the SCOPUS, Clarivate Analytics WoS, and other databases export file format. \code{format} can be \code{c('api', 'bibtex', 'csv', 'endnote','excel','plaintext', 'pubmed')}. Default is \code{format = "plaintext"}.
#' @param remove.duplicates is logical. If TRUE, the function will remove duplicated items checking by DOI and database ID.
#' @return a data frame with cases corresponding to articles and variables to Field Tags in the original export file.
#' 
#' I.e We have three files download from Web of Science in plaintext format, file will be:
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

convert2df<-function(file,dbsource="wos",format="plaintext", remove.duplicates=TRUE){

  allowed_formats <- c('api', 'bibtex', 'csv', 'endnote','excel','plaintext', 'pubmed') 
  allowed_db <- c('cochrane','dimensions','generic','isi','openalex', 'openalex_api','pubmed','scopus','wos', 'lens')
  
  cat("\nConverting your",dbsource,"collection into a bibliographic dataframe\n\n")
  if (length(setdiff(dbsource,allowed_db))>0){
    cat("\n 'dbsource' argument is not properly specified")
    cat("\n 'dbsource' argument has to be a character string matching one among:",allowed_db, "\n")
    }
  if (length(setdiff(format,allowed_formats))>0){
    cat("\n 'format' argument is not properly specified")
    cat("\n 'format' argument has to be a character string matching one among:",allowed_formats,"\n")
  }

  if (dbsource=="wos") dbsource <- "isi"
  if (format=="endnote") format <- "plaintext"
  if (format == "lens") format <- "csv"
  
  
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
    ## db LENS
    lens = {
      M <- csvLens2df(file)
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
               M$DB <- "DIMENSIONS"
             })
      
    },
    openalex={
      M <- csvOA2df(file)
    },
    openalex_api = {
      M <- apiOA2df(file)
# if (!"bibliometrixDB" %in% class(file)) {
#   M <- openalexR::oa2bibliometrix(file)
# } else {
#   M <- file
# }
    }
  )
  if ("PY" %in% names(M)){M$PY=as.numeric(M$PY)} else {M$PY <- NA}
  
  if ("TC" %in% names(M)){
    M$TC=as.numeric(M$TC)
    M$TC[is.na(M$TC)] <- 0
  } else {M$TC <- 0}
  
  if (!("CR" %in% names(M))){
    M$CR="none"
  } else {
    M$CR <- trim.leading(trimES(gsub("\\[,||\\[||\\]|| \\.\\. || \\. ","",M$CR)))  # remove foreign characters from CR (i.e. Chinese, Russian characters)
  }
  
  if (dbsource %in% c("cochrane","openalex_api")){M$AU <- gsub(intToUtf8(8217),intToUtf8(39),M$AU)}
  
  cat("Done!\n\n")
  
  if (!(dbsource %in% c("pubmed", "lens", "openalex_api"))) {
    ## AU_UN field creation
    if ("C1" %in% names(M)) {
      cat("\nGenerating affiliation field tag AU_UN from C1:  ")
      
      if (!"AU_UN" %in% names(M)) M <- metaTagExtraction(M, Field = "AU_UN")
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
  
  # remove double ;
  M <- M %>% 
    mutate_if(is.character, ~gsub(";;",";",.x))
  
  ### SR field creation
  if (isTRUE(remove.duplicates)){
    switch(dbsource,
           isi={
             id_field <- "UT"
           },
           scopus={
             if (format=="csv"){
               id_field <- "UT"
             } else {
               id_field <- "TI"
             }
             
           },
           openalex={
             id_field <- "id_oa"
           },
           openalex_api={
             id_field <- "id_oa"
           },
           dimneisons={
             id_field <- "UT"
           },
           pubmed={
             id_field <- "PMID"
           },
           lens={
             id_field <- "UT"
           },
           {
             id_field <- "TI"
           })
    d <- duplicated(M[id_field]) 
    if (sum(d)>0) cat("\nRemoved ",sum(d),"duplicated documents\n")
    M <- M[!d,]
    }
  suppressWarnings(M <- metaTagExtraction(M, Field="SR"))
  row.names(M) <- M$SR
  
 
  
  ### bibliometrix>DB class
  class(M) <- c("bibliometrixDB", "data.frame")
  
  return(M)

}



