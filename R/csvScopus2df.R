csvScopus2df <- function(file){
  
  
  ## import all files in a single data frame
  for (i in 1:length(file)){
    D <- rio::import(file[i], quote='"')
    
    if (i>1){
      l <- intersect(l,names(D))
      DATA <- rbind(DATA[l],D[l])
    }else{
      l <- names(D)
      DATA <- D}
  }
  
  ## Post-Processing

  # column re-labelling
  DATA <- labelling(DATA)
  
  # Authors' names cleaning (surname and initials)
  DATA$AU=gsub("\\.", "", DATA$AU)
  DATA$AU=gsub(",", ";", DATA$AU)
  
  # Affiliation
  DATA$C1 <- unlist(lapply(strsplit(DATA$C1,";"), function(l){
    l <- paste(gsub(".*\\., ","",l), collapse=";",sep="")
  }))

  # Iso Source Titles
  DATA$J9 <- gsub("\\.","",DATA$JI)
 
  DI <- DATA$DI
  URL <- DATA$URL
  DATA <- data.frame(lapply(DATA,toupper),stringsAsFactors = FALSE)
  DATA$DI <- DI
  DATA$URL <- URL
  return(DATA)
} 




labelling <- function(DATA){
  ## column re-labelling
  label <- names(DATA)
  label <- gsub("Abbreviated Source Title","JI",label)
  label <- gsub("Authors with affiliations","C1",label)
  label <- gsub("Authors","AU",label)
  label <- gsub("Source title","SO",label)
  label <- gsub("Title","TI",label)
  label <- gsub("Year","PY",label)
  label <- gsub("Volume","VL",label)
  label <- gsub("Issue","IS",label)
  label <- gsub("Page count","PP",label)
  label <- gsub("Cited by","TC",label)
  label <- gsub("DOI","DI",label)
  label <- gsub("Link","URL",label)
  label <- gsub("Abstract","AB",label)
  label <- gsub("Author Keywords","DE",label)
  label <- gsub("Index Keywords","ID",label)
  label <- gsub("Funding Details","FU",label)
  label <- gsub("Funding Text 1","FX",label)
  label <- gsub("References","CR",label)
  label <- gsub("Correspondence Address","RP",label)
  label <- gsub("Funding Details","FU",label)
  label <- gsub("Language of Original Document","LA",label)
  label <- gsub("Document Type","DT",label)
  label <- gsub("Source","DB",label)
  label <- gsub("EID","UT",label)
  names(DATA) <- label
  return(DATA)
}
