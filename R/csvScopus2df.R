utils::globalVariables(c("X1","X2","tag","orig"))

csvScopus2df <- function(file){
  options(readr.num_columns = 0)
  
  ## import all files in a single data frame
  for (i in 1:length(file)){
    D <- read_csv(file[i], na=character(), quote='"', trim_ws = FALSE, progress = show_progress(),
                  col_types = cols(.default = col_character())) %>%  # Introduced to remove cols parsing errors
      #mutate(across(!where(is.numeric), as.character)) %>%   # not yet necessary with the inclusion of previuos line
      mutate(across(where(is.character), \(x) tidyr::replace_na(x,""))) %>% as.data.frame()
    
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
  
  ### store raw affiliation format to extract link among authors and affiliations
  DATA$C1raw <- DATA$C1
  ###
  
  # Affiliation
  if (!("C1" %in% names(DATA))){DATA$C1 <- NA}else{
    DATA$C1 <- unlist(lapply(strsplit(DATA$C1,";"), function(l){
      l <- paste(gsub(".*\\., ","",l), collapse=";",sep="")
    }))
  }
  # Iso Source Titles
  if ("JI" %in% names(DATA)){
    DATA$J9 <- gsub("\\.","",DATA$JI)
    }else{
      DATA$J9 <- DATA$JI <- sapply(DATA$SO, AbbrevTitle, USE.NAMES = FALSE)
    }
 
  DI <- DATA$DI
  URL <- DATA$URL
  DATA <- data.frame(lapply(DATA,toupper))
  DATA$DI <- DI
  DATA$URL <- URL
  return(DATA)
} 




labelling <- function(DATA){
  ## column re-labelling
  
  df_tag <- data.frame(
    rbind(c("Abbreviated Source Title","JI"),
          c("Authors with affiliations","C1"),
          c("Author Addresses","C1"),
          c("Authors","AU"),
          c("Author Names","AU"),
          c("Author full names", "AF"),
          c("Source title","SO"),
          c("Titles","TI"),
          c("Title","TI"),
          c("Publication Year","PY"),
          c("Year","PY"),
          c("Volume","VL"),
          c("Issue","IS"),
          c("Page count","PP"),
          c("Cited by","TC"),
          c("DOI","DI"),
          c("Link","URL"),
          c("Abstract","AB"),
          c("Author Keywords","DE"),
          c("Indexed Keywords","ID"),
          c("Index Keywords","ID"),
          c("Funding Details","FU"),
          c("Funding Texts","FX"),
          c("Funding Text 1","FX"),
          c("References","CR"),
          c("Correspondence Address","RP"),
          c("Publisher","PU"),
          c("Open Access","OA"),
          c("Language of Original Document","LA"),
          c("Document Type","DT"),
          c("Source","DB"),
          c("EID","UT"))) %>% 
    rename(orig = X1,
           tag = X2)
  
  label <- data.frame(orig=names(DATA)) %>% 
    left_join(df_tag,by = "orig") %>% 
    mutate(tag = ifelse(is.na(tag),orig,tag))
  
  names(DATA) <- label$tag
  
  
  # label <- names(DATA)
  # label <- gsub("Abbreviated Source Title","JI",label)
  # label <- gsub("Authors with affiliations","C1",label)
  # label <- gsub("Author Addresses","C1",label)
  # #label <- gsub("Affiliations","RP",label)
  # label <- gsub("Authors","AU",label)
  # label <- gsub("Author Names","AU",label)
  # label <- gsub("Source title","SO",label)
  # label <- gsub("Titles","TI",label)
  # label <- gsub("Title","TI",label)
  # label <- gsub("Publication Year","PY",label)
  # label <- gsub("Year","PY",label)
  # label <- gsub("Volume","VL",label)
  # label <- gsub("Issue","IS",label)
  # label <- gsub("Page count","PP",label)
  # label <- gsub("Cited by","TC",label)
  # label <- gsub("DOI","DI",label)
  # label <- gsub("Link","URL",label)
  # label <- gsub("Abstract","AB",label)
  # label <- gsub("Author Keywords","DE",label)
  # label <- gsub("Index Keywords","ID",label)
  # label <- gsub("Funding Details","FU",label)
  # label <- gsub("Funding Text 1","FX",label)
  # label <- gsub("References","CR",label)
  # label <- gsub("Correspondence Address","RP",label)
  # label <- gsub("Funding Details","FU",label)
  # label <- gsub("Language of Original Document","LA",label)
  # label <- gsub("Document Type","DT",label)
  # label <- gsub("Source","DB",label)
  # label <- gsub("EID","UT",label)
  # names(DATA) <- label
  return(DATA)
}
