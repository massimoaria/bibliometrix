#' Completeness of bibliographic metadata
#'
#' It calculates the percentage of missing data in the metadata of a bibliographic data frame. 
#' 
#' Each metadata is assigned a status c("Excellent," "Good," "Acceptable," "Critical," "Completely missing") 
#' depending on the percentage of missing data ("0%," "1% to 10%," "11% to 20%"m "21% to 99%," "100%").
#' 
#' The results of the function allow us to understand which analyses can be performed with bibliometrix 
#' and which cannot based on the completeness (or status) of different metadata.
#' @param df is a bibliographic data frame obtained by \code{\link{convert2df}} function.
#' 
#' @return The function \code{missingData} returns a list containing two objects:
#' \tabular{lll}{
#' \code{allTags}  \tab   \tab is a data frame including results for all original metadata tags from the collection\cr
#' \code{mandatoryTags}\tab    \tab is a data frame that included only the tags needed for analysis with bibliometrix and biblioshiny.}
#'
#' @examples
#' data(scientometrics, package = "bibliometrixData")
#' res <- missingData(scientometrics)
#' print(res$mandatoryTags)
#'
#' @export
#' 
missingData <- function(df) {
  cols <- names(df)
  missing_counts <- sapply(cols, function(x){
    sum(is.na(df[,x]) | df[,x] %in% c("NA,0000,NA","NA",""))
    })
  missing_pct <- round(missing_counts/nrow(df) * 100, 2)
  df_all <- data.frame(cols, missing_counts, missing_pct)
  
  tag <- unlist(
    strsplit(
      "AB,AU,C1,CR,DE,DI,DT,ID,LA,NR,PY,RP,SO,TC,TI,WC",","
      )
  )
  description <- trimws(unlist(
    strsplit(
      "Abstract, Author,Affiliation,Cited References,Keywords,DOI,Document Type,Keywords Plus,Language,Number of Cited References,
      Publication Year,Corresponding Author, Journal, Total Citation, Title, Science Categories", ","
    )
  ))
  
  df_all <- df_all %>% 
    mutate(status = status(missing_pct)) %>% 
    replace_na(replace = list(missing_counts = nrow(df), missing_pct = 100))
  
  df_tags <- data.frame(tag, description) %>% 
    left_join(df_all, by = c("tag" = "cols")) %>% 
    replace_na(replace = list(missing_counts = nrow(df), missing_pct = 100, status = "Completely missing")) %>% 
    arrange(missing_pct,description)
  
  results <- list(allTags=df_all, mandatoryTags=df_tags)
  return(results)
}

status <- function(x){
  y <- character(length(x))
  y[x==0] <- "Excellent"
  y[x>0 & x<= 10] <- "Good"
  y[x>10 & x<= 20] <- "Acceptable"
  y[x>20 & x<=50] <- "Poor"
  y[x>50 & x<100] <- "Critical"
  y[is.na(x) | x==100] <- "Completely missing"
  return(y)
}

