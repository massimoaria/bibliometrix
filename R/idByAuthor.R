#' Get Complete Author Information and ID from Scopus 
#'
#' Uses SCOPUS API author search to identify author identification information.
#'
#' @param df is a dataframe composed of three columns:
#' \tabular{lll}{
#' \code{lastname}\tab   \tab author's last name\cr
#' \code{firstname}\tab   \tab author's first name\cr
#' \code{affiliation}\tab   \tab Part of the affiliation name (university name, city, etc.)}
#' i.e. df[1,1:3]<-c("aria","massimo","naples")
#' When affiliation is not specified, the field df$affiliation have to be NA.
#' i.e. df[2,1:3]<-c("cuccurullo","corrado", NA)
#' @param api_key is a character. It contains the Elsevier API key. Information about how to obtain an API Key \href{https://dev.elsevier.com/sc_apis.html}{Elsevier API website}
#' @return a data frame with cases corresponding to authors and variables to author's information and ID got from SCOPUS.
#' @examples
#' ## Request a personal API Key to Elsevier web page https://dev.elsevier.com/sc_apis.html
#' #
#' # api_key="your api key"
#'
#' ## create a data frame with the list of authors to get information and IDs
#' # i.e. df[1,1:3]<-c("aria","massimo","naples")
#' #      df[2,1:3]<-c("cuccurullo","corrado", NA)
#' 
#' ## run idByAuthor function
#' #
#' # authorsID <- idByAuthor(df, api_key)
#' 
#' @seealso \code{\link{retrievalByAuthorID}} for downloading the complete author bibliographic collection from SCOPUS
#' 
#' @export

idByAuthor<-function(df,api_key){
  
  n=dim(df)[1]
  
  ### download authors' info
  AU_ID=NA
  AU_AFF=NA
  AU_count=NA
  
  for (j in 1:n){
    lastname=tolower(df[j,1])
    firstname=tolower(df[j,2])
    if (!is.na(df[j,3])){query=paste("affil(",df[j,3],")",sep="")}else{query=NULL}
    cat("\nSearching author's info: ",toupper(df[j,1]),toupper(df[j,2]))
    
    AU_info=get_complete_author_info(last_name=lastname,first_name=firstname, api_key=api_key,query=query)
  
    ### author id
    if (AU_info$content$`search-results`$`opensearch:totalResults`!=0){
      AU_ID[j]=AU_info[[2]]$`search-results`$entr[[1]]$`dc:identifier`
      AU_ID[j]=gsub("AUTHOR_ID:","",AU_ID[j])
      AU_info2=AU_info[[2]]
      aff=AU_info2$`search-results`$entry[[1]]$`affiliation-current`
      AU_AFF[j]=paste(aff$`affiliation-name`, ", ",aff$`affiliation-city`,", ", aff$`affiliation-country`,sep="")
      ### author document counts
      AU_count[j]=AU_info[[2]]$`search-results`$entr[[1]]$`document-count`
      }else{
      AU_ID[j]=NA
      AU_AFF[j]=NA
      AU_count[j]=NA
      }
  }
  authorsID=data.frame(lastname=df[,1],firstname=df[,2],id=AU_ID,affiliation=AU_AFF,count=AU_count,stringsAsFactors = FALSE)
  return(authorsID)
}
    
