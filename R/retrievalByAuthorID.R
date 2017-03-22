#' Get Author Content on SCOPUS by ID
#'
#' Uses SCOPUS API search to get information about documents on a set of authors using SCOPUS ID.
#'
#' @param id is a vector of characters containing the author's SCOPUS IDs. 
#' SCOPUS IDs con be obtained using the function \code{\link{idByAuthor}}.
#' @param api_key is a character. It contains the Elsvier API key. Information about how to obtain an API Key \href{https://dev.elsevier.com/sc_apis.html}{Elsevier API website}
#' @param remove.duplicated is logical. If TRUE duplicated documents will be deleted from the bibliographic collection.
#' @return a data frame with cases corresponding to articles and variables to main Field Tags named using the standard ISI WoS Field Tag codify. 
#' The main field tags are:
#'
#' \tabular{lll}{
#' \code{AU}\tab   \tab Authors\cr
#' \code{TI}\tab   \tab Document Title\cr
#' \code{SO}\tab   \tab Publication Name (or Source)\cr
#' \code{DT}\tab   \tab Document Type\cr
#' \code{DE}\tab   \tab Authors' Keywords\cr
#' \code{ID}\tab   \tab Keywords associated by SCOPUS or ISI database \cr
#' \code{AB}\tab   \tab Abstract\cr
#' \code{C1}\tab   \tab Author Address\cr
#' \code{RP}\tab   \tab Reprint Address\cr
#' \code{TC}\tab   \tab Times Cited\cr
#' \code{PY}\tab   \tab Year\cr
#' \code{UT}\tab   \tab Unique Article Identifier\cr
#' \code{DB}\tab   \tab Database\cr}
#'
#' LIMITATIONS: 
#' Currently, SCOPUS API does not allow to download document references. 
#' As consequence, it is not possible to perform co-citation analysis (the field CR is empty).
#'
#' @examples
#' ## Request a personal API Key to Elsevier web page https://dev.elsevier.com/sc_apis.html
#' 
#' ## api_key="your api key"
#'
#' ## create a data frame with the list of authors to get information and IDs
#' # i.e. df[1,1:3] <- c("aria","massimo","naples")
#' #      df[2,1:3] <- c("cuccurullo","corrado", "naples")
#' 
#' ## run idByAuthor function
#' #
#' # authorsID <- idByAuthor(df, api_key)
#' #
#' 
#' ## extract the IDs
#' # 
#' # id <- authorsID[,3]
#' #
#' 
#' ## create the bibliographic collection
#' # 
#' # M <- retrievalByAuthor(id, api_key)
#' #
#' 
#' @seealso \code{\link{idByAuthor}} for downloading auhtor information and SCOPUS ID.
#' 
#' @export
#' 
retrievalByAuthorID<-function(id, api_key, remove.duplicated=TRUE){
  
  id=id[!is.na(id)]
  M_list=list()
  n=length(id)
  M=data.frame(AU=NA,TI=NA,AB=NA,SO=NA,JI=NA,DOI=NA,DT=NA,DE=NA,PY=NA,TC=NA,C1=NA,RP=NA,UT=NA,AU_CO=NA)
  for (j in 1:n){
    AU_ID=id[j]
    cat("\n Query n. ",j,"   Author ID: ",AU_ID)
    ### documents of an author
    AU_S=author_search(au_id = AU_ID, api_key=api_key)
    AU_count=length(AU_S[[1]])
    
    ### first document information
    AU=NA
    TI=NA
    AB=NA
    SO=NA
    JI=NA
    DOI=NA
    PY=NA
    TC=NA
    C1=NA
    RP=NA
    UT=NA
    DT=NA
    DE=NA
    AU_CO=NA
  
    ### documents retrieval
    for (i in 1:AU_count){
      D=AU_S[[1]][[i]]
      AU[i]=paste(unlist(lapply(D$author,function(l){a=l$authname})),collapse=";")
      
      AU[i]=paste(unique(trim(unlist(strsplit(AU[i],";")))),collapse =";")
      
      if (!is.null(D$`dc:title`)){TI[i]=D$`dc:title`}else{TI[i]=NA}
      if (!is.null(D$`dc:description`)){AB[i]=D$`dc:description`}else{AB[i]=NA}
      if (!is.null(D$subtypeDescription)){DT[i]=D$subtypeDescription}else{DT[i]=NA}
      if (!is.null(D$`dc:identifier`)){UT[i]=D$`dc:identifier`}else{UT[i]=NA}
      if (!is.null(D$`prism:publicationName`)){SO[i]=D$`prism:publicationName`}else{SO[i]=NA}
      JI[i]=SO[i]
      if (!is.null(D$`prism:doi`)){DOI[i]=D$`prism:doi`}else{DOI[i]=NA}
      if (!is.null(D$authkeywords)){DE[i]=gsub("\\|",";",D$authkeywords)}else{DE[i]=NA}
      if (!is.null(D$`prism:coverDate`)){PY[i]=as.numeric(substring(D$`prism:coverDate`,1,4))}else{PY[i]=NA}
      if (!is.null(D$`citedby-count`)){TC[i]=as.numeric(D$`citedby-count`)}else{TC[i]=NA}
      if (!is.null(D$affiliation)){
        aff=D$affiliation
        C1[i]=paste(unlist(lapply(aff,function(l){
          a=paste(l$affilname, ", ",l$`affiliation-city`,", ", l$`affiliation-country`,sep="")})),collapse=";")
        l=aff[[1]]
        RP[i]=paste(l$affilname, ", ",l$`affiliation-city`,", ", l$`affiliation-country`,sep="")
        AU_CO[i]=paste(unlist(lapply(aff,function(l){a=l$`affiliation-country`})),collapse=";")
      }else{
        C1[i]=NA 
        RP[i]=NA 
        AU_CO[i]=NA}
    }
    M_AU=data.frame(AU,TI,AB,SO,JI,DOI,DT,DE,PY,TC,C1,RP,UT,AU_CO,stringsAsFactors = FALSE)
    M=rbind(M,M_AU)
    M_list[[j]]=M_AU
    names(M_list)[j]=id[j]
  }
  M=M[-1,]  ### remove first empty row
  
  if (isTRUE(remove.duplicated)){
    d=duplicated(gsub("[^[:alnum:] ]","",M$UT))
    cat("\n",sum(d),"duplicated documents have been removed\n")
    M=M[!d,]
  }
  M$CR=NA
  M$DB="SCOPUS"
  M$ID=M$DE
  M <- mutate_each(M, funs(toupper))
  results=list(M=M,authorDocuments=M_list)
  return(results)
}

