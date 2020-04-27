#' Get Author Content on SCOPUS by ID
#'
#' Uses SCOPUS API search to get information about documents on a set of authors using SCOPUS ID.
#'
#' @param id is a vector of characters containing the author's SCOPUS IDs. 
#' SCOPUS IDs con be obtained using the function \code{\link{idByAuthor}}.
#' @param api_key is a character. It contains the Elsvier API key. Information about how to obtain an API Key \href{https://dev.elsevier.com/sc_apis.html}{Elsevier API website}
#' @param remove.duplicated is logical. If TRUE duplicated documents will be deleted from the bibliographic collection.
#' @param country is logical. If TRUE authors' country information will be downloaded from SCOPUS.
#' @return a list containing two objects: (i) M which is a data frame with cases corresponding to articles and variables to main Field Tags named using the standard ISI WoS Field Tag codify. 
#' M includes the entire bibliographic collection downloaded from SCOPUS.
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
#' (ii) authorDocuments which is a list containing a bibliographic data frame for each author.
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
#' # res <- retrievalByAuthorID(id, api_key)
#' #
#' # M <- res$M  # the entire bibliographic data frame
#' # M <- res$authorDocuments # the list containing a bibliographic data frame for each author
#' 
#' @seealso \code{\link{idByAuthor}} for downloading author information and SCOPUS ID.
#' 
#' @export
#' 
retrievalByAuthorID<-function(id, api_key, remove.duplicated=TRUE, country=TRUE){
  
  id=id[!is.na(id)]
  M_list=list()
  n=length(id)
  nomi=c("au_id","name","affil_id","affilname","n_auth","n_affils","citations","journal","description",
         "title","pii","doi","eid","cover_date","cover_display_date","prism_url","dc_identifier",
         "dc_creator","prism_issn","prism_eIssn","prism_pageRange","dc_description","prism_aggregationType",
         "subtype","authkeywords","source_id" )
  M=data.frame(matrix(NA,1,length(nomi)))
  names(M)=nomi
  
  for (j in 1:n) {
    AU_ID = id[j]
    cat("\n Query n. ", j, "   Author ID: ", AU_ID)
    ### documents of an author
    
    AU_S <- tryCatch(
      author_df_orig(
        au_id = AU_ID,
        api_key = api_key,
        all_author_info = TRUE,
        verbose = FALSE
      ), error = function(e) err = 1)
    
    if (class(AU_S)!="numeric") {
      AU_S$cover_date = substr(as.character(AU_S$cover_date), 1, 4)
      
      for (i in 1:dim(AU_S)[2]) {
        if (is.factor(AU_S[[i]])) {
          AU_S[[i]] = as.character(AU_S[[i]])
        }
      }
      
      M_AU = data.frame(AU_S, stringsAsFactors = FALSE)
      
      if (dim(M_AU)[2] <= dim(M)[2]) {
        M_AU[setdiff(names(M), names(M_AU))] = NA
      }
      M = rbind(M, M_AU[names(M)])
      M_list[[j]] = M_AU
      names(M_list)[j] = id[j]
    } else {cat("\n Error in id:",AU_ID, "retrieval\n")}
    
  }
  M=M[-1,]  ### remove first empty row
  names(M)=c("AU_ID","AU","C1_ID","C1","nAU","nC1","TC","SO","DT","TI","PII","DI","EID","PY","CDD", "URL","UT","AU1","ISSN","EISSN","PAG","AB","PT","SUBTYPE","DE","SO_ID")
  if (isTRUE(remove.duplicated)){
    d=duplicated(gsub("[^[:alnum:] ]","",M$UT))
    cat("\n",sum(d),"duplicated documents have been removed\n")
    M=M[!d,]
  }
  M$CR=NA
  M$DB="SCOPUS"
  M$DE=gsub("\\| ",";",M$DE)
  M$ID=M$DE
  
  
  ### da rivedere ###
  if (isTRUE(country)){
  M$AU_CO=paste(M$C1_ID,";",sep="")
  
  ### country retrieval
  cat("\nAuthors' country retrieval\n\n")
  aff_id=sort(unique(unlist(strsplit(M$C1_ID,";"))))
  aff_id=aff_id[nchar(aff_id)>1]
  
  AFF=data.frame(ID=NA,NAME=NA,CO=NA)
  for (i in 1:length(aff_id)){
    a=affiliation_retrieval(aff_id[i],api_key=api_key,verbose=FALSE)
    AFF[i,1]=aff_id[i]
    if(length(a$content$`affiliation-retrieval-response`$`affiliation-name`)>0){AFF[i,2]=a$content$`affiliation-retrieval-response`$`affiliation-name`}
    if(length(a$content$`affiliation-retrieval-response`$country)>0){AFF[i,3]=a$content$`affiliation-retrieval-response`$country}
    cat("\nAffiliation ID: ", AFF[i,1],"   Name: ",AFF[i,2], ",", AFF[i,3])
    M$AU_CO=gsub(paste(aff_id[i],";",sep = ""),paste(AFF[i,3],";",sep=""),M$AU_CO)
  }
  M$AU_CO=gsub(";;",";",M$AU_CO)
  M$AU_CO[nchar(M$AU_CO)<3]=NA
  M$AU1_CO=unlist(lapply(strsplit(M$AU_CO,";"), function(l){
    l=l[1]
  }))
  UN=strsplit(M$C1,";")
  CO=strsplit(M$AU_CO,";")
  
  for (i in 1:length(UN)){
    M$C1[i]=paste(paste(UN[[i]],", ",CO[[i]],sep=""),collapse=";")
  }
  #########
  }
  M <- data.frame(lapply(M,toupper),stringsAsFactors = FALSE)
  M$TC=as.numeric(M$TC)
  M$PY=as.numeric(M$PY)
  M$DB="SCOPUS"
  M$RP=unlist(lapply(strsplit(M$C1,";"), function(l){
    l=l[1]
  }))
  M$CR<-NA
  M$J9<-M$JI<-M$SO
  ### SR field creation
  suppressWarnings(M <- metaTagExtraction(M, Field="SR"))
  ### identify duplicated SRs 
  SR=M$SR
  tab=table(SR)
  tab2=table(tab)
  ind=as.numeric(names(tab2))
  ind=ind[which(ind>1)]
  if (length(ind)>0){
    for (i in ind){
      indice=names(which(tab==i))
      for (j in indice){
        indice2=which(SR==j)
        SR[indice2]=paste(SR[indice2],as.character(1:length(indice2)),sep=" ")
      }
    }
  }
  
  row.names(M) <- SR
  
  results <- list(M=M,authorDocuments=M_list)
  return(results)
}

