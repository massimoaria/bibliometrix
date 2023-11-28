#' Calculate the normalized citation score metric
#'
#' It calculates the normalized citation score for documents, authors and sources using both global and local citations.
#' 
#' The document Normalized Citation Score (NCS) of a document is calculated by dividing the actual count of citing items by the expected 
#' citation rate for documents with the same year of publication. 
#' 
#' The MNCS of a set of documents, for example the collected works of an individual, or published on a journal, is the average of the NCS values for all the documents in the set.
#' 
#' The NGCS is the NCS calculated using the global citations (total citations that a document received considering the whole bibliographic database).
#' 
#' The NLCS is the NCS calculated using the local citations (total citations that a document received from a set of documents included in the same collection).
#' 
#' 
#' 
#' @param M is a bibliographic data frame obtained by \code{\link{convert2df}} function.
#' @param field is a character. It indicates the unit of analysis on which calculate the NCS. It can be equal to \code{field = c("documents", "authors", "sources")}. Default is \code{field = "documents"}.
#' @param impact.measure is a character. It indicates the impact measure used to rank cluster elements (documents, authors or sources).
#' It can be \code{impact.measure = c("local", "global")}.\\
#' With \code{impact.measure = "local"}, \link{normalizeCitationScore} calculates elements impact using the Normalized Local Citation Score while 
#' using \code{impact.measure = "global"}, the function uses the Normalized Global Citation Score to measure elements impact. 
#' @return a dataframe.
#'
#' 
#'
#' @examples
#' 
#' \dontrun{
#' data(management, package = "bibliometrixData")
#' NCS <- normalizeCitationScore(management, field = "authors", impact.measure = "local")
#' }
#'
#' @export
normalizeCitationScore <- function(M,field="documents", impact.measure = "local"){

  
  if (!(field %in% c("documents", "authors", "sources"))) {
    cat('\nfield argument is incorrect.\n\nPlease select one of the following choices: "documents", "authors", "sources"\n\n')
    return(NA)
  }
  M$TC=as.numeric(M$TC)
  M$PY=as.numeric(M$PY)
  
  if (impact.measure=="local"){
    log <- capture.output(M <- localCitations(M, fast.search = FALSE, sep = ";")$M)
  } else {
    M$LCS <- 0
  }
  
  M <- M %>% 
    group_by(.data$PY) %>%
    mutate(LCS = replace(.data$LCS, .data$LCS==0, 1),
           NGCS = .data$TC/mean(.data$TC,na.rm=TRUE),
           NLCS = .data$LCS/mean(.data$LCS,na.rm=TRUE)) %>%
    ungroup() %>% as.data.frame()
  
  
  switch(field,
         documents={
           #### Documents Impact by Normalized Local Citations ####
           NCS <- M %>%
             select(.data$SR, .data$PY, .data$NGCS, .data$NLCS, .data$TC, .data$LCS) %>%
             rename(MNGCS = .data$NGCS,
                    MNLCS = .data$NLCS,
                    LC = .data$LCS
             ) %>% 
             rename(documents=.data$SR) %>% as.data.frame()
         },
         authors={
           #### Authors Impact by Normalized Local Citations ####
           AU=names(tableTag(M,"AU"))
           df=data.frame("Author"="NA","SR"=NA,"year"=NA, "TC"=NA,"LCS"=NA,"NGCS"=NA,"NLCS"=NA)
           
           if (!("DI" %in% names(M))){M$DI="NA"}
           for (i in 1:length(AU)){
             
             ind=which(regexpr(AU[i],M$AU)>-1)
             dfAU=data.frame("Author"=rep(AU[i],length(ind)), "SR"=M$PY[ind], "year"=M$PY[ind],
                             "TC"=M$TC[ind], "LCS"=M$LCS[ind],"NGCS"=M$NGCS[ind],
                             "NLCS"=M$NLCS[ind])
             df=rbind(df,dfAU)
           }
           df=df[-1,]
           
           NCS <- df  %>%
             group_by(.data$Author) %>%
             summarize(NP = length(.data$year),
                       MNGCS = mean(.data$NGCS,na.rm=TRUE),
                       MNLCS = mean(.data$NLCS,na.rm=TRUE),
                       TC = mean(.data$TC,na.rm=TRUE),
                       LC = mean(.data$LCS,na.rm=TRUE) 
             ) %>% 
             rename(authors=.data$Author) %>% as.data.frame()
         },
         sources={
           #### Source Impact by Normalized Local Citations ####
           NCS <- M %>% 
             group_by(.data$SO) %>%
             summarize(NP = length(.data$PY),
                       MNGCS = mean(.data$NGCS,na.rm=TRUE),
                       MNLCS = mean(.data$NLCS,na.rm=TRUE),
                       TC = mean(.data$TC,na.rm=TRUE),
                       LC = mean(.data$LCS,na.rm=TRUE)
             ) %>%
             rename(sources=.data$SO) %>% as.data.frame()
         })
  
  if (impact.measure == "global"){
    NCS <- NCS[,-c(4,6)]
  }else{
    NCS$MNLCS[is.na(NCS$MNLCS)] <- 0
  }
  
  return(NCS)
}
