#' DEPRECATED: Load a sequence of ISI or SCOPUS Export files into a large character object
#'
#' The function readFiled is deprecated. You can import and convert your export files directly using the function \code{\link{convert2df}}.
#' 
#' @param ... is a sequence of names of files downloaded from WOS.(in plain text or bibtex format) or SCOPUS Export file (exclusively in bibtex format).
#' @return a character vector of length the number of lines read.
#'
#' @examples
#' # WoS or SCOPUS Export files can be read using \code{\link{readFiles}} function:
#'
#' # largechar <- readFiles('filename1.txt','filename2.txt','filename3.txt')
#'
#' # filename1.txt, filename2.txt and filename3.txt are ISI or SCOPUS Export file 
#' # in plain text or bibtex format.
#'
#' # D <- readFiles('https://www.bibliometrix.org/datasets/bibliometrics_articles.txt')
#'
#' @seealso \code{\link{convert2df}} for converting SCOPUS of ISI Export file into a dataframe
#' 
#' @export

readFiles <- function(...){
  
  cat("\nFrom version 3.0.0, the function readFiles has been dropped.\nPlease use the function 'convert2df' to import and convert your export files")
  # arguments <- unlist(list(...))
  # k=length(arguments)
  # D=list()
  # enc="UTF-8"
  # origEnc=getOption("encoding")
  # if (origEnc=="UTF-8"){options(encoding = "native.enc")}
  #   for (i in 1:k){
  #     D[[i]]=suppressWarnings(
  #       iconv(readLines(arguments[i],encoding = "UTF-8"),"latin1", "ASCII", sub="")
  #       #conv(readLines(arguments[[i]]))
  #       )
  #     }
  # D=unlist(D)
  # options(encoding = origEnc)
  # Encoding(D) <- "UTF-8"
  # return(D)
  return(NULL)
  
}