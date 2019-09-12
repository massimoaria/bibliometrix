#' Load a sequence of ISI or SCOPUS Export files into a large character object
#'
#' It loads a sequence of SCOPUS and Clarivate Analytics WoS export files and create a large character vector from it.
#'
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
#' # D <- readFiles('http://www.bibliometrix.org/datasets/bibliometrics_articles.txt')
#'
#' @seealso \code{\link{convert2df}} for converting SCOPUS of ISI Export file into a dataframe
#' 
#' @export

readFiles <- function(...){
  
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