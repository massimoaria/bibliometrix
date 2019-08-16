#' Convert a SCOPUS Export file into a data frame
#'
#' It is an internal function used by \code{\link{convert2df}} to convert a SCOPUS Export file and create a data frame from it, 
#' with cases corresponding to articles and variables to Field Tag in the original file.
#'
#' @param D is a character array containing data read from a SCOPUS Export file (in bibtex format).
#' @return a data frame with cases corresponding to articles and variables to Field Tag in the original SCOPUS file.
#' @examples
#' # A SCOPUS Export file can be read using \code{\link{readFiles}} function:
#'
#' # largechar <- readFiles('filename1.bib','filename2.bib2,...)
#'
#' # filename.bib is a SCOPUS Export file in plain text format.
#'
#' #largechar <- readFiles('http://www.bibliometrix.org/datasets/scopus.bib')
#'
#' 
#' #scopus_df <- scopus2df(largechar)
#'
#' @seealso \code{\link{isi2df}} for converting ISI Export file (in plain text format)
#' @family converting functions
#' 
#' @export

scopus2df<-function(D){

  # this is a legacy function (for old scripts)
  DATA=bib2df(D,dbsource="scopus")
  
  return(DATA)
  
}
