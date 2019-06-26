#' Convert a Clarivate Analytics WoS Export file into a data frame
#'
#' It is an internal function used by \code{\link{convert2df}} to convert a Clarivate Analytics WoS Export file 
#' and create a data frame from it, with cases corresponding to articles and variables to Field Tag in the original file.
#'
#' @param D is a character array containing data read from an WoS Export file (in bibtex format).
#' @return a data frame with cases corresponding to articles and variables to Field Tag in the original SCOPUS file.
#' @examples
#' # A ISI Export file can be read using \code{\link{readLines}} function:
#'
#' # largechar <- readFiles('filename1.bib','filename2.bib2,...)
#'
#' # filename.bib is a Clarivate Analytics WoS Export file in plain text format.
#'
#' # largechar <- readFiles('http://www.bibliometrix.org/datasets/ranking.bib')
#' 
#' # ranking <- isibib2df(largechar)
#'
#' @seealso \code{\link{isi2df}} for converting ISI Export file (in plain text format)
#' @family converting functions
#' 
#' @export

isibib2df<-function(D){

  # this is a legacy function (for old scripts)
  DATA=bib2df(D,dbsource="isi")
  
  return(DATA)
}
