#' Dataset of "Bibliometrics" manuscripts.
#'
#' The set of manuscripts which the title containing the word "bibliometrics" and published in a journal indexed by Clarivate Analytics WoS database.\cr
#' Period: 2006 - 2015\cr
#' Database: \href{https://www.webofknowledge.com}{Clarivate Analytics Web of Science}\cr
#'
#' @format #' A data frame with 99 rows (manuscripts) and 16 variables (WOS tag field):
#' \describe{
#' \item{AU}{Authors}
#' \item{TI}{Document Title}
#' \item{SO}{Publication Name (or Source)}
#' \item{JI}{ISO Source Abbreviation}
#' \item{DT}{Document Type}
#' \item{DE}{Author Keywords}
#' \item{ID}{Keywords associated by ISI or SCOPUS database}
#' \item{AB}{Abstract}
#' \item{C1}{Author Address}
#' \item{RP}{Reprint Address}
#' \item{CR}{Cited References}
#' \item{TC}{Times Cited}
#' \item{PY}{Year}
#' \item{SC}{Subject Category}
#' \item{UT}{Unique Article Identifier}
#' \item{DB}{Database}
#' }
#'  
#' @source \url{https://www.webofknowledge.com}
#' @name biblio_df
NULL
