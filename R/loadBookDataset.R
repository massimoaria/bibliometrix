#' Load the Biblioshiny Book Dataset
#'
#' Downloads and caches the management collection dataset used in the book
#' "Science Mapping Analysis - A primer with Biblioshiny"
#' by Massimo Aria and Corrado Cuccurullo (McGraw-Hill, 2026).
#' The dataset is downloaded from GitHub on first use and
#' cached locally in \code{~/.bibliometrix/} for subsequent calls.
#'
#' The dataset contains a comprehensive collection of original research articles,
#' written in English, that employ bibliometric or scientometric methods within
#' management and business fields, as indexed by Web of Science, over the period
#' 1985--2025.
#'
#' @param force_download Logical. If \code{TRUE}, re-downloads the dataset even
#'   if a cached copy exists locally. Default is \code{FALSE}.
#' @importFrom utils download.file
#'
#' @return A bibliometrix data frame containing the book dataset.
#'
#' @references
#' Aria, M., & Cuccurullo, C. (2026). \emph{Science Mapping Analysis - A primer
#' with Biblioshiny}. McGraw-Hill. ISBN: 978-88-386-2297-7.
#'
#' @examples
#' \dontrun{
#' M <- loadBookDataset()
#' }
#'
#' @export
loadBookDataset <- function(force_download = FALSE) {
  cache_dir <- file.path(path.expand("~"), ".bibliometrix")
  cache_file <- file.path(cache_dir, "management_collection.rdata")
  url <- "https://github.com/massimoaria/biblioshiny-book/raw/main/datasets/management_collection.rdata"

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (isTRUE(force_download) || !file.exists(cache_file)) {
    tryCatch(
      download.file(url, cache_file, mode = "wb", quiet = TRUE),
      error = function(e) {
        if (file.exists(cache_file)) file.remove(cache_file)
        stop("Failed to download the book dataset: ", e$message, call. = FALSE)
      }
    )
  }

  env <- new.env(parent = emptyenv())
  load(cache_file, envir = env)
  obj_names <- ls(env)

  if (length(obj_names) == 0) {
    stop("The downloaded file does not contain any R objects.", call. = FALSE)
  }

  get(obj_names[1], envir = env)
}
