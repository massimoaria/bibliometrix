# openalexR::oa_fetch(), with works converted to a tibble that openalexR 3.1.1
# can no longer build.
#
# Since October 2026 OpenAlex also returns a `doi` inside `primary_location`.
# openalexR::works2df() copies every field of `primary_location` except `id`
# and `source` next to the fields of the work, so the tibble gets two `doi`
# columns and fails with "Column name `doi` must not be duplicated" for any
# work. When works are asked as a tibble or data frame, they are fetched as a
# list, the fields of `primary_location` that repeat a field of the work are
# dropped (the work keeps its own), and the list is converted by
# openalexR::oa2df(), as oa_fetch() itself does. Every other request is passed
# to oa_fetch() unchanged.
#' @keywords internal
#' @noRd
oaFetch <- function(..., output = "tibble") {
  args <- list(...)
  convert <- identical(args$entity, "works") &&
    output %in% c("tibble", "dataframe") &&
    !isTRUE(args$count_only) &&
    is.null(args$group_by)
  if (!convert) {
    return(openalexR::oa_fetch(..., output = output))
  }
  res <- openalexR::oa_fetch(..., output = "list")
  if (length(res) == 0) {
    return(NULL)
  }
  # a single work comes back as the record itself
  works <- if (!is.null(res$id)) list(res) else res
  works <- lapply(works, oaDropRepeatedLocationFields)
  openalexR::oa2df(
    works,
    entity = "works",
    options = args$options,
    abstract = if (is.null(args$abstract)) TRUE else args$abstract,
    verbose = isTRUE(args$verbose)
  )
}

oaDropRepeatedLocationFields <- function(work) {
  loc <- work$primary_location
  if (is.list(loc) && length(loc) > 0) {
    repeated <- setdiff(intersect(names(loc), names(work)), c("id", "source"))
    work$primary_location <- loc[setdiff(names(loc), repeated)]
  }
  work
}
