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
#
# OpenAlex also returns `observed_orcids` for every author, an empty list when
# the author has no ORCID. openalexR turns a NULL field of an author into NA but
# keeps an empty one, and tibble::as_tibble() recycles the author's row to
# length 0: the author vanishes from `authorships` (3.1.0 to 3.1.2.9000). The
# empty fields of each author are made NULL first, so they become NA.
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
  works <- lapply(works, function(w) oaKeepEmptyAuthorFields(oaDropRepeatedLocationFields(w)))
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

oaKeepEmptyAuthorFields <- function(work) {
  if (!is.list(work$authorships) || length(work$authorships) == 0) {
    return(work)
  }
  work$authorships <- lapply(work$authorships, function(a) {
    if (is.list(a) && is.list(a$author) && length(a$author) > 0) {
      # lapply() keeps the NULL elements, which openalexR then turns into NA
      a$author <- lapply(a$author, function(v) if (length(v) == 0) NULL else v)
    }
    a
  })
  work
}
