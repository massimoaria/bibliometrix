# Unicode-space normalisation (internal).
#
# Exports from Scopus, WoS, OpenAlex, PubMed, ... sometimes carry non-ASCII
# separators inside names and titles: no-break space (U+00A0), narrow no-break
# space (U+202F), thin/figure spaces and the other Zs characters, zero-width
# space (U+200B), word joiner (U+2060) and the BOM (U+FEFF). R's regex
# [:space:]/[^[:alnum:]] (TRE) does not treat all of them as spaces, so
# "Abu<U+00A0>Afifa M." and "Abu Afifa M." became two different authors.
# Word joiner and BOM are invisible non-breaks and are removed; every run of
# the other characters (ASCII space and tab included) becomes a single ASCII
# space. U+200C/U+200D (zero-width non-joiner/joiner) are left alone: they are
# meaningful inside Persian and Indic words. NA stays NA; no trimming here.
# Used by convert2df(), mergeDbSources() and completeMetadata().
#' @noRd
.norm_spaces <- function(x) {
  x <- stringi::stri_replace_all_regex(x, "[\\u2060\\uFEFF]", "")
  stringi::stri_replace_all_regex(x, "[\\p{Zs}\\t\\u200B]+", " ")
}

# Strings .norm_spaces() would change: a run of 2+ space-like characters, or
# any space-like character other than the ASCII space (the Zs class is
# U+0020, U+00A0, U+1680, U+2000-U+200A, U+202F, U+205F, U+3000).
.NORM_SPACES_HIT <- paste0(
  "[\\p{Zs}\\t\\u200B]{2,}|",
  "[\\t\\u00A0\\u1680\\u2000-\\u200B\\u202F\\u205F\\u2060\\u3000\\uFEFF]"
)

# Apply .norm_spaces() to every character column of a data frame, keeping
# names, attributes and NA. Only the values that change are rewritten (and
# trimmed); the others are left untouched.
#' @noRd
.norm_spaces_df <- function(M) {
  for (f in names(M)) {
    x <- M[[f]]
    if (!is.character(x)) next
    hit <- stringi::stri_detect_regex(x, .NORM_SPACES_HIT)
    hit[is.na(hit)] <- FALSE
    if (any(hit)) {
      x[hit] <- trimws(.norm_spaces(x[hit]))
      M[[f]] <- x
    }
  }
  M
}
