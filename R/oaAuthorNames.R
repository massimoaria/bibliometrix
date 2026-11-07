# OpenAlex gives author names in natural order ("Frederick H. Silver"), while
# every other converter, and every analysis that reads AU, expects the surname
# first ("SILVER FH"). Rewrite the AU and AF fields of OpenAlex records in that
# form, so the same author is the same string in a merged collection.
#
# AF is the source of the rewrite and stays a full name ("SILVER, FREDERICK H"),
# which makes the function idempotent: a name already holding a comma is not
# reordered. Author positions are preserved, so AU stays aligned with AU_ID.
oaAuthorForm <- function(M, rows = seq_len(nrow(M))) {
  if (!all(c("AU", "AF") %in% names(M)) || length(rows) == 0) {
    return(M)
  }
  rows <- rows[!is.na(M$AF[rows]) & nzchar(trimws(M$AF[rows]))]
  if (length(rows) == 0) {
    return(M)
  }

  names_by_doc <- strsplit(M$AF[rows], "\\s*;\\s*")
  all_names <- unlist(names_by_doc, use.names = FALSE)
  uniq <- unique(all_names)
  parsed <- oaParseNames(uniq)
  n <- lengths(names_by_doc)
  doc <- rep(seq_along(rows), n)
  idx <- match(all_names, uniq)
  collapse <- function(x) {
    out <- rep(NA_character_, length(rows))
    pasted <- tapply(x, doc, paste, collapse = ";")
    out[as.integer(names(pasted))] <- pasted
    out
  }
  M$AU[rows] <- collapse(parsed$AU[idx])
  M$AF[rows] <- collapse(parsed$AF[idx])

  # The corresponding author follows AU. A name not found among the original
  # ones is already in the new form, or is not an author name: it stays as it is.
  for (col in intersect(c("corresponding_author_name", "AU_CORR"), names(M))) {
    x <- trimws(toupper(M[[col]][rows]))
    known <- match(x, toupper(uniq))
    M[[col]][rows] <- ifelse(is.na(known), M[[col]][rows], parsed$AU[known])
  }
  M
}

# The words that open a surname of more than one word. The surname is the last
# word of the name plus the particles right before it, and at least one given
# name always remains. Other compound surnames ("Garcia Lopez") cannot be told
# from a middle name, and only their last word is taken as the surname.
oaSurnameParticles <- c(
  "VAN", "VON", "DER", "DEN", "TER", "TEN", "DE", "DEL", "DELLA", "DELLE",
  "DEGLI", "DI", "DA", "DAL", "DALLA", "DOS", "DAS", "DO", "DU", "LA", "EL",
  "AL", "BIN", "IBN"
)

oaCollectiveAuthor <- paste0(
  "\\b(ORGANI[SZ]ATION|CONSORTIUM|COLLABORATION|COLLABORATIVE|COMMITTEE|GROUP|",
  "ASSOCIATION|SOCIETY|UNIVERSITY|INSTITUTE|NETWORK|FOUNDATION|TEAM|",
  "INVESTIGATORS|CENTER|CENTRE)\\b"
)

oaParseNames <- function(x) {
  # The typographic hyphens and apostrophes the API returns (see #596)
  x <- gsub("[\u2010\u2011\u2012\u2013]", "-", x)
  x <- gsub("\u2019", "'", x)
  x <- trimws(gsub("\\s+", " ", gsub("\\.", " ", toupper(x))))
  # A trailing group in parentheses ("WEI YANG (109917)") is not part of the
  # name: left in place it would be taken for the surname.
  bare <- trimws(gsub("(\\s*\\([^()]*\\))+$", "", x))
  x <- ifelse(nzchar(bare), bare, x)

  parse_one <- function(name) {
    if (!nzchar(name) || grepl(oaCollectiveAuthor, name)) {
      return(c(name, name))
    }
    if (grepl(",", name, fixed = TRUE)) {
      surname <- trimws(sub(",.*$", "", name))
      given <- trimws(gsub(",", " ", sub("^[^,]*,", "", name)))
      given <- strsplit(trimws(gsub("\\s+", " ", given)), " ")[[1]]
    } else {
      tokens <- strsplit(name, " ")[[1]]
      if (length(tokens) > 2) {
        tokens <- tokens[!(seq_along(tokens) == length(tokens) &
          tokens %in% c("JR", "SR", "II", "III", "IV"))]
      }
      n <- length(tokens)
      if (n < 2) {
        return(c(name, name))
      }
      start <- n
      while (start > 2 && tokens[start - 1] %in% oaSurnameParticles) {
        start <- start - 1
      }
      surname <- paste(tokens[start:n], collapse = " ")
      given <- tokens[seq_len(start - 1)]
    }
    if (length(given) == 0 || !nzchar(surname)) {
      return(c(name, name))
    }
    pieces <- unlist(strsplit(given, "-", fixed = TRUE))
    pieces <- pieces[nzchar(pieces)]
    initials <- paste(substr(pieces, 1, 1), collapse = "")
    c(
      trimws(paste(surname, initials)),
      paste0(surname, ", ", paste(given, collapse = " "))
    )
  }

  res <- vapply(x, parse_one, character(2), USE.NAMES = FALSE)
  list(AU = res[1, ], AF = res[2, ])
}
