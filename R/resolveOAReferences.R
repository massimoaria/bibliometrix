# Internal helpers shared by completeMetadata() and biblioshiny's
# OpenAlex query builder. They convert OpenAlex "work" list objects into
# WoS-style cited-reference strings (used by bibliometrix CR analyses).
# Promoted from inst/biblioshiny/openalex_api.R so the package code can
# reuse the same conversion path without depending on Shiny.

# ---- LTWA-style abbreviation lookup -----------------------------------------

#' @noRd
.journal_word_abbrev <- c(
  "JOURNAL" = "J", "INTERNATIONAL" = "INT", "AMERICAN" = "AM",
  "SCIENCE" = "SCI", "SCIENCES" = "SCI", "SCIENTIFIC" = "SCI",
  "TECHNOLOGY" = "TECHNOL", "TECHNOLOGICAL" = "TECHNOL",
  "RESEARCH" = "RES", "REVIEW" = "REV", "REVIEWS" = "REV",
  "MANAGEMENT" = "MANAGE", "INFORMATION" = "INFORM",
  "ANNUAL" = "ANN", "SOCIAL" = "SOC", "SOCIETY" = "SOC",
  "APPLIED" = "APPL", "APPLICATION" = "APPL", "APPLICATIONS" = "APPL",
  "ENGINEERING" = "ENG", "EDUCATION" = "EDUC", "EDUCATIONAL" = "EDUC",
  "ECONOMICS" = "ECON", "ECONOMIC" = "ECON",
  "ENVIRONMENTAL" = "ENVIRON", "ENVIRONMENT" = "ENVIRON",
  "PSYCHOLOGY" = "PSYCHOL", "PSYCHOLOGICAL" = "PSYCHOL",
  "BIOLOGICAL" = "BIOL", "BIOLOGY" = "BIOL",
  "MEDICAL" = "MED", "MEDICINE" = "MED", "CLINICAL" = "CLIN",
  "PHYSICAL" = "PHYS", "PHYSICS" = "PHYS",
  "CHEMISTRY" = "CHEM", "CHEMICAL" = "CHEM",
  "MATHEMATICAL" = "MATH", "MATHEMATICS" = "MATH",
  "POLITICAL" = "POLIT", "POLITICS" = "POLIT",
  "COMMUNICATION" = "COMMUN", "COMMUNICATIONS" = "COMMUN",
  "COMPUTATIONAL" = "COMPUT", "COMPUTER" = "COMPUT", "COMPUTING" = "COMPUT",
  "STATISTICAL" = "STAT", "STATISTICS" = "STAT",
  "EUROPEAN" = "EUR", "BRITISH" = "BRIT", "CANADIAN" = "CAN",
  "NATIONAL" = "NATL", "GENERAL" = "GEN",
  "AGRICULTURAL" = "AGRIC", "AGRICULTURE" = "AGRIC",
  "GEOGRAPHY" = "GEOGR", "GEOGRAPHICAL" = "GEOGR",
  "PHILOSOPHY" = "PHILOS", "PHILOSOPHICAL" = "PHILOS",
  "UNIVERSITY" = "UNIV", "ASSOCIATION" = "ASSOC",
  "DEVELOPMENT" = "DEV", "EXPERIMENTAL" = "EXP",
  "PROCEEDINGS" = "PROC", "TRANSACTIONS" = "TRANS",
  "LETTERS" = "LETT", "REPORTS" = "REP", "REPORT" = "REP",
  "ANALYSIS" = "ANAL", "ANALYTICAL" = "ANAL",
  "BEHAVIOR" = "BEHAV", "BEHAVIORAL" = "BEHAV",
  "BEHAVIOUR" = "BEHAV", "BEHAVIOURAL" = "BEHAV",
  "COGNITIVE" = "COGN", "CULTURAL" = "CULT", "CULTURE" = "CULT",
  "CURRENT" = "CURR", "DISEASE" = "DIS", "DISEASES" = "DIS",
  "ECOLOGY" = "ECOL", "ECOLOGICAL" = "ECOL",
  "ENERGY" = "ENERG", "MATERIALS" = "MATER", "MOLECULAR" = "MOL",
  "NUTRITION" = "NUTR", "NUTRITIONAL" = "NUTR",
  "OPERATIONS" = "OPER", "ORGANIZATION" = "ORGAN", "ORGANIZATIONAL" = "ORGAN",
  "PHARMACEUTICAL" = "PHARM", "PHARMACY" = "PHARM",
  "STRUCTURAL" = "STRUCT", "SUSTAINABLE" = "SUSTAIN", "SUSTAINABILITY" = "SUSTAIN",
  "THEORETICAL" = "THEOR", "THERAPY" = "THER", "THERAPEUTIC" = "THER",
  "TOURISM" = "TOUR", "OPINION" = "OPIN",
  "NETWORKS" = "NETW", "NETWORK" = "NETW",
  "DECISION" = "DECIS", "MECHANICS" = "MECH", "MECHANICAL" = "MECH",
  "HOSPITALITY" = "HOSP", "PROCESSING" = "PROCESS",
  "SYSTEMS" = "SYST", "SYSTEM" = "SYST",
  "DYNAMICS" = "DYN", "DYNAMIC" = "DYN",
  "PUBLIC" = "PUBL", "POLICY" = "POL",
  "GLOBAL" = "GLOB", "ATMOSPHERIC" = "ATMOS",
  "BIOMEDICAL" = "BIOMED", "BIOCHEMISTRY" = "BIOCHEM", "BIOCHEMICAL" = "BIOCHEM",
  "MICROBIOLOGY" = "MICROBIOL", "GENETICS" = "GENET",
  "NEUROSCIENCE" = "NEUROSCI", "NEUROLOGY" = "NEUROL",
  "ADVANCED" = "ADV", "ADVANCES" = "ADV",
  "METHODS" = "METH", "METHODOLOGY" = "METHODOL",
  "HISTORY" = "HIST", "HISTORICAL" = "HIST",
  "ELECTRONIC" = "ELECTRON", "GEOPHYSICAL" = "GEOPHYS",
  "INDUSTRIAL" = "IND", "INDUSTRY" = "IND",
  "MARINE" = "MAR", "VETERINARY" = "VET",
  "ORGANIC" = "ORG", "INORGANIC" = "INORG",
  "ZOOLOGY" = "ZOOL", "ZOOLOGICAL" = "ZOOL",
  "ASTRONOMY" = "ASTRON", "ASTROPHYSICAL" = "ASTROPHYS",
  "ARCHIVES" = "ARCH", "ARCHIVE" = "ARCH",
  "LIBRARY" = "LIBR", "PROBLEMS" = "PROBL",
  "PLANNING" = "PLAN", "REGIONAL" = "REG",
  "STUDIES" = "STUD", "QUARTERLY" = "Q",
  "ABSTRACT" = "ABSTR", "BULLETIN" = "BULL",
  "CONGRESS" = "CONGR", "CONFERENCE" = "CONF",
  "DOCUMENTATION" = "DOC", "GEOGRAPHIC" = "GEOGR",
  "ENGINEERING" = "ENG", "LANGUAGE" = "LANG",
  "LINGUISTICS" = "LINGUIST", "SURGERY" = "SURG", "SURGICAL" = "SURG"
)

#' @noRd
.journal_stop_words <- c(
  "of", "the", "and", "for", "in", "on", "a", "an", "to",
  "de", "la", "le", "les", "des", "du", "et",
  "der", "die", "das", "und", "fur",
  "del", "di", "e", "il", "lo"
)

# ---- Helpers ----------------------------------------------------------------

#' @noRd
abbreviate_source_name <- function(name) {
  if (is.null(name) || is.na(name) || name == "") return("")

  words <- unlist(strsplit(name, "[\\s:,]+"))
  words <- words[words != ""]

  if (length(words) > 1) {
    keep <- c(TRUE, !tolower(words[-1]) %in% .journal_stop_words)
    words <- words[keep]
  }

  abbreviated <- vapply(words, function(w) {
    W <- toupper(w)
    if (W %in% names(.journal_word_abbrev)) {
      return(.journal_word_abbrev[[W]])
    }
    if (nchar(W) <= 4) return(W)
    return(substr(W, 1, 5))
  }, character(1), USE.NAMES = FALSE)

  paste(toupper(abbreviated), collapse = " ")
}

#' @noRd
format_cr_author <- function(authorships) {
  if (is.null(authorships) || length(authorships) == 0) return("")
  first_auth <- authorships[[1]]
  name <- first_auth$author$display_name
  if (is.null(name) || is.na(name) || name == "") return("")

  parts <- unlist(strsplit(trimws(name), "\\s+"))
  if (length(parts) == 0) return("")
  if (length(parts) == 1) return(toupper(parts[1]))

  surname <- toupper(parts[length(parts)])
  given <- parts[-length(parts)]
  initials <- paste(substr(toupper(given), 1, 1), collapse = "")

  paste0(surname, " ", initials)
}

#' @noRd
build_single_cr <- function(work) {
  parts <- character(0)

  auth <- format_cr_author(work$authorships)
  if (auth != "") parts <- c(parts, auth)

  year <- work$publication_year
  if (!is.null(year) && !is.na(year)) parts <- c(parts, as.character(year))

  source_name <- tryCatch(
    work$primary_location$source$display_name,
    error = function(e) NULL
  )
  if (!is.null(source_name) && !is.na(source_name) && source_name != "") {
    parts <- c(parts, abbreviate_source_name(source_name))
  }

  vol <- tryCatch(work$biblio$volume, error = function(e) NULL)
  if (!is.null(vol) && !is.na(vol) && vol != "") {
    parts <- c(parts, paste0("V", vol))
  }

  page <- tryCatch(work$biblio$first_page, error = function(e) NULL)
  if (!is.null(page) && !is.na(page) && page != "") {
    parts <- c(parts, paste0("P", page))
  }

  doi <- work$doi
  if (!is.null(doi) && !is.na(doi) && doi != "") {
    doi_clean <- gsub("^https?://doi\\.org/", "", doi)
    parts <- c(parts, paste0("DOI ", toupper(doi_clean)))
  }

  if (length(parts) == 0) return(NA_character_)
  paste(parts, collapse = ", ")
}
