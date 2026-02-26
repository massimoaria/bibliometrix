## ------------------------------------------------------------------
## Script: Generate data/countries.rda from the CSV source
##
## This script reads data-raw/countries.csv, validates it, and saves
## the resulting data frame as data/countries.rda for use by
## bibliometrix functions (biblioAnalysis, metaTagExtraction, etc.).
##
## HOW TO UPDATE THE COUNTRY DICTIONARY:
##   1. Edit data-raw/countries.csv  (add/modify rows as needed)
##   2. Run this script:  source("data-raw/countries.R")
##   3. Update the @format row count in R/countries.R if rows changed
##   4. Commit both data-raw/countries.csv and data/countries.rda
##
## IMPORTANT – Alias entries and normalization:
##   Some countries have both an old and a new official name
##   (e.g. TURKEY / TURKIYE). Both names must appear in the CSV so
##   that bibliometric databases using either variant are matched.
##   The canonical name used in output is controlled by gsub()
##   mappings in R/metaTagExtraction.R (AU_CO and AU1_CO functions).
##   When adding a new alias here, remember to add the corresponding
##   gsub() normalization rule there as well.
##
## CSV columns:
##   countries  – country name (UPPERCASE, used for pattern matching)
##   continent  – continent name (UPPERCASE)
##   iso2       – ISO 3166-1 alpha-2 code (NA for sub-national units)
##   Longitude  – country centroid longitude (decimal degrees)
##   Latitude   – country centroid latitude  (decimal degrees)
## ------------------------------------------------------------------

# Read source CSV
countries <- read.csv(
  "data-raw/countries.csv",
  stringsAsFactors = FALSE,
  na.strings = c("NA", "")
)

# Validate required columns
required_cols <- c("countries", "continent", "iso2", "Longitude", "Latitude")
missing <- setdiff(required_cols, names(countries))
if (length(missing) > 0) {
  stop("Missing columns in countries.csv: ", paste(missing, collapse = ", "))
}

# Enforce uppercase for country and continent names
countries$countries <- toupper(trimws(countries$countries))
countries$continent <- toupper(trimws(countries$continent))
countries$iso2 <- trimws(countries$iso2)

# Check for empty country names
empty <- which(countries$countries == "" | is.na(countries$countries))
if (length(empty) > 0) {
  stop("Empty country names found at rows: ", paste(empty, collapse = ", "))
}

# Check for duplicate country names
dupes <- countries$countries[duplicated(countries$countries)]
if (length(dupes) > 0) {
  stop("Duplicate country names: ", paste(unique(dupes), collapse = ", "))
}

# Sort alphabetically by country name
countries <- countries[order(countries$countries), ]
rownames(countries) <- seq_len(nrow(countries))

# Save
save(countries, file = "data/countries.rda", compress = "xz")

cat(
  "countries.rda saved successfully:",
  nrow(countries), "entries,",
  length(unique(countries$continent)), "continents,",
  sum(!is.na(countries$iso2)), "with ISO2 codes\n"
)
