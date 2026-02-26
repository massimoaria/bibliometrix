# ==============================================================================
# UI Component - WebOfScience-like Query Builder for OpenAlex (Collapsible)
# ==============================================================================

openAlexUI <- function() {
  tagList(
    fluidRow(
      column(
        width = 12,
        # Query Builder Box (Collapsible)
        wellPanel(
          id = "oaQueryPanel",

          # Header with collapse button
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
            h4(
              "OpenAlex Data Collection",
              style = "font-weight: bold; margin: 0;"
            ),
            actionButton(
              "oaToggleQueryPanel",
              icon("chevron-up"),
              class = "btn-sm btn-outline-secondary",
              style = "padding: 5px 10px;",
              title = "Collapse/Expand"
            )
          ),

          # API Key Warning Banner
          uiOutput("oaApiKeyWarning"),

          # Query content (collapsible)
          div(
            id = "oaQueryContent",

            # Query Builder Section (WebOfScience-like)
            div(
              id = "query-builder-container",
              style = "margin-bottom: 20px;",

              # Initial query row (NO operator column)
              div(
                id = "query-row-1",
                class = "query-row",
                style = "margin-bottom: 10px;",
                fluidRow(
                  column(1),
                  column(
                    width = 2,
                    selectInput(
                      "oaField_1",
                      NULL,
                      choices = c(
                        "All Fields" = "default",
                        "Title" = "title",
                        "Abstract" = "abstract",
                        "Title and Abstract" = "title_abstract",
                        "Author" = "author",
                        "Concept" = "concept"
                      ),
                      selected = "title_abstract",
                      width = "100%"
                    )
                  ),
                  column(
                    width = 8,
                    textInput(
                      "oaQuery_1",
                      NULL,
                      value = "",
                      placeholder = 'Example: "machine learning" OR AI',
                      width = "100%"
                    )
                  ),
                  column(
                    width = 1,
                    div(
                      id = "remove-button-1",
                      style = "visibility: hidden;",
                      actionButton(
                        "oaRemoveRow_1",
                        icon("minus-circle"),
                        class = "btn-danger btn-sm",
                        style = "margin-top: 0px;",
                        title = "Remove row"
                      )
                    )
                  )
                )
              )
            ),

            # Help text for search syntax
            div(
              style = "margin-bottom: 15px; font-size: 12px; color: #666;",
              HTML(
                "<strong>Search tips:</strong> Use quotes for exact phrases (e.g., \"science mapping\"). Boolean operators AND, OR, NOT must be UPPERCASE."
              )
            ),

            # Add row, date range, and Search buttons
            div(
              style = "margin-bottom: 20px; display: flex; align-items: center; gap: 10px;",
              actionButton(
                "oaAddRow",
                label = "+ Add row",
                icon = icon("plus"),
                class = "btn-outline-primary btn-sm"
              ),
              actionButton(
                "oaToggleDateRange",
                label = "+ Date range",
                icon = icon("calendar"),
                class = "btn-outline-primary btn-sm"
              ),
              actionButton(
                "oaSearchQuery",
                label = "Search",
                icon = icon("search"),
                class = "btn-info btn-sm",
                style = "margin-left: auto;"
              )
            ),

            # Date Range Section (initially visible)
            div(
              id = "oaDateRangeSection",
              style = "display: block;", # Visibile di default
              wellPanel(
                style = "background-color: #f8f9fa;",
                h5("Date Range Filter", style = "font-weight: bold;"),
                fluidRow(
                  column(
                    width = 6,
                    numericInput(
                      "oaYearFrom",
                      "From Year:",
                      value = 1985,
                      min = 1900,
                      max = as.integer(format(Sys.Date(), "%Y"))
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      "oaYearTo",
                      "To Year:",
                      value = as.integer(format(Sys.Date(), "%Y")),
                      min = 1900,
                      max = as.integer(format(Sys.Date(), "%Y"))
                    )
                  )
                )
              )
            ),

            # Advanced filters toggle
            actionButton(
              "oaToggleAdvanced",
              label = "Advanced filters",
              icon = icon("filter"),
              class = "btn-link",
              style = "padding: 5px; margin-bottom: 10px;"
            ),

            # Additional Filters Section (initially visible)
            div(
              id = "oaAdvancedSection",
              style = "display: block;", # Visibile di default
              wellPanel(
                style = "background-color: #f8f9fa;",
                fluidRow(
                  column(
                    width = 4,
                    selectizeInput(
                      "oaDocType",
                      "Document Type:",
                      choices = c(
                        "All" = "",
                        "Article" = "article",
                        "Book Chapter" = "book-chapter",
                        "Book" = "book",
                        "Dataset" = "dataset",
                        "Dissertation" = "dissertation",
                        "Editorial" = "editorial",
                        "Letter" = "letter",
                        "Preprint" = "preprint",
                        "Review" = "review"
                      ),
                      selected = "",
                      multiple = TRUE
                    )
                  ),
                  column(
                    width = 4,
                    selectizeInput(
                      "oaLanguage",
                      "Language:",
                      choices = c(
                        "All" = "",
                        "English" = "en",
                        "Spanish" = "es",
                        "French" = "fr",
                        "German" = "de",
                        "Italian" = "it",
                        "Portuguese" = "pt",
                        "Chinese" = "zh",
                        "Japanese" = "ja"
                      ),
                      selected = "",
                      multiple = TRUE
                    )
                  ),
                  column(
                    width = 4,
                    numericInput(
                      "oaMaxRecords",
                      "Max Records:",
                      value = 1000,
                      min = 1,
                      max = 10000,
                      step = 100
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 3,
                    selectizeInput(
                      "oaOpenAccess",
                      "Open Access:",
                      choices = c(
                        "All" = "",
                        "Gold" = "gold",
                        "Green" = "green",
                        "Hybrid" = "hybrid",
                        "Bronze" = "bronze",
                        "Diamond" = "diamond",
                        "Closed" = "closed"
                      ),
                      selected = "",
                      multiple = TRUE
                    )
                  ),
                  column(
                    width = 3,
                    selectizeInput(
                      "oaSDG",
                      "SDG:",
                      choices = c(
                        "All" = "",
                        "1 - No Poverty" = "1",
                        "2 - Zero Hunger" = "2",
                        "3 - Good Health and Well-being" = "3",
                        "4 - Quality Education" = "4",
                        "5 - Gender Equality" = "5",
                        "6 - Clean Water and Sanitation" = "6",
                        "7 - Affordable and Clean Energy" = "7",
                        "8 - Decent Work and Economic Growth" = "8",
                        "9 - Industry, Innovation and Infrastructure" = "9",
                        "10 - Reduced Inequalities" = "10",
                        "11 - Sustainable Cities and Communities" = "11",
                        "12 - Responsible Consumption and Production" = "12",
                        "13 - Climate Action" = "13",
                        "14 - Life Below Water" = "14",
                        "15 - Life on Land" = "15",
                        "16 - Peace, Justice and Strong Institutions" = "16",
                        "17 - Partnerships for the Goals" = "17"
                      ),
                      selected = "",
                      multiple = TRUE
                    )
                  ),
                  column(
                    width = 3,
                    selectizeInput(
                      "oaTopic",
                      "Topic:",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(placeholder = "Enter a query first...")
                    )
                  ),
                  column(
                    width = 3,
                    selectizeInput(
                      "oaJournal",
                      "Journal:",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(placeholder = "Enter a query first...")
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 6, offset = 6,
                    radioButtons(
                      "oaDropdownSort",
                      NULL,
                      choices = c("Sort by count" = "count", "Sort A-Z" = "alpha"),
                      selected = "count",
                      inline = TRUE
                    )
                  )
                )
              )
            ),
            # Action buttons section
            div(
              style = "margin-top: 10px; display: flex; justify-content: space-between; align-items: center;",

              # Left side: Reactive query count message + working indicator
              div(
                style = "display: flex; align-items: center; gap: 8px;",
                uiOutput("oaQueryCountInfo"),
                span(
                  id = "oaWorkingLabel",
                  style = "display: none; color: #dc3545; font-size: 13px; font-weight: 600;",
                  icon("spinner", class = "fa-spin", style = "margin-right: 4px;"),
                  "working..."
                )
              ),

              # Right side: Refs checkbox, Clear and Search buttons
              div(
                style = "display: flex; align-items: center; gap: 10px;",
                div(
                  style = "margin-bottom: 0;",
                  checkboxInput(
                    "oaFetchRefs",
                    "Download cited references",
                    value = FALSE
                  ),
                  conditionalPanel(
                    condition = "input.oaFetchRefs == true",
                    div(
                      style = "margin-left: 5px; margin-top: -5px; font-size: 12px;",
                      radioButtons(
                        "oaRefsFilter",
                        NULL,
                        choices = c(
                          "Only cited > 1 (faster)" = "multiple",
                          "All references" = "all"
                        ),
                        selected = "multiple",
                        inline = TRUE
                      )
                    )
                  )
                ),
                actionButton(
                  "oaClearQuery",
                  "Clear",
                  icon = icon("times"),
                  class = "btn-outline-secondary"
                ),
                actionButton(
                  "oaFetchData",
                  "Download",
                  icon = icon("download"),
                  class = "btn-primary"
                )
              )
            )
          )
        )
      )
    ),

    # Data Display Section (shown when data is available)
    conditionalPanel(
      condition = "output.oaDataAvailable",
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4("Downloaded Collection", style = "font-weight: bold;"),

            # Progress and results info
            uiOutput("oaProgressInfo"),

            # Export buttons
            div(
              style = "margin-bottom: 15px; margin-top: 15px;",
              downloadButton(
                "oaDownloadExcel",
                "Export to Excel",
                icon = icon("file-excel"),
                class = "btn-success"
              ),
              downloadButton(
                "oaDownloadRData",
                "Export to RData",
                icon = icon("database"),
                class = "btn-info",
                style = "margin-left: 10px;"
              )
            ),

            # Data table output
            div(
              style = "margin-top: 15px;",
              uiOutput("oaDataTable")
            )
          )
        )
      )
    ),

    # JavaScript for collapsible functionality
    tags$script(HTML(
      "
  $(document).ready(function() {
    // Initialize collapsed state
    var isCollapsed = false;
    
    // Initialize visibility states
    var dateRangeVisible = true;
    var advancedVisible = true;
    
    // Handle toggle button click for main panel
    $('#oaToggleQueryPanel').on('click', function() {
      isCollapsed = !isCollapsed;
      $('#oaQueryContent').slideToggle(300);
      
      // Update button icon
      if (isCollapsed) {
        $(this).html('<i class=\"fa fa-chevron-down\"></i>');
      } else {
        $(this).html('<i class=\"fa fa-chevron-up\"></i>');
      }
    });
    
    // Handle date range toggle
    $('#oaToggleDateRange').on('click', function() {
      dateRangeVisible = !dateRangeVisible;
      $('#oaDateRangeSection').slideToggle(300);
      
      // Update button text
      if (dateRangeVisible) {
        $(this).html('<i class=\"fa fa-calendar\"></i> - Date range');
      } else {
        $(this).html('<i class=\"fa fa-calendar\"></i> + Date range');
      }
      
      // Notify Shiny
      Shiny.setInputValue('oaShowDateRange', dateRangeVisible);
    });
    
    // Handle advanced filters toggle
    $('#oaToggleAdvanced').on('click', function() {
      advancedVisible = !advancedVisible;
      $('#oaAdvancedSection').slideToggle(300);
      
      // Update button text
      if (advancedVisible) {
        $(this).html('<i class=\"fa fa-filter\"></i> Hide advanced filters');
      } else {
        $(this).html('<i class=\"fa fa-filter\"></i> Show advanced filters');
      }
      
      // Notify Shiny
      Shiny.setInputValue('oaShowAdvanced', advancedVisible);
    });
    
    // Set initial button labels
    $('#oaToggleDateRange').html('<i class=\"fa fa-calendar\"></i> - Date range');
    $('#oaToggleAdvanced').html('<i class=\"fa fa-filter\"></i> Hide advanced filters');
  });
  
  // Custom message handler to collapse panel when data is loaded
  Shiny.addCustomMessageHandler('oaCollapseQueryPanel', function(message) {
    $('#oaQueryContent').slideUp(300);
    $('#oaToggleQueryPanel').html('<i class=\"fa fa-chevron-down\"></i>');
  });

  // Show 'working...' indicator when Shiny is busy (scoped to OA panel)
  $(document).on('shiny:busy', function() {
    if ($('#oaQueryPanel').is(':visible')) {
      $('#oaWorkingLabel').show();
    }
  });
  $(document).on('shiny:idle', function() {
    $('#oaWorkingLabel').hide();
  });
"
    ))
  )
}


# ==============================================================================
# Helper Functions - OpenAlex Reference Resolution
# ==============================================================================

# LTWA-like lookup for journal title word abbreviations
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

# Stop words to remove from journal titles
.journal_stop_words <- c(
  "of", "the", "and", "for", "in", "on", "a", "an", "to",
  "de", "la", "le", "les", "des", "du", "et",
  "der", "die", "das", "und", "fur",
  "del", "di", "e", "il", "lo"
)

# Abbreviate a journal/source name to LTWA-like form
abbreviate_source_name <- function(name) {
  if (is.null(name) || is.na(name) || name == "") return("")

  words <- unlist(strsplit(name, "[\\s:,]+"))
  words <- words[words != ""]

  # Remove stop words (keep first word always)
  if (length(words) > 1) {
    keep <- c(TRUE, !tolower(words[-1]) %in% .journal_stop_words)
    words <- words[keep]
  }

  # Abbreviate each word
  abbreviated <- vapply(words, function(w) {
    W <- toupper(w)
    # Check lookup table
    if (W %in% names(.journal_word_abbrev)) {
      return(.journal_word_abbrev[[W]])
    }
    # Short words: keep as is
    if (nchar(W) <= 4) return(W)
    # Fallback: truncate to 5 chars
    return(substr(W, 1, 5))
  }, character(1), USE.NAMES = FALSE)

  paste(toupper(abbreviated), collapse = " ")
}

# Format first author from OpenAlex authorships list for CR string
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

# Build a single WOS-style CR string from an OpenAlex work (list format)
build_single_cr <- function(work) {
  parts <- character(0)

  # Author
  auth <- format_cr_author(work$authorships)
  if (auth != "") parts <- c(parts, auth)

  # Year
  year <- work$publication_year
  if (!is.null(year) && !is.na(year)) parts <- c(parts, as.character(year))

  # Journal
  source_name <- tryCatch(
    work$primary_location$source$display_name,
    error = function(e) NULL
  )
  if (!is.null(source_name) && !is.na(source_name) && source_name != "") {
    parts <- c(parts, abbreviate_source_name(source_name))
  }

  # Volume
  vol <- tryCatch(work$biblio$volume, error = function(e) NULL)
  if (!is.null(vol) && !is.na(vol) && vol != "") {
    parts <- c(parts, paste0("V", vol))
  }

  # First page
  page <- tryCatch(work$biblio$first_page, error = function(e) NULL)
  if (!is.null(page) && !is.na(page) && page != "") {
    parts <- c(parts, paste0("P", page))
  }

  # DOI
  doi <- work$doi
  if (!is.null(doi) && !is.na(doi) && doi != "") {
    doi_clean <- gsub("^https?://doi\\.org/", "", doi)
    parts <- c(parts, paste0("DOI ", toupper(doi_clean)))
  }

  if (length(parts) == 0) return(NA_character_)
  paste(parts, collapse = ", ")
}

# Resolve all references for a set of OpenAlex works
# Returns a named list: openalex_id -> CR string
resolve_openalex_references <- function(oa_data, progress_id = NULL,
                                        progress_bar_id = NULL,
                                        progress_pct_id = NULL,
                                        only_multiple = FALSE) {
  # Collect all referenced work IDs
  all_refs <- unlist(lapply(oa_data, function(w) w$referenced_works))
  all_refs <- all_refs[!is.na(all_refs) & all_refs != ""]

  if (length(all_refs) == 0) return(list())

  # Filter: keep only references cited more than once
  if (isTRUE(only_multiple)) {
    ref_freq <- table(all_refs)
    all_ref_ids <- names(ref_freq[ref_freq > 1])
  } else {
    all_ref_ids <- unique(all_refs)
  }

  if (length(all_ref_ids) == 0) return(list())

  # Batch fetch referenced works (50 per batch to stay within URL limits)
  batch_size <- 50
  n_batches <- ceiling(length(all_ref_ids) / batch_size)
  ref_lookup <- list()

  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, length(all_ref_ids))
    batch_ids <- all_ref_ids[start_idx:end_idx]

    pct <- round(i / n_batches * 100)

    # Update progress bar if available
    if (!is.null(progress_bar_id)) {
      shinyjs::runjs(sprintf(
        "$('#%s').css('width', '%d%%');", progress_bar_id, pct
      ))
    }
    if (!is.null(progress_pct_id)) {
      shinyjs::html(progress_pct_id, sprintf(
        "Batch %d/%d (%d%%)", i, n_batches, pct
      ))
    } else if (!is.null(progress_id)) {
      # Fallback: text-only progress
      shinyjs::html(
        progress_id,
        sprintf(
          "Resolving references... batch %d/%d (%s unique refs)",
          i, n_batches, format(length(all_ref_ids), big.mark = ",")
        )
      )
    }

    tryCatch({
      # Strip URL prefix for the filter
      short_ids <- gsub("https://openalex.org/", "", batch_ids)
      result <- openalexR::oa_fetch(
        entity = "works",
        openalex_id = paste(short_ids, collapse = "|"),
        output = "list",
        verbose = FALSE
      )

      if (is.list(result)) {
        for (work in result) {
          if (!is.null(work$id)) {
            cr <- build_single_cr(work)
            if (!is.na(cr)) ref_lookup[[work$id]] <- cr
          }
        }
      }
    }, error = function(e) {
      # Skip failed batches silently
    })
  }

  return(ref_lookup)
}

# Build CR column for each paper given oa_data and ref_lookup
build_cr_column <- function(oa_data, ref_lookup) {
  vapply(oa_data, function(work) {
    ref_ids <- work$referenced_works
    if (is.null(ref_ids) || length(ref_ids) == 0) return(NA_character_)

    cr_strings <- unlist(lapply(ref_ids, function(rid) {
      if (rid %in% names(ref_lookup)) ref_lookup[[rid]] else NULL
    }))

    if (length(cr_strings) == 0) return(NA_character_)
    paste(cr_strings, collapse = "; ")
  }, character(1))
}


# ==============================================================================
# Server Component - WebOfScience-like Query Builder
# ==============================================================================

openAlexServer <- function(input, output, session, values) {
  # Reactive values to track query rows and toggle states
  queryRows <- reactiveVal(1)
  showDateRange <- reactiveVal(TRUE)
  showAdvanced <- reactiveVal(TRUE)
  downloadProgress <- reactiveVal(NULL)
  topicChoices <- reactiveVal(NULL)
  journalChoices <- reactiveVal(NULL)
  submittedQueryFilters <- reactiveVal(NULL) # keyword filters submitted via Search button

  # API Key Warning Banner
  output$oaApiKeyWarning <- renderUI({
    # React to changes in the API key value
    values$oaApiKey
    oa_apikey <- Sys.getenv("openalexR.apikey", unset = "")
    if (oa_apikey == "") {
      div(
        style = "margin-bottom: 15px; padding: 12px 15px; background-color: #fff3cd; border-radius: 5px; border-left: 4px solid #f39c12; display: flex; align-items: center;",
        icon("exclamation-triangle", style = "color: #f39c12; margin-right: 10px; font-size: 18px;"),
        tags$span(
          style = "color: #856404; font-size: 13px;",
          HTML(
            "<strong>No API key configured.</strong> Without an API key you are limited to 100 credits/day (testing only). Go to <strong>Settings</strong> to set your free OpenAlex API key (100,000 credits/day)."
          )
        )
      )
    }
  })

  # Disable Topic/Journal dropdowns at startup (no data yet)
  observeEvent(TRUE, {
    shinyjs::disable("oaTopic")
    shinyjs::disable("oaJournal")
    shinyjs::runjs("$('#oaTopic').closest('.form-group').css('opacity', '0.5');")
    shinyjs::runjs("$('#oaJournal').closest('.form-group').css('opacity', '0.5');")
  }, once = TRUE)

  # Toggle date range visibility
  observeEvent(input$oaToggleDateRange, {
    current <- showDateRange()
    showDateRange(!current)
    session$sendCustomMessage('oaToggleDateRange', !current)
  })

  # Toggle advanced filters visibility
  observeEvent(input$oaToggleAdvanced, {
    current <- showAdvanced()
    showAdvanced(!current)
    session$sendCustomMessage('oaToggleAdvanced', !current)
  })

  # Add new query row
  observeEvent(input$oaAddRow, {
    currentRows <- queryRows()
    newRowId <- currentRows + 1
    queryRows(newRowId)

    # Insert new row in UI (WITH operator column)
    insertUI(
      selector = "#query-builder-container",
      where = "beforeEnd",
      ui = div(
        id = paste0("query-row-", newRowId),
        class = "query-row",
        style = "margin-bottom: 10px;",
        fluidRow(
          column(
            width = 1,
            selectInput(
              paste0("oaOperator_", newRowId),
              NULL,
              choices = c("And" = "AND", "Or" = "OR", "Not" = "NOT"),
              selected = "AND",
              width = "100%"
            )
          ),
          column(
            width = 2,
            selectInput(
              paste0("oaField_", newRowId),
              NULL,
              choices = c(
                "All Fields" = "default",
                "Title" = "title",
                "Abstract" = "abstract",
                "Title and Abstract" = "title_abstract",
                "Author" = "author",
                "Concept" = "concept"
              ),
              selected = "default",
              width = "100%"
            )
          ),
          column(
            width = 8,
            textInput(
              paste0("oaQuery_", newRowId),
              NULL,
              value = "",
              placeholder = 'Example: "deep learning"',
              width = "100%"
            )
          ),
          column(
            width = 1,
            actionButton(
              paste0("oaRemoveRow_", newRowId),
              icon("minus-circle"),
              class = "btn-danger btn-sm",
              style = "margin-top: 0px;",
              title = "Remove row"
            )
          )
        )
      )
    )

    # Make remove button visible for first row if adding second row
    if (newRowId == 2) {
      runjs("$('#remove-button-1').css('visibility', 'visible');")
    }

    # Add observer for the remove button of the new row
    local({
      rowId <- newRowId
      observeEvent(input[[paste0("oaRemoveRow_", rowId)]], {
        removeUI(selector = paste0("#query-row-", rowId))

        # Check if only one row remains
        remainingRows <- length(
          lapply(1:queryRows(), function(i) {
            input[[paste0("oaQuery_", i)]]
          })
        )

        if (remainingRows == 1) {
          runjs("$('#remove-button-1').css('visibility', 'hidden');")
        }
      })
    })
  })

  # Clear all query fields
  observeEvent(input$oaClearQuery, {
    # Reset all query inputs
    for (i in 1:queryRows()) {
      updateTextInput(session, paste0("oaQuery_", i), value = "")
      updateSelectInput(session, paste0("oaField_", i), selected = "default")
      if (i > 1) {
        updateSelectInput(session, paste0("oaOperator_", i), selected = "AND")
      }
    }

    # Reset filters
    updateNumericInput(session, "oaYearFrom", value = 2020)
    updateNumericInput(
      session,
      "oaYearTo",
      value = as.integer(format(Sys.Date(), "%Y"))
    )
    updateSelectizeInput(session, "oaDocType", selected = "")
    updateSelectizeInput(session, "oaLanguage", selected = "")
    updateNumericInput(session, "oaMaxRecords", value = 1000)
    updateSelectizeInput(session, "oaOpenAccess", selected = "")
    updateSelectizeInput(session, "oaSDG", selected = "")
    updateSelectizeInput(session, "oaTopic", choices = character(0), selected = character(0))
    updateSelectizeInput(session, "oaJournal", choices = character(0), selected = character(0))
    updateRadioButtons(session, "oaDropdownSort", selected = "count")
    updateCheckboxInput(session, "oaFetchRefs", value = FALSE)
    updateRadioButtons(session, "oaRefsFilter", selected = "multiple")
    topicChoices(NULL)
    journalChoices(NULL)
    topicData(NULL)
    journalData(NULL)
    submittedQueryFilters(NULL)
  })

  # Search button: submit keyword query and activate reactive filters
  observeEvent(input$oaSearchQuery, {
    # Collect all query rows
    queryList <- list()
    for (i in 1:queryRows()) {
      queryText <- input[[paste0("oaQuery_", i)]]
      if (!is.null(queryText) && queryText != "") {
        queryList[[length(queryList) + 1]] <- list(
          query = queryText,
          field = input[[paste0("oaField_", i)]],
          operator = if (i > 1) input[[paste0("oaOperator_", i)]] else NULL
        )
      }
    }

    if (length(queryList) == 0) {
      showNotification("Please enter at least one search term.", type = "warning", duration = 4)
      return()
    }

    search_filters <- build_query_filters(queryList)

    # Add date range filter
    if (showDateRange()) {
      if (!is.na(input$oaYearFrom)) {
        search_filters$from_publication_date <- paste0(input$oaYearFrom, "-01-01")
      }
      if (!is.na(input$oaYearTo)) {
        search_filters$to_publication_date <- paste0(input$oaYearTo, "-12-31")
      }
    }

    submittedQueryFilters(search_filters)
  })

  # Helper function to get field name for filter
  get_filter_name <- function(field) {
    switch(
      field,
      "title" = "title.search",
      "abstract" = "abstract.search",
      "title_abstract" = "title_and_abstract.search",
      "author" = "authorships.author.display_name.search",
      "concept" = "concepts.display_name.search",
      "default" = "default.search"
    )
  }

  # Helper function to build query filters
  build_query_filters <- function(queryList) {
    if (length(queryList) == 0) {
      return(list())
    }

    # Group queries by field
    field_queries <- list()

    for (i in seq_along(queryList)) {
      item <- queryList[[i]]
      field <- item$field
      filter_name <- get_filter_name(field)

      if (is.null(field_queries[[filter_name]])) {
        # First query for this field
        field_queries[[filter_name]] <- list(
          terms = c(item$query),
          operators = c()
        )
      } else {
        # Add operator and term
        field_queries[[filter_name]]$operators <- c(
          field_queries[[filter_name]]$operators,
          item$operator
        )
        field_queries[[filter_name]]$terms <- c(
          field_queries[[filter_name]]$terms,
          item$query
        )
      }
    }

    # Build the filter arguments
    search_filters <- list()

    for (filter_name in names(field_queries)) {
      field_data <- field_queries[[filter_name]]

      if (length(field_data$terms) == 1) {
        # Single term - just use it as is
        search_filters[[filter_name]] <- field_data$terms[1]
      } else {
        # Multiple terms - combine with operators
        query_parts <- c(field_data$terms[1])

        for (j in seq_along(field_data$operators)) {
          operator <- field_data$operators[j]
          term <- field_data$terms[j + 1]
          query_parts <- c(query_parts, operator, term)
        }

        # Combine into single string
        combined_query <- paste(query_parts, collapse = " ")
        search_filters[[filter_name]] <- combined_query
      }
    }

    return(search_filters)
  }

  # Reactive: base search filters (without Topic and Journal)
  # Keyword filters come from submittedQueryFilters (set by Search button).
  # Advanced filters (DocType, Language, etc.) are reactive after search is submitted.
  baseSearchFilters <- reactive({
    search_filters <- submittedQueryFilters()
    if (is.null(search_filters)) return(NULL)

    # Add advanced filters (except Topic and Journal) - these are reactive
    if (showAdvanced()) {
      if (!is.null(input$oaDocType) && length(input$oaDocType) > 0) {
        doc_types <- input$oaDocType[input$oaDocType != ""]
        if (length(doc_types) > 0) search_filters$type <- doc_types
      }
      if (!is.null(input$oaLanguage) && length(input$oaLanguage) > 0) {
        languages <- input$oaLanguage[input$oaLanguage != ""]
        if (length(languages) > 0) search_filters$language <- languages
      }
      if (!is.null(input$oaOpenAccess) && length(input$oaOpenAccess) > 0) {
        oa_statuses <- input$oaOpenAccess[input$oaOpenAccess != ""]
        if (length(oa_statuses) > 0) search_filters$open_access.oa_status <- oa_statuses
      }
      if (!is.null(input$oaSDG) && length(input$oaSDG) > 0) {
        sdg_ids <- input$oaSDG[input$oaSDG != ""]
        if (length(sdg_ids) > 0) {
          search_filters$sustainable_development_goals.id <- paste0("https://metadata.un.org/sdg/", sdg_ids)
        }
      }
    }

    return(search_filters)
  })

  # Debounced version of baseSearchFilters for group_by observer (2s delay)
  baseSearchFiltersDebounced <- debounce(baseSearchFilters, 2000)

  # Raw data from group_by API (data.frames with key, key_display_name, count)
  topicData <- reactiveVal(NULL)
  journalData <- reactiveVal(NULL)

  # Helper: build sorted named vector from group_by result
  sort_groupby_choices <- function(data, sort_order) {
    if (is.null(data) || nrow(data) == 0) return(character(0))
    if (sort_order == "alpha") {
      data <- data[order(data$key_display_name), ]
    } else {
      # Sort descending by count (ensure numeric comparison)
      data <- data[order(as.numeric(data$count), decreasing = TRUE), ]
    }
    setNames(data$key, paste0(data$key_display_name, " (", data$count, ")"))
  }

  # Observer: fetch Topic and Journal raw data via group_by
  observe({
    base_filters <- baseSearchFiltersDebounced()

    if (is.null(base_filters)) {
      topicData(NULL)
      journalData(NULL)
      shinyjs::disable("oaTopic")
      shinyjs::disable("oaJournal")
      shinyjs::runjs("$('#oaTopic').closest('.form-group').css('opacity', '0.5');")
      shinyjs::runjs("$('#oaJournal').closest('.form-group').css('opacity', '0.5');")
      return()
    }

    # Grey out and disable while loading
    shinyjs::disable("oaTopic")
    shinyjs::disable("oaJournal")
    shinyjs::runjs("$('#oaTopic').closest('.form-group').css('opacity', '0.5');")
    shinyjs::runjs("$('#oaJournal').closest('.form-group').css('opacity', '0.5');")

    tryCatch({
      topic_result <- do.call(
        openalexR::oa_fetch,
        c(list(entity = "works", group_by = "topics.id", verbose = FALSE), base_filters)
      )
      if (!is.null(topic_result) && nrow(topic_result) > 0) {
        topicData(topic_result)
      } else {
        topicData(NULL)
      }
    }, error = function(e) {
      topicData(NULL)
    })

    tryCatch({
      journal_result <- do.call(
        openalexR::oa_fetch,
        c(list(entity = "works", group_by = "primary_location.source.id", verbose = FALSE), base_filters)
      )
      if (!is.null(journal_result) && nrow(journal_result) > 0) {
        journalData(journal_result)
      } else {
        journalData(NULL)
      }
    }, error = function(e) {
      journalData(NULL)
    })

    # Re-enable after loading completes
    shinyjs::enable("oaTopic")
    shinyjs::enable("oaJournal")
    shinyjs::runjs("$('#oaTopic').closest('.form-group').css('opacity', '1');")
    shinyjs::runjs("$('#oaJournal').closest('.form-group').css('opacity', '1');")
  })

  # Observer: update dropdown choices when raw data or sort order changes
  observe({
    td <- topicData()
    jd <- journalData()
    sort_order <- input$oaDropdownSort

    current_topics <- isolate(input$oaTopic)
    current_journals <- isolate(input$oaJournal)

    tc <- sort_groupby_choices(td, sort_order)
    if (length(tc) > 0) {
      topicChoices(tc)
      valid_topics <- current_topics[current_topics %in% tc]
      updateSelectizeInput(session, "oaTopic", choices = tc, selected = valid_topics)
    } else {
      topicChoices(NULL)
      updateSelectizeInput(session, "oaTopic", choices = character(0), selected = character(0))
    }

    jc <- sort_groupby_choices(jd, sort_order)
    if (length(jc) > 0) {
      journalChoices(jc)
      valid_journals <- current_journals[current_journals %in% jc]
      updateSelectizeInput(session, "oaJournal", choices = jc, selected = valid_journals)
    } else {
      journalChoices(NULL)
      updateSelectizeInput(session, "oaJournal", choices = character(0), selected = character(0))
    }
  })

  # Reactive: full search filters (base + Topic + Journal)
  fullSearchFilters <- reactive({
    base_filters <- baseSearchFilters()
    if (is.null(base_filters)) return(NULL)

    search_filters <- base_filters

    if (showAdvanced()) {
      if (!is.null(input$oaTopic) && length(input$oaTopic) > 0) {
        topics <- input$oaTopic[input$oaTopic != ""]
        if (length(topics) > 0) search_filters$topics.id <- topics
      }
      if (!is.null(input$oaJournal) && length(input$oaJournal) > 0) {
        journals <- input$oaJournal[input$oaJournal != ""]
        if (length(journals) > 0) search_filters$primary_location.source.id <- journals
      }
    }

    return(search_filters)
  })

  # Debounced version of fullSearchFilters for count query (1s delay)
  fullSearchFiltersDebounced <- debounce(fullSearchFilters, 1000)

  # Reactive query count - updates automatically when query changes
  output$oaQueryCountInfo <- renderUI({
    search_filters <- fullSearchFiltersDebounced()

    # If no query, show a hint
    if (is.null(search_filters)) {
      return(
        div(
          style = "display: inline-block; padding: 5px 10px; color: #6c757d; font-size: 13px; font-style: italic;",
          icon("info-circle"),
          " Enter search terms to see record count"
        )
      )
    }

    tryCatch(
      {
        # Get the count of available records
        count_result <- do.call(
          openalexR::oa_fetch,
          c(
            list(
              entity = "works",
              count_only = TRUE,
              verbose = FALSE
            ),
            search_filters
          )
        )

        total_available <- as.integer(count_result$count)

        div(
          style = "display: inline-block; padding: 5px 10px; background-color: #d4edda; color: #155724; border-radius: 4px; font-size: 13px; font-weight: 500;",
          icon("check-circle"),
          sprintf(" Found %s records", format(total_available, big.mark = ","))
        )
      },
      error = function(e) {
        div(
          style = "display: inline-block; padding: 5px 10px; background-color: #f8d7da; color: #721c24; border-radius: 4px; font-size: 13px;",
          icon("exclamation-circle"),
          " Error: ",
          e$message
        )
      }
    )
  })

  # Download data from OpenAlex
  observeEvent(input$oaFetchData, {
    # Check if API key or polite email is configured
    oa_email <- Sys.getenv("openalexR.mailto", unset = "")
    oa_apikey <- Sys.getenv("openalexR.apikey", unset = "")
    if (oa_email == "" && oa_apikey == "") {
      showModal(modalDialog(
        title = "OpenAlex API Not Configured",
        div(
          style = "padding: 10px;",
          p(
            icon("exclamation-triangle", style = "color: #f39c12; margin-right: 8px;"),
            tags$strong("No API key or email configured for OpenAlex API.")
          ),
          p("Without an API key, you are limited to 100 credits/day (testing only). A free API key provides 100,000 credits/day."),
          p("Please go to ", tags$strong("Settings"), " and set your OpenAlex API key or email address."),
          p(
            style = "color: #666; font-size: 12px; margin-top: 15px;",
            "You can still proceed, but we strongly recommend setting an API key for better performance."
          )
        ),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("oaProceedWithoutEmail", "Proceed anyway", class = "btn-warning")
        )
      ))
      return()
    }

    # Run the actual search
    oaRunSearch(search_filters = fullSearchFilters())
  })

  # Allow proceeding without email after warning
  observeEvent(input$oaProceedWithoutEmail, {
    removeModal()
    oaRunSearch(search_filters = fullSearchFilters())
  })

  # Extracted search logic into a function
  oaRunSearch <- function(search_filters) {
    # Validate that at least one query exists
    if (is.null(search_filters)) {
      showModal(modalDialog(
        title = "Error",
        "Please enter at least one search term.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }

    # Show progress modal with dynamic content
    showModal(modalDialog(
      title = "Downloading from OpenAlex",
      div(
        div(
          style = "text-align: center; margin: 20px 0;",
          icon("spinner", class = "fa-spin fa-3x")
        ),
        div(
          id = "download-progress-text",
          style = "text-align: center; font-size: 14px;",
          "Initializing query..."
        )
      ),
      footer = NULL,
      easyClose = FALSE
    ))

    tryCatch(
      {
        # Calculate pages needed
        max_records <- min(input$oaMaxRecords, 10000)
        pages_needed <- ceiling(max_records / 200)

        print("Search filters:")
        print(search_filters)

        # First, get the count of available records
        count_result <- do.call(
          openalexR::oa_fetch,
          c(
            list(
              entity = "works",
              count_only = TRUE,
              verbose = FALSE
            ),
            search_filters
          )
        )

        total_available <- as.integer(count_result$count)
        records_to_download <- min(total_available, max_records)

        # Update progress modal with count information
        shinyjs::html(
          "download-progress-text",
          sprintf(
            "Found %s records. Downloading %s records...",
            format(total_available, big.mark = ","),
            format(records_to_download, big.mark = ",")
          )
        )

        # Fetch data from OpenAlex as list
        oa_data <- do.call(
          openalexR::oa_fetch,
          c(
            list(
              entity = "works",
              output = "list",
              abstract = TRUE,
              per_page = 200,
              pages = 1:pages_needed,
              verbose = TRUE
            ),
            search_filters
          )
        )

        # Update progress
        shinyjs::html(
          "download-progress-text",
          sprintf(
            "Converting %s records...",
            format(length(oa_data), big.mark = ",")
          )
        )

        # Create temporary directory if it doesn't exist
        temp_dir <- getWD()
        temp_file <- file.path(
          temp_dir,
          paste0("openalex_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rdata")
        )

        # Save the list as .rdata file
        save(oa_data, file = temp_file)

        # Convert using bibliometrix convert2df function
        M <- convert2df(
          file = temp_file,
          dbsource = "openalex_api",
          format = "api"
        )

        # Clean up temporary file
        unlink(temp_file)

        # Limit to max records if necessary
        if (nrow(M) > max_records) {
          M <- M[1:max_records, ]
          oa_data <- oa_data[1:max_records]
        }

        # Resolve cited references if requested
        if (isTRUE(input$oaFetchRefs)) {
          only_multiple <- isTRUE(input$oaRefsFilter == "multiple")
          all_refs <- unlist(lapply(oa_data, function(w) w$referenced_works))
          all_refs <- all_refs[!is.na(all_refs) & all_refs != ""]
          n_total <- length(unique(all_refs))
          if (only_multiple) {
            ref_freq <- table(all_refs)
            n_to_resolve <- sum(ref_freq > 1)
          } else {
            n_to_resolve <- n_total
          }

          shinyjs::html(
            "download-progress-text",
            paste0(
              sprintf("Resolving %s of %s unique cited references...",
                      format(n_to_resolve, big.mark = ","),
                      format(n_total, big.mark = ",")),
              '<div style="margin-top: 10px; background-color: #e9ecef; border-radius: 4px; height: 20px; width: 100%;">',
              '<div id="download-progress-bar" style="background-color: #007bff; height: 100%; border-radius: 4px; width: 0%; transition: width 0.3s ease;"></div>',
              '</div>',
              '<div id="download-progress-pct" style="text-align: center; margin-top: 5px; font-size: 12px; color: #666;">0%</div>'
            )
          )

          ref_lookup <- resolve_openalex_references(oa_data,
            progress_id = "download-progress-text",
            progress_bar_id = "download-progress-bar",
            progress_pct_id = "download-progress-pct",
            only_multiple = only_multiple)

          if (length(ref_lookup) > 0) {
            shinyjs::html("download-progress-text", "Building citation strings...")
            cr_col <- build_cr_column(oa_data[1:nrow(M)], ref_lookup)
            M$CRids <- M$CR
            M$CR <- cr_col
          }
        }

        # Close progress modal
        removeModal()

        # Collapse query panel after successful download
        session$sendCustomMessage('oaCollapseQueryPanel', TRUE)

        # Show success message
        showModal(modalDialog(
          title = "Success",
          div(
            p(
              sprintf(
                "Successfully downloaded %s records from OpenAlex.",
                format(nrow(M), big.mark = ",")
              )
            ),
            if (total_available > max_records) {
              p(
                style = "color: #856404; background-color: #fff3cd; padding: 10px; border-radius: 5px;",
                sprintf(
                  "Note: %s total records found, but limited to %s records.",
                  format(total_available, big.mark = ","),
                  format(max_records, big.mark = ",")
                )
              )
            }
          ),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))

        values = initial(values)
        M <- M %>% mergeKeywords(force = F)
        # Update values
        values$data_source <- "OpenAlex"
        values$M <- M
        values$Morig = M
        values$SCdf <- wcTable(M)
        values$COdf <- countryTable(M)
        values$Histfield = "NA"
        values$results = list("NA")
        if (ncol(values$M) > 1) {
          values$rest_sidebar <- TRUE
        }
        if (ncol(values$M) > 1) {
          values$loadMenu <- "openalex_api"
          showModal(missingModalAPI(session))
        }
      },
      error = function(e) {
        removeModal()
        showModal(modalDialog(
          title = "Error",
          paste("An error occurred:", e$message),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
    )
  }

  # Render data table
  output$oaDataTable <- renderUI({
    req(values$M)
    req(values$data_source == "OpenAlex")

    MData = as.data.frame(apply(values$M, 2, function(x) {
      substring(x, 1, 150)
    }))
    MData$DOI <-
      paste0(
        '<a href=\"https://doi.org/',
        MData$DI,
        '\" target=\"_blank\">',
        MData$DI,
        '</a>'
      )
    nome = c("DOI", names(MData)[-length(names(MData))])
    MData = MData[nome]
    renderBibliobox(
      MData,
      nrow = 3,
      filename = "Table",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = TRUE,
      size = '70%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = FALSE,
      escape = FALSE,
      selection = FALSE,
      scrollX = TRUE
    )
  })

  # Check if data is available for conditional panel
  output$oaDataAvailable <- reactive({
    !is.null(values$M) && values$data_source == "OpenAlex"
  })
  outputOptions(output, "oaDataAvailable", suspendWhenHidden = FALSE)

  # Display progress information
  output$oaProgressInfo <- renderUI({
    if (!is.null(values$M) && values$data_source == "OpenAlex") {
      div(
        class = "alert alert-success",
        icon("check-circle"),
        strong(" Data loaded: "),
        sprintf(
          "%s documents from OpenAlex",
          format(nrow(values$M), big.mark = ",")
        )
      )
    }
  })

  # Download handler for Excel
  output$oaDownloadExcel <- downloadHandler(
    filename = function() {
      paste0("openalex_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(values$M)
      req(values$data_source == "OpenAlex")

      # Write data to Excel file
      openxlsx::write.xlsx(values$M, file = file)
    }
  )

  # Download handler for RData
  output$oaDownloadRData <- downloadHandler(
    filename = function() {
      paste0("openalex_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".RData")
    },
    content = function(file) {
      req(values$M)
      req(values$data_source == "OpenAlex")

      # Save data as RData file
      M <- values$M
      save(M, file = file)
    }
  )

  missingModalAPI <- function(session) {
    ns <- session$ns
    modalDialog(
      uiOutput("missingTitle"),
      uiOutput(ns("missingDataTable")),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton(
          label = "Advice",
          inputId = "missingMessage",
          icon = icon("exclamation-sign", lib = "glyphicon")
        ),
        actionButton(
          label = "Report",
          inputId = "missingReport",
          icon = icon("plus", lib = "glyphicon")
        ),
        actionButton(
          label = "Save",
          inputId = "missingDataTable",
          icon = icon("camera", lib = "glyphicon")
        ),
        modalButton(label = "Close")
      ),
    )
  }
}
