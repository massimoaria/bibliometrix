# ### packages for biblishiny()
libraries <- function() {
  all_ok <- TRUE

  parse_pkg <- function(pkg_str) {
    # Estrae nome e versione minima, se specificata
    matches <- regmatches(
      pkg_str,
      regexec("^([a-zA-Z0-9\\.]+)(?: \\(>= ([0-9\\.]+)\\))?$", pkg_str)
    )[[1]]
    if (length(matches) >= 2) {
      list(
        name = matches[2],
        min_version = ifelse(length(matches) == 3, matches[3], NA)
      )
    } else {
      list(name = pkg_str, min_version = NA)
    }
  }

  safe_install <- function(pkg_str) {
    pkg_info <- parse_pkg(pkg_str)
    pkg <- pkg_info$name
    min_ver <- pkg_info$min_version

    need_install <- FALSE

    if (pkg %in% rownames(installed.packages())) {
      if (!is.na(min_ver)) {
        installed_ver <- as.character(packageVersion(pkg))
        if (compareVersion(installed_ver, min_ver) < 0) {
          message(sprintf(
            "The installed version of '%s' (%s) is lower than the required (%s).",
            pkg,
            installed_ver,
            min_ver
          ))
          need_install <- TRUE
        }
      }
    } else {
      need_install <- TRUE
    }

    if (need_install) {
      # install.packages() signals most failures as a warning and returns
      # normally, so a package that could not be installed used to fall through
      # to require() and be reported as a generic "missing package". Capture the
      # reason here and let the caller show it.
      msg <- tryCatch(
        {
          install.packages(pkg)
          NULL
        },
        error = function(e) conditionMessage(e),
        warning = function(w) conditionMessage(w)
      )
      if (!is.null(msg)) {
        message(sprintf("Could not install '%s': %s", pkg, msg))
      }
    }

    return(require(pkg, character.only = TRUE, quietly = TRUE))
  }

  pkgs <- c(
    "httr2",
    "base64enc",
    "bibliometrix",
    "zip",
    "shiny",
    "igraph",
    "stringr",
    "contentanalysis",
    "ggplot2",
    "wordcloud2",
    "ggmap",
    "maps",
    "pdftools (>= 3.6.0)",
    "tidytext",
    "visNetwork",
    "plotly",
    "fontawesome",
    "shinydashboardPlus",
    "shinydashboard",
    "shinyjs",
    "curl (>= 6.3.0)",
    "RCurl",
    "openxlsx",
    "shinyWidgets",
    "chromote",
    "pagedown",
    "Matrix",
    "dimensionsR",
    "pubmedR",
    "dplyr",
    "tidyr",
    "sparkline",
    "tidygraph",
    "ggraph",
    "future",
    "promises"
  )

  # The names of the packages that failed are what the user actually needs: the
  # old return value was a single logical, so Biblioshiny could only say that
  # "some packages are missing" and blame the internet connection, which sent
  # users looking for a connectivity problem they did not have.
  pkg_names <- vapply(pkgs, function(p) parse_pkg(p)$name, character(1), USE.NAMES = FALSE)

  suppressPackageStartupMessages({
    results <- vapply(pkgs, safe_install, logical(1), USE.NAMES = FALSE)
    all_ok <- all(results)
  })

  return(list(
    ok = all_ok,
    # kept with the version requirement, for the human-readable list ...
    missing = pkgs[!results],
    # ... and bare, so the suggested install.packages() call can be pasted as is
    missing_names = pkg_names[!results]
  ))
}

messageItem2 <- function(
  from,
  message,
  icon = shiny::icon("user"),
  time = NULL,
  href = NULL,
  inputId = NULL
) {
  if (is.null(href)) {
    href <- "#"
  }
  shiny::tags$li(shiny::a(
    id = inputId,
    class = if (!is.null(inputId)) {
      "action-button"
    },
    href = href,
    target = "_blank",
    icon,
    shiny::h4(
      from,
      if (!is.null(time)) {
        shiny::tags$small(shiny::icon("clock-o"), time)
      }
    ),
    shiny::p(message)
  ))
}
