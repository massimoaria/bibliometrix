# ### packages for biblishiny()
libraries <- function() {
  is_windows <- tolower(Sys.info()[["sysname"]]) == "windows"
  if (!is_windows & !require(pak, quietly = TRUE)) {
    install.packages("pak")
    if (!require(pak, quietly = TRUE)) return(FALSE)
  }
  
  all_ok <- TRUE
  
  parse_pkg <- function(pkg_str) {
    # Estrae nome e versione minima, se specificata
    matches <- regmatches(pkg_str, regexec("^([a-zA-Z0-9\\.]+)(?: \\(>= ([0-9\\.]+)\\))?$", pkg_str))[[1]]
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
          message(sprintf("La versione installata di '%s' (%s) è inferiore alla richiesta (%s).", 
                          pkg, installed_ver, min_ver))
          need_install <- TRUE
        }
      }
    } else {
      need_install <- TRUE
    }
    
    if (need_install) {
      if (is_windows) {
        install.packages(pkg)
      } else {
        pak::pkg_install(pkg, ask= FALSE)
      }
    }
    
    return(require(pkg, character.only = TRUE, quietly = TRUE))
  }
  
  pkgs <- c(
    "httr2", "base64enc", "bibliometrix", "zip", "shiny", "igraph", "DT", 
    "ggplot2", "wordcloud2", "ggmap", "maps", 
    "visNetwork", "plotly", "fontawesome", "shinydashboardPlus", 
    "shinydashboard", "shinyjs", "curl (>= 6.3.0)","RCurl", "openxlsx", "shinyWidgets", 
    "chromote", "pagedown", "Matrix", "dimensionsR", "pubmedR", 
    "dplyr", "tidyr", "sparkline", "tidygraph", "ggraph"
  )
  
  suppressPackageStartupMessages({
    results <- vapply(pkgs, safe_install, logical(1))
    all_ok <- all(results)
  })
  
  return(all_ok)
}


# libraries <- function() {
#   is_windows <- tolower(Sys.info()[["sysname"]]) == "windows"
#   if (!is_windows & !require(pak, quietly = TRUE)) {
#     install.packages("pak")
#     if (!require(pak, quietly = TRUE)) return(FALSE)
#   }
#   
#   all_ok <- TRUE
#   
#   safe_install <- function(pkg) {
#     success <- require(pkg, character.only = TRUE, quietly = TRUE)
#     if (!success) {
#       if (is_windows) {
#         install.packages(pkg)
#       } else {
#         
#         pak::pkg_install(pkg)
#       }
#       success <- require(pkg, character.only = TRUE, quietly = TRUE)
#     }
#     return(success)
#   }
# 
#   pkgs <- c(
#     "httr2", "base64enc", "bibliometrix", "zip", "shiny", "igraph", "DT", 
#     "ggplot2", "wordcloud2", "ggmap", "maps", 
#     "visNetwork", "plotly", "fontawesome", "shinydashboardPlus", 
#     "shinydashboard", "shinyjs", "curl (>= 6.3.0)","RCurl", "openxlsx", "shinyWidgets", 
#     "chromote", "pagedown", "Matrix", "dimensionsR", "pubmedR", 
#     "dplyr", "tidyr", "sparkline", "tidygraph", "ggraph"
#   )
# 
#   suppressPackageStartupMessages({
#     results <- vapply(pkgs, safe_install, logical(1))
#     all_ok <- all(results)
#   })
#   
#   return(all_ok)
# }


messageItem2 <- function(from, message, icon = shiny::icon("user"), time = NULL,
                         href = NULL, inputId = NULL) {
  if (is.null(href)) {
    href <- "#"
  }
  shiny::tags$li(shiny::a(id = inputId, class = if (!is.null(inputId)) {
    "action-button"
  }, href = href, target = "_blank", icon, shiny::h4(from, if (!is.null(time)) {
    shiny::tags$small(shiny::icon("clock-o"), time)
  }), shiny::p(message)))
}
