### COMMON FUNCTIONS ####

# Number format abbreviated
format_abbreviated <- function(x) {
  if (is.na(x)) {
    return("--")
  }
  if (x >= 1e6) {
    return(paste0(format(round(x / 1e6, 2), nsmall = 2), "M"))
  } else if (x >= 1e3) {
    return(paste0(format(round(x / 1e3, 0), nsmall = 0), "K"))
  } else {
    return(as.character(x))
  }
}

# total package download
total_downloads <- function(
  pkg_name = "bibliometrix",
  from = "2016-01-01",
  to = Sys.Date()
) {
  # Function to get total downloads of a package from CRAN logs
  # Args:
  #   pkg_name: Name of the package as a string
  # Returns:
  #   Total number of downloads as an integer

  if (!is_Online()) {
    return(NA)
  }

  #today <- Sys.Date()
  if (!is.character(pkg_name) || length(pkg_name) != 1) {
    stop("pkg_name must be a single string.")
  }

  url <- paste0(
    "https://cranlogs.r-pkg.org/downloads/total/",
    from,
    ":",
    to,
    "/",
    pkg_name
  )

  # if (!is_Online(timeout = 1, url)) {
  #   return(NA)
  # }
  if (!check_online(host = url, timeout = 1, method = "http")) {
    return(NA)
  }

  json_text <- tryCatch(
    {
      readLines(url, warn = FALSE)
    },
    error = function(e) {
      return(NA)
    }
  )

  # Se già nel tryCatch è tornato "NA", esci subito
  if (identical(json_text, "NA")) {
    return(NA)
  }

  # Extract the number manually (not robust)
  txt <- unlist(strsplit(json_text, ","))
  txt <- txt[grepl("downloads", txt)]

  if (length(txt) == 0) {
    return(NA)
  }

  downloads <- gsub("[^0-9]", "", txt)

  return(as.integer(downloads))
}

# FILTER FUNCTIONS ----
read_journal_ranking <- function(file_path) {
  ext <- tools::file_ext(file_path)

  suppressMessages(
    journals <- switch(
      tolower(ext),
      "csv" = read.csv(file_path, header = TRUE, stringsAsFactors = FALSE),
      "xlsx" = {
        readxl::read_excel(file_path, col_names = TRUE)
      },
      stop(
        "Unsupported file format. Please upload a .csv, .txt, or .xlsx file."
      )
    )
  )
  journals <- journals %>% select(1, 2)
  # journals <- journals[!is.na(journals)]
  # journals <- toupper(trimws(journals))
  names(journals) <- c("SO", "Ranking")
  journals <- journals %>%
    mutate(SO = toupper(trimws(SO)))
  return(journals)
}

read_journal_list <- function(file_path) {
  ext <- tools::file_ext(file_path)

  suppressMessages(
    journals <- switch(
      tolower(ext),
      "csv" = read.csv(file_path, header = FALSE, stringsAsFactors = FALSE)[[
        1
      ]],
      "txt" = readLines(file_path, warn = FALSE),
      "xlsx" = {
        readxl::read_excel(file_path, col_names = FALSE)[[1]]
      },
      stop(
        "Unsupported file format. Please upload a .csv, .txt, or .xlsx file."
      )
    )
  )

  journals <- journals[!is.na(journals)]
  journals <- toupper(trimws(journals))
  return(journals)
}

wcTable <- function(M) {
  # Function to extract Science Category (WC) information from metadata
  if ("WC" %in% names(M)) {
    WC <- strsplit(M$WC, ";")
    df <- data.frame(
      SR = rep(M$SR, lengths(WC)),
      WC = unlist(WC),
      stringsAsFactors = FALSE
    )

    df$WC <- trimws(df$WC) # Remove leading and trailing whitespace
  } else {
    df <- data.frame(SR = M$SR, WC = "N.A.", stringsAsFactors = FALSE)
  }

  return(df)
}

countryTable <- function(M) {
  data("countries", envir = environment())
  # Function to extract country information from metadata
  if (!("AU_CO" %in% names(M))) {
    M <- metaTagExtraction(M, "AU_CO")
  }

  CO <- strsplit(M$AU_CO, ";")
  df <- data.frame(
    SR = rep(M$SR, lengths(CO)),
    CO = trimws(unlist(CO)),
    stringsAsFactors = FALSE
  )

  df$CO <- gsub("[[:digit:]]", "", df$CO)
  df$CO <- gsub(".", "", df$CO, fixed = TRUE)
  df$CO <- gsub(";;", ";", df$CO, fixed = TRUE)
  df$CO <- gsub("UNITED STATES", "USA", df$CO)
  df$CO <- gsub("RUSSIAN FEDERATION", "RUSSIA", df$CO)
  df$CO <- gsub("TAIWAN", "CHINA", df$CO)
  df$CO <- gsub("ENGLAND", "UNITED KINGDOM", df$CO)
  df$CO <- gsub("SCOTLAND", "UNITED KINGDOM", df$CO)
  df$CO <- gsub("WALES", "UNITED KINGDOM", df$CO)
  df$CO <- gsub("NORTH IRELAND", "UNITED KINGDOM", df$CO)
  df$CO <- gsub("UK", "UNITED KINGDOM", df$CO)
  #df$CO <- gsub("KOREA", "SOUTH KOREA", df$CO)

  df <- df %>%
    left_join(
      countries %>% select(countries, continent),
      by = c("CO" = "countries")
    ) %>%
    mutate(CO = ifelse(is.na(CO), "Unknown", CO)) %>%
    mutate(continent = ifelse(is.na(continent), "Unknown", continent)) %>%
    mutate(CO = ifelse(CO == "UNKNOWN", "Unknown", CO))
}

# LOAD FUNCTIONS -----

formatDB <- function(obj) {
  ext <- sub(".*\\.", "", obj[1])
  switch(
    ext,
    txt = {
      format <- "plaintext"
    },
    csv = {
      format <- "csv"
    },
    bib = {
      format <- "bibtex"
    },
    ciw = {
      format <- "endnote"
    },
    xlsx = {
      format <- "excel"
    }
  )
  return(format)
}

## smart_load function ----
smart_load <- function(file) {
  var <- load(file)
  n <- length(var)
  if (!"M" %in% var) {
    if (n == 1) {
      eval(parse(text = paste0("M <- ", var)))
    } else {
      stop("I could not find bibliometrixDB object in your data file: ", file)
    }
  }
  rm(list = var[var != "M"])
  if (("M" %in% ls()) & inherits(M, "bibliometrixDB")) {
    return(M)
  } else {
    stop(
      "Please make sure your RData/Rda file contains a bibliometrixDB object (M)."
    )
  }
}


## merge collections ----
merge_files <- function(files) {
  ## load xlsx or rdata bibliometrix files
  if ("datapath" %in% names(files)) {
    file <- files$datapath
    ext <- unlist(lapply(file, getFileNameExtension))
  }

  Mfile <- list()
  n <- 0
  for (i in 1:length(file)) {
    extF <- ext[i]
    filename <- file[i]

    switch(
      tolower(extF),
      xlsx = {
        Mfile[[i]] <- readxl::read_excel(filename, col_types = "text") %>%
          as.data.frame()
        Mfile[[i]]$PY <- as.numeric(Mfile[[i]]$PY)
        Mfile[[i]]$TC <- as.numeric(Mfile[[i]]$TC)
      },
      rdata = {
        Mfile[[i]] <- smart_load(filename)
      }
    )
    n <- n + nrow(Mfile[[i]])
  }

  # merge bibliometrix files
  M <- mergeDbSources(Mfile, remove.duplicated = T)

  # save original size as attribute
  attr(M, "nMerge") <- n

  return(M)
}

## dynamic watch emoji icons ---
watchEmoji <- function(i) {
  emoji <- c(
    "🕐",
    "🕑",
    "🕒",
    "🕓",
    "🕔",
    "🕕",
    "🕖",
    "🕗",
    "🕘",
    "🕙",
    "🕚",
    "🕛"
  )
  # i is a positive int number, reduce it to an int from 1 to 12
  multiple <- floor(i / 12)
  if (multiple > 0) {
    i <- i %% (12 * multiple)
    if (i == 0) {
      i <- 12
    }
  }
  emoji[i]
}

## RESET MODAL DIALOG INPUTS
resetModalButtons <- function(session) {
  session$sendCustomMessage("button_id", "null")
  session$sendCustomMessage("button_id2", "null")
  # session$sendCustomMessage("click", "null")
  # session$sendCustomMessage("click-dend", "null")
  # runjs("Shiny.setInputValue('plotly_click-A', null);")
  return(session)
}


# DATA TABLE FORMAT ----
DTformat <- function(
  df,
  nrow = 10,
  filename = "Table",
  pagelength = TRUE,
  left = NULL,
  right = NULL,
  numeric = NULL,
  dom = TRUE,
  size = "85%",
  filter = "top",
  columnShort = NULL,
  columnSmall = NULL,
  round = 2,
  title = "",
  button = FALSE,
  escape = FALSE,
  selection = FALSE,
  scrollX = FALSE,
  scrollY = FALSE,
  summary = FALSE
) {
  if ("text" %in% names(df)) {
    df <- df %>%
      mutate(text = gsub("<|>", "", text))
  }

  if (length(columnShort) > 0) {
    columnDefs <- list(
      list(
        className = "dt-center",
        targets = 0:(length(names(df)) - 1)
      ),
      list(
        targets = columnShort - 1,
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 500 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 500) + '...</span>' : data;",
          "}"
        )
      )
    )
  } else {
    columnDefs <- list(list(
      className = "dt-center",
      targets = 0:(length(names(df)) - 1)
    ))
  }

  initComplete <- NULL
  # Summary Button
  if (summary == "documents" & "Paper" %in% names(df)) {
    df <- df %>%
      mutate(
        Summary = paste0(
          '<div style="display: flex; justify-content: center; align-items: center; height: 100%; width: 100%;">',
          '<button id="custom_btn" style="',
          'width: 24px; height: 24px; ',
          'border-radius: 50%; ',
          'border: none; ',
          'background: linear-gradient(135deg, #4285f4 0%, #1976d2 100%); ',
          'color: white; ',
          'cursor: pointer; ',
          'display: flex; ',
          'align-items: center; ',
          'justify-content: center; ',
          'box-shadow: 0 2px 4px rgba(0,0,0,0.2); ',
          'transition: all 0.3s ease; ',
          '" ',
          'onmouseover="this.style.transform=\'scale(1.1)\'; this.style.boxShadow=\'0 4px 8px rgba(0,0,0,0.3)\';" ',
          'onmouseout="this.style.transform=\'scale(1)\'; this.style.boxShadow=\'0 2px 4px rgba(0,0,0,0.2)\';" ',
          'onclick="Shiny.onInputChange(\'button_id\', \'',
          Paper,
          '\')">',
          '<i class="fas fa-search-plus" style="font-size: 14px;"></i>',
          '</button>',
          '</div>'
        )
      ) %>%
      select(Summary, everything())
  } else if (summary == "historiograph" & "Paper" %in% names(df)) {
    df <- df %>%
      mutate(
        Summary = paste0(
          '<div style="display: flex; justify-content: center; align-items: center; height: 100%; width: 100%;">',
          '<button id="custom_btn" style="',
          'width: 32px; height: 32px; ',
          'border-radius: 50%; ',
          'border: none; ',
          'background: linear-gradient(135deg, #4285f4 0%, #1976d2 100%); ',
          'color: white; ',
          'cursor: pointer; ',
          'display: flex; ',
          'align-items: center; ',
          'justify-content: center; ',
          'box-shadow: 0 2px 4px rgba(0,0,0,0.2); ',
          'transition: all 0.3s ease; ',
          '" ',
          'onmouseover="this.style.transform=\'scale(1.1)\'; this.style.boxShadow=\'0 4px 8px rgba(0,0,0,0.3)\';" ',
          'onmouseout="this.style.transform=\'scale(1)\'; this.style.boxShadow=\'0 2px 4px rgba(0,0,0,0.2)\';" ',
          'onclick="Shiny.onInputChange(\'button_id\', \'',
          SR,
          '\')">',
          '<i class="fas fa-search-plus" style="font-size: 14px;"></i>',
          '</button>',
          '</div>'
        )
      ) %>%
      select(Summary, everything()) %>%
      select(-SR)
  } else if (summary == "authors" & "Author" %in% names(df)) {
    df <- df %>%
      # mutate(Bio = paste0('<button id="custom_btn2" onclick="Shiny.onInputChange(\'button_id2\', \'', Author, '\')">▶️</button>')) %>%
      # select(Bio, everything()) %>%
      mutate(
        Author = paste0(
          '<span class="author-link" onclick="show_author_modal(\'',
          gsub("'", "\\\\'", Author),
          '\')">',
          Author,
          '</span>'
        )
      )
    initComplete = JS(
      "function(settings, json) {",
      "  window.show_author_modal = function(author) {",
      "    Shiny.setInputValue('selected_author', author, {priority: 'event'});",
      "  };",
      "}"
    )
    escape = FALSE
  }

  if (isTRUE(button)) {
    if (isTRUE(pagelength)) {
      buttons <- list(
        list(extend = "pageLength"),
        list(
          extend = "excel",
          filename = paste0(filename, "_bibliometrix_", Sys.Date()),
          title = " ",
          header = TRUE,
          exportOptions = list(
            modifier = list(page = "all")
          )
        )
      )
    } else {
      buttons <- list(
        list(
          extend = "excel",
          filename = paste0(filename, "_bibliometrix_", Sys.Date()),
          title = " ",
          header = TRUE,
          exportOptions = list(
            modifier = list(page = "all")
          )
        )
      )
    }
  } else {
    buttons <- list(list(extend = "pageLength"))
  }

  if (isTRUE(dom)) {
    dom <- "Brtip"
  } else if (dom == FALSE) {
    dom <- "Bftp"
  } else {
    dom <- "t"
  }

  if (nchar(title) > 0) {
    caption <- htmltools::tags$caption(
      style = "caption-side: top; text-align: center; color:black;  font-size:140% ;",
      title
    )
  } else {
    caption <- htmltools::tags$caption(
      style = "caption-side: top; text-align: center; color:black;  font-size:140% ;",
      ""
    )
  }

  if (isTRUE(selection)) {
    extensions <- c("Buttons", "Select", "ColReorder", "FixedHeader")
    buttons <- c(buttons, c("selectAll", "selectNone"))
    select <- list(style = "multiple", items = "row", selected = 1:nrow(df))
    # selection = list(mode = 'multiple', selected = 1:nrow(df), target = 'row')
  } else {
    extensions <- c("Buttons", "ColReorder", "FixedHeader")
    select <- NULL
    # selection = "none"
  }

  tab <- DT::datatable(
    df,
    escape = escape,
    rownames = FALSE,
    caption = caption,
    selection = "none",
    extensions = extensions,
    filter = filter,
    options = list(
      headerCallback = DT::JS(
        "function(thead) {",
        "  $(thead).css('font-size', '1em');",
        "}"
      ),
      initComplete = initComplete,
      colReorder = TRUE,
      fixedHeader = TRUE,
      pageLength = nrow,
      autoWidth = TRUE,
      scrollX = scrollX,
      scrollY = scrollY,
      dom = dom,
      buttons = buttons,
      select = select,
      lengthMenu = list(
        c(10, 25, 50, -1),
        c("10 rows", "25 rows", "50 rows", "Show all")
      ),
      columnDefs = columnDefs
    ),
    class = "cell-border compact stripe"
  ) %>%
    DT::formatStyle(
      names(df),
      backgroundColor = "white",
      textAlign = "center",
      fontSize = size
    )

  ## left aligning

  if (!is.null(left)) {
    tab <- tab %>%
      DT::formatStyle(
        names(df)[left],
        backgroundColor = "white",
        textAlign = "left",
        fontSize = size
      )
  }

  # right aligning
  if (!is.null(right)) {
    tab <- tab %>%
      DT::formatStyle(
        names(df)[right],
        backgroundColor = "white",
        textAlign = "right",
        fontSize = size
      )
  }

  # numeric round
  if (!is.null(numeric)) {
    tab <- tab %>%
      formatRound(names(df)[c(numeric)], digits = round)
  }

  tab
}


authorNameFormat <- function(M, format) {
  if (format == "AF" & "AF" %in% names(M)) {
    M <- M %>%
      rename(
        AU_IN = AU,
        AU = AF
      )
  }
  return(M)
}

split_text_numbers <- function(input_str, UT) {
  # Split the string into components based on "; "
  components <- unlist(strsplit(input_str, "; ", fixed = TRUE))

  # Initialize two vectors to store the separated parts
  texts <- character(length(components))
  numbers <- numeric(length(components))

  # Iterate through each component to separate text and numbers
  for (i in seq_along(components)) {
    # Extract the text using regex, matching everything up to " ("
    texts[i] <- gsub("\\s\\(.*$", "", components[i])

    # Extract the numbers using regex, matching digits inside parentheses
    numbers[i] <- as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", components[i]))
  }

  # Return a list with texts and numbers separated
  data.frame(Texts = texts, Numbers = numbers, UT = UT)
}


AuthorNameMerge <- function(M) {
  df_list <- list()
  for (i in 1:nrow(M)) {
    if (nchar(M$AU[i]) > 0) {
      df_list[[i]] <- split_text_numbers(M$AU[i], M$UT[i])
    }
  }

  df <- do.call(rbind, df_list)

  AU <- df %>%
    group_by(Numbers, Texts) %>%
    count() %>%
    group_by(Numbers) %>%
    arrange(desc(n)) %>%
    mutate(AU = Texts[1]) %>%
    select(-"n", -"Texts") %>%
    ungroup() %>%
    distinct()

  df <- df %>%
    left_join(AU, by = "Numbers") %>%
    group_by(UT) %>%
    summarize(
      AU = paste0(AU, collapse = ";"),
      AU_ID = paste0(Numbers, collapse = ";")
    )

  M <- M %>%
    rename(AU_original = AU) %>%
    left_join(df, by = "UT")
  return(M)
}

getFileNameExtension <- function(fn) {
  # remove a path
  splitted <- strsplit(x = fn, split = "/")[[1]]
  # or use .Platform$file.sep in stead of '/'
  fn <- splitted[length(splitted)]
  ext <- ""
  splitted <- strsplit(x = fn, split = "\\.")[[1]]
  l <- length(splitted)
  if (l > 1 && sum(splitted[1:(l - 1)] != "")) {
    ext <- splitted[l]
  }
  # the extention must be the suffix of a non-empty name
  ext
}

# Initial to upper case
firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


# string preview (stopwords)
strPreview <- function(string, sep = ",") {
  str1 <- unlist(strsplit(string, sep))
  str1 <- str1[1:min(c(length(str1), 5))]
  str1 <- paste(str1, collapse = sep)
  HTML(paste("<pre>", "File Preview: ", str1, "</pre>", sep = "<br/>"))
}

# string preview (synonyms)
strSynPreview <- function(string) {
  string <- string[1]
  str1 <- unlist(strsplit(string, ";"))
  str1 <- str1[1:min(c(length(str1), 5))]
  str1 <- paste(
    paste(str1[1], " <- ", collapse = ""),
    paste(str1[-1], collapse = ";"),
    collapse = ""
  )
  HTML(paste("<pre>", "File Preview: ", str1, "</pre>", sep = "<br/>"))
}


### LIFE CYCLE PLOTLY FUNCTION ----

#' Plot Life Cycle Analysis Results with ggplot2
#'
#' Creates ggplot2 plots from lifeCycle analysis results
#'
#' @param results Output from lifeCycle() function
#' @param plot_type Character, either "annual" or "cumulative" to specify which plot to generate
#'
#' @return A ggplot2 object
#'
#' @export
ggplotLifeCycle <- function(results, plot_type = c("annual", "cumulative")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }

  plot_type <- match.arg(plot_type)

  # Extract data
  complete_curve <- results$complete_curve
  observed_data <- results$data
  params <- results$parameters_real_years
  metrics <- results$metrics
  K <- params$K
  tm_year <- params$tm_year
  R2 <- metrics$R_squared

  if (plot_type == "annual") {
    # Plot 1: Annual Publications
    max_annual <- max(complete_curve$annual, na.rm = TRUE)

    p <- ggplot() +
      geom_line(
        data = complete_curve,
        aes(x = year, y = annual),
        color = "blue",
        linewidth = 1
      ) +
      geom_point(
        data = observed_data,
        aes(x = PY, y = n),
        color = "blue",
        size = 3
      ) +
      geom_vline(
        xintercept = tm_year,
        linetype = "dashed",
        color = "red",
        linewidth = 0.7
      ) +
      annotate(
        "text",
        x = tm_year,
        y = max_annual * 0.95,
        label = sprintf("Peak: %.1f", tm_year),
        hjust = -0.1,
        color = "red",
        size = 3.5
      ) +
      annotate(
        "text",
        x = max(complete_curve$year),
        y = max_annual * 1.05,
        label = sprintf("R² = %.3f", R2),
        hjust = 1,
        size = 4
      ) +
      labs(
        title = "Life Cycle - Annual Publications",
        x = "Year",
        y = "Publications (Annual)"
      ) +
      scale_y_continuous(limits = c(0, max_annual * 1.1)) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        panel.grid.minor = element_blank()
      )

    return(p)
  } else if (plot_type == "cumulative") {
    # Plot 2: Cumulative Publications
    reference_lines <- data.frame(
      y = c(K * 0.5, K * 0.9, K * 0.99),
      label = c("50%", "90%", "99%")
    )

    p <- ggplot() +
      geom_line(
        data = complete_curve,
        aes(x = year, y = cumulative),
        color = "darkgreen",
        linewidth = 1
      ) +
      geom_point(
        data = observed_data,
        aes(x = PY, y = cumulative),
        color = "darkgreen",
        size = 3
      ) +
      geom_hline(
        data = reference_lines,
        aes(yintercept = y),
        linetype = "dotted",
        color = "gray50",
        linewidth = 0.5
      ) +
      geom_text(
        data = reference_lines,
        aes(x = min(complete_curve$year), y = y, label = label),
        hjust = 0,
        vjust = -0.5,
        size = 3,
        color = "gray50"
      ) +
      labs(
        title = "Cumulative Growth Curve",
        x = "Year",
        y = "Cumulative Publications"
      ) +
      scale_y_continuous(limits = c(0, K * 1.05)) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        panel.grid.minor = element_blank()
      )

    return(p)
  }
}

#' Plot Life Cycle Analysis with Plotly
#'
#' Creates interactive plotly visualizations from lifeCycle results
#' for use in biblioshiny
#'
#' @param results Output from lifeCycle() function
#' @param plot_type Character: "annual" or "cumulative" (default: "annual")
#'
#' @return A plotly object
#'
plotLifeCycle <- function(results, plot_type = c("annual", "cumulative")) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop(
      "Package 'plotly' is required. Please install it with: install.packages('plotly')"
    )
  }

  plot_type <- match.arg(plot_type)

  # Extract data
  df <- results$data
  complete <- results$complete_curve
  params <- results$parameters_real_years
  metrics <- results$metrics

  # Separate observed and forecast
  last_obs_year <- max(df$PY)
  observed <- complete[complete$year <= last_obs_year, ]
  forecast <- complete[complete$year > last_obs_year, ]

  if (plot_type == "annual") {
    # === ANNUAL PUBLICATIONS PLOT ===

    p <- plotly::plot_ly()

    # Observed curve (historical fit)
    p <- p %>%
      plotly::add_trace(
        data = observed,
        x = ~year,
        y = ~annual,
        type = 'scatter',
        mode = 'lines',
        name = 'Logistic fit',
        line = list(color = '#1f77b4', width = 2),
        hovertemplate = paste0(
          '<b>Year:</b> %{x}<br>',
          '<b>Annual:</b> %{y:.0f}<br>',
          '<extra></extra>'
        )
      )

    # Forecast curve
    if (nrow(forecast) > 0) {
      p <- p %>%
        plotly::add_trace(
          data = forecast,
          x = ~year,
          y = ~annual,
          type = 'scatter',
          mode = 'lines',
          name = 'Forecast',
          line = list(color = '#1f77b4', width = 2, dash = 'dash'),
          hovertemplate = paste0(
            '<b>Year:</b> %{x}<br>',
            '<b>Projected:</b> %{y:.0f}<br>',
            '<extra></extra>'
          )
        )
    }

    # Observed data points
    p <- p %>%
      plotly::add_trace(
        data = df,
        x = ~PY,
        y = ~n,
        type = 'scatter',
        mode = 'markers',
        name = 'Observed',
        marker = list(color = '#1f77b4', size = 8),
        hovertemplate = paste0(
          '<b>Year:</b> %{x}<br>',
          '<b>Publications:</b> %{y}<br>',
          '<extra></extra>'
        )
      )

    # Peak year line
    p <- p %>%
      plotly::add_trace(
        x = c(params$tm_year, params$tm_year),
        y = c(0, max(complete$annual) * 1.1),
        type = 'scatter',
        mode = 'lines',
        name = paste0('Peak year (', round(params$tm_year, 1), ')'),
        line = list(color = 'red', width = 1.5, dash = 'dash'),
        hoverinfo = 'name'
      )

    # Layout
    p <- p %>%
      plotly::layout(
        title = list(
          text = sprintf(
            "Life Cycle - Annual Publications<br><sup>R² = %.3f | Peak = %.0f publications in %.1f</sup>",
            metrics$R_squared,
            params$peak_annual,
            params$tm_year
          ),
          x = 0.5,
          xanchor = 'center'
        ),
        xaxis = list(
          title = "Year",
          showgrid = FALSE
        ),
        yaxis = list(
          title = "Publications (Annual)",
          showgrid = TRUE,
          gridcolor = '#f0f0f0',
          rangemode = 'tozero'
        ),
        hovermode = 'closest',
        showlegend = TRUE,
        legend = list(
          x = 0.02,
          y = 0.98,
          bgcolor = 'rgba(255, 255, 255, 0.8)',
          bordercolor = '#ddd',
          borderwidth = 1
        ),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  } else {
    # === CUMULATIVE PUBLICATIONS PLOT ===

    K <- params$K

    p <- plotly::plot_ly()

    # Observed curve
    p <- p %>%
      plotly::add_trace(
        data = observed,
        x = ~year,
        y = ~cumulative,
        type = 'scatter',
        mode = 'lines',
        name = 'Logistic fit',
        line = list(color = '#2ca02c', width = 2),
        hovertemplate = paste0(
          '<b>Year:</b> %{x}<br>',
          '<b>Cumulative:</b> %{y:.0f}<br>',
          '<b>% of K:</b> %{customdata:.1f}%<br>',
          '<extra></extra>'
        ),
        customdata = ~ (cumulative / K * 100)
      )

    # Forecast curve
    if (nrow(forecast) > 0) {
      p <- p %>%
        plotly::add_trace(
          data = forecast,
          x = ~year,
          y = ~cumulative,
          type = 'scatter',
          mode = 'lines',
          name = 'Forecast',
          line = list(color = '#2ca02c', width = 2, dash = 'dash'),
          hovertemplate = paste0(
            '<b>Year:</b> %{x}<br>',
            '<b>Projected:</b> %{y:.0f}<br>',
            '<b>% of K:</b> %{customdata:.1f}%<br>',
            '<extra></extra>'
          ),
          customdata = ~ (cumulative / K * 100)
        )
    }

    # Observed data points
    p <- p %>%
      plotly::add_trace(
        data = df,
        x = ~PY,
        y = ~cumulative,
        type = 'scatter',
        mode = 'markers',
        name = 'Observed',
        marker = list(color = '#2ca02c', size = 8),
        hovertemplate = paste0(
          '<b>Year:</b> %{x}<br>',
          '<b>Cumulative:</b> %{y:.0f}<br>',
          '<b>% of K:</b> %{customdata:.1f}%<br>',
          '<extra></extra>'
        ),
        customdata = ~ (cumulative / K * 100)
      )

    # Reference lines (50%, 90%, 99%)
    ref_levels <- data.frame(
      level = c(0.5, 0.9, 0.99),
      label = c("50%", "90%", "99%")
    )

    for (i in 1:nrow(ref_levels)) {
      p <- p %>%
        plotly::add_trace(
          x = c(min(complete$year), max(complete$year)),
          y = c(K * ref_levels$level[i], K * ref_levels$level[i]),
          type = 'scatter',
          mode = 'lines',
          name = ref_levels$label[i],
          line = list(color = 'gray', width = 1, dash = 'dot'),
          hovertemplate = paste0(
            '<b>',
            ref_levels$label[i],
            ' of K</b><br>',
            '<b>Value:</b> ',
            round(K * ref_levels$level[i]),
            '<br>',
            '<extra></extra>'
          ),
          showlegend = FALSE
        )

      # Add annotations
      p <- p %>%
        plotly::add_annotations(
          x = min(complete$year),
          y = K * ref_levels$level[i],
          text = ref_levels$label[i],
          xanchor = 'left',
          yanchor = 'middle',
          showarrow = FALSE,
          font = list(size = 10, color = 'gray')
        )
    }

    # Layout
    p <- p %>%
      plotly::layout(
        title = list(
          text = sprintf(
            "Cumulative Growth Curve<br><sup>Saturation (K) = %.0f publications</sup>",
            K
          ),
          x = 0.5,
          xanchor = 'center'
        ),
        xaxis = list(
          title = "Year",
          showgrid = FALSE
        ),
        yaxis = list(
          title = "Cumulative Publications",
          showgrid = TRUE,
          gridcolor = '#f0f0f0',
          rangemode = 'tozero',
          range = c(0, K * 1.05)
        ),
        hovermode = 'closest',
        showlegend = TRUE,
        legend = list(
          x = 0.02,
          y = 0.98,
          bgcolor = 'rgba(255, 255, 255, 0.8)',
          bordercolor = '#ddd',
          borderwidth = 1
        ),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  }

  return(p)
}

### AUTHOR BIO SKETCH ----

#### GLOBAL PROFILE ----
# Function to get all unique authors from papers with valid DOI only
get_all_authors <- function(df, separator = ";") {
  authors_column <- df$AU
  doi_column <- if ("DI" %in% names(df)) df$DI else NULL

  # If doi_column is provided, filter authors based on valid DOI
  if (!is.null(doi_column)) {
    # Identify entries with valid DOI (not NA and not "<NA>")
    valid_doi <- !is.na(doi_column) & doi_column != "<NA>" & doi_column != ""

    # Filter authors column based on valid DOI
    authors_column <- authors_column[valid_doi]
  }

  # Handle NA and empty strings in authors column
  valid_entries <- authors_column[!is.na(authors_column) & authors_column != ""]

  # Return empty character vector if no valid entries
  if (length(valid_entries) == 0) {
    return(character(0))
  }

  # Split all author strings using the specified separator
  all_authors <- unlist(strsplit(valid_entries, separator, fixed = TRUE))

  # Remove leading and trailing whitespace from each author name
  all_authors_clean <- trimws(all_authors)

  # Remove empty elements that might result from splitting
  all_authors_clean <- all_authors_clean[all_authors_clean != ""]

  # Remove [ANONYMOUS] entries
  all_authors_clean <- all_authors_clean[all_authors_clean != "[ANONYMOUS]"]

  # Return unique authors only
  return(unique(all_authors_clean %>% sort()))
}


authorCard <- function(selected_author, values) {
  req(selected_author)
  works_exact <- findAuthorWorks(
    selected_author,
    values$M,
    exact_match = TRUE
  ) %>%
    filter(!is.na(doi))
  if (nrow(works_exact) == 0) {
    return(HTML("No works found for this author.", type = "error"))
  }
  author_position <- works_exact$author_position[1]
  doi <- works_exact$doi[1]
  on_line <- check_online()

  if (on_line) {
    if (!is.null(values$author_data)) {
      author_data <- values$author_data
    } else {
      author_data <- tibble(AUid = character(), display_name = character())
    }

    if (selected_author %in% author_data$AUid) {
      AU_data <- author_data %>% dplyr::filter(AUid == selected_author)
    } else {
      suppressWarnings(
        AU_data <- tryCatch(
          {
            authorBio(author_position = author_position, doi = doi)
          },
          error = function(e) {
            NULL
          }
        )
      )
      # check if AUid is a tibble
      if (is.data.frame(AU_data)) {
        author_data <- bind_rows(
          author_data,
          AU_data %>%
            mutate(AUid = selected_author)
        )
        values$author_data <- author_data
      } else {
        return(HTML("No author data found.", type = "error"))
      }
    }
    # values$author_data <- author_data
    card <- create_author_bio_card(
      AU_data,
      width = "100%",
      show_trends = TRUE,
      show_topics = TRUE,
      max_topics = 20
    )
  } else {
    card <- HTML(
      "No internet connection. Unable to fetch author data.",
      type = "error"
    )
  }
}

create_empty_author_bio_card <- function(
  author_name = "Author Name",
  width = "100%",
  message = "Author data not found or not yet retrieved"
) {
  # Metrics cards with placeholder values
  metrics_cards <- fluidRow(
    column(
      3,
      div(
        class = "metric-card",
        style = "background: linear-gradient(135deg, #bdc3c7 0%, #95a5a6 100%); 
                        color: white; padding: 15px; border-radius: 8px; text-align: center; opacity: 0.7;",
        h4("--", style = "margin: 0; font-size: 24px;"),
        p(
          "Publications",
          style = "margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;"
        )
      )
    ),
    column(
      3,
      div(
        class = "metric-card",
        style = "background: linear-gradient(135deg, #bdc3c7 0%, #95a5a6 100%); 
                        color: white; padding: 15px; border-radius: 8px; text-align: center; opacity: 0.7;",
        h4("--", style = "margin: 0; font-size: 24px;"),
        p(
          "Citations",
          style = "margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;"
        )
      )
    ),
    column(
      3,
      div(
        class = "metric-card",
        style = "background: linear-gradient(135deg, #bdc3c7 0%, #95a5a6 100%); 
                        color: white; padding: 15px; border-radius: 8px; text-align: center; opacity: 0.7;",
        h4("--", style = "margin: 0; font-size: 24px;"),
        p(
          "H-Index",
          style = "margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;"
        )
      )
    ),
    column(
      3,
      div(
        class = "metric-card",
        style = "background: linear-gradient(135deg, #bdc3c7 0%, #95a5a6 100%); 
                        color: white; padding: 15px; border-radius: 8px; text-align: center; opacity: 0.7;",
        h4("--", style = "margin: 0; font-size: 24px;"),
        p(
          "2yr Mean Cit.",
          style = "margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;"
        )
      )
    )
  )

  # Empty trend chart placeholder
  trend_chart <- tagList(
    h4(
      "Publication Trends (Last 10 Years)",
      style = "margin-top: 20px; color: #95A5A6;"
    ),
    div(
      style = "height: 200px; background: #f8f9fa; border-radius: 8px; padding: 15px; 
                 display: flex; align-items: center; justify-content: center;",
      div(
        style = "text-align: center; color: #95A5A6;",
        tags$i(
          class = "fa fa-chart-bar",
          style = "font-size: 48px; margin-bottom: 10px; opacity: 0.3;"
        ),
        br(),
        "No trend data available"
      )
    )
  )

  # Empty topics section
  topics_section <- tagList(
    h4("Main Research Topics", style = "margin-top: 20px; color: #95A5A6;"),
    div(
      class = "topics-container",
      style = "text-align: center; padding: 20px; 
                                            background: #f8f9fa; border-radius: 8px;",
      div(
        style = "color: #95A5A6;",
        tags$i(
          class = "fa fa-tags",
          style = "font-size: 36px; margin-bottom: 10px; opacity: 0.3;"
        ),
        br(),
        "No research topics available"
      )
    )
  )

  # Main card UI
  div(
    class = "author-bio-card-empty",
    style = paste0(
      "width: ",
      width,
      "; background: white; 
                   border-radius: 12px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); 
                   padding: 25px; margin: 15px 0; font-family: 'Segoe UI', Tahoma, sans-serif;
                   opacity: 0.8; border: 2px dashed #bdc3c7;"
    ),

    # Header section
    div(
      class = "author-header",
      style = "border-bottom: 2px solid #ecf0f1; padding-bottom: 20px;",
      fluidRow(
        column(
          8,
          h2(
            author_name,
            style = "margin: 0 0 10px 0; color: #95A5A6; font-weight: 600;"
          ),
          h5(
            "Institution not available",
            style = "margin: 0 0 5px 0; color: #BDC3C7; font-weight: 400;"
          ),
          p(
            "📍 Country not available",
            style = "margin: 0 0 10px 0; color: #BDC3C7;"
          )
        ),
        column(
          4,
          div(
            style = "text-align: right; padding-top: 10px;",
            div(
              tags$span("ORCID not available", style = "color: #95A5A6;"),
              style = "margin-bottom: 8px;"
            ),
            div(tags$span(
              "OpenAlex Profile not available",
              style = "color: #95A5A6;"
            ))
          )
        )
      )
    ),

    # Info message
    div(
      style = "margin: 20px 0; padding: 15px; background: #fff3cd; border: 1px solid #ffeeba; 
                 border-radius: 8px; color: #856404;",
      tags$i(class = "fa fa-info-circle", style = "margin-right: 8px;"),
      strong("Information: "),
      message
    ),

    # Metrics section
    div(
      style = "margin: 20px 0;",
      h4(
        "Bibliometric Indicators",
        style = "margin-bottom: 15px; color: #95A5A6;"
      ),
      metrics_cards
    ),

    # Additional metrics
    div(
      style = "margin: 15px 0; padding: 15px; background: #f8f9fa; border-radius: 8px;",
      fluidRow(
        column(6, strong("i10-Index: "), span("--", style = "color: #95A5A6;")),
        column(
          6,
          strong("OpenAlex ID: "),
          span(
            "Not available",
            style = "color: #95A5A6; font-family: monospace; font-size: 12px;"
          )
        )
      )
    ),

    # Trends and topics placeholders
    trend_chart,
    topics_section,

    # Footer
    div(
      style = "margin-top: 25px; padding-top: 15px; border-top: 1px solid #ecf0f1; 
                 font-size: 11px; color: #95A5A6; text-align: center;",
      paste(
        "Please retrieve author data to view bibliometric information -",
        format(Sys.time(), "%Y-%m-%d %H:%M")
      )
    )
  )
}

create_author_bio_card <- function(
  author_data,
  width = "100%",
  show_trends = TRUE,
  show_topics = TRUE,
  max_topics = 5
) {
  # Extract key information safely
  author_name <- author_data$display_name[1] %||% "Unknown Author"
  institution <- author_data$primary_affiliation[1] %||%
    author_data$last_known_institutions[[1]]$display_name[1] %||%
    "Institution not available"
  institution_ror <- author_data$primary_affiliation_ror[1] %||%
    author_data$last_known_institutions[[1]]$ror[1] %||%
    NA
  country <- author_data$primary_affiliation_country[1] %||%
    author_data$last_known_institutions[[1]]$country_code[1] %||%
    "Country not available"

  works_count <- author_data$works_count[1] %||% 0
  citations <- author_data$cited_by_count[1] %||% 0
  h_index <- author_data$h_index[1] %||% 0
  i10_index <- author_data$i10_index[1] %||% 0
  mean_citedness <- author_data$`2yr_mean_citedness`[1] %||% 0

  orcid <- author_data$orcid[1]
  position_type <- author_data$author_position_type[1] %||% "author"
  is_corresponding <- author_data$is_corresponding[1] %||% FALSE

  # Create ORCID link if available
  orcid_link <- if (!is.null(orcid) && !is.na(orcid)) {
    tags$a(
      href = orcid,
      target = "_blank",
      tags$img(src = "ORCID.jpg", style = "height: 16px; margin-right: 5px;"),
      "ORCID Profile",
      style = "text-decoration: none; color: #338B13;"
    )
  } else {
    tags$span("ORCID not available", style = "color: #666;")
  }

  # Create OpenAlex link
  openalex_id <- gsub("https://openalex.org/", "", author_data$id[1])
  openalex_link <- tags$a(
    href = paste0("https://openalex.org/", openalex_id),
    target = "_blank",
    tags$img(src = "openalex.jpg", style = "height: 16px; margin-right: 5px;"),
    "OpenAlex Profile",
    style = "text-decoration: none; color: #E74C3C;"
  )

  # Format numbers with thousands separator
  format_number <- function(x) {
    if (is.null(x) || is.na(x)) {
      return("0")
    }
    format(x, big.mark = ",", scientific = FALSE)
  }

  # Create metrics cards
  metrics_cards <- fluidRow(
    column(
      3,
      div(
        class = "metric-card",
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                        color: white; padding: 15px; border-radius: 8px; text-align: center;",
        h4(format_number(works_count), style = "margin: 0; font-size: 24px;"),
        p(
          "Publications",
          style = "margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;"
        )
      )
    ),
    column(
      3,
      div(
        class = "metric-card",
        style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); 
                        color: white; padding: 15px; border-radius: 8px; text-align: center;",
        h4(format_number(citations), style = "margin: 0; font-size: 24px;"),
        p(
          "Citations",
          style = "margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;"
        )
      )
    ),
    column(
      3,
      div(
        class = "metric-card",
        style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); 
                        color: white; padding: 15px; border-radius: 8px; text-align: center;",
        h4(h_index, style = "margin: 0; font-size: 24px;"),
        p(
          "H-Index",
          style = "margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;"
        )
      )
    ),
    column(
      3,
      div(
        class = "metric-card",
        style = "background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%); 
                        color: white; padding: 15px; border-radius: 8px; text-align: center;",
        h4(round(mean_citedness, 1), style = "margin: 0; font-size: 24px;"),
        p(
          "2yr Mean Cit.",
          style = "margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;"
        )
      )
    )
  )

  # Create publication trend chart if requested
  trend_chart <- if (show_trends && !is.null(author_data$counts_by_year[[1]])) {
    counts_data <- author_data$counts_by_year[[1]]
    recent_data <- counts_data[
      counts_data$year >= (max(counts_data$year) - 9),
    ]
    # Order by year from oldest to newest
    recent_data <- recent_data[order(recent_data$year), ]

    tagList(
      h4(
        "Publication Trends (Last 10 Years)",
        style = "margin-top: 20px; color: #2C3E50;"
      ),
      div(
        style = "height: 200px; background: #f8f9fa; border-radius: 8px; padding: 15px;",
        # Simple bar chart representation
        div(
          style = "display: flex; align-items: end; height: 170px; gap: 3px;",
          lapply(1:nrow(recent_data), function(i) {
            height_pct <- (recent_data$works_count[i] /
              max(recent_data$works_count, na.rm = TRUE)) *
              100
            div(
              style = paste0(
                "background: linear-gradient(to top, #3498db, #2980b9); 
                                   height: ",
                height_pct,
                "%; 
                                   width: ",
                100 / nrow(recent_data) - 1,
                "%; 
                                   border-radius: 3px 3px 0 0;
                                   position: relative;"
              ),
              div(
                style = "position: absolute; bottom: -20px; font-size: 10px; 
                               width: 100%; text-align: center; color: #666;",
                recent_data$year[i]
              ),
              div(
                style = "position: absolute; top: -15px; font-size: 9px; 
                               width: 100%; text-align: center; color: #333; font-weight: bold;",
                recent_data$works_count[i]
              )
            )
          })
        )
      )
    )
  } else {
    NULL
  }

  # Create research topics section if requested
  topics_section <- if (show_topics && !is.null(author_data$topics[[1]])) {
    topics_data <- author_data$topics[[1]]
    top_topics <- topics_data[topics_data$type == "topic", ][
      1:min(max_topics, sum(topics_data$type == "topic")),
    ] %>%
      sample_frac(size = 1)

    # Calculate font sizes based on counts
    if (nrow(top_topics) > 0) {
      min_count <- min(top_topics$count, na.rm = TRUE)
      max_count <- max(top_topics$count, na.rm = TRUE)
      min_font_size <- 10
      max_font_size <- 18

      # Calculate proportional font sizes
      font_sizes <- if (max_count == min_count) {
        rep(min_font_size, nrow(top_topics))
      } else {
        min_font_size +
          (top_topics$count - min_count) /
            (max_count - min_count) *
            (max_font_size - min_font_size)
      }
    }

    tagList(
      h4("Main Research Topics", style = "margin-top: 20px; color: #2C3E50;"),
      div(
        class = "topics-container",
        style = "text-align: center;",
        lapply(1:nrow(top_topics), function(i) {
          if (!is.na(top_topics$display_name[i]) && !is.na(top_topics$id[i])) {
            tags$a(
              href = top_topics$id[i],
              target = "_blank",
              class = "topic-badge",
              style = paste0(
                "display: inline-block; background: ",
                colorlist()[i],
                # sample(
                # c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6"),
                # 1),
                "; opacity: 0.7; color: white; padding: 5px 10px; margin: 3px; 
                            border-radius: 15px; font-weight: 500; text-decoration: none;
                            font-size: ",
                round(font_sizes[i], 1),
                "px;"
              ),
              paste0(top_topics$display_name[i], " (", top_topics$count[i], ")")
            )
          }
        })
      )
    )
  } else {
    NULL
  }

  institution_link <- tags$a(
    href = institution_ror,
    target = "_blank",
    institution
    # ,style = "text-decoration: none; color: #E74C3C;"
  )

  # Main card UI
  div(
    class = "author-bio-card",
    style = paste0(
      "width: ",
      width,
      "; background: white; 
                   border-radius: 12px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); 
                   padding: 25px; margin: 15px 0; font-family: 'Segoe UI', Tahoma, sans-serif;"
    ),

    # Header section
    div(
      class = "author-header",
      style = "border-bottom: 2px solid #ecf0f1; padding-bottom: 20px;",
      fluidRow(
        column(
          8,
          h2(
            author_name,
            style = "margin: 0 0 10px 0; color: #2C3E50; font-weight: 600;"
          ),
          h5(
            institution_link,
            style = "margin: 0 0 5px 0; color: #7F8C8D; font-weight: 400;"
          ),
          p(paste("📍", country), style = "margin: 0 0 10px 0; color: #95A5A6;")
        ),
        column(
          4,
          div(
            style = "text-align: right; padding-top: 10px;",
            div(orcid_link, style = "margin-bottom: 8px;"),
            div(openalex_link)
          )
        )
      )
    ),

    # Metrics section
    div(
      style = "margin: 20px 0;",
      h4(
        "Bibliometric Indicators",
        style = "margin-bottom: 15px; color: #2C3E50;"
      ),
      metrics_cards
    ),

    # Additional metrics
    div(
      style = "margin: 15px 0; padding: 15px; background: #f8f9fa; border-radius: 8px;",
      fluidRow(
        column(
          6,
          strong("i10-Index: "),
          span(i10_index, style = "color: #2980b9;")
        ),
        column(
          6,
          strong("OpenAlex ID: "),
          span(
            openalex_id,
            style = "color: #666; font-family: monospace; font-size: 12px;"
          )
        )
      )
    ),

    # Trends and topics
    trend_chart,
    topics_section,

    # Footer with source information
    div(
      style = "margin-top: 25px; padding-top: 15px; border-top: 1px solid #ecf0f1; 
                 font-size: 11px; color: #95A5A6; text-align: center;",
      paste(
        "Data retrieved from OpenAlex on",
        format(author_data$query_timestamp[1], "%Y-%m-%d %H:%M")
      )
    )
  )
}

# Wrapper function for use in Shiny renderUI

render_author_bio_card <- function(author_data, ...) {
  renderUI({
    create_author_bio_card(author_data, ...)
  })
}

#### LOCAL PROFILE ----
create_local_author_bio_card <- function(
  local_author_data,
  selected_author,
  max_py = 2024,
  width = "100%",
  show_trends = TRUE,
  show_keywords = TRUE,
  max_keywords = 8,
  max_works_display = 100
) {
  # Extract author information
  author_name <- to_title_case(selected_author)

  # Calculate metrics from local data
  works_count <- nrow(local_author_data)
  total_citations <- sum(as.numeric(local_author_data$TC), na.rm = TRUE)
  years <- as.numeric(local_author_data$PY)
  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  # Calculate mean citations per year (weighted by age of publications)
  publication_ages <- current_year - years
  publication_ages[publication_ages <= 0] <- 1 # Avoid division by zero
  mean_citations_per_year <- mean(
    as.numeric(local_author_data$TC) / publication_ages,
    na.rm = TRUE
  )

  # Calculate additional metrics
  years_active <- max(years, na.rm = TRUE) - min(years, na.rm = TRUE) + 1
  avg_citations_per_work <- total_citations / works_count

  # Calculate h-index (simplified version)
  citations_sorted <- sort(as.numeric(local_author_data$TC), decreasing = TRUE)
  h_index <- sum(citations_sorted >= seq_along(citations_sorted))

  # Get most recent years for activity
  recent_years <- years[years >= (max_py - 4)]
  recent_productivity <- length(recent_years)

  # Format numbers with thousands separator
  format_number <- function(x) {
    if (is.null(x) || is.na(x)) {
      return("0")
    }
    format(round(x), big.mark = ",", scientific = FALSE)
  }

  # Create metrics cards
  metrics_cards <- fluidRow(
    column(
      3,
      div(
        class = "metric-card",
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                        color: white; padding: 15px; border-radius: 8px; text-align: center;",
        h4(format_number(works_count), style = "margin: 0; font-size: 24px;"),
        p(
          "Publications",
          style = "margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;"
        )
      )
    ),
    column(
      3,
      div(
        class = "metric-card",
        style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); 
                        color: white; padding: 15px; border-radius: 8px; text-align: center;",
        h4(
          format_number(total_citations),
          style = "margin: 0; font-size: 24px;"
        ),
        p(
          "Total Citations",
          style = "margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;"
        )
      )
    ),
    column(
      3,
      div(
        class = "metric-card",
        style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); 
                        color: white; padding: 15px; border-radius: 8px; text-align: center;",
        h4(h_index, style = "margin: 0; font-size: 24px;"),
        p(
          "H-Index (Local)",
          style = "margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;"
        )
      )
    ),
    column(
      3,
      div(
        class = "metric-card",
        style = "background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%); 
                        color: white; padding: 15px; border-radius: 8px; text-align: center;",
        h4(
          round(mean_citations_per_year, 1),
          style = "margin: 0; font-size: 24px;"
        ),
        p(
          "Cit./Year",
          style = "margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;"
        )
      )
    )
  )

  # Create publication trend chart if requested
  trend_chart <- if (show_trends && length(years) > 0) {
    # Create yearly publication counts
    year_counts <- table(years)
    year_df <- data.frame(
      year = as.numeric(names(year_counts)),
      count = as.numeric(year_counts)
    )
    # Get recent 10 years
    recent_years_range <- max(year_df$year) - 9
    recent_data <- year_df[year_df$year >= recent_years_range, ]
    # Fill missing years with 0
    all_years <- recent_years_range:max(year_df$year)
    complete_data <- data.frame(year = all_years)
    complete_data <- merge(complete_data, recent_data, all = TRUE)
    complete_data$count[is.na(complete_data$count)] <- 0
    complete_data <- complete_data[order(complete_data$year), ]

    if (nrow(complete_data) > 0) {
      tagList(
        h4(
          "Publication Trends (Last 10 Years)",
          style = "margin-top: 20px; color: #2C3E50;"
        ),
        div(
          style = "height: 200px; background: #f8f9fa; border-radius: 8px; padding: 15px;",
          div(
            style = "display: flex; align-items: end; height: 170px; gap: 3px;",
            lapply(1:nrow(complete_data), function(i) {
              max_count <- max(complete_data$count, na.rm = TRUE)
              height_pct <- if (max_count > 0) {
                (complete_data$count[i] / max_count) * 100
              } else {
                0
              }
              div(
                style = paste0(
                  "background: linear-gradient(to top, #3498db, #2980b9); 
                                     height: ",
                  max(height_pct, 2),
                  "%; 
                                     width: ",
                  100 / nrow(complete_data) - 1,
                  "%; 
                                     border-radius: 3px 3px 0 0;
                                     position: relative;"
                ),
                div(
                  style = "position: absolute; bottom: -20px; font-size: 10px; 
                                 width: 100%; text-align: center; color: #666;",
                  complete_data$year[i]
                ),
                div(
                  style = "position: absolute; top: -15px; font-size: 9px; 
                                 width: 100%; text-align: center; color: #333; font-weight: bold;",
                  complete_data$count[i]
                )
              )
            })
          )
        )
      )
    }
  } else {
    NULL
  }

  # Create keywords section if requested
  keywords_section <- if (show_keywords && "DE" %in% names(local_author_data)) {
    # Extract and process keywords
    all_keywords <- unlist(strsplit(
      paste(local_author_data$DE, collapse = ";"),
      ";"
    ))
    all_keywords <- to_title_case(trimws(toupper(all_keywords)))
    all_keywords <- all_keywords[all_keywords != "" & !is.na(all_keywords)]

    if (length(all_keywords) > 0) {
      keyword_freq <- sort(table(all_keywords), decreasing = TRUE)
      top_keywords <- head(keyword_freq, max_keywords)
      top_keywords <- top_keywords[sample(length(top_keywords))] # Shuffle order

      # Calculate font sizes based on frequency
      if (length(top_keywords) > 0) {
        min_freq <- min(top_keywords)
        max_freq <- max(top_keywords)
        min_font_size <- 10
        max_font_size <- 18

        font_sizes <- if (max_freq == min_freq) {
          rep(min_font_size, length(top_keywords))
        } else {
          min_font_size +
            (top_keywords - min_freq) /
              (max_freq - min_freq) *
              (max_font_size - min_font_size)
        }
      }

      tagList(
        h4("Main Keywords", style = "margin-top: 20px; color: #2C3E50;"),
        div(
          class = "keywords-container",
          style = "text-align: center;",
          lapply(1:length(top_keywords), function(i) {
            colors <- c(
              "#3498db",
              "#e74c3c",
              "#2ecc71",
              "#f39c12",
              "#9b59b6",
              "#1abc9c",
              "#34495e",
              "#e67e22"
            )
            color <- colors[((i - 1) %% length(colors)) + 1]

            tags$span(
              class = "keyword-badge",
              style = paste0(
                "display: inline-block; background: ",
                color,
                "; opacity: 0.8; color: white; padding: 5px 12px; margin: 3px; 
                            border-radius: 15px; font-weight: 500;
                            font-size: ",
                round(font_sizes[i], 1),
                "px;"
              ),
              paste0(names(top_keywords)[i], " (", top_keywords[i], ")")
            )
          })
        )
      )
    }
  } else {
    NULL
  }

  # Create works list (scrollable)
  works_section <- if (nrow(local_author_data) > 0) {
    # Prepare works data
    works_to_show <- head(
      local_author_data[
        order(as.numeric(local_author_data$PY), decreasing = TRUE),
      ],
      max_works_display
    )

    works_list <- lapply(1:nrow(works_to_show), function(i) {
      work <- works_to_show[i, ]
      title <- work$TI %||% "Title not available"
      journal <- work$SO %||% "Journal not available"
      year <- work$PY %||% "Year not available"
      doi <- work$DI %||% ""
      citations <- as.numeric(work$TC) %||% 0

      # Create DOI link if available
      doi_link <- if (!is.null(doi) && !is.na(doi) && doi != "") {
        tags$a(
          href = paste0("https://doi.org/", doi),
          target = "_blank",
          style = "color: #3498db; text-decoration: none; font-size: 11px;",
          paste0("DOI: ", doi)
        )
      } else {
        tags$span(
          "DOI not available",
          style = "color: #95a5a6; font-size: 11px;"
        )
      }

      div(
        class = "work-item",
        style = "border-bottom: 1px solid #ecf0f1; padding: 12px 0; margin: 0;",
        div(
          style = "margin-bottom: 8px;",
          tags$h6(
            title,
            style = "margin: 0 0 5px 0; color: #2c3e50; font-weight: 600; line-height: 1.3;"
          )
        ),
        div(
          style = "margin-bottom: 8px;",
          tags$span(
            journal,
            style = "color: #7f8c8d; font-style: italic; font-size: 13px; margin-right: 15px;"
          ),
          tags$span(
            paste0("(", year, ")"),
            style = "color: #95a5a6; font-size: 13px; margin-right: 15px;"
          ),
          tags$span(
            paste0("Citations: ", format_number(citations)),
            style = "color: #e74c3c; font-weight: 500; font-size: 12px;"
          )
        ),
        div(style = "margin-top: 5px;", doi_link)
      )
    })

    tagList(
      h4(
        paste0("Publications (", nrow(local_author_data), " total)"),
        style = "margin-top: 20px; color: #2C3E50;"
      ),
      div(
        class = "works-container",
        style = "max-height: 400px; overflow-y: auto; background: #f8f9fa; 
                   border-radius: 8px; padding: 15px; border: 1px solid #e9ecef;",
        works_list
      )
    )
  } else {
    NULL
  }

  # Main card UI
  div(
    class = "local-author-bio-card",
    style = paste0(
      "width: ",
      width,
      "; background: white; 
                   border-radius: 12px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); 
                   padding: 25px; margin: 15px 0; font-family: 'Segoe UI', Tahoma, sans-serif;"
    ),

    # Header section
    div(
      class = "author-header",
      style = "border-bottom: 2px solid #ecf0f1; padding-bottom: 20px;",
      h2(
        author_name,
        style = "margin: 0 0 10px 0; color: #2C3E50; font-weight: 600;"
      ),
      p(
        "📊 Local Collection Profile",
        style = "margin: 0 0 10px 0; color: #7F8C8D; font-style: italic;"
      )
    ),

    # Metrics section
    div(
      style = "margin: 20px 0;",
      h4(
        "Local Bibliometric Indicators",
        style = "margin-bottom: 15px; color: #2C3E50;"
      ),
      metrics_cards
    ),

    # Additional metrics
    div(
      style = "margin: 15px 0; padding: 15px; background: #f8f9fa; border-radius: 8px;",
      fluidRow(
        column(
          4,
          strong("Years Active: "),
          span(years_active, style = "color: #2980b9;")
        ),
        column(
          4,
          strong("Avg Cit./Work: "),
          span(round(avg_citations_per_work, 1), style = "color: #27ae60;")
        ),
        column(
          4,
          strong("Recent Activity (5yr): "),
          span(recent_productivity, style = "color: #e74c3c;")
        )
      )
    ),

    # Trends and keywords
    trend_chart,
    keywords_section,

    # Works section
    works_section,

    # Footer with source information
    div(
      style = "margin-top: 25px; padding-top: 15px; border-top: 1px solid #ecf0f1; 
                 font-size: 11px; color: #95A5A6; text-align: center;",
      paste(
        "Local collection data analyzed on",
        format(Sys.time(), "%Y-%m-%d %H:%M")
      )
    )
  )
}

create_empty_local_author_bio_card <- function(
  author_name = "Author Name Not Available",
  width = "100%",
  message = "Local author data not found or not yet processed"
) {
  # Metrics cards with placeholder values
  metrics_cards <- fluidRow(
    column(
      3,
      div(
        class = "metric-card",
        style = "background: linear-gradient(135deg, #bdc3c7 0%, #95a5a6 100%); 
                        color: white; padding: 15px; border-radius: 8px; text-align: center; opacity: 0.7;",
        h4("--", style = "margin: 0; font-size: 24px;"),
        p(
          "Publications",
          style = "margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;"
        )
      )
    ),
    column(
      3,
      div(
        class = "metric-card",
        style = "background: linear-gradient(135deg, #bdc3c7 0%, #95a5a6 100%); 
                        color: white; padding: 15px; border-radius: 8px; text-align: center; opacity: 0.7;",
        h4("--", style = "margin: 0; font-size: 24px;"),
        p(
          "Total Citations",
          style = "margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;"
        )
      )
    ),
    column(
      3,
      div(
        class = "metric-card",
        style = "background: linear-gradient(135deg, #bdc3c7 0%, #95a5a6 100%); 
                        color: white; padding: 15px; border-radius: 8px; text-align: center; opacity: 0.7;",
        h4("--", style = "margin: 0; font-size: 24px;"),
        p(
          "H-Index (Local)",
          style = "margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;"
        )
      )
    ),
    column(
      3,
      div(
        class = "metric-card",
        style = "background: linear-gradient(135deg, #bdc3c7 0%, #95a5a6 100%); 
                        color: white; padding: 15px; border-radius: 8px; text-align: center; opacity: 0.7;",
        h4("--", style = "margin: 0; font-size: 24px;"),
        p(
          "Cit./Year",
          style = "margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;"
        )
      )
    )
  )

  # Empty trend chart placeholder
  trend_chart <- tagList(
    h4(
      "Publication Trends (Last 10 Years)",
      style = "margin-top: 20px; color: #95A5A6;"
    ),
    div(
      style = "height: 200px; background: #f8f9fa; border-radius: 8px; padding: 15px; 
                 display: flex; align-items: center; justify-content: center; border: 1px solid #e9ecef;",
      div(
        style = "text-align: center; color: #95A5A6;",
        tags$div(
          "📊",
          style = "font-size: 48px; margin-bottom: 10px; opacity: 0.3;"
        ),
        br(),
        "No trend data available"
      )
    )
  )

  # Empty keywords section
  keywords_section <- tagList(
    h4("Main Keywords", style = "margin-top: 20px; color: #95A5A6;"),
    div(
      class = "keywords-container",
      style = "text-align: center; padding: 20px; 
                                             background: #f8f9fa; border-radius: 8px; border: 1px solid #e9ecef;",
      div(
        style = "color: #95A5A6;",
        tags$div(
          "🏷️",
          style = "font-size: 36px; margin-bottom: 10px; opacity: 0.3;"
        ),
        br(),
        "No keywords available"
      )
    )
  )

  # Empty works section
  works_section <- tagList(
    h4("Publications (0 total)", style = "margin-top: 20px; color: #95A5A6;"),
    div(
      class = "works-container",
      style = "height: 200px; background: #f8f9fa; border-radius: 8px; padding: 15px; 
                 border: 1px solid #e9ecef; display: flex; align-items: center; justify-content: center;",
      div(
        style = "text-align: center; color: #95A5A6;",
        tags$div(
          "📄",
          style = "font-size: 48px; margin-bottom: 10px; opacity: 0.3;"
        ),
        br(),
        "No publications available",
        br(),
        tags$small(
          "Publications will appear here when local data is processed",
          style = "font-style: italic; opacity: 0.7;"
        )
      )
    )
  )

  # Main card UI
  div(
    class = "local-author-bio-card-empty",
    style = paste0(
      "width: ",
      width,
      "; background: white; 
                   border-radius: 12px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); 
                   padding: 25px; margin: 15px 0; font-family: 'Segoe UI', Tahoma, sans-serif;
                   opacity: 0.8; border: 2px dashed #bdc3c7;"
    ),

    # Header section
    div(
      class = "author-header",
      style = "border-bottom: 2px solid #ecf0f1; padding-bottom: 20px;",
      h2(
        author_name,
        style = "margin: 0 0 10px 0; color: #95A5A6; font-weight: 600;"
      ),
      p(
        "📊 Local Collection Profile",
        style = "margin: 0 0 10px 0; color: #BDC3C7; font-style: italic;"
      )
    ),

    # Info message
    div(
      style = "margin: 20px 0; padding: 15px; background: #fff3cd; border: 1px solid #ffeeba; 
                 border-radius: 8px; color: #856404;",
      tags$span("ℹ️", style = "margin-right: 8px; font-size: 16px;"),
      strong("Information: "),
      message
    ),

    # Metrics section
    div(
      style = "margin: 20px 0;",
      h4(
        "Local Bibliometric Indicators",
        style = "margin-bottom: 15px; color: #95A5A6;"
      ),
      metrics_cards
    ),

    # Additional metrics
    div(
      style = "margin: 15px 0; padding: 15px; background: #f8f9fa; border-radius: 8px; border: 1px solid #e9ecef;",
      fluidRow(
        column(
          4,
          strong("Years Active: "),
          span("--", style = "color: #95A5A6;")
        ),
        column(
          4,
          strong("Avg Cit./Work: "),
          span("--", style = "color: #95A5A6;")
        ),
        column(
          4,
          strong("Recent Activity (5yr): "),
          span("--", style = "color: #95A5A6;")
        )
      )
    ),

    # Trends, keywords, and works placeholders
    trend_chart,
    keywords_section,
    works_section,

    # Footer
    div(
      style = "margin-top: 25px; padding-top: 15px; border-top: 1px solid #ecf0f1; 
                 font-size: 11px; color: #95A5A6; text-align: center;",
      paste(
        "Please process local collection data to view bibliometric information -",
        format(Sys.time(), "%Y-%m-%d %H:%M")
      )
    )
  )
}

# Helper function for safe extraction (null coalescing operator)
`%||%` <- function(x, y) if (is.null(x) || is.na(x) || length(x) == 0) y else x


# from igraph to png file
igraph2PNG <- function(x, filename, width = 10, height = 7, dpi = 75) {
  V(x)$centr <- centr_betw(x)$res
  df <- data.frame(
    name = V(x)$label,
    cluster = V(x)$color,
    centr = V(x)$centr
  ) %>%
    group_by(cluster) %>%
    slice_head(n = 3)
  V(x)$label[!(V(x)$label %in% df$name)] <- ""
  png(
    filename = filename,
    width = width,
    height = height,
    unit = "in",
    res = dpi
  )
  grid::grid.draw(plot(x))
  dev.off()
}

# from ggplot to plotly
plot.ly <- function(
  g,
  flip = FALSE,
  side = "r",
  aspectratio = 1,
  size = 0.15,
  data.type = 2,
  height = 0,
  customdata = NA
) {
  g <- g + labs(title = NULL)

  gg <- ggplotly(g, tooltip = "text") %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "toImage",
        "sendDataToCloud",
        "pan2d",
        "select2d",
        "lasso2d",
        "toggleSpikelines",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )

  return(gg)
}

freqPlot <- function(
  xx,
  x,
  y,
  textLaby,
  textLabx,
  title,
  values,
  string.max = 70
) {
  xl <- c(
    max(xx[, x]) - 0.02 - diff(range(xx[, x])) * 0.125,
    max(xx[, x]) - 0.02
  ) +
    1
  yl <- c(1, 1 + length(unique(xx[, y])) * 0.125)

  Text <- paste(textLaby, ": ", xx[, y], "\n", textLabx, ": ", xx[, x])

  if (title == "Most Local Cited References" & values$M$DB[1] == "SCOPUS") {
    xx[, y] <- gsub(
      "^(.+?)\\.,.*\\((\\d{4})\\)$",
      paste0("\\1", "., ", "\\2"),
      xx[, y]
    )
  }

  xx[, y] <- substr(xx[, y], 1, string.max)

  g <- ggplot(xx, aes(x = xx[, x], y = xx[, y], label = xx[, x], text = Text)) +
    geom_segment(
      aes(x = 0, y = xx[, y], xend = xx[, x], yend = xx[, y]),
      color = "grey50"
    ) +
    geom_point(aes(color = -xx[, x], size = xx[, x]), show.legend = FALSE) +
    scale_radius(range = c(5, 12)) +
    geom_text(color = "white", size = 3) +
    scale_y_discrete(limits = rev(xx[, y])) +
    scale_fill_continuous(type = "gradient") +
    labs(title = title, y = textLaby) +
    labs(x = textLabx) +
    expand_limits(y = c(1, length(xx[, y]) + 1)) +
    theme_minimal() +
    theme(axis.text.y = element_text(angle = 0, hjust = 0)) +
    annotation_custom(
      values$logoGrid,
      xmin = xl[1],
      xmax = xl[2],
      ymin = yl[1],
      ymax = yl[2]
    )

  return(g)
}

emptyPlot <- function(errortext) {
  g <- ggplot() +
    theme_void() +
    theme(legend.position = "none") +
    annotate("text", x = 4, y = 25, label = errortext, size = 10)
  plot(g)
}

count.duplicates <- function(DF) {
  x <- do.call("paste", c(DF, sep = "\r"))
  ox <- order(x)
  rl <- rle(x[ox])
  cbind(DF[ox[cumsum(rl$lengths)], , drop = FALSE], count = rl$lengths)
}

reduceRefs <- function(A) {
  ind <- unlist(regexec("*V[0-9]", A))
  A[ind > -1] <- substr(A[ind > -1], 1, (ind[ind > -1] - 1))
  ind <- unlist(regexec("*DOI ", A))
  A[ind > -1] <- substr(A[ind > -1], 1, (ind[ind > -1] - 1))
  return(A)
}

check_online <- function(
  host = "8.8.8.8",
  timeout = 5,
  # min_success = 1,
  method = "ping" # method = c("ping", "socket", "http")
) {
  #method <- match.arg(method)

  if (method == "ping") {
    # Usa solo il codice di ritorno, non analizza l'output
    ping_cmd <- if (.Platform$OS.type == "windows") {
      sprintf("ping -n 1 -w %d %s", timeout * 1000, host)
    } else {
      sprintf("ping -c 1 -W %d %s", timeout, host)
    }
    exit_code <- suppressWarnings(system(
      ping_cmd,
      ignore.stdout = TRUE,
      ignore.stderr = TRUE
    ))
    return(exit_code == 0)
  } else if (method == "socket") {
    # Connessione TCP a DNS Google (porta 53)
    tryCatch(
      {
        con <- socketConnection(
          host = host,
          port = 53,
          blocking = TRUE,
          open = "r+",
          timeout = timeout
        )
        close(con)
        return(TRUE)
      },
      error = function(e) {
        return(FALSE)
      }
    )
  } else if (method == "http") {
    # Richiesta HTTP
    tryCatch(
      {
        # check if host start with http or https and add if missing
        if (!grepl("^https?://", host)) {
          host <- paste0("https://", host)
        }

        # con <- url("https://www.google.com", open = "rb")
        con <- url(host, open = "rb")
        on.exit(close(con))
        readLines(con, n = 1, warn = FALSE)
        return(TRUE)
      },
      error = function(e) {
        return(FALSE)
      }
    )
  }
}

# check_online <- function(host = "8.8.8.8", min_success = 1) {
#   # Use ping command to test connectivity (works on Windows, Linux, Mac)
#   ping_cmd <- if (.Platform$OS.type == "windows") {
#     sprintf("ping -n %d %s", min_success, host)
#   } else {
#     sprintf("ping -c %d %s", min_success, host)
#   }
#
#   result <- suppressWarnings(system(
#     ping_cmd,
#     intern = TRUE,
#     ignore.stderr = TRUE
#   ))
#
#   success <- any(grepl("time=", result, ignore.case = TRUE))
#
#   if (success) {
#     # message("✅ Host is reachable.")
#     # Extract average latency (optional)
#     latency_line <- result[grepl("time=", result)]
#     times <- as.numeric(sub(".*time=([0-9.]+).*", "\\1", latency_line))
#     avg_time <- mean(times, na.rm = TRUE)
#     # message(sprintf("📶 Average latency: %.1f ms", avg_time))
#     if (avg_time < 200) {
#       return(TRUE)
#     } else {
#       return(FALSE)
#     }
#     #return(TRUE)
#   } else {
#     #message(FALSE)
#     return(FALSE)
#   }
# }

notifications <- function() {
  ## check connection and download notifications
  #online <- is_online()
  online <- check_online(host = "www.bibliometrix.org")
  location <- "https://www.bibliometrix.org/bs_notifications/biblioshiny_notifications.csv"
  notifOnline <- NULL
  if (isTRUE(online)) {
    notifOnline <- read.csv(location, header = TRUE, sep = ",")
    # ## add check to avoid blocked app when internet connection is to slow

    if (is.null(notifOnline)) {
      online <- FALSE
    } else {
      notifOnline$href[nchar(notifOnline$href) < 6] <- NA
    }
  }

  ## check if a file exists on the local machine and load it
  home <- homeFolder()

  file <- paste(home, "/biblioshiny_notifications.csv", sep = "")
  fileTrue <- file.exists(file)
  if (isTRUE(fileTrue)) {
    suppressWarnings(notifLocal <- read.csv(file, header = TRUE, sep = ","))
  }

  A <- c("noA", "A")
  B <- c("noB", "B")
  status <- paste(A[online + 1], B[fileTrue + 1], sep = "")

  switch(
    status,
    # missing both files (online and local)
    noAnoB = {
      notifTot <- data.frame(
        nots = "No notifications",
        href = NA,
        status = "info"
      ) %>%
        mutate(status = "info")
    },
    # missing online file. The local one exists.
    noAB = {
      notifTot <- notifLocal %>%
        filter(action == TRUE) %>%
        mutate(status = "info")
    },
    # missing the local file. The online one exists.
    AnoB = {
      # notifOnline <- notifOnline %>%
      #   dplyr::slice_head(n = 5)
      notifTot <- notifOnline %>%
        dplyr::filter(action == TRUE) %>%
        mutate(status = "danger") %>%
        dplyr::slice_head(n = 5)
      notifOnline %>%
        dplyr::filter(action == TRUE) %>%
        write.csv(file = file, quote = FALSE, row.names = FALSE)
    },
    # both files exist.
    AB = {
      notifTot <- left_join(
        notifOnline %>% mutate(status = "danger"),
        notifLocal %>% mutate(status = "info"),
        by = "nots"
      ) %>%
        mutate(status = tidyr::replace_na(status.y, "danger")) %>%
        rename(
          href = href.x,
          action = action.x
        ) %>%
        select(nots, href, action, status) %>%
        arrange(status) %>%
        dplyr::filter(action == TRUE) %>%
        dplyr::slice_head(n = 5)
      notifTot %>%
        select(-status) %>%
        write.csv(file = file, quote = FALSE, row.names = FALSE)
    }
  )

  return(notifTot)
}

is_Online <- function(timeout = 3, url = "https://www.bibliometrix.org") {
  RCurl::url.exists(url, timeout = timeout)
}

initial <- function(values) {
  values$results <- list("NA")
  values$log <- "working..."
  values$load <- "FALSE"
  values$field <- values$cocngrams <- "NA"
  values$citField <- values$colField <- values$citSep <- "NA"
  values$NetWords <- values$NetRefs <- values$ColNetRefs <- matrix(NA, 1, 1)
  values$Title <- "Network"
  values$Histfield <- "NA"
  values$histlog <- "working..."
  values$kk <- 0
  values$histsearch <- "NA"
  values$citShortlabel <- "NA"
  values$S <- list("NA")
  values$GR <- "NA"
  values$nMerge <- NULL
  values$collection_description <- NULL
  ### column to export in TALL
  values$corpusCol <- c(
    "Title" = "TI",
    "Abstract" = "AB",
    "Author's Keywords" = "DE"
  )
  values$metadataCol <- c(
    "Publication Year" = "PY",
    "Document Type" = "DT",
    "DOI" = "DI",
    "Open Access" = "OA",
    "Language" = "LA",
    "First Author" = "AU1"
  )

  # Chrome enviroment variable
  if (inherits(try(pagedown::find_chrome(), silent = T), "try-error")) {
    values$Chrome_url <- NULL
  } else {
    values$Chrome_url <- pagedown::find_chrome()
  }

  return(values)
}

resetAnalysis <- function() {
  # reset menus
  output$lifeCycleSummaryUIid <- renderUI({
    div()
  })
  output$DLCPlotYear <- renderUI({
    div()
  })

  output$DLCPlotCum <- renderUI({
    div()
  })
}

### TALL Export functions ----
tallExport <- function(M, tallFields, tallMetadata, metadataCol) {
  corpus <- NULL
  ## Corpus Fields ##
  if ("Abstract" %in% tallFields) {
    if (!"AB_raw" %in% names(M)) {
      M <- M %>%
        mutate(AB_raw = sapply(AB, capitalize_after_dot, USE.NAMES = FALSE))
    }
    corpus <- c(corpus, "AB_raw")
  }

  if ("Title" %in% tallFields) {
    if (!"TI_raw" %in% names(M)) {
      M <- M %>%
        mutate(TI_raw = sapply(TI, capitalize_after_dot, USE.NAMES = FALSE))
    }
    corpus <- c(corpus, "TI_raw")
  }

  if ("Author's Keywords" %in% tallFields) {
    if (!"DE_raw" %in% names(M)) {
      M <- M %>%
        mutate(DE_raw = sapply(DE, capitalize_after_dot, USE.NAMES = FALSE))
    }
    corpus <- c(corpus, "DE_raw")
  }

  corpus <- c(corpus, as.character(metadataCol[tallMetadata]))

  M <- M %>%
    select(SR, any_of(corpus)) %>%
    rename(doc_id = SR)
  names(M) <- gsub("_raw", "", names(M))

  return(M)
}

capitalize_after_dot <- function(text) {
  # Tutto minuscolo
  text <- tolower(text)

  # Prima lettera della stringa maiuscola
  text <- paste0(toupper(substr(text, 1, 1)), substr(text, 2, nchar(text)))

  # Maiuscola dopo punto (o ! o ?) + spazio
  text <- gsub("([\\.\\!\\?]\\s*)([a-z])", "\\1\\U\\2", text, perl = TRUE)

  return(text)
}


### ANALYSIS FUNCTIONS ####
### Descriptive functions ----

ValueBoxes <- function(M) {
  # calculate statistics for Biblioshiny ValueBoxes
  df <- data.frame(Description = rep(NA, 12), Results = rep(NA, 12))

  ## VB  1 - Time span
  df[1, ] <- c("Timespan", paste(range(M$PY, na.rm = T), collapse = ":"))

  ## VB  2 - Authors
  listAU <- (strsplit(M$AU, ";"))
  nAU <- lengths(listAU)
  listAU <- unique(trimws((unlist(listAU))))
  listAU <- listAU[!is.na(listAU)]
  df[2, ] <- c("Authors", length(listAU))

  ## VB  3 - Author's Keywords (DE)
  if (!"DE" %in% names(M)) {
    M$DE <- ""
  }
  DE <- unique(trimws(gsub("\\s+|\\.|\\,", " ", unlist(strsplit(M$DE, ";")))))
  DE <- DE[!is.na(DE)]

  df[3, ] <- c("Author's Keywords (DE)", length(DE))

  ## VB  4 - Sources
  df[4, ] <- c("Sources (Journals, Books, etc)", length(unique(M$SO)))

  ## VB  5 - Authors of single-authored docs

  df[5, ] <- c(
    "Authors of single-authored docs",
    length(unique(M$AU[nAU == 1]))
  )

  ## VB  6 - References
  CR <- trimws(gsub("\\s+|\\.|\\,", " ", unlist(strsplit(M$CR, ";"))))
  CR <- CR[nchar(CR) > 0 & !is.na(CR)]
  nCR <- length(unique(CR))
  if (nCR == 1) {
    nCR <- 0
  }
  df[6, ] <- c("References", nCR)

  ## VB  7 - Documents
  df[7, ] <- c("Documents", nrow(M))

  ## VB  8 - International Co-Authorship
  if (!"AU_CO" %in% names(M)) {
    M <- metaTagExtraction(M, "AU_CO")
  }
  AU_CO <- strsplit(M$AU_CO, ";")
  Coll <- unlist(lapply(AU_CO, function(l) {
    length(unique(l)) > 1
  }))
  Coll <- sum(Coll) / nrow(M) * 100
  df[8, ] <- c("International co-authorships %", format(Coll, digits = 4))

  ## VB  9 - Document Average Age
  age <- as.numeric(substr(Sys.Date(), 1, 4)) - M$PY
  df[9, ] <- c(
    "Document Average Age",
    format(mean(age, na.rm = TRUE), digits = 3)
  )

  ## VB 10 - Annual Growth Rate
  Y <- table(M$PY)
  ny <- diff(range(M$PY, na.rm = TRUE))
  CAGR <- as.numeric(round(((Y[length(Y)] / Y[1])^(1 / (ny)) - 1) * 100, 2))
  df[10, ] <- c("Annual Growth Rate %", CAGR)

  ## VB 11 - Co-Authors per Doc
  df[11, ] <- c("Co-Authors per Doc", format(mean(nAU, na.rm = T), digit = 3))

  ## VB 12 - Average citations per doc
  df[12, ] <- c(
    "Average citations per doc",
    format(mean(M$TC, na.rm = T), digit = 4)
  )

  DT <- M %>%
    mutate(DT = tolower(DT)) %>%
    count(DT) %>%
    rename(
      Description = DT,
      Results = n
    )

  # Indexed Keywords (ID)
  ID <- unique(trimws(gsub("\\s+|\\.|\\,", " ", unlist(strsplit(M$ID, ";")))))
  ID <- ID[!is.na(ID)]
  df[nrow(df) + 1, ] <- c("Keywords Plus (ID)", length(ID))

  # Single authored docs

  df[nrow(df) + 1, ] <- c("Single-authored docs", sum(nAU == 1))

  df2 <- data.frame(
    Description = c(
      "MAIN INFORMATION ABOUT DATA",
      "Timespan",
      "Sources (Journals, Books, etc)",
      "Documents",
      "Annual Growth Rate %",
      "Document Average Age",
      "Average citations per doc",
      "References",
      "DOCUMENT CONTENTS",
      "Keywords Plus (ID)",
      "Author's Keywords (DE)",
      "AUTHORS",
      "Authors",
      "Authors of single-authored docs",
      "AUTHORS COLLABORATION",
      "Single-authored docs",
      "Co-Authors per Doc",
      "International co-authorships %",
      "DOCUMENT TYPES"
    )
  )

  df <- left_join(df2, df, by = "Description") %>%
    rbind(DT) %>%
    mutate(Results = replace_na(Results, ""))

  return(df)
}

countryCollab <- function(M) {
  sep <- ";"
  if (!("AU_CO" %in% names(M))) {
    M <- metaTagExtraction(M, Field = "AU_CO", sep)
  }
  if (!("AU1_CO" %in% names(M))) {
    M <- metaTagExtraction(M, Field = "AU1_CO", sep)
  }

  M$nCO <- as.numeric(unlist(lapply(strsplit(M$AU_CO, ";"), function(l) {
    length(unique(l)) > 1
  })))

  M$AU1_CO <- trim(gsub("[[:digit:]]", "", M$AU1_CO))
  M$AU1_CO <- gsub("UNITED STATES", "USA", M$AU1_CO)
  M$AU1_CO <- gsub("RUSSIAN FEDERATION", "RUSSIA", M$AU1_CO)
  M$AU1_CO <- gsub("TAIWAN", "CHINA", M$AU1_CO)
  M$AU1_CO <- gsub("ENGLAND", "UNITED KINGDOM", M$AU1_CO)
  M$AU1_CO <- gsub("SCOTLAND", "UNITED KINGDOM", M$AU1_CO)
  M$AU1_CO <- gsub("WALES", "UNITED KINGDOM", M$AU1_CO)
  M$AU1_CO <- gsub("NORTH IRELAND", "UNITED KINGDOM", M$AU1_CO)

  df <- M %>%
    group_by(AU1_CO) %>%
    select(AU1_CO, nCO) %>%
    summarize(
      Articles = n(),
      SCP = sum(nCO == 0),
      MCP = sum(nCO == 1)
    ) %>%
    rename(Country = AU1_CO) %>%
    arrange(desc(Articles))

  return(df)
}

Hindex_plot <- function(values, type, input) {
  hindex <- function(values, type, input) {
    switch(
      type,
      author = {
        # AU <- trim(gsub(",","",names(tableTag(values$M,"AU"))))
        values$H <- Hindex(
          values$M,
          field = "author",
          elements = NULL,
          sep = ";",
          years = Inf
        )$H %>%
          arrange(desc(h_index))
      },
      source = {
        # SO <- names(sort(table(values$M$SO),decreasing = TRUE))
        values$H <- Hindex(
          values$M,
          field = "source",
          elements = NULL,
          sep = ";",
          years = Inf
        )$H %>%
          arrange(desc(h_index))
      }
    )

    return(values)
  }

  values <- hindex(values, type = type, input)

  xx <- values$H
  if (type == "author") {
    K <- input$Hkauthor
    measure <- input$HmeasureAuthors
    title <- "Authors' Local Impact"
    xn <- "Authors"
  } else {
    K <- input$Hksource
    measure <- input$HmeasureSources
    title <- "Sources' Local Impact"
    xn <- "Sources"
  }
  if (K > dim(xx)[1]) {
    k <- dim(xx)[1]
  } else {
    k <- K
  }

  switch(
    measure,
    h = {
      m <- 2
    },
    g = {
      m <- 3
    },
    m = {
      m <- 4
      xx[, m] <- round(xx[, m], 2)
    },
    tc = {
      m <- 5
    }
  )
  xx <- xx[order(-xx[, m]), ]
  xx <- xx[1:k, c(1, m)]

  g <- freqPlot(
    xx,
    x = 2,
    y = 1,
    textLaby = xn,
    textLabx = paste("Impact Measure:", toupper(measure)),
    title = paste(title, "by", toupper(measure), "index"),
    values
  )

  res <- list(values = values, g = g)
  return(res)
}

descriptive <- function(values, type) {
  switch(
    type,
    "tab2" = {
      TAB <- values$M %>%
        group_by(PY) %>%
        count() %>%
        rename(
          Year = PY,
          Articles = n
        ) %>%
        right_join(
          data.frame(
            Year = seq(
              min(values$M$PY, na.rm = TRUE),
              max(values$M$PY, na.rm = TRUE)
            )
          ),
          by = "Year"
        ) %>%
        mutate(Articles = replace_na(Articles, 0)) %>%
        arrange(Year) %>%
        as.data.frame()

      ny <- diff(range(TAB$Year))
      values$GR <- round(
        ((TAB[nrow(TAB), 2] / TAB[1, 2])^(1 / (ny)) - 1) * 100,
        digits = 2
      )
    },
    "tab3" = {
      listAU <- (strsplit(values$M$AU, ";"))
      nAU <- lengths(listAU)
      fracAU <- rep(1 / nAU, nAU)
      TAB <- tibble(Author = unlist(listAU), fracAU = fracAU) %>%
        group_by(Author) %>%
        summarize(
          Articles = n(),
          AuthorFrac = sum(fracAU)
        ) %>%
        arrange(desc(Articles)) %>%
        as.data.frame()
      names(TAB) <- c("Authors", "Articles", "Articles Fractionalized")
      # print(S$MostProdAuthors)
    },
    "tab4" = {
      y <- as.numeric(substr(Sys.Date(), 1, 4))
      TAB <- values$M %>%
        mutate(TCperYear = TC / (y + 1 - PY)) %>%
        select(SR, DI, TC, TCperYear, PY) %>%
        group_by(PY) %>%
        mutate(NTC = TC / mean(TC)) %>%
        ungroup() %>%
        select(-PY) %>%
        arrange(desc(TC)) %>%
        as.data.frame()
      names(TAB) <- c(
        "Paper",
        "DOI",
        "Total Citations",
        "TC per Year",
        "Normalized TC"
      )
    },
    "tab5" = {
      TAB <- countryCollab(values$M)
      TAB <- TAB %>%
        mutate(Freq = Articles / sum(Articles)) %>%
        mutate(MCP_Ratio = MCP / Articles) %>%
        drop_na(Country)
    },
    "tab6" = {
      if (!"AU1_CO" %in% names(values$M)) {
        values$M <- metaTagExtraction(values$M, "AU1_CO")
      }
      TAB <- values$M %>%
        select(AU1_CO, TC) %>%
        drop_na(AU1_CO) %>%
        rename(
          Country = AU1_CO,
          TotalCitation = TC
        ) %>%
        group_by(Country) %>%
        summarise(
          "TC" = sum(TotalCitation),
          "Average Article Citations" = round(
            sum(TotalCitation) / length(TotalCitation),
            1
          )
        ) %>%
        arrange(-TC) %>%
        as.data.frame(.data)
    },
    "tab7" = {
      TAB <- values$M %>%
        select(SO) %>%
        group_by(SO) %>%
        count() %>%
        arrange(desc(n)) %>%
        rename(
          Sources = SO,
          Articles = n
        ) %>%
        as.data.frame()
    },
    "tab10" = {
      TAB <- mapworld(values$M)$tab
    },
    "tab11" = {
      if (!("AU_UN" %in% names(values$M))) {
        values$M <- metaTagExtraction(values$M, Field = "AU_UN")
      }
      TAB <- data.frame(Affiliation = unlist(strsplit(values$M$AU_UN, ";"))) %>%
        group_by(Affiliation) %>%
        count() %>%
        drop_na(Affiliation) %>%
        arrange(desc(n)) %>%
        rename(Articles = n) %>%
        filter(Affiliation != "NA") %>%
        as.data.frame()
    },
    "tab12" = {
      TAB <- tableTag(values$M, "C1")
      TAB <- data.frame(Affiliations = names(TAB), Articles = as.numeric(TAB))
      TAB <- TAB[nchar(TAB[, 1]) > 4, ]
      # names(TAB)=c("Affiliations", "Articles")
    },
    "tab13" = {
      CR <- localCitations(values$M, fast.search = FALSE, verbose = FALSE)
      TAB <- CR$Authors
      # TAB=data.frame(Authors=names(CR$Authors$Author), Citations=as.numeric(CR$Cited))
    }
  )
  values$TAB <- TAB
  res <- list(values = values, TAB = TAB)
  return(res)
}

AffiliationOverTime <- function(values, n) {
  if (!("AU_UN" %in% names(values$M))) {
    values$M <- metaTagExtraction(values$M, Field = "AU_UN")
  }
  AFF <- strsplit(values$M$AU_UN, ";")
  nAFF <- lengths(AFF)

  AFFY <- data.frame(
    Affiliation = unlist(AFF),
    Year = rep(values$M$PY, nAFF)
  ) %>%
    filter(Affiliation != "NA") %>%
    drop_na(Affiliation, Year) %>%
    group_by(Affiliation, Year) %>%
    count() %>%
    group_by(Affiliation) %>%
    arrange(Year) %>%
    ungroup() %>%
    pivot_wider(Affiliation, names_from = Year, values_from = n) %>%
    mutate_all(~ replace(., is.na(.), 0)) %>%
    pivot_longer(
      cols = !Affiliation,
      names_to = "Year",
      values_to = "Articles"
    ) %>%
    group_by(Affiliation) %>%
    mutate(Articles = cumsum(Articles))

  Affselected <- AFFY %>%
    filter(Year == max(Year)) %>%
    ungroup() %>%
    slice_max(Articles, n = n)

  values$AffOverTime <- AFFY %>%
    filter(Affiliation %in% Affselected$Affiliation) %>%
    mutate(Year = Year %>% as.numeric())

  Text <- paste(
    values$AffOverTime$Affiliation,
    " (",
    values$AffOverTime$Year,
    ") ",
    values$AffOverTime$Articles,
    sep = ""
  )
  width_scale <- 1.7 * 26 / length(unique(values$AffOverTime$Affiliation))
  x <- c(
    max(values$AffOverTime$Year) -
      0.02 -
      diff(range(values$AffOverTime$Year)) * 0.15,
    max(values$AffOverTime$Year) - 0.02
  ) +
    1
  y <- c(
    min(values$AffOverTime$Articles),
    min(values$AffOverTime$Articles) +
      diff(range(values$AffOverTime$Articles)) * 0.15
  )

  values$AffOverTimePlot <- ggplot(
    values$AffOverTime,
    aes(
      x = Year,
      y = Articles,
      group = Affiliation,
      color = Affiliation,
      text = Text
    )
  ) +
    geom_line() +
    labs(
      x = "Year",
      y = "Articles",
      title = "Affiliations' Production over Time"
    ) +
    scale_x_continuous(
      breaks = (values$AffOverTime$Year[seq(
        1,
        length(values$AffOverTime$Year),
        by = ceiling(length(values$AffOverTime$Year) / 20)
      )])
    ) +
    geom_hline(aes(yintercept = 0), alpha = 0.1) +
    labs(color = "Affiliation") +
    theme(
      text = element_text(color = "#444444"),
      legend.text = ggplot2::element_text(size = width_scale),
      legend.box.margin = margin(6, 6, 6, 6),
      legend.title = ggplot2::element_text(
        size = 1.5 * width_scale,
        face = "bold"
      ),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.key.size = grid::unit(width_scale / 50, "inch"),
      legend.key.width = grid::unit(width_scale / 50, "inch"),
      plot.caption = element_text(
        size = 9,
        hjust = 0.5,
        color = "black",
        face = "bold"
      ),
      panel.background = element_rect(fill = "#FFFFFF"),
      panel.grid.minor = element_line(color = "#EFEFEF"),
      panel.grid.major = element_line(color = "#EFEFEF"),
      plot.title = element_text(size = 24),
      axis.title = element_text(size = 14, color = "#555555"),
      axis.title.y = element_text(vjust = 1, angle = 90),
      axis.title.x = element_text(hjust = 0.95, angle = 0),
      axis.text.x = element_text(size = 10, angle = 90),
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      axis.line.y = element_line(color = "black", linewidth = 0.5)
    ) +
    annotation_custom(
      values$logoGrid,
      xmin = x[1],
      xmax = x[2],
      ymin = y[1],
      ymax = y[2]
    )
  return(values)
}

CountryOverTime <- function(values, n) {
  if (!("AU_CO" %in% names(values$M))) {
    values$M <- metaTagExtraction(values$M, Field = "AU_CO")
  }
  AFF <- strsplit(values$M$AU_CO, ";")
  nAFF <- lengths(AFF)

  AFFY <- data.frame(
    Affiliation = unlist(AFF),
    Year = rep(values$M$PY, nAFF)
  ) %>%
    drop_na(Affiliation, Year) %>%
    group_by(Affiliation, Year) %>%
    count() %>%
    group_by(Affiliation) %>%
    arrange(Year) %>%
    ungroup() %>%
    pivot_wider(Affiliation, names_from = Year, values_from = n) %>%
    mutate_all(~ replace(., is.na(.), 0)) %>%
    pivot_longer(
      cols = !Affiliation,
      names_to = "Year",
      values_to = "Articles"
    ) %>%
    group_by(Affiliation) %>%
    mutate(Articles = cumsum(Articles))

  Affselected <- AFFY %>%
    filter(Year == max(Year)) %>%
    ungroup() %>%
    slice_max(Articles, n = n)

  values$CountryOverTime <- AFFY %>%
    filter(Affiliation %in% Affselected$Affiliation) %>%
    mutate(Year = Year %>% as.numeric()) %>%
    rename(Country = Affiliation)

  Text <- paste(
    values$CountryOverTime$Country,
    " (",
    values$CountryOverTime$Year,
    ") ",
    values$CountryOverTime$Articles,
    sep = ""
  )
  width_scale <- 1.7 * 26 / length(unique(values$CountryOverTime$Country))
  x <- c(
    max(values$CountryOverTime$Year) -
      0.02 -
      diff(range(values$CountryOverTime$Year)) * 0.15,
    max(values$CountryOverTime$Year) - 0.02
  ) +
    1
  y <- c(
    min(values$CountryOverTime$Articles),
    min(values$CountryOverTime$Articles) +
      diff(range(values$CountryOverTime$Articles)) * 0.15
  )

  values$CountryOverTimePlot <- ggplot(
    values$CountryOverTime,
    aes(x = Year, y = Articles, group = Country, color = Country, text = Text)
  ) +
    geom_line() +
    labs(
      x = "Year",
      y = "Articles",
      title = "Country Production over Time"
    ) +
    scale_x_continuous(
      breaks = (values$CountryOverTime$Year[seq(
        1,
        length(values$CountryOverTime$Year),
        by = ceiling(length(values$CountryOverTime$Year) / 20)
      )])
    ) +
    geom_hline(aes(yintercept = 0), alpha = 0.1) +
    labs(color = "Country") +
    theme(
      text = element_text(color = "#444444"),
      legend.text = ggplot2::element_text(size = width_scale),
      legend.box.margin = margin(6, 6, 6, 6),
      legend.title = ggplot2::element_text(
        size = 1.5 * width_scale,
        face = "bold"
      ),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.key.size = grid::unit(width_scale / 50, "inch"),
      legend.key.width = grid::unit(width_scale / 50, "inch"),
      plot.caption = element_text(
        size = 9,
        hjust = 0.5,
        color = "black",
        face = "bold"
      ),
      panel.background = element_rect(fill = "#FFFFFF"),
      panel.grid.minor = element_line(color = "#EFEFEF"),
      panel.grid.major = element_line(color = "#EFEFEF"),
      plot.title = element_text(size = 24),
      axis.title = element_text(size = 14, color = "#555555"),
      axis.title.y = element_text(vjust = 1, angle = 90),
      axis.title.x = element_text(hjust = 0.95, angle = 0),
      axis.text.x = element_text(size = 10, angle = 90),
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      axis.line.y = element_line(color = "black", linewidth = 0.5)
    ) +
    annotation_custom(
      values$logoGrid,
      xmin = x[1],
      xmax = x[2],
      ymin = y[1],
      ymax = y[2]
    )
  return(values)
}

wordlist <- function(
  M,
  Field,
  n,
  measure,
  ngrams,
  remove.terms = NULL,
  synonyms = NULL
) {
  switch(
    Field,
    ID = {
      v <- tableTag(M, "ID", remove.terms = remove.terms, synonyms = synonyms)
    },
    DE = {
      v <- tableTag(M, "DE", remove.terms = remove.terms, synonyms = synonyms)
    },
    KW_Merged = {
      v <- tableTag(
        M,
        "KW_Merged",
        remove.terms = remove.terms,
        synonyms = synonyms
      )
    },
    TI = {
      if (!("TI_TM" %in% names(M))) {
        v <- tableTag(
          M,
          "TI",
          ngrams = ngrams,
          remove.terms = remove.terms,
          synonyms = synonyms
        )
      }
    },
    AB = {
      if (!("AB_TM" %in% names(M))) {
        v <- tableTag(
          M,
          "AB",
          ngrams = ngrams,
          remove.terms = remove.terms,
          synonyms = synonyms
        )
      }
    },
    WC = {
      v <- tableTag(M, "WC")
    }
  )
  names(v) <- tolower(names(v))
  # v=tableTag(values$M,"ID")
  n <- min(c(n, length(v)))
  Words <- data.frame(
    Terms = names(v)[1:n],
    Frequency = (as.numeric(v)[1:n]),
    stringsAsFactors = FALSE
  )
  W <- Words
  switch(
    measure,
    identity = {},
    sqrt = {
      W$Frequency <- sqrt(W$Frequency)
    },
    log = {
      W$Frequency <- log(W$Frequency + 1)
    },
    log10 = {
      W$Frequency <- log10(W$Frequency + 1)
    }
  )

  results <- list(v = v, W = W, Words = Words)
  return(results)
}

readStopwordsFile <- function(file, sep = ",") {
  if (!is.null(file)) {
    req(file$datapath)
    remove.terms <- unlist(strsplit(readr::read_lines(file$datapath), sep))
  } else {
    remove.terms <- NULL
  }
  return(remove.terms)
}

readSynWordsFile <- function(file, sep = ",") {
  if (!is.null(file)) {
    req(file$datapath)
    syn.terms <- readr::read_lines(file$datapath)
    if (sep != ";") syn.terms <- gsub(sep, ";", syn.terms)
  } else {
    syn.terms <- NULL
  }
  return(syn.terms)
}

mapworld <- function(M, values) {
  if (!("AU_CO" %in% names(M))) {
    M <- metaTagExtraction(M, "AU_CO")
  }
  CO <- as.data.frame(tableTag(M, "AU_CO"))
  CO$Tab <- gsub("[[:digit:]]", "", CO$Tab)
  CO$Tab <- gsub(".", "", CO$Tab, fixed = TRUE)
  CO$Tab <- gsub(";;", ";", CO$Tab, fixed = TRUE)
  CO$Tab <- gsub("UNITED STATES", "USA", CO$Tab)
  CO$Tab <- gsub("RUSSIAN FEDERATION", "RUSSIA", CO$Tab)
  CO$Tab <- gsub("TAIWAN", "CHINA", CO$Tab)
  CO$Tab <- gsub("ENGLAND", "UNITED KINGDOM", CO$Tab)
  CO$Tab <- gsub("SCOTLAND", "UNITED KINGDOM", CO$Tab)
  CO$Tab <- gsub("WALES", "UNITED KINGDOM", CO$Tab)
  CO$Tab <- gsub("NORTH IRELAND", "UNITED KINGDOM", CO$Tab)
  CO$Tab <- gsub("UNITED KINGDOM", "UK", CO$Tab)
  CO$Tab <- gsub("KOREA", "SOUTH KOREA", CO$Tab)

  map.world <- map_data("world")
  map.world$region <- toupper(map.world$region)

  # dplyr::anti_join(CO, map.world, by = c('Tab' = 'region'))

  country.prod <- dplyr::left_join(map.world, CO, by = c("region" = "Tab"))

  tab <- data.frame(
    country.prod %>%
      dplyr::group_by(region) %>%
      dplyr::summarise(Freq = mean(Freq))
  )

  tab <- tab[!is.na(tab$Freq), ]

  tab <- tab[order(-tab$Freq), ]

  # breaks=as.numeric(round(quantile(CO$Freq,c(0.2,0.4,0.6,0.8,1))))
  # names(breaks)=breaks
  # breaks=log(breaks)
  breaks <- as.numeric(cut(CO$Freq, breaks = 10))
  names(breaks) <- breaks

  g <- ggplot(
    country.prod,
    aes(
      x = long,
      y = lat,
      group = group,
      text = paste("Country: ", region, "\nN.of Documents: ", Freq)
    )
  ) +
    geom_polygon(aes(fill = Freq, group = group)) +
    scale_fill_continuous(
      low = "#87CEEB",
      high = "dodgerblue4",
      breaks = breaks,
      na.value = "grey80"
    ) +
    guides(fill = guide_legend(reverse = T)) +
    # geom_text(data=centroids, aes(label = centroids$Tab, x = centroids$long, y = centroids$lat, group=centroids$Tab)) +
    labs(
      fill = "N.Documents",
      title = "Country Scientific Production",
      x = NULL,
      y = NULL
    ) +
    theme(
      text = element_text(color = "#333333"),
      plot.title = element_text(size = 28),
      plot.subtitle = element_text(size = 14),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#FFFFFF"), #' #333333'
      plot.background = element_rect(fill = "#FFFFFF"),
      legend.position = "none"
      # ,legend.background = element_blank()
      # ,legend.key = element_blank()
    ) +
    annotation_custom(
      values$logoGrid,
      xmin = 143,
      xmax = 189.5,
      ymin = -69,
      ymax = -48
    )

  results <- list(g = g, tab = tab)
  return(results)
}

### Structure fuctions ----
CAmap <- function(input, values) {
  if ((input$CSfield %in% names(values$M))) {
    if (input$CSfield %in% c("TI", "AB")) {
      ngrams <- as.numeric(input$CSngrams)
    } else {
      ngrams <- 1
    }

    ### load file with terms to remove
    if (input$CSStopFile == "Y") {
      remove.terms <- trimws(values$CSremove.terms$stopword)
    } else {
      remove.terms <- NULL
    }
    # values$CSremove.terms <- remove.terms
    ### end of block
    ### load file with synonyms
    if (input$FASynFile == "Y") {
      synonyms <- values$FAsyn.terms %>%
        group_by(term) %>%
        mutate(term = paste0(term, ";", synonyms)) %>%
        select(term)
      synonyms <- synonyms$term
    } else {
      synonyms <- NULL
    }
    # values$FAsyn.terms <- synonyms
    ### end of block

    tab <- tableTag(values$M, input$CSfield, ngrams = ngrams)
    if (length(tab >= 2)) {
      minDegree <- as.numeric(tab[input$CSn])

      values$CS <- conceptualStructure(
        values$M,
        method = input$method,
        field = input$CSfield,
        minDegree = minDegree,
        clust = input$nClustersCS,
        k.max = 8,
        stemming = F,
        labelsize = input$CSlabelsize / 2,
        documents = input$CSdoc,
        graph = FALSE,
        ngrams = ngrams,
        remove.terms = remove.terms,
        synonyms = synonyms
      )
      if (input$method != "MDS") {
        CSData <- values$CS$docCoord
        CSData <- data.frame(Documents = row.names(CSData), CSData)
        CSData$dim1 <- round(CSData$dim1, 2)
        CSData$dim2 <- round(CSData$dim2, 2)
        CSData$contrib <- round(CSData$contrib, 2)
        values$CS$CSData <- CSData
      } else {
        values$CS$CSData <- data.frame(Docuemnts = NA, dim1 = NA, dim2 = NA)
      }

      switch(
        input$method,
        CA = {
          WData <- data.frame(
            word = row.names(values$CS$km.res$data.clust),
            values$CS$km.res$data.clust,
            stringsAsFactors = FALSE
          )
          names(WData)[4] <- "cluster"
        },
        MCA = {
          WData <- data.frame(
            word = row.names(values$CS$km.res$data.clust),
            values$CS$km.res$data.clust,
            stringsAsFactors = FALSE
          )
          names(WData)[4] <- "cluster"
        },
        MDS = {
          WData <- data.frame(
            word = row.names(values$CS$res),
            values$CS$res,
            cluster = values$CS$km.res$cluster
          )
        }
      )

      WData$Dim1 <- round(WData$Dim1, 2)
      WData$Dim2 <- round(WData$Dim2, 2)
      values$CS$WData <- WData
    } else {
      emptyPlot("Selected field is not included in your data collection")
      values$CS <- list("NA")
    }
  } else {
    emptyPlot("Selected field is not included in your data collection")
    values$CS <- list("NA")
  }
  return(values)
}

historiograph <- function(input, values) {
  min.cit <- 0

  # if (values$Histfield=="NA"){
  values$histResults <- histNetwork(
    values$M,
    min.citations = min.cit,
    sep = ";"
  )
  # values$Histfield="done"
  # }
  values$histResults$histData <- values$histResults$histData %>%
    tibble::rownames_to_column(var = "SR")
  # titlelabel <- input$titlelabel
  values$histlog <- (values$histPlot <- histPlot(
    values$histResults,
    n = input$histNodes,
    size = input$histsize,
    remove.isolates = (input$hist.isolates == "yes"),
    labelsize = input$histlabelsize,
    label = input$titlelabel,
    verbose = FALSE
  ))

  values$histResults$histData$DOI <- paste0(
    '<a href=\"https://doi.org/',
    values$histResults$histData$DOI,
    '\" target=\"_blank\">',
    values$histResults$histData$DOI,
    "</a>"
  )

  values$histResults$histData <- values$histResults$histData %>%
    left_join(
      values$histPlot$layout %>%
        select(name, color),
      by = c("Paper" = "name")
    ) %>%
    drop_na(color) %>%
    mutate(cluster = match(color, unique(color))) %>%
    select(!color) %>%
    group_by(cluster) %>%
    arrange(Year, .by_group = TRUE)

  return(values)
}


### Network functions ----
degreePlot <- function(net) {
  # deg <- data.frame(node = names(net$nodeDegree), x= (1:length(net$nodeDegree)), y = net$nodeDegree)
  ma <- function(x, n = 5) {
    stats::filter(x, rep(1 / n, n), sides = 1)
  }

  deg <- net$nodeDegree %>%
    mutate(x = row_number())

  p <- ggplot(
    data = deg,
    aes(
      x = x,
      y = degree,
      text = paste(node, " - Degree ", round(degree, 3), sep = "")
    )
  ) +
    geom_point() +
    geom_line(aes(group = "NA"), color = "#002F80", alpha = .5) +
    # geom_hline(yintercept=cutting$degree, linetype="dashed",color = '#002F80', alpha = .5)+
    theme(
      text = element_text(color = "#444444"),
      panel.background = element_rect(fill = "#FFFFFF"),
      panel.grid.minor = element_line(color = "#EFEFEF"),
      panel.grid.major = element_line(color = "#EFEFEF"),
      plot.title = element_text(size = 24),
      axis.title = element_text(size = 14, color = "#555555"),
      axis.title.y = element_text(vjust = 1, angle = 0),
      axis.title.x = element_text(hjust = 0),
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      axis.line.y = element_line(color = "black", linewidth = 0.5)
    ) +
    labs(x = "Node", y = "Cumulative Degree", title = "Node Degrees")
  return(p)
}

# Associate a Year to each Keyword
keywords2Years <- function(M, field = "DE", n = 100) {
  suppressMessages(Y <- KeywordGrowth(M, Tag = field, top = Inf, cdf = FALSE))

  ## Normalize data and exclude tot from normalization
  df <- Y %>%
    rowwise() %>%
    mutate(year_freq = sum(c_across(!matches("Year")))) %>%
    mutate(across(!matches("Year"), ~ .x / year_freq)) %>%
    mutate(across(!matches("Year"), ~ replace_na(.x, 0)))

  df_long <- df %>%
    pivot_longer(
      cols = -c(Year, year_freq),
      names_to = "Keyword",
      values_to = "Probability"
    )

  df_max_year <- df_long %>%
    group_by(Keyword) %>%
    filter(Probability == max(Probability)) %>%
    slice_max(order_by = year_freq, n = 1, with_ties = FALSE) %>%
    ungroup()

  return(df_max_year)
}

cocNetwork <- function(input, values) {
  n <- input$Nodes
  label.n <- input$Labels

  ### load file with terms to remove
  if (input$COCStopFile == "Y") {
    remove.terms <- trimws(values$COCremove.terms$stopword)
  } else {
    remove.terms <- NULL
  }
  # values$COCremove.terms <- remove.terms
  ### end of block
  ### load file with synonyms
  if (input$COCSynFile == "Y") {
    synonyms <- values$COCsyn.terms %>%
      group_by(term) %>%
      mutate(term = paste0(term, ";", synonyms)) %>%
      select(term)
    synonyms <- synonyms$term
  } else {
    synonyms <- NULL
  }
  # values$COCsyn.terms <- synonyms
  ### end of block

  if ((input$field %in% names(values$M))) {
    if (
      (dim(values$NetWords)[1]) == 1 |
        !(input$field == values$field) |
        !(input$cocngrams == values$cocngrams) |
        ((dim(values$NetWords)[1]) != input$Nodes)
    ) {
      values$field <- input$field
      values$ngrams <- input$cocngrams

      switch(
        input$field,
        ID = {
          values$NetWords <- biblioNetwork(
            values$M,
            analysis = "co-occurrences",
            network = "keywords",
            n = n,
            sep = ";",
            remove.terms = remove.terms,
            synonyms = synonyms
          )
          values$Title <- "Keywords Plus Network"
        },
        DE = {
          values$NetWords <- biblioNetwork(
            values$M,
            analysis = "co-occurrences",
            network = "author_keywords",
            n = n,
            sep = ";",
            remove.terms = remove.terms,
            synonyms = synonyms
          )
          values$Title <- "Authors' Keywords network"
        },
        KW_Merged = {
          values$NetWords <- biblioNetwork(
            values$M,
            analysis = "co-occurrences",
            network = "all_keywords",
            n = n,
            sep = ";",
            remove.terms = remove.terms,
            synonyms = synonyms
          )
          values$Title <- "All Keywords network"
        },
        TI = {
          # if(!("TI_TM" %in% names(values$M))){
          values$M <- termExtraction(
            values$M,
            Field = "TI",
            verbose = FALSE,
            ngrams = as.numeric(input$cocngrams),
            remove.terms = remove.terms,
            synonyms = synonyms
          )
          # }
          values$NetWords <- biblioNetwork(
            values$M,
            analysis = "co-occurrences",
            network = "titles",
            n = n,
            sep = ";"
          )
          values$Title <- "Title Words network"
        },
        AB = {
          # if(!("AB_TM" %in% names(values$M))){
          values$M <- termExtraction(
            values$M,
            Field = "AB",
            verbose = FALSE,
            ngrams = as.numeric(input$cocngrams),
            remove.terms = remove.terms,
            synonyms = synonyms
          )
          # }
          values$NetWords <- biblioNetwork(
            values$M,
            analysis = "co-occurrences",
            network = "abstracts",
            n = n,
            sep = ";"
          )
          values$Title <- "Abstract Words network"
        },
        WC = {
          WSC <- cocMatrix(values$M, Field = "WC", binary = FALSE)
          values$NetWords <- crossprod(WSC, WSC)
          values$Title <- "Subject Categories network"
        }
      )
    }

    if (label.n > n) {
      label.n <- n
    }
    if (input$normalize == "none") {
      normalize <- NULL
    } else {
      normalize <- input$normalize
    }
    if (input$label.cex == "Yes") {
      label.cex <- TRUE
    } else {
      label.cex <- FALSE
    }
    if (input$coc.curved == "Yes") {
      curved <- TRUE
    } else {
      curved <- FALSE
    }

    values$cocnet <- networkPlot(
      values$NetWords,
      normalize = normalize,
      Title = values$Title,
      type = input$layout,
      size.cex = TRUE,
      size = 5,
      remove.multiple = F,
      edgesize = input$edgesize * 3,
      labelsize = input$labelsize,
      label.cex = label.cex,
      label.n = label.n,
      edges.min = input$edges.min,
      label.color = F,
      curved = curved,
      alpha = input$cocAlpha,
      cluster = input$cocCluster,
      remove.isolates = (input$coc.isolates == "yes"),
      community.repulsion = input$coc.repulsion / 2,
      seed = values$random_seed,
      verbose = FALSE
    )

    g <- values$cocnet$graph
    Y <- keywords2Years(values$M, field = input$field, n = Inf)
    label <- data.frame(Keyword = igraph::V(g)$name)
    df <- label %>%
      left_join(Y %>% mutate(Keyword = tolower(Keyword)), by = "Keyword") %>%
      rename(year_med = Year)
    igraph::V(g)$year_med <- df$year_med

    if (input$cocyears == "Yes") {
      col <- hcl.colors(
        (diff(range(df$year_med)) + 1) * 10,
        palette = "Blues 3"
      )
      igraph::V(g)$color <- col[(max(df$year_med) - df$year_med + 1) * 10]
    }
    values$cocnet$graph <- g
  } else {
    emptyPlot("Selected field is not included in your data collection")
  }
  return(values)
}

intellectualStructure <- function(input, values) {
  n <- input$citNodes
  label.n <- input$citLabels

  if (
    (dim(values$NetRefs)[1]) == 1 |
      !(input$citField == values$citField) |
      !(input$citSep == values$citSep) |
      !(input$citShortlabel == values$citShortlabel) |
      ((dim(values$NetRefs)[1]) != input$citNodes)
  ) {
    values$citField <- input$citField
    values$citSep <- input$citSep
    if (input$citShortlabel == "Yes") {
      shortlabel <- TRUE
    } else {
      shortlabel <- FALSE
    }
    values$citShortlabel <- input$citShortlabel
    switch(
      input$citField,
      CR = {
        values$NetRefs <- biblioNetwork(
          values$M,
          analysis = "co-citation",
          network = "references",
          n = n,
          sep = input$citSep,
          shortlabel = shortlabel
        )
        values$Title <- "Cited References network"
      },
      CR_AU = {
        if (!("CR_AU" %in% names(values$M))) {
          values$M <- metaTagExtraction(
            values$M,
            Field = "CR_AU",
            sep = input$citSep
          )
        }
        values$NetRefs <- biblioNetwork(
          values$M,
          analysis = "co-citation",
          network = "authors",
          n = n,
          sep = input$citSep
        )
        values$Title <- "Cited Authors network"
      },
      CR_SO = {
        if (!("CR_SO" %in% names(values$M))) {
          values$M <- metaTagExtraction(
            values$M,
            Field = "CR_SO",
            sep = input$citSep
          )
        }
        values$NetRefs <- biblioNetwork(
          values$M,
          analysis = "co-citation",
          network = "sources",
          n = n,
          sep = input$citSep
        )
        values$Title <- "Cited Sources network"
      }
    )
  }

  if (label.n > n) {
    label.n <- n
  }
  if (input$citlabel.cex == "Yes") {
    label.cex <- TRUE
  } else {
    label.cex <- FALSE
  }
  if (input$cocit.curved == "Yes") {
    curved <- TRUE
  } else {
    curved <- FALSE
  }

  values$cocitnet <- networkPlot(
    values$NetRefs,
    normalize = NULL,
    Title = values$Title,
    type = input$citlayout,
    size.cex = TRUE,
    size = 5,
    remove.multiple = F,
    edgesize = input$citedgesize * 3,
    labelsize = input$citlabelsize,
    label.cex = label.cex,
    curved = curved,
    label.n = label.n,
    edges.min = input$citedges.min,
    label.color = F,
    remove.isolates = (input$cit.isolates == "yes"),
    alpha = 0.7,
    cluster = input$cocitCluster,
    community.repulsion = input$cocit.repulsion / 2,
    verbose = FALSE
  )
  return(values)
}

authors2Years <- function(M, field = "AU") {
  WAU <- cocMatrix(M, field)
  WPY <- cocMatrix(M, "PY")
  B <- crossprod(WPY, WAU) %>%
    as.matrix() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Year")

  # create a data frame that startig from B associate at each author the year of the first non zero value
  C <- B %>%
    pivot_longer(-Year, names_to = "Item", values_to = "Value") %>%
    filter(Value > 0) %>%
    group_by(Item) %>%
    summarise(FirstYear = min(Year)) %>%
    ungroup()

  return(C)
}

socialStructure <- function(input, values) {
  n <- input$colNodes
  label.n <- input$colLabels

  if (
    (dim(values$ColNetRefs)[1]) == 1 |
      !(input$colField == values$colField) |
      ((dim(values$ColNetRefs)[1]) != input$colNodes)
  ) {
    values$colField <- input$colField

    if (!"nAU" %in% names(values$M)) {
      values$M$nAU <- str_count(values$M$AU, ";") + 1
    }

    switch(
      input$colField,
      COL_AU = {
        if (input$col.filterMaxAuthors) {
          M_AU <- values$M %>% filter(nAU <= 20)
        } else {
          M_AU <- values$M
        }
        values$ColNetRefs <- biblioNetwork(
          M_AU,
          analysis = "collaboration",
          network = "authors",
          n = n,
          sep = ";"
        )
        values$Title <- "Author Collaboration network"
        values$fieldCOL <- "AU"
      },
      COL_UN = {
        if (!("AU_UN" %in% names(values$M))) {
          values$M <- metaTagExtraction(values$M, Field = "AU_UN", sep = ";")
        }
        values$ColNetRefs <- biblioNetwork(
          values$M,
          analysis = "collaboration",
          network = "universities",
          n = n,
          sep = ";"
        )
        values$Title <- "Edu Collaboration network"
        values$fieldCOL <- "AU_UN"
      },
      COL_CO = {
        if (!("AU_CO" %in% names(values$M))) {
          values$M <- metaTagExtraction(values$M, Field = "AU_CO", sep = ";")
        }
        values$ColNetRefs <- biblioNetwork(
          values$M,
          analysis = "collaboration",
          network = "countries",
          n = n,
          sep = ";"
        )
        values$Title <- "Country Collaboration network"
        values$fieldCOL <- "AU_CO"
        # values$cluster="none"
      }
    )
  }

  if (label.n > n) {
    label.n <- n
  }
  if (input$colnormalize == "none") {
    normalize <- NULL
  } else {
    normalize <- input$colnormalize
  }
  if (input$collabel.cex == "Yes") {
    label.cex <- TRUE
  } else {
    label.cex <- FALSE
  }
  if (input$soc.curved == "Yes") {
    curved <- TRUE
  } else {
    curved <- FALSE
  }

  type <- input$collayout
  if (input$collayout == "worldmap") {
    type <- "auto"
  }

  values$colnet <- networkPlot(
    values$ColNetRefs,
    normalize = normalize,
    Title = values$Title,
    type = type,
    size.cex = TRUE,
    size = 5,
    remove.multiple = F,
    edgesize = input$coledgesize * 3,
    labelsize = input$collabelsize,
    label.cex = label.cex,
    curved = curved,
    label.n = label.n,
    edges.min = input$coledges.min,
    label.color = F,
    alpha = input$colAlpha,
    remove.isolates = (input$col.isolates == "yes"),
    cluster = input$colCluster,
    community.repulsion = input$col.repulsion / 2,
    verbose = FALSE
  )

  g <- values$colnet$graph
  Y <- authors2Years(values$M, values$fieldCOL)
  label <- data.frame(Item = igraph::V(g)$name)
  df <- label %>%
    left_join(Y %>% mutate(Item = tolower(Item)), by = "Item") %>%
    rename(year_med = FirstYear)
  igraph::V(g)$year_med <- df$year_med

  values$colnet$graph <- g

  return(values)
}

countrycollaboration <- function(M, label, edgesize, min.edges, values) {
  M <- metaTagExtraction(M, "AU_CO")
  net <- biblioNetwork(M, analysis = "collaboration", network = "countries")
  CO <- data.frame(Tab = rownames(net), Freq = diag(net))
  bsk.network <- igraph::graph_from_adjacency_matrix(net, mode = "undirected")
  COedges <- as.data.frame(igraph::ends(
    bsk.network,
    igraph::E(bsk.network),
    names = TRUE
  ))

  map.world <- map_data("world")
  map.world$region <- toupper(map.world$region)
  map.world$region <- gsub("^UK$", "UNITED KINGDOM", map.world$region)
  map.world$region <- gsub("^SOUTH KOREA$", "KOREA", map.world$region)

  country.prod <- dplyr::left_join(map.world, CO, by = c("region" = "Tab"))

  # breaks <- as.numeric(round(quantile(CO$Freq,seq(0.1,1,by=0.1))))
  breaks <- as.numeric(cut(CO$Freq, breaks = 10))
  names(breaks) <- breaks
  # breaks=breaks
  data("countries", envir = environment())
  names(countries)[1] <- "Tab"

  COedges <- dplyr::inner_join(COedges, countries, by = c("V1" = "Tab"))
  COedges <- dplyr::inner_join(COedges, countries, by = c("V2" = "Tab"))
  COedges <- COedges[COedges$V1 != COedges$V2, ]
  COedges <- count.duplicates(COedges)
  tab <- COedges
  COedges <- COedges[COedges$count >= min.edges, ]
  COedges$region <- paste(
    "\nCollaboration between\n",
    COedges$V1,
    "\n and \n",
    COedges$V2
  )

  g <- ggplot(
    country.prod,
    aes(x = long, y = lat, group = group, text = paste("Country: ", region))
  ) +
    geom_polygon(aes(fill = Freq)) +
    scale_fill_continuous(
      low = "#87CEEB",
      high = "dodgerblue4",
      breaks = breaks,
      na.value = "grey80"
    ) +
    # guides(fill = guide_legend(reverse = T)) +
    guides(colour = FALSE, fill = FALSE) +
    # geom_curve(data=COedges, aes(x = Longitude.x , y = Latitude.x, xend = Longitude.y, yend = Latitude.y,     # draw edges as arcs
    #                              color = "firebrick4", size = count, group=continent.x),
    #            curvature = 0.33,
    #            alpha = 0.5) +
    geom_segment(
      data = COedges,
      aes(
        x = Longitude.x,
        y = Latitude.x,
        xend = Longitude.y,
        yend = Latitude.y, # draw edges as arcs
        size = count,
        group = continent.x
      ),
      color = "orangered4", # FFB347",
      # curvature = 0.33,
      alpha = 0.3
    ) +
    scale_size_continuous(guide = FALSE, range = c(0.25, edgesize)) +
    labs(title = NULL, x = "Latitude", y = "Longitude") +
    theme(
      text = element_text(color = "#333333"),
      plot.title = element_text(size = 28),
      plot.subtitle = element_text(size = 14),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#FFFFFF"), #' #333333'
      plot.background = element_rect(fill = "#FFFFFF"),
      legend.position = c(.18, .36),
      legend.background = element_blank(),
      legend.key = element_blank()
    ) +
    annotation_custom(
      values$logoGrid,
      xmin = 143,
      xmax = 189.5,
      ymin = -69,
      ymax = -48
    )
  if (isTRUE(label)) {
    CO <- dplyr::inner_join(CO, countries, by = c("Tab" = "Tab"))
    g <- g +
      # ggrepel::geom_text_repel(data=CO, aes(x = Longitude, y = Latitude, label = Tab, group=continent),             # draw text labels
      #                          hjust = 0, nudge_x = 1, nudge_y = 4,
      #                          size = 3, color = "orange", fontface = "bold")
      ggrepel::geom_text(
        data = CO,
        aes(x = Longitude, y = Latitude, label = Tab, group = continent), # draw text labels
        hjust = 0,
        nudge_x = 1,
        nudge_y = 4,
        size = 3,
        color = "orange",
        fontface = "bold"
      )
  }

  results <- list(g = g, tab = tab)
  return(results)
}
### visNetwork tools ----
netLayout <- function(type) {
  switch(
    type,
    auto = {
      l <- "layout_nicely"
    },
    circle = {
      l <- "layout_in_circle"
    },
    mds = {
      l <- "layout_with_mds"
    },
    star = {
      l <- "layout_as_star"
    },
    sphere = {
      l <- "layout_on_sphere"
    },
    fruchterman = {
      l <- "layout_with_fr"
    },
    kamada = {
      l <- "layout_with_kk"
    }
  )
  return(l)
}

savenetwork <- function(con, VIS) {
  VIS %>%
    visOptions(height = "800px") %>%
    visNetwork::visSave(con)
}

igraph2vis <- function(
  g,
  curved,
  labelsize,
  opacity,
  type,
  shape,
  net,
  shadow = TRUE,
  edgesize = 5,
  noOverlap = TRUE
) {
  LABEL <- igraph::V(g)$name

  LABEL[igraph::V(g)$labelsize == 0] <- ""

  vn <- visNetwork::toVisNetworkData(g)

  vn$nodes$label <- LABEL
  vn$edges$num <- 1
  vn$edges$dashes <- FALSE
  vn$edges$dashes[vn$edges$lty == 2] <- TRUE

  ## opacity
  vn$nodes$color <- adjustcolor(vn$nodes$color, alpha.f = min(c(opacity, 1)))
  ## set a darkest gray for iter-cluster edges
  vn$edges$color <- paste(substr(vn$edges$color, 1, 7), "90", sep = "")
  vn$edges$color[substr(vn$edges$color, 1, 7) == "#B3B3B3"] <- "#69696960"
  vn$edges$color <- adjustcolor(vn$edges$color, alpha.f = opacity)

  ## removing multiple edges
  vn$edges <- unique(vn$edges)

  vn$edges$width <- vn$edges$width^2 / (max(vn$edges$width^2)) * (10 + edgesize)

  # if (edgesize==0){
  #   vn$edges$hidden <- TRUE
  #   }else{vn$edges$hidden <- FALSE}

  ## labelsize
  vn$nodes$font.size <- vn$nodes$deg
  scalemin <- 20
  scalemax <- 150
  Min <- min(vn$nodes$font.size)
  Max <- max(vn$nodes$font.size)
  if (Max > Min) {
    size <- (vn$nodes$font.size - Min) / (Max - Min) * 15 * labelsize + 10
  } else {
    size <- 10 * labelsize
  }
  size[size < scalemin] <- scalemin
  size[size > scalemax] <- scalemax
  vn$nodes$font.size <- size
  l <- netLayout(type)

  ### TO ADD SHAPE AND FONT COLOR OPTIONS
  coords <- net$layout

  vn$nodes$size <- vn$nodes$font.size * 0.7

  # vn$nodes$font.color <- adjustcolor("black", alpha.f = min(c(opacity,1)))

  if (shape %in% c("dot", "square")) {
    vn$nodes$font.vadjust <- -0.7 * vn$nodes$font.size
  } else {
    vn$nodes$font.vadjust <- 0
  }

  opacity_font <- sqrt(
    (vn$nodes$font.size - min(vn$nodes$font.size)) /
      diff(range(vn$nodes$font.size))
  ) *
    opacity +
    0.3
  if (is.nan(opacity_font[1])) {
    opacity_font <- rep(0.3, length(opacity_font))
  }

  if (labelsize > 0) {
    vn$nodes$font.color <- unlist(lapply(opacity_font, function(x) {
      adjustcolor("black", alpha.f = x)
    }))
  } else {
    vn$nodes$font.color <- adjustcolor("black", alpha.f = 0)
  }
  ## avoid label overlaps
  if (noOverlap) {
    threshold <- 0.05
    ymax <- diff(range(coords[, 2]))
    xmax <- diff(range(coords[, 1]))
    threshold2 <- threshold * mean(xmax, ymax)
    w <- data.frame(
      x = coords[, 1],
      y = coords[, 2],
      labelToPlot = vn$nodes$label,
      dotSize = size,
      row.names = vn$nodes$label
    )
    labelToRemove <- avoidNetOverlaps(w, threshold = threshold2)
  } else {
    labelToRemove <- ""
  }

  vn$nodes <- vn$nodes %>%
    mutate(
      label = ifelse(label %in% labelToRemove, "", label),
      title = id
    )
  ##

  VIS <-
    visNetwork::visNetwork(
      nodes = vn$nodes,
      edges = vn$edges,
      type = "full",
      smooth = TRUE,
      physics = FALSE
    ) %>%
    visNetwork::visNodes(
      shadow = shadow,
      shape = shape,
      font = list(
        color = vn$nodes$font.color,
        size = vn$nodes$font.size,
        vadjust = vn$nodes$font.vadjust
      )
    ) %>%
    visNetwork::visIgraphLayout(
      layout = "layout.norm",
      layoutMatrix = coords,
      type = "full"
    ) %>%
    visNetwork::visEdges(smooth = list(type = "horizontal")) %>%
    visNetwork::visOptions(
      highlightNearest = list(enabled = T, hover = T, degree = 1),
      nodesIdSelection = T
    ) %>%
    visNetwork::visInteraction(
      dragNodes = TRUE,
      navigationButtons = F,
      hideEdgesOnDrag = TRUE,
      zoomSpeed = 0.4
    ) %>%
    visNetwork::visOptions(
      manipulation = curved,
      height = "100%",
      width = "100%"
    )

  return(list(VIS = VIS, vn = vn, type = type, l = l, curved = curved))
}

## function to avoid label overlapping ----
avoidNetOverlaps <- function(w, threshold = 0.10) {
  w[, 2] <- w[, 2] / 2

  Ds <- dist(
    w %>%
      dplyr::filter(labelToPlot != "") %>%
      select(1:2),
    method = "manhattan",
    upper = T
  ) %>%
    dist2df() %>%
    rename(
      from = row,
      to = col,
      dist = value
    ) %>%
    left_join(
      w %>% dplyr::filter(labelToPlot != "") %>% select(labelToPlot, dotSize),
      by = c("from" = "labelToPlot")
    ) %>%
    rename(w_from = dotSize) %>%
    left_join(
      w %>% dplyr::filter(labelToPlot != "") %>% select(labelToPlot, dotSize),
      by = c("to" = "labelToPlot")
    ) %>%
    rename(w_to = dotSize) %>%
    filter(dist < threshold)

  if (nrow(Ds) > 0) {
    st <- TRUE
    i <- 1
    label <- NULL
    case <- "n"

    while (isTRUE(st)) {
      if (Ds$w_from[i] > Ds$w_to[i] & Ds$dist[i] < threshold) {
        case <- "y"
        lab <- Ds$to[i]
      } else if (Ds$w_from[i] <= Ds$w_to[i] & Ds$dist[i] < threshold) {
        case <- "y"
        lab <- Ds$from[i]
      }

      switch(
        case,
        "y" = {
          Ds <- Ds[Ds$from != lab, ]
          Ds <- Ds[Ds$to != lab, ]
          label <- c(label, lab)
        },
        "n" = {
          Ds <- Ds[-1, ]
        }
      )

      if (i >= nrow(Ds)) {
        st <- FALSE
      }
      case <- "n"
      # print(nrow(Ds))
    }
  } else {
    label <- NULL
  }
  label
}


## visnetwork for subgraphs
igraph2visClust <- function(
  g,
  curved = FALSE,
  labelsize = 3,
  opacity = 0.7,
  shape = "dot",
  shadow = TRUE,
  edgesize = 5
) {
  LABEL <- igraph::V(g)$name

  LABEL[igraph::V(g)$labelsize == 0] <- ""

  vn <- visNetwork::toVisNetworkData(g)

  vn$nodes$label <- LABEL
  vn$edges$num <- 1
  vn$edges$dashes <- FALSE
  vn$edges$dashes[vn$edges$lty == 2] <- TRUE

  ## opacity
  vn$nodes$color <- adjustcolor(vn$nodes$color, alpha.f = min(c(opacity, 1)))
  ## set a darkest gray for iter-cluster edges
  vn$edges$color <- paste(substr(vn$edges$color, 1, 7), "90", sep = "")
  vn$edges$color[substr(vn$edges$color, 1, 7) == "#B3B3B3"] <- "#69696960"
  vn$edges$color <- adjustcolor(vn$edges$color, alpha.f = opacity)

  ## removing multiple edges
  vn$edges <- unique(vn$edges)

  vn$edges$width <- vn$edges$width^2 / (max(vn$edges$width^2)) * (5 + edgesize)

  ## labelsize
  scalemin <- 20
  scalemax <- 100
  # aggiunta
  vn$nodes$font.size <- vn$nodes$deg
  #
  Min <- min(vn$nodes$font.size)
  Max <- max(vn$nodes$font.size)
  if (Max > Min) {
    size <- (vn$nodes$font.size - Min) / (Max - Min) * 15 * labelsize #+10
  } else {
    size <- 5 * labelsize
  }
  size[size < scalemin] <- scalemin
  size[size > scalemax] <- scalemax
  vn$nodes$font.size <- size
  # l<-netLayout(type)

  ### TO ADD SHAPE AND FONT COLOR OPTIONS

  vn$nodes$size <- vn$nodes$font.size * 0.4

  if (shape %in% c("dot", "square")) {
    vn$nodes$font.vadjust <- -0.7 * vn$nodes$font.size
  } else {
    vn$nodes$font.vadjust <- 0
  }

  opacity_font <- sqrt(
    (vn$nodes$font.size - min(vn$nodes$font.size)) /
      diff(range(vn$nodes$font.size))
  ) *
    opacity +
    0.3
  if (is.nan(opacity_font[1])) {
    opacity_font <- rep(0.3, length(opacity_font))
  }

  if (labelsize > 0) {
    vn$nodes$font.color <- unlist(lapply(opacity_font, function(x) {
      adjustcolor("black", alpha.f = x)
    }))
  } else {
    vn$nodes$font.color <- adjustcolor("black", alpha.f = 0)
  }

  VIS <-
    visNetwork::visNetwork(
      nodes = vn$nodes,
      edges = vn$edges,
      type = "full",
      smooth = TRUE,
      physics = FALSE
    ) %>%
    visNetwork::visNodes(
      shadow = shadow,
      shape = shape,
      font = list(
        color = vn$nodes$font.color,
        size = vn$nodes$font.size,
        vadjust = vn$nodes$font.vadjust
      )
    ) %>%
    visNetwork::visIgraphLayout(layout = "layout_nicely", type = "full") %>%
    visNetwork::visEdges(smooth = list(type = "horizontal")) %>%
    visNetwork::visOptions(
      highlightNearest = list(enabled = T, hover = T, degree = 1),
      nodesIdSelection = T
    ) %>%
    visNetwork::visInteraction(
      dragNodes = TRUE,
      navigationButtons = F,
      hideEdgesOnDrag = TRUE,
      zoomSpeed = 0.4
    ) %>%
    visNetwork::visOptions(
      manipulation = curved,
      height = "100%",
      width = "100%"
    )

  return(list(VIS = VIS, vn = vn))
}


hist2vis <- function(
  net,
  labelsize = 2,
  nodesize = 2,
  curved = FALSE,
  shape = "dot",
  opacity = 0.7,
  labeltype = "short",
  timeline = TRUE
) {
  LABEL <- igraph::V(net$net)$id

  LABEL[igraph::V(net$net)$labelsize == 0] <- ""

  layout <- net$layout %>%
    dplyr::select(x, y, color, name)

  vn <- visNetwork::toVisNetworkData(net$net)

  vn$nodes$short_label <- LABEL

  if (labeltype != "short") {
    vn$nodes$label <- paste0(vn$nodes$years, ": ", LABEL)
  } else {
    vn$nodes$label <- LABEL
  }

  vn$nodes <- dplyr::left_join(vn$nodes, layout, by = c("id" = "name"))

  vn$edges$num <- 1
  vn$edges$dashes <- FALSE
  vn$edges$dashes[vn$edges$lty == 2] <- TRUE
  vn$edges$color <- "grey"

  ## opacity
  vn$nodes$font.color <- vn$nodes$color

  vn$nodes$color <- adjustcolor(
    vn$nodes$color,
    alpha.f = min(c(opacity - 0.2, 1))
  )
  vn$edges$color <- adjustcolor(vn$edges$color, alpha.f = opacity - 0.2)
  vn$edges$smooth <- curved

  ## removing multiple edges
  vn$edges <- unique(vn$edges)

  ## labelsize
  scalemin <- 20
  scalemax <- 150
  size <- 10 * labelsize
  size[size < scalemin] <- scalemin
  size[size > scalemax] <- scalemax
  vn$nodes$font.size <- size * 0.5
  vn$nodes$size <- nodesize * 2

  if (shape %in% c("dot", "square")) {
    vn$nodes$font.vadjust <- -0.7 * vn$nodes$font.size
  } else {
    vn$nodes$font.vadjust <- 0
  }

  text_data <- net$graph.data %>%
    select(Label, DOI, LCS, GCS) %>%
    rename(id = Label) %>%
    filter(!duplicated(id))

  vn$nodes <- vn$nodes %>% left_join(text_data, by = "id")

  ## split node tooltips into two strings
  title <- strsplit(stringi::stri_trans_totitle(vn$nodes$title), " ")

  vn$nodes$title <- unlist(lapply(title, function(l) {
    n <- floor(length(l) / 2)
    paste0(
      paste(l[1:n], collapse = " ", sep = ""),
      "<br>",
      paste(l[(n + 1):length(l)], collapse = " ", sep = "")
    )
  }))

  vn$nodes <- vn$nodes %>%
    mutate(
      title_orig = title,
      title = paste(
        "<b>Title</b>: ",
        title,
        "<br><b>DOI</b>: ",
        paste0(
          '<a href=\"https://doi.org/',
          DOI,
          '\" target=\"_blank\">',
          # "DOI: ",
          DOI,
          "</a>"
        ),
        "<br><b>GCS</b>: ",
        GCS,
        "<br><b>LCS</b>: ",
        LCS,
        sep = ""
      )
    )

  ## add time line
  vn$nodes$group <- "normal"
  vn$nodes$shape <- "dot"
  vn$nodes$shadow <- TRUE

  # nr <- nrow(vn$nodes)
  # y <- max(vn$nodes$y)
  # vn$nodes[nr + 1, c("id", "title", "label", "color", "font.color")] <-
  #   c(rep("logo", 3), "black", "white")
  # vn$nodes$x[nr + 1] <- max(vn$nodes$x, na.rm = TRUE) + 1
  # vn$nodes$y[nr + 1] <- y
  # vn$nodes$size[nr + 1] <- vn$nodes$size[nr] * 4
  # vn$nodes$years[nr + 1] <- as.numeric(vn$nodes$x[nr + 1])
  # vn$nodes$font.size[nr + 1] <- vn$nodes$font.size[nr]
  # vn$nodes$group[nr + 1] <- "logo"
  # vn$nodes$shape[nr + 1] <- "image"
  # vn$nodes$image[nr + 1] <- "logo.jpg"
  # vn$nodes$fixed.x <- TRUE
  # vn$nodes$fixed.y <- FALSE
  # vn$nodes$fixed.y[nr + 1] <- TRUE
  # vn$nodes$shadow[nr + 1] <- FALSE

  # coords <- vn$nodes[, c("x", "y")] %>%
  #   as.matrix()
  #
  # coords[, 2] <- coords[, 2]^(1 / 2)

  tooltipStyle <- ("position: fixed;visibility:hidden;padding: 5px;white-space: nowrap;
                  font-size:12px;font-color:black;background-color:white;")

  ## Font opacity
  vn$nodes$LCS[is.na(vn$nodes$LCS)] <- max(vn$nodes$LCS, na.rm = TRUE)
  opacity_font <- sqrt(
    (vn$nodes$LCS - min(vn$nodes$LCS)) / diff(range(vn$nodes$LCS))
  ) *
    0.6 +
    0.4

  vn$nodes$size <- opacity_font * 5 * nodesize
  vn$nodes$size[nrow(vn$nodes)] <- max(5 * nodesize)

  for (i in 1:nrow(vn$nodes)) {
    vn$nodes$font.color[i] <- adjustcolor(
      vn$nodes$font.color[i],
      alpha.f = opacity_font[i]
    )
  }

  x <- vn$nodes$x
  y <- vn$nodes$y
  vn$nodes$x <- y
  vn$nodes$y <- x

  vn$nodes <- assign_horizontal_coords_clusters_adaptive(vn$nodes)

  vn$nodes$fixed.x <- FALSE
  vn$nodes$fixed.y <- TRUE

  coords <- vn$nodes[, c("x", "y")] %>%
    as.matrix()
  coords[, 2] <- coords[, 2]

  VIS <-
    visNetwork::visNetwork(
      nodes = vn$nodes,
      edges = vn$edges,
      type = "full",
      smooth = TRUE,
      physics = FALSE
    ) %>%
    visNetwork::visNodes(
      shadow = vn$nodes$shadow,
      shape = shape,
      size = vn$nodes$size,
      font = list(
        color = vn$nodes$font.color,
        size = vn$nodes$font.size,
        vadjust = vn$nodes$font.vadjust
      )
    ) %>%
    visNetwork::visIgraphLayout(
      layout = "layout.norm",
      layoutMatrix = coords,
      type = "full"
    ) %>%
    #visNetwork::visEdges(smooth = list(type = "horizontal"), arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5))) %>%
    visNetwork::visEdges(
      smooth = list(enabled = TRUE, type = "dynamic", roundness = 0.3),
      arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5))
    ) %>%
    visNetwork::visInteraction(
      dragNodes = T,
      navigationButtons = F,
      hideEdgesOnDrag = F,
      tooltipStyle = tooltipStyle,
      zoomSpeed = 0.2
    ) %>%
    visNetwork::visOptions(
      highlightNearest = list(
        enabled = T,
        hover = T,
        degree = list(from = 1),
        algorithm = "hierarchical"
      ),
      nodesIdSelection = F,
      manipulation = FALSE,
      height = "100%",
      width = "100%"
    )

  return(list(VIS = VIS, vn = vn, type = "historiograph", curved = curved))
}

## calculate node coordinates in historiograph
assign_horizontal_coords_clusters_adaptive <- function(
  nodes_df,
  spacing_base = 1.0,
  cluster_spacing = 6,
  tol = 0.15
) {
  clusters <- nodes_df %>%
    count(color, name = "n_cluster") %>%
    arrange(desc(n_cluster)) %>%
    mutate(
      cluster_id = row_number(),
      cluster_center = (cluster_id - mean(cluster_id)) * cluster_spacing
    )

  nodes_df <- nodes_df %>%
    left_join(clusters, by = "color")

  nodes_df <- nodes_df %>%
    group_by(years, color) %>%
    mutate(
      n_nodes = n(),
      spacing = spacing_base * n_nodes, # USA direttamente il numero di nodi
      x = (cluster_center[1] + spacing[1] * (row_number() - (n() + 1) / 2)) *
        runif(1, 1 - tol, 1 + tol)
    ) %>%
    ungroup()

  return(nodes_df)
}

## Pajek Export
graph2Pajek <- function(graph, filename = "my_pajek_network") {
  nodes <- igraph::as_data_frame(graph, what = c("vertices")) %>%
    mutate(id = row_number())

  edges <- igraph::as_data_frame(graph, what = c("edges"))
  edges <- edges %>%
    left_join(nodes %>% select(id, name), by = c("from" = "name")) %>%
    rename(id_from = id) %>%
    left_join(nodes %>% select(id, name), by = c("to" = "name")) %>%
    rename(id_to = id)

  ### Creation of NET file
  file <- paste0(filename, ".net")

  # Nodes
  write(paste0("*Vertices ", nrow(nodes)), file = file)
  write(paste0(nodes$id, ' "', nodes$name, '"'), file = file, append = T)

  # Edges
  write(paste0("*Edges ", nrow(nodes)), file = file, append = T)
  write(
    paste0(edges$id_from, " ", edges$id_to, " ", edges$weight),
    file = file,
    append = T
  )

  ### Creation of VEC file
  file <- paste0(filename, ".vec")

  # Nodes
  write(paste0("*Vertices ", nrow(nodes)), file = file)
  write(paste0(nodes$deg), file = file, append = T)

  ### Creation of CLU file
  file <- paste0(filename, ".clu")

  # Nodes
  write(paste0("*Vertices ", nrow(nodes)), file = file)
  write(paste0(nodes$community), file = file, append = T)
}


## Dendogram to Visnetwork
dend2vis <- function(hc, labelsize, nclusters = 1, community = FALSE) {
  # community = TRUE means that hc is an igraph community detection object
  # community = FALSE mean that hc is a hclust object

  # transform and plot a community igraph object using dendrogram
  if (community) {
    hc <- as.hclust(hc, use.modularity = TRUE)
  }

  h_tail <- round((max(hc$height) * 0.12), 1)

  hc$height <- hc$height + h_tail

  VIS <- visHclust(
    hc,
    cutree = nclusters,
    colorEdges = "grey60",
    horizontal = TRUE,
    export = FALSE
  )
  VIS$x$edges <- data.frame(color = unique(VIS$x$edges$color)) %>%
    mutate(new_color = colorlist()[1:nrow(.)]) %>%
    right_join(VIS$x$edges, by = "color") %>%
    select(-color) %>%
    rename(color = new_color)
  VIS$x$nodes <- VIS$x$nodes %>%
    mutate(
      label = ifelse(group != "individual", NA, label),
      group = ifelse(group == "individual", "word", group),
      title = gsub("individuals", "words", title),
      value = 1,
      scaling.min = 10,
      scaling.max = 10
    )
  coords <- VIS$x$nodes %>%
    select(x, y) %>%
    as.matrix()

  edges <- VIS$x$edges
  nodes <- VIS$x$nodes %>%
    select(id, label) %>%
    dplyr::filter(label != "1")

  VIS$x$edges <- edges %>%
    select(-id) %>%
    left_join(nodes, by = c("to" = "id")) %>%
    select(-label.x) %>%
    rename(label = label.y) %>%
    mutate(
      value = 10,
      font.color = color,
      font.size = labelsize * 10,
      font.vadjust = -0.2 * font.size,
      label = ifelse(is.na(label), "", label)
    )

  VIS <- VIS %>%
    visGroups(
      groupname = "group",
      color = "gray90",
      shape = "dot",
      size = 10
    ) %>%
    visGroups(
      groupname = "word",
      font = list(size = 0),
      color = list(
        background = "white",
        border = "#80B1D3",
        highlight = "#e2e9e9",
        hover = "orange"
      ),
      shape = "box"
    ) %>%
    visNodes(font = list(align = VIS$x$nodes$font.align)) %>%
    visNetwork::visOptions(
      highlightNearest = list(
        enabled = T,
        hover = T,
        degree = list(to = 1000, from = 0),
        algorithm = "hierarchical"
      ),
      nodesIdSelection = FALSE,
      manipulation = FALSE,
      height = "100%",
      width = "100%"
    ) %>%
    visNetwork::visInteraction(
      dragNodes = FALSE,
      navigationButtons = F,
      hideEdgesOnDrag = TRUE,
      zoomSpeed = 0.4
    ) %>%
    visIgraphLayout(
      layout = "layout.norm",
      layoutMatrix = coords,
      type = "full"
    ) %>%
    visEdges(font = list(align = "top", size = VIS$x$edges$font.size)) %>%
    visEvents(
      click = "function(nodes){
                  Shiny.onInputChange('click_dend', nodes.nodes[0]);
                  ;}"
    )

  for (i in 1:nrow(VIS$x$nodes)) {
    if (VIS$x$nodes$group[i] == "group") {
      old_inertia <- as.character(VIS$x$nodes$inertia[i])
      inertia <- as.character(VIS$x$nodes$inertia[i] - h_tail)
      VIS$x$nodes$title[i] <- gsub(old_inertia, inertia, VIS$x$nodes$title[i])
    }
  }

  VIS
}

## Factorial Analysis dynamic plots
ca2plotly <- function(
  CS,
  method = "MCA",
  dimX = 1,
  dimY = 2,
  topWordPlot = Inf,
  threshold = 0.10,
  labelsize = 16,
  size = 5
) {
  LABEL <- CS$WData$word
  switch(
    method,
    CA = {
      contrib <- rowSums(CS$coord$contrib %>% as.data.frame()) / 2
      wordCoord <- CS$coord$coord[, 1:2] %>%
        data.frame() %>%
        mutate(
          label = LABEL,
          contrib = contrib
        ) %>%
        select(c(3, 1, 2, 4))
      row.names(wordCoord) <- LABEL
      xlabel <- paste0("Dim 1 (", round(CS$res$eigCorr$perc[1], 2), "%)")
      ylabel <- paste0("Dim 2 (", round(CS$res$eigCorr$perc[2], 2), "%)")
    },
    MCA = {
      contrib <- rowSums(CS$coord$contrib %>% as.data.frame()) / 2
      wordCoord <- CS$coord$coord[, 1:2] %>%
        data.frame() %>%
        mutate(
          label = LABEL,
          contrib = contrib
        ) %>%
        select(c(3, 1, 2, 4))
      row.names(wordCoord) <- LABEL
      xlabel <- paste0("Dim 1 (", round(CS$res$eigCorr$perc[1], 2), "%)")
      ylabel <- paste0("Dim 2 (", round(CS$res$eigCorr$perc[2], 2), "%)")
    },
    MDS = {
      contrib <- size
      xlabel <- "Dim 1"
      ylabel <- "Dim 2"
      wordCoord <- CS$WData %>%
        data.frame() %>%
        select(1:3) %>%
        mutate(contrib = contrib / 2) %>%
        rename(label = "word")
    }
  )

  dimContrLabel <- paste0("Contrib", c(dimX, dimY))
  ymax <- diff(range((wordCoord[, 3])))
  xmax <- diff(range((wordCoord[, 2])))
  threshold2 <- threshold * mean(xmax, ymax)

  # scaled size for dots
  dotScale <- (wordCoord$contrib) + size

  # Threshold labels to plot
  thres <- sort(dotScale, decreasing = TRUE)[min(topWordPlot, nrow(wordCoord))]

  names(wordCoord)[2:3] <- c("Dim1", "Dim2")

  wordCoord <- wordCoord %>%
    mutate(
      dotSize = dotScale,
      groups = CS$km.res$cluster,
      labelToPlot = ifelse(dotSize >= thres, label, ""),
      font.color = ifelse(
        labelToPlot == "",
        NA,
        adjustcolor(colorlist()[groups], alpha.f = 0.85)
      ),
      font.size = round(dotSize * 2, 0)
    )

  ## Avoid label overlapping
  labelToRemove <- avoidOverlaps(
    wordCoord,
    threshold = threshold2,
    dimX = dimX,
    dimY = dimY
  )
  wordCoord <- wordCoord %>%
    mutate(
      labelToPlot = ifelse(labelToPlot %in% labelToRemove, "", labelToPlot)
    ) %>%
    mutate(
      label = gsub("_1", "", label),
      labelToPlot = gsub("_1", "", labelToPlot)
    )

  hoverText <- paste(
    " <b>",
    wordCoord$label,
    "</b>\n Contribute: ",
    round(wordCoord$contrib, 3),
    sep = ""
  )

  fig <- plot_ly(
    data = wordCoord,
    x = wordCoord[, "Dim1"],
    y = wordCoord[, "Dim2"], # customdata=results$wordCoord,
    type = "scatter",
    mode = "markers",
    marker = list(
      size = dotScale,
      color = adjustcolor(colorlist()[wordCoord$groups], alpha.f = 0.3), #' rgb(79, 121, 66, .5)',
      line = list(
        color = adjustcolor(colorlist()[wordCoord$groups], alpha.f = 0.3), #' rgb(79, 121, 66, .8)',
        width = 2
      )
    ),
    text = hoverText,
    hoverinfo = "text",
    alpha = .3
  )

  fig <- fig %>%
    layout(
      yaxis = list(
        title = ylabel,
        showgrid = TRUE,
        showline = FALSE,
        showticklabels = TRUE,
        domain = c(0, 1)
      ),
      xaxis = list(
        title = xlabel,
        zeroline = TRUE,
        showgrid = TRUE,
        showline = FALSE,
        showticklabels = TRUE
      ),
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    )

  for (i in seq_len(max(wordCoord$groups))) {
    if (method == "MDS") {
      w <- wordCoord %>%
        dplyr::filter(groups == i) %>%
        mutate(
          Dim1 = Dim1 + 0.005,
          Dim2 = Dim2 + 0.005
        )
    } else {
      w <- wordCoord %>%
        dplyr::filter(groups == i) %>%
        mutate(
          Dim1 = Dim1 + dotSize * 0.005,
          Dim2 = Dim2 + dotSize * 0.01
        )
    }

    if (max(CS$hull_data$clust) > 1) {
      hull_df <- CS$hull_data %>% dplyr::filter(clust == i)
      fig <- fig %>%
        add_polygons(
          x = hull_df$Dim1,
          y = hull_df$Dim2,
          inherit = FALSE,
          showlegend = FALSE,
          color = I(hull_df$color[1]),
          opacity = 0.3,
          line = list(width = 2),
          text = paste0("Cluster ", i),
          hoverinfo = "text",
          hoveron = "points"
        )
    }
    fig <- fig %>%
      add_annotations(
        data = w,
        x = ~Dim1,
        y = ~Dim2,
        xref = "x1",
        yref = "y",
        text = ~labelToPlot,
        font = list(
          family = "sans serif",
          size = labelsize,
          color = w$font.color[1]
        ), #' rgb(79, 121, 66)'),
        showarrow = FALSE
      )
  }

  fig <- fig %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        #' toImage',
        "sendDataToCloud",
        "pan2d",
        "select2d",
        "lasso2d",
        "toggleSpikelines",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    ) %>%
    event_register("plotly_selecting")
  return(fig)
}


## function to avoid label overlapping ----
avoidOverlaps <- function(w, threshold = 0.10, dimX = 1, dimY = 2) {
  w[, "Dim2"] <- w[, "Dim2"] / 3

  Ds <- dist(
    w %>%
      dplyr::filter(labelToPlot != "") %>%
      select(Dim1, Dim2),
    method = "manhattan",
    upper = T
  ) %>%
    dist2df() %>%
    rename(
      from = row,
      to = col,
      dist = value
    ) %>%
    left_join(
      w %>% dplyr::filter(labelToPlot != "") %>% select(labelToPlot, dotSize),
      by = c("from" = "labelToPlot")
    ) %>%
    rename(w_from = dotSize) %>%
    left_join(
      w %>% dplyr::filter(labelToPlot != "") %>% select(labelToPlot, dotSize),
      by = c("to" = "labelToPlot")
    ) %>%
    rename(w_to = dotSize) %>%
    filter(dist < threshold)

  st <- TRUE
  i <- 1
  label <- NULL
  case <- "n"

  while (isTRUE(st)) {
    if (Ds$w_from[i] > Ds$w_to[i] & Ds$dist[i] < threshold) {
      case <- "y"
      lab <- Ds$to[i]
    } else if (Ds$w_from[i] <= Ds$w_to[i] & Ds$dist[i] < threshold) {
      case <- "y"
      lab <- Ds$from[i]
    }

    switch(
      case,
      "y" = {
        Ds <- Ds[Ds$from != lab, ]
        Ds <- Ds[Ds$to != lab, ]
        label <- c(label, lab)
      },
      "n" = {
        Ds <- Ds[-1, ]
      }
    )

    if (i >= nrow(Ds)) {
      st <- FALSE
    }
    case <- "n"
    # print(nrow(Ds))
  }

  label
}

## convert a distance object into a data.frame ----
dist2df <- function(inDist) {
  if (class(inDist) != "dist") {
    stop("wrong input type")
  }
  A <- attr(inDist, "Size")
  B <- if (is.null(attr(inDist, "Labels"))) {
    sequence(A)
  } else {
    attr(inDist, "Labels")
  }
  if (isTRUE(attr(inDist, "Diag"))) {
    attr(inDist, "Diag") <- FALSE
  }
  if (isTRUE(attr(inDist, "Upper"))) {
    attr(inDist, "Upper") <- FALSE
  }
  data.frame(
    row = B[unlist(lapply(sequence(A)[-1], function(x) x:A))],
    col = rep(B[-length(B)], (length(B) - 1):1),
    value = as.vector(inDist)
  )
}

### Excel report functions
addDataWb <- function(list_df, wb, sheetname) {
  l <- length(list_df)
  startRow <- 1
  for (i in 1:l) {
    df <- list_df[[i]]
    n <- nrow(df)
    writeDataTable(
      wb,
      sheetname,
      df,
      startRow = startRow,
      startCol = 1,
      tableStyle = "TableStyleMedium20"
    )
    startRow <- startRow + n + 3
  }
  return(wb)
}

addDataScreenWb <- function(list_df, wb, sheetname) {
  ind <- which(regexpr(sheetname, wb$sheet_names) > -1)
  if (length(ind) > 0) {
    sheetname <- paste(sheetname, "(", length(ind) + 1, ")", sep = "")
  }
  addWorksheet(wb = wb, sheetName = sheetname, gridLines = FALSE)
  if (!is.null(list_df)) {
    addDataWb(list_df, wb, sheetname)
    col <- max(unlist(lapply(list_df, ncol))) + 2
  } else {
    col <- 1
  }

  results <- list(wb = wb, col = col, sheetname = sheetname)
  return(results)
}

addGgplotsWb <- function(
  list_plot,
  wb,
  sheetname,
  col,
  width = 10,
  height = 7,
  dpi = 75
) {
  l <- length(list_plot)
  startRow <- 1
  for (i in 1:l) {
    fileName <- tempfile(
      pattern = "figureImage",
      fileext = ".png"
    )
    if (inherits(list_plot[[i]], "ggplot")) {
      ggsave(
        plot = list_plot[[i]],
        filename = fileName,
        width = width,
        height = height,
        units = "in",
        dpi = dpi
      )
    }
    if (inherits(list_plot[[i]], "igraph")) {
      igraph2PNG(
        x = list_plot[[i]],
        filename = fileName,
        width = width,
        height = height,
        dpi = dpi
      )
    }
    if (inherits(list_plot[[i]], "bibliodendrogram")) {
      # print("dendrogram plot")
      # 1. Open jpeg file
      png(
        filename = fileName,
        width = width,
        height = height,
        res = 300,
        units = "in"
      )
      # 2. Create the plot
      plot(list_plot[[i]])
      # 3. Close the file
      dev.off()
    }
    insertImage(
      wb = wb,
      sheet = sheetname,
      file = fileName,
      width = width,
      height = height,
      startRow = startRow,
      startCol = col,
      units = "in",
      dpi = dpi
    )
    startRow <- startRow + (height * 6) + 1
  }
  return(wb)
}

screenSh <- function(p, zoom = 2, type = "vis") {
  tmpdir <- tempdir()
  fileName <- tempfile(
    pattern = "figureImage",
    tmpdir = tmpdir,
    fileext = ".png"
  ) # %>% substr(.,2,nchar(.))

  plot2png(p, filename = fileName, zoom = zoom, type = type, tmpdir = tmpdir)

  return(fileName)
}

screenShot <- function(p, filename, type) {
  home <- homeFolder()

  # setting up the main directory
  # filename <- paste0(file.path(home,"downloads/"),filename)
  if ("downloads" %in% tolower(dir(home))) {
    filename <- paste0(file.path(home, "downloads"), "/", filename)
  } else {
    filename <- paste0(home, "/", filename)
  }

  plot2png(p, filename, zoom = 2, type = type, tmpdir = tempdir())
}

plot2png <- function(p, filename, zoom = 2, type = "vis", tmpdir) {
  html_name <- tempfile(
    fileext = ".html",
    tmpdir = tmpdir
  )
  switch(
    type,
    vis = {
      visSave(p, html_name)
    },
    plotly = {
      htmlwidgets::saveWidget(p, file = html_name)
    }
  )
  biblioShot(url = html_name, zoom = zoom, file = filename) # , verbose=FALSE)

  popUpGeneric(
    title = NULL,
    type = "success",
    color = c("#1d8fe1"),
    subtitle = paste0("Plot was saved in the following path: ", filename),
    btn_labels = "OK",
    size = "40%"
  )
}

addScreenWb <- function(df, wb, width = 14, height = 8, dpi = 75) {
  names(df) <- c("sheet", "file", "n")
  if (nrow(df) > 0) {
    sheet <- unique(df$sheet)
    for (i in 1:length(sheet)) {
      sh <- sheet[i]
      df_sh <- df %>% dplyr::filter(sheet == sh)
      l <- nrow(df_sh)
      startRow <- 1
      for (j in 1:l) {
        fileName <- df_sh$file[j]
        insertImage(
          wb = wb,
          sheet = sh,
          file = fileName,
          width = width,
          height = height,
          startRow = startRow,
          startCol = df_sh$n[j],
          units = "in",
          dpi = dpi
        )
        startRow <- startRow + (height * 10) + 3
      }
    }
  }
  return(wb)
}

addSheetToReport <- function(list_df, list_plot, sheetname, wb, dpi = 75) {
  ind <- which(regexpr(sheetname, wb$sheet_names) > -1)
  if (length(ind) > 0) {
    sheetname <- paste(sheetname, "(", length(ind) + 1, ")", sep = "")
  }
  addWorksheet(wb, sheetname, gridLines = FALSE)

  if (!is.null(list_df)) {
    col <- max(unlist(lapply(list_df, ncol))) + 2
    wb <- addDataWb(list_df, wb = wb, sheetname = sheetname)
  } else {
    col <- 1
  }

  if (!is.null(list_plot)) {
    wb <- addGgplotsWb(
      list_plot,
      wb = wb,
      sheetname = sheetname,
      col = col,
      dpi = dpi
    )
  }
  # values$sheet_name <- sheetname
  return(wb)
}

short2long <- function(df, myC) {
  z <- unlist(lapply(myC, function(x) {
    y <- gsub(r"{\s*\([^\)]+\)}", "", x)
    gsub(y, df$long[df$short == y], x)
  }))
  names(myC) <- z
  return(myC)
}

dfLabel <- function() {
  short <- c(
    "Empty Report",
    "MissingData",
    "MainInfo",
    "AnnualSciProd",
    "AnnualCitPerYear",
    "LifeCycle",
    "ThreeFieldsPlot",
    "MostRelSources",
    "MostLocCitSources",
    "BradfordLaw",
    "SourceLocImpact",
    "SourceProdOverTime",
    "MostRelAuthors",
    "MostLocCitAuthors",
    "AuthorProdOverTime",
    "LotkaLaw",
    "AuthorLocImpact",
    "MostRelAffiliations",
    "AffOverTime",
    "CorrAuthCountries",
    "CountrySciProd",
    "CountryProdOverTime",
    "MostCitCountries",
    "MostGlobCitDocs",
    "MostLocCitDocs",
    "MostLocCitRefs",
    "RPYS",
    "MostFreqWords",
    "WordCloud",
    "TreeMap",
    "WordFreqOverTime",
    "TrendTopics",
    "CouplingMap",
    "CoWordNet",
    "ThematicMap",
    "ThematicEvolution",
    "TE_Period_1",
    "TE_Period_2",
    "TE_Period_3",
    "TE_Period_4",
    "TE_Period_5",
    "FactorialAnalysis",
    "CoCitNet",
    "Historiograph",
    "CollabNet",
    "CollabWorldMap"
  )

  long <- c(
    "Empty Report",
    "Missing Data Table",
    "Main Information",
    "Annual Scientific Production",
    "Annual Citation Per Year",
    "Life Cycle of Publications",
    "Three-Field Plot",
    "Most Relevant Sources",
    "Most Local Cited Sources",
    "Bradfords Law",
    "Sources Local Impact",
    "Sources Production over Time",
    "Most Relevant Authors",
    "Most Local Cited Authors",
    "Authors Production over Time",
    "Lotkas Law",
    "Authors Local Impact",
    "Most Relevant Affiliations",
    "Affiliations Production over Time",
    "Corresponding Authors Countries",
    "Countries Scientific Production",
    "Countries Production over Time",
    "Most Cited Countries",
    "Most Global Cited Documents",
    "Most Local Cited Documents",
    "Most Local Cited References",
    "Reference Spectroscopy",
    "Most Frequent Words",
    "WordCloud",
    "TreeMap",
    "Words Frequency over Time",
    "Trend Topics",
    "Clustering by Coupling",
    "Co-occurence Network",
    "Thematic Map",
    "Thematic Evolution",
    "TE_Period_1",
    "TE_Period_2",
    "TE_Period_3",
    "TE_Period_4",
    "TE_Period_5",
    "Factorial Analysis",
    "Co-citation Network",
    "Historiograph",
    "Collaboration Network",
    "Countries Collaboration World Map"
  )
  data.frame(short = short, long = long)
}

## Generic PopUp
popUpGeneric <- function(
  title = NULL,
  type = "success",
  color = c("#1d8fe1", "#913333", "#FFA800"),
  subtitle = "",
  btn_labels = "OK",
  size = "40%"
) {
  showButton <- TRUE
  timer <- NA
  show_alert(
    title = title,
    text = subtitle,
    type = type,
    size = size,
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    showConfirmButton = showButton,
    showCancelButton = FALSE,
    btn_labels = btn_labels,
    btn_colors = color,
    timer = timer,
    imageUrl = "",
    animation = TRUE
  )
}


## Ad to Report PopUp
popUp <- function(title = NULL, type = "success", btn_labels = "OK") {
  switch(
    type,
    success = {
      title <- paste(title, "\n added to report", sep = "")
      subtitle <- ""
      btn_colors <- "#1d8fe1"
      showButton <- TRUE
      timer <- 3000
    },
    error = {
      title <- "No results to add to the report "
      subtitle <- "Please Run the analysis and then Add it to the report"
      btn_colors <- "#913333"
      showButton <- TRUE
      timer <- 3000
    },
    waiting = {
      title <- "Please wait... "
      subtitle <- "Adding results to report"
      btn_colors <- "#FFA800"
      showButton <- FALSE
      btn_labels <- NA
      timer <- NA
    }
  )

  show_alert(
    title = title,
    text = subtitle,
    type = type,
    size = "s",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    showConfirmButton = showButton,
    showCancelButton = FALSE,
    btn_labels = btn_labels,
    btn_colors = btn_colors,
    timer = timer,
    imageUrl = "",
    animation = TRUE
  )
}

colorlist <- function() {
  c(
    "#E41A1C",
    "#377EB8",
    "#4DAF4A",
    "#984EA3",
    "#FF7F00",
    "#A65628",
    "#F781BF",
    "#999999",
    "#66C2A5",
    "#FC8D62",
    "#8DA0CB",
    "#E78AC3",
    "#A6D854",
    "#FFD92F",
    "#B3B3B3",
    "#A6CEE3",
    "#1F78B4",
    "#B2DF8A",
    "#33A02C",
    "#FB9A99",
    "#E31A1C",
    "#FDBF6F",
    "#FF7F00",
    "#CAB2D6",
    "#6A3D9A",
    "#B15928",
    "#8DD3C7",
    "#BEBADA",
    "#FB8072",
    "#80B1D3",
    "#FDB462",
    "#B3DE69",
    "#D9D9D9",
    "#BC80BD",
    "#CCEBC5"
  )
}

overlayPlotly <- function(VIS) {
  # colorscale_VOS=matrix(c(0, 'rgba(66,65,135,255)', 0.1, 'rgba(34,170,134,255)',
  #                         0.3, 'rgba(202,224,31,255)',
  #                         1, 'rgba(244,227,92,255)'),4,2, byrow=T)

  # colorscale_Our=matrix(c(0, 'rgba(238,238,238,255)',
  #                         0.1, 'rgba(232,202,177,255)',
  #                         0.2, 'rgba(217,137,100,255)',
  #                         0.6, 'rgba(199,107,90,255)',
  #                         0.9, 'rgba(164,38,39,255)',
  #                         1,   'rgba(178,34,34,255)'),
  #                       6,2, byrow=T)

  Reds <- matrix(
    c(
      "0",
      "rgb(255,255,255)",
      "0.05",
      "rgb(238,238,238)",
      "0.125",
      "rgb(254,224,210)",
      "0.25",
      "rgb(252,187,161)",
      "0.375",
      "rgb(252,146,114)",
      "0.5",
      "rgb(251,106,74)",
      "0.625",
      "rgb(239,59,44)",
      "0.75",
      "rgb(203,24,29)",
      "0.875",
      "rgb(165,15,21)",
      "1",
      "rgb(103,0,13)"
    )
  )

  nodes <- VIS$x$nodes %>%
    mutate(
      y = y * (-1),
      font.size = (((font.size - min(font.size)) / diff(range(font.size))) *
        20) +
        10
    )

  colori <- c(
    "Blackbody",
    "Bluered",
    "Blues",
    "Cividis",
    "Earth",
    "Electric",
    "Greens",
    "Greys",
    "Hot",
    "Jet",
    "Picnic",
    "Portland",
    "Rainbow",
    "RdBu",
    "Reds",
    "Viridis",
    "YlGnBu",
    "YlOrRd"
  )

  nodes2 <- nodes %>%
    group_by(id) %>%
    mutate(log = ceiling(log(deg))) %>%
    slice(rep(1, each = log))

  p <- plot_ly(nodes2, x = ~x, y = ~y) %>%
    add_histogram2d(
      histnorm = "density",
      zsmooth = "fast",
      colorscale = Reds,
      # colorscale=colori[15],
      showscale = FALSE
    )

  for (i in 1:nrow(nodes)) {
    p <- p %>%
      add_annotations(
        xref = "x1",
        yref = "y",
        x = nodes$x[i],
        y = nodes$y[i],
        text = nodes$label[i],
        font = list(
          family = "Arial",
          size = nodes$font.size[i],
          color = adjustcolor(nodes$font.color[i], alpha.f = 0.8)
        ),
        showarrow = FALSE
      )
  }
  p <- p %>%
    layout(
      yaxis = list(
        title = "",
        zeroline = FALSE,
        showgrid = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        domain = c(-1, 1),
        gridcolor = "#FFFFFF",
        tickvals = list(NA)
      ),
      xaxis = list(
        title = "",
        zeroline = FALSE,
        showgrid = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        domain = c(-1, 1),
        gridcolor = "#FFFFFF",
        tickvals = list(NA)
      ),
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      showlegend = FALSE
    ) %>%
    style(hoverinfo = "none") %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        #' toImage',
        "sendDataToCloud",
        "pan2d",
        "select2d",
        "lasso2d",
        "toggleSpikelines",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )
  return(p)
}


# MENU VISIBILITY MANAGEMENT ----
# This function updates visibility of submenu items based on metadata availability
updateMenuVisibility <- function(session, values) {
  # Extract metadata availability flags
  TC <- !("TC" %in% values$missTags)
  MLCS <- MLCA <- ("ISI" %in% values$M$DB[1] & !"CR" %in% values$missTags)
  ISI <- ("ISI" %in% values$M$DB[1])
  AFF <- !("C1" %in% values$missTags)
  CR <- !("CR" %in% values$missTags)
  MCC <- (!"TC" %in% values$missTags & !"C1" %in% values$missTags)
  DB_CR <- (sum(c("SCOPUS", "ISI") %in% values$M$DB[1]) > 0)
  DB_TC <- (sum(c("SCOPUS", "ISI", "OPENALEX", "LENS") %in% values$M$DB[1]) > 0)

  # Build JavaScript code to show/hide submenu items based on metadata
  js_code <- "
    // Helper function to find submenu item by text within a parent
    function findSubmenuItem(parentText, itemText) {
      var parent = null;
      $('.sidebar-menu > .treeview').each(function() {
        if ($(this).find('> a > span').first().text().trim() === parentText) {
          parent = $(this);
          return false;
        }
      });
      
      if (parent) {
        var found = null;
        parent.find('ul.treeview-menu > li').each(function() {
          var text = $(this).find('a span').first().text().trim();
          if (text === itemText) {
            found = $(this);
            return false;
          }
        });
        return found;
      }
      return null;
    }
    
    // Helper function to find section header (text-only li)
    function findSectionHeader(parentText, headerText) {
      var parent = null;
      $('.sidebar-menu > .treeview').each(function() {
        if ($(this).find('> a > span').first().text().trim() === parentText) {
          parent = $(this);
          return false;
        }
      });
      
      if (parent) {
        var found = null;
        parent.find('ul.treeview-menu > li').each(function() {
          if ($(this).children('a').length === 0 && $(this).text().trim() === headerText) {
            found = $(this);
            return false;
          }
        });
        return found;
      }
      return null;
    }
  "

  # Add conditional visibility for Sources submenu items
  if (MLCS) {
    js_code <- paste0(
      js_code,
      "\nfindSubmenuItem('Sources', 'Most Local Cited Sources')?.show();"
    )
  } else {
    js_code <- paste0(
      js_code,
      "\nfindSubmenuItem('Sources', 'Most Local Cited Sources')?.hide();"
    )
  }

  if (TC) {
    js_code <- paste0(
      js_code,
      "\nfindSubmenuItem('Sources', 'Sources\\' Local Impact')?.show();"
    )
  } else {
    js_code <- paste0(
      js_code,
      "\nfindSubmenuItem('Sources', 'Sources\\' Local Impact')?.hide();"
    )
  }

  # Add conditional visibility for Authors submenu items
  if (MLCA) {
    js_code <- paste0(
      js_code,
      "\nfindSubmenuItem('Authors', 'Most Local Cited Authors')?.show();"
    )
  } else {
    js_code <- paste0(
      js_code,
      "\nfindSubmenuItem('Authors', 'Most Local Cited Authors')?.hide();"
    )
  }

  if (TC) {
    js_code <- paste0(
      js_code,
      "\nfindSubmenuItem('Authors', 'Authors\\' Local Impact')?.show();"
    )
  } else {
    js_code <- paste0(
      js_code,
      "\nfindSubmenuItem('Authors', 'Authors\\' Local Impact')?.hide();"
    )
  }

  # Affiliations section
  if (AFF) {
    js_code <- paste0(
      js_code,
      "
    findSectionHeader('Authors', 'Affiliations')?.show();
    findSubmenuItem('Authors', 'Most Relevant Affiliations')?.show();
    findSubmenuItem('Authors', 'Affiliations\\' Production over Time')?.show();
    findSectionHeader('Authors', 'Countries')?.show();
    findSubmenuItem('Authors', 'Corresponding Author\\'s Countries')?.show();
    findSubmenuItem('Authors', 'Countries\\' Scientific Production')?.show();
    findSubmenuItem('Authors', 'Countries\\' Production over Time')?.show();
    "
    )
  } else {
    js_code <- paste0(
      js_code,
      "
    findSectionHeader('Authors', 'Affiliations')?.hide();
    findSubmenuItem('Authors', 'Most Relevant Affiliations')?.hide();
    findSubmenuItem('Authors', 'Affiliations\\' Production over Time')?.hide();
    findSectionHeader('Authors', 'Countries')?.hide();
    findSubmenuItem('Authors', 'Corresponding Author\\'s Countries')?.hide();
    findSubmenuItem('Authors', 'Countries\\' Scientific Production')?.hide();
    findSubmenuItem('Authors', 'Countries\\' Production over Time')?.hide();
    "
    )
  }

  if (MCC) {
    js_code <- paste0(
      js_code,
      "\nfindSubmenuItem('Authors', 'Most Cited Countries')?.show();"
    )
  } else {
    js_code <- paste0(
      js_code,
      "\nfindSubmenuItem('Authors', 'Most Cited Countries')?.hide();"
    )
  }

  # Documents submenu items
  if (TC) {
    js_code <- paste0(
      js_code,
      "\nfindSubmenuItem('Documents', 'Most Global Cited Documents')?.show();"
    )
  } else {
    js_code <- paste0(
      js_code,
      "\nfindSubmenuItem('Documents', 'Most Global Cited Documents')?.hide();"
    )
  }

  if (DB_TC && CR && TC) {
    js_code <- paste0(
      js_code,
      "\nfindSubmenuItem('Documents', 'Most Local Cited Documents')?.show();"
    )
  } else {
    js_code <- paste0(
      js_code,
      "\nfindSubmenuItem('Documents', 'Most Local Cited Documents')?.hide();"
    )
  }

  # Cited References section
  if (DB_CR) {
    js_code <- paste0(
      js_code,
      "
    findSectionHeader('Documents', 'Cited References')?.show();
    findSubmenuItem('Documents', 'Most Local Cited References')?.show();
    findSubmenuItem('Documents', 'References Spectroscopy')?.show();
    "
    )
  } else {
    js_code <- paste0(
      js_code,
      "
    findSectionHeader('Documents', 'Cited References')?.hide();
    findSubmenuItem('Documents', 'Most Local Cited References')?.hide();
    findSubmenuItem('Documents', 'References Spectroscopy')?.hide();
    "
    )
  }

  # Intellectual Structure - hide entire menu if no CR
  if (!CR) {
    js_code <- paste0(
      js_code,
      "
    $('.sidebar-menu .treeview').each(function() {
      if ($(this).find('> a > span').first().text().trim() === 'Intellectual Structure') {
        $(this).hide();
      }
    });
    "
    )
  } else {
    js_code <- paste0(
      js_code,
      "
    $('.sidebar-menu .treeview').each(function() {
      if ($(this).find('> a > span').first().text().trim() === 'Intellectual Structure') {
        $(this).show();
      }
    });
    "
    )

    if (DB_TC && CR) {
      js_code <- paste0(
        js_code,
        "\nfindSubmenuItem('Intellectual Structure', 'Historiograph')?.show();"
      )
    } else {
      js_code <- paste0(
        js_code,
        "\nfindSubmenuItem('Intellectual Structure', 'Historiograph')?.hide();"
      )
    }
  }

  # Social Structure
  if (AFF) {
    js_code <- paste0(
      js_code,
      "\nfindSubmenuItem('Social Structure', 'Countries\\' Collaboration World Map')?.show();"
    )
  } else {
    js_code <- paste0(
      js_code,
      "\nfindSubmenuItem('Social Structure', 'Countries\\' Collaboration World Map')?.hide();"
    )
  }

  # Execute the JavaScript code
  shinyjs::runjs(js_code)

  # Update list of disabled menu items for navigation control
  out <- character(0)

  if (!isTRUE(TC)) {
    out <- c(
      out,
      "Average Citations per Year",
      "Sources' Local Impact",
      "Authors' Local Impact",
      "Most Global Cited Documents"
    )
  }
  if (!isTRUE(MLCS)) {
    out <- c(out, "Most Local Cited Sources")
  }
  if (!isTRUE(ISI)) {
    out <- c(out, "Most Local Cited Authors")
  }
  if (!isTRUE(AFF)) {
    out <- c(
      out,
      "Most Relevant Affiliations",
      "Affiliations' Production over Time",
      "Corresponding Author's Countries",
      "Countries' Scientific Production",
      "Countries' Production over Time",
      "Countries' Collaboration World Map"
    )
  }
  if (!isTRUE(MCC)) {
    out <- c(out, "Most Cited Countries")
  }
  if (!(isTRUE(DB_TC) & isTRUE(CR) & isTRUE(TC))) {
    out <- c(out, "Most Local Cited Documents")
  }
  if (!isTRUE(DB_CR)) {
    out <- c(out, "Most Local Cited References", "References Spectroscopy")
  }
  if (!isTRUE(CR)) {
    out <- c(out, "Co-citation Network")
  }
  if (!(isTRUE(DB_TC) & isTRUE(CR))) {
    out <- c(out, "Historiograph")
  }

  values$out <- out
}

# find home folder
homeFolder <- function() {
  switch(
    Sys.info()[["sysname"]],
    Windows = {
      home <- Sys.getenv("R_USER")
    },
    Linux = {
      home <- Sys.getenv("HOME")
    },
    Darwin = {
      home <- Sys.getenv("HOME")
    }
  )
  return(home)
}
