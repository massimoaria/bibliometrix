## gemini AI tools
gemini_ai <- function(
  image = NULL,
  docs = NULL,
  prompt = "Explain these images",
  model = "2.0-flash",
  image_type = "png",
  retry_503 = 5,
  api_key = NULL,
  outputSize = "medium"
) {
  mime_doc_types <- list(
    pdf = "application/pdf",
    txt = "text/plain",
    html = "text/html",
    csv = "text/csv",
    rtf = "text/rtf"
  )

  switch(
    outputSize,
    "small" = {
      generation_config <- list(
        temperature = 1,
        maxOutputTokens = 8192,
        topP = 0.95,
        topK = 40,
        seed = 1234
      )
    },
    "medium" = {
      generation_config <- list(
        temperature = 1,
        maxOutputTokens = 16384, #8192,
        topP = 0.95,
        topK = 40,
        seed = 1234
      )
    },
    "large" = {
      generation_config <- list(
        temperature = 1,
        maxOutputTokens = 32768, #8192,
        topP = 0.95,
        topK = 40,
        seed = 1234
      )
    },
    "huge" = {
      generation_config <- list(
        temperature = 1,
        maxOutputTokens = 131072, #8192,
        topP = 0.95,
        topK = 40,
        seed = 1234
      )
    }
  )

  # # Default config
  # generation_config <- list(
  #   temperature = 1,
  #   maxOutputTokens = 16384,#8192,
  #   topP = 0.95,
  #   topK = 40,
  #   seed = 1234
  # )

  # Build URL
  model_query <- paste0("gemini-", model, ":generateContent")
  url <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    model_query
  )
  if (is.null(api_key)) {
    api_key <- Sys.getenv("GEMINI_API_KEY")
  }

  # Base structure of parts
  parts <- list(list(text = prompt))

  # Handle images if provided
  if (!is.null(image)) {
    if (!is.vector(image)) {
      image <- as.vector(image)
    }
    mime_type <- paste0("image/", image_type)

    for (img_path in image) {
      if (!file.exists(img_path)) {
        return(paste0("❌ Error: Image file does not exist: ", img_path))
      }

      image_data <- tryCatch(
        base64enc::base64encode(img_path),
        error = function(e) {
          return(NULL)
        }
      )

      if (is.null(image_data)) {
        return(paste0("❌ Failed to encode image: ", img_path))
      }

      parts <- append(
        parts,
        list(
          list(
            inline_data = list(
              mime_type = mime_type,
              data = image_data
            )
          )
        )
      )
    }
  }

  # Handle documents if provided
  if (!is.null(docs)) {
    if (!is.vector(docs)) {
      docs <- as.vector(docs)
    }
    for (doc_path in docs) {
      if (!file.exists(doc_path)) {
        return(paste0("❌ Error: Document file does not exist: ", doc_path))
      }

      doc_data <- tryCatch(
        base64enc::base64encode(doc_path),
        error = function(e) {
          return(NULL)
        }
      )

      if (is.null(doc_data)) {
        return(paste0("❌ Failed to encode document: ", doc_path))
      }

      doc_type <- tools::file_ext(doc_path) |> tolower()

      if (doc_type %in% names(mime_doc_types)) {
        mime_type <- mime_doc_types[[doc_type]]
      } else {
        mime_type <- "application/pdf" # Default to PDF if unknown type
      }

      parts <- append(
        parts,
        list(
          list(
            inline_data = list(
              mime_type = "application/pdf",
              data = doc_data
            )
          )
        )
      )
    }
  }

  # Assemble request body
  request_body <- list(
    contents = list(
      parts = parts
    ),
    generationConfig = generation_config
  )

  # Retry loop
  for (attempt in seq_len(retry_503)) {
    # Build and send request
    req <- request(url) |>
      req_url_query(key = api_key) |>
      req_headers("Content-Type" = "application/json") |>
      req_body_json(request_body)

    resp <- tryCatch(
      req_perform(req),
      error = function(e) {
        return(list(
          status_code = stringr::str_extract(e$message, "(?<=HTTP )\\d+") |>
            as.numeric(),
          error = TRUE,
          message = paste("❌ Request failed with error:", e$message)
        ))
      }
    )

    # # Handle connection-level error
    # if (is.list(resp) && isTRUE(resp$error)) {
    #   return(resp$message)
    # }

    # Retry on HTTP 503 or 429
    if (resp$status_code %in% c(429, 503)) {
      if (attempt < retry_503) {
        message(paste0(
          "⚠️ HTTP 503 (Service Unavailable) - retrying in 2 seconds (attempt ",
          attempt,
          "/",
          retry_503,
          ")..."
        ))
        Sys.sleep(2)
        next
      } else {
        return(
          paste0(
            "❌ HTTP 503: Service Unavailable.\n",
            "The Google Gemini servers are currently overloaded or under maintenance.\n",
            "All retry attempts failed (",
            retry_503,
            "). Please try again in a few minutes. Alternatively, consider using a different AI model with lower latency."
          )
        )
      }
    }

    # HTTP errors
    # 400 api key not valid
    if (resp$status_code == 400) {
      msg <- tryCatch(
        {
          parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp))
          parsed$error$message
        },
        error = function(e) {
          "Please check your API key. It seems to be not valid!"
        }
      )
      return(paste0("❌ HTTP ", resp$status_code, ": ", msg))
    }
    # Other HTTP errors
    if (resp$status_code != 200) {
      msg <- tryCatch(
        {
          parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp))
          parsed$error$message
        },
        error = function(e) {
          "Service unavailable or unexpected error. Please check your API key and usage limit."
        }
      )

      return(paste0("❌ HTTP ", resp$status_code, ": ", msg))
    }

    # Successful response
    candidates <- httr2::resp_body_json(resp)$candidates
    outputs <- unlist(lapply(candidates, \(c) c$content$parts))
    return(outputs)
  }
}


setGeminiAPI <- function(api_key) {
  # 1. Controllo validità dell'API key
  apiCheck <- gemini_ai(
    image = NULL,
    prompt = "Hello",
    model = "2.0-flash",
    image_type = "png",
    retry_503 = 5,
    api_key = api_key
  )

  contains_http_error <- grepl("HTTP\\s*[1-5][0-9]{2}", apiCheck)

  if (contains_http_error) {
    return(list(
      valid = FALSE,
      message = "❌ API key seems be not valid! Please, check it or your connection."
    ))
  }

  if (is.null(api_key) || !is.character(api_key) || nchar(api_key) == 0) {
    return(list(
      valid = FALSE,
      message = "❌ API key must be a non-empty string."
    ))
  }

  if (nchar(api_key) < 10) {
    return(list(
      valid = FALSE,
      message = "❌ API key seems too short. Please verify your key."
    ))
  }

  # 2. Mostra solo gli ultimi 4 caratteri per feedback
  last_chars <- 4
  last <- substr(
    api_key,
    max(1, nchar(api_key) - last_chars + 1),
    nchar(api_key)
  )

  # 3. Imposta la variabile d'ambiente
  Sys.setenv(GEMINI_API_KEY = api_key)

  return(list(
    valid = TRUE,
    message = paste0(
      paste0(rep("*", nchar(api_key) - 4), collapse = ""),
      last,
      collapse = ""
    )
  ))
}

showGeminiAPI <- function() {
  api_key <- Sys.getenv("GEMINI_API_KEY")
  last_chars <- 4
  last <- substr(
    api_key,
    max(1, nchar(api_key) - last_chars + 1),
    nchar(api_key)
  )
  last <- paste0(
    paste0(rep("*", nchar(api_key) - 4), collapse = ""),
    last,
    collapse = ""
  )
  return(last)
}

load_api_key <- function(path = path_gemini_key) {
  if (file.exists(path)) {
    key <- readLines(path, warn = FALSE)
    if (nchar(key) >= 10) {
      Sys.setenv(GEMINI_API_KEY = key)
      return(TRUE)
    }
  }
  return(FALSE)
}

loadGeminiModel = function(file) {
  # load info about model type and output size
  if (file.exists(file)) {
    model <- readLines(file, warn = FALSE)
  } else {
    model <- c("2.0-flash", "medium")
  }
  if (length(model == 1)) {
    model <- c(model, "medium")
  }
  return(model)
}

saveGeminiModel = function(model, file) {
  if (file.exists(file)) {
    file.remove(file)
  }
  writeLines(model, file)
}

geminiOutput <- function(title = "", content = "", values) {
  if (is.null(content)) {
    content <- "Click the 'Ask Biblio AI' button to analyze the visual outputs and provide an automatic interpretation of your results based on the graphs.\n
This AI-powered feature leverages Google Gemini to help you understand patterns and insights emerging from your contextual analysis.\n  \n  \n \n  \n  \n  \n"
  }
  box(
    title = title,
    width = 12,
    status = "info",
    solidHeader = TRUE,
    div(
      id = "typing-box",
      style = "white-space: pre-wrap; background-color:#f9f9f9; padding:15px; border:1px solid #ccc; border-radius:5px; max-height:400px; overflow-y: auto;",
      #HTML(text_to_html(content))
      HTML(gemini_to_html(content))
    ),
    br(),
    em(
      "You can modify or enrich the proposed prompt with additional context or details about your analysis to help 'Biblio AI' generate a more accurate and meaningful interpretation."
    ),
    textAreaInput(
      inputId = "gemini_additional",
      label = NULL,
      value = values$gemini_model_parameters,
      placeholder = "You can provide additional context or details about your analysis to help 'Biblio AI' generate a more accurate and meaningful interpretation.",
      rows = 3,
      width = "100%"
    ),
    fluidRow(
      column(
        4,
        align = "center",
        actionButton(
          "gemini_btn",
          "Ask Biblio AI",
          style = "color: white;",
          icon(name = "microchip", lib = "font-awesome"),
          width = "80%"
        )
      ),
      column(
        4,
        align = "center",
        actionButton(
          "copy_btn",
          "Copy",
          style = "color: white;",
          icon = icon("clipboard"),
          width = "80%"
        )
      ),
      column(
        4,
        align = "center",
        downloadButton(
          outputId = "save_btn",
          label = "Save",
          icon = icon("download"),
          style = "width: 80%;"
        )
      )
    )
  )
}

biblioAiPrompts <- function(values, activeTab) {
  ## ROle definition for Gemini as Biblio AI assistant
  promptInitial <- "You are Biblio AI, an AI assistant integrated within Biblioshiny. Your task is to support researchers in interpreting and critically discussing the results of their bibliometric analyses, offering insights, contextual explanations, and guidance for data-driven interpretation. "

  switch(
    activeTab,
    "mainInfo" = {
      prompt <- paste0(
        "Provide an interpretation of these statistics summarizing the bibliographic collection. ",
        "Focus on key metrics such as the number of documents, sources, authors, and citations, ",
        "and discuss what they reveal about the scope, productivity, and impact of the collection. ",
        "This is the list of statistics: ",
        merge_df_to_string(values$TABvb)
      )
    },
    "lifeCycle" = {
      prompt <- paste0(
        "Analyze the provided 'life cycle' plots showing the bibliographic collection's scientific production fitted to a logistic growth model. ",
        "You have been provided with: (1) an ANNUAL plot showing yearly publication counts, (2) a CUMULATIVE plot showing the total publications over time, ",
        "and (3) the logistic model parameters and goodness-of-fit metrics.\n\n",

        "Please provide a comprehensive interpretation covering:\n",
        "1. GROWTH PATTERN: Describe the overall growth trajectory (exponential, linear, or saturation phase). ",
        "Identify the inflection point where growth rate changes.\n",
        "2. MODEL FIT: Evaluate how well the logistic curve fits the observed data based on the provided metrics (R², RMSE, etc.). ",
        "Comment on any systematic deviations.\n",
        "3. KEY PARAMETERS: Interpret the logistic model parameters:\n",
        "   - K (carrying capacity): the estimated maximum cumulative production\n",
        "   - r (growth rate): the intrinsic rate of increase\n",
        "   - t₀ (inflection point): when maximum growth rate occurred\n",
        "4. TRENDS AND PHASES: Identify distinct phases in the publication timeline:\n",
        "   - Emergence phase (slow initial growth)\n",
        "   - Expansion phase (rapid growth)\n",
        "   - Maturity/saturation phase (if present)\n",
        "5. SCIENTIFIC IMPLICATIONS: Discuss what these patterns suggest about the research field's maturity, ",
        "potential factors influencing growth (e.g., funding, technological advances, paradigm shifts), ",
        "and future trajectories. Is the field still growing or approaching saturation?\n\n",

        "Provide your analysis in a structured format with clear sections.",
        "This a the list of statistics: Parameters ",
        merge_df_to_string(values$DLC$parameters),
        ", parameters_real_years ",
        merge_df_to_string(values$DLC$parameters_real_years),
        ", and goodness_of_fit ",
        merge_df_to_string(values$DLC$metrics)
      )
    },
    "threeFieldPlot" = {
      prompt <- paste0(
        "Provide an interpretation of this Three-Field Plot. The central field represents the target metadata,",
        " while the left and the right fields are the destination metadata. The plot visualizes the connections",
        " among these three dimensions by linking the target field witgh the two other metadata.",
        " Analyze the structure of the network, and describe how these elements are interconnected."
      )
    },
    "authorsProdOverTime" = {
      docs <- apot2Docs(values$AUProdOverTime, n = 3) %>% merge_df_to_string()
      prompt <- paste0(
        "Provide an interpretation of this 'Authors’ Production Over Time' plot, which displays bibliometric trends",
        " for the top authors in the field. For each author, the red horizontal line represents their scientific timeline,",
        " indicating the span of their active publishing years. Each bubble corresponds to a publication year: its size reflects",
        " the number of articles published that year, while the color intensity represents the total number of citations per year (TC/year).",
        " Analyze the evolution of individual authors’ productivity and impact, identifying key periods of activity, citation peaks, ",
        "and possible shifts in research influence over time. A list of the most three cited-per-year articles for each author is provided: ",
        docs
      )
    },
    "correspAuthorCountry" = {
      countries <- merge_df_to_string(country2collab(values$TABCo))
      prompt <- paste0(
        "Provide an interpretation of this 'Corresponding Author’s Country Collaboration Plot'. The plot displays the number of scientific publications",
        " by corresponding authors for each country, distinguishing between Single Country Publications (SCP) and Multiple Country Publications (MCP). ",
        "SCP represents articles authored exclusively within one country, while MCP refers to collaborative works involving authors from different countries. ",
        "The MCP Ratio, calculated as MCP divided by the total number of publications, indicates the extent of international collaboration. ",
        "Focus on identifying the most productive countries, comparing their levels of international collaboration, and discussing the balance between domestic ",
        "and global research engagement. A list of the most 20 frequent countries is provided: ",
        countries
      )
    },
    "mostLocalCitDoc" = {
      docs <- localCit2docs(values$TABLocDoc, n = 20) %>% merge_df_to_string()
      prompt <- paste0(
        "Provide an interpretation of this table reporting the most locally and globally cited articles in the collection. Global Citations (GC) represent the total",
        " number of citations a document has received across all records indexed in the bibliographic database, indicating its overall scholarly impact. Local Citations (LC),",
        " on the other hand, count the number of citations a document has received within this specific dataset, reflecting its relevance to the focused research field. ",
        "Both indicators are normalized by NTC (Normalized Total Citations), which adjusts citation counts relative to the average number of citations for publications from the same year.",
        " This normalization allows for fair comparison across time. Focus on identifying articles with strong global influence, local relevance, or both.",
        "A list of the most 20 local cited articles is provided: ",
        docs
      )
    },
    "trendTopic" = {
      prompt <- paste0(
        "Provide an interpretation of this trend topics plot, where each year is associated with the k words that have the highest annual median frequency. ",
        "For each word, the light blue line represents the interquartile range (from the first to the third quartile) of its frequency time series, and the central",
        "point marks the median value. The size of the bubbles is proportional to the annual frequency, reflecting the relative prominence of each term over time. "
      )
    },
    "ReferenceSpect" = {
      references <- merge_df_to_string(values$res$peaks)
      sequenceTop5 <- values$res$Sequences %>%
        filter(Class != "") %>%
        group_by(Class) %>%
        slice_max(Freq, n = 5) %>%
        rename("Totale Citation" = "Freq")
      prompt <- paste0(
        "Provide an interpretation of this Reference Publication Year Spectroscopy plot (RPYS, Marx et al. 2014, JAIST).",
        " The black line shows the number of cited references by publication year. ",
        "The red line represents the deviation from the 5-year median citation frequency.",
        " This highlights peak years of historical significance. A list of the most cited references is provided for the first ten peak years identified by the red line,",
        " along with their citation frequencies: ",
        references,
        "\nIn addition, please highlight which references are the most important according to the following categories: Hot Paper, Life Cycle, Sleeping Beauty,",
        "
                         and Constant Performer (Thor A. et al. 2018, Identifying single influential publications in a research field: new analysis opportunities of the CRExplorer. Scientometrics).",
        " Below is the list of references with their assigned category.",
        sequenceTop5
      )
    },
    "coOccurenceNetwork" = {
      prompt <- paste0(
        "Provide an interpretation of this 'word co-occurrence' network.",
        " Focus on the structure of the network, the presence of communities (topics), and the relevance of the most connected terms."
      )
    },
    "thematicMap" = {
      docs <- merge_df_to_string(doc2clust(values$TM))
      prompt <- paste0(
        "Provide an interpretation of this 'strategic map'",
        "Here there is the list of three most central articles for each cluster: ",
        docs,
        ". Focus on the structure of the strategic map, the description of clusters and their centrality and density measures, and the description of the articles belonging to them."
      )
    },
    "thematicEvolution" = {
      prompt <- paste0(
        "Provide an interpretation of these plots.",
        "The first plot illustrates topic evolution across different time periods, showing how thematic areas develop or shift over time. ",
        "The other plots display strategic maps for each period, highlighting the positioning of topics based on centrality and density. ",
        "Focus on identifying emerging, declining, or stable topics and their strategic importance."
      )
    },
    "factorialAnalysis" = {
      prompt <- "Provide an interpretation of this 'factorial map'. Focus on the structure of the map, the presence of clusters, and the relevance of the most contributing terms."
    },
    "coCitationNetwork" = {
      prompt <- "Provide an interpretation of this 'co-citation' network. Focus on the structure of the network, the presence of communities, and the relevance of the most connected terms."
    },
    "historiograph" = {
      titles <- hist2docs(values$histPlotVis$VIS$x$nodes) %>%
        merge_df_to_string()
      prompt <- paste0(
        "Interpret this historiograph, a temporal citation network built by mapping direct citation links among documents. ",
        "Highlight the main citation paths, pivotal works, and any notable temporal trends in knowledge development, looking also to the article titles and their topics.",
        " Focus on the temporal evolution of each cluster",
        "Here there is the list of each paper (node) and its title: ",
        titles
      )
    },
    "collabNetwork" = {
      prompt <- paste0(
        "Provide an interpretation of this 'collaboration' network",
        ". Focus on the structure of the network, the presence of communities, and the relevance of the most connected terms."
      )
    },
    "collabWorldMap" = {
      #values$WMGemini
      prompt <- paste0(
        "Provide an interpretation of this 'Countries’ Collaboration World Map'. The map visualizes international scientific ",
        "collaboration by showing, for each country, the total number of articles with at least one contributing author. ",
        "The color intensity of each country is proportional to its research output. The connecting lines represent collaborative ",
        "links between countries, based on co-authorship across all authors (not only corresponding authors). ",
        "Focus on identifying major hubs of scientific production, key international partnerships, and global patterns of collaboration."
      )
    },
    {
      prompt <- paste0(
        "Provide an interpretation of this plot creted with 'bibliometrix R Package'"
      )
    }
  )
  prompt <- paste0(promptInitial, prompt)
  #if (!activeTab %in% c("mainInfo", "thematicMap", "trendTopic")) prompt <- paste0(prompt, " Provide also scientific references about the methodological description")
  return(prompt)
}

geminiGenerate <- function(
  values,
  activeTab,
  gemini_additional,
  gemini_model_parameters,
  input
) {
  if (gemini_additional != "") {
    desc <- paste0(
      values$collection_description,
      gemini_additional,
      gemini_model_parameters,
      collapse = ". "
    )
  } else {
    desc <- paste0(
      values$collection_description,
      gemini_model_parameters,
      collapse = ". "
    )
  }
  prompt <- biblioAiPrompts(values, activeTab)
  switch(
    activeTab,
    "mainInfo" = {
      req(values$TABvb)
      values$MainInfoGemini <- geminiPromptImage(
        obj = NULL,
        type = "text",
        prompt = prompt,
        key = values$geminiAPI,
        desc = desc,
        values = values
      )
    },
    "lifeCycle" = {
      req(values$DLC)
      files <- DLC2Gemini(values$DLCplotYear, values$DLCplotCum)
      values$DLCGemini <- geminiPromptImage(
        obj = files,
        type = "multi",
        prompt = prompt,
        key = values$geminiAPI,
        desc = desc,
        values = values
      )
    },
    "threeFieldPlot" = {
      req(values$TFP)
      values$TFPGemini <- geminiPromptImage(
        obj = values$TFP,
        type = "plotly",
        prompt = prompt,
        key = values$geminiAPI,
        desc = desc,
        values = values
      )
    },
    "authorsProdOverTime" = {
      req(values$AUProdOverTime)
      values$ApotGemini <- geminiPromptImage(
        obj = values$AUProdOverTime$graph,
        type = "ggplot2",
        prompt = prompt,
        key = values$geminiAPI,
        desc = desc,
        values = values
      )
    },
    "correspAuthorCountry" = {
      req(values$TABCo)
      values$MostRelCountriesGemini <- geminiPromptImage(
        obj = values$MRCOplot,
        type = "ggplot2",
        prompt = prompt,
        key = values$geminiAPI,
        desc = desc,
        values = values
      )
    },
    "mostLocalCitDoc" = {
      req(values$TABLocDoc)
      values$MostLocCitDocsGemini <- geminiPromptImage(
        obj = NULL,
        type = "text",
        prompt = prompt,
        key = values$geminiAPI,
        desc = desc,
        values = values
      )
    },
    "trendTopic" = {
      req(values$trendTopics)
      values$trendTopicsGemini <- geminiPromptImage(
        obj = values$trendTopics$graph,
        type = "ggplot2",
        prompt = prompt,
        key = values$geminiAPI,
        desc = desc,
        values = values
      )
    },
    "ReferenceSpect" = {
      req(values$res)
      values$rpysGemini <- geminiPromptImage(
        obj = values$res$spectroscopy,
        type = "ggplot2",
        prompt = prompt,
        key = values$geminiAPI,
        desc = desc,
        values = values
      )
    },
    "coOccurenceNetwork" = {
      req(values$COCnetwork)
      values$cocGemini <- geminiPromptImage(
        obj = values$COCnetwork$VIS,
        type = "vis",
        prompt = prompt,
        key = values$geminiAPI,
        desc = desc,
        values = values
      )
    },
    "thematicMap" = {
      req(values$TMmap)
      values$TMGemini <- geminiPromptImage(
        obj = values$TMmap,
        type = "plotly",
        prompt = prompt,
        key = values$geminiAPI,
        desc = desc,
        values = values
      )
    },
    "thematicEvolution" = {
      req(values$nexus)
      files <- TE2Gemini(values$nexus, values$TEplot)
      values$TEGemini <- geminiPromptImage(
        obj = files,
        type = "multi",
        prompt = prompt,
        key = values$geminiAPI,
        desc = desc,
        values = values
      )
    },
    "factorialAnalysis" = {
      req(values$plotCS)
      values$CSGemini <- geminiPromptImage(
        obj = values$plotCS,
        type = "plotly",
        prompt = prompt,
        key = values$geminiAPI,
        desc = desc,
        values = values
      )
    },
    "coCitationNetwork" = {
      req(values$COCITnetwork)
      values$cocitGemini <- geminiPromptImage(
        obj = values$COCITnetwork$VIS,
        type = "vis",
        prompt = prompt,
        key = values$geminiAPI,
        desc = desc,
        values = values
      )
    },
    "historiograph" = {
      req(values$histPlotVis$VIS)
      values$histGemini <- geminiPromptImage(
        obj = values$histPlotVis$VIS,
        type = "vis",
        prompt = prompt,
        key = values$geminiAPI,
        desc = desc,
        values = values
      )
    },
    "collabNetwork" = {
      req(values$COLnetwork$VIS)
      values$colGemini <- geminiPromptImage(
        obj = values$COLnetwork$VIS,
        type = "vis",
        prompt = prompt,
        key = values$geminiAPI,
        desc = desc,
        values = values
      )
    },
    "collabWorldMap" = {
      values$WMGemini <- geminiPromptImage(
        obj = values$WMmap$g,
        type = "ggplot2",
        prompt = prompt,
        key = values$geminiAPI,
        desc = desc,
        values = values
      )
    }
  )
  return(values)
}

geminiParameterPrompt <- function(values, activeTab, input) {
  if ("DB_Original" %in% names(values$M)) {
    DB <- unique(values$M$DB_Original)
    if (length(DB) > 1) {
      DB[length(DB)] <- paste0("and ", DB[length(DB)])
    }
    DB <- gsub("ISI", "WOS", paste0(DB, collapse = ", "))

    DB <- paste0(
      "databases ",
      DB,
      " merged by Bibliometrix R package routines."
    )
  } else {
    if (values$M$DB[1] == "ISI") {
      DB <- "WOS"
    } else {
      DB <- values$M$DB[1]
    }
    DB <- paste0("database ", DB, ".")
  }
  txt <- paste0(
    "The analysis was performed on a collection downloaded from the following bibliographic ",
    DB
  )
  missingTags <- values$missingdf %>%
    filter(status %in% "Completely missing") %>%
    pull(description)
  if (length(missingTags) > 0) {
    if ("DB_Original" %in% names(values$M)) {
      txt1 <- paste0(
        " Some metadata fields like ",
        paste0(missingTags, collapse = ", "),
        " are missing due to limitations in the merging process and 
                                                           non-standard formats across databases."
      )
      txt <- paste0(txt, txt1)
    }
  }

  switch(
    activeTab,
    "mainInfo" = {
      req(values$TABvb)
    },
    "lifeCycle" = {
      req(values$DLC)
    },
    "threeFieldPlot" = {
      req(values$TFP)
      txt <- paste0(
        txt,
        " This graph was generated with the following parameters: target field '",
        input$CentralField,
        "', right field '",
        input$RightField,
        "', and left field '",
        input$LeftField,
        "'."
      )
    },
    "trendTopic" = {
      req(values$trendTopics)
      txt <- paste0(
        txt,
        " This graph was generated with the following parameters: ",
        merge_df_to_string(values$trendTopics$params)
      )
    },
    "authorsProdOverTime" = {
      req(values$AUProdOverTime)
    },
    "correspAuthorCountry" = {
      req(values$TABCo)
    },
    "mostLocalCitDoc" = {
      req(values$TABLocDoc)
    },
    "ReferenceSpect" = {
      req(values$res)
    },
    "coOccurenceNetwork" = {
      req(values$COCnetwork)
      txt <- paste0(
        txt,
        " This graph was generated with the following parameters: ",
        merge_df_to_string(values$cocnet$params)
      )
    },
    "thematicMap" = {
      req(values$TMmap)
      txt <- paste0(
        txt,
        "  This graph was generated with the following parameters: ",
        merge_df_to_string(values$TM$params)
      )
    },
    "thematicEvolution" = {
      req(values$nexus)
      txt <- paste0(
        txt,
        " The following parameters were used for each strategic map: ",
        merge_df_to_string(values$nexus$TM[[1]]$params)
      )
    },
    "factorialAnalysis" = {
      req(values$plotCS)
      txt <- paste0(
        txt,
        "  This graph was generated with the following parameters: ",
        merge_df_to_string(values$CS$params)
      )
    },
    "coCitationNetwork" = {
      req(values$COCITnetwork)
      txt <- paste0(
        txt,
        "  This graph was generated with the following parameters: ",
        merge_df_to_string(values$cocitnet$params)
      )
    },
    "historiograph" = {
      req(values$histPlotVis$VIS)
    },
    "collabNetwork" = {
      req(values$COLnetwork$VIS)
      txt <- paste0(
        txt,
        "  This graph was generated with the following parameters: ",
        merge_df_to_string(values$colnet$params)
      )
    },
    "collabWorldMap" = {
      req(values$WMmap)
    }
  )
  return(txt)
}

## gemini prompt for images
geminiPromptImage <- function(
  obj,
  type = "vis",
  prompt = "Explain the topics in this map",
  key,
  desc = NULL,
  values
) {
  ## Check Computer configuration to work with Biblio AI
  ### Internet Connection
  if (!is_online()) {
    res <- '⚠️ **Note**: Biblio AI requires an active internet connection to work.'
    return(res)
  }
  ### Chromium Browser
  if (is.null(values$Chrome_url)) {
    res <- '⚠️ **Note**: Biblio AI requires a **Chrome-based browser** (such as Google Chrome or Microsoft Edge) installed on your computer to work correctly.'
    return(res)
  }

  if (key) {
    if (!is.null(desc)) {
      prompt <- paste0(prompt, desc, collapse = ". ")
    }
    tmpdir <- tempdir()
    owd <- setwd(tmpdir)
    on.exit(setwd(owd))
    file_path <- paste0(tempfile(), ".png", collapse = "")
    switch(
      type,
      "vis" = {
        plot2pngGemini(obj, filename = file_path, type = "vis")
      },
      "plotly" = {
        plot2pngGemini(obj, filename = file_path, type = "plotly")
      },
      "multi" = {
        # prompt with multiple image files
        file_path <- obj
      },
      "text" = {
        # prompt based only on text
        file_path <- NULL
      },
      "ggplot2" = {
        ggsave(
          filename = file_path,
          plot = obj,
          dpi = 72,
          height = 7,
          width = 14,
          bg = "transparent"
        )
      }
    )

    res <- gemini_ai(
      image = file_path,
      prompt = prompt,
      image_type = "png",
      model = values$gemini_api_model,
      outputSize = values$gemini_output_size
    )
  } else {
    res <- 'To access this feature, please provide a valid Gemini AI API key. You can obtain your API key by visiting the official <a href="https://aistudio.google.com/" target="_blank">Google AI Studio website</a>.'
  }

  return(res)
}

geminiWaitingMessage <- function(values, activeTab) {
  messageTxt <- "⌛ Thinking..."

  switch(
    activeTab,
    "mainInfo" = {
      req(values$TABvb)
      values$MainInfoGemini <- messageTxt
    },
    "lifeCycle" = {
      req(values$DLC)
      values$DLCGemini <- messageTxt
    },
    "threeFieldPlot" = {
      req(values$TFP)
      values$TFPGemini <- messageTxt
    },
    "authorsProdOverTime" = {
      req(values$AUProdOverTime)
      values$ApotGemini <- messageTxt
    },
    "correspAuthorCountry" = {
      req(values$TABCo)
      values$MostRelCountriesGemini <- messageTxt
    },
    "mostLocalCitDoc" = {
      req(values$TABLocDoc)
      values$MostLocCitDocsGemini <- messageTxt
    },
    "trendTopic" = {
      req(values$trendTopics)
      values$trendTopicsGemini <- messageTxt
    },
    "ReferenceSpect" = {
      req(values$res)
      values$rpysGemini <- messageTxt
    },
    "coOccurenceNetwork" = {
      req(values$COCnetwork)
      values$cocGemini <- messageTxt
    },
    "thematicMap" = {
      req(values$TM)
      values$TMGemini <- messageTxt
    },
    "thematicEvolution" = {
      req(values$nexus)
      values$TEGemini <- messageTxt
    },
    "factorialAnalysis" = {
      req(values$plotCS)
      values$CSGemini <- messageTxt
    },
    "coCitationNetwork" = {
      req(values$COCITnetwork)
      values$cocitGemini <- messageTxt
    },
    "historiograph" = {
      req(values$histPlotVis$VIS)
      values$histGemini <- messageTxt
    },
    "collabNetwork" = {
      req(values$COLnetwork$VIS)
      values$colGemini <- messageTxt
    },
    "collabWorldMap" = {
      req(values$WMmap)
      values$WMGemini <- messageTxt
    }
  )
  return(values)
}

geminiSave <- function(values, activeTab) {
  switch(
    activeTab,
    "mainInfo" = {
      gemini <- values$MainInfoGemini
    },
    "lifeCycle" = {
      gemini <- values$DLCGemini
    },
    "threeFieldPlot" = {
      gemini <- values$TFPGemini
    },
    "authorsProdOverTime" = {
      gemini <- values$ApotGemini
    },
    "correspAuthorCountry" = {
      gemini <- values$MostRelCountriesGemini
    },
    "mostLocalCitDoc" = {
      gemini <- values$MostLocCitDocsGemini
    },
    "trendTopic" = {
      gemini <- values$trendTopicsGemini
    },
    "ReferenceSpect" = {
      gemini <- values$rpysGemini
    },
    "coOccurenceNetwork" = {
      gemini <- values$cocGemini
    },
    "thematicMap" = {
      gemini <- values$TMGemini
    },
    "thematicEvolution" = {
      gemini <- values$TEGemini
    },
    "factorialAnalysis" = {
      gemini <- values$CSGemini
    },
    "coCitationNetwork" = {
      gemini <- values$cocitGemini
    },
    "historiograph" = {
      gemini <- values$histGemini
    },
    "collabNetwork" = {
      gemini <- values$colGemini
    },
    "collabWorldMap" = {
      gemini <- values$WMGemini
    }
  )
  if (is.null(gemini)) {
    gemini <- "Click 'Ask Biblio AI' for help. "
  }
  return(gemini)
}

## Summarize document with Gemini AI ---
summaryAI <- function(values, i, model) {
  # Construct the prompt for Gemini AI
  prompt <- paste0(
    "Summarize the content of the following publication",
    " in no more than 250 words. ",
    "Title: ",
    values$M$TI[i],
    "Authors: ",
    values$M$AU[i],
    "Journal: ",
    values$M$SO[i],
    "Publication Year: ",
    values$M$PY[i],
    "Abstract: ",
    values$M$AB[i],
    "DOI: ",
    values$M$DI[i],
    "Try to access to article website using its DOI and extract more information about the article content. ",
    "If the article is open access, try to extract the full text and summarize it. ",
    "If the article is not open access, try to extract more information from the article website. ",
    "If you cannot access to the article website, just summarize the content using the title and the abstract. ",
    "Do not mention that you are an AI model. ",
    "Do not mention that you cannot access to the article website. ",
    "Do not mention that you do not have access to the full text. ",
    "Just provide the summary. "
  )

  # Gemini AI call
  res <- tryCatch(
    {
      gemini_ai(
        image = NULL,
        prompt = prompt,
        model = model,
        image_type = "png",
        retry_503 = 2,
        api_key = NULL
      )
    },
    error = function(e) {
      "Error generating automatic summary."
    }
  )

  return(res)
}


## Normalize abstract text
normalize_uppercase_text <- function(text) {
  # Check that input is a character string
  if (!is.character(text) || length(text) != 1) {
    stop("Input must be a character string")
  }

  # Convert everything to lowercase
  text_lower <- tolower(text)

  # Capitalize the first letter of the text
  text_lower <- paste0(
    toupper(substring(text_lower, 1, 1)),
    substring(text_lower, 2)
  )

  # Pattern to identify end of sentence followed by space and letter
  # Considers period, exclamation mark, question mark
  text_normalized <- gsub(
    "([.!?])\\s+([a-z])",
    "\\1 \\U\\2",
    text_lower,
    perl = TRUE
  )

  # Handle common abbreviations that should not be followed by uppercase
  # Examples: Dr., Prof., etc., vs., i.e., e.g.
  common_abbrev <- c(
    "dr\\.",
    "prof\\.",
    "etc\\.",
    "vs\\.",
    "i\\.e\\.",
    "e\\.g\\.",
    "mr\\.",
    "mrs\\.",
    "ms\\.",
    "jr\\.",
    "sr\\.",
    "phd\\."
  )

  for (abbrev in common_abbrev) {
    # Convert to lowercase the letters after common abbreviations
    pattern <- paste0("(", abbrev, ")\\s+([A-Z])")
    text_normalized <- gsub(
      pattern,
      "\\1 \\L\\2",
      text_normalized,
      perl = TRUE,
      ignore.case = TRUE
    )
  }

  # Capitalize after colons (optional - common in academic titles)
  text_normalized <- gsub(
    "(:)\\s+([a-z])",
    "\\1 \\U\\2",
    text_normalized,
    perl = TRUE
  )

  return(text_normalized)
}

## To title upper case metadata fields
to_title_case <- function(texts, exclude_articles = TRUE) {
  # --- Input Validation ---
  # Check that the input is a character vector. If not, stop and show an error.
  if (!is.character(texts)) {
    stop("Input must be a character vector.")
  }

  # --- Configuration ---
  # Define a list of "small words" (articles, prepositions, conjunctions) that
  # should remain in lowercase unless they are the first or last word in the title.
  small_words <- c(
    "a",
    "an",
    "and",
    "as",
    "at",
    "but",
    "by",
    "for",
    "if",
    "in",
    "of",
    "on",
    "or",
    "the",
    "to",
    "up",
    "via",
    "with",
    "from",
    "into",
    "over",
    "upon",
    "down",
    "off",
    "out",
    "per",
    "than",
    "through",
    "under",
    "within",
    "without",
    "about",
    "across",
    "after",
    "against",
    "along",
    "among",
    "around",
    "before",
    "behind",
    "below",
    "beneath",
    "beside",
    "between",
    "beyond",
    "during",
    "except",
    "inside",
    "near",
    "since",
    "toward",
    "towards",
    "until",
    "where",
    "while"
  )

  # --- Helper Function ---
  # A helper function to capitalize the first letter of a single word.
  # It also handles hyphenated words (e.g., "state-of-the-art" -> "State-of-the-Art").
  capitalize_first <- function(word) {
    # Check for hyphens
    if (grepl("-", word)) {
      # If a hyphen exists, split the word into parts
      parts <- strsplit(word, "-")[[1]]
      # Capitalize the first letter of each part
      capitalized_parts <- sapply(parts, function(part) {
        if (nchar(part) > 0) {
          paste0(toupper(substring(part, 1, 1)), substring(part, 2))
        } else {
          part # Return empty parts as they are
        }
      })
      # Re-join the parts with a hyphen
      return(paste(capitalized_parts, collapse = "-"))
    } else {
      # If no hyphen, just capitalize the first letter of the word
      paste0(toupper(substring(word, 1, 1)), substring(word, 2))
    }
  }

  # --- Main Processing Logic ---
  # Use sapply() to apply the title-casing logic to each element of the input vector 'texts'.
  # 'sapply' iterates over the vector and collects the results into a new vector.
  # USE.NAMES = FALSE ensures the output is an unnamed vector.
  result_vector <- sapply(
    texts,
    function(single_text) {
      # Handle edge cases: return NA or empty strings without modification.
      if (is.na(single_text) || nchar(single_text) == 0) {
        return(single_text)
      }

      # Convert the entire string to lowercase to ensure consistency.
      text_lower <- tolower(single_text)

      # Split the string into a vector of words based on whitespace.
      words <- strsplit(text_lower, "\\s+")[[1]]

      # Apply capitalization based on the 'exclude_articles' flag.
      if (exclude_articles) {
        # If TRUE, apply capitalization selectively.
        capitalized_words <- sapply(seq_along(words), function(i) {
          word <- words[i]
          # Always capitalize the first word, the last word, or any word NOT in the 'small_words' list.
          if (i == 1 || i == length(words) || !word %in% small_words) {
            capitalize_first(word)
          } else {
            word # Otherwise, keep the small word in lowercase.
          }
        })
      } else {
        # If FALSE, capitalize every word.
        capitalized_words <- sapply(words, capitalize_first)
      }

      # Recombine the processed words into a single string, separated by spaces.
      paste(capitalized_words, collapse = " ")
    },
    USE.NAMES = FALSE
  )

  # Return the final vector of title-cased strings.
  return(result_vector)
}


merge_df_to_string <- function(df) {
  # Check if the input has at least two columns
  if (ncol(df) < 2) {
    stop("The data frame must have at least two columns.")
  }

  # Ensure the input is a data frame
  df <- as.data.frame(df)

  # Convert each row into a "param: value" format
  row_strings <- apply(df[, 1:2], 1, function(row) {
    paste0(row[1], ": ", row[2])
  })

  # Concatenate all row strings using "; " as separator
  final_string <- paste(row_strings, collapse = "; ")

  return(final_string)
}

plot2pngGemini <- function(p, filename, zoom = 2, type = "vis") {
  html_name <- tempfile(fileext = ".html")
  switch(
    type,
    vis = {
      visSave(p, html_name)
    },
    plotly = {
      htmlwidgets::saveWidget(p, file = html_name)
    }
  )

  biblioShot(html_name, zoom = zoom, file = filename)
}

## save all plots of the life cycle analysis
DLC2Gemini <- function(DLCplotYear, DLCplotCum) {
  plots <- c("annual", "cumulative")
  files <- unlist(lapply(plots, function(x) {
    paste0(tempdir(), "/plotDLC_", x, ".png")
  }))

  plot2pngGemini(DLCplotYear, filename = files[1], type = "plotly")
  plot2pngGemini(DLCplotCum, filename = files[2], type = "plotly")

  return(files)
}

## save all plots of the thematic evolution analysis
TE2Gemini <- function(nexus, plotTE) {
  K <- length(nexus$TM)
  periods <- nexus$Nodes %>% select(group) %>% distinct() %>% pull()
  files <- unlist(lapply(periods, function(x) {
    paste0(tempdir(), "/", x, "_period.png")
  }))
  files <- c(paste0(tempdir(), "/Evolution_Plot.png"), files)

  plot2pngGemini(plotTE, filename = files[1], type = "plotly")

  for (k in 2:(K + 1)) {
    suppressMessages(ggsave(
      filename = files[k],
      plot = nexus$TM[[k - 1]]$map +
        ggplot2::labs(
          title = paste0("Period: ", periods[k - 1]),
          dpi = 72,
          height = 7,
          width = 14,
          bg = "transparent"
        )
    ))
  }
  return(files)
}

copy_to_clipboard <- function(x) {
  # Check the operating system
  sys_info <- Sys.info()
  os_type <- tolower(sys_info["sysname"])

  # Convert the object to a string if it is not already
  if (!is.character(x)) {
    x <- capture.output(print(x))
  }

  # Copy to clipboard based on the operating system
  if (os_type == "windows") {
    writeClipboard(x)
  } else if (os_type == "darwin") {
    # macOS
    con <- pipe("pbcopy", "w")
    writeLines(x, con)
    close(con)
  } else if (os_type == "linux") {
    # Use xclip or xsel, if available
    if (nzchar(Sys.which("xclip"))) {
      con <- pipe("xclip -selection clipboard", "w")
      writeLines(x, con)
      close(con)
    } else if (nzchar(Sys.which("xsel"))) {
      con <- pipe("xsel --clipboard --input", "w")
      writeLines(x, con)
      close(con)
    } else {
      stop(
        "Neither 'xclip' nor 'xsel' are available. Please install one of them to use the clipboard on Linux."
      )
    }
  } else {
    stop("Unrecognized or unsupported operating system.")
  }
}

## New function to convert gemini output as HTML blocks
gemini_to_html <- function(text, font_size = "16px") {
  # Remove original leading/trailing whitespace
  text <- trimws(text)

  # Divide text into lines
  lines <- unlist(strsplit(text, "\n", fixed = TRUE))

  # Remove empty rows
  lines <- lines[lines != ""]

  # Initialize HTML output with CSS styles including font size
  html_lines <- c(
    paste0(
      "<div style='font-family: Arial, sans-serif; line-height: 1.3; margin: 0 auto; padding: 20px; font-size: ",
      font_size,
      ";'>"
    )
  )
  in_list <- FALSE
  list_type <- ""

  for (i in 1:length(lines)) {
    line <- trimws(lines[i])

    # Jump empty lines
    if (line == "") {
      next
    }

    # Title management (lines enclosed in **)
    if (stringr::str_detect(line, "^\\*\\*[^*]+\\*\\*$")) {
      # Close any open lists
      if (in_list) {
        if (list_type == "ul") {
          html_lines <- c(html_lines, "</ul>")
        } else {
          html_lines <- c(html_lines, "</ol>")
        }
        in_list <- FALSE
      }

      # Convert titles
      title_text <- stringr::str_replace_all(line, "^\\*\\*(.+)\\*\\*$", "\\1")
      html_lines <- c(
        html_lines,
        paste0(
          "<h3 style='color: #333; border-bottom: 2px solid #007acc; padding-bottom: 5px; margin-bottom: 10px; margin-top: 20px;'>",
          title_text,
          "</h3>"
        )
      )
      next
    }

    # Managing bulleted lists (starting with *)
    if (stringr::str_detect(line, "^\\s*\\*\\s+")) {
      # If we are not already in a bulleted list, start it
      if (!in_list || list_type != "ul") {
        if (in_list && list_type == "ol") {
          html_lines <- c(html_lines, "</ol>")
        }
        html_lines <- c(
          html_lines,
          "<ul style='margin-bottom: 10px; margin-top: 5px;'>"
        )
        in_list <- TRUE
        list_type <- "ul"
      }

      # Remove the asterisk and format the content
      item_text <- stringr::str_replace(line, "^\\s*\\*\\s+", "")
      item_text <- format_inline_text(item_text)
      html_lines <- c(
        html_lines,
        paste0("<li style='margin-bottom: 3px;'>", item_text, "</li>")
      )
      next
    }

    # Managing numbered lists (starting with a number followed by a period)
    if (stringr::str_detect(line, "^\\s*\\d+\\.\\s+")) {
      # If we are not already in a numbered list, start it
      if (!in_list || list_type != "ol") {
        if (in_list && list_type == "ul") {
          html_lines <- c(html_lines, "</ul>")
        }
        html_lines <- c(
          html_lines,
          "<ol style='margin-bottom: 10px; margin-top: 5px;'>"
        )
        in_list <- TRUE
        list_type <- "ol"
      }

      # Remove the number and format the content
      item_text <- stringr::str_replace(line, "^\\s*\\d+\\.\\s+", "")
      item_text <- format_inline_text(item_text)
      html_lines <- c(
        html_lines,
        paste0("<li style='margin-bottom: 3px;'>", item_text, "</li>")
      )
      next
    }

    # If we get here and we're in a list, let's close it
    if (in_list) {
      if (list_type == "ul") {
        html_lines <- c(html_lines, "</ul>")
      } else {
        html_lines <- c(html_lines, "</ol>")
      }
      in_list <- FALSE
    }

    # Normal paragraph management
    formatted_line <- format_inline_text(line)
    html_lines <- c(
      html_lines,
      paste0("<p style='margin-bottom: 8px;'>", formatted_line, "</p>")
    )
  }

  # Close any lists that remain open
  if (in_list) {
    if (list_type == "ul") {
      html_lines <- c(html_lines, "</ul>")
    } else {
      html_lines <- c(html_lines, "</ol>")
    }
  }

  # Close the container div
  html_lines <- c(html_lines, "</div>")

  # Merge all lines
  html_result <- paste(html_lines, collapse = "\n")

  return(html_result)
}

# Auxiliary function for formatting inline text
format_inline_text <- function(text) {
  # Bold management (**text**)
  text <- stringr::str_replace_all(
    text,
    "\\*\\*([^*]+)\\*\\*",
    "<strong>\\1</strong>"
  )

  # Italic management (*text*) - only if it is not already in bold
  text <- stringr::str_replace_all(
    text,
    "(?<!\\*)\\*([^*]+)\\*(?!\\*)",
    "<em>\\1</em>"
  )

  # Handling text in quotation marks as inline code ("text")
  text <- stringr::str_replace_all(
    text,
    '"([^"]+)"',
    '<code style="background-color: #f4f4f4; padding: 2px 4px; border-radius: 3px; font-family: \'Courier New\', monospace;">\\1</code>'
  )

  # Handling parentheses with percentages or numerical values
  text <- stringr::str_replace_all(
    text,
    "\\(([^)]*%[^)]*)\\)",
    "<span style='color: #007acc; font-weight: bold;'>(\\1)</span>"
  )

  return(text)
}


# Thematic Map top 3 documents for each cluster
doc2clust <- function(res, n = 3) {
  df <- res$doc2clust %>%
    drop_na(Assigned_cluster) %>%
    group_by(Assigned_cluster) %>%
    slice_max(pagerank, n = n) %>%
    mutate(cluster = paste(SR, ", pagerank ", round(pagerank, 3), sep = "")) %>%
    select(Assigned_cluster, cluster)
}

# APOT top 3 Documents per author
apot2Docs <- function(res, n = 3) {
  df <- res$dfPapersAU %>%
    group_by(Author) %>%
    slice_max(TCpY, n = n) %>%
    mutate(
      SR = paste(TI, SO, year, paste0("TCpY ", round(TCpY, 1)), sep = ", ")
    ) %>%
    select(Author, SR)
}

# Country collaboration table
country2collab <- function(df, n = 20) {
  df <- df %>%
    slice_max(Articles, n = n) %>%
    mutate(
      SR = paste(
        paste0("Article ", Articles, sep = ", "),
        paste0("SCP ", SCP, sep = ", "),
        paste0("MCP ", MCP, sep = ", "),
        paste0("MCP % ", round(`MCP %`, 1), sep = ", ")
      )
    ) %>%
    select(Country, SR)
}

#Local citation table
localCit2docs <- function(df, n = 20) {
  df <- df %>%
    slice_max(Local.Citations, n = n) %>%
    mutate(
      SR = paste(
        paste0("LC ", Local.Citations, sep = ", "),
        paste0("GC ", Global.Citations, sep = ", "),
        paste0("NLC ", round(Normalized.Local.Citations, 2), sep = ", "),
        paste0("NGC ", round(Normalized.Global.Citations, 2)),
        sep = ""
      )
    ) %>%
    select(Document, SR)
}

# Historiograph doc titles
hist2docs <- function(df, n = 20) {
  df %>%
    slice_max(order_by = LCS, n = n) %>%
    select(short_label, title_orig)
}

# Top 10 peaks in RPYS
rpysPeaks <- function(res, n = 10) {
  df_peaks <- res$rpysTable %>%
    arrange(Year) %>%
    mutate(
      is_peak = (diffMedian > lag(diffMedian)) & (diffMedian > lead(diffMedian))
    ) %>%
    dplyr::filter(is_peak) %>%
    arrange(desc(diffMedian)) %>%
    slice_head(n = n)

  df2 <- res$CR %>%
    group_by(Year) %>%
    slice_max(Freq, n = 3, with_ties = FALSE) %>%
    dplyr::filter(Year %in% df_peaks$Year)

  return(peaks = df2)
}
