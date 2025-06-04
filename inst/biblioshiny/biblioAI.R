## gemini AI tools
gemini_ai <- function(image = NULL,
                      prompt = "Explain these images",
                      model = "2.0-flash",
                      type = "png",
                      retry_503 = 3,
                      api_key=NULL) {
  
  # Default config
  generation_config <- list(
    temperature = 1,
    maxOutputTokens = 16384,#8192,
    topP = 0.95,
    topK = 40,
    seed = 1234
  )
  
  # Build URL
  model_query <- paste0("gemini-", model, ":generateContent")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  if (is.null(api_key)) api_key <- Sys.getenv("GEMINI_API_KEY")
  
  # Base structure of parts
  parts <- list(list(text = prompt))
  
  # Handle images if provided
  if (!is.null(image)) {
    if (!is.vector(image)) image <- as.vector(image)
    mime_type <- paste0("image/", type)
    
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
      
      parts <- append(parts, list(
        list(inline_data = list(
          mime_type = mime_type,
          data = image_data
        ))
      ))
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
        return(list(status_code=stringr::str_extract(e$message, "(?<=HTTP )\\d+")|> as.numeric(), 
                    error = TRUE, message = paste("❌ Request failed with error:", e$message)))
      }
    )
    
    # # Handle connection-level error
    # if (is.list(resp) && isTRUE(resp$error)) {
    #   return(resp$message)
    # }

    # Retry on HTTP 503 or 429
    if (resp$status_code %in% c(429,503)) {
      if (attempt < retry_503) {
        message(paste0("⚠️ HTTP 503 (Service Unavailable) - retrying in 2 seconds (attempt ", attempt, "/", retry_503, ")..."))
        Sys.sleep(2)
        next
      } else {
        return(
          paste0(
            "❌ HTTP 503: Service Unavailable.\n",
            "The Google Gemini servers are currently overloaded or under maintenance.\n",
            "All retry attempts failed (", retry_503, "). Please try again later."
          )
        )
      }
    }
    
    # HTTP errors
    # 400 api key not valid
    if (resp$status_code == 400) {
      msg <- tryCatch({
        parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp))
        parsed$error$message
      }, error = function(e) {
        "Please check your API key. It seems to be not valid!"
      })
      return(paste0("❌ HTTP ", resp$status_code, ": ", msg))
    }
    # Other HTTP errors
    if (resp$status_code != 200) {
      msg <- tryCatch({
        parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp))
        parsed$error$message
      }, error = function(e) {
        "Service unavailable or unexpected error. Please check your API key and usage limit."
      })
      
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
  apiCheck <- gemini_ai(image = NULL,
                     prompt = "Hello",
                     model = "2.0-flash",
                     type = "png",
                     retry_503 = 3, api_key=api_key)
  
  contains_http_error <- grepl("HTTP\\s*[1-5][0-9]{2}", apiCheck)
  
  if (contains_http_error) {
    return(list(valid=FALSE, message="❌ API key seems be not valid! Please, check it or your connection."))
  }
  
  if (is.null(api_key) || !is.character(api_key) || nchar(api_key) == 0) {
    return(list(valid=FALSE, message="❌ API key must be a non-empty string."))
  }
  
  if (nchar(api_key) < 10) {
    return(list(valid=FALSE, message="❌ API key seems too short. Please verify your key."))
  }
  
  # 2. Mostra solo gli ultimi 4 caratteri per feedback
  last_chars <- 4
  last <- substr(api_key, max(1, nchar(api_key) - last_chars + 1), nchar(api_key))
  
  # 3. Imposta la variabile d'ambiente
  Sys.setenv(GEMINI_API_KEY = api_key)
  
  return(list(valid=TRUE, message=paste0(paste0(rep("*",nchar(api_key)-4), collapse=""),last,collapse="")))
}

showGeminiAPI <- function(){
  api_key <- Sys.getenv("GEMINI_API_KEY")
  last_chars <- 4
  last <- substr(api_key, max(1, nchar(api_key) - last_chars + 1), nchar(api_key))
  last <- paste0(paste0(rep("*",nchar(api_key)-4), collapse=""),last,collapse="")
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

geminiOutput <- function(title = "", content = "", values){
  if (is.null(content)){
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
      HTML(text_to_html(content))
    ),
    br(),
    em("You can modify or enrich the proposed prompt with additional context or details about your analysis to help 'Biblio AI' generate a more accurate and meaningful interpretation."),
    textAreaInput(
      inputId = "gemini_additional",
      label = NULL,
      value = values$gemini_model_parameters,
      placeholder = "You can provide additional context or details about your analysis to help 'Biblio AI' generate a more accurate and meaningful interpretation.",
      rows = 3,
      width = "100%"
    ),
    fluidRow(
      column(4, align = "center",
             actionButton("gemini_btn", "Ask Biblio AI", style = "color: white;",
                          icon(name = "microchip", lib = "font-awesome"),
                          width = "80%")
      ),
      column(4, align = "center",
             actionButton("copy_btn", "Copy", style = "color: white;", icon = icon("clipboard"),
                          width = "80%")
      ),
      column(4, align = "center",
             downloadButton(outputId = "save_btn", label = "Save", icon = icon("download"),
                            style = "width: 80%;")
      )
    )
  )
}

biblioAiPrompts <- function(values, activeTab){
  
  ## ROle definition for Gemini as Biblio AI assistant
  promptInitial <- "You are Biblio AI, an AI assistant integrated within Biblioshiny. Your task is to support researchers in interpreting and critically discussing the results of their bibliometric analyses, offering insights, contextual explanations, and guidance for data-driven interpretation. "
  
  switch(activeTab,
         "mainInfo"={
           prompt <- paste0("Provide an interpretation of these statistics summarizing the bibliographic collection. ",
                            "Focus on key metrics such as the number of documents, sources, authors, and citations, ",
                            "and discuss what they reveal about the scope, productivity, and impact of the collection. ",
                            "This is the list of statistics: ",merge_df_to_string(values$TABvb))
         },
         "threeFieldPlot"={
           prompt <- paste0("Provide an interpretation of this Three-Field Plot. The central field represents the target metadata,",
                            " while the left and the right fields are the destination metadata. The plot visualizes the connections",
                            " among these three dimensions by linking the target field witgh the two other metadata.",
                            " Analyze the structure of the network, and describe how these elements are interconnected.")
         },
         "authorsProdOverTime"={
           docs <- apot2Docs(values$AUProdOverTime, n=3) %>% merge_df_to_string()
           prompt <- paste0("Provide an interpretation of this 'Authors’ Production Over Time' plot, which displays bibliometric trends",
                            " for the top authors in the field. For each author, the red horizontal line represents their scientific timeline,",
                            " indicating the span of their active publishing years. Each bubble corresponds to a publication year: its size reflects",
                            " the number of articles published that year, while the color intensity represents the total number of citations per year (TC/year).",
                            " Analyze the evolution of individual authors’ productivity and impact, identifying key periods of activity, citation peaks, ",
                            "and possible shifts in research influence over time. A list of the most three cited-per-year articles for each author is provided: ", docs)
         },
         "correspAuthorCountry"={
           countries <- merge_df_to_string(country2collab(values$TABCo))
           prompt <- paste0("Provide an interpretation of this 'Corresponding Author’s Country Collaboration Plot'. The plot displays the number of scientific publications",
                            " by corresponding authors for each country, distinguishing between Single Country Publications (SCP) and Multiple Country Publications (MCP). ",
                            "SCP represents articles authored exclusively within one country, while MCP refers to collaborative works involving authors from different countries. ",
                            "The MCP Ratio, calculated as MCP divided by the total number of publications, indicates the extent of international collaboration. ",
                            "Focus on identifying the most productive countries, comparing their levels of international collaboration, and discussing the balance between domestic ",
                            "and global research engagement. A list of the most 20 frequent countries is provided: ", countries)
         },
         "mostLocalCitDoc"={
           docs <- localCit2docs(values$TABLocDoc,n=20) %>% merge_df_to_string()
           prompt <- paste0("Provide an interpretation of this table reporting the most locally and globally cited articles in the collection. Global Citations (GC) represent the total",
                            " number of citations a document has received across all records indexed in the bibliographic database, indicating its overall scholarly impact. Local Citations (LC),",
                            " on the other hand, count the number of citations a document has received within this specific dataset, reflecting its relevance to the focused research field. ",
                            "Both indicators are normalized by NTC (Normalized Total Citations), which adjusts citation counts relative to the average number of citations for publications from the same year.",
                            " This normalization allows for fair comparison across time. Focus on identifying articles with strong global influence, local relevance, or both.",
                            "A list of the most 20 local cited articles is provided: ", docs)
         },
         "trendTopic"={
           prompt <- paste0("Provide an interpretation of this trend topics plot, where each year is associated with the k words that have the highest annual median frequency. ", 
           "For each word, the light blue line represents the interquartile range (from the first to the third quartile) of its frequency time series, and the central", 
           "point marks the median value. The size of the bubbles is proportional to the annual frequency, reflecting the relative prominence of each term over time. ")
         },
         "ReferenceSpect"={
           references <- merge_df_to_string(rpysPeaks(values$res, n=10))
           prompt <- paste0("Provide an interpretation of this Reference Publication Year Spectroscopy (RPYS) plot.", 
                         " The black line shows the number of cited references by publication year. ",
                         "The red line represents the deviation from the 5-year median citation frequency, calculated using a non-centered window composed of the five preceding years.",
                         " This highlights peak years of historical significance. A list of the most cited references is provided for the first ten peak years identified by the red line,",
                         " along with their citation frequencies: ",
                         references)
         },
         "coOccurenceNetwork" = {
           prompt <- paste0("Provide an interpretation of this 'word co-occurrence' network.",
                            " Focus on the structure of the network, the presence of communities (topics), and the relevance of the most connected terms.")
         },
         "thematicMap"={
           docs <- merge_df_to_string(doc2clust(values$TM))
           prompt <- paste0("Provide an interpretation of this 'strategic map'",
                            "Here there is the list of three most central articles for each cluster: ",docs,
                            ". Focus on the structure of the strategic map, the description of clusters and their centrality and density measures, and the description of the articles belonging to them.")
         },
         "thematicEvolution"={
           prompt <- paste0("Provide an interpretation of these plots.", 
                                                "The first plot illustrates topic evolution across different time periods, showing how thematic areas develop or shift over time. ",
                                                "The other plots display strategic maps for each period, highlighting the positioning of topics based on centrality and density. ",
                                                "Focus on identifying emerging, declining, or stable topics and their strategic importance.")
         },
         "factorialAnalysis"={
           prompt <- "Provide an interpretation of this 'factorial map'. Focus on the structure of the map, the presence of clusters, and the relevance of the most contributing terms."
         },
         "coCitationNetwork"={
           prompt <- "Provide an interpretation of this 'co-citation' network. Focus on the structure of the network, the presence of communities, and the relevance of the most connected terms."
         },
         "historiograph"={
           titles <- hist2docs(values$histPlotVis$VIS$x$nodes) %>% merge_df_to_string()
           prompt <- paste0("Interpret this historiograph, a temporal citation network built by mapping direct citation links among documents. ",
                            #" The y-axis represents publication years, and directed edges indicate citations between articles. ",
                         "Highlight the main citation paths, pivotal works, and any notable temporal trends in knowledge development, looking also to the article titles and their topics.",
                         " Focus on the temporal evolution of each cluster",
                            "Here there is the list of each paper (node) and its title: ",titles)
           # prompt <- paste0("This historiograph represents a temporal citation network, constructed by mapping direct citation links among documents. Please identify and describe the main clusters",
           #                  " as research streams evolving over time. For each cluster, provide a label based on the titles of its pivotal documents. Explain the temporal trajectory of each stream,",
           #                  " highlighting foundational or highly cited works, and discuss any thematic overlaps or transitions between clusters.",
           #                   "Here there is the list of the first 20 most cited papers and its title: ",titles)
         },
         "collabNetwork"={
           prompt <- paste0("Provide an interpretation of this 'collaboration' network", 
                            ". Focus on the structure of the network, the presence of communities, and the relevance of the most connected terms.")
         },
         "collabWorldMap"={
           #values$WMGemini
           prompt <- paste0("Provide an interpretation of this 'Countries’ Collaboration World Map'. The map visualizes international scientific ",
                            "collaboration by showing, for each country, the total number of articles with at least one contributing author. ",
                            "The color intensity of each country is proportional to its research output. The connecting lines represent collaborative ",
                            "links between countries, based on co-authorship across all authors (not only corresponding authors). ",
                            "Focus on identifying major hubs of scientific production, key international partnerships, and global patterns of collaboration.")
         },
         {
           prompt <- paste0("Provide an interpretation of this plot creted with 'bibliometrix R Package'")
         }
  )
  prompt <- paste0(promptInitial,prompt)
  #if (!activeTab %in% c("mainInfo", "thematicMap", "trendTopic")) prompt <- paste0(prompt, " Provide also scientific references about the methodological description")
  return(prompt)
}

geminiGenerate <- function(values, activeTab, gemini_additional, gemini_model_parameters, input){
  
  if (gemini_additional!="") {
    desc <- paste0(values$collection_description, gemini_additional, gemini_model_parameters, collapse=". ")
  } else {
    desc <- paste0(values$collection_description, gemini_model_parameters, collapse=". ")
  }
  prompt <- biblioAiPrompts(values, activeTab)
  switch(activeTab,
         "mainInfo"={
           req(values$TABvb)
           values$MainInfoGemini <- geminiPromptImage(obj=NULL, type="text",
                                                 prompt=prompt,
                                                 key=values$geminiAPI, desc=desc, values=values)
         },
         "threeFieldPlot"={
           req(values$TFP)
           values$TFPGemini <- geminiPromptImage(obj=values$TFP, type="plotly",
                                                 prompt=prompt,
                                                 key=values$geminiAPI, desc=desc, values=values)
         },
         "authorsProdOverTime"={
           req(values$AUProdOverTime)
           values$ApotGemini <- geminiPromptImage(obj=values$AUProdOverTime$graph, type="ggplot2",
                                                  prompt=prompt,
                                                  key=values$geminiAPI, desc=desc, values=values)
         },
         "correspAuthorCountry"={
           req(values$TABCo)
           values$MostRelCountriesGemini <- geminiPromptImage(obj=values$MRCOplot, type="ggplot2",
                                                              prompt=prompt,
                                                              key=values$geminiAPI, desc=desc, values=values)
         },
         "mostLocalCitDoc"={
           req(values$TABLocDoc)
           values$MostLocCitDocsGemini <- geminiPromptImage(obj=NULL, type="text",
                                                         prompt=prompt,
                                                         key=values$geminiAPI, desc=desc, values=values)
         },
         "trendTopic"={
           req(values$trendTopics)
           values$trendTopicsGemini <- geminiPromptImage(obj=values$trendTopics$graph, type="ggplot2",
                                                 prompt=prompt,
                                                 key=values$geminiAPI, desc=desc, values=values)
         },
         "ReferenceSpect"={
           req(values$res)
           values$rpysGemini <- geminiPromptImage(obj=values$res$spectroscopy, type="ggplot2",
                                                         prompt=prompt,
                                                         key=values$geminiAPI, desc=desc, values=values)
         },
         "coOccurenceNetwork" = {
           req(values$COCnetwork)
           values$cocGemini <- geminiPromptImage(obj=values$COCnetwork$VIS, type="vis",
                                                 prompt=prompt,
                                                 key=values$geminiAPI, desc=desc, values=values)
         },
         "thematicMap"={
           req(values$TMmap)
           values$TMGemini <- geminiPromptImage(obj=values$TMmap, type="plotly",
                                                prompt=prompt,
                                                key=values$geminiAPI, desc=desc, values=values)
         },
         "thematicEvolution"={
           req(values$nexus)
           files <- TE2Gemini(values$nexus, values$TEplot)
           values$TEGemini <- geminiPromptImage(obj=files, type="multi",
                                                prompt=prompt,
                                                key=values$geminiAPI, desc=desc, values=values)
         },
         "factorialAnalysis"={
           req(values$plotCS)
           values$CSGemini <- geminiPromptImage(obj=values$plotCS, type="plotly",
                                                prompt=prompt,
                                                key=values$geminiAPI, desc=desc, values=values)
         },
         "coCitationNetwork"={
           req(values$COCITnetwork)
           values$cocitGemini <- geminiPromptImage(obj=values$COCITnetwork$VIS, type="vis",
                                                   prompt=prompt,
                                                   key=values$geminiAPI, desc=desc, values=values)
         },
         "historiograph"={
           req(values$histPlotVis$VIS)
           values$histGemini <- geminiPromptImage(obj=values$histPlotVis$VIS, type="vis",
                                                  prompt=prompt,
                                                  key=values$geminiAPI, desc=desc, values=values)
         },
         "collabNetwork"={
           req(values$COLnetwork$VIS)
           values$colGemini <- geminiPromptImage(obj=values$COLnetwork$VIS, type="vis",
                                                 prompt=prompt,
                                                 key=values$geminiAPI, desc=desc, values=values)
         },
         "collabWorldMap"={
           values$WMGemini <- geminiPromptImage(obj=values$WMmap$g, type="ggplot2",
                                                prompt=prompt,
                                                key=values$geminiAPI, desc=desc, values=values)
         }
  )
  return(values)
}

geminiParameterPrompt <- function(values, activeTab, input){
  if ("DB_Original" %in% names(values$M)) {
    DB <- unique(values$M$DB_Original)
    if (length(DB)>1) DB[length(DB)] <- paste0("and ",DB[length(DB)]) 
    DB <- gsub("ISI","WOS", paste0(DB, collapse=", "))
    
    DB <- paste0("databases ",DB, " merged by Bibliometrix R package routines.")
  } else {
    if (values$M$DB[1]=="ISI") {DB <- "WOS" } else {DB <- values$M$DB[1]}
    DB <- paste0("database ",DB,".")
  }
  txt <- paste0("The analysis was performed on a collection downloaded from the following bibliographic ", DB)
  missingTags <- values$missingdf %>% 
    filter(status %in% "Completely missing") %>% 
    pull(description)
  if (length(missingTags)>0){
    if ("DB_Original" %in% names(values$M)) {
      txt1 <- paste0(" Some metadata fields like ", paste0(missingTags, collapse=", "), 
                                                           " are missing due to limitations in the merging process and 
                                                           non-standard formats across databases.")
    txt <- paste0(txt, txt1)
    }
  } 

  switch(activeTab,
         "mainInfo"={
           req(values$TABvb)
         },
         "threeFieldPlot"={
           req(values$TFP)
           txt <- paste0(txt, " This graph was generated with the following parameters: target field '",
                         input$CentralField, "', right field '", input$RightField, "', and left field '", input$LeftField, "'.")
         },
         "trendTopic"={
           req(values$trendTopics)
           txt <- paste0(txt, " This graph was generated with the following parameters: ", merge_df_to_string(values$trendTopics$params))
         },
         "authorsProdOverTime"={
           req(values$AUProdOverTime)
         },
         "correspAuthorCountry"={
           req(values$TABCo)
         },
         "mostLocalCitDoc"={
           req(values$TABLocDoc)
         },
         "ReferenceSpect"={
           req(values$res)
           },
         "coOccurenceNetwork" = {
           req(values$COCnetwork)
           txt <- paste0(txt, " This graph was generated with the following parameters: ", merge_df_to_string(values$cocnet$params))
         },
         "thematicMap"={
           req(values$TMmap)
           txt <- paste0(txt, "  This graph was generated with the following parameters: ", merge_df_to_string(values$TM$params))
         },
         "thematicEvolution"={
           req(values$nexus)
           txt <- paste0(txt, " The following parameters were used for each strategic map: ", merge_df_to_string(values$nexus$TM[[1]]$params))
         },
         "factorialAnalysis"={
           req(values$plotCS)
           txt <- paste0(txt, "  This graph was generated with the following parameters: ", merge_df_to_string(values$CS$params))
         },
         "coCitationNetwork"={
           req(values$COCITnetwork)
           txt <- paste0(txt, "  This graph was generated with the following parameters: ", merge_df_to_string(values$cocitnet$params))
         },
         "historiograph"={
           req(values$histPlotVis$VIS)
         },
         "collabNetwork"={
           req(values$COLnetwork$VIS)
           txt <- paste0(txt, "  This graph was generated with the following parameters: ", merge_df_to_string(values$colnet$params))
         },
         "collabWorldMap"={
           req(values$WMmap)
         }
  )
  return(txt)
}

## gemini prompt for images
geminiPromptImage <- function(obj, type="vis", prompt="Explain the topics in this map", key, desc = NULL, values){
  
  ## Check Computer configuration to work with Biblio AI
  ### Internet Connection
  if (!is_online()){
    res <- '⚠️ **Note**: Biblio AI requires an active internet connection to work.'
    return(res)
  }
  ### Chromium Browser
  if (is.null(values$Chrome_url)) {
    res <- '⚠️ **Note**: Biblio AI requires a **Chrome-based browser** (such as Google Chrome or Microsoft Edge) installed on your computer to work correctly.'
    return(res)
  }
  
  if (key){
    if (!is.null(desc)) prompt <- paste0(prompt,desc,collapse=". ")
    tmpdir <- tempdir()
    owd <- setwd(tmpdir)
    on.exit(setwd(owd))
    file_path <- paste0(tempfile(),".png",collapse="")
    switch(type,
           "vis"={
             plot2pngGemini(obj, filename=file_path, type="vis")
           },
           "plotly"={
             plot2pngGemini(obj, filename=file_path, type="plotly")
           },
           "multi"={ # prompt with multiple image files
             file_path <- obj
           },
           "text"={ # prompt based only on text
             file_path <- NULL
           },
           "ggplot2"={
             ggsave(filename = file_path, plot = obj, dpi = 72, height = 7, width = 14, bg = "transparent")
           })
    
    res <- gemini_ai(image = file_path,
                     prompt = prompt)
  } else {
    res <- 'To access this feature, please provide a valid Gemini AI API key. You can obtain your API key by visiting the official <a href="https://aistudio.google.com/" target="_blank">Google AI Studio website</a>.'
  }
  
  return(res)
}

geminiWaitingMessage <- function(values, activeTab){
  
  messageTxt <- "⌛ Thinking..."
  
  switch(activeTab,
         "mainInfo"={
           req(values$TABvb)
           values$MainInfoGemini <- messageTxt
         },
         "threeFieldPlot"={
           req(values$TFP)
           values$TFPGemini <- messageTxt
         },
         "authorsProdOverTime"={
           req(values$AUProdOverTime)
           values$ApotGemini <- messageTxt
         },
         "correspAuthorCountry"={
           req(values$TABCo)
           values$MostRelCountriesGemini  <- messageTxt
         },
         "mostLocalCitDoc"={
           req(values$TABLocDoc)
           values$MostLocCitDocsGemini <- messageTxt
         },
         "trendTopic"={
           req(values$trendTopics)
           values$trendTopicsGemini <- messageTxt
         },
         "ReferenceSpect"={
           req(values$res)
           values$rpysGemini <- messageTxt
         },
         "coOccurenceNetwork" = {
           req(values$COCnetwork)
           values$cocGemini <- messageTxt
         },
         "thematicMap"={
           req(values$TM)
           values$TMGemini <- messageTxt
         },
         "thematicEvolution"={
           req(values$nexus)
           values$TEGemini <- messageTxt
         },
         "factorialAnalysis"={
           req(values$plotCS)
           values$CSGemini <- messageTxt
         },
         "coCitationNetwork"={
           req(values$COCITnetwork)
           values$cocitGemini <- messageTxt
         },
         "historiograph"={
           req(values$histPlotVis$VIS)
           values$histGemini <- messageTxt
         },
         "collabNetwork"={
           req(values$COLnetwork$VIS)
           values$colGemini <- messageTxt
         },
         "collabWorldMap"={
           req(values$WMmap)
           values$WMGemini <- messageTxt
         }
  )
  return(values)
}

geminiContextualOutput <- function(values, activeTab){

    switch(activeTab,
           "mainInfo"={
             messageTxt <- values$MainInfoGemini
           },
           "threeFieldPlot"={
             messageTxt <- values$TFPGemini
           },
           "authorsProdOverTime"={
             messageTxt <- values$ApotGemini
           },
           "correspAuthorCountry"={
             messageTxt <- values$MostRelCountriesGemini
           },
           "mostLocalCitDoc"={
             messageTxt <- values$MostLocCitDocsGemini
           },
           "trendTopic"={
             messageTxt <- values$trendTopicsGemini
           },
           "ReferenceSpect"={
             messageTxt <- values$rpysGemini
           },
           "coOccurenceNetwork" = {
             messageTxt <- values$cocGemini
           },
           "thematicMap"={
             messageTxt <- values$TMGemini
           },
           "thematicEvolution"={
             messageTxt <- values$TEGemini
           },
           "factorialAnalysis"={
             messageTxt <- values$CSGemini
           },
           "coCitationNetwork"={
             messageTxt <- values$cocitGemini
           },
           "historiograph"={
             messageTxt <- values$histGemini
           },
           "collabNetwork"={
             messageTxt <- values$colGemini
           },
           "collabWorldMap"={
             messageTxt <- values$WMGemini
           }
    )
    return(messageTxt)
}

geminiSave <- function(values, activeTab){
  
  switch(activeTab,
         "mainInfo"={
           gemini <- values$MainInfoGemini
         },
         "threeFieldPlot"={
           gemini <- values$TFPGemini
         },
         "authorsProdOverTime"={
           gemini <- values$ApotGemini
         },
         "correspAuthorCountry"={
           gemini <- values$MostRelCountriesGemini 
         },
         "mostLocalCitDoc"={
           gemini <- values$MostLocCitDocsGemini
         },
         "trendTopic"={
           gemini <- values$trendTopicsGemini
         },
         "ReferenceSpect"={
           gemini <- values$rpysGemini
         },
         "coOccurenceNetwork" = {
           gemini <- values$cocGemini
         },
         "thematicMap"={
           gemini <- values$TMGemini
         },
         "thematicEvolution"={
           gemini <- values$TEGemini
         },
         "factorialAnalysis"={
           gemini <- values$CSGemini
         },
         "coCitationNetwork"={
           gemini <- values$cocitGemini
         },
         "historiograph"={
           gemini <- values$histGemini
         },
         "collabNetwork"={
           gemini <- values$colGemini
         },
         "collabWorldMap"={
           gemini <- values$WMGemini
         }
  )
  if (is.null(gemini)) gemini <- "Click 'Ask Biblio AI' for help. "
  return(gemini)
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
  switch(type,
         vis = {
           visSave(p, html_name)
         },
         plotly = {
           htmlwidgets::saveWidget(p, file = html_name)
         }
  )
  
  biblioShot(html_name, zoom = zoom, file = filename)
}

## save all plots of the thematic evolution analysis
TE2Gemini <- function(nexus, plotTE){
  K <- length(nexus$TM)
  periods <- nexus$Nodes %>% select(group) %>% distinct() %>% pull()
  files <- unlist(lapply(periods, function(x){
    paste0(tempdir(),"/",x,"_period.png")
  }))
  files <- c(paste0(tempdir(),"/Evolution_Plot.png"), files)
  
  plot2pngGemini(plotTE, filename=files[1], type="vis")
  
  for (k in 2:(K+1)){
    suppressMessages(ggsave(filename = files[k], plot = nexus$TM[[k-1]]$map + 
             ggplot2::labs(title = paste0("Period: ",periods[k-1]), 
                           dpi = 72, height = 7, width = 14, bg = "transparent")))
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
  } else if (os_type == "darwin") {  # macOS
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
      stop("Neither 'xclip' nor 'xsel' are available. Please install one of them to use the clipboard on Linux.")
    }
  } else {
    stop("Unrecognized or unsupported operating system.")
  }
}

# convert gemini output to HTML
text_to_html <- function(input_text) {
  input_text <- c(input_text,"\n")
  # Escape HTML special characters
  escape_html <- function(text) {
    text <- gsub("&", "&amp;", text)
    text <- gsub("<", "&lt;", text)
    text <- gsub(">", "&gt;", text)
    text
  }
  
  # Convert markdown-style bold (**text**) to <strong>
  convert_bold <- function(text) {
    gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", text)
  }
  
  # Process each paragraph
  paragraphs <- unlist(strsplit(input_text, "\n\n"))
  html_paragraphs <- lapply(paragraphs, function(p) {
    lines <- unlist(strsplit(p, "\n"))
    lines <- sapply(lines, escape_html) # escape special characters
    lines <- sapply(lines, convert_bold) # convert **bold**
    
    if (all(grepl("^\\*\\s+", lines))) {
      # Convert to unordered list
      lines <- gsub("^\\*\\s+", "", lines)
      items <- paste0("<li>", lines, "</li>", collapse = "\n")
      return(paste0("<ul>\n", items, "\n</ul>"))
    } else {
      # Regular paragraph
      return(paste0("<p>", paste(lines, collapse = "<br/>"), "</p>"))
    }
  })
  
  # Combine all HTML parts
  html_body <- paste(html_paragraphs, collapse = "\n\n")
  html <- paste0("<html>\n<body>\n", html_body, "\n</body>\n</html>")
  html <- gsub("\n","",html)
  return(html)
}

# From HTML to text Blocks 
html_to_blocks <- function(raw_html){
  html_body <- sub(".*<body[^>]*>", "", raw_html)
  html_body <- sub("</body>.*", "", html_body)
  
  blocks <- stringr::str_split(
    html_body,
    "(?=<p|<ul|<ol|<li|<h[1-6]|<blockquote|<pre|<table|<div)",
    simplify = FALSE
  )[[1]]
  
  blocks <- trimws(blocks)
  blocks <- blocks[nzchar(blocks)]
}

# Peaks identification in RPYS
rpysPeaks <- function(res, n=10){
  df_peaks <- res$rpysTable %>%
    arrange(Year) %>%
    mutate(
      is_peak = (diffMedian5 > lag(diffMedian5)) & (diffMedian5 > lead(diffMedian5))
    ) %>%
    filter(is_peak) %>%
    arrange(desc(diffMedian5)) %>%
    slice_head(n = n)  
  
  df2 <- res$CR %>% 
    group_by(Year) %>% 
    slice_max(Freq, n=3) %>% 
    filter(Year %in% df_peaks$Year)
  
  return(peaks=df2)
}

# Thematic Map top 3 documents for each cluster
doc2clust <- function(res, n=3){
  df <- res$doc2clust %>% 
    drop_na(Assigned_cluster) %>% 
    group_by(Assigned_cluster) %>% 
    slice_max(pagerank, n=n) %>% 
    mutate(cluster = paste(SR,", pagerank ",round(pagerank,3), sep="")) %>% 
    select(Assigned_cluster, cluster)
}

# APOT top 3 Documents per author
apot2Docs <- function(res, n=3){
  df <- res$dfPapersAU %>% 
    group_by(Author) %>% 
    slice_max(TCpY, n=n) %>% 
    mutate(SR = paste(TI, SO,year, paste0("TCpY ",round(TCpY,1)), sep=", ")) %>% 
    select(Author,SR)
}

# Country collaboration table
country2collab <- function(df, n=20){
  df <- df %>%
    slice_max(Articles, n=n) %>% 
    mutate(SR = paste(paste0("Article ",Articles, sep=", "), paste0("SCP ",SCP, sep=", "),paste0("MCP ",MCP, sep=", "), paste0("MCP % ",round(`MCP %`,1), sep=", "))) %>% 
    select(Country,SR)
}

#Local citation table
localCit2docs <- function(df,n=20){
  df <- df %>%
    slice_max(Local.Citations, n=n) %>% 
    mutate(SR = paste(paste0("LC ",Local.Citations, sep=", "), paste0("GC ",Global.Citations, sep=", "), 
                      paste0("NLC ",round(Normalized.Local.Citations,2), sep=", "), paste0("NGC ",round(Normalized.Global.Citations,2)), sep="")) %>% 
    select(Document,SR)
}

# Historiograph doc titles
hist2docs <- function(df, n=20){
  df %>% 
    slice_max(order_by = LCS, n=n) %>% 
    select(short_label,title_orig)
}