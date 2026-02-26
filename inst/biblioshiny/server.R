source("utils.R", local = TRUE)
source("libraries.R", local = TRUE)
source("biblioShot.R", local = TRUE)
source("biblioAI.R", local = TRUE)
source("contentAnalysisUI.R", local = TRUE)
source("contentAnalysisServer.R", local = TRUE)
source("article_summary.R", local = TRUE)
source("lifeCycleUI.R", local = TRUE)
source("openalex_api.R", local = TRUE)
source("pubmed_api.R", local = TRUE)
source("Htmlboxformat.R", local = TRUE)

suppressMessages(res <- libraries())

if (!res) {
  stop(
    "Biblioshiny cannot be loaded, some packages are missing. Please check your internet connection and try again."
  )
}

#### SERVER ####
server <- function(input, output, session) {
  # Enable shinyjs
  shinyjs::useShinyjs()

  # Resize all chart widgets when navigating back to a tab
  # Fixes issue where async-rendered plots have wrong dimensions
  resizeChartsJS <- '
    setTimeout(function(){
      // Resize plotly widgets
      var plots = document.querySelectorAll(".js-plotly-plot");
      if(plots.length > 0 && window.Plotly){
        plots.forEach(function(p){
          if(p.offsetParent !== null){
            Plotly.Plots.resize(p);
          }
        });
      }
      // Resize visNetwork widgets
      var visWidgets = document.querySelectorAll(".visNetwork");
      visWidgets.forEach(function(el){
        if(el.offsetParent !== null){
          var widget = HTMLWidgets.find("#" + el.id);
          if(widget && widget.network){
            var container = el.parentElement;
            if(container){
              widget.network.setSize(container.offsetWidth + "px", container.offsetHeight + "px");
              widget.network.fit();
            }
          }
        }
      });
    }, 500);
  '
  observeEvent(input$sidebarmenu, {
    shinyjs::runjs(resizeChartsJS)
  })
  observeEvent(input$activeTab, {
    shinyjs::runjs(resizeChartsJS)
  })

  # Stop App button handler
  observeEvent(input$stop_app, {
    showModal(modalDialog(
      title = "Stop Biblioshiny",
      "Are you sure you want to stop the application?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_stop_app", "Stop", class = "btn-danger")
      )
    ))
  })
  observeEvent(input$confirm_stop_app, {
    removeModal()
    # Show goodbye page, try to close browser tab, then stop the R app
    shinyjs::runjs(
      "
      document.title = 'Biblioshiny - Stopped';
      document.body.innerHTML = '<div style=\"display:flex;flex-direction:column;align-items:center;justify-content:center;height:100vh;background:#f5f5f5;font-family:Helvetica,Arial,sans-serif;\">' +
        '<i class=\"fa fa-check-circle\" style=\"font-size:64px;color:#4CAF50;margin-bottom:20px;\"></i>' +
        '<h2 style=\"color:#333;margin-bottom:10px;\">Biblioshiny has been stopped</h2>' +
        '<p style=\"color:#777;font-size:16px;\">You can safely close this tab.</p>' +
      '</div>';
      setTimeout(function(){ window.close(); }, 1500);
    "
    )
    # Delay stopApp() to allow the goodbye page to render
    later::later(
      function() {
        stopApp()
      },
      delay = 2
    )
  })

  ## suppress warnings
  options(warn = -1)

  if (inherits(try(pagedown::find_chrome(), silent = T), "try-error")) {
    Chrome_url <- NULL
  } else {
    Chrome_url <- pagedown::find_chrome()
  }

  #  Sys.setenv (CHROMOTE_CHROME = Chrome_url)

  ## chrome configuration for server environments
  is_headless <- identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps") ||
    (Sys.info()["sysname"] == "Linux" && Sys.getenv("DISPLAY") == "")

  if (is_headless) {
    message("Configurazione Chrome per ambiente headless")
    chromote::set_default_chromote_object(
      chromote::Chromote$new(chromote::Chrome$new(
        args = c(
          "--headless=new", # Usa la nuova modalità headless
          "--disable-gpu", # Disabilita GPU
          "--no-sandbox", # Necessario per server
          "--disable-dev-shm-usage", # Evita problemi memoria condivisa
          "--disable-setuid-sandbox", # Sandbox alternativo
          "--disable-software-rasterizer", # Disabilita rasterizer software
          "--disable-extensions", # Niente estensioni
          "--disable-background-networking", # Riduce processi background
          "--disable-sync", # Disabilita sincronizzazione
          "--metrics-recording-only", # Solo metriche
          "--mute-audio", # Disabilita audio
          "--no-first-run", # Salta primo avvio
          "--disable-features=VizDisplayCompositor", # Disabilita compositor
          "--force-color-profile=srgb", # Forza profilo colore
          "--window-size=1920,1080" # Dimensione finestra virtuale
        )
      ))
    )
  }
  ## end configuration

  ## Check if Chrome browser is installed on the computer
  if (is.null(Chrome_url)) {
    showModal(modalDialog(
      title = strong("Warning message!"),
      HTML(
        "Chrome or a Chromium-based browser is not installed on your computer.<br>
If you do not have either of these browsers installed, Biblioshiny will be unable to export graphs.<br>
To ensure the functionality of Biblioshiny,
           please download Chrome by <a href='https://www.google.com/chrome/' target='_blank' > <b>clicking here</b></a>."
      ),
      footer = modalButton("Dismiss"),
      easyClose = TRUE
    ))
  } else {
    Sys.setenv(CHROMOTE_CHROME = Chrome_url)
  }

  # svuota la cartella temporanea
  unlink(getWD(), recursive = TRUE)

  ## file upload max size
  maxUploadSize <- 200 # default value
  maxUploadSize <- getShinyOption("maxUploadSize", maxUploadSize)
  options(shiny.maxRequestSize = maxUploadSize * 1024^2)

  ## max rows limit
  max_rows <- getShinyOption("biblioshiny.max.rows", Inf)

  ## initial values
  selected_author <- reactiveVal()
  data("logo", package = "bibliometrix", envir = environment())
  values = reactiveValues()
  values$Chrome_url <- Chrome_url
  values$sidebar <- sidebarMenu()
  values$rest_sidebar <- FALSE
  values$list_file <- data.frame(sheet = NULL, file = NULL, n = NULL)
  values$wb <- openxlsx::createWorkbook()
  values$dfLabel <- dfLabel()
  values$myChoices <- "Empty Report"
  values$logo <- logo
  values$logoGrid <- grid::rasterGrob(logo, interpolate = TRUE)
  values$out <- NULL
  values$loadMenu <- NA

  ### column to export in TALL
  if (suppressPackageStartupMessages(!require("tall", quietly = TRUE))) {
    values$TALLmissing <- TRUE
  } else {
    values$TALLmissing <- FALSE
  }
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

  ### setting values
  values$dpi <- 300
  values$h <- 7
  #values$w <- 14
  values$path <- paste0(getwd(), .Platform$file.sep)
  ###

  values$results <- list("NA")
  values$log <- "working..."
  values$load = "FALSE"
  values$field = values$cocngrams = "NA"
  values$citField = values$colField = values$citSep = "NA"
  values$NetWords = values$NetRefs = values$ColNetRefs = matrix(NA, 1, 1)
  values$Title = "Network"
  values$Histfield = "NA"
  values$histlog = "working..."
  values$kk = 0
  values$M = data.frame(PY = 0)
  values$histsearch = "NA"

  ## Computation cache keys and results
  values$cache_TM_key <- NULL
  values$cache_TE_key <- NULL
  values$TE_ready <- 0
  values$TE_computing <- FALSE
  values$TE_error <- NULL
  values$RPYS_ready <- 0
  values$RPYS_computing <- FALSE
  values$RPYS_error <- NULL
  values$TMAP_ready <- 0
  values$CMMAP_ready <- 0
  values$COC_ready <- 0
  values$CS_ready <- 0
  values$COCIT_ready <- 0
  values$COL_ready <- 0
  values$HIST_ready <- 0
  values$WM_ready <- 0
  values$MRSources_ready <- 0
  values$MRAuthors_ready <- 0
  values$MLCAuthors_ready <- 0
  values$MRAff_ready <- 0
  values$CAUCountries_ready <- 0
  values$MCCountries_ready <- 0
  values$MGCDocs_ready <- 0
  values$cache_HIST_key <- NULL
  values$cache_HAu_key <- NULL
  values$cache_HAu_result <- NULL
  values$cache_HSo_key <- NULL
  values$cache_HSo_result <- NULL
  values$citShortlabel = "NA"
  values$S = list("NA")
  values$GR = "NA"
  values$dsToken <- "Wrong account or password"
  values$dsSample <- 0
  values$dsQuery <- ""
  values$pmQuery <- " "
  values$pmSample <- 0
  values$ApiOk <- 0
  values$checkControlBar <- FALSE

  ## Openalex API
  values$data_source <- ""

  ## Diachronic networks
  values$index_coc <- 0
  values$playing_coc <- TRUE
  values$paused_coc <- FALSE
  values$index_col <- 0
  values$playing_col <- TRUE
  values$paused_col <- FALSE

  ## Content Analysis
  values$pdf_text = NULL
  values$analysis_results = NULL
  values$network_plot = NULL
  values$analysis_running = FALSE

  ## gemini api and model
  home <- homeFolder()
  path_gemini_key <- file.path(home, ".biblio_gemini_key.txt")
  # check if sub directory exists
  values$geminiAPI <- load_api_key(path_gemini_key)
  values$collection_description <- NULL
  values$gemini_additional <- NULL

  ## OpenAlex polite pool email
  path_oa_email <- file.path(home, ".biblio_openalex_email.txt")
  if (file.exists(path_oa_email)) {
    oa_saved_email <- trimws(readLines(path_oa_email, warn = FALSE)[1])
    if (!is.na(oa_saved_email) && nchar(oa_saved_email) >= 5) {
      Sys.setenv(openalexR.mailto = oa_saved_email)
      options(openalexR.mailto = oa_saved_email)
      values$oaPoliteEmail <- oa_saved_email
    } else {
      values$oaPoliteEmail <- NULL
    }
  } else {
    values$oaPoliteEmail <- NULL
  }

  ## OpenAlex API key
  path_oa_apikey <- file.path(home, ".biblio_openalex_apikey.txt")
  if (file.exists(path_oa_apikey)) {
    oa_saved_apikey <- trimws(readLines(path_oa_apikey, warn = FALSE)[1])
    if (!is.na(oa_saved_apikey) && nchar(oa_saved_apikey) >= 10) {
      Sys.setenv(openalexR.apikey = oa_saved_apikey)
      options(openalexR.apikey = oa_saved_apikey)
      values$oaApiKey <- oa_saved_apikey
    } else {
      values$oaApiKey <- NULL
    }
  } else {
    values$oaApiKey <- NULL
  }

  path_gemini_model <- file.path(home, ".biblio_gemini_model.txt")
  gemini_api_model <- loadGeminiModel(path_gemini_model)
  values$gemini_api_model <- gemini_api_model[1]
  values$gemini_output_size <- gemini_api_model[2]

  ## Setup async execution for AI calls
  future::plan(future::multisession, workers = 2)

  # Show analysis menu items when data is loaded
  observeEvent(
    values$M,
    {
      if (!is.null(values$M) && ncol(values$M) > 4) {
        # Section headers are already visible, so we only show menu items

        # Show menu items (IDs were added by JavaScript)
        shinyjs::show(
          "menu-filters",
          anim = TRUE,
          animType = "fade",
          time = 0.3
        )
        shinyjs::show(
          "menu-overview",
          anim = TRUE,
          animType = "fade",
          time = 0.3
        )
        shinyjs::show(
          "menu-sources",
          anim = TRUE,
          animType = "fade",
          time = 0.3
        )
        shinyjs::show(
          "menu-authors",
          anim = TRUE,
          animType = "fade",
          time = 0.3
        )
        shinyjs::show(
          "menu-documents",
          anim = TRUE,
          animType = "fade",
          time = 0.3
        )
        shinyjs::show(
          "menu-clustering",
          anim = TRUE,
          animType = "fade",
          time = 0.3
        )
        shinyjs::show(
          "menu-conceptual",
          anim = TRUE,
          animType = "fade",
          time = 0.3
        )
        shinyjs::show(
          "menu-intellectual",
          anim = TRUE,
          animType = "fade",
          time = 0.3
        )
        shinyjs::show("menu-social", anim = TRUE, animType = "fade", time = 0.3)
        shinyjs::show("menu-report", anim = TRUE, animType = "fade", time = 0.3)
        shinyjs::show("menu-tall", anim = TRUE, animType = "fade", time = 0.3)
      }
    },
    ignoreInit = TRUE,
    once = TRUE
  )
  # Help Menu URLs and Version
  intro <- "https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html"
  importData <- "https://www.bibliometrix.org/vignettes/Data-Importing-and-Converting.html"
  slides <- "https://bibliometrix.org/biblioshiny/assets/player/KeynoteDHTMLPlayer.html#0"
  biblioshinyVersion <- as.character(packageVersion("bibliometrix"))
  ## Donation Card ----
  observeEvent(input$show_donate, {
    showModal(modalDialog(
      title = div(
        style = "text-align: center; margin-bottom: 10px;",
        icon("heart", style = "color: #e74c3c; font-size: 26px;"),
        span(
          " Support Bibliometrix and Biblioshiny",
          style = "font-size: 24px; font-weight: bold; color: #2c3e50; margin-left: 10px;"
        )
      ),
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close"),

      div(
        style = "padding: 20px;",

        # Messaggio principale
        div(
          style = "text-align: center; margin-bottom: 30px;",

          div(
            style = "font-size: 16px; color: #2c3e50; line-height: 1.8; margin-bottom: 20px;",
            "Bibliometrix and Biblioshiny are free and open-source tools developed with passion by our research team.",
            tags$br(),
            "Your support helps us maintain and improve the project."
          ),

          div(
            style = "font-size: 18px; color: #e74c3c; font-weight: bold; margin-bottom: 25px;",
            icon("heart", style = "margin-right: 8px;"),
            "Help us keep Bibliometrix and Biblioshiny free for everyone"
          )
        ),

        # Card donazione principale
        tags$a(
          href = "https://www.bibliometrix.org/home/index.php/donation",
          target = "_blank",
          style = "text-decoration: none; display: block;",
          div(
            style = "background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%); padding: 30px; border-radius: 12px; box-shadow: 0 5px 20px rgba(231, 76, 60, 0.3); transition: all 0.3s; cursor: pointer;",
            onmouseover = "this.style.transform='translateY(-5px)'; this.style.boxShadow='0 8px 25px rgba(231, 76, 60, 0.4)';",
            onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 5px 20px rgba(231, 76, 60, 0.3)';",

            div(
              style = "text-align: center;",

              # Icona grande
              div(
                style = "width: 80px; height: 80px; border-radius: 50%; background: white; display: flex; align-items: center; justify-content: center; margin: 0 auto 20px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
                icon(
                  "hand-holding-heart",
                  style = "color: #e74c3c; font-size: 35px;"
                )
              ),

              # Titolo
              div(
                "Make a Donation",
                style = "color: white; font-size: 24px; font-weight: bold; margin-bottom: 12px;"
              ),

              # Descrizione
              div(
                "Support the development and maintenance of Bibliometrix and Biblioshiny",
                style = "color: rgba(255,255,255,0.95); font-size: 14px; margin-bottom: 20px; line-height: 1.6;"
              ),

              # Button
              div(
                icon("external-link-alt", style = "margin-right: 8px;"),
                "Donate Now",
                style = "color: #e74c3c; font-size: 16px; font-weight: 700; display: inline-block; padding: 12px 30px; background: white; border-radius: 8px; box-shadow: 0 3px 10px rgba(0,0,0,0.1);"
              )
            )
          )
        ),

        # Info aggiuntive
        div(
          style = "margin-top: 25px; padding: 20px; background: #f8f9fa; border-radius: 8px; border-left: 4px solid #e74c3c;",

          div(
            icon("info-circle", style = "color: #3498db; margin-right: 8px;"),
            strong("Why donate?", style = "color: #2c3e50; font-size: 14px;")
          ),

          div(
            style = "margin-top: 10px; color: #7f8c8d; font-size: 13px; line-height: 1.6;",
            tags$ul(
              style = "margin: 10px 0; padding-left: 20px;",
              tags$li("Keep the tool free and accessible"),
              tags$li("Fund new features and improvements"),
              tags$li("Support open-source research software"),
              tags$li("Enable continuous maintenance and updates")
            )
          )
        )
      )
    ))
  })

  ## Team Card ----
  observeEvent(input$show_team, {
    showModal(modalDialog(
      title = div(
        style = "text-align: center; margin-bottom: 10px;",
        icon("users", style = "color: #3c8dbc; font-size: 26px;"),
        span(
          " Bibliometrix Creators",
          style = "font-size: 24px; font-weight: bold; color: #2c3e50; margin-left: 10px;"
        )
      ),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),

      # Creators Section
      div(
        style = "padding: 20px;",

        div(
          style = "margin-bottom: 35px;",
          div(
            style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 25px; margin-bottom: 10px;",

            createAuthorCard(
              name = "Massimo Aria",
              title = "Full Professor of Statistics for Social Sciences",
              affiliation = "University of Naples Federico II",
              url = "https://www.massimoaria.com",
              photo = "images/team/massimo_aria.jpg",
              scholar = FALSE
            ),

            createAuthorCard(
              name = "Corrado Cuccurullo",
              title = "Full Professor of Corporate Governance",
              affiliation = "University of Campania Luigi Vanvitelli",
              url = "https://scholar.google.com/citations?user=mfW3fRwAAAAJ&hl=it",
              photo = "images/team/corrado_cuccurullo.jpg",
              scholar = FALSE
            )
          )
        )
      )
    ))
  })

  ## Link Card ----
  observeEvent(input$show_credits, {
    showModal(modalDialog(
      title = div(
        style = "text-align: center; margin-bottom: 10px;",
        icon("cube", style = "color: #667eea; font-size: 26px;"),
        span(
          " Credits",
          style = "font-size: 24px; font-weight: bold; color: #2c3e50; margin-left: 10px;"
        )
      ),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),

      div(
        style = "padding: 15px; max-height: 70vh; overflow-y: auto;",

        # Container con layout a griglia
        div(
          style = "display: grid; grid-template-columns: repeat(2, 1fr); gap: 15px;",

          # Bibliometrix Website Card
          tags$a(
            href = "https://www.bibliometrix.org",
            target = "_blank",
            style = "text-decoration: none; display: block;",
            div(
              style = "background: linear-gradient(135deg, #2193b0 0%, #6dd5ed 100%); padding: 18px; border-radius: 10px; box-shadow: 0 4px 15px rgba(33, 147, 176, 0.3); transition: all 0.3s; cursor: pointer; height: 100%;",
              onmouseover = "this.style.transform='translateY(-3px)'; this.style.boxShadow='0 6px 20px rgba(33, 147, 176, 0.4)';",
              onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 4px 15px rgba(33, 147, 176, 0.3)';",

              div(
                style = "display: flex; align-items: center; margin-bottom: 10px;",
                div(
                  style = "width: 50px; height: 50px; border-radius: 50%; background: white; display: flex; align-items: center; justify-content: center; margin-right: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                  icon(
                    "project-diagram",
                    style = "color: #2193b0; font-size: 24px;"
                  )
                ),
                div(
                  div(
                    "Bibliometrix",
                    style = "color: white; font-size: 20px; font-weight: bold; margin-bottom: 3px;"
                  ),
                  div(
                    "Official Website",
                    style = "color: rgba(255,255,255,0.9); font-size: 12px;"
                  )
                )
              ),

              div(
                style = "color: rgba(255,255,255,0.95); font-size: 12px; line-height: 1.5; margin-bottom: 10px;",
                "Comprehensive R-Tool for Science Mapping and Bibliometric Analysis. Documentation, tutorials, and case studies."
              ),

              div(
                icon("external-link-alt", style = "margin-right: 5px;"),
                "Visit Website",
                style = "color: white; font-size: 12px; font-weight: 600; display: inline-block; padding: 6px 12px; background: rgba(255,255,255,0.2); border-radius: 5px;"
              )
            )
          ),

          # Summer School Card
          tags$a(
            href = "https://www.bibliometrix.org/sssm",
            target = "_blank",
            style = "text-decoration: none; display: block;",
            div(
              style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); padding: 18px; border-radius: 10px; box-shadow: 0 4px 15px rgba(245, 87, 108, 0.3); transition: all 0.3s; cursor: pointer; height: 100%;",
              onmouseover = "this.style.transform='translateY(-3px)'; this.style.boxShadow='0 6px 20px rgba(245, 87, 108, 0.4)';",
              onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 4px 15px rgba(245, 87, 108, 0.3)';",

              div(
                style = "display: flex; align-items: center; margin-bottom: 10px;",
                div(
                  style = "width: 50px; height: 50px; border-radius: 50%; background: white; display: flex; align-items: center; justify-content: center; margin-right: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                  icon(
                    "graduation-cap",
                    style = "color: #f5576c; font-size: 24px;"
                  )
                ),
                div(
                  div(
                    "SSSM",
                    style = "color: white; font-size: 20px; font-weight: bold; margin-bottom: 3px;"
                  ),
                  div(
                    "Summer School & Seminars",
                    style = "color: rgba(255,255,255,0.9); font-size: 12px;"
                  )
                )
              ),

              div(
                style = "color: rgba(255,255,255,0.95); font-size: 12px; line-height: 1.5; margin-bottom: 10px;",
                "Summer School in Science Mapping. Advanced training courses, workshops, and seminars on bibliometric analysis and research evaluation."
              ),

              div(
                icon("external-link-alt", style = "margin-right: 5px;"),
                "Learn More",
                style = "color: white; font-size: 12px; font-weight: 600; display: inline-block; padding: 6px 12px; background: rgba(255,255,255,0.2); border-radius: 5px;"
              )
            )
          ),

          # K-Synth Card
          tags$a(
            href = "https://www.k-synth.com",
            target = "_blank",
            style = "text-decoration: none; display: block;",
            div(
              style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 18px; border-radius: 10px; box-shadow: 0 4px 15px rgba(102, 126, 234, 0.3); transition: all 0.3s; cursor: pointer; height: 100%;",
              onmouseover = "this.style.transform='translateY(-3px)'; this.style.boxShadow='0 6px 20px rgba(102, 126, 234, 0.4)';",
              onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 4px 15px rgba(102, 126, 234, 0.3)';",

              div(
                style = "display: flex; align-items: center; margin-bottom: 10px;",
                div(
                  style = "width: 50px; height: 50px; border-radius: 50%; background: white; display: flex; align-items: center; justify-content: center; margin-right: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                  icon(
                    "watchman-monitoring",
                    style = "color: #667eea; font-size: 24px;"
                  )
                ),
                div(
                  div(
                    "K-Synth srl",
                    style = "color: white; font-size: 20px; font-weight: bold; margin-bottom: 3px;"
                  ),
                  div(
                    "Academic Spin-Off - Univ. Naples",
                    style = "color: rgba(255,255,255,0.9); font-size: 12px;"
                  )
                )
              ),

              div(
                style = "color: rgba(255,255,255,0.95); font-size: 12px; line-height: 1.5; margin-bottom: 10px;",
                "Science-centric information & intelligence Specialist Firm. Research, consulting, and knowledge production through data science methods."
              ),

              div(
                icon("external-link-alt", style = "margin-right: 5px;"),
                "Visit K-Synth",
                style = "color: white; font-size: 12px; font-weight: 600; display: inline-block; padding: 6px 12px; background: rgba(255,255,255,0.2); border-radius: 5px;"
              )
            )
          ),

          # GitHub Card
          tags$a(
            href = "https://github.com/massimoaria/bibliometrix",
            target = "_blank",
            style = "text-decoration: none; display: block;",
            div(
              style = "background: linear-gradient(135deg, #24292e 0%, #000000 100%); padding: 18px; border-radius: 10px; box-shadow: 0 4px 15px rgba(0, 0, 0, 0.3); transition: all 0.3s; cursor: pointer; height: 100%;",
              onmouseover = "this.style.transform='translateY(-3px)'; this.style.boxShadow='0 6px 20px rgba(0, 0, 0, 0.4)';",
              onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 4px 15px rgba(0, 0, 0, 0.3)';",

              div(
                style = "display: flex; align-items: center; margin-bottom: 10px;",
                div(
                  style = "width: 50px; height: 50px; border-radius: 50%; background: white; display: flex; align-items: center; justify-content: center; margin-right: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                  icon("github", style = "color: #24292e; font-size: 24px;")
                ),
                div(
                  div(
                    "GitHub",
                    style = "color: white; font-size: 20px; font-weight: bold; margin-bottom: 3px;"
                  ),
                  div(
                    "Open Source Repository",
                    style = "color: rgba(255,255,255,0.9); font-size: 12px;"
                  )
                )
              ),

              div(
                style = "color: rgba(255,255,255,0.95); font-size: 12px; line-height: 1.5; margin-bottom: 10px;",
                "Access the source code, report issues, contribute to development, and stay updated with the latest releases."
              ),

              div(
                icon("external-link-alt", style = "margin-right: 5px;"),
                "View Repository",
                style = "color: white; font-size: 12px; font-weight: 600; display: inline-block; padding: 6px 12px; background: rgba(255,255,255,0.2); border-radius: 5px;"
              )
            )
          )
        )
      )
    ))
  })

  ## Help Menu ----
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = div(
        style = "text-align: center; margin-bottom: 10px;",
        icon("question-circle", style = "color: #3498db; font-size: 26px;"),
        span(
          " Help Menu",
          style = "font-size: 24px; font-weight: bold; color: #2c3e50; margin-left: 10px;"
        )
      ),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),

      div(
        style = "padding: 15px;",

        # Container con layout a griglia
        div(
          style = "display: grid; grid-template-columns: repeat(2, 1fr); gap: 15px;",

          # Package Tutorial Card
          tags$a(
            href = intro,
            target = "_blank",
            style = "text-decoration: none; display: block;",
            div(
              style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 18px; border-radius: 10px; box-shadow: 0 4px 15px rgba(102, 126, 234, 0.3); transition: all 0.3s; cursor: pointer; height: 100%;",
              onmouseover = "this.style.transform='translateY(-3px)'; this.style.boxShadow='0 6px 20px rgba(102, 126, 234, 0.4)';",
              onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 4px 15px rgba(102, 126, 234, 0.3)';",

              div(
                style = "display: flex; align-items: center; margin-bottom: 10px;",
                div(
                  style = "width: 50px; height: 50px; border-radius: 50%; background: white; display: flex; align-items: center; justify-content: center; margin-right: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                  icon(
                    "play-circle",
                    style = "color: #667eea; font-size: 24px;"
                  )
                ),
                div(
                  div(
                    "Package Tutorial",
                    style = "color: white; font-size: 20px; font-weight: bold; margin-bottom: 3px;"
                  ),
                  div(
                    "Getting Started Guide",
                    style = "color: rgba(255,255,255,0.9); font-size: 12px;"
                  )
                )
              ),

              div(
                style = "color: rgba(255,255,255,0.95); font-size: 12px; line-height: 1.5; margin-bottom: 10px;",
                "Learn the basics of bibliometrix with our comprehensive interactive tutorial. Perfect for beginners."
              ),

              div(
                icon("external-link-alt", style = "margin-right: 5px;"),
                "Start Tutorial",
                style = "color: white; font-size: 12px; font-weight: 600; display: inline-block; padding: 6px 12px; background: rgba(255,255,255,0.2); border-radius: 5px;"
              )
            )
          ),

          # Convert and Import Data Card
          tags$a(
            href = importData,
            target = "_blank",
            style = "text-decoration: none; display: block;",
            div(
              style = "background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); padding: 18px; border-radius: 10px; box-shadow: 0 4px 15px rgba(56, 239, 125, 0.3); transition: all 0.3s; cursor: pointer; height: 100%;",
              onmouseover = "this.style.transform='translateY(-3px)'; this.style.boxShadow='0 6px 20px rgba(56, 239, 125, 0.4)';",
              onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 4px 15px rgba(56, 239, 125, 0.3)';",

              div(
                style = "display: flex; align-items: center; margin-bottom: 10px;",
                div(
                  style = "width: 50px; height: 50px; border-radius: 50%; background: white; display: flex; align-items: center; justify-content: center; margin-right: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                  icon(
                    "file-import",
                    style = "color: #11998e; font-size: 24px;"
                  )
                ),
                div(
                  div(
                    "Import Data",
                    style = "color: white; font-size: 20px; font-weight: bold; margin-bottom: 3px;"
                  ),
                  div(
                    "Data Conversion Guide",
                    style = "color: rgba(255,255,255,0.9); font-size: 12px;"
                  )
                )
              ),

              div(
                style = "color: rgba(255,255,255,0.95); font-size: 12px; line-height: 1.5; margin-bottom: 10px;",
                "Step-by-step instructions on how to convert and import bibliographic data from various sources into bibliometrix."
              ),

              div(
                icon("external-link-alt", style = "margin-right: 5px;"),
                "View Guide",
                style = "color: white; font-size: 12px; font-weight: 600; display: inline-block; padding: 6px 12px; background: rgba(255,255,255,0.2); border-radius: 5px;"
              )
            )
          ),

          # biblioshiny Tutorial Card
          tags$a(
            href = slides,
            target = "_blank",
            style = "text-decoration: none; display: block;",
            div(
              style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); padding: 18px; border-radius: 10px; box-shadow: 0 4px 15px rgba(245, 87, 108, 0.3); transition: all 0.3s; cursor: pointer; height: 100%;",
              onmouseover = "this.style.transform='translateY(-3px)'; this.style.boxShadow='0 6px 20px rgba(245, 87, 108, 0.4)';",
              onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 4px 15px rgba(245, 87, 108, 0.3)';",

              div(
                style = "display: flex; align-items: center; margin-bottom: 10px;",
                div(
                  style = "width: 50px; height: 50px; border-radius: 50%; background: white; display: flex; align-items: center; justify-content: center; margin-right: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                  icon(
                    "file-powerpoint",
                    style = "color: #f5576c; font-size: 24px;"
                  )
                ),
                div(
                  div(
                    "biblioshiny Tutorial",
                    style = "color: white; font-size: 20px; font-weight: bold; margin-bottom: 3px;"
                  ),
                  div(
                    "Interactive Slides",
                    style = "color: rgba(255,255,255,0.9); font-size: 12px;"
                  )
                )
              ),

              div(
                style = "color: rgba(255,255,255,0.95); font-size: 12px; line-height: 1.5; margin-bottom: 10px;",
                "Complete walkthrough of biblioshiny's web interface with practical examples and best practices for your analysis."
              ),

              div(
                icon("external-link-alt", style = "margin-right: 5px;"),
                "View Slides",
                style = "color: white; font-size: 12px; font-weight: 600; display: inline-block; padding: 6px 12px; background: rgba(255,255,255,0.2); border-radius: 5px;"
              )
            )
          ),

          # Version Info Card
          div(
            style = "background: linear-gradient(135deg, #3498db 0%, #2980b9 100%); padding: 18px; border-radius: 10px; box-shadow: 0 4px 15px rgba(52, 152, 219, 0.3); height: 100%;",

            div(
              style = "display: flex; align-items: center; margin-bottom: 10px;",
              div(
                style = "width: 50px; height: 50px; border-radius: 50%; background: white; display: flex; align-items: center; justify-content: center; margin-right: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                icon("info-circle", style = "color: #3498db; font-size: 24px;")
              ),
              div(
                div(
                  "Version Info",
                  style = "color: white; font-size: 20px; font-weight: bold; margin-bottom: 3px;"
                ),
                div(
                  "Current Release",
                  style = "color: rgba(255,255,255,0.9); font-size: 12px;"
                )
              )
            ),

            div(
              style = "color: rgba(255,255,255,0.95); font-size: 12px; line-height: 1.5; margin-bottom: 10px;",
              paste0(
                "You are currently using biblioshiny version ",
                biblioshinyVersion,
                ". Check our GitHub repository for the latest updates and release notes."
              )
            ),

            div(
              icon("code-branch", style = "margin-right: 5px;"),
              paste0("v", biblioshinyVersion),
              style = "color: white; font-size: 12px; font-weight: 600; display: inline-block; padding: 6px 12px; background: rgba(255,255,255,0.2); border-radius: 5px;"
            )
          )
        )
      )
    ))
  })

  ## NOTIFICATION ITEM ----
  # output$notificationMenu <- renderMenu({
  #   notifTot <- notifications()
  #   values$nots <- apply(notifTot, 1, function(row) {
  #     ## extract href from messages
  #     if (is.na(row[["href"]])) {
  #       href <- NULL
  #     } else {
  #       href <- paste(
  #         "javascript:void(window.open('",
  #         row[["href"]],
  #         "', '_blank'))",
  #         sep = ""
  #       )
  #     }
  #
  #     ## add bold to new messages and split the long ones in two rows
  #     if (row[["status"]] == "danger") {
  #       ### new messages
  #       textRows <- paste("tags$strong('", row[["nots"]], "')", sep = "")
  #       textRows <- strsplit(
  #         substr(textRows, 1, 85),
  #         "(?<=.{48})",
  #         perl = TRUE
  #       )[[1]]
  #       if (length(textRows) > 1) {
  #         textRows <- paste(
  #           "tags$div(",
  #           textRows[1],
  #           "',tags$br(),'",
  #           textRows[2],
  #           ")",
  #           sep = ""
  #         )
  #       } else {
  #         textRows <- paste("tags$div(", textRows, ")", sep = "")
  #       }
  #     } else {
  #       ## old messages
  #       textRows <- strsplit(
  #         substr(row[["nots"]], 1, 70),
  #         "(?<=.{35})",
  #         perl = TRUE
  #       )[[1]]
  #       if (length(textRows) > 1) {
  #         textRows <- paste(
  #           "tags$div('",
  #           textRows[1],
  #           "',tags$br(),'",
  #           textRows[2],
  #           "')",
  #           sep = ""
  #         )
  #       } else {
  #         textRows <- paste("tags$div('", textRows, "')", sep = "")
  #       }
  #     }
  #
  #     notificationItem(
  #       text = eval(parse(text = textRows)),
  #       icon = if (row[["status"]] == "danger") {
  #         fa_i(name = "envelope")
  #       } else {
  #         fa_i(name = "envelope-open")
  #       },
  #       status = row[["status"]],
  #       href = href
  #     )
  #   })
  #
  #   if ("danger" %in% notifTot[["status"]]) {
  #     badge = "danger"
  #     icon_name = "envelope"
  #   } else {
  #     badge = NULL
  #     icon_name = "envelope-open"
  #   }
  #
  #   dropdownMenu(
  #     type = "notifications",
  #     .list = values$nots,
  #     headerText = "",
  #     badgeStatus = NULL,
  #     icon = fa_i(name = icon_name)
  #   )
  # })

  ## SIDEBAR MENU ----

  # Initialize menu IDs when app starts
  observeEvent(
    input$menu_init_trigger,
    {
      session$sendCustomMessage("initMenuIds", list())
    },
    ignoreInit = FALSE,
    once = TRUE
  )

  observeEvent(input$applyLoad, {
    updateTabItems(session, "sidebarmenu", "loadData")
  })

  observeEvent(input$oaFetchData, {
    updateTabItems(session, "sidebarmenu", "openalexMenu")
  })

  observeEvent(input$pmFetchData, {
    updateTabItems(session, "sidebarmenu", "pubmedMenu")
  })

  # Settings button handler
  observeEvent(
    input$go_to_settings,
    {
      updateTabItems(session, "sidebarmenu", "settings")
    },
    ignoreInit = TRUE
  )

  # observeEvent(input$apiApply, {
  #   updateTabItems(session, "sidebarmenu", "gathData")
  # })

  observeEvent(values$missTags, {
    switch(
      values$loadMenu,
      "load" = {
        updateTabItems(session, "sidebarmenu", "loadData")
      },
      "merge" = {
        updateTabItems(session, "sidebarmenu", "mergeData")
      },
      "openalex_api" = {
        updateTabItems(session, "sidebarmenu", "openalexMenu")
      },
      "pubmed_api" = {
        updateTabItems(session, "sidebarmenu", "pubmedMenu")
      }
    )
    values$loadMenu <- NA
  })

  observeEvent(input$applyMerge, {
    updateTabItems(session, "sidebarmenu", "mergeData")
  })

  # Flag to control menu visibility
  output$dataLoaded <- reactive({
    return(!is.null(values$M) && nrow(values$M) > 0)
  })
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)

  ## observe Gemini copy2clipboard button
  observeEvent(input$copy_btn, {
    content <- geminiSave(values, input$sidebarmenu)

    content_to_send <- unname(as.character(content))
    #copy_to_clipboard(content)
    session$sendCustomMessage("copy_to_clipboard_js", content_to_send)
  })

  ## observe Gemini Save button
  output$save_btn <- downloadHandler(
    filename = function() {
      paste0("BiblioAI_", input$sidebarmenu, ".txt")
    },
    content <- function(file) {
      txtOutput <- geminiSave(values, input$sidebarmenu)
      writeLines(txtOutput, con = file)
    },
    contentType = "txt"
  )

  ## observe gemini generate button

  observeEvent(input$gemini_btn, {
    values$gemini_additional <- input$gemini_additional ## additional info to Gemini prompt
    activeTab <- input$sidebarmenu
    values <- geminiWaitingMessage(values, activeTab)

    # Sync: prepare prompt and images
    prep <- geminiPrepareAll(values, activeTab, input)
    if (!prep$can_proceed) {
      values[[prep$field]] <- prep$error_msg
      return()
    }

    # Snapshot for future (no reactive refs)
    prompt <- prep$prompt
    image_paths <- prep$image_paths
    model <- values$gemini_api_model
    output_size <- values$gemini_output_size
    field <- prep$field

    # Async: API call in background
    promises::future_promise(
      {
        gemini_ai(
          image = image_paths,
          prompt = prompt,
          model = model,
          outputSize = output_size
        )
      },
      seed = TRUE
    ) %...>%
      (function(result) {
        values[[field]] <- result
        geminiCleanupFiles(image_paths)
      }) %...!%
      (function(err) {
        values[[field]] <- paste("Error:", conditionMessage(err))
        geminiCleanupFiles(image_paths)
      })

    NULL # Don't block the observer
  })

  observeEvent(input$applyLoad, {
    output$collection_descriptionUI <- renderUI({
      textAreaInput(
        inputId = "collection_description_merge",
        label = "Brief description about your collection",
        placeholder = "Please provide a brief description of your bibliographic collection (e.g., type of content, domain, research hypotheses, timespan) to improve prompts for the BIBLIO AI Assistant.\n\nExample: The corpus consists of 150 academic articles from biomedical journals published between 2015 and 2020...",
        value = NULL,
        rows = 3,
        width = "100%"
      )
    })
  })

  observeEvent(values$M, {
    updateTextAreaInput(
      session = getDefaultReactiveDomain(),
      inputId = "collection_description",
      label = "Brief description about your collection",
      value = values$collection_description
    )
    updateTextAreaInput(
      session = getDefaultReactiveDomain(),
      inputId = "collection_description_merge",
      label = "Brief description about your collection",
      value = values$collection_description
    )
  })

  observeEvent(
    eventExpr = {
      input$collection_description
    },
    handlerExpr = {
      if (
        input$collection_description != "" &
          nchar(input$collection_description) > 1
      ) {
        values$collection_description <- input$collection_description
      }
    },
    ignoreNULL = TRUE
  )

  observeEvent(
    eventExpr = {
      input$collection_description_merge
    },
    handlerExpr = {
      if (
        input$collection_description_merge != "" &
          nchar(input$collection_description_merge) > 1
      ) {
        values$collection_description <- input$collection_description_merge
      }
    },
    ignoreNULL = TRUE
  )

  ## Load Menu ----

  DATAloading <- eventReactive(input$applyLoad, {
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    if (input$load == "demo") {
      data(management, package = "bibliometrixData")
      values = initial(values)
      row.names(management) <- management$SR
      management <- management %>% mergeKeywords(force = T)
      values$M <- management
      values$Morig = management
      values$SCdf <- wcTable(management)
      values$COdf <- countryTable(management)
      values$Histfield = "NA"
      values$results = list("NA")
      values$rest_sidebar <- TRUE
      values$missingdf <- df <- missingData(values$M)$mandatoryTags
      values$missTags <- NULL
      # values$menu <- menuList(values)
      updateMenuVisibility(session, values)
      values$collection_description <- 'A collection of scientific articles about the use of bibliometric approaches in business and management disciplines. Period: 1985–2020. This collection was identified by retrieving all documents indexed under the subject categories “Management” and "Business" that contain at least one of the following terms in their topic fields: “science map”, "bibliometric*".'

      showModal(missingModal(session))
      return()
    }
    inFile <- input$file1

    if (!is.null(inFile) & input$load == "import") {
      ext <- tools::file_ext(inFile$datapath)

      ## ---- max.rows check ----
      if (is.finite(max_rows)) {
        # Derive format from original filename (inFile$name), not from temp path
        orig_ext <- tolower(tools::file_ext(inFile$name[1]))
        fmt <- switch(
          orig_ext,
          txt = if (input$dbsource == "pubmed") "pubmed" else "plaintext",
          csv = "csv",
          bib = "bibtex",
          ciw = "plaintext",
          xlsx = "excel",
          zip = NULL, # countRecords will unzip and auto-detect
          NULL
        )
        # For ZIP files, we need to give countRecords a file with .zip extension
        file_to_check <- inFile$datapath[1]
        if (orig_ext == "zip") {
          zip_tmp <- paste0(file_to_check, ".zip")
          file.copy(file_to_check, zip_tmp)
          file_to_check <- zip_tmp
        }
        n_records <- tryCatch(
          countRecords(file_to_check, dbsource = input$dbsource, format = fmt),
          error = function(e) NA_integer_
        )
        if (orig_ext == "zip" && file.exists(zip_tmp)) {
          file.remove(zip_tmp)
        }

        if (!is.na(n_records) && n_records > max_rows) {
          showModal(modalDialog(
            title = tags$strong(
              icon("exclamation-triangle"),
              " File too large"
            ),
            tags$p(
              sprintf(
                "The selected file contains %s records, which exceeds the maximum allowed limit of %s.",
                format(n_records, big.mark = ","),
                format(as.integer(max_rows), big.mark = ",")
              )
            ),
            tags$p(
              "Please reduce the number of records in your file and try again."
            ),
            footer = modalButton("OK"),
            easyClose = TRUE
          ))
          return(NULL)
        }
      }
      ## ---- end max.rows check ----

      switch(
        input$dbsource,
        isi = {
          switch(
            ext,
            ###  WoS ZIP Files
            zip = {
              # Crea un percorso per una cartella temporanea specifica per questa sessione
              temp_exdir <- file.path(getWD(), "unzipped_files")

              # Estrae i file nella cartella temporanea
              # (exdir viene creata automaticamente se non esiste)
              D <- utils::unzip(inFile$datapath, exdir = temp_exdir)
              #D <- utils::unzip(inFile$datapath)
              withProgress(message = 'Conversion in progress', value = 0, {
                M <- convert2df(
                  D,
                  dbsource = input$dbsource,
                  format = formatDB(D)
                )
                M <- authorNameFormat(M, input$authorName)
              })
              on.exit(unlink(temp_exdir, recursive = TRUE))
            },
            ### WoS Txt/Bib Files
            {
              withProgress(message = 'Conversion in progress', value = 0, {
                M <- convert2df(
                  inFile$datapath,
                  dbsource = input$dbsource,
                  format = formatDB(inFile$datapath)
                )
                M <- authorNameFormat(M, input$authorName)
              })
            }
          )
        },
        scopus = {
          switch(
            ext,
            ###  Scopus ZIP Files
            zip = {
              # Crea un percorso per una cartella temporanea specifica per questa sessione
              temp_exdir <- file.path(getWD(), "unzipped_files")

              # Estrae i file nella cartella temporanea
              # (exdir viene creata automaticamente se non esiste)
              D <- utils::unzip(inFile$datapath, exdir = temp_exdir)

              withProgress(message = 'Conversion in progress', value = 0, {
                # Usiamo i percorsi dei file estratti (contenuti in D)
                M <- convert2df(
                  D,
                  dbsource = input$dbsource,
                  format = formatDB(D)
                )

                M <- authorNameFormat(M, input$authorName)

                if (formatDB(D) == "csv" & input$authorName == "AF") {
                  M <- AuthorNameMerge(M)
                }
              })

              on.exit(unlink(temp_exdir, recursive = TRUE))
            },
            ### Scopus CSV/Bib Files
            csv = {
              withProgress(message = 'Conversion in progress', value = 0, {
                M <- convert2df(
                  inFile$datapath,
                  dbsource = input$dbsource,
                  format = "csv"
                )
                M <- authorNameFormat(M, input$authorName)
                if (input$authorName == "AF") M <- AuthorNameMerge(M)
              })
            },
            bib = {
              withProgress(message = 'Conversion in progress', value = 0, {
                M <- convert2df(
                  inFile$datapath,
                  dbsource = input$dbsource,
                  format = "bibtex"
                )
                M <- authorNameFormat(M, input$authorName)
              })
            }
          )
        },
        openalex = {
          withProgress(message = 'Conversion in progress', value = 0, {
            M <- convert2df(
              inFile$datapath,
              dbsource = input$dbsource,
              format = "csv"
            )
          })

          # Resolve cited references if requested
          if (isTRUE(input$importFetchRefs)) {
            # Check if CR column has actual reference IDs (old format) or is empty/NA (new format)
            has_cr_ids <- "CR" %in% names(M) && any(!is.na(M$CR) & M$CR != "")
            only_multiple <- isTRUE(input$importRefsFilter == "multiple")

            showModal(modalDialog(
              title = "Resolving Cited References",
              div(
                div(
                  style = "text-align: center; margin: 20px 0;",
                  icon("spinner", class = "fa-spin fa-3x")
                ),
                div(
                  id = "import-ref-progress",
                  style = "text-align: center; font-size: 14px;",
                  if (has_cr_ids) {
                    "Collecting reference IDs..."
                  } else {
                    "Fetching references from OpenAlex API..."
                  }
                ),
                div(
                  style = "margin-top: 10px; background-color: #e9ecef; border-radius: 4px; height: 20px; width: 100%;",
                  div(
                    id = "import-ref-bar",
                    style = "background-color: #007bff; height: 100%; border-radius: 4px; width: 0%; transition: width 0.3s ease;"
                  )
                ),
                div(
                  id = "import-ref-pct",
                  style = "text-align: center; margin-top: 5px; font-size: 12px; color: #666;",
                  "0%"
                )
              ),
              footer = NULL,
              easyClose = FALSE
            ))

            tryCatch(
              {
                if (has_cr_ids) {
                  # Old format: CR column contains OpenAlex reference IDs
                  all_cr <- M$CR[!is.na(M$CR) & M$CR != ""]
                  all_ref_ids <- unique(trimws(unlist(strsplit(all_cr, ";"))))
                  all_ref_ids <- all_ref_ids[grepl(
                    "^https://openalex\\.org/W",
                    all_ref_ids
                  )]

                  if (length(all_ref_ids) > 0) {
                    shinyjs::html(
                      "import-ref-progress",
                      sprintf(
                        "Resolving %s unique references...",
                        format(length(all_ref_ids), big.mark = ",")
                      )
                    )

                    oa_data_fake <- lapply(seq_len(nrow(M)), function(i) {
                      cr <- M$CR[i]
                      if (is.na(cr) || cr == "") {
                        list(referenced_works = character(0))
                      } else {
                        refs <- trimws(unlist(strsplit(cr, ";")))
                        refs <- refs[grepl("^https://openalex\\.org/W", refs)]
                        list(referenced_works = refs)
                      }
                    })

                    ref_lookup <- resolve_openalex_references(
                      oa_data_fake,
                      progress_id = "import-ref-progress",
                      progress_bar_id = "import-ref-bar",
                      progress_pct_id = "import-ref-pct",
                      only_multiple = only_multiple
                    )

                    if (length(ref_lookup) > 0) {
                      shinyjs::html(
                        "import-ref-progress",
                        "Building citation strings..."
                      )
                      cr_col <- build_cr_column(oa_data_fake, ref_lookup)
                      M$CRids <- M$CR
                      M$CR <- cr_col
                    }
                  }
                } else {
                  # New format: no CR in CSV, fetch referenced_works via API using id_oa
                  work_ids <- M$id_oa[!is.na(M$id_oa) & M$id_oa != ""]
                  if (length(work_ids) > 0) {
                    shinyjs::html(
                      "import-ref-progress",
                      sprintf(
                        "Fetching referenced works for %s publications...",
                        format(length(work_ids), big.mark = ",")
                      )
                    )

                    # Fetch referenced_works for each publication in batches of 50
                    batch_size <- 50
                    n_batches <- ceiling(length(work_ids) / batch_size)
                    ref_map <- setNames(
                      vector("list", length(work_ids)),
                      work_ids
                    )

                    for (b in seq_len(n_batches)) {
                      start_idx <- (b - 1) * batch_size + 1
                      end_idx <- min(b * batch_size, length(work_ids))
                      batch_ids <- work_ids[start_idx:end_idx]

                      pct <- round(b / n_batches * 50) # first half: 0-50%
                      shinyjs::runjs(sprintf(
                        "$('#import-ref-bar').css('width', '%d%%');",
                        pct
                      ))
                      shinyjs::html(
                        "import-ref-pct",
                        sprintf(
                          "Fetching references... batch %d/%d (%d%%)",
                          b,
                          n_batches,
                          pct
                        )
                      )

                      tryCatch(
                        {
                          result <- openalexR::oa_fetch(
                            entity = "works",
                            openalex_id = paste(batch_ids, collapse = "|"),
                            output = "list",
                            verbose = FALSE
                          )
                          if (is.list(result)) {
                            for (work in result) {
                              wid <- gsub("https://openalex.org/", "", work$id)
                              if (
                                !is.null(wid) && !is.null(work$referenced_works)
                              ) {
                                ref_map[[wid]] <- work$referenced_works
                              }
                            }
                          }
                        },
                        error = function(e) {
                          # Skip failed batches silently
                        }
                      )
                    }

                    # Build oa_data_fake structure from fetched referenced_works
                    oa_data_fake <- lapply(seq_len(nrow(M)), function(i) {
                      refs <- ref_map[[M$id_oa[i]]]
                      if (is.null(refs) || length(refs) == 0) {
                        list(referenced_works = character(0))
                      } else {
                        list(referenced_works = refs)
                      }
                    })

                    # Collect all unique ref IDs and resolve them
                    all_ref_ids <- unique(unlist(lapply(
                      oa_data_fake,
                      function(w) w$referenced_works
                    )))
                    all_ref_ids <- all_ref_ids[
                      !is.na(all_ref_ids) & all_ref_ids != ""
                    ]

                    if (length(all_ref_ids) > 0) {
                      shinyjs::html(
                        "import-ref-progress",
                        sprintf(
                          "Resolving %s unique references...",
                          format(length(all_ref_ids), big.mark = ",")
                        )
                      )

                      ref_lookup <- resolve_openalex_references(
                        oa_data_fake,
                        progress_id = "import-ref-progress",
                        progress_bar_id = "import-ref-bar",
                        progress_pct_id = "import-ref-pct",
                        only_multiple = only_multiple
                      )

                      if (length(ref_lookup) > 0) {
                        shinyjs::html(
                          "import-ref-progress",
                          "Building citation strings..."
                        )
                        cr_col <- build_cr_column(oa_data_fake, ref_lookup)
                        # Save OpenAlex IDs for direct citation analysis (histNetwork)
                        M$CRids <- vapply(
                          oa_data_fake,
                          function(w) {
                            ids <- gsub(
                              "https://openalex.org/",
                              "",
                              w$referenced_works
                            )
                            if (length(ids) == 0) {
                              NA_character_
                            } else {
                              paste(ids, collapse = ";")
                            }
                          },
                          character(1)
                        )
                        M$CR <- cr_col
                        M$NR <- vapply(
                          oa_data_fake,
                          function(w) length(w$referenced_works),
                          integer(1)
                        )
                      }
                    }
                  }
                }

                removeModal()
              },
              error = function(e) {
                removeModal()
                showModal(modalDialog(
                  title = "Warning",
                  paste(
                    "Reference resolution failed:",
                    e$message,
                    "\nThe collection was imported without resolved references."
                  ),
                  easyClose = TRUE,
                  footer = modalButton("OK")
                ))
              }
            )
          }
        },
        openalex_api = {
          M <- convert2df(
            inFile$datapath,
            dbsource = input$dbsource,
            format = "api"
          )
        },
        lens = {
          switch(
            ext,
            ###  Lens.org ZIP Files
            zip = {
              # Crea un percorso per una cartella temporanea specifica per questa sessione
              temp_exdir <- file.path(getWD, "unzipped_files")
              # Estrae i file nella cartella temporanea
              # (exdir viene creata automaticamente se non esiste)
              D <- utils::unzip(inFile$datapath, exdir = temp_exdir)
              # D <- utils::unzip(inFile$datapath)
              withProgress(message = 'Conversion in progress', value = 0, {
                M <- convert2df(
                  D,
                  dbsource = input$dbsource,
                  format = formatDB(D)
                )
              })
              on.exit(unlink(temp_exdir, recursive = TRUE))
            },
            ### Lens.org CSV Files
            {
              withProgress(message = 'Conversion in progress', value = 0, {
                M <- convert2df(
                  inFile$datapath,
                  dbsource = input$dbsource,
                  format = formatDB(inFile$datapath)
                )
              })
            }
          )
        },
        cochrane = {
          switch(
            ext,
            ###  Cochrane ZIP Files
            zip = {
              # D <- utils::unzip(inFile$datapath)
              # Crea un percorso per una cartella temporanea specifica per questa sessione
              temp_exdir <- file.path(getWD, "unzipped_files")

              # Estrae i file nella cartella temporanea
              # (exdir viene creata automaticamente se non esiste)
              D <- utils::unzip(inFile$datapath, exdir = temp_exdir)
              withProgress(message = 'Conversion in progress', value = 0, {
                M <- convert2df(
                  D,
                  dbsource = input$dbsource,
                  format = formatDB(D)
                )
              })
              on.exit(unlink(temp_exdir, recursive = TRUE))
            },
            ### Cochrane txt files
            {
              withProgress(message = 'Conversion in progress', value = 0, {
                M <- convert2df(
                  inFile$datapath,
                  dbsource = input$dbsource,
                  format = "plaintext"
                )
              })
            }
          )
        },
        pubmed = {
          switch(
            ext,
            ###  Pubmed ZIP Files
            zip = {
              # D <- utils::unzip(inFile$datapath)
              # Crea un percorso per una cartella temporanea specifica per questa sessione
              temp_exdir <- file.path(getWD(), "unzipped_files")

              # Estrae i file nella cartella temporanea
              # (exdir viene creata automaticamente se non esiste)
              D <- utils::unzip(inFile$datapath, exdir = temp_exdir)
              withProgress(message = 'Conversion in progress', value = 0, {
                M <- convert2df(D, dbsource = input$dbsource, format = "pubmed")
              })
              on.exit(unlink(temp_exdir, recursive = TRUE))
            },
            ### Pubmed txt Files
            txt = {
              withProgress(message = 'Conversion in progress', value = 0, {
                M <- convert2df(
                  inFile$datapath,
                  dbsource = input$dbsource,
                  format = "pubmed"
                )
              })
            }
          )
        },
        dimensions = {
          switch(
            ext,
            ###  Dimensions ZIP Files
            zip = {
              # D = utils::unzip(inFile$datapath)
              # Crea un percorso per una cartella temporanea specifica per questa sessione
              temp_exdir <- file.path(getWD(), "unzipped_files")

              # Estrae i file nella cartella temporanea
              # (exdir viene creata automaticamente se non esiste)
              D <- utils::unzip(inFile$datapath, exdir = temp_exdir)
              withProgress(message = 'Conversion in progress', value = 0, {
                M <-
                  convert2df(D, dbsource = input$dbsource, format = formatDB(D))
              })
              on.exit(unlink(temp_exdir, recursive = TRUE))
            },
            ### Dimensions Xlsx/csv Files
            xlsx = {
              withProgress(message = 'Conversion in progress', value = 0, {
                M <-
                  convert2df(
                    inFile$datapath,
                    dbsource = "dimensions",
                    format = "excel"
                  )
              })
            },
            csv = {
              withProgress(message = 'Conversion in progress', value = 0, {
                M <-
                  convert2df(
                    inFile$datapath,
                    dbsource = "dimensions",
                    format = "csv"
                  )
              })
            }
          )
        }
      )
    } else if (!is.null(inFile) & input$load == "load") {
      ext <- tolower(tools::file_ext(inFile$datapath))

      ## ---- max.rows check for load ----
      if (is.finite(max_rows)) {
        orig_ext_load <- tolower(tools::file_ext(inFile$name[1]))
        fmt <- switch(
          orig_ext_load,
          xlsx = "excel",
          rdata = "rdata",
          rda = "rdata",
          rds = "rds",
          NULL
        )
        n_records <- tryCatch(
          countRecords(inFile$datapath[1], format = fmt),
          error = function(e) NA_integer_
        )
        if (!is.na(n_records) && n_records > max_rows) {
          showModal(modalDialog(
            title = tags$strong(
              icon("exclamation-triangle"),
              " File too large"
            ),
            tags$p(
              sprintf(
                "The selected file contains %s records, which exceeds the maximum allowed limit of %s.",
                format(n_records, big.mark = ","),
                format(as.integer(max_rows), big.mark = ",")
              )
            ),
            tags$p(
              "Please reduce the number of records in your file and try again."
            ),
            footer = modalButton("OK"),
            easyClose = TRUE
          ))
          return(NULL)
        }
      }
      ## ---- end max.rows check ----

      switch(
        ext,
        ### excel format
        xlsx = {
          M <- readxl::read_excel(inFile$datapath, col_types = "text") %>%
            as.data.frame()
          M$PY <- as.numeric(M$PY)
          M$TC <- as.numeric(M$TC)
          class(M) <- c("bibliometrixDB", "data.frame")
          ### M row names
          ### identify duplicated SRs
          SR <- M$SR
          tab <- table(SR)
          tab2 <- table(tab)
          ind <- as.numeric(names(tab2))
          ind <- ind[which(ind > 1)]
          if (length(ind) > 0) {
            for (i in ind) {
              indice = names(which(tab == i))
              for (j in indice) {
                indice2 <- which(SR == j)
                SR[indice2] <- paste(
                  SR[indice2],
                  as.character(1:length(indice2)),
                  sep = " "
                )
              }
            }
          }
          M$SR <- SR
          row.names(M) <- SR
        },
        ### RData format
        rdata = {
          M <- smart_load(inFile$datapath)
        },
        rda = {
          M <- smart_load(inFile$datapath)
        },
        rds = {
          M <- readRDS(inFile$datapath)
        }
      )
    } else if (is.null(inFile)) {
      return(NULL)
    }

    values = initial(values)
    ## remove not useful columns
    ind <- which(substr(names(M), 1, 2) == "X.")
    if (length(ind) > 0) {
      M <- M[, -ind]
    }
    ##
    M <- M %>% mergeKeywords(force = F)
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
      values$loadMenu <- "load"
      showModal(missingModal(session))
    }
  })

  output$contents <- renderUI({
    DATAloading()
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
      scrollX = TRUE,
      scrollY = TRUE
    )
    # out <- tryCatch(
    #   {
    #     HTML(htmlBoxFormat(
    #       MData,
    #       nrow = 3,
    #       filename = "Table",
    #       pagelength = TRUE,
    #       left = NULL,
    #       right = NULL,
    #       numeric = NULL,
    #       dom = TRUE,
    #       size = '70%',
    #       filter = "top",
    #       columnShort = NULL,
    #       columnSmall = NULL,
    #       round = 2,
    #       title = "",
    #       button = FALSE,
    #       escape = FALSE,
    #       selection = FALSE,
    #       scrollX = TRUE,
    #       scrollY = TRUE
    #     ))
    #   },
    #   error = function(e) {
    #     # In caso di errore nella tua funzione, mostra l'errore nel box invece di bloccare tutto
    #     tags$div(
    #       style = "color:red; padding:20px;",
    #       paste("Errore htmlBoxFormat:", e$message)
    #     )
    #   }
    # )
  })

  ## Openalex API Query Sample Size ----
  openAlexServer(input, output, session, values)

  ## Pubmed API Query Sample Size ----
  pubmedServer(input, output, session, values)

  ## Merge Menu ----
  DATAmerging <- eventReactive(input$applyMerge, {
    inFile <- input$fileMerge

    if (!is.null(inFile)) {
      #save(inFile,file="prova.rdata")
      M <- merge_files(inFile)
    } else if (is.null(inFile)) {
      return(NULL)
    }

    values = initial(values)
    ## remove not useful columns
    ind <- which(substr(names(M), 1, 2) == "X.")
    if (length(ind) > 0) {
      M <- M[, -ind]
    }
    ##

    values$M <- M
    values$Morig = M
    values$SCdf <- wcTable(M)
    values$COdf <- countryTable(M)
    values$nMerge <- attr(M, "nMerge")
    values$Histfield = "NA"
    values$results = list("NA")
    if (ncol(values$M) > 1) {
      values$rest_sidebar <- TRUE
    }
    if (ncol(values$M) > 1) {
      values$loadMenu <- "merge"
      showModal(missingModal(session))
    }
  })

  output$contentsMerge <- renderUI({
    DATAmerging()
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

  observeEvent(input$applyMerge, {
    output$collection_description_mergeUI <- renderUI({
      textAreaInput(
        inputId = "collection_description",
        label = "Brief description about your collection",
        placeholder = "Please provide a brief description of your bibliographic collection (e.g., source, type of content, domain) to improve prompts for the BIBLIO AI Assistant.\n\nExample: The corpus consists of 150 academic articles from biomedical journals published between 2015 and 2020...",
        value = NULL,
        rows = 3,
        width = "100%"
      )
    })
  })

  ### Missing Data in Metadata ----
  output$missingDataTable <- renderUI({
    values$missingdf <- df <- missingData(values$M)$mandatoryTags
    values$missTags <- df$tag[df$missing_pct > 50]
    # values$menu <- menuList(values)
    updateMenuVisibility(session, values)

    names(df) <- c(
      "Metadata",
      "Description",
      "Missing Counts",
      "Missing %",
      "Status"
    )

    # Pre-processing del dataframe per aggiungere colori e formattazione
    df_formatted <- df %>%
      mutate(
        # Arrotonda la colonna "Missing %" a 2 decimali
        `Missing %` = round(`Missing %`, 2),

        # Aggiungi colori di background alla colonna Status basandosi sul valore
        Status = case_when(
          Status == "Completely missing" ~
            paste0(
              '<span style="display: block; background-color: #b22222; color: white; padding: 5px; text-align: center; border-radius: 3px;">',
              Status,
              '</span>'
            ),
          Status == "Critical" ~
            paste0(
              '<span style="display: block; background-color: #f08080; color: white; padding: 5px; text-align: center; border-radius: 3px;">',
              Status,
              '</span>'
            ),
          Status == "Poor" ~
            paste0(
              '<span style="display: block; background-color: lightgrey; color: black; padding: 5px; text-align: center; border-radius: 3px;">',
              Status,
              '</span>'
            ),
          Status == "Acceptable" ~
            paste0(
              '<span style="display: block; background-color: #f0e68c; color: black; padding: 5px; text-align: center; border-radius: 3px;">',
              Status,
              '</span>'
            ),
          Status == "Good" ~
            paste0(
              '<span style="display: block; background-color: #90ee90; color: black; padding: 5px; text-align: center; border-radius: 3px;">',
              Status,
              '</span>'
            ),
          Status == "Excellent" ~
            paste0(
              '<span style="display: block; background-color: #32cd32; color: white; padding: 5px; text-align: center; border-radius: 3px;">',
              Status,
              '</span>'
            ),
          TRUE ~ Status
        )
      )

    values$missingdf_formatted <- df_formatted

    # Crea la tabella con htmlBoxFormat
    values$missingDataTable <- renderBibliobox(
      df = df_formatted,
      nrow = nrow(df_formatted), # Mostra tutte le righe
      escape = FALSE, # Permette HTML nelle celle
      scrollX = TRUE, # Scroll orizzontale
      dom = FALSE, # Nasconde controlli DOM extra
      filter = "none", # Nessun filtro
      pagelength = FALSE, # Nasconde menu lunghezza pagina
      button = FALSE, # Nessun bottone export
      round = 2 # Arrotondamento generale a 2 decimali
    )
    values$missingDataTable
  })

  observeEvent(input$missingMessage, {
    tag <- values$missingdf$description[
      values$missingdf$status %in% c("Critical", "Completely missing")
    ]
    if (length(values$out) > 0) {
      text <- paste(
        "The following analyses could not be performed: <br><br>",
        paste("- ", "<em>", values$out, "</em>", "<br>", collapse = ""),
        "<br>These menu will be hidden in the Biblioshiny dashboard!",
        collapse = ""
      )
      type <- "warning"
    } else {
      text <- "Your metadata have no critical issues"
      type <- "success"
    }

    show_alert(
      title = NULL,
      #text = HTML(paste("Analyses that require the following information:<br>",paste("- ",tag,"<br>", collapse=""),"cannot be performed!",collapse="")),
      text = tagList(
        div(
          h4(HTML(text)),
          style = "text-align:left"
        )
      ),
      type = type,
      size = "s",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      btn_labels = "OK",
      btn_colors = "#1d8fe1",
      timer = NULL,
      imageUrl = "",
      animation = TRUE
    )
  })

  output$missingTitle <- renderUI({
    ndocs <- nrow(values$M)
    if ("DB_Original" %in% names(values$M)) {
      DB <- paste0(length(unique(values$M$DB_Original)), " DBs")
      txt1 <- paste0(
        "Completeness of metadata -- ",
        strong(ndocs),
        " docs merged from ",
        DB
      )
      txt2 <- paste0(
        "Original size ",
        strong(values$nMerge),
        " docs -- Deleted ",
        strong(values$nMerge - ndocs),
        " duplicated docs"
      )
    } else {
      DB <- bibliometrix:::firstup(values$M$DB[1])
      txt1 <- paste0(
        "Completeness of metadata -- ",
        strong(ndocs),
        " docs from ",
        strong(DB)
      )
      txt2 <- ""
    }

    tagList(
      div(
        h3(HTML(txt1)),
        br(),
        h4(HTML(txt2)),
        style = "text-align:center"
      )
    )
  })

  missingModal <- function(session) {
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
          inputId = "missingDataSave",
          icon = icon("camera", lib = "glyphicon")
        ),
        modalButton(label = "Close")
      ),
    )
  }

  observeEvent(input$missingReport, {
    if (!is.null(values$missingdf_formatted)) {
      sheetname <- "MissingData"
      list_df <- list(values$missingdf_formatted[, -5])
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      values$fileMDT <- screenSh(
        values$missingdf_formatted,
        zoom = 2,
        type = "df2html"
      )
      values$list_file <- rbind(
        values$list_file,
        c(sheetname, values$fileMDT, res$col)
      )
      popUp(title = "Missing Data Table", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ## export functions ----
  output$collection.save <- downloadHandler(
    filename = function() {
      paste(
        "Bibliometrix-Export-File-",
        Sys.Date(),
        ".",
        input$save_file,
        sep = ""
      )
    },
    content <- function(file) {
      tr <- FALSE
      if ("CR" %in% names(values$M)) {
        tr <- (sum(nchar(values$M$CR) > 32767, na.rm = TRUE)) > 0
      }

      if (tr & input$save_file == "xlsx") {
        show_alert(
          text = tags$span(
            tags$h4(
              "Some documents have too long a list of references that cannot be saved in excel (>32767 characters).",
              style = "color: firebrick;"
            ),
            tags$br(),
            tags$h4(
              "Data in the column CR could be truncated.",
              style = "color: firebrick;"
            )
          ),
          title = "Please save the collection using the 'RData' format",
          type = "warning",
          width = "50%", ##NEW ----
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          html = FALSE,
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          btn_labels = "OK",
          btn_colors = "#913333",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
        suppressWarnings(openxlsx::write.xlsx(values$M, file = file))
      } else {
        switch(
          input$save_file,
          xlsx = {
            suppressWarnings(openxlsx::write.xlsx(values$M, file = file))
          },
          RData = {
            M = values$M
            save(M, file = file)
          }
        )
      }
    },
    contentType = input$save_file
  )

  output$collection.saveMerge <- downloadHandler(
    filename = function() {
      paste(
        "Bibliometrix-Export-File-",
        Sys.Date(),
        ".",
        input$save_fileMerge,
        sep = ""
      )
    },
    content <- function(file) {
      tr <- FALSE
      if ("CR" %in% names(values$M)) {
        tr <- (sum(nchar(values$M$CR) > 32767, na.rm = TRUE)) > 0
      }

      if (tr & input$save_file == "xlsx") {
        show_alert(
          text = tags$span(
            tags$h4(
              "Some documents have too long a list of references that cannot be saved in excel (>32767 characters).",
              style = "color: firebrick;"
            ),
            tags$br(),
            tags$h4(
              "Data in the column CR could be truncated.",
              style = "color: firebrick;"
            )
          ),
          #text = "Some documents have too long a list of references that cannot be saved in excel (>32767 characters).\nData in the column CR could be truncated",
          title = "Please save the collection using the 'RData' format",
          type = "warning",
          width = "50%", ##NEW ----
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          html = FALSE,
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          btn_labels = "OK",
          btn_colors = "#913333",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
        suppressWarnings(openxlsx::write.xlsx(values$M, file = file))
      } else {
        switch(
          input$save_fileMerge,
          xlsx = {
            suppressWarnings(openxlsx::write.xlsx(values$M, file = file))
          },
          RData = {
            M = values$M
            save(M, file = file)
          }
        )
      }
    },
    contentType = input$save_fileMerge
  )

  output$collection.save_api <- downloadHandler(
    filename = function() {
      paste(
        "Bibliometrix-Export-File-",
        Sys.Date(),
        ".",
        input$save_file_api,
        sep = ""
      )
    },
    content <- function(file) {
      switch(
        input$save_file_api,
        xlsx = {
          suppressWarnings(openxlsx::write.xlsx(values$M, file = file))
        },
        RData = {
          M = values$M
          save(M, file = file)
        }
      )
    },
    contentType = input$save_file_api
  )

  output$textLog2 <- renderUI({
    k = dim(values$M)[1]
    if (k == 1) {
      k = 0
    }
    log = paste("Number of Documents", k)

    div(
      style = "background-color: #f8f9fa; padding: 12px; border-radius: 5px; border: 1px solid #dee2e6; margin: 10px 0;",
      tags$label(
        "Conversion results",
        style = "font-weight: 600; color: #495057; margin-bottom: 5px; display: block; font-size: 14px;"
      ),
      div(
        log,
        style = "color: #212529; font-size: 15px; font-weight: 500;"
      )
    )
  })

  dsModal <- function(failed = FALSE) {
    modalDialog(
      title = "Dimensions API",
      size = "l",
      h4(em(
        strong("1) Get a token using your Dimensions credentials")
      )),
      textInput(
        "dsAccount",
        "Account",
        "",
        width = NULL,
        placeholder = NULL
      ),
      passwordInput(
        "dsPassword",
        "Password",
        "",
        width = NULL,
        placeholder = NULL
      ),
      actionButton("dsToken", "Get a token "),
      h5(tags$b("Token")),
      verbatimTextOutput("tokenLog", placeholder = FALSE),
      tags$hr(),
      h4(em(strong("2) Create a query"))),
      textInput(
        "dsWords",
        "Words",
        "",
        width = NULL,
        placeholder = NULL
      ),
      selectInput(
        "dsFullsearch",
        label = "search field",
        choices = c("Title and Abstract only" = FALSE, "Full text" = TRUE),
        selected = FALSE
      ),
      textInput(
        "dsCategories",
        "Science Categories",
        "",
        width = NULL,
        placeholder = NULL
      ),
      numericInput("dsStartYear", "Start Year", value = 1990),
      numericInput(
        "dsEndYear",
        "End Year",
        value = as.numeric(substr(Sys.time(), 1, 4))
      ),
      actionButton("dsQuery", "Create the query "),
      h5(tags$b("Your query")),
      verbatimTextOutput("queryLog", placeholder = FALSE),
      h5(tags$b("Documents returned using your query")),
      verbatimTextOutput("sampleLog", placeholder = FALSE),
      uiOutput("sliderLimit"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("dsok", "OK")
      )
    )
  }

  ### Show Dimensions modal when button is clicked.
  observeEvent(input$dsShow, {
    showModal(dsModal())
  })

  observeEvent(input$dsok, {
    removeModal()
    values$M <- data.frame(Message = "Waiting for data")
  })

  output$tokenLog <- renderText({
    input$dsToken
    isolate({
      capture.output(
        Token <- dsAuth(username = input$dsAccount, password = input$dsPassword)
      )
      if (Token == 1) {
        values$dsToken <- "Wrong account or password"
      } else {
        values$dsToken <- Token
      }
      values$dsToken
    })
  })

  DSQUERYload <- eventReactive(input$dsQuery, {
    values$dsQuery <- dsQueryBuild(
      item = "publications",
      words = input$dsWords,
      full.search = input$dsFullsearch,
      type = "article",
      categories = input$dsCategories,
      start_year = input$dsStartYear,
      end_year = input$dsEndYear
    )
    dsSample <- 0
    capture.output(
      dsSample <- dsApiRequest(
        token = values$dsToken,
        query = values$dsQuery,
        limit = 0
      )
    )
    if (class(dsSample) == "numeric") {
      values$dsSample <- 0
    } else {
      values$dsSample <- dsSample$total_count
    }
  })

  output$queryLog <- renderText({
    DSQUERYload()
    values$dsQuery
  })

  output$queryLog2 <- renderText({
    DSQUERYload()
    values$dsQuery
  })

  output$sampleLog <- renderText({
    DSQUERYload()
    mes <- paste(
      "Dimensions returns ",
      values$dsSample,
      " documents",
      collapse = "",
      sep = ""
    )
    mes
  })

  output$sampleLog2 <- renderText({
    if (nrow(values$M) < 2) {
      n <- 0
    } else {
      n <- nrow(values$M)
    }
    mes <- paste(
      "Dimensions API returns ",
      n,
      " documents",
      collapse = "",
      sep = ""
    )
    values$ApiOk <- 0
    return(mes)
  })

  output$sliderLimit <- renderUI({
    sliderInput(
      "sliderLimit",
      "Total document to download",
      min = 1,
      max = values$dsSample,
      value = values$dsSample,
      step = 1
    )
  })

  # ### API MENU: PubMed ----
  # ### PubMed modal
  # pmModal <- function(failed = FALSE) {
  #   modalDialog(
  #     title = "PubMed API",
  #     size = "l",
  #     h4(em(strong(
  #       "1) Generate a valid query"
  #     ))),
  #     textInput(
  #       "pmQueryText",
  #       "Search terms",
  #       " ",
  #       width = NULL,
  #       placeholder = NULL
  #     ),
  #     numericInput("pmStartYear", "Start Year", value = 1990),
  #     numericInput(
  #       "pmEndYear",
  #       "End Year",
  #       value = as.numeric(substr(Sys.time(), 1, 4))
  #     ),
  #     actionButton("pmQuery", "Try the query "),
  #     h5(tags$b("Query Translation")),
  #     verbatimTextOutput("pmQueryLog", placeholder = FALSE),
  #     h5(tags$b("Documents returned using your query")),
  #     verbatimTextOutput("pmSampleLog", placeholder = FALSE),
  #     tags$hr(),
  #     h4(em(
  #       strong("2) Choose how many documents to download")
  #     )),
  #     uiOutput("pmSliderLimit"),
  #     footer = tagList(
  #       modalButton("Cancel"),
  #       actionButton("pmok", "OK")
  #     )
  #   )
  # }
  #
  # # Show modal when button is clicked.
  # observeEvent(input$pmShow, {
  #   showModal(pmModal())
  # })
  #
  # observeEvent(input$pmok, {
  #   removeModal()
  # })
  #
  # pmQUERYLOAD <- eventReactive(input$pmQuery, {
  #   query = paste(
  #     input$pmQueryText,
  #     "[Title/Abstract] AND english[LA] AND Journal Article[PT] AND ",
  #     input$pmStartYear,
  #     ":",
  #     input$pmEndYear,
  #     "[DP]",
  #     sep = ""
  #   )
  #   res <- pmQueryTotalCount(query = query, api_key = NULL)
  #   if (class(res) == "list") {
  #     values$pmSample <- res$total_count
  #     values$pmQuery <- res$query_translation
  #   }
  #   values$pmQuery <- res$query_translation
  # })
  # output$pmQueryLog <- renderText({
  #   pmQUERYLOAD()
  #   values$pmQuery
  # })
  #
  # output$pmQueryLog2 <- renderText({
  #   pmQUERYLOAD()
  #   values$pmQuery
  # })
  #
  # output$pmSampleLog <- renderText({
  #   pmQUERYLOAD()
  #   mes <- paste(
  #     "PubMed returns ",
  #     values$pmSample,
  #     " documents",
  #     collapse = "",
  #     sep = ""
  #   )
  #   mes
  # })
  # output$pmSampleLog2 <- renderText({
  #   if (nrow(values$M) < 2) {
  #     n <- 0
  #   } else {
  #     n <- nrow(values$M)
  #   }
  #
  #   mes <- paste(
  #     "PubMed API returns ",
  #     n,
  #     " documents",
  #     collapse = "",
  #     sep = ""
  #   )
  #   values$ApiOk <- 0
  #   return(mes)
  # })
  #
  # output$pmSliderLimit <- renderUI({
  #   sliderInput(
  #     "pmSliderLimit",
  #     "Total document to download",
  #     min = 1,
  #     max = values$pmSample,
  #     value = values$pmSample,
  #     step = 1
  #   )
  # })
  #
  # ### API MENU: Content Download ----
  # APIDOWNLOAD <- eventReactive(input$apiApply, {
  #   values = initial(values)
  #   values$M <- data.frame(Message = "Waiting for data")
  #   switch(
  #     input$dbapi,
  #     ds = {
  #       if (input$dsWords != "") {
  #         D <-
  #           dsApiRequest(
  #             token = values$dsToken,
  #             query = values$dsQuery,
  #             limit = input$sliderLimit
  #           )
  #         M <- convert2df(D, "dimensions", "api")
  #         values$ApiOk <- 1
  #         values$M <- M
  #         values$Morig = M
  #         values$SCdf <- wcTable(M)
  #         values$COdf <- countryTable(M)
  #         if (ncol(values$M) > 1) {
  #           values$rest_sidebar <- TRUE
  #         }
  #         if (ncol(values$M) > 1) {
  #           showModal(missingModal(session))
  #         }
  #         values$Histfield = "NA"
  #         values$results = list("NA")
  #         contentTable(values)
  #       }
  #     },
  #     pubmed = {
  #       if (input$pmQueryText != " ") {
  #         D <-
  #           pmApiRequest(
  #             query = values$pmQuery,
  #             limit = input$pmSliderLimit,
  #             api_key = NULL
  #           )
  #         M <- convert2df(D, "pubmed", "api")
  #         values$ApiOk <- 1
  #         values$M <- M
  #         values$Morig = M
  #         if (ncol(values$M) > 1) {
  #           values$rest_sidebar <- TRUE
  #         }
  #         if (ncol(values$M) > 1) {
  #           showModal(missingModal(session))
  #         }
  #         values$Histfield = "NA"
  #         values$results = list("NA")
  #       }
  #     }
  #   )
  # })
  #
  # output$apiContents <- renderUI({
  #   APIDOWNLOAD()
  #   contentTable(values)
  # })
  #
  # ### function returns a formatted data.frame ----
  # contentTable <- function(values) {
  #   MData = as.data.frame(apply(values$M, 2, function(x) {
  #     substring(x, 1, 150)
  #   }))
  #   MData$DOI <-
  #     paste0(
  #       '<a href=\"https://doi.org/',
  #       MData$DI,
  #       '\" target=\"_blank\">',
  #       MData$DI,
  #       '</a>'
  #     )
  #   nome = c("DOI", names(MData)[-length(names(MData))])
  #   MData = MData[nome]
  #   renderBibliobox(
  #     MData,
  #     nrow = 3,
  #     filename = "Table",
  #     pagelength = TRUE,
  #     left = NULL,
  #     right = NULL,
  #     numeric = NULL,
  #     dom = TRUE,
  #     size = '70%',
  #     filter = "top",
  #     columnShort = NULL,
  #     columnSmall = NULL,
  #     round = 2,
  #     title = "",
  #     button = FALSE,
  #     escape = FALSE,
  #     selection = FALSE,
  #     scrollX = TRUE
  #   )
  # }

  # REFERENCE MATCHING MENU ----

  ## ============================================================================
  ## Logic per Reference Matching
  ## ============================================================================

  # Reactive values to store matching results and original data
  refMatch_results <- reactiveVal(NULL)
  refMatch_M_original <- reactiveVal(NULL)
  refMatch_applied <- reactiveVal(FALSE)
  refMatch_selected_for_merge <- reactiveVal(character(0))

  # Store original M when first accessing the tab
  observeEvent(
    values$M,
    {
      if (is.null(refMatch_M_original())) {
        refMatch_M_original(values$M)
      }
    },
    ignoreInit = TRUE,
    once = TRUE
  )

  # Run matching when button is clicked
  observeEvent(input$refMatch_run, {
    req(values$M)

    if (is.null(refMatch_M_original())) {
      refMatch_M_original(values$M)
    }

    # Show loading indicator
    shinyjs::show("refMatch_loadingIndicator")

    # Collapse the matching options box
    shinyjs::runjs(
      "$('#refMatch_optionsBox').closest('.box').removeClass('collapsed-box').addClass('collapsed-box');"
    )
    shinyjs::runjs("$('#refMatch_optionsBox').slideUp();")

    # Small delay to allow UI update
    Sys.sleep(0.1)

    tryCatch(
      {
        results <- applyCitationMatching(
          M = values$M,
          threshold = input$refMatch_threshold,
          method = input$refMatch_method
        )

        refMatch_results(results)
        refMatch_selected_for_merge(character(0))

        # Hide loading indicator
        shinyjs::hide("refMatch_loadingIndicator")

        showNotification(
          "Citation matching completed successfully!",
          type = "message",
          duration = 5
        )
      },
      error = function(e) {
        shinyjs::hide("refMatch_loadingIndicator")
        showNotification(
          paste("Error during matching:", e$message),
          type = "error",
          duration = 10
        )
      }
    )
  })

  # Highlight clicked row in the table
  observeEvent(input$refMatch_row_clicked, {
    shinyjs::runjs(sprintf(
      "if (window.refMatchTable) window.refMatchTable.highlightClickedRow(%d);",
      input$refMatch_row_clicked
    ))
    # Expand the variants box when a row is clicked
    shinyjs::runjs(
      "$('.box:has(#refMatch_variantsTable)').removeClass('collapsed-box'); $('.box:has(#refMatch_variantsTable) .box-body').slideDown();"
    )
  })

  # Toggle selection for manual merge
  observeEvent(input$refMatch_toggleSelection, {
    req(refMatch_results())
    req(input$refMatch_row_clicked)

    selected_row <- input$refMatch_row_clicked

    # Reconstruct top_table to get the citation at the clicked row
    top_table <- refMatch_results()$summary %>%
      select(
        Citation = CR_canonical,
        `Times Cited` = n,
        `Variants Found` = n_variants
      ) %>%
      arrange(desc(`Times Cited`)) %>%
      filter(`Times Cited` > 1)

    req(selected_row <= nrow(top_table))
    selected_citation <- top_table$Citation[selected_row]

    current_selection <- refMatch_selected_for_merge()

    if (selected_citation %in% current_selection) {
      # Remove from selection
      refMatch_selected_for_merge(setdiff(current_selection, selected_citation))
      shinyjs::runjs(sprintf(
        "if (window.refMatchTable) window.refMatchTable.updateRowSelection(%d, false);",
        selected_row
      ))
    } else {
      # Add to selection
      refMatch_selected_for_merge(c(current_selection, selected_citation))
      shinyjs::runjs(sprintf(
        "if (window.refMatchTable) window.refMatchTable.updateRowSelection(%d, true);",
        selected_row
      ))
    }
  })

  # Clear manual selection
  observeEvent(input$refMatch_clearSelection, {
    refMatch_selected_for_merge(character(0))

    # Update table visuals without re-rendering
    shinyjs::runjs(
      "if (window.refMatchTable) window.refMatchTable.clearAllSelections();"
    )

    showNotification("Selection cleared", type = "message", duration = 3)
  })

  # Perform manual merge
  observeEvent(input$refMatch_confirmMerge, {
    req(refMatch_results())
    selected <- refMatch_selected_for_merge()

    if (length(selected) < 2) {
      showNotification(
        "Please select at least 2 citations to merge",
        type = "warning",
        duration = 5
      )
      return()
    }

    showModal(
      modalDialog(
        title = "Confirm Manual Merge",
        tags$div(
          tags$p(tags$b(paste(
            "You are about to merge",
            length(selected),
            "citations:"
          ))),
          tags$ul(
            lapply(selected, function(cit) {
              tags$li(substr(cit, 1, 100))
            })
          ),
          tags$hr(),
          tags$p("Select the canonical form to use:"),
          radioButtons(
            "refMatch_canonicalChoice",
            NULL,
            choices = setNames(selected, substr(selected, 1, 100)),
            selected = selected[1]
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("refMatch_executeMerge", "Merge", class = "btn-primary")
        ),
        size = "l"
      )
    )
  })

  # Execute the merge
  observeEvent(input$refMatch_executeMerge, {
    req(refMatch_results())
    req(input$refMatch_canonicalChoice)

    selected <- refMatch_selected_for_merge()
    canonical <- input$refMatch_canonicalChoice

    withProgress(message = 'Merging citations...', value = 0, {
      tryCatch(
        {
          incProgress(0.3, detail = "Updating citation clusters...")

          results <- refMatch_results()

          all_variants <- results$full_data %>%
            filter(CR_canonical %in% selected) %>%
            pull(CR) %>%
            unique()

          results$full_data <- results$full_data %>%
            mutate(
              CR_canonical = ifelse(
                CR %in% all_variants,
                canonical,
                CR_canonical
              )
            )

          incProgress(0.6, detail = "Recalculating statistics...")

          results$summary <- results$full_data %>%
            group_by(CR_canonical) %>%
            summarise(
              n = n(),
              n_variants = n_distinct(CR),
              variants = paste(unique(CR), collapse = " | "),
              .groups = "drop"
            ) %>%
            arrange(desc(n))

          results$CR_normalized <- results$full_data %>%
            group_by(SR) %>%
            summarise(
              CR = paste(unique(CR_canonical), collapse = ";"),
              n_references = n_distinct(CR_canonical),
              .groups = "drop"
            )

          incProgress(0.9, detail = "Finalizing...")

          # Store updated results (triggers table re-render)
          refMatch_results(results)

          # Clear selection
          old_selection <- refMatch_selected_for_merge()
          refMatch_selected_for_merge(character(0))

          removeModal()

          incProgress(1.0, detail = "Complete!")

          showNotification(
            HTML(paste0(
              "<b>Manual merge completed!</b><br>",
              length(old_selection),
              " citations have been merged into one.<br>",
              "Statistics have been updated."
            )),
            type = "message",
            duration = 8
          )
        },
        error = function(e) {
          showNotification(
            paste("Error during manual merge:", e$message),
            type = "error",
            duration = 10
          )
        }
      )
    })
  })

  # Apply normalized citations to values$M
  observeEvent(input$refMatch_apply, {
    req(refMatch_results())
    req(values$M)

    withProgress(message = 'Applying normalized citations...', value = 0, {
      tryCatch(
        {
          incProgress(0.3, detail = "Updating CR field...")

          if (
            input$refMatch_keepOriginal && !"CR_original" %in% names(values$M)
          ) {
            values$M$CR_original <- values$M$CR
          }

          values$M <- values$M %>%
            mutate(CR_original = CR) %>%
            select(-CR) %>%
            left_join(
              refMatch_results()$CR_normalized %>% select(SR, CR),
              by = "SR"
            )

          if (input$refMatch_addStats) {
            stats <- refMatch_results()$CR_normalized %>%
              select(SR, n_references_normalized = n_references)

            values$M <- values$M %>%
              left_join(stats, by = "SR")
          }

          incProgress(0.8, detail = "Finalizing...")

          refMatch_applied(TRUE)

          incProgress(1.0, detail = "Complete!")

          showNotification(
            HTML(paste0(
              "<b>Normalized citations applied!</b><br>",
              "The CR field has been updated with normalized citations.<br>",
              "You can now continue with other analyses."
            )),
            type = "message",
            duration = 8
          )
        },
        error = function(e) {
          showNotification(
            paste("Error applying normalization:", e$message),
            type = "error",
            duration = 10
          )
        }
      )
    })
  })

  # Reset to original data
  observeEvent(input$refMatch_reset, {
    req(refMatch_M_original())

    showModal(
      modalDialog(
        title = "Confirm Reset",
        "Are you sure you want to reset to the original data? This will undo all citation normalization.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            "refMatch_confirmReset",
            "Yes, Reset",
            class = "btn-danger"
          )
        )
      )
    )
  })

  # Confirm reset
  observeEvent(input$refMatch_confirmReset, {
    values$M <- refMatch_M_original()
    refMatch_applied(FALSE)
    refMatch_results(NULL)
    refMatch_selected_for_merge(character(0))

    removeModal()

    # Expand the matching options box
    shinyjs::runjs(
      "$('#refMatch_optionsBox').closest('.box').removeClass('collapsed-box');"
    )
    shinyjs::runjs("$('#refMatch_optionsBox').slideDown();")

    showNotification(
      "Data reset to original state. All normalization has been removed.",
      type = "warning",
      duration = 5
    )
  })

  # Status messages
  output$refMatch_runStatus <- renderUI({
    req(refMatch_results())

    div(
      style = "margin-top: 10px;",
      tags$div(
        class = "alert alert-success",
        icon("check-circle"),
        tags$b(" Matching completed!"),
        br(),
        tags$small("Results are ready. Click 'Apply' to update your data.")
      )
    )
  })

  output$refMatch_applyStatus <- renderUI({
    if (refMatch_applied()) {
      div(
        style = "margin-top: 10px;",
        tags$div(
          class = "alert alert-success",
          icon("check-circle"),
          tags$b(" Applied!"),
          br(),
          tags$small("Normalized citations are active in your dataset.")
        )
      )
    } else {
      NULL
    }
  })

  # Selection status - INLINE version
  output$refMatch_selectionStatusInline <- renderUI({
    n_selected <- length(refMatch_selected_for_merge())

    if (n_selected == 0) {
      tags$span(
        style = "color: #999; font-size: 13px;",
        icon("info-circle"),
        " No citations selected"
      )
    } else if (n_selected == 1) {
      tags$span(
        style = "color: #f39c12; font-weight: bold; font-size: 13px;",
        icon("exclamation-triangle"),
        sprintf(" %d citation selected (need at least 2)", n_selected)
      )
    } else {
      tags$span(
        style = "color: #00a65a; font-weight: bold; font-size: 14px;",
        icon("check-circle"),
        sprintf(" %d citations selected - ready to merge!", n_selected)
      )
    }
  })

  # Value boxes
  output$refMatch_original <- renderValueBox({
    req(refMatch_results())

    n_orig <- length(unique(refMatch_results()$full_data$CR))

    valueBox(
      value = formatC(n_orig, format = "d", big.mark = ","),
      subtitle = "Original Citations",
      icon = icon("book"),
      color = "blue"
    )
  })

  output$refMatch_normalized <- renderValueBox({
    req(refMatch_results())

    n_norm <- nrow(refMatch_results()$summary)

    valueBox(
      value = formatC(n_norm, format = "d", big.mark = ","),
      subtitle = "Unique Citations (after matching)",
      icon = icon("check-double"),
      color = "green"
    )
  })

  output$refMatch_duplicates <- renderValueBox({
    req(refMatch_results())

    n_orig <- length(unique(refMatch_results()$full_data$CR))
    n_norm <- nrow(refMatch_results()$summary)
    n_dup <- n_orig - n_norm
    perc <- round(100 * n_dup / n_orig, 1)

    valueBox(
      value = paste0(
        formatC(n_dup, format = "d", big.mark = ","),
        " (",
        perc,
        "%)"
      ),
      subtitle = "Merged Variants",
      icon = icon("copy"),
      color = "red"
    )
  })

  # Plots
  output$refMatch_clusterSizePlot <- renderPlot({
    req(refMatch_results())

    cluster_sizes <- table(refMatch_results()$summary$n_variants)

    df_plot <- data.frame(
      n_variants = as.numeric(names(cluster_sizes)),
      count = as.numeric(cluster_sizes)
    )

    ggplot(df_plot, aes(x = factor(n_variants), y = count)) +
      geom_col(fill = "#3c8dbc", alpha = 0.8) +
      labs(
        title = "Distribution of Variant Counts",
        x = "Number of Variants per Citation",
        y = "Number of Citations"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 9)
      )
  })

  output$refMatch_variantsPlot <- renderPlot({
    req(refMatch_results())

    top_variants <- refMatch_results()$summary %>%
      arrange(desc(n_variants)) %>%
      head(10)

    ggplot(
      top_variants,
      aes(x = reorder(substr(CR_canonical, 1, 40), n_variants), y = n_variants)
    ) +
      geom_col(fill = "#f39c12", alpha = 0.8) +
      coord_flip() +
      labs(
        title = "Citations with Most Variants",
        x = "",
        y = "Number of Variants"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 12),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 9)
      )
  })

  # Top citations table
  output$refMatch_topCitations <- renderUI({
    req(refMatch_results())

    top_table <- refMatch_results()$summary %>%
      select(
        Citation = CR_canonical,
        `Times Cited` = n,
        `Variants Found` = n_variants
      ) %>%
      arrange(desc(`Times Cited`)) %>%
      filter(`Times Cited` > 1)

    # Add selection column based on current merge selections
    selected <- isolate(refMatch_selected_for_merge())
    top_table <- top_table %>%
      mutate(
        Selected = ifelse(Citation %in% selected, "TRUE", "FALSE"),
        .before = 1
      )

    table_id <- "refMatch_htmlTable"
    pageSize <- 10
    n_cols <- ncol(top_table)
    col_names <- colnames(top_table)

    # Header
    header_cols <- paste0(
      sprintf(
        '<th class="sortable-header" data-col="%d" style="text-align: left; background-color: #f8f9fa; border-bottom: 2px solid #dee2e6; padding: 12px 25px 12px 10px; position: relative; white-space: nowrap;">%s <span class="sort-icon" style="color: #ccc; font-size: 0.9em; position: absolute; right: 8px; top: 50%; transform: translateY(-50%);">&#8693;</span></th>',
        0:(n_cols - 1),
        col_names
      ),
      collapse = ""
    )
    header_html <- paste0(
      "<thead><tr style='cursor: pointer;'>",
      header_cols,
      "</tr>"
    )

    # Filter row
    filter_cells <- paste0(
      sprintf(
        '<td><input type="text" class="form-control table-filter-%s" data-col="%d" placeholder="Filter..." style="width:100%%; font-size:11px; height:24px; padding:2px 5px;"></td>',
        table_id,
        0:(n_cols - 1)
      ),
      collapse = ""
    )
    filter_html <- paste0('<tr class="filter-row">', filter_cells, "</tr>")
    header_html <- paste0(header_html, filter_html, "</thead>")

    # Body rows
    cell_cols <- vector("list", n_cols)
    for (j in 1:n_cols) {
      vals <- as.character(top_table[[j]])
      sort_vals <- gsub("<.*?>", "", vals)

      if (j == 1) {
        # Selected column - special styling
        is_sel <- vals == "TRUE"
        cell_bg <- ifelse(
          is_sel,
          "background-color: #28a745; color: #ffffff;",
          "background-color: #ffffff; color: #000000;"
        )
        cell_cols[[j]] <- sprintf(
          '<td class="sel-cell" data-sort="%s" style="%s padding: 8px; border-bottom: 1px solid #eee; font-weight: bold; text-align: center;">%s</td>',
          sort_vals,
          cell_bg,
          vals
        )
      } else if (j %in% c(3, 4)) {
        # Numeric columns (Times Cited, Variants Found)
        cell_cols[[j]] <- sprintf(
          '<td data-sort="%s" style="text-align: right; padding: 8px; border-bottom: 1px solid #eee;">%s</td>',
          sort_vals,
          vals
        )
      } else {
        cell_cols[[j]] <- sprintf(
          '<td data-sort="%s" style="text-align: left; padding: 8px; border-bottom: 1px solid #eee;">%s</td>',
          sort_vals,
          vals
        )
      }
    }
    row_contents <- do.call(paste0, cell_cols)

    # Row background based on selection
    row_bg <- ifelse(
      top_table$Selected == "TRUE",
      "background-color: #d4edda;",
      ""
    )

    body_rows <- sprintf(
      '<tr data-orig-row="%d" style="cursor: pointer; %s" onclick="Shiny.setInputValue(\'refMatch_row_clicked\', %d, {priority: \'event\'})">%s</tr>',
      1:nrow(top_table),
      row_bg,
      1:nrow(top_table),
      row_contents
    )
    tbody_html <- paste0("<tbody>", paste(body_rows, collapse = ""), "</tbody>")

    # Full HTML
    tagList(
      tags$div(
        id = paste0("container_", table_id),
        style = "font-size: 85%; background: white; border: 1px solid #ddd; border-radius: 4px; margin-bottom: 20px; font-family: sans-serif;",

        tags$div(
          style = "overflow-x: auto;",
          tags$table(
            id = table_id,
            class = "table table-hover",
            style = "width: 100%; margin-bottom: 0; table-layout: auto;",
            HTML(header_html),
            HTML(tbody_html)
          )
        ),

        tags$div(
          style = "padding: 10px; border-top: 1px solid #eee; display: flex; justify-content: space-between; align-items: center;",
          tags$div(
            id = paste0("info_", table_id),
            style = "color: #777; font-size: 12px;"
          ),
          tags$div(class = "btn-group", id = paste0("pager_", table_id))
        )
      ),

      tags$script(HTML(sprintf(
        '
        (function() {
          var tid = "%s";
          var pageSize = %d;
          var currentPage = 0;
          var visibleRows = [];
          var sortCol = -1;
          var sortAsc = true;

          function init() {
            var table = document.getElementById(tid);
            if (!table) { setTimeout(init, 100); return; }
            var tbody = table.querySelector("tbody");
            var allRows = Array.from(tbody.querySelectorAll("tr"));
            visibleRows = allRows;

            function updateTable() {
              var start = currentPage * pageSize;
              var end = start + pageSize;
              allRows.forEach(function(r) { r.style.display = "none"; });
              visibleRows.slice(start, end).forEach(function(r) {
                r.style.display = "";
                tbody.appendChild(r);
              });
              document.getElementById("info_" + tid).innerText =
                "Showing " + (visibleRows.length > 0 ? start + 1 : 0) +
                "-" + Math.min(end, visibleRows.length) + " of " + visibleRows.length;
              renderPager();
            }

            function renderPager() {
              var pager = document.getElementById("pager_" + tid);
              var pageCount = Math.ceil(visibleRows.length / pageSize);
              pager.innerHTML = "";
              if (pageCount <= 1 && visibleRows.length <= pageSize) return;
              function createBtn(label, target, active, disabled) {
                var b = document.createElement("button");
                b.className = "btn btn-default btn-xs" + (active ? " active" : "");
                b.innerText = label; b.disabled = disabled;
                b.onclick = function() { currentPage = target; updateTable(); };
                pager.appendChild(b);
              }
              createBtn("\u00AB", Math.max(0, currentPage - 1), false, currentPage === 0);
              for (var i = 0; i < pageCount; i++) {
                if (i === 0 || i === pageCount - 1 || (i >= currentPage - 1 && i <= currentPage + 1)) {
                  createBtn(i + 1, i, i === currentPage);
                } else if (i === currentPage - 2 || i === currentPage + 2) {
                  var s = document.createElement("span"); s.innerText = "..."; s.style.padding = "0 5px";
                  pager.appendChild(s);
                }
              }
              createBtn("\u00BB", Math.min(pageCount - 1, currentPage + 1), false, currentPage === pageCount - 1);
            }

            table.querySelectorAll(".sortable-header").forEach(function(th) {
              th.onclick = function() {
                var col = parseInt(this.getAttribute("data-col"));
                sortAsc = (sortCol === col) ? !sortAsc : true;
                sortCol = col;
                table.querySelectorAll(".sort-icon").forEach(function(si) { si.innerText = "\u21C5"; si.style.color = "#ccc"; });
                this.querySelector(".sort-icon").innerText = sortAsc ? "\u25B2" : "\u25BC";
                this.querySelector(".sort-icon").style.color = "#333";
                visibleRows.sort(function(a, b) {
                  var valA = a.cells[col].getAttribute("data-sort").trim();
                  var valB = b.cells[col].getAttribute("data-sort").trim();
                  if (!isNaN(valA) && !isNaN(valB) && valA !== "" && valB !== "") {
                    return sortAsc ? parseFloat(valA) - parseFloat(valB) : parseFloat(valB) - parseFloat(valA);
                  }
                  return sortAsc ? valA.localeCompare(valB) : valB.localeCompare(valA);
                });
                currentPage = 0;
                updateTable();
              };
            });

            var filters = document.querySelectorAll(".table-filter-" + tid);
            filters.forEach(function(filterInput) {
              filterInput.addEventListener("input", function() {
                var fVals = Array.from(filters).map(function(f) { return f.value.toUpperCase(); });
                allRows.forEach(function(row) {
                  var isMatch = fVals.every(function(val, idx) {
                    if (!val) return true;
                    return row.cells[idx].getAttribute("data-sort").toUpperCase().indexOf(val) >= 0;
                  });
                  row.setAttribute("data-is-filtered", isMatch ? "true" : "false");
                });
                visibleRows = allRows.filter(function(r) { return r.getAttribute("data-is-filtered") === "true"; });
                currentPage = 0;
                updateTable();
              });
            });

            allRows.forEach(function(r) { r.setAttribute("data-is-filtered", "true"); });
            updateTable();

            // Expose API for external JS updates (toggle selection, clear, highlight)
            window.refMatchTable = {
              updateRowSelection: function(origRow, isSelected) {
                var row = table.querySelector("tbody tr[data-orig-row=\\"" + origRow + "\\"]");
                if (!row) return;
                row.style.backgroundColor = isSelected ? "#d4edda" : "";
                var selCell = row.cells[0];
                selCell.innerText = isSelected ? "TRUE" : "FALSE";
                selCell.setAttribute("data-sort", isSelected ? "TRUE" : "FALSE");
                selCell.style.backgroundColor = isSelected ? "#28a745" : "#ffffff";
                selCell.style.color = isSelected ? "#ffffff" : "#000000";
              },
              clearAllSelections: function() {
                allRows.forEach(function(row) {
                  row.style.backgroundColor = "";
                  var selCell = row.cells[0];
                  selCell.innerText = "FALSE";
                  selCell.setAttribute("data-sort", "FALSE");
                  selCell.style.backgroundColor = "#ffffff";
                  selCell.style.color = "#000000";
                });
              },
              highlightClickedRow: function(origRow) {
                allRows.forEach(function(row) {
                  var isClicked = row.getAttribute("data-orig-row") === String(origRow);
                  row.style.outline = isClicked ? "2px solid #3c8dbc" : "";
                  row.style.outlineOffset = isClicked ? "-2px" : "";
                });
              }
            };
          }
          init();
        })();
        ',
        table_id,
        pageSize
      )))
    )
  })

  # Variants table
  output$refMatch_variantsTable <- renderUI({
    req(refMatch_results())
    req(input$refMatch_row_clicked)

    selected_row <- input$refMatch_row_clicked

    # Reconstruct top_table to get the citation at the clicked row
    top_table <- refMatch_results()$summary %>%
      select(
        Citation = CR_canonical,
        `Times Cited` = n,
        `Variants Found` = n_variants
      ) %>%
      arrange(desc(`Times Cited`)) %>%
      filter(`Times Cited` > 1)

    req(selected_row <= nrow(top_table))
    selected_citation <- top_table$Citation[selected_row]

    variants <- refMatch_results()$full_data %>%
      filter(CR_canonical == selected_citation) %>%
      select(Variant = CR) %>%
      group_by(Variant) %>%
      summarise(Count = n(), .groups = "drop") %>%
      arrange(desc(Count))

    HTML(htmlBoxFormat(
      variants,
      nrow = 10,
      title = paste0("Variants of: ", substr(selected_citation, 1, 80), "..."),
      right = "Count",
      numeric = "Count",
      scrollX = TRUE
    ))
  })

  # Download handlers
  output$refMatch_download <- downloadHandler(
    filename = function() {
      format <- input$refMatch_exportFormat
      base_name <- input$refMatch_filename

      if (format == "xlsx") {
        paste0(base_name, ".xlsx")
      } else if (format == "rdata") {
        paste0(base_name, ".RData")
      } else {
        paste0(base_name, ".zip")
      }
    },

    content = function(file) {
      req(refMatch_results())
      req(values$M)

      withProgress(message = 'Preparing export...', value = 0, {
        incProgress(0.3, detail = "Processing data...")

        M_normalized <- values$M

        if (!refMatch_applied()) {
          if (input$refMatch_keepOriginal) {
            M_normalized$CR_original <- M_normalized$CR
          }

          M_normalized <- M_normalized %>%
            select(-CR) %>%
            left_join(
              refMatch_results()$CR_normalized %>% select(SR, CR),
              by = "SR"
            )

          if (input$refMatch_addStats) {
            stats <- refMatch_results()$CR_normalized %>%
              select(SR, n_references_normalized = n_references)

            M_normalized <- M_normalized %>%
              left_join(stats, by = "SR")
          }
        }

        incProgress(0.6, detail = "Writing file...")

        format <- input$refMatch_exportFormat

        if (format == "xlsx") {
          openxlsx::write.xlsx(M_normalized, file)
        } else if (format == "rdata") {
          M <- M_normalized
          save(M, file = file)
        } else {
          temp_dir <- getWD()
          xlsx_file <- file.path(
            temp_dir,
            paste0(input$refMatch_filename, ".xlsx")
          )
          rdata_file <- file.path(
            temp_dir,
            paste0(input$refMatch_filename, ".RData")
          )

          openxlsx::write.xlsx(M_normalized, xlsx_file)
          M <- M_normalized
          save(M, file = rdata_file)

          zip(file, files = c(xlsx_file, rdata_file), flags = "-j")
        }

        incProgress(1.0, detail = "Complete!")
      })
    }
  )

  output$refMatch_downloadReport <- downloadHandler(
    filename = function() {
      paste0("citation_matching_report_", Sys.Date(), ".xlsx")
    },

    content = function(file) {
      req(refMatch_results())

      withProgress(message = 'Generating report...', value = 0, {
        incProgress(0.3, detail = "Preparing sheets...")

        report_data <- list(
          "Summary" = refMatch_results()$summary,
          "Full_Data" = refMatch_results()$full_data %>% select(-blocking_key),
          "Normalized_CR" = refMatch_results()$CR_normalized,
          "Parameters" = data.frame(
            Parameter = c(
              "Threshold",
              "Method",
              "Date",
              "Original_Citations",
              "Normalized_Citations",
              "Duplicates_Found",
              "Applied_to_Data"
            ),
            Value = c(
              input$refMatch_threshold,
              input$refMatch_method,
              as.character(Sys.Date()),
              length(unique(refMatch_results()$full_data$CR)),
              nrow(refMatch_results()$summary),
              length(unique(refMatch_results()$full_data$CR)) -
                nrow(refMatch_results()$summary),
              ifelse(refMatch_applied(), "Yes", "No")
            )
          )
        )

        incProgress(0.7, detail = "Writing Excel file...")

        openxlsx::write.xlsx(report_data, file)

        incProgress(1.0, detail = "Complete!")
      })
    }
  )

  # FILTERS MENU ----

  #### Box about Filter Results ----
  output$textDim <- renderUI({
    n_doc_current <- dim(values$M)[1]
    n_doc_total <- dim(values$Morig)[1]

    n_sources_current <- length(unique(values$M$SO))
    n_sources_total <- length(unique(values$Morig$SO))

    n_authors_current <- length(unique(unlist(strsplit(values$M$AU, ";"))))
    n_authors_total <- length(unique(unlist(strsplit(values$Morig$AU, ";"))))

    HTML(paste0(
      "<div class='fade-in' style='
      border: 1px solid #ddd;
      border-radius: 10px;
      padding: 15px 20px;
      background-color: #ffffff;
      font-family: monospace;
      font-size: 15px;
      width: 100%;
      max-width: 400px;
      box-shadow: 0 1px 4px rgba(0, 0, 0, 0.1);
      margin-top: 15px;
    '>",

      "<div style='display: flex; justify-content: space-between;'>",
      "<div style='flex: 1; font-weight: bold;'>Documents</div>",
      "<div style='flex: 1; text-align: right;'>",
      n_doc_current,
      " of ",
      n_doc_total,
      "</div>",
      "</div>",

      "<div style='display: flex; justify-content: space-between; margin-top: 5px;'>",
      "<div style='flex: 1; font-weight: bold;'>Sources</div>",
      "<div style='flex: 1; text-align: right;'>",
      n_sources_current,
      " of ",
      n_sources_total,
      "</div>",
      "</div>",

      "<div style='display: flex; justify-content: space-between; margin-top: 5px;'>",
      "<div style='flex: 1; font-weight: bold;'>Authors</div>",
      "<div style='flex: 1; text-align: right;'>",
      n_authors_current,
      " of ",
      n_authors_total,
      "</div>",
      "</div>",

      "</div>"
    ))
  })

  #### Journal List Message ----
  output$journal_list_ui <- renderUI({
    req(input$journal_list_upload)
    div(
      id = "journal_list_helptext",
      helpText(
        "Journal list loaded. Now it will be used to filter the results.",
        style = "color:#dc3545"
      ),
      div(
        align = "center",
        actionBttn(
          inputId = "viewJournals",
          label = strong("View List"),
          width = "50%",
          style = "pill",
          color = "primary",
          size = "s",
          icon = icon(name = "eye", lib = "font-awesome")
        )
      ),
      br()
    )
  })

  observeEvent(input$viewJournals, {
    journal_list_html <- paste0(
      "<ul style='padding-left: 20px;'>",
      paste0(
        "<li>",
        stringi::stri_trans_totitle(values$journal_list),
        "</li>",
        collapse = ""
      ),
      "</ul>"
    )

    content_html <- paste0(
      "<div style='max-height: 300px; overflow-y: auto; text-align: left;'>",
      journal_list_html,
      "</div>"
    )

    shinyWidgets::show_alert(
      title = "Journal List Loaded",
      text = HTML(content_html),
      type = "info",
      html = TRUE,
      btn_labels = "OK"
    )
  })

  observeEvent(input$journal_list_upload, {
    req(input$journal_list_upload)

    journals <- read_journal_list(input$journal_list_upload$datapath)
    values$journal_list <- journals
    journal_list_html <- paste0(
      "<ul style='padding-left: 20px;'>",
      paste0(
        "<li>",
        stringi::stri_trans_totitle(journals),
        "</li>",
        collapse = ""
      ),
      "</ul>"
    )

    content_html <- paste0(
      "<div style='max-height: 300px; overflow-y: auto; text-align: left;'>",
      journal_list_html,
      "</div>"
    )

    shinyWidgets::show_alert(
      title = "Journal List Loaded",
      text = HTML(content_html),
      type = "info",
      html = TRUE,
      btn_labels = "OK"
    )
  })

  #### Journal Ranking list ----
  observeEvent(input$journal_ranking_upload, {
    req(input$journal_ranking_upload)
    # Load the journal ranking data
    ranking <- read_journal_ranking(input$journal_ranking_upload$datapath)
    if (!is.null(ranking)) {
      SOnotRanked <- setdiff(values$Morig$SO, ranking$SO)
      if (length(SOnotRanked) > 0) {
        ranking <- bind_rows(
          ranking,
          data.frame(
            SO = SOnotRanked,
            Ranking = rep("Not Ranked", length(SOnotRanked))
          )
        )
      }
    }
    values$journal_ranking <- ranking
    shinyjs::show("journal_ranking_subset")
  })

  output$journal_ranking_ui <- renderUI({
    req(values$journal_ranking)
    ranking <- values$journal_ranking %>%
      select("Ranking") %>%
      pull() %>%
      unique()
    selectInput(
      "journal_ranking_subset",
      "Select a Ranking Subset",
      choices = ranking,
      selected = ranking,
      multiple = TRUE
    )
  })

  output$journal_ranking_ui_view <- renderUI({
    req(input$journal_ranking_upload)
    div(
      id = "journal_ranking_helptext",
      helpText(
        "Journal Ranking list loaded. Now it will be used to filter the results.",
        style = "color:#dc3545"
      ),
      div(
        align = "center",
        actionBttn(
          inputId = "viewRankingJournals",
          label = strong("View Ranking"),
          width = "50%",
          style = "pill",
          color = "primary",
          size = "s",
          icon = icon(name = "eye", lib = "font-awesome")
        )
      ),
      br()
    )
  })

  observeEvent(input$viewRankingJournals, {
    journal_ranking_html <- paste0(
      "<ul style='padding-left: 20px;'>",
      paste0(
        "<li>",
        values$journal_ranking$Ranking,
        " -- ",
        stringi::stri_trans_totitle(values$journal_ranking$SO),
        "</li>",
        collapse = ""
      ),
      "</ul>"
    )

    content_html <- paste0(
      "<div style='max-height: 300px; overflow-y: auto; text-align: left;'>",
      journal_ranking_html,
      "</div>"
    )

    shinyWidgets::show_alert(
      title = "Journal Ranking List",
      text = HTML(content_html),
      type = "info",
      html = TRUE,
      btn_labels = "OK"
    )
  })

  #### Update Filter Inputs ----
  observe({
    req(values$Morig)
    if (!"TCpY" %in% names(values$Morig)) {
      values$Morig <- values$Morig %>%
        mutate(
          Age = as.numeric(substr(Sys.time(), 1, 4)) - PY + 1,
          TCpY = round(TC / Age, 2)
        ) %>%
        group_by(PY) %>%
        mutate(NTC = TC / mean(TC, na.rm = T)) %>%
        as.data.frame()
    }

    artType = sort(unique(values$Morig$DT))
    if ("LA" %in% names(values$Morig)) {
      LA <- sort(unique(values$Morig$LA))
    } else {
      values$Morig$LA <- "N.A."
      LA <- "N.A."
    }
    values$Morig$TCpY <- as.numeric(values$Morig$TCpY)

    updateSelectizeInput(
      session,
      "selectType",
      choices = artType,
      selected = artType,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "selectLA",
      choices = LA,
      selected = LA,
      server = TRUE
    )
    updateSliderInput(
      session,
      "sliderPY",
      min = min(values$Morig$PY, na.rm = T),
      max = max(values$Morig$PY, na.rm = T),
      value = c(
        min(values$Morig$PY, na.rm = T),
        max(values$Morig$PY, na.rm = T)
      )
    )
    updateMultiInput(
      session,
      "subject_category",
      choices = sort(unique(values$SCdf$WC)),
      selected = sort(unique(values$SCdf$WC))
    )
    updateSliderInput(
      session,
      "sliderTC",
      min = 0,
      max = max(values$Morig$TC),
      value = c(0, max(values$Morig$TC))
    )
    updateSliderInput(
      session,
      "sliderTCpY",
      min = floor(min(values$Morig$TCpY, na.rm = T)),
      max = ceiling(max(values$Morig$TCpY, na.rm = T)),
      step = 0.1,
      value = c(
        floor(min(values$Morig$TCpY, na.rm = T)),
        ceiling(max(values$Morig$TCpY, na.rm = T))
      )
    )
  })

  observe({
    req(values$SCdf)
    updateMultiInput(
      session,
      "subject_category",
      choices = sort(unique(values$SCdf$WC)),
      selected = sort(unique(values$SCdf$WC))
    )
    if (length(unique(values$SCdf$WC)) == 1) {
      if (unique(values$SCdf$WC) == "N.A.") shinyjs::hide("subject_category")
    } else {
      shinyjs::show("subject_category")
    }
  })

  observe({
    req(values$Morig) # assicurati che i dati siano già caricati
    CO <- sort(unique(
      values$COdf %>% dplyr::filter(continent %in% input$region) %>% pull(CO)
    ))
    updateMultiInput(session, "country", choices = CO, selected = CO)
  })

  #### Update filtered data ----
  observeEvent(input$applyFilter, {
    DTfiltered()
    updateTabItems(session, "sidebarmenu", "filters")
    showNotification(
      "Filters applied successfully.",
      type = "message",
      duration = 3
    )
  })

  #### Reset Filters ----
  observeEvent(input$resetFilter, {
    values$M <- values$Morig
    updateTabItems(session, "sidebarmenu", "filters")

    # reset del fileInput
    shinyjs::reset("journal_list_upload")
    shinyjs::hide("journal_list_helptext")
    values$journal_list <- unique(values$Morig$SO)
    values$journal_ranking <- NULL
    shinyjs::reset("journal_ranking_upload")
    shinyjs::hide("journal_ranking_subset")
    shinyjs::hide("journal_ranking_helptext")

    if (!"TCpY" %in% names(values$Morig)) {
      values$Morig <- values$Morig %>%
        mutate(
          Age = as.numeric(substr(Sys.time(), 1, 4)) - PY + 1,
          TCpY = round(TC / Age, 2)
        ) %>%
        group_by(PY) %>%
        mutate(NTC = TC / mean(TC, na.rm = T)) %>%
        as.data.frame()
    }

    artType = sort(unique(values$Morig$DT))
    if ("LA" %in% names(values$Morig)) {
      LA <- sort(unique(values$Morig$LA))
    } else {
      values$Morig$LA <- "N.A."
      LA <- "N.A."
    }

    updateSelectizeInput(
      session,
      "selectType",
      choices = artType,
      selected = artType,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "selectLA",
      choices = LA,
      selected = LA,
      server = TRUE
    )
    updateSliderInput(
      session,
      "sliderPY",
      min = min(values$Morig$PY, na.rm = T),
      max = max(values$Morig$PY, na.rm = T),
      value = c(
        min(values$Morig$PY, na.rm = T),
        max(values$Morig$PY, na.rm = T)
      )
    )
    updateMultiInput(
      session,
      "subject_category",
      choices = sort(unique(values$SCdf$WC)),
      selected = sort(unique(values$SCdf$WC))
    )

    updateSelectizeInput(
      session,
      "region",
      selected = c(
        "AFRICA",
        "ASIA",
        "EUROPE",
        "NORTH AMERICA",
        "SOUTH AMERICA",
        "SEVEN SEAS (OPEN OCEAN)",
        "OCEANIA",
        "Unknown"
      )
    ) # supponendo sia già calcolato
    CO <- sort(unique(
      values$COdf %>% dplyr::filter(continent %in% input$region) %>% pull(CO)
    ))
    updateMultiInput(session, "country", choices = unique(CO), selected = CO)
    updateSelectInput(session, "bradfordSources", selected = "all")
    if (!is.null(values$journal_ranking)) {
      updateSelectInput(
        session,
        "journal_raking_subset",
        selected = values$journal_ranking %>%
          select("Ranking") %>%
          pull() %>%
          unique()
      )
    }
    updateSliderInput(
      session,
      "sliderTC",
      min = 0,
      max = max(values$Morig$TC),
      value = c(0, max(values$Morig$TC))
    )
    updateSliderInput(
      session,
      "sliderTCpY",
      min = floor(min(values$Morig$TCpY, na.rm = T)),
      max = ceiling(max(values$Morig$TCpY, na.rm = T)),
      step = 0.1,
      value = c(
        floor(min(values$Morig$TCpY, na.rm = T)),
        ceiling(max(values$Morig$TCpY, na.rm = T))
      )
    )
    showNotification(
      "Filters reset to default.",
      type = "warning",
      duration = 3
    )
  })

  #### Filter Data Reactive Function ----
  DTfiltered <- eventReactive(input$applyFilter, {
    M <- values$Morig
    B <- bradford(M)$table
    # list of documents per subject categories
    wc <- values$SCdf %>%
      dplyr::filter(WC %in% input$subject_category) %>%
      pull(SR) %>%
      unique()
    # list of documents per country
    co <- values$COdf %>%
      dplyr::filter(CO %in% input$country) %>%
      pull(SR) %>%
      unique()

    if (!is.null(values$journal_list)) {
      M <- M %>%
        dplyr::filter(SO %in% values$journal_list) # filter by journal list
    }

    if (inherits(values$journal_ranking, "data.frame")) {
      soR <- values$journal_ranking %>%
        dplyr::filter(Ranking %in% input$journal_ranking_subset) %>%
        pull(SO) %>%
        unique()
      M <- M %>%
        dplyr::filter(SO %in% soR) # filter by journal ranking
    }

    M <- M %>%
      dplyr::filter(PY >= input$sliderPY[1], PY <= input$sliderPY[2]) %>% # publication year
      dplyr::filter(
        TCpY >= input$sliderTCpY[1],
        TCpY <= input$sliderTCpY[2]
      ) %>% # average citations per year
      dplyr::filter(TC >= input$sliderTC[1], TC <= input$sliderTC[2]) %>% # total citations
      dplyr::filter(DT %in% input$selectType) %>% # document type
      dplyr::filter(LA %in% input$selectLA) %>% # language
      dplyr::filter(SR %in% wc) %>% # subject categories
      dplyr::filter(SR %in% co) # countries

    switch(
      input$bradfordSources,
      "core" = {
        so <- B$SO[B$Zone %in% "Zone 1"]
      },
      "zone2" = {
        so <- B$SO[B$Zone %in% c("Zone 1", "Zone 2")]
      },
      "all" = {
        so <- B$SO
      }
    )
    M <- M %>%
      dplyr::filter(SO %in% so)

    values <- initial(values)
    row.names(M) <- M$SR
    class(M) <- c("bibliometrixDB", "data.frame")
    values$M <- M
  })

  #### Show Filtered Data Modal ----
  observeEvent(input$viewDataFilter, {
    showModal(modalDialog(
      title = "Filtered Data",
      size = "l",
      uiOutput("dataFiltered"),
      footer = tagList(
        #downloadButton("collection.save", "Save Filtered Data"),
        modalButton("Close")
      )
    ))
  })

  output$dataFiltered <- renderUI({
    DTfiltered()
    Mdisp <- values$M %>%
      select(SR, AU, TI, SO, PY, LA, DT, TC, TCpY, DI) %>%
      as.data.frame()

    if (dim(Mdisp)[1] > 0) {
      renderBibliobox(
        Mdisp,
        nrow = 3,
        filename = "Filtered_DataTable",
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
    } else {
      Mdisp = data.frame(Message = "Empty collection", row.names = " ")
    }
  })

  # OVERVIEW ----
  ### Main Info ----
  output$MainInfo <- renderUI({
    renderBibliobox(
      values$TABvb,
      nrow = 50,
      filename = "Main_Information",
      pagelength = TRUE,
      left = 1,
      right = 2,
      numeric = NULL,
      dom = TRUE,
      size = '100%',
      filter = "none",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  #### box1 ---------------
  output$Timespan <- renderValueBox({
    TAB <- ValueBoxes(values$M)
    values$TABvb <- TAB
    valueBox(
      value = p(
        TAB[TAB$Description == "Timespan", 1],
        style = 'font-size:16px;color:white;'
      ),
      subtitle = p(
        strong((TAB[TAB$Description == "Timespan", 2])),
        style = 'font-size:36px;color:white;',
        align = "center"
      ),
      icon = fa_i(name = "hourglass"),
      color = "blue",
      width = NULL
    )
  })

  #### box2 ---------------
  output$au <- renderValueBox({
    TAB <- values$TABvb
    valueBox(
      value = p(
        TAB[TAB$Description == "Authors", 1],
        style = 'font-size:16px;color:white;'
      ),
      subtitle = p(
        strong(TAB[TAB$Description == "Authors", 2]),
        style = 'font-size:36px;color:white;',
        align = "center"
      ),
      icon = fa_i(name = "user"),
      color = "light-blue",
      width = NULL
    )
  })

  #### box3 ------------
  output$kw <- renderValueBox({
    TAB <- values$TABvb
    valueBox(
      value = p(
        TAB[TAB$Description == "Author's Keywords (DE)", 1],
        style = 'font-size:16px;color:white;'
      ),
      subtitle = p(
        strong(TAB[TAB$Description == "Author's Keywords (DE)", 2]),
        style = 'font-size:36px;color:white;',
        align = "center"
      ),
      icon = fa_i(name = "spell-check"),
      color = "aqua",
      width = NULL
    )
  })

  #### box4 ---------------
  output$so <- renderValueBox({
    TAB <- values$TABvb
    valueBox(
      value = p("Sources", style = 'font-size:16px;color:white;'),
      subtitle = p(
        strong(TAB[TAB$Description == "Sources (Journals, Books, etc)", 2]),
        style = 'font-size:36px;color:white;',
        align = "center"
      ),
      icon = fa_i(name = "book"),
      color = "blue",
      width = NULL
    )
  })

  #### box5 --------------------
  output$auS1 <- renderValueBox({
    TAB <- values$TABvb
    valueBox(
      value = p(
        TAB[TAB$Description == "Authors of single-authored docs", 1],
        style = 'font-size:16px;color:white;'
      ),
      subtitle = p(
        strong(TAB[TAB$Description == "Authors of single-authored docs", 2]),
        style = 'font-size:36px;color:white;',
        align = "center"
      ),
      icon = fa_i(name = "pen-fancy"),
      color = "light-blue",
      width = NULL
    )
  })

  #### box6 -------------
  output$cr <- renderValueBox({
    TAB <- values$TABvb
    valueBox(
      value = p(
        TAB[TAB$Description == "References", 1],
        style = 'font-size:16px;color:white;'
      ),
      subtitle = p(
        strong(TAB[TAB$Description == "References", 2]),
        style = 'font-size:36px;color:white;',
        align = "center"
      ),
      icon = fa_i(name = "file"),
      color = "aqua",
      width = NULL
    )
  })

  #### box7 ----------------
  output$doc <- renderValueBox({
    TAB <- values$TABvb
    valueBox(
      value = p(
        TAB[TAB$Description == "Documents", 1],
        style = 'font-size:16px;color:white;'
      ),
      subtitle = p(
        strong(TAB[TAB$Description == "Documents", 2]),
        style = 'font-size:36px;color:white;',
        align = "center"
      ),
      icon = fa_i(name = "layer-group"),
      color = "blue",
      width = NULL
    )
  })

  #### box8 ---------------
  output$col <- renderValueBox({
    TAB <- values$TABvb
    valueBox(
      value = p(
        strong("International Co-Authorship"),
        style = 'font-size:16px;color:white;'
      ),
      subtitle = p(
        strong(
          TAB[TAB$Description == "International co-authorships %", 2],
          " %"
        ),
        style = 'font-size:36px;color:white;',
        align = "center"
      ),
      icon = icon("globe", lib = "glyphicon"),
      color = "light-blue",
      width = NULL
    )
  })

  #### box9 ---------------
  output$agePerDoc <- renderValueBox({
    TAB <- values$TABvb
    valueBox(
      value = p(
        TAB[TAB$Description == "Document Average Age", 1],
        style = 'font-size:16px;color:white;'
      ),
      subtitle = p(
        strong(TAB[TAB$Description == "Document Average Age", 2]),
        style = 'font-size:36px;color:white;',
        align = "center"
      ),
      icon = fa_i(name = "calendar"),
      color = "aqua",
      width = NULL
    )
  })

  #### box10 ------------------
  output$cagr <- renderValueBox({
    TAB <- values$TABvb
    valueBox(
      value = p(
        strong("Annual Growth Rate"),
        style = 'font-size:16px;color:white;'
      ),
      subtitle = p(
        strong(TAB[TAB$Description == "Annual Growth Rate %", 2], " %"),
        style = 'font-size:36px;color:white;',
        align = "center"
      ),
      icon = icon("arrow-up", lib = "glyphicon"),
      color = "blue",
      width = NULL
    )
  })

  #### box11 ------
  output$coAuPerDoc <- renderValueBox({
    TAB <- values$TABvb
    valueBox(
      value = p(
        TAB[TAB$Description == "Co-Authors per Doc", 1],
        style = 'font-size:16px;color:white;'
      ),
      subtitle = p(
        strong(TAB[TAB$Description == "Co-Authors per Doc", 2]),
        style = 'font-size:36px;color:white;',
        align = "center"
      ),
      icon = fa_i(name = "users"),
      color = "light-blue",
      width = NULL
    )
  })

  #### box12 -------
  output$tc <- renderValueBox({
    TAB <- values$TABvb
    valueBox(
      value = p(
        TAB[TAB$Description == "Average citations per doc", 1],
        style = 'font-size:16px;color:white;'
      ),
      subtitle = p(
        strong(TAB[TAB$Description == "Average citations per doc", 2]),
        style = 'font-size:36px;color:white;',
        align = "center"
      ),
      icon = icon("volume-up", lib = "glyphicon"),
      color = "aqua",
      width = NULL
    )
  })

  observeEvent(input$reportMI, {
    if (!is.null(values$TABvb)) {
      sheetname <- "MainInfo"
      list_df <- list(values$TABvb)
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      values$wb <- res$wb
      popUp(title = "Main Information", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  # gemini button for word network
  output$MainInfoGeminiUI <- renderUI({
    #values <- geminiWaitingMessage(values, input$sidebarmenu)
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$MainInfoGemini, values)
  })

  # Annual Production ----
  output$CAGR <- renderText({
    paste0(values$GR, " %")
  })

  output$AnnualProdPlot <- renderPlotly({
    res <- descriptive(values, type = "tab2")
    values <- res$values
    Y <- values$TAB

    names(Y) = c("Year", "Freq")
    x <- c(
      max(Y$Year) - 0.02 - diff(range(Y$Year)) * 0.125,
      max(Y$Year) - 0.02
    ) +
      1
    y <- c(min(Y$Freq), min(Y$Freq) + diff(range(Y$Freq)) * 0.125)

    g = ggplot2::ggplot(
      Y,
      aes(
        x = Year,
        y = Freq,
        text = paste("Year: ", Year, "\nN .of Documents: ", Freq)
      )
    ) +
      geom_line(aes(group = "NA")) +
      #geom_area(aes(group="NA"),fill = 'grey90', alpha = .5) +
      labs(x = 'Year', y = 'Articles', title = "Annual Scientific Production") +
      scale_x_continuous(breaks = (Y$Year[seq(1, length(Y$Year), by = 2)])) +
      theme(
        text = element_text(color = "#444444"),
        panel.background = element_rect(fill = '#FFFFFF'),
        panel.grid.minor = element_line(color = '#EFEFEF'),
        panel.grid.major = element_line(color = '#EFEFEF'),
        plot.title = element_text(size = 24),
        axis.title = element_text(size = 14, color = '#555555'),
        axis.title.y = element_text(vjust = 1, angle = 0),
        axis.title.x = element_text(hjust = 0),
        axis.text.x = element_text(vjust = 1, angle = 90),
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
    values$ASPplot <- g

    plot.ly(g, flip = FALSE, side = "r", aspectratio = 1.7, size = 0.10)
  })

  observeEvent(input$reportASP, {
    if (!is.null(values$TAB)) {
      list_df <- list(values$TAB)
      list_plot <- list(values$ASPplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "AnnualSciProd",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Annual Scientific Production", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  output$ASPplot.save <- downloadHandler(
    filename = function() {
      paste("AnnualScientificProduction-", Sys.Date(), ".png", sep = "")
    },

    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$ASPplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$AnnualProdTable <- renderUI({
    TAB <- values$TAB
    renderBibliobox(
      TAB,
      nrow = 10,
      filename = "Annual_Production",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = TRUE,
      size = '100%',
      filter = "none",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  ## Annual Citation per Year ----
  output$AnnualTotCitperYearPlot <- renderPlotly({
    current_year = as.numeric(substr(Sys.Date(), 1, 4)) + 1
    Table2 <- values$M %>%
      group_by(PY) %>%
      summarize(MeanTCperArt = round(mean(TC, na.rm = TRUE), 2), N = n()) %>%
      mutate(
        MeanTCperYear = round(MeanTCperArt / (current_year - PY), 2),
        CitableYears = current_year - PY
      ) %>%
      rename(Year = PY) %>%
      drop_na()
    values$AnnualTotCitperYear = Table2
    Table2$group = "A"

    x <- c(
      max(Table2$Year) - 0.02 - diff(range(Table2$Year)) * 0.125,
      max(Table2$Year) - 0.02
    ) +
      1
    y <- c(
      min(Table2$MeanTCperYear),
      min(Table2$MeanTCperYear) + diff(range(Table2$MeanTCperYear)) * 0.125
    )

    g <- ggplot(
      Table2,
      aes(
        x = Year,
        y = MeanTCperYear,
        text = paste(
          "Year: ",
          Year,
          "\nAverage Citations per Year: ",
          round(MeanTCperYear, 1)
        )
      )
    ) +
      geom_line(aes(x = Year, y = MeanTCperYear, group = group)) +
      #geom_area(aes(x = Year, y = MeanTCperYear, group=group),fill = 'grey90', alpha = .5) +
      labs(x = 'Year', y = 'Citations', title = "Average Citations per Year") +
      scale_x_continuous(
        breaks = (Table2$Year[seq(1, length(Table2$Year), by = 2)])
      ) +
      theme(
        text = element_text(color = "#444444"),
        panel.background = element_rect(fill = '#FFFFFF'),
        panel.grid.minor = element_line(color = '#EFEFEF'),
        panel.grid.major = element_line(color = '#EFEFEF'),
        plot.title = element_text(size = 24),
        axis.title = element_text(size = 14, color = '#555555'),
        axis.title.y = element_text(vjust = 1, angle = 0),
        axis.title.x = element_text(hjust = 0),
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
    values$ACpYplot <- g
    plot.ly(g, flip = FALSE, side = "r", aspectratio = 1.7, size = 0.10)
  })

  observeEvent(input$reportACpY, {
    if (!is.null(values$AnnualTotCitperYear)) {
      list_df <- list(values$AnnualTotCitperYear)
      list_plot <- list(values$ACpYplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "AnnualCitPerYear",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Average Citations per Year", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  output$ACpYplot.save <- downloadHandler(
    filename = function() {
      paste("AverageArticleCitationPerYear-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$ACpYplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$AnnualTotCitperYearTable <- renderUI({
    TAB <- values$AnnualTotCitperYear
    renderBibliobox(
      TAB,
      nrow = 10,
      filename = "Annual_Total_Citation_per_Year",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = c(2, 4),
      dom = TRUE,
      size = '100%',
      filter = "none",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  ## Life Cycle ----

  observeEvent(input$applyDLC, {
    req(values$M)
    if (nrow(values$M) > 1) {
      values$DLC <- lifeCycle(
        values$M %>% group_by(PY) %>% count(),
        forecast_years = 20,
        plot = FALSE
      )
      values$DLCplotYear <- plotLifeCycle(values$DLC, plot_type = "annual")
      values$DLCplotCum <- plotLifeCycle(values$DLC, plot_type = "cumulative")
      values$DLC$parameters <- values$DLC$parameters %>%
        as.data.frame() %>%
        tibble::rownames_to_column("Metrics") %>%
        rename(Value = ".")
    } else {
      values$DLC = NULL
    }
  })

  output$lifeCycleSummaryUIid <- renderUI({
    req(values$DLC)
    lifeCycleSummaryUI(values$DLC)
  })

  output$DLCPlotYear <- renderPlotly({
    req(values$DLCplotYear)
    values$DLCplotYear
  })

  output$DLCPlotCum <- renderPlotly({
    req(values$DLCplotCum)
    values$DLCplotCum
  })

  observeEvent(input$reportDLC, {
    if (!is.null(values$DLC)) {
      values$DLC$residuals <- values$DLC$residuals %>%
        as.data.frame() %>%
        tibble::rownames_to_column("Year") %>%
        rename(Residuals = ".")
      values$DLC$base_year <- values$DLC$base_year %>%
        as.data.frame() %>%
        rename(base_year = ".")
      list_df <- values$DLC
      list_plot <- list(
        ggplotLifeCycle(values$DLC, plot_type = c("annual")),
        ggplotLifeCycle(values$DLC, plot_type = c("cumulative"))
      )
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "LifeCycle",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Life Cycle", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  # gemini button for DLC
  output$DLCGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$DLCGemini, values)
  })

  output$DLCplot.save <- downloadHandler(
    filename = function() {
      paste("LifeCycle-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      tmpdir <- getWD()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))

      myfile <- NULL
      for (i in c("annual", "cumulative")) {
        filename <- basename(sub(".zip", paste0("_", i, ".png"), file))
        myfile <- c(myfile, filename)

        ggsave(
          filename = filename,
          plot = ggplotLifeCycle(values$DLC, plot_type = c(i)),
          dpi = values$dpi,
          height = values$h,
          width = values$h * 2,
          bg = "white"
        )
      }

      zip::zip(zipfile = file, files = myfile, include_directories = FALSE)
    },
    contentType = "zip"
  )

  ## Three Fields Plot ----
  TFP <- eventReactive(input$apply3F, {
    fields = c(input$LeftField, input$CentralField, input$RightField)
    threeFieldsPlot(
      values$M,
      fields = fields,
      n = c(input$LeftFieldn, input$CentralFieldn, input$RightFieldn)
    )
  })

  output$ThreeFieldsPlot <- renderPlotly({
    values$TFP <- TFP()

    is.reactive(values$TFP)
    values$TFP
  })

  # gemini button for Three Fields Plot
  output$TFPGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$TFPGemini, values)
  })

  observeEvent(input$reportTFP, {
    if (!is.null(values$TFP)) {
      sheetname <- "ThreeFieldsPlot"
      ind <- which(regexpr(sheetname, values$wb$sheet_names) > -1)
      if (length(ind) > 0) {
        sheetname <- paste(sheetname, "(", length(ind) + 1, ")", sep = "")
      }
      addWorksheet(wb = values$wb, sheetName = sheetname, gridLines = FALSE)
      values$fileTFP <- screenSh(values$TFP, zoom = 2, type = "plotly")
      values$list_file <- rbind(
        values$list_file,
        c(sheetname, values$fileTFP, 1)
      )
      popUp(title = "Three-Field Plot", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  # SOURCES MENU ----
  ### Most Relevant Sources ----
  observeEvent(input$applyMRSources, {
    M_data <- values$M
    k <- input$MostRelSourcesK
    logoGrid <- values$logoGrid

    showNotification(
      "Most Relevant Sources: computing...",
      id = "MRS_progress",
      type = "message",
      duration = NULL
    )

    p <- promises::future_promise(
      {
        TAB <- M_data %>%
          dplyr::group_by(SO) %>%
          dplyr::count() %>%
          dplyr::arrange(dplyr::desc(n)) %>%
          dplyr::rename(Sources = SO, Articles = n) %>%
          as.data.frame()
        TAB
      },
      seed = TRUE,
      packages = c("dplyr")
    ) %...>%
      (function(TAB) {
        removeNotification("MRS_progress")
        values$TAB <- TAB
        values$TABSo <- TAB
        xx <- TAB %>% tidyr::drop_na()
        if (k > nrow(xx)) {
          k <- nrow(xx)
        }
        xx <- xx %>% dplyr::slice_head(n = k)
        xx$Sources <- substr(xx$Sources, 1, 50)
        g <- freqPlot(
          xx,
          x = 2,
          y = 1,
          textLaby = "Sources",
          textLabx = "N. of Documents",
          title = "Most Relevant Sources",
          values
        )
        values$MRSplot <- g
        values$MRSources_ready <- values$MRSources_ready + 1
      }) %...!%
      (function(err) {
        removeNotification("MRS_progress")
        showNotification(
          paste("Sources error:", conditionMessage(err)),
          type = "error",
          duration = 8
        )
      })
    return(p)
  })

  output$MRSplot.save <- downloadHandler(
    filename = function() {
      paste("MostRelevantSources-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$MRSplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$MostRelSourcesPlot <- renderPlotly({
    req(values$MRSources_ready > 0)
    plot.ly(
      values$MRSplot,
      flip = FALSE,
      side = "r",
      aspectratio = 1.1,
      size = 0.10
    )
  })

  output$MostRelSourcesTable <- renderUI({
    req(values$MRSources_ready > 0)

    TAB <- values$TABSo %>% drop_na()
    renderBibliobox(
      TAB,
      nrow = 10,
      filename = "Most_Relevant_Sources",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportMRS, {
    if (!is.null(values$TABSo)) {
      list_df <- list(values$TABSo %>% drop_na())
      list_plot <- list(values$MRSplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "MostRelSources",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Most Relevant Sources", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Most Local Cited Sources ----
  MLCSources <- eventReactive(input$applyMLCSources, {
    values$M = metaTagExtraction(values$M, "CR_SO")
    TAB = tableTag(values$M, "CR_SO")
    TAB = data.frame(Sources = names(TAB), Articles = as.numeric(TAB))
    values$TABSoCit <- TAB
    xx <- TAB
    if (input$MostRelCitSourcesK > dim(xx)[1]) {
      k = dim(xx)[1]
    } else {
      k = input$MostRelCitSourcesK
    }
    xx = subset(xx, row.names(xx) %in% row.names(xx)[1:k])
    xx$Articles = as.numeric(xx$Articles)
    xx$Sources = substr(xx$Sources, 1, 50)

    g <- freqPlot(
      xx,
      x = 2,
      y = 1,
      textLaby = "Cited Sources",
      textLabx = "N. of Local Citations",
      title = "Most Local Cited Sources",
      values
    )

    values$MLCSplot <- g
    return(g)
  })

  output$MLCSplot.save <- downloadHandler(
    filename = function() {
      paste("MostLocalCitedSources-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$MLCSplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$MostRelCitSourcesPlot <- renderPlotly({
    g <- MLCSources()

    plot.ly(g, flip = FALSE, side = "r", aspectratio = 1.3, size = 0.10)
  })

  output$MostRelCitSourcesTable <- renderUI({
    g <- MLCSources()
    TAB <- values$TABSoCit
    renderBibliobox(
      TAB,
      nrow = 10,
      filename = "Most_Cited_Sources",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportMLS, {
    if (!is.null(values$TABSoCit)) {
      list_df <- list(values$TABSoCit)
      list_plot <- list(values$MLCSplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "MostLocCitSources",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Most Local Cited Sources", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Bradford's Law ----
  output$bradfordPlot <- renderPlotly({
    values$bradford = bradford(values$M)
    plot.ly(
      values$bradford$graph,
      flip = FALSE,
      side = "r",
      aspectratio = 1.6,
      size = 0.15
    )
  })

  output$BLplot.save <- downloadHandler(
    filename = function() {
      paste("BradfordLaws-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$bradford$graph,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$bradfordTable <- renderUI({
    renderBibliobox(
      values$bradford$table,
      nrow = 10,
      filename = "Bradford_Law",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportBradford, {
    if (!is.null(values$bradford$table)) {
      list_df <- list(values$bradford$table)
      list_plot <- list(values$bradford$graph)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "BradfordLaw",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Core Sources by Bradford's Law", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Sources' Impact ----
  Hsource <- eventReactive(input$applyHsource, {
    ## Cache check: skip Hindex_plot() if params unchanged
    cache_key <- make_cache_key(
      fp = data_fingerprint(values$M),
      type = "source",
      k = input$Hksource,
      measure = input$HmeasureSources
    )

    if (!identical(cache_key, values$cache_HSo_key)) {
      withProgress(message = 'Calculation in progress', value = 0, {
        res <- Hindex_plot(values, type = "source", input)
      })
      values$SIplot <- res$g
      values$cache_HSo_key <- cache_key
      values$cache_HSo_result <- plot.ly(
        res$g,
        flip = FALSE,
        side = "r",
        aspectratio = 1.3,
        size = 0.10
      )
    }
    values$cache_HSo_result
  })

  output$SIplot.save <- downloadHandler(
    filename = function() {
      paste("SourceImpact-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$SIplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$SourceHindexPlot <- renderPlotly({
    Hsource()
  })

  output$SourceHindexTable <- renderUI({
    renderBibliobox(
      values$H %>% rename(Source = Element),
      nrow = 10,
      filename = "Source_Impact",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 4,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportSI, {
    if (!is.null(values$H)) {
      list_df <- list(values$H %>% rename(Source = Element))
      list_plot <- list(values$SIplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "SourceLocImpact",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Sources' Local Impact", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Source Growth ----
  SOGrowth <- eventReactive(input$applySOGrowth, {
    if (input$cumSO == "Cum") {
      cdf = TRUE
      laby = "Cumulate occurrences"
    } else {
      cdf = FALSE
      laby = "Annual occurrences"
    }

    values$PYSO = sourceGrowth(values$M, input$topSO[2], cdf = cdf)
    if (input$topSO[1] > 1) {
      values$PYSO <- values$PYSO[-c(2:(input$topSO[1]))]
    }

    term = names(values$PYSO)[-1]

    term = rep(term, each = dim(values$PYSO)[1])
    n = dim(values$PYSO)[1] * (dim(values$PYSO)[2] - 1)
    freq = matrix(as.matrix(values$PYSO[, -1]), n, 1)
    values$SODF = data.frame(
      Year = rep(values$PYSO$Year, (dim(values$PYSO)[2] - 1)),
      Source = term,
      Freq = freq
    )

    Text <- paste(
      values$SODF$Source,
      " (",
      values$SODF$Year,
      ") ",
      values$SODF$Freq,
      sep = ""
    )

    width_scale <- 1.7 * 26 / length(unique(values$SODF$Source))

    x <- c(
      max(values$SODF$Year) - 0.02 - diff(range(values$SODF$Year)) * 0.15,
      max(values$SODF$Year) - 0.02
    ) +
      1
    y <- c(
      min(values$SODF$Freq),
      min(values$SODF$Freq) + diff(range(values$SODF$Freq)) * 0.15
    )

    g = ggplot(
      values$SODF,
      aes(
        x = values$SODF$Year,
        y = values$SODF$Freq,
        group = values$SODF$Source,
        color = values$SODF$Source,
        text = Text
      )
    ) +
      geom_line() +
      labs(x = 'Year', y = laby, title = "Sources' Production over Time") +
      scale_x_continuous(
        breaks = (values$PYSO$Year[seq(
          1,
          length(values$PYSO$Year),
          by = ceiling(length(values$PYSO$Year) / 20)
        )])
      ) +
      geom_hline(aes(yintercept = 0), alpha = 0.1) +
      labs(color = "Source") +
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
        panel.background = element_rect(fill = '#FFFFFF'),
        panel.grid.minor = element_line(color = '#EFEFEF'),
        panel.grid.major = element_line(color = '#EFEFEF'),
        plot.title = element_text(size = 24),
        axis.title = element_text(size = 14, color = '#555555'),
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

    values$SDplot <- g
    return(g)
  })

  output$SDplot.save <- downloadHandler(
    filename = function() {
      paste("SourceDynamics-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$SDplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$soGrowthPlot <- renderPlotly({
    g <- SOGrowth()

    leg <- list(
      orientation = 'h',
      y = -0.15,
      font = list(
        family = "sans-serif",
        size = 10,
        color = "#000"
      ),
      bgcolor = "#FFFFFF",
      bordercolor = "#FFFFFF",
      borderwidth = 2
    )

    plot.ly(g, flip = FALSE, side = "r", aspectratio = 1.8, size = 0.10) %>%
      layout(legend = leg) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          'toImage',
          'sendDataToCloud',
          'pan2d',
          'select2d',
          'lasso2d',
          'toggleSpikelines',
          'hoverClosestCartesian',
          'hoverCompareCartesian'
        )
      ) %>%
      layout(hovermode = 'compare')
  })

  output$soGrowthtable <- renderUI({
    g <- SOGrowth()
    soData = values$PYSO
    renderBibliobox(
      soData,
      nrow = 10,
      filename = "Source_Prod_over_Time",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportSD, {
    if (!is.null(values$PYSO)) {
      list_df <- list(values$PYSO)
      list_plot <- list(values$SDplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "SourceProdOverTime",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Sources' Production over Time", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  # AUTHORS MENU ----
  ## Authors ----

  ### Author Profile ----
  observe({
    req(values$M)
    if (!is.null(values$M$AU)) {
      authors <- values$M %>%
        select(AU, DI) %>%
        get_all_authors()
      updateSelectizeInput(
        session,
        "authorSearch",
        choices = c("", authors),
        selected = "",
        server = TRUE
      )
    }
  })

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$authorPageAReset
    },
    handlerExpr = {
      req(values$M)
      authors <- values$M %>%
        select(AU, DI) %>%
        get_all_authors()
      updateSelectizeInput(
        session,
        "authorSearch",
        choices = c("", authors),
        selected = "",
        server = TRUE
      )
      output$AuthorBioPageUI <- renderUI({
        create_empty_author_bio_card()
      })
      output$AuthorLocalProfileUI <- renderUI({
        create_empty_local_author_bio_card()
      })
    }
  )

  output$AuthorBioPageUI <- renderUI({
    create_empty_author_bio_card()
  })

  output$AuthorLocalProfileUI <- renderUI({
    create_empty_local_author_bio_card()
  })

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$authorPageApply
    },
    handlerExpr = {
      req(values$M)
      selected_author <- input$authorSearch

      # Show the spinner
      shinyjs::show("authorFetchingSpinner")

      # Use tryCatch to manage erros and the spinner
      tryCatch(
        {
          # Fetch data
          authorGlobalProfile <- authorCard(selected_author, values)

          output$AuthorBioPageUI <- renderUI({
            authorGlobalProfile
          })

          local_author <- values$author_data %>%
            dplyr::filter(AUid == selected_author) %>%
            pull(display_name)

          local_data <- values$M[
            gregexpr(selected_author, values$M$AU) > -1,
          ] %>%
            mutate(
              TI = to_title_case(TI),
              SO = to_title_case(SO)
            )

          authorLocalProfile <- create_local_author_bio_card(
            local_author_data = local_data,
            selected_author = local_author,
            max_py = values$M$PY %>% max(na.rm = TRUE),
            width = "100%",
            show_trends = TRUE,
            show_keywords = TRUE,
            max_keywords = 20,
            max_works_display = 50
          )

          output$AuthorLocalProfileUI <- renderUI({
            authorLocalProfile
          })
        },
        error = function(e) {
          # Manage the error
          showNotification(
            paste("Error fetching author data:", e$message),
            type = "error",
            duration = 5
          )
        },
        finally = {
          # Hide the spinner
          shinyjs::hide("authorFetchingSpinner")
        }
      )
    }
  )

  ### Most Relevant Authors ----
  observeEvent(input$applyMRAuthors, {
    M_data <- values$M
    k <- input$MostRelAuthorsK
    freq_measure <- input$AuFreqMeasure
    n_docs <- nrow(M_data)

    showNotification(
      "Most Relevant Authors: computing...",
      id = "MRA_progress",
      type = "message",
      duration = NULL
    )

    p <- promises::future_promise(
      {
        listAU <- strsplit(M_data$AU, ";")
        nAU <- lengths(listAU)
        fracAU <- rep(1 / nAU, nAU)
        TAB <- tibble::tibble(Author = unlist(listAU), fracAU = fracAU) %>%
          dplyr::group_by(Author) %>%
          dplyr::summarize(Articles = dplyr::n(), AuthorFrac = sum(fracAU)) %>%
          dplyr::arrange(dplyr::desc(Articles)) %>%
          as.data.frame()
        names(TAB) <- c("Authors", "Articles", "Articles Fractionalized")
        TAB
      },
      seed = TRUE,
      packages = c("dplyr", "tibble")
    ) %...>%
      (function(TAB) {
        removeNotification("MRA_progress")
        values$TAB <- TAB
        values$TABAu <- TAB
        xx <- TAB
        switch(
          freq_measure,
          t = {
            lab <- "N. of Documents"
            xx <- xx[, 1:2]
          },
          p = {
            xx <- xx[, 1:2]
            xx[, 2] <- as.numeric(xx[, 2]) / n_docs * 100
            lab <- "N. of Documents (in %)"
          },
          f = {
            xx <- xx[, c(1, 3)]
            lab <- "N. of Documents (Fractionalized)"
          }
        )
        xx[, 2] <- as.numeric(xx[, 2])
        if (k > nrow(xx)) {
          k <- nrow(xx)
        }
        xx <- xx[1:k, ]
        xx[, 2] <- round(xx[, 2], 1)
        xx <- xx[order(-xx[, 2]), ]
        g <- freqPlot(
          xx,
          x = 2,
          y = 1,
          textLaby = "Authors",
          textLabx = lab,
          title = "Most Relevant Authors",
          values
        )
        values$MRAplot <- g
        values$MRAuthors_ready <- values$MRAuthors_ready + 1
      }) %...!%
      (function(err) {
        removeNotification("MRA_progress")
        showNotification(
          paste("Authors error:", conditionMessage(err)),
          type = "error",
          duration = 8
        )
      })
    return(p)
  })

  output$MRAplot.save <- downloadHandler(
    filename = function() {
      paste("MostRelevantAuthors-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$MRAplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$MostRelAuthorsPlot <- renderPlotly({
    req(values$MRAuthors_ready > 0)
    plot.ly(
      values$MRAplot,
      flip = FALSE,
      side = "r",
      aspectratio = 1.3,
      size = 0.10
    )
  })

  output$MostRelAuthorsTable <- renderUI({
    TAB <- values$TABAu %>%
      rename("Author" = Authors)
    renderBibliobox(
      TAB,
      nrow = 10,
      filename = "Most_Relevant_Authors",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 3,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE,
      summary = "authors"
    )
  })

  observeEvent(input$reportMRA, {
    if (!is.null(values$TABAu)) {
      list_df <- list(values$TABAu)
      list_plot <- list(values$MRAplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "MostRelAuthors",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Most Relevant Authors", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Author BIO Card ----
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$selected_author
    },
    handlerExpr = {
      if (input$selected_author != "null") {
        showModal(modalAuthorBio(session))
      }
    }
  )

  modalAuthorBio <- function(session) {
    ns <- session$ns
    modalDialog(
      title = div("Author Profile", style = "text-align: center; margin: 0;"),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          title = "Global Profile",
          uiOutput(ns("AuthorBioPageUI2"))
        ),
        tabPanel(
          title = "Local Profile",
          uiOutput(ns("AuthorLocalProfileUI2"))
        )
      ),
      size = "l",
      easyClose = FALSE,
      footer = tagList(
        actionButton(
          label = "Close",
          inputId = "closeModalAuthorBio",
          style = "color: #ffff;",
          icon = icon("remove", lib = "glyphicon")
        )
      ),
    )
  }

  observeEvent(input$closeModalAuthorBio, {
    removeModal(session = getDefaultReactiveDomain())
    resetModalButtons(session = getDefaultReactiveDomain())
  })

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$selected_author
    },
    handlerExpr = {
      req(values$M)
      selected_author <- input$selected_author
      authorGlobalProfile <- authorCard(selected_author, values)
      output$AuthorBioPageUI2 <- renderUI({
        authorGlobalProfile
      })

      local_author <- values$author_data %>%
        dplyr::filter(AUid == selected_author) %>%
        pull(display_name)

      local_data <- values$M[gregexpr(selected_author, values$M$AU) > -1, ] %>%
        mutate(TI = to_title_case(TI), SO = to_title_case(SO))

      authorLocalProfile <- create_local_author_bio_card(
        local_author_data = local_data,
        selected_author = local_author,
        max_py = values$M$PY %>% max(na.rm = TRUE),
        width = "100%",
        show_trends = TRUE,
        show_keywords = TRUE,
        max_keywords = 20,
        max_works_display = 50
      )
      output$AuthorLocalProfileUI2 <- renderUI({
        authorLocalProfile
      })
    }
  )

  ### Most Cited Authors ----
  observeEvent(input$applyMLCAuthors, {
    M_data <- values$M
    k <- input$MostCitAuthorsK

    showNotification(
      "Most Local Cited Authors: computing...",
      id = "MLCA_progress",
      type = "message",
      duration = NULL
    )

    p <- promises::future_promise(
      {
        CR <- bibliometrix::localCitations(
          M_data,
          fast.search = FALSE,
          verbose = FALSE
        )
        CR$Authors
      },
      seed = TRUE,
      packages = c("bibliometrix", "dplyr")
    ) %...>%
      (function(TAB) {
        removeNotification("MLCA_progress")
        values$TAB <- TAB
        values$TABAuCit <- TAB
        xx <- TAB
        xx[, 2] <- as.numeric(xx[, 2])
        if (k > nrow(xx)) {
          k <- nrow(xx)
        }
        xx <- xx[1:k, ]
        xx[, 2] <- round(xx[, 2], 1)
        xx <- xx[order(-xx[, 2]), ]
        g <- freqPlot(
          xx,
          x = 2,
          y = 1,
          textLaby = "Authors",
          textLabx = "Local Citations",
          title = "Most Local Cited Authors",
          values
        )
        values$MLCAplot <- g
        values$MLCAuthors_ready <- values$MLCAuthors_ready + 1
      }) %...!%
      (function(err) {
        removeNotification("MLCA_progress")
        showNotification(
          paste("Local Citations error:", conditionMessage(err)),
          type = "error",
          duration = 8
        )
      })
    return(p)
  })

  output$MLCAplot.save <- downloadHandler(
    filename = function() {
      paste("MostLocalCitedAuthors-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$MLCAplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$MostCitAuthorsPlot <- renderPlotly({
    req(values$MLCAuthors_ready > 0)
    plot.ly(
      values$MLCAplot,
      flip = FALSE,
      side = "r",
      aspectratio = 1.3,
      size = 0.10
    )
  })

  output$MostCitAuthorsTable <- renderUI({
    TAB <- values$TABAuCit
    renderBibliobox(
      TAB,
      nrow = 10,
      filename = "Most_Local_Cited_Authors",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportMLCA, {
    if (!is.null(values$TABAuCit)) {
      list_df <- list(values$TABAuCit)
      list_plot <- list(values$MLCAplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "MostLocCitAuthors",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Most Local Cited Authors", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Authors' Impact ----
  HAuthors <- eventReactive(input$applyHAuthors, {
    ## Cache check: skip Hindex_plot() if params unchanged
    cache_key <- make_cache_key(
      fp = data_fingerprint(values$M),
      type = "author",
      k = input$Hkauthor,
      measure = input$HmeasureAuthors
    )

    if (!identical(cache_key, values$cache_HAu_key)) {
      withProgress(message = 'Calculation in progress', value = 0, {
        res <- Hindex_plot(values, type = "author", input)
      })
      values$AIplot <- res$g
      values$cache_HAu_key <- cache_key
      values$cache_HAu_result <- res
    }
    return(values$cache_HAu_result)
  })

  output$AIplot.save <- downloadHandler(
    filename = function() {
      paste("AuthorImpact-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$AIplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$AuthorHindexPlot <- renderPlotly({
    res <- HAuthors()
    plot.ly(res$g, flip = FALSE, side = "r", aspectratio = 1.3, size = 0.10)
  })

  output$AuthorHindexTable <- renderUI({
    renderBibliobox(
      values$H %>% rename(Author = Element),
      nrow = 10,
      filename = "Author_Impact",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 4,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportAI, {
    if (!is.null(values$H)) {
      list_df <- list(values$H %>% rename(Author = Element))
      list_plot <- list(values$AIplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "AuthorLocImpact",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Authors' Local Impact", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Authors Production Over Time ----
  AUoverTime <- eventReactive(input$applyAUoverTime, {
    values$AUProdOverTime <- authorProdOverTime(
      values$M,
      k = input$TopAuthorsProdK,
      graph = FALSE
    )
  })

  # gemini button for Apot
  output$ApotGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$ApotGemini, values)
  })

  output$APOTplot.save <- downloadHandler(
    filename = function() {
      paste("AuthorsProductionOverTime-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$AUProdOverTime$graph,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2.5,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$TopAuthorsProdPlot <- renderPlotly({
    AUoverTime()
    plot.ly(
      values$AUProdOverTime$graph,
      flip = TRUE,
      side = "l",
      aspectratio = 1
    )
  })

  output$TopAuthorsProdTable <- renderUI({
    AUoverTime()

    TAB <- values$AUProdOverTime$dfAU
    renderBibliobox(
      TAB,
      nrow = 10,
      filename = "Author_Prod_over_Time",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 5,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$TopAuthorsProdTablePapers <- renderUI({
    AUoverTime()
    TAB <- values$AUProdOverTime$dfPapersAU
    TAB$DOI = paste0(
      '<a href=\"https://doi.org/',
      TAB$DOI,
      '\" target=\"_blank\">',
      TAB$DOI,
      '</a>'
    )
    renderBibliobox(
      TAB,
      nrow = 10,
      filename = "Author_Prod_over_Time_Docs",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 7,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportAPOT, {
    if (!is.null(values$AUProdOverTime$dfPapersAU)) {
      list_df <- list(values$AUProdOverTime$dfPapersAU)
      list_plot <- list(values$AUProdOverTime$graph)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "AuthorProdOverTime",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Authors' Production over Time", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Lotka Law ----
  output$lotkaPlot <- renderPlotly({
    values$lotka <- lotka(values$M)
    values$LLplot <- values$lotka$g
    plot.ly(
      values$lotka$g_shiny,
      flip = FALSE,
      side = "r",
      aspectratio = 1.4,
      size = 0.10
    )
  })

  output$LLplot.save <- downloadHandler(
    filename = function() {
      paste("LotkaLaw-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$LLplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$lotkaTable <- renderUI({
    renderBibliobox(
      values$lotka$AuthorProd,
      nrow = 10,
      filename = "Lotka_Law",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = c(3, 4),
      dom = FALSE,
      size = '100%',
      filter = "none",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportLotka, {
    if (!is.null(values$lotka$AuthorProd)) {
      list_df <- list(values$lotka$AuthorProd)
      list_plot <- list(values$LLplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "LotkaLaw",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Author Productivity through Lotka's Law", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  # Affiliations ----
  ### Most Relevant Affiliations ----
  observeEvent(input$applyMRAffiliations, {
    M_data <- values$M
    k <- input$MostRelAffiliationsK
    disAff <- input$disAff

    # Synchronous: metaTagExtraction if needed
    if (disAff == "Y" && !("AU_UN" %in% names(M_data))) {
      M_data <- metaTagExtraction(M_data, Field = "AU_UN")
      values$M <- M_data
    }

    showNotification(
      "Affiliations: computing...",
      id = "AFF_progress",
      type = "message",
      duration = NULL
    )

    p <- promises::future_promise(
      {
        if (disAff == "Y") {
          TAB <- data.frame(
            Affiliation = unlist(strsplit(M_data$AU_UN, ";"))
          ) %>%
            dplyr::group_by(Affiliation) %>%
            dplyr::count() %>%
            tidyr::drop_na(Affiliation) %>%
            dplyr::arrange(dplyr::desc(n)) %>%
            dplyr::rename(Articles = n) %>%
            dplyr::filter(Affiliation != "NA") %>%
            as.data.frame()
        } else {
          TAB <- bibliometrix::tableTag(M_data, "C1")
          TAB <- data.frame(
            Affiliations = names(TAB),
            Articles = as.numeric(TAB)
          )
          TAB <- TAB[nchar(TAB[, 1]) > 4, ]
        }
        TAB
      },
      seed = TRUE,
      packages = c("dplyr", "tidyr", "bibliometrix")
    ) %...>%
      (function(TAB) {
        removeNotification("AFF_progress")
        values$TAB <- TAB
        values$TABAff <- TAB
        xx <- TAB
        names(xx) <- c("AFF", "Freq")
        if (k > nrow(xx)) {
          k <- nrow(xx)
        }
        xx <- xx[1:k, ]
        g <- freqPlot(
          xx,
          x = 2,
          y = 1,
          textLaby = "Affiliations",
          textLabx = "Articles",
          title = "Most Relevant Affiliations",
          values
        )
        values$AFFplot <- g
        values$MRAff_ready <- values$MRAff_ready + 1
      }) %...!%
      (function(err) {
        removeNotification("AFF_progress")
        showNotification(
          paste("Affiliations error:", conditionMessage(err)),
          type = "error",
          duration = 8
        )
      })
    return(p)
  })

  output$AFFplot.save <- downloadHandler(
    filename = function() {
      paste("MostRelevantAffiliations-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$AFFplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$MostRelAffiliationsPlot <- renderPlotly({
    req(values$MRAff_ready > 0)
    plot.ly(
      values$AFFplot,
      flip = FALSE,
      side = "r",
      aspectratio = 1,
      size = 0.15
    )
  })

  output$MostRelAffiliationsTable <- renderUI({
    req(values$MRAff_ready > 0)

    TAB <- values$TABAff
    renderBibliobox(
      TAB,
      nrow = 10,
      filename = "Most_Relevant_Affiliations",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportMRAFF, {
    if (!is.null(values$TABAff)) {
      list_df <- list(values$TABAff)
      list_plot <- list(values$AFFplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "MostRelAffiliations",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Most Relevant Affiliations", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Affiliation OverTime ----
  AFFGrowth <- eventReactive(input$applyAFFGrowth, {
    values <- AffiliationOverTime(values, input$topAFF)
  })

  output$AffOverTimeplot.save <- downloadHandler(
    filename = function() {
      paste("AffiliationOverTime-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$AffOverTimePlot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$AffOverTimePlot <- renderPlotly({
    AFFGrowth()
    g <- values$AffOverTimePlot
    leg <- list(
      orientation = 'h',
      y = -0.15,
      font = list(
        family = "sans-serif",
        size = 10,
        color = "#000"
      ),
      bgcolor = "#FFFFFF",
      bordercolor = "#FFFFFF",
      borderwidth = 2
    )

    plot.ly(g, flip = FALSE, side = "r", aspectratio = 1.8, size = 0.10) %>%
      layout(legend = leg) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          'sendDataToCloud',
          'pan2d',
          'select2d',
          'lasso2d',
          'toggleSpikelines'
        )
      ) %>%
      layout(hovermode = 'compare')
  })

  output$AffOverTimeTable <- renderUI({
    AFFGrowth()
    afftimeData <- values$AffOverTime
    renderBibliobox(
      afftimeData,
      nrow = 10,
      filename = "Affiliation_over_Time",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportAFFPOT, {
    if (!is.null(values$AffOverTime)) {
      list_df <- list(values$AffOverTime)
      list_plot <- list(values$AffOverTimePlot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "AffOverTime",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Affiliations' Production over Time", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  # Countries ----
  ### Country by Corresponding Authors ----
  observeEvent(input$applyCAUCountries, {
    M_data <- values$M
    k <- input$MostRelCountriesK
    logoGrid <- values$logoGrid

    showNotification(
      "Countries: computing...",
      id = "CAU_progress",
      type = "message",
      duration = NULL
    )

    p <- promises::future_promise(
      {
        countryCollab(M_data)
      },
      seed = TRUE,
      globals = list(
        countryCollab = countryCollab,
        M_data = M_data,
        trim = trim
      ),
      packages = c("bibliometrix", "dplyr")
    ) %...>%
      (function(TABCo) {
        removeNotification("CAU_progress")
        TABCo <- TABCo %>%
          dplyr::mutate(Freq = Articles / sum(Articles)) %>%
          dplyr::mutate(MCP_Ratio = MCP / Articles) %>%
          tidyr::drop_na(Country)
        values$TAB <- TABCo

        xx <- TABCo %>%
          dplyr::slice_head(n = k) %>%
          dplyr::select(Country, SCP, MCP)
        xx <- xx[order(-(xx$SCP + xx$MCP)), ]
        xx1 <- cbind(xx[, 1:2], rep("SCP", k))
        names(xx1) <- c("Country", "Freq", "Collaboration")
        xx2 <- cbind(xx[, c(1, 3)], rep("MCP", k))
        names(xx2) <- c("Country", "Freq", "Collaboration")
        xx <- rbind(xx2, xx1)
        xx$Country <- factor(xx$Country, levels = xx$Country[1:nrow(xx2)])

        xx2 <- xx %>%
          dplyr::group_by(Country) %>%
          dplyr::summarize(Freq = sum(Freq))
        x <- c(0.5, 0.5 + length(levels(xx2$Country)) * 0.125) + 1
        y <- c(
          max(xx2$Freq) - 0.02 - diff(range(xx2$Freq)) * 0.125,
          max(xx2$Freq) - 0.02
        )

        g <- suppressWarnings(
          ggplot2::ggplot(
            data = xx,
            aes(
              x = Country,
              y = Freq,
              fill = Collaboration,
              text = paste("Country: ", Country, "\nN.of Documents: ", Freq)
            )
          ) +
            geom_bar(aes(group = "NA"), stat = "identity") +
            scale_x_discrete(limits = rev(levels(xx$Country))) +
            scale_fill_discrete(
              name = "Collaboration",
              breaks = c("SCP", "MCP")
            ) +
            labs(
              title = "Corresponding Author's Countries",
              x = "Countries",
              y = "N. of Documents",
              caption = "SCP: Single Country Publications, MCP: Multiple Country Publications"
            ) +
            theme(
              plot.caption = element_text(
                size = 9,
                hjust = 0.5,
                color = "blue",
                face = "italic"
              ),
              panel.background = element_rect(fill = '#FFFFFF'),
              panel.grid.major.y = element_line(color = '#EFEFEF'),
              plot.title = element_text(size = 24),
              axis.title = element_text(size = 14, color = '#555555'),
              axis.title.y = element_text(vjust = 1, angle = 0),
              axis.title.x = element_text(hjust = 0),
              axis.line.x = element_line(color = "black", linewidth = 0.5)
            ) +
            coord_flip()
        ) +
          annotation_custom(
            logoGrid,
            xmin = x[1],
            xmax = x[2],
            ymin = y[1],
            ymax = y[2]
          )

        values$TABCo <- TABCo %>%
          dplyr::mutate(Freq = Freq * 100, MCP_Ratio = MCP_Ratio * 100) %>%
          dplyr::rename("Articles %" = Freq, "MCP %" = MCP_Ratio) %>%
          dplyr::select(Country, "Articles", "Articles %", SCP, MCP, "MCP %")
        values$MRCOplot <- g
        values$CAUCountries_ready <- values$CAUCountries_ready + 1
      }) %...!%
      (function(err) {
        removeNotification("CAU_progress")
        showNotification(
          paste("Countries error:", conditionMessage(err)),
          type = "error",
          duration = 8
        )
      })
    return(p)
  })

  # gemini button for word network
  output$MostRelCountriesGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$MostRelCountriesGemini, values)
  })

  output$MRCOplot.save <- downloadHandler(
    filename = function() {
      paste("MostRelevantCountries-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$MRCOplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$MostRelCountriesPlot <- renderPlotly({
    req(values$CAUCountries_ready > 0)
    plot.ly(
      values$MRCOplot,
      flip = T,
      side = "r",
      aspectratio = 1.4,
      size = 0.10,
      data.type = 1
    )
  })

  output$MostRelCountriesTable <- renderUI({
    req(values$CAUCountries_ready > 0)

    TAB <- values$TABCo
    renderBibliobox(
      TAB,
      nrow = 10,
      filename = "Most_Relevant_Countries_By_Corresponding_Author",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = c(3, 6),
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 1,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportMRCO, {
    if (!is.null(values$TABCo)) {
      list_df <- list(values$TABCo)
      list_plot <- list(values$MRCOplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "CorrAuthCountries",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Corresponding Author's Countries", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Country Production ----
  output$countryProdPlot <- renderPlotly({
    values$mapworld <- mapworld(values$M, values)
    plot.ly(
      values$mapworld$g,
      flip = FALSE,
      side = "r",
      aspectratio = 1.7,
      size = 0.07,
      data.type = 1,
      height = 15
    )
  })

  output$CSPplot.save <- downloadHandler(
    filename = function() {
      paste("CountryScientificProduction-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$mapworld$g,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$countryProdTable <- renderUI({
    TAB <- values$mapworld$tab %>% rename(Country = region)
    renderBibliobox(
      TAB,
      nrow = 10,
      filename = "Country_Production",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportCSP, {
    if (!is.null(values$mapworld$tab)) {
      list_df <- list(values$mapworld$tab)
      list_plot <- list(values$mapworld$g)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "CountrySciProd",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Countries' Scientific Production", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Countries' Production Over Time ----
  COGrowth <- eventReactive(input$applyCOGrowth, {
    values <- CountryOverTime(values, input$topCO)
  })

  output$CountryOverTimeplot.save <- downloadHandler(
    filename = function() {
      paste("CountryOverTime-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$CountryOverTimePlot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$CountryOverTimePlot <- renderPlotly({
    COGrowth()
    g <- values$CountryOverTimePlot
    leg <- list(
      orientation = 'h',
      y = -0.15,
      font = list(
        family = "sans-serif",
        size = 10,
        color = "#000"
      ),
      bgcolor = "#FFFFFF",
      bordercolor = "#FFFFFF",
      borderwidth = 2
    )

    plot.ly(g, flip = FALSE, side = "r", aspectratio = 1.8, size = 0.10) %>%
      layout(legend = leg) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          'toImage',
          'sendDataToCloud',
          'pan2d',
          'select2d',
          'lasso2d',
          'toggleSpikelines',
          'hoverClosestCartesian',
          'hoverCompareCartesian'
        )
      ) %>%
      layout(hovermode = 'compare')
  })

  output$CountryOverTimeTable <- renderUI({
    COGrowth()
    cotimeData = values$CountryOverTime
    renderBibliobox(
      cotimeData,
      nrow = 10,
      filename = "Countries_Production_Over_Time",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportCPOT, {
    if (!is.null(values$CountryOverTime)) {
      list_df <- list(values$CountryOverTime)
      list_plot <- list(values$CountryOverTimePlot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "CountryProdOverTime",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Countries' Production over Time", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Most Cited Country ----
  observeEvent(input$applyMCCountries, {
    M_data <- values$M
    k <- input$MostCitCountriesK
    measure <- input$CitCountriesMeasure

    # Synchronous: metaTagExtraction if needed
    if (!"AU1_CO" %in% names(M_data)) {
      M_data <- metaTagExtraction(M_data, "AU1_CO")
      values$M <- M_data
    }

    showNotification(
      "Most Cited Countries: computing...",
      id = "MCC_progress",
      type = "message",
      duration = NULL
    )

    p <- promises::future_promise(
      {
        M_data %>%
          dplyr::select(AU1_CO, TC) %>%
          tidyr::drop_na(AU1_CO) %>%
          dplyr::rename(Country = AU1_CO, TotalCitation = TC) %>%
          dplyr::group_by(Country) %>%
          dplyr::summarise(
            "TC" = sum(TotalCitation),
            "Average Article Citations" = round(
              sum(TotalCitation) / length(TotalCitation),
              1
            )
          ) %>%
          dplyr::arrange(-TC) %>%
          as.data.frame()
      },
      seed = TRUE,
      packages = c("dplyr", "tidyr")
    ) %...>%
      (function(TAB) {
        removeNotification("MCC_progress")
        values$TAB <- TAB
        values$TABCitCo <- TAB
        xx <- TAB
        xx[, 2] <- as.numeric(xx[, 2])
        xx[, 3] <- as.numeric(xx[, 3])
        if (k > nrow(xx)) {
          k <- nrow(xx)
        }
        if (measure == "TC") {
          xx <- xx[1:k, c(1, 2)]
          laby <- "N. of Citations"
        } else {
          xx <- xx[order(-xx[, 3]), ]
          xx <- xx[1:k, c(1, 3)]
          laby <- "Average Article Citations"
        }
        g <- freqPlot(
          xx,
          x = 2,
          y = 1,
          textLaby = "Countries",
          textLabx = laby,
          title = "Most Cited Countries",
          values
        )
        values$MCCplot <- g
        values$MCCountries_ready <- values$MCCountries_ready + 1
      }) %...!%
      (function(err) {
        removeNotification("MCC_progress")
        showNotification(
          paste("Most Cited Countries error:", conditionMessage(err)),
          type = "error",
          duration = 8
        )
      })
    return(p)
  })

  output$MCCplot.save <- downloadHandler(
    filename = function() {
      paste("MostCitedCountries-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$MCCplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$MostCitCountriesPlot <- renderPlotly({
    req(values$MCCountries_ready > 0)
    plot.ly(
      values$MCCplot,
      flip = FALSE,
      side = "r",
      aspectratio = 1.3,
      size = 0.10
    )
  })

  output$MostCitCountriesTable <- renderUI({
    req(values$MCCountries_ready > 0)
    TAB <- values$TABCitCo
    renderBibliobox(
      TAB,
      nrow = 10,
      filename = "Most_Cited_Countries",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 3,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportMCCO, {
    if (!is.null(values$TABCitCo)) {
      list_df <- list(values$TABCitCo)
      list_plot <- list(values$MCCplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "MostCitCountries",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Most Cited Countries", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  # DOCUMENTS MENU ----
  ## Documents ----

  ### function to show summary modal
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$button_id
    },
    handlerExpr = {
      if (input$button_id != "null") {
        showModal(modalDocSummary(session))
      }
    }
  )

  modalDocSummary <- function(session) {
    ns <- session$ns
    modalDialog(
      shinycssloaders::withSpinner(
        uiOutput(ns("DocSummary")),
        caption = HTML("<br><strong>Thinking...</strong>"),
        image = "ai_small2.gif",
        color = "#466fc4"
      ),
      #uiOutput(ns("DocSummary")),
      size = "l",
      easyClose = FALSE,
      footer = tagList(
        actionButton(
          label = "Close",
          inputId = "closeModalDocSummary",
          style = "color: #ffff;",
          icon = icon("remove", lib = "glyphicon")
        )
      ),
    )
  }

  observeEvent(input$closeModalDocSummary, {
    removeModal(session = getDefaultReactiveDomain())
    # session$sendCustomMessage("click", 'null') # reset input value to plot modal more times
    resetModalButtons(session = getDefaultReactiveDomain())
  })

  # Reactive value to store async DocSummary result
  docSummaryAI <- reactiveVal(NULL)

  # Trigger async summary when button_id changes
  observeEvent(
    input$button_id,
    {
      if (is.null(input$button_id) || input$button_id == "null") {
        return()
      }
      id <- input$button_id
      i <- which(values$M$SR == id)
      if (length(i) == 0) {
        return()
      }

      docSummaryAI(NULL) # Reset to trigger spinner

      # Snapshot values for the future
      M_TI <- values$M$TI[i]
      M_AU <- values$M$AU[i]
      M_SO <- values$M$SO[i]
      M_PY <- values$M$PY[i]
      M_AB <- values$M$AB[i]
      M_DI <- values$M$DI[i]
      model <- values$gemini_api_model
      idx <- i

      promises::future_promise(
        {
          summaryAI_data <- list(
            M = list(
              TI = M_TI,
              AU = M_AU,
              SO = M_SO,
              PY = M_PY,
              AB = M_AB,
              DI = M_DI
            )
          )
          summaryAI(summaryAI_data, i = 1, model = model)
        },
        seed = TRUE
      ) %...>%
        (function(result) {
          docSummaryAI(result)
        }) %...!%
        (function(err) {
          docSummaryAI(paste("Error:", conditionMessage(err)))
        })
    },
    ignoreNULL = TRUE
  )

  output$DocSummary <- renderUI({
    if (!is.null(input$button_id)) {
      id <- input$button_id
    }
    i <- which(values$M$SR == id)

    # Check if index is valid
    if (length(i) == 0) {
      return(NULL)
    }

    res <- docSummaryAI()
    req(res) # Wait for async result

    # Create HTML card
    div(
      class = "document-summary-card",
      style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; padding: 20px; margin: 10px 0; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;",

      # Card header
      div(
        style = "border-bottom: 2px solid #007bff; margin-bottom: 15px; padding-bottom: 10px;",
        h3(
          style = "color: #007bff; margin: 0; font-size: 1.4em; font-weight: 600;",
          icon("file-text", class = "fa fa-file-text"),
          " Document Summary"
        )
      ),

      # Bibliographic information
      div(
        class = "bibliographic-info",
        style = "margin-bottom: 20px;",

        # Title
        div(
          style = "margin-bottom: 12px;",
          strong("Title:", style = "color: #495057; font-weight: 600;"),
          br(),
          span(
            style = "font-size: 1.3em; color: #212529; line-height: 1.4;",
            ifelse(
              is.na(values$M$TI[i]) || values$M$TI[i] == "",
              "Not available",
              to_title_case(values$M$TI[i])
            )
          )
        ),

        # Authors
        div(
          style = "margin-bottom: 12px;",
          strong("Authors:", style = "color: #495057; font-weight: 600;"),
          br(),
          span(
            style = "color: #6c757d; font-style: italic;",
            ifelse(
              is.na(values$M$AU[i]) || values$M$AU[i] == "",
              "Not available",
              to_title_case(gsub(";", "; ", values$M$AU[i]))
            )
          )
        ),

        # Publication info in one row
        div(
          style = "display: flex; flex-wrap: wrap; gap: 20px; margin-bottom: 12px;",

          div(
            strong("Journal:", style = "color: #495057; font-weight: 600;"),
            br(),
            span(
              style = "color: #28a745; font-weight: 500;",
              ifelse(
                is.na(values$M$SO[i]) || values$M$SO[i] == "",
                "N/A",
                to_title_case(values$M$SO[i])
              )
            )
          ),

          div(
            strong("Year:", style = "color: #495057; font-weight: 600;"),
            br(),
            span(
              style = "color: #ffc107; font-weight: 600;",
              ifelse(
                is.na(values$M$PY[i]) || values$M$PY[i] == "",
                "N/A",
                values$M$PY[i]
              )
            )
          ),

          div(
            strong("DOI:", style = "color: #495057; font-weight: 600;"),
            br(),
            if (!is.na(values$M$DI[i]) && values$M$DI[i] != "") {
              a(
                href = paste0("https://doi.org/", values$M$DI[i]),
                target = "_blank",
                style = "color: #dc3545; text-decoration: none; font-weight: 500;",
                values$M$DI[i],
                icon(
                  "external-link-alt",
                  class = "fa fa-external-link-alt",
                  style = "margin-left: 5px; font-size: 0.8em;"
                )
              )
            } else {
              span(style = "color: #6c757d;", "Not available")
            }
          )
        )
      ),

      # Abstract (if available)
      if (!is.na(values$M$AB[i]) && values$M$AB[i] != "") {
        div(
          style = "margin-bottom: 20px; padding: 15px; background-color: #e9ecef; border-radius: 6px; border-left: 4px solid #6c757d;",
          div(
            style = "display: flex; align-items: center; margin-bottom: 10px;",
            strong(
              "Abstract:",
              style = "color: #495057; font-weight: 600; font-size: 1.05em;"
            ),
          ),
          # strong("Abstract:", style = "color: #495057; font-weight: 600; font-size: 1.05em;"),
          # br(), br(),
          div(
            style = "color: #495057; line-height: 1.5; text-align: justify;",
            HTML(gemini_to_html(
              normalize_uppercase_text(values$M$AB[i]),
              font_size = "16px"
            ))
          )
        )
      },

      # AI Summary
      div(
        style = "margin-bottom: 20px; padding: 15px; background-color: #e7f3ff; border: 1px solid #b3d9ff; border-radius: 6px; border-left: 4px solid #007bff;",
        div(
          style = "display: flex; align-items: center; margin-bottom: 10px;",
          icon(
            "robot",
            class = "fa fa-robot",
            style = "color: #007bff; margin-right: 8px; font-size: 1.2em;"
          ),
          strong(
            "Biblio AI-Generated Summary:",
            style = "color: #007bff; font-weight: 600; font-size: 1.05em;"
          )
        ),
        div(
          style = "color: #495057; line-height: 1.5; text-align: justify;",
          if (
            is.null(res) || res == "" || grepl("Error", res, ignore.case = TRUE)
          ) {
            span(
              style = "color: #dc3545; font-style: italic;",
              "Summary not available at the moment. Please try again later."
            )
          } else {
            HTML(gemini_to_html(res, font_size = "16px"))
          }
        )
      ),

      # Footer with timestamp
      div(
        style = "margin-top: 15px; padding-top: 10px; border-top: 1px solid #dee2e6; text-align: right; font-size: 0.85em; color: #6c757d;",
        icon("clock", class = "fa fa-clock", style = "margin-right: 5px;"),
        "Generated on: ",
        format(Sys.time(), "%m/%d/%Y at %H:%M")
      )
    )
  })

  ### Most Global Cited Documents ----

  observeEvent(input$applyMGCDocuments, {
    M_data <- values$M
    k <- input$MostCitDocsK
    measure <- input$CitDocsMeasure

    showNotification(
      "Most Cited Documents: computing...",
      id = "MGCD_progress",
      type = "message",
      duration = NULL
    )

    p <- promises::future_promise(
      {
        y <- as.numeric(substr(Sys.Date(), 1, 4))
        TAB <- M_data %>%
          dplyr::mutate(TCperYear = round(TC / (y + 1 - PY), 1)) %>%
          dplyr::select(SR, DI, TC, TCperYear, PY) %>%
          dplyr::group_by(PY) %>%
          dplyr::mutate(NTC = TC / mean(TC)) %>%
          dplyr::ungroup() %>%
          dplyr::select(-PY) %>%
          dplyr::arrange(dplyr::desc(TC)) %>%
          as.data.frame()
        names(TAB) <- c(
          "Paper",
          "DOI",
          "Total Citations",
          "TC per Year",
          "Normalized TC"
        )
        TAB
      },
      seed = TRUE,
      packages = c("dplyr")
    ) %...>%
      (function(TAB) {
        removeNotification("MGCD_progress")
        values$TAB <- TAB
        values$TABGlobDoc <- TAB
        if (measure == "TC") {
          xx <- TAB %>% dplyr::select(1, 3)
          lab <- "Global Citations"
        } else {
          xx <- TAB %>% dplyr::select(1, 4)
          xx[, 2] <- round(xx[, 2], 1)
          lab <- "Global Citations per Year"
        }
        if (k > nrow(xx)) {
          k <- nrow(xx)
        }
        xx <- xx[1:k, ]
        g <- freqPlot(
          xx,
          x = 2,
          y = 1,
          textLaby = "Documents",
          textLabx = lab,
          title = "Most Global Cited Documents",
          values
        )
        values$MGCDplot <- g
        values$MGCDocs_ready <- values$MGCDocs_ready + 1
      }) %...!%
      (function(err) {
        removeNotification("MGCD_progress")
        showNotification(
          paste("Most Cited Documents error:", conditionMessage(err)),
          type = "error",
          duration = 8
        )
      })
    return(p)
  })

  output$MGCDplot.save <- downloadHandler(
    filename = function() {
      paste("MostGlobalCitedDocuments-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$MGCDplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$MostCitDocsPlot <- renderPlotly({
    req(values$MGCDocs_ready > 0)
    plot.ly(
      values$MGCDplot,
      flip = FALSE,
      side = "r",
      aspectratio = 1,
      size = 0.10
    )
  })

  output$MostCitDocsTable <- renderUI({
    req(values$MGCDocs_ready > 0)
    TAB <- values$TABGlobDoc
    TAB$DOI <- paste0(
      '<a href=\"https://doi.org/',
      TAB$DOI,
      '\" target=\"_blank\">',
      TAB$DOI,
      '</a>'
    )
    renderBibliobox(
      TAB,
      nrow = 10,
      filename = "Most_Global_Cited_Documents",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 5:6,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE,
      summary = "documents"
    )
  })

  observeEvent(input$reportMCD, {
    if (!is.null(values$TABGlobDoc)) {
      list_df <- list(values$TABGlobDoc)
      list_plot <- list(values$MGCDplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "MostGlobCitDocs",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Most Global Cited Documents", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Most Local Cited Documents ----
  MLCDocuments <- eventReactive(input$applyMLCDocuments, {
    withProgress(message = 'Calculation in progress', value = 0, {
      TAB <- localCitations(
        values$M,
        fast.search = FALSE,
        sep = input$LocCitSep
      )$Paper
      TAB <- TAB %>%
        group_by(Year) %>%
        mutate(
          Ratio = LCS / GCS * 100,
          NLCS = LCS / mean(LCS),
          NGCS = GCS / mean(GCS)
        ) %>%
        ungroup() %>%
        as.data.frame()
    })

    xx = data.frame(
      Document = as.character(TAB[, 1]),
      DOI = as.character(TAB[, 2]),
      Year = TAB[, 3],
      "Local Citations" = TAB[, 4],
      "Global Citations" = TAB[, 5],
      "LC/GC Ratio" = TAB[6],
      "Normalized Local Citations" = TAB[, 7],
      "Normalized Global Citations" = TAB[, 8]
    )
    values$TABLocDoc = xx

    if (input$MostLocCitDocsK > dim(xx)[1]) {
      k = dim(xx)[1]
    } else {
      k = input$MostLocCitDocsK
    }

    xx = xx[1:k, ]

    g <- freqPlot(
      xx,
      x = 4,
      y = 1,
      textLaby = "Documents",
      textLabx = "Local Citations",
      title = "Most Local Cited Documents",
      values
    )

    values$MLCDplot <- g
    return(g)
  })

  # gemini button for word network
  output$MostLocCitDocsGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$MostLocCitDocsGemini, values)
  })

  output$MLCDplot.save <- downloadHandler(
    filename = function() {
      paste("MostLocalCitedDocuments-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$MLCDplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$MostLocCitDocsPlot <- renderPlotly({
    g <- MLCDocuments()
    plot.ly(g, flip = FALSE, side = "r", aspectratio = 1, size = 0.10)
  })

  output$MostLocCitDocsTable <- renderUI({
    TAB <- values$TABLocDoc
    TAB$DOI <- paste0(
      '<a href=\"https://doi.org/',
      TAB$DOI,
      '\" target=\"_blank\">',
      TAB$DOI,
      '</a>'
    )

    names(TAB)[c(1, 4:8)] <- c(
      "Paper",
      "Local Citations",
      "Global Citations",
      "LC/GC Ratio (%)",
      "Normalized Local Citations",
      "Normalized Global Citations"
    )
    renderBibliobox(
      TAB,
      nrow = 10,
      filename = "Most_Local_Cited_Documents",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 7:9,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE,
      summary = "documents"
    )
  })

  observeEvent(input$reportMLCD, {
    if (!is.null(values$TABLocDoc)) {
      list_df <- list(values$TABLocDoc)
      list_plot <- list(values$MLCDplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "MostLocCitDocs",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Most Local Cited Documents", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ## Cited References ----
  ### Most Local Cited References ----
  MLCReferences <- eventReactive(input$applyMLCReferences, {
    CR <- citations(values$M, sep = input$CitRefsSep)$Cited
    TAB <- data.frame(names(CR), as.numeric(CR))
    names(TAB) <- c("Cited References", "Citations")
    values$TABCitRef <- TAB %>%
      dplyr::filter(`Cited References` != "ANONYMOUS, NO TITLE CAPTURED")

    xx = values$TABCitRef
    if (input$MostCitRefsK > dim(xx)[1]) {
      k = dim(xx)[1]
    } else {
      k = input$MostCitRefsK
    }

    xx = xx[1:k, ]

    g <- freqPlot(
      xx,
      x = 2,
      y = 1,
      textLaby = "References",
      textLabx = "Local Citations",
      title = "Most Local Cited References",
      values,
      string.max = 70
    )

    values$MLCRplot <- g
    return(g)
  })

  output$MLCRplot.save <- downloadHandler(
    filename = function() {
      paste("MostLocalCitedReferences-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$MLCRplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$MostCitRefsPlot <- renderPlotly({
    g <- MLCReferences()
    plot.ly(g, flip = FALSE, side = "r", aspectratio = 0.6, size = 0.20)
  })

  output$MostCitRefsTable <- renderUI({
    g <- MLCReferences()
    TAB <- values$TABCitRef

    TAB$link <- trimES(gsub("[[:punct:]]", " ", reduceRefs(TAB[, 1])))
    TAB$link <- paste0(
      '<a href=\"https://scholar.google.it/scholar?hl=en&as_sdt=0%2C5&q=',
      TAB$link,
      '\" target=\"_blank\">',
      'link',
      '</a>'
    )

    TAB = TAB[, c(3, 1, 2)]
    names(TAB)[1] = "Google Scholar"
    renderBibliobox(
      TAB,
      nrow = 10,
      filename = "Most_Local_Cited_References",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportMLCR, {
    if (!is.null(values$TABCitRef)) {
      list_df <- list(values$TABCitRef)
      list_plot <- list(values$MLCRplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "MostLocCitRefs",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Most Local Cited References", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Reference Spectroscopy ----
  observeEvent(input$applyRPYS, {
    timespan <- c(
      max(values$M$PY, na.rm = TRUE) - 100,
      max(values$M$PY, na.rm = TRUE) - 3
    )
    if (!is.na(input$rpysMinYear)) {
      timespan[1] <- input$rpysMinYear
    }
    if (!is.na(input$rpysMaxYear)) {
      timespan[2] <- input$rpysMaxYear
    }
    timespan <- sort(timespan)

    values$RPYS_computing <- TRUE
    values$RPYS_error <- NULL
    showNotification(
      "Reference Spectroscopy: computing...",
      id = "RPYS_progress",
      type = "message",
      duration = NULL
    )

    # Snapshot variables for the future
    M_data <- values$M
    sep <- input$rpysSep
    median.window <- input$rpysMedianWindow

    p <- promises::future_promise(
      {
        bibliometrix::rpys(
          M_data,
          sep = sep,
          timespan = timespan,
          median.window = median.window,
          graph = FALSE
        )
      },
      seed = TRUE,
      packages = c("bibliometrix", "dplyr", "tidyr", "Matrix", "ggplot2")
    ) %...>%
      (function(res) {
        removeNotification("RPYS_progress")
        res$peaks <- rpysPeaks(res, n = 10)
        values$res <- res
        values$RPYS_computing <- FALSE
        values$RPYS_ready <- values$RPYS_ready + 1
      }) %...!%
      (function(err) {
        removeNotification("RPYS_progress")
        values$RPYS_computing <- FALSE
        values$RPYS_error <- conditionMessage(err)
        showNotification(
          paste("RPYS error:", values$RPYS_error),
          type = "error",
          duration = 8
        )
      })

    return(p)
  })

  output$RSplot.save <- downloadHandler(
    filename = function() {
      paste("ReferenceSpectroscopy-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$res$spectroscopy,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  # gemini button for rpys
  output$rpysGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$rpysGemini, values)
  })

  output$rpysPlot <- renderPlotly({
    req(values$RPYS_ready > 0)
    plot.ly(values$res$spectroscopy, side = "l", aspectratio = 1.3, size = 0.10)
  })

  output$rpysTable <- renderUI({
    req(values$RPYS_ready > 0)
    rpysData = values$res$rpysTable
    renderBibliobox(
      rpysData,
      nrow = 10,
      filename = "RPYS",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$crTable <- renderUI({
    req(values$RPYS_ready > 0)
    crData = values$res$CR
    crData$link <- paste0(
      '<a href=\"https://scholar.google.it/scholar?hl=en&as_sdt=0%2C5&q=',
      crData$Reference,
      '\" target=\"_blank\">',
      'link',
      '</a>'
    )

    crData = crData[order(-as.numeric(crData$Year), -crData$Freq), ]
    names(crData) = c("Year", "Reference", "Local Citations", "Google link")
    crData <- crData[, c(1, 4, 2, 3)]
    renderBibliobox(
      crData,
      nrow = 10,
      filename = "RPYS_Documents",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$rpysSequence <- renderUI({
    req(values$RPYS_ready > 0)
    paperClass <- ifelse(
      input$rpysInfluential == "Not Influent",
      "",
      input$rpysInfluential
    )
    crData <- values$res$Sequences %>%
      dplyr::filter(regexpr(paperClass, Class) > -1)
    crData$link <- paste0(
      '<a href=\"https://scholar.google.it/scholar?hl=en&as_sdt=0%2C5&q=',
      crData$CR,
      '\" target=\"_blank\">',
      'link',
      '</a>'
    )
    crData <- crData %>% select(RPY, CR, Freq, link, sequence, Class)
    names(crData) = c(
      "Year",
      "Reference",
      "Local Citations",
      "Google link",
      "Citation Sequence",
      "Sequence Type"
    )
    renderBibliobox(
      crData,
      nrow = 10,
      filename = "RPYS_InfluentialReferences",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$rpysPeaks <- renderUI({
    req(values$RPYS_ready > 0)
    crData <- values$res$peaks
    names(crData) = c("Year", "Reference", "Local Citations")
    renderBibliobox(
      crData,
      nrow = 10,
      filename = "RPYS_Top10Peaks",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportRPYS, {
    if (!is.null(values$res$CR)) {
      list_df <- list(
        values$res$CR,
        values$res$rpysTable,
        values$res$Sequences,
        values$res$peaks
      )
      list_plot <- list(values$res$spectroscopy)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "RPYS",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Reference Spectroscopy", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ## Words ----
  ### Most Frequent Words ----

  observeEvent(input$MostRelWordsStop, {
    values$MRWremove.terms <- data.frame(
      stopword = trimws(readStopwordsFile(
        file = input$MostRelWordsStop,
        sep = input$MostRelWordsSep
      ))
    )
    values$GenericSL <- values$MRWremove.terms
    popUpGeneric(
      title = "Stopword list",
      type = NULL,
      color = c("#1d8fe1"),
      subtitle = uiOutput("stopwordList"),
      btn_labels = "OK"
    )
  })

  observeEvent(input$MRWSyn, {
    synonyms <- trimws(readSynWordsFile(
      file = input$MRWSyn,
      sep = input$MRWSynSep
    ))
    term <- unlist(lapply(strsplit(synonyms, ";"), function(l) {
      l[1]
    }))
    synList <- unlist(lapply(strsplit(synonyms, ";"), function(l) {
      paste0(trimws(l[-1]), collapse = ";")
    }))
    values$MRWsyn.terms <- data.frame(term = term, synonyms = synList)
    values$GenericSYN <- values$MRWsyn.terms
    popUpGeneric(
      title = "Synonym List",
      type = NULL,
      color = c("#1d8fe1"),
      subtitle = uiOutput("synonymList"),
      btn_labels = "OK"
    )
  })

  output$stopwordList <- renderUI({
    renderBibliobox(
      values$GenericSL,
      nrow = Inf,
      filename = "Stopword_List",
      pagelength = FALSE,
      left = 1,
      right = NULL,
      numeric = NULL,
      dom = "none",
      size = '90%',
      filter = "none",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = FALSE,
      escape = FALSE,
      selection = FALSE,
      scrollY = TRUE
    )
  })

  output$synonymList <- renderUI({
    renderBibliobox(
      values$GenericSYN,
      nrow = Inf,
      filename = "Stopword_List",
      pagelength = FALSE,
      left = 1,
      right = NULL,
      numeric = NULL,
      dom = "none",
      size = '90%',
      filter = "none",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = FALSE,
      escape = FALSE,
      selection = FALSE,
      scrollY = TRUE
    )
  })

  MFWords <- eventReactive(input$applyMFWords, {
    if (input$MostRelWords %in% c("TI", "AB")) {
      ngrams <- as.numeric(input$MRWngrams)
    } else {
      ngrams <- 1
    }

    ### load file with terms to remove
    if (input$MostRelWordsStopFile == "Y") {
      remove.terms <- trimws(values$MRWremove.terms$stopword)
    } else {
      remove.terms <- NULL
    }
    #values$MRWremove.terms <- remove.terms
    ### end of block

    ### load file with synonyms
    if (input$MRWSynFile == "Y") {
      synonyms <- values$MRWsyn.terms %>%
        group_by(term) %>%
        mutate(term = paste0(term, ";", synonyms)) %>%
        select(term)
      synonyms <- synonyms$term
    } else {
      synonyms <- NULL
    }
    #values$MRWsyn.terms <- synonyms
    ### end of block

    WR <- wordlist(
      values$M,
      Field = input$MostRelWords,
      n = Inf,
      measure = "identity",
      ngrams = ngrams,
      remove.terms = remove.terms,
      synonyms = synonyms
    )$v

    TAB <- data.frame(names(WR), as.numeric(WR))
    names(TAB) <- c("Words", "Occurrences")
    values$TABWord <- TAB

    xx = values$TABWord
    if (input$MostRelWordsN > dim(xx)[1]) {
      k = dim(xx)[1]
    } else {
      k = input$MostRelWordsN
    }

    xx = xx[1:k, ]
    switch(
      input$MostRelWords,
      ID = {
        lab = "Keywords Plus"
      },
      DE = {
        lab = "Author's Keywords"
      },
      KW_Merged = {
        lab = "All Keywords"
      },
      TI = {
        lab = "Title's Words"
      },
      AB = {
        lab = "Abstract's Words"
      },
      WC = {
        lab = "Subject Categories"
      }
    )

    g <- freqPlot(
      xx,
      x = 2,
      y = 1,
      textLaby = lab,
      textLabx = "Occurrences",
      title = "Most Relevant Words",
      values
    )

    values$MRWplot <- g
    return(g)
  })

  output$MRWplot.save <- downloadHandler(
    filename = function() {
      paste("MostRelevantWords-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$MRWplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$MostRelWordsPlot <- renderPlotly({
    g <- MFWords()
    plot.ly(g, side = "r", aspectratio = 1.3, size = 0.10)
  })

  output$MostRelWordsTable <- renderUI({
    g <- MFWords()

    TAB <- values$TABWord
    renderBibliobox(
      TAB,
      nrow = 10,
      filename = "Most_Frequent_Words",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportMFW, {
    if (!is.null(values$TABWord)) {
      list_df <- list(values$TABWord)
      list_plot <- list(values$MRWplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "MostFreqWords",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Most Frequent Words", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### WordCloud ----
  observeEvent(input$WCStop, {
    values$WCremove.terms <- data.frame(
      stopword = trimws(readStopwordsFile(
        file = input$WCStop,
        sep = input$WCSep
      ))
    )
    values$GenericSL <- values$WCremove.terms
    popUpGeneric(
      title = "Stopword list",
      type = NULL,
      color = c("#1d8fe1"),
      subtitle = uiOutput("stopwordList"),
      btn_labels = "OK"
    )
  })

  observeEvent(input$WCSyn, {
    synonyms <- trimws(readSynWordsFile(
      file = input$WCSyn,
      sep = input$WCSynSep
    ))
    term <- unlist(lapply(strsplit(synonyms, ";"), function(l) {
      l[1]
    }))
    synList <- unlist(lapply(strsplit(synonyms, ";"), function(l) {
      paste0(trimws(l[-1]), collapse = ";")
    }))
    values$WCsyn.terms <- data.frame(term = term, synonyms = synList)
    values$GenericSYN <- values$WCsyn.terms
    popUpGeneric(
      title = "Synonym List",
      type = NULL,
      color = c("#1d8fe1"),
      subtitle = uiOutput("synonymList"),
      btn_labels = "OK"
    )
  })

  WordCloud <- eventReactive(input$applyWordCloud, {
    if (input$summaryTerms %in% c("TI", "AB")) {
      ngrams <- as.numeric(input$summaryTermsngrams)
    } else {
      ngrams <- 1
    }

    ### load file with terms to remove
    if (input$WCStopFile == "Y") {
      remove.terms <- trimws(values$WCremove.terms$stopword)
    } else {
      remove.terms <- NULL
    }
    #values$WCremove.terms <- remove.terms
    ### end of block

    ### load file with synonyms
    if (input$WCSynFile == "Y") {
      synonyms <- values$WCsyn.terms %>%
        group_by(term) %>%
        mutate(term = paste0(term, ";", synonyms)) %>%
        select(term)
      synonyms <- synonyms$term
      print(synonyms)
    } else {
      synonyms <- NULL
    }
    #values$WCsyn.terms <- synonyms
    ### end of block

    resW = wordlist(
      M = values$M,
      Field = input$summaryTerms,
      n = input$n_words,
      measure = input$measure,
      ngrams = ngrams,
      remove.terms = remove.terms,
      synonyms = synonyms
    )

    W = resW$W
    values$Words <- resW$Words

    values$WordCloud <- wordcloud2::wordcloud2(
      W,
      size = input$scale,
      minSize = 0,
      gridSize = input$padding,
      fontFamily = input$font,
      fontWeight = 'normal',
      color = input$wcCol,
      backgroundColor = "white", #input$wcBGCol,
      minRotation = 0,
      maxRotation = input$rotate / 10,
      shuffle = TRUE,
      rotateRatio = 0.7,
      shape = input$wcShape,
      ellipticity = input$ellipticity,
      widgetsize = NULL,
      figPath = NULL,
      hoverFunction = NULL
    )
  })

  output$wordcloud <- wordcloud2::renderWordcloud2({
    WordCloud()
    values$WordCloud
  })

  observeEvent(input$reportWC, {
    if (!is.null(values$Words)) {
      sheetname <- "WordCloud"
      list_df <- list(values$Words)
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      values$wb <- res$wb
      #values$fileTFP <- screenSh(selector = "#wordcloud") ## screenshot
      values$fileWC <- screenSh(values$WordCloud, zoom = 2, type = "plotly")
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$fileWC, res$col)
      )
      popUp(title = "WordCloud", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### TreeMap ----
  observeEvent(input$TreeMapStop, {
    values$TreeMapremove.terms <- data.frame(
      stopword = trimws(readStopwordsFile(
        file = input$TreeMapStop,
        sep = input$TreeMapSep
      ))
    )
    values$GenericSL <- values$TreeMapremove.terms
    popUpGeneric(
      title = "Stopword list",
      type = NULL,
      color = c("#1d8fe1"),
      subtitle = uiOutput("stopwordList"),
      btn_labels = "OK"
    )
  })

  observeEvent(input$TreeMapSyn, {
    synonyms <- trimws(readSynWordsFile(
      file = input$TreeMapSyn,
      sep = input$TreeMapSynSep
    ))
    term <- unlist(lapply(strsplit(synonyms, ";"), function(l) {
      l[1]
    }))
    synList <- unlist(lapply(strsplit(synonyms, ";"), function(l) {
      paste0(trimws(l[-1]), collapse = ";")
    }))
    values$TreeMapsyn.terms <- data.frame(term = term, synonyms = synList)
    values$GenericSYN <- values$TreeMapsyn.terms
    popUpGeneric(
      title = "Synonym List",
      type = NULL,
      color = c("#1d8fe1"),
      subtitle = uiOutput("synonymList"),
      btn_labels = "OK"
    )
  })

  TreeMap <- eventReactive(input$applyTreeMap, {
    if (input$treeTerms %in% c("TI", "AB")) {
      ngrams <- as.numeric(input$treeTermsngrams)
    } else {
      ngrams <- 1
    }
    ### load file with terms to remove
    if (input$TreeMapStopFile == "Y") {
      remove.terms <- trimws(values$TreeMapremove.terms$stopword)
    } else {
      remove.terms <- NULL
    }
    #values$TreeMapremove.terms <- remove.terms
    ### end of block
    ### load file with synonyms
    if (input$TreeMapSynFile == "Y") {
      synonyms <- values$TreeMapsyn.terms %>%
        group_by(term) %>%
        mutate(term = paste0(term, ";", synonyms)) %>%
        select(term)
      synonyms <- synonyms$term
    } else {
      synonyms <- NULL
    }
    #values$TreeMapsyn.terms <- synonyms
    ### end of block

    resW = wordlist(
      M = values$M,
      Field = input$treeTerms,
      n = input$treen_words,
      measure = "identity",
      ngrams = ngrams,
      remove.terms = remove.terms,
      synonyms = synonyms
    )

    W = resW$W
    values$TreeMap <- plot_ly(
      type = 'treemap',
      labels = W[, 1],
      parents = "Tree",
      values = W[, 2],
      textinfo = "label+value+percent entry",
      domain = list(column = 0)
    ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          'toImage',
          'sendDataToCloud',
          'pan2d',
          'select2d',
          'lasso2d',
          'toggleSpikelines',
          'hoverClosestCartesian',
          'hoverCompareCartesian'
        )
      )

    values$WordsT = resW$Words
    return(resW$Words)
  })

  output$treemap <- renderPlotly({
    TreeMap()
    values$TreeMap
  })

  output$wordTable <- renderUI({
    WordCloud()
    renderBibliobox(
      values$Words,
      nrow = 10,
      filename = "Most_Frequent_Words",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$treeTable <- renderUI(
    {
      WordsT <- TreeMap()
      renderBibliobox(
        values$WordsT,
        nrow = 10,
        filename = "Most_Frequent_Words",
        pagelength = TRUE,
        left = NULL,
        right = NULL,
        numeric = NULL,
        dom = FALSE,
        size = '100%',
        filter = "top",
        columnShort = NULL,
        columnSmall = NULL,
        round = 2,
        title = "",
        button = TRUE,
        escape = FALSE,
        selection = FALSE
      )
    } #,
    #height = 600,
    #width = 900
  )

  observeEvent(input$reportTREEMAP, {
    if (!is.null(values$WordsT)) {
      sheetname <- "TreeMap"
      list_df <- list(values$WordsT)
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      values$wb <- res$wb
      #values$fileTFP <- screenSh(selector = "#treemap") ## screenshot
      values$fileTreeMap <- screenSh(values$TreeMap, zoom = 2, type = "plotly")
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$fileTreeMap, res$col)
      )
      popUp(title = "TreeMap", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Words' Frequency over Time ----
  observeEvent(input$WDStop, {
    values$WDremove.terms <- data.frame(
      stopword = trimws(readStopwordsFile(
        file = input$WDStop,
        sep = input$WDSep
      ))
    )
    values$GenericSL <- values$WDremove.terms
    popUpGeneric(
      title = "Stopword list",
      type = NULL,
      color = c("#1d8fe1"),
      subtitle = uiOutput("stopwordList"),
      btn_labels = "OK"
    )
  })

  observeEvent(input$WDSyn, {
    synonyms <- trimws(readSynWordsFile(
      file = input$WDSyn,
      sep = input$WDSynSep
    ))
    term <- unlist(lapply(strsplit(synonyms, ";"), function(l) {
      l[1]
    }))
    synList <- unlist(lapply(strsplit(synonyms, ";"), function(l) {
      paste0(trimws(l[-1]), collapse = ";")
    }))
    values$WDsyn.terms <- data.frame(term = term, synonyms = synList)
    values$GenericSYN <- values$WDsyn.terms
    popUpGeneric(
      title = "Synonym List",
      type = NULL,
      color = c("#1d8fe1"),
      subtitle = uiOutput("synonymList"),
      btn_labels = "OK"
    )
  })

  WDynamics <- eventReactive(input$applyWD, {
    if (input$cumTerms == "Cum") {
      cdf = TRUE
      laby = "Cumulate occurrences"
    } else {
      cdf = FALSE
      laby = "Annual occurrences"
    }

    ### load file with terms to remove
    if (input$WDStopFile == "Y") {
      remove.terms <- trimws(values$WDremove.terms$stopword)
    } else {
      remove.terms <- NULL
    }
    #values$WDremove.terms <- remove.terms
    ### end of block

    ### load file with synonyms
    if (input$WDSynFile == "Y") {
      synonyms <- values$WDsyn.terms %>%
        group_by(term) %>%
        mutate(term = paste0(term, ";", synonyms)) %>%
        select(term)
      synonyms <- synonyms$term
    } else {
      synonyms <- NULL
    }
    #values$WDsyn.terms <- synonyms
    ### end of block

    switch(
      input$growthTerms,
      ID = {
        KW = KeywordGrowth(
          values$M,
          Tag = "ID",
          sep = ";",
          top = input$topkw[2],
          cdf = cdf,
          remove.terms = remove.terms,
          synonyms = synonyms
        )
      },
      DE = {
        KW = KeywordGrowth(
          values$M,
          Tag = "DE",
          sep = ";",
          top = input$topkw[2],
          cdf = cdf,
          remove.terms = remove.terms,
          synonyms = synonyms
        )
      },
      KW_Merged = {
        KW = KeywordGrowth(
          values$M,
          Tag = "KW_Merged",
          sep = ";",
          top = input$topkw[2],
          cdf = cdf,
          remove.terms = remove.terms,
          synonyms = synonyms
        )
      },
      TI = {
        values$M = termExtraction(
          values$M,
          Field = "TI",
          verbose = FALSE,
          ngrams = as.numeric(input$growthTermsngrams),
          remove.terms = remove.terms,
          synonyms = synonyms
        )
        KW = KeywordGrowth(
          values$M,
          Tag = "TI_TM",
          sep = ";",
          top = input$topkw[2],
          cdf = cdf
        )
      },
      AB = {
        values$M = termExtraction(
          values$M,
          Field = "AB",
          verbose = FALSE,
          ngrams = as.numeric(input$growthTermsngrams),
          remove.terms = remove.terms,
          synonyms = synonyms
        )
        KW = KeywordGrowth(
          values$M,
          Tag = "AB_TM",
          sep = ";",
          top = input$topkw[2],
          cdf = cdf
        )
      }
    )

    values$KW = KW[, c(1, seq(input$topkw[1], input$topkw[2]) + 1)]

    term = names(values$KW)[-1]
    term = rep(term, each = dim(values$KW)[1])
    n = dim(values$KW)[1] * (dim(values$KW)[2] - 1)
    freq = matrix(as.matrix(values$KW[, -1]), n, 1)
    values$DF = data.frame(
      Year = rep(values$KW$Year, (dim(values$KW)[2] - 1)),
      Term = term,
      Freq = freq
    )

    width_scale <- 2.5 * 26 / length(unique(values$DF$Term))

    Text <- paste(
      values$DF$Term,
      " (",
      values$DF$Year,
      ") ",
      values$DF$Freq,
      sep = ""
    )

    x <- c(
      max(values$DF$Year) - 0.02 - diff(range(values$DF$Year)) * 0.20,
      max(values$DF$Year) - 0.02
    ) -
      1
    y <- c(
      min(values$DF$Freq),
      min(values$DF$Freq) + diff(range(values$DF$Freq)) * 0.20
    )

    g <- ggplot(
      values$DF,
      aes(x = Year, y = Freq, group = Term, color = Term, text = Text)
    ) +
      geom_line() +
      labs(x = 'Year', y = laby, title = "Words' Frequency over Time") +
      scale_x_continuous(
        breaks = (values$KW$Year[seq(
          1,
          length(values$KW$Year),
          by = ceiling(length(values$KW$Year) / 20)
        )])
      ) +
      geom_hline(aes(yintercept = 0), alpha = 0.1) +
      labs(color = "Term") +
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
        panel.background = element_rect(fill = '#FFFFFF'),
        panel.grid.minor = element_line(color = '#EFEFEF'),
        panel.grid.major = element_line(color = '#EFEFEF'),
        plot.title = element_text(size = 24),
        axis.title = element_text(size = 14, color = '#555555'),
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

    values$WDplot <- g
    return(g)
  })

  output$WDplot.save <- downloadHandler(
    filename = function() {
      paste("WordsFrequencyOverTime-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$WDplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$kwGrowthPlot <- renderPlotly({
    g <- WDynamics()

    leg <- list(
      orientation = 'h',
      y = -0.15,
      font = list(
        family = "sans-serif",
        size = 10,
        color = "#000"
      ),
      bgcolor = "#FFFFFF",
      bordercolor = "#FFFFFF",
      borderwidth = 2
    )

    plot.ly(g, flip = FALSE, side = "r", aspectratio = 1.6, size = 0.10) %>%
      layout(legend = leg) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          'toImage',
          'sendDataToCloud',
          'pan2d',
          'select2d',
          'lasso2d',
          'toggleSpikelines',
          'hoverClosestCartesian',
          'hoverCompareCartesian'
        )
      ) %>%
      layout(hovermode = 'compare')
  })

  output$kwGrowthtable <- renderUI({
    g <- WDynamics()
    kwData <- values$KW
    renderBibliobox(
      kwData,
      nrow = 10,
      filename = "Word_Dynamics",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportWD, {
    if (!is.null(values$KW)) {
      list_df <- list(values$KW)
      list_plot <- list(values$WDplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "WordFreqOverTime",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Words' Frequency over Time", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Trend Topics ----

  observeEvent(input$TTStop, {
    values$TTremove.terms <- data.frame(
      stopword = trimws(readStopwordsFile(
        file = input$TTStop,
        sep = input$TTSep
      ))
    )
    values$GenericSL <- values$TTremove.terms
    popUpGeneric(
      title = "Stopword list",
      type = NULL,
      color = c("#1d8fe1"),
      subtitle = uiOutput("stopwordList"),
      btn_labels = "OK"
    )
  })

  observeEvent(input$TTSyn, {
    synonyms <- trimws(readSynWordsFile(
      file = input$TTSyn,
      sep = input$TTSynSep
    ))
    term <- unlist(lapply(strsplit(synonyms, ";"), function(l) {
      l[1]
    }))
    synList <- unlist(lapply(strsplit(synonyms, ";"), function(l) {
      paste0(trimws(l[-1]), collapse = ";")
    }))
    values$TTsyn.terms <- data.frame(term = term, synonyms = synList)
    values$GenericSYN <- values$TTsyn.terms
    popUpGeneric(
      title = "Synonym List",
      type = NULL,
      color = c("#1d8fe1"),
      subtitle = uiOutput("synonymList"),
      btn_labels = "OK"
    )
  })

  output$trendSliderPY <- renderUI({
    sliderInput(
      "trendSliderPY",
      "Timespan",
      min = min(values$M$PY, na.rm = T),
      sep = "",
      max = max(values$M$PY, na.rm = T),
      value = c(min(values$M$PY, na.rm = T), max(values$M$PY, na.rm = T))
    )
  })

  TrendTopics <- eventReactive(input$applyTrendTopics, {
    ### load file with terms to remove
    if (input$TTStopFile == "Y") {
      remove.terms <- trimws(values$TTremove.terms$stopword)
    } else {
      remove.terms <- NULL
    }
    #values$TTremove.terms <- remove.terms
    ### end of block

    ### load file with synonyms
    if (input$TTSynFile == "Y") {
      synonyms <- values$TTsyn.terms %>%
        group_by(term) %>%
        mutate(term = paste0(term, ";", synonyms)) %>%
        select(term)
      synonyms <- synonyms$term
    } else {
      synonyms <- NULL
    }
    #values$TTsyn.terms <- synonyms
    ### end of block

    if (input$trendTerms %in% c("TI", "AB")) {
      values$M = termExtraction(
        values$M,
        Field = input$trendTerms,
        stemming = input$trendStemming,
        verbose = FALSE,
        ngrams = as.numeric(input$trendTermsngrams)
      )
      field = paste(input$trendTerms, "_TM", sep = "")
    } else {
      field = input$trendTerms
    }
    values$trendTopics <- fieldByYear(
      values$M,
      field = field,
      timespan = input$trendSliderPY,
      min.freq = input$trendMinFreq,
      n.items = input$trendNItems,
      remove.terms = remove.terms,
      synonyms = synonyms,
      dynamic.plot = TRUE,
      graph = FALSE
    )
    values$trendTopics$params <- data.frame(
      description = c("Textual field", "N. of words per Year"),
      value = c(field, input$trendNItems)
    )
    return(values$trendTopics$graph)
  })

  # gemini button for word network
  output$trendTopicsGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$trendTopicsGemini, values)
  })

  output$TTplot.save <- downloadHandler(
    filename = function() {
      paste("TrendTopics-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$trendTopics$graph,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$trendTopicsPlot <- renderPlotly({
    g <- TrendTopics()
    plot.ly(g, flip = TRUE, side = "r", size = 0.1, aspectratio = 1.3)
  })

  output$trendTopicsTable <- renderUI({
    TrendTopics()
    tpData = values$trendTopics$df_graph %>%
      rename(
        Term = item,
        Frequency = freq,
        "Year (Q1)" = year_q1,
        "Year (Median)" = year_med,
        "Year (Q3)" = year_q3
      )
    renderBibliobox(
      tpData,
      nrow = 10,
      filename = "TrendTopic",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportTT, {
    if (!is.null(values$trendTopics$df_graph)) {
      list_df <- list(
        values$trendTopics$df_graph %>%
          rename(
            Term = item,
            Frequency = freq,
            "Year (Q1)" = year_q1,
            "Year (Median)" = year_med,
            "Year (Q3)" = year_q3
          )
      )
      list_plot <- list(values$trendTopics$graph)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "TrendTopics",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Trend Topics", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  # CLUSTERING ----
  ### Clustering by Coupling ----
  observeEvent(input$applyCM, {
    M_data <- values$M
    cm_analysis <- input$CManalysis
    cm_field <- input$CMfield
    cm_n <- input$CMn
    cm_freq <- input$CMfreq
    cm_ngrams <- as.numeric(input$CMngrams)
    cm_repulsion <- input$CMrepulsion
    cm_impact <- input$CMimpact
    cm_stemming <- input$CMstemming
    cm_size <- input$sizeCM
    cm_labeling <- input$CMlabeling
    cm_nlabels <- input$CMn.labels

    showNotification(
      "Coupling Map: computing...",
      id = "CM_progress",
      type = "message",
      duration = NULL
    )

    p <- promises::future_promise(
      {
        bibliometrix::couplingMap(
          M_data,
          analysis = cm_analysis,
          field = cm_field,
          n = cm_n,
          minfreq = cm_freq,
          ngrams = cm_ngrams,
          community.repulsion = cm_repulsion,
          impact.measure = cm_impact,
          stemming = cm_stemming,
          size = cm_size,
          label.term = cm_labeling,
          n.labels = cm_nlabels,
          repel = FALSE
        )
      },
      seed = TRUE,
      packages = c("bibliometrix", "dplyr", "tidyr", "ggplot2", "igraph")
    ) %...>%
      (function(CM) {
        removeNotification("CM_progress")
        CM$data <- CM$data[, c(1, 5, 2)]
        CM$clusters <- CM$clusters[, c(7, 1:4, 6)]
        values$CM <- CM
        values$CMMAP_ready <- values$CMMAP_ready + 1
      }) %...!%
      (function(err) {
        removeNotification("CM_progress")
        showNotification(
          paste("Coupling Map error:", conditionMessage(err)),
          type = "error",
          duration = 8
        )
      })
    return(p)
  })

  output$CMPlot <- renderPlotly({
    req(values$CMMAP_ready > 0)
    plot.ly(values$CM$map, size = 0.15, aspectratio = 1.3)
  })

  output$CMNetPlot <- renderVisNetwork({
    req(values$CMMAP_ready > 0)
    values$networkCM <- igraph2vis(
      g = values$CM$net$graph,
      curved = (input$coc.curved == "Yes"),
      labelsize = input$labelsize,
      opacity = input$cocAlpha,
      type = input$layout,
      shape = input$coc.shape,
      net = values$CM$net,
      shadow = TRUE
    )
    values$networkCM$VIS
  })

  output$CMplot.save <- downloadHandler(
    filename = function() {
      paste("CouplingMap-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$CM$map,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$CMTable <- renderUI({
    req(values$CMMAP_ready > 0)
    #cmData=values$CM$data[,c(2,1,3,5)]
    cmData <- values$CM$data
    renderBibliobox(
      cmData,
      nrow = 10,
      filename = "CouplingMap",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 2,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$CMTableCluster <- renderUI({
    req(values$CMMAP_ready > 0)
    #cmData=values$CM$clusters[,c(7,1:4,6)]
    cmData <- values$CM$clusters
    renderBibliobox(
      cmData,
      nrow = 10,
      filename = "CouplingMap_Clusters",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 4:5,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportCM, {
    if (!is.null(values$CM$data)) {
      popUp(title = NULL, type = "waiting")
      list_df <- list(values$CM$params, values$CM$data, values$CM$clusters)
      list_plot <- list(values$CM$map, values$CM$net$graph)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "CouplingMap",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Coupling Map", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  # CONCEPTUAL STRUCTURE ----
  ### Network approach ----
  #### Co-occurrences network ----
  observeEvent(input$COCStop, {
    values$COCremove.terms <- data.frame(
      stopword = trimws(readStopwordsFile(
        file = input$COCStop,
        sep = input$COCSep
      ))
    )
    values$GenericSL <- values$COCremove.terms
    popUpGeneric(
      title = "Stopword list",
      type = NULL,
      color = c("#1d8fe1"),
      subtitle = uiOutput("stopwordList"),
      btn_labels = "OK"
    )
  })

  observeEvent(input$COCSyn, {
    synonyms <- trimws(readSynWordsFile(
      file = input$COCSyn,
      sep = input$COCSynSep
    ))
    term <- unlist(lapply(strsplit(synonyms, ";"), function(l) {
      l[1]
    }))
    synList <- unlist(lapply(strsplit(synonyms, ";"), function(l) {
      paste0(trimws(l[-1]), collapse = ";")
    }))
    values$COCsyn.terms <- data.frame(term = term, synonyms = synList)
    values$GenericSYN <- values$COCsyn.terms
    popUpGeneric(
      title = "Synonym List",
      type = NULL,
      color = c("#1d8fe1"),
      subtitle = uiOutput("synonymList"),
      btn_labels = "OK"
    )
  })

  observeEvent(input$applyCoc, {
    # Extract params synchronously
    n <- input$Nodes
    label.n <- input$Labels
    coc_field <- input$field
    coc_ngrams <- input$cocngrams
    coc_layout <- input$layout
    coc_curved <- (input$coc.curved == "Yes")
    coc_labelsize <- input$labelsize
    coc_alpha <- input$cocAlpha
    coc_shape <- input$coc.shape
    coc_shadow <- (input$coc.shadow == "Yes")
    coc_edgesize <- input$edgesize
    coc_noOverlap <- input$noOverlap
    coc_normalize <- if (input$normalize == "none") NULL else input$normalize
    coc_label_cex <- (input$label.cex == "Yes")
    coc_edges_min <- input$edges.min
    coc_isolates <- (input$coc.isolates == "yes")
    coc_cluster <- input$cocCluster
    coc_repulsion <- input$coc.repulsion / 2
    coc_years <- (input$cocyears == "Yes")
    random_seed <- values$random_seed

    remove.terms <- if (input$COCStopFile == "Y") {
      trimws(values$COCremove.terms$stopword)
    } else {
      NULL
    }
    synonyms <- if (input$COCSynFile == "Y") {
      s <- values$COCsyn.terms %>%
        dplyr::group_by(term) %>%
        dplyr::mutate(term = paste0(term, ";", synonyms)) %>%
        dplyr::select(term)
      s$term
    } else {
      NULL
    }

    if (!(coc_field %in% names(values$M))) {
      showNotification(
        "Selected field is not included in your data collection",
        type = "error",
        duration = 5
      )
      return()
    }

    # Synchronous pre-processing: termExtraction modifies M
    M_data <- values$M
    if (coc_field == "TI") {
      M_data <- termExtraction(
        M_data,
        Field = "TI",
        verbose = FALSE,
        ngrams = as.numeric(coc_ngrams),
        remove.terms = remove.terms,
        synonyms = synonyms
      )
      values$M <- M_data
    } else if (coc_field == "AB") {
      M_data <- termExtraction(
        M_data,
        Field = "AB",
        verbose = FALSE,
        ngrams = as.numeric(coc_ngrams),
        remove.terms = remove.terms,
        synonyms = synonyms
      )
      values$M <- M_data
    }

    # Check if network needs rebuild
    need_rebuild <- (dim(values$NetWords)[1] == 1 |
      !(coc_field == values$field) |
      !(coc_ngrams == values$cocngrams) |
      (dim(values$NetWords)[1] != n))

    if (label.n > n) {
      label.n <- n
    }
    values$field <- coc_field
    values$ngrams <- coc_ngrams

    showNotification(
      "Co-occurrence Network: computing...",
      id = "COC_progress",
      type = "message",
      duration = NULL
    )

    # Build NetWords synchronously if needed (fast for cached), then run networkPlot in future
    if (need_rebuild) {
      NetWords <- switch(
        coc_field,
        ID = bibliometrix::biblioNetwork(
          M_data,
          analysis = "co-occurrences",
          network = "keywords",
          n = n,
          sep = ";",
          remove.terms = remove.terms,
          synonyms = synonyms
        ),
        DE = bibliometrix::biblioNetwork(
          M_data,
          analysis = "co-occurrences",
          network = "author_keywords",
          n = n,
          sep = ";",
          remove.terms = remove.terms,
          synonyms = synonyms
        ),
        KW_Merged = bibliometrix::biblioNetwork(
          M_data,
          analysis = "co-occurrences",
          network = "all_keywords",
          n = n,
          sep = ";",
          remove.terms = remove.terms,
          synonyms = synonyms
        ),
        TI = bibliometrix::biblioNetwork(
          M_data,
          analysis = "co-occurrences",
          network = "titles",
          n = n,
          sep = ";"
        ),
        AB = bibliometrix::biblioNetwork(
          M_data,
          analysis = "co-occurrences",
          network = "abstracts",
          n = n,
          sep = ";"
        ),
        WC = {
          WSC <- bibliometrix::cocMatrix(M_data, Field = "WC", binary = FALSE)
          crossprod(WSC, WSC)
        }
      )
      values$NetWords <- NetWords
      values$Title <- switch(
        coc_field,
        ID = "Keywords Plus Network",
        DE = "Authors' Keywords network",
        KW_Merged = "All Keywords network",
        TI = "Title Words network",
        AB = "Abstract Words network",
        WC = "Subject Categories network"
      )
    } else {
      NetWords <- values$NetWords
    }
    net_title <- values$Title

    p <- promises::future_promise(
      {
        bibliometrix::networkPlot(
          NetWords,
          normalize = coc_normalize,
          Title = net_title,
          type = coc_layout,
          size.cex = TRUE,
          size = 5,
          remove.multiple = FALSE,
          edgesize = coc_edgesize * 3,
          labelsize = coc_labelsize,
          label.cex = coc_label_cex,
          label.n = label.n,
          edges.min = coc_edges_min,
          label.color = FALSE,
          curved = coc_curved,
          alpha = coc_alpha,
          cluster = coc_cluster,
          remove.isolates = coc_isolates,
          community.repulsion = coc_repulsion,
          seed = random_seed,
          verbose = FALSE
        )
      },
      seed = TRUE,
      packages = c("bibliometrix", "dplyr", "igraph", "ggplot2", "Matrix")
    ) %...>%
      (function(cocnet) {
        removeNotification("COC_progress")

        # Add year info to graph
        g <- cocnet$graph
        Y <- keywords2Years(M_data, field = coc_field, n = Inf)
        label_df <- data.frame(Keyword = igraph::V(g)$name)
        df <- label_df %>%
          dplyr::left_join(
            Y %>% dplyr::mutate(Keyword = tolower(Keyword)),
            by = "Keyword"
          ) %>%
          dplyr::rename(year_med = Year)
        igraph::V(g)$year_med <- df$year_med
        if (coc_years) {
          col <- hcl.colors(
            (diff(range(df$year_med)) + 1) * 10,
            palette = "Blues 3"
          )
          igraph::V(g)$color <- col[(max(df$year_med) - df$year_med + 1) * 10]
        }
        cocnet$graph <- g
        values$cocnet <- cocnet

        values$COCnetwork <- igraph2vis(
          g = cocnet$graph,
          curved = coc_curved,
          labelsize = coc_labelsize,
          opacity = coc_alpha,
          type = coc_layout,
          shape = coc_shape,
          net = cocnet,
          shadow = coc_shadow,
          edgesize = coc_edgesize,
          noOverlap = coc_noOverlap
        )
        values$cocOverlay <- overlayPlotly(values$COCnetwork$VIS)
        values$degreePlot <- degreePlot(cocnet)
        values$years_coc <- sort(unique(c(
          M_data %>% tidyr::drop_na(PY) %>% dplyr::pull(PY)
        )))
        values$index_coc <- 0
        values$playing_coc <- FALSE
        values$paused_coc <- FALSE
        values$current_year_coc <- min(values$years_coc)
        output$year_slider_cocUI <- renderUI({
          sliderInput(
            "year_slider_coc",
            "Year",
            min = min(values$years_coc),
            max = max(values$years_coc),
            value = min(values$years_coc),
            step = 1,
            animate = FALSE,
            width = "100%",
            ticks = FALSE,
            round = TRUE,
            sep = ""
          )
        })
        values$COC_ready <- values$COC_ready + 1
      }) %...!%
      (function(err) {
        removeNotification("COC_progress")
        showNotification(
          paste("Co-occurrence Network error:", conditionMessage(err)),
          type = "error",
          duration = 8
        )
      })
    return(p)
  })

  output$cocPlot <- renderVisNetwork({
    req(values$COC_ready > 0)
    values$COCnetwork$VIS
  })

  output$cocOverlay <- renderPlotly({
    req(values$COC_ready > 0)
    values$cocOverlay
  })

  ## Co-Occurrence Network over Time ####
  observe({
    req(values$COCnetwork$VIS)
    invalidateLater(as.numeric(input$speed_coc), session)
    isolate({
      #years <- sort(unique(c(values$COCnetwork$VIS$x$nodes$year_med)))
      # years <- sort(unique(c(values$M %>% drop_na(PY) %>% pull(PY))))
      if (
        values$playing_coc &&
          !values$paused_coc &&
          values$index_coc < length(values$years_coc)
      ) {
        values$index_coc <- values$index_coc + 1
        yr <- values$years_coc[values$index_coc]
        values$current_year_coc <- yr
        updateSliderInput(
          session,
          "year_slider_coc",
          value = yr,
          min = min(values$years_coc),
          max = max(values$years_coc)
        )
      }
    })
  })

  # funzione per creare la rete
  render_network_coc <- reactive({
    req(values$COCnetwork$VIS)
    selected_year <- values$current_year_coc
    show_nodes <- values$COCnetwork$VIS$x$nodes %>%
      dplyr::filter(year_med <= selected_year) %>%
      mutate(title = paste(title, year_med, sep = " "))
    show_edges <- values$COCnetwork$VIS$x$edges %>%
      dplyr::filter(from %in% show_nodes$id & to %in% show_nodes$id)

    coords <- show_nodes %>% select(x, y) %>% as.matrix()

    values$COCnetworkOverTime <- visNetwork::visNetwork(
      nodes = show_nodes,
      edges = show_edges,
      type = "full",
      smooth = TRUE,
      physics = FALSE
    ) %>%
      visNetwork::visNodes(
        shadow = TRUE,
        shape = "dot",
        font = list(
          color = show_nodes$font.color,
          size = show_nodes$font.size,
          vadjust = show_nodes$font.vadjust
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
        manipulation = FALSE,
        height = "100%",
        width = "100%"
      )
    values$COCnetworkOverTime
  })

  # aggiorna rete quando cambia lo slider
  observeEvent(input$year_slider_coc, {
    values$current_year_coc <- as.numeric(input$year_slider_coc)
    output$cocOverTime <- renderVisNetwork({
      render_network_coc()
    })
  })

  output$cocYearUI <- renderUI({
    req(values$current_year_coc)
    h3(
      paste(watchEmoji(values$current_year_coc), values$current_year_coc),
      style = "text-align: left; color: #0e4770; font-weight: bold;"
    )
  })

  # start
  observeEvent(input$start_coc, {
    # years <- sort(unique(c(values$COCnetwork$VIS$x$nodes$year_med)))
    # years <- sort(unique(c(values$M %>% drop_na(PY) %>% pull(PY))))
    values$index_coc <- 0
    values$playing_coc <- TRUE
    values$paused_coc <- FALSE
    values$current_year_coc <- min(values$years_coc)
    #shinyjs::hide("export_coc")
    updateSliderInput(session, "year_slider_coc", value = min(values$years_coc))
    output$cocOverTime <- renderVisNetwork({
      render_network_coc()
    })
  })

  observeEvent(input$pause_coc, {
    values$paused_coc <- !values$paused_coc
    # if paused show the button "export_coc" else hide it
    if (values$paused_coc) {
      output$export_cocUI <- renderUI({
        actionButton("export_coc", "⬇ Export", width = "90%")
      })
    } else {
      output$export_cocUI <- renderUI({})
    }
  })

  # reset
  observeEvent(input$reset_coc, {
    values$playing_coc <- FALSE
    values$paused_coc <- FALSE
    values$index_coc <- 0
    # years <- sort(unique(c(values$COCnetwork$VIS$x$nodes$year_med)))
    # years <- sort(unique(c(values$M %>% drop_na(PY) %>% pull(PY))))
    values$current_year_coc <- min(values$years_coc)
    updateSliderInput(session, "year_slider_coc", value = min(values$years_coc))
    output$cocOverTime <- renderVisNetwork({
      nodes <- values$COCnetwork$VIS$x$nodes %>% dplyr::filter(year_med < 0)
      edges <- values$COCnetwork$VIS$x$edges %>%
        dplyr::filter(from %in% nodes$id)
      visNetwork(nodes = nodes, edges = edges)
    })
  })

  ### end Network over Time ----

  # gemini button for word network
  output$cocGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$cocGemini, values)
  })

  output$network.coc <- downloadHandler(
    filename = function() {
      paste("Co_occurrence_network-", Sys.Date(), ".zip", sep = "")
    },
    content <- function(file) {
      tmpdir <- getWD()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      myfile <- paste("mynetwork-", Sys.Date(), sep = "")
      files <- paste0(myfile, c(".net", ".vec", ".clu"))
      graph2Pajek(values$cocnet$graph, filename = myfile)
      zip::zip(file, files)
    },
    contentType = "zip"
  )

  ##### save coc network image as html ####
  output$networkCoc.fig <- downloadHandler(
    filename = "network.html",
    content <- function(con) {
      savenetwork(con, values$COCnetwork$VIS)
    },
    contentType = "html"
  )

  output$cocTable <- renderUI({
    req(values$COC_ready > 0)
    cocData = values$cocnet$cluster_res
    names(cocData) = c(
      "Node",
      "Cluster",
      "Betweenness",
      "Closeness",
      "PageRank"
    )
    renderBibliobox(
      cocData,
      nrow = 10,
      filename = "CoWord_Network",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 3:5,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  ### Degree Plot Co-word analysis ----
  output$cocDegree <- renderPlotly({
    req(values$COC_ready > 0)
    #values$degreePlot <- degreePlot(values$cocnet)
    plot.ly(values$degreePlot)
  })

  observeEvent(input$reportCOC, {
    if (!is.null(values$cocnet$cluster_res)) {
      names(values$cocnet$cluster_res) = c(
        "Node",
        "Cluster",
        "Betweenness",
        "Closeness",
        "PageRank"
      )
      sheetname <- "CoWordNet"
      list_df <- list(values$cocnet$cluster_res)
      list_plot <- list(values$degreePlot)
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      #values$wb <- res$wb
      values$wb <- addGgplotsWb(
        list_plot,
        wb = res$wb,
        res$sheetname,
        col = res$col + 16,
        width = 10,
        height = 7,
        dpi = 75
      )
      #values$fileTFP <- screenSh(selector = "#cocPlot") ## screenshot
      values$fileCOC <- screenSh(values$COCnetwork$VIS, zoom = 2, type = "vis")
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$fileCOC, res$col)
      )
      popUp(title = "Co-occurrence Network", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Correspondence Analysis ----
  observeEvent(input$CSStop, {
    values$CSremove.terms <- data.frame(
      stopword = trimws(readStopwordsFile(
        file = input$CSStop,
        sep = input$CSSep
      ))
    )
    values$GenericSL <- values$CSremove.terms
    popUpGeneric(
      title = "Stopword list",
      type = NULL,
      color = c("#1d8fe1"),
      subtitle = uiOutput("stopwordList"),
      btn_labels = "OK"
    )
  })

  observeEvent(input$FASyn, {
    synonyms <- trimws(readSynWordsFile(
      file = input$FASyn,
      sep = input$FASynSep
    ))
    term <- unlist(lapply(strsplit(synonyms, ";"), function(l) {
      l[1]
    }))
    synList <- unlist(lapply(strsplit(synonyms, ";"), function(l) {
      paste0(trimws(l[-1]), collapse = ";")
    }))
    values$FAsyn.terms <- data.frame(term = term, synonyms = synList)
    values$GenericSYN <- values$FAsyn.terms
    popUpGeneric(
      title = "Synonym List",
      type = NULL,
      color = c("#1d8fe1"),
      subtitle = uiOutput("synonymList"),
      btn_labels = "OK"
    )
  })

  observeEvent(input$applyCA, {
    cs_field <- input$CSfield
    cs_method <- input$method
    cs_n <- input$CSn
    cs_nclusters <- input$nClustersCS
    cs_labelsize <- input$CSlabelsize
    cs_doc <- input$CSdoc
    cs_ngrams <- if (cs_field %in% c("TI", "AB")) {
      as.numeric(input$CSngrams)
    } else {
      1
    }

    if (!(cs_field %in% names(values$M))) {
      showNotification(
        "Selected field is not included in your data collection",
        type = "error",
        duration = 5
      )
      return()
    }

    remove.terms <- if (input$CSStopFile == "Y") {
      trimws(values$CSremove.terms$stopword)
    } else {
      NULL
    }
    synonyms <- if (input$FASynFile == "Y") {
      s <- values$FAsyn.terms %>%
        dplyr::group_by(term) %>%
        dplyr::mutate(term = paste0(term, ";", synonyms)) %>%
        dplyr::select(term)
      s$term
    } else {
      NULL
    }

    M_data <- values$M
    tab <- tableTag(M_data, cs_field, ngrams = cs_ngrams)
    if (length(tab) < 2) {
      showNotification(
        "Not enough terms for analysis",
        type = "error",
        duration = 5
      )
      return()
    }
    minDegree <- as.numeric(tab[cs_n])

    showNotification(
      "Conceptual Structure: computing...",
      id = "CS_progress",
      type = "message",
      duration = NULL
    )

    p <- promises::future_promise(
      {
        bibliometrix::conceptualStructure(
          M_data,
          method = cs_method,
          field = cs_field,
          minDegree = minDegree,
          clust = cs_nclusters,
          k.max = 8,
          stemming = FALSE,
          labelsize = cs_labelsize / 2,
          documents = cs_doc,
          graph = FALSE,
          ngrams = cs_ngrams,
          remove.terms = remove.terms,
          synonyms = synonyms
        )
      },
      seed = TRUE,
      packages = c("bibliometrix", "dplyr", "tidyr", "ggplot2")
    ) %...>%
      (function(CS) {
        removeNotification("CS_progress")
        if (cs_method != "MDS") {
          CSData <- CS$docCoord
          CSData <- data.frame(Documents = row.names(CSData), CSData)
          CSData$dim1 <- round(CSData$dim1, 2)
          CSData$dim2 <- round(CSData$dim2, 2)
          CSData$contrib <- round(CSData$contrib, 2)
          CS$CSData <- CSData
        } else {
          CS$CSData <- data.frame(Docuemnts = NA, dim1 = NA, dim2 = NA)
        }
        switch(
          cs_method,
          CA = {
            WData <- data.frame(
              word = row.names(CS$km.res$data.clust),
              CS$km.res$data.clust,
              stringsAsFactors = FALSE
            )
            names(WData)[4] <- "cluster"
          },
          MCA = {
            WData <- data.frame(
              word = row.names(CS$km.res$data.clust),
              CS$km.res$data.clust,
              stringsAsFactors = FALSE
            )
            names(WData)[4] <- "cluster"
          },
          MDS = {
            WData <- data.frame(
              word = row.names(CS$res),
              CS$res,
              cluster = CS$km.res$cluster
            )
          }
        )
        WData$Dim1 <- round(WData$Dim1, 2)
        WData$Dim2 <- round(WData$Dim2, 2)
        CS$WData <- WData
        values$CS <- CS
        values$plotCS <- ca2plotly(
          CS,
          method = cs_method,
          dimX = 1,
          dimY = 2,
          topWordPlot = Inf,
          threshold = 0.05,
          labelsize = cs_labelsize * 2,
          size = cs_labelsize * 1.5
        )
        values$dendCS <- dend2vis(
          CS$km.res,
          labelsize = cs_labelsize,
          nclusters = as.numeric(cs_nclusters),
          community = FALSE
        )
        values$CS_ready <- values$CS_ready + 1
      }) %...!%
      (function(err) {
        removeNotification("CS_progress")
        showNotification(
          paste("Conceptual Structure error:", conditionMessage(err)),
          type = "error",
          duration = 8
        )
      })
    return(p)
  })

  # gemini button for correspondence analysis
  output$CSGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$CSGemini, values)
  })

  output$FAplot.save <- downloadHandler(
    filename = function() {
      #
      paste("FactorialAnalysis_", Sys.Date(), ".zip", sep = "")
    },
    content <- function(file) {
      #go to a temp dir to avoid permission issues
      owd <- setwd(getWD())
      on.exit(setwd(owd))
      files <- c(
        paste("FactorialMap_", Sys.Date(), ".png", sep = ""),
        paste("Dendrogram_", Sys.Date(), ".png", sep = "")
        #paste("MostContribDocuments_", Sys.Date(), ".png", sep=""),
        #paste("MostCitedDocuments_", Sys.Date(), ".png", sep="")
      )
      ggsave(
        filename = files[1],
        plot = values$CS$graph_terms,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 1.5,
        bg = "white"
      )
      png(
        filename = files[2],
        height = values$h,
        width = values$h * 2,
        units = "in",
        res = values$dpi
      )
      plot(values$CS$graph_dendogram)
      dev.off()
      zip::zip(file, files)
    },
    contentType = "zip"
  )

  output$CSPlot1 <- renderPlotly({
    req(values$CS_ready > 0)
    #CS=values$CS
    #save(CS,file="provaCS.rdata")
    values$plotCS #<- ca2plotly(values$CS, method=input$method ,dimX = 1, dimY = 2, topWordPlot = Inf, threshold=0.05, labelsize = input$CSlabelsize*2, size=input$CSlabelsize*1.5)
  })

  output$CSPlot4 <- renderVisNetwork({
    req(values$CS_ready > 0)
    #dend2vis(values$CS$km.res, labelsize=input$CSlabelsize, nclusters=as.numeric(input$nClustersCS), community=FALSE)
    values$dendCS
    #values$CS$graph_dendogram)
  })

  output$CSTableW <- renderUI({
    req(values$CS_ready > 0)
    WData <- values$CS$WData
    renderBibliobox(
      WData,
      nrow = 10,
      filename = "CoWord_Factorial_Analysis_Words_By_Cluster",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 2:3,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$CSTableD <- renderUI({
    req(values$CS_ready > 0)
    CSData <- values$CS$CSData
    renderBibliobox(
      CSData,
      nrow = 10,
      filename = "CoWord_Factorial_Analysis_Articles_By_Cluster",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 2:4,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  # add to report
  observeEvent(input$reportFA, {
    if (!is.null(values$CS$params)) {
      list_df <- list(values$CS$params, values$CS$WData, values$CS$CSData)
      list_plot <- list(values$CS$graph_terms, values$CS$graph_dendogram)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "FactorialAnalysis",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Factorial Analysis", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Thematic Map ----
  observeEvent(input$TMStop, {
    values$TMremove.terms <- data.frame(
      stopword = trimws(readStopwordsFile(
        file = input$TMStop,
        sep = input$TMSep
      ))
    )
    values$GenericSL <- values$TMremove.terms
    popUpGeneric(
      title = "Stopword list",
      type = NULL,
      color = c("#1d8fe1"),
      subtitle = uiOutput("stopwordList"),
      btn_labels = "OK"
    )
  })

  observeEvent(input$TMapSyn, {
    synonyms <- trimws(readSynWordsFile(
      file = input$TMapSyn,
      sep = input$TMapSynSep
    ))
    term <- unlist(lapply(strsplit(synonyms, ";"), function(l) {
      l[1]
    }))
    synList <- unlist(lapply(strsplit(synonyms, ";"), function(l) {
      paste0(trimws(l[-1]), collapse = ";")
    }))
    values$TMapsyn.terms <- data.frame(term = term, synonyms = synList)
    values$GenericSYN <- values$TMapsyn.terms
    popUpGeneric(
      title = "Synonym List",
      type = NULL,
      color = c("#1d8fe1"),
      subtitle = uiOutput("synonymList"),
      btn_labels = "OK"
    )
  })

  observeEvent(input$applyTM, {
    if (input$TMfield %in% c("TI", "AB")) {
      ngrams <- as.numeric(input$TMngrams)
    } else {
      ngrams <- 1
    }

    ### load file with terms to remove
    if (input$TMStopFile == "Y") {
      remove.terms <- trimws(values$TMremove.terms$stopword)
    } else {
      remove.terms <- NULL
    }

    ### load file with synonyms
    if (input$TMapSynFile == "Y") {
      synonyms <- values$TMapsyn.terms %>%
        group_by(term) %>%
        mutate(term = paste0(term, ";", synonyms)) %>%
        select(term)
      synonyms <- synonyms$term
    } else {
      synonyms <- NULL
    }

    ## Cache check: skip thematicMap() if params unchanged
    cache_key <- make_cache_key(
      fp = data_fingerprint(values$M),
      field = input$TMfield,
      n = input$TMn,
      minfreq = input$TMfreq,
      ngrams = ngrams,
      repulsion = input$TMrepulsion,
      stemming = input$TMstemming,
      size = input$sizeTM,
      cluster = input$TMCluster,
      n.labels = input$TMn.labels,
      alpha = input$TMalpha,
      remove.terms = remove.terms,
      synonyms = synonyms,
      seed = values$random_seed
    )

    if (identical(cache_key, values$cache_TM_key)) {
      values$TMAP_ready <- values$TMAP_ready + 1
      return()
    }

    showNotification(
      "Thematic Map: computing...",
      id = "TM_progress",
      type = "message",
      duration = NULL
    )

    # Snapshot
    M_data <- values$M
    field <- input$TMfield
    n <- input$TMn
    minfreq <- input$TMfreq
    repulsion <- input$TMrepulsion
    stemming <- input$TMstemming
    size <- input$sizeTM
    cluster <- input$TMCluster
    n.labels <- input$TMn.labels
    alpha <- input$TMalpha
    seed <- values$random_seed

    p <- promises::future_promise(
      {
        bibliometrix::thematicMap(
          M_data,
          field = field,
          n = n,
          minfreq = minfreq,
          ngrams = ngrams,
          community.repulsion = repulsion,
          stemming = stemming,
          size = size,
          cluster = cluster,
          n.labels = n.labels,
          repel = FALSE,
          remove.terms = remove.terms,
          synonyms = synonyms,
          subgraphs = TRUE,
          alpha = alpha,
          seed = seed
        )
      },
      seed = TRUE,
      packages = c("bibliometrix", "dplyr", "tidyr")
    ) %...>%
      (function(TM) {
        removeNotification("TM_progress")
        if (TM$nclust == 0) {
          showNotification(
            "No topics found. Please select different parameters.",
            type = "error",
            duration = 8
          )
          return()
        }
        values$TM <- TM
        values$TM$doc2clust <- TM$documentToClusters %>%
          select(Assigned_cluster, SR, pagerank)
        values$TM$documentToClusters$DI <- paste0(
          '<a href=\"https://doi.org/',
          TM$documentToClusters$DI,
          '\" target=\"_blank\">',
          TM$documentToClusters$DI,
          '</a>'
        )
        names(values$TM$documentToClusters)[1:9] <- c(
          "DOI",
          "Authors",
          "Title",
          "Source",
          "Year",
          "TotalCitation",
          "TCperYear",
          "NTC",
          "SR"
        )
        values$TM$words <- values$TM$words[, -c(4, 6)]
        values$TM$clusters_orig <- values$TM$clusters
        values$TM$clusters <- values$TM$clusters[, c(9, 5:8, 11)]
        names(values$TM$clusters) <- c(
          "Cluster",
          "CallonCentrality",
          "CallonDensity",
          "RankCentrality",
          "RankDensity",
          "ClusterFrequency"
        )
        values$TMmap <- plot.ly(
          values$TM$map,
          size = 0.07,
          aspectratio = 1.3,
          customdata = values$TM$clusters$color
        )
        values$cache_TM_key <- cache_key
        values$TMAP_ready <- values$TMAP_ready + 1
      }) %...!%
      (function(err) {
        removeNotification("TM_progress")
        showNotification(
          paste("Thematic Map error:", conditionMessage(err)),
          type = "error",
          duration = 8
        )
      })
    return(p)
  })
  output$TMPlot <- renderPlotly({
    req(values$TMAP_ready > 0)
    values$TMmap
  })

  # gemini button for Thematic Map
  output$TMGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$TMGemini, values)
  })

  ### click cluster networks

  plotModal <- function(session) {
    ns <- session$ns
    modalDialog(
      h3(strong(("Cluster Network"))),
      visNetworkOutput(ns("cocPlotClust")),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton(
          label = "Save",
          inputId = "cocPlotClust",
          icon = icon("camera", lib = "glyphicon")
        ),
        modalButton("Close")
      ),
    )
  }

  observeEvent(input$cocPlotClust, {
    #Time <- format(Sys.time(),'%H%M%S')
    filename = paste(
      "TMClusterGraph-",
      "_",
      gsub(" |:", "", Sys.time()),
      ".png",
      sep = ""
    )
    screenShot(values$plotClust, filename = filename, type = "vis")
  })

  observeEvent(event_data("plotly_click"), {
    if (input$sidebarmenu == "thematicMap") {
      showModal(plotModal(session))
    }
  })

  output$cocPlotClust <- renderVisNetwork({
    values$d <- event_data("plotly_click")
    coord <- values$d[c("x", "y")]
    color <- values$TM$clusters_orig %>%
      dplyr::filter(rcentrality == coord$x, rdensity == coord$y) %>%
      select(color) %>%
      as.character()
    g <- values$TM$subgraphs[[color]]
    values$plotClust <- igraph2visClust(
      g,
      curved = F,
      labelsize = 4,
      opacity = 0.5,
      shape = "dot",
      shadow = TRUE,
      edgesize = 5
    )$VIS
    values$plotClust
  })

  ### end click cluster subgraphs

  output$NetPlot <- renderVisNetwork({
    req(values$TMAP_ready > 0)
    values$networkTM <- igraph2vis(
      g = values$TM$net$graph,
      curved = (input$coc.curved == "Yes"),
      labelsize = input$labelsize,
      opacity = input$cocAlpha,
      type = input$layout,
      shape = input$coc.shape,
      net = values$TM$net,
      noOverlap = input$noOverlapTM
    )
    values$networkTM$VIS
  })

  output$TMplot.save <- downloadHandler(
    filename = function() {
      paste("ThematicMap-", Sys.Date(), ".png", sep = "")
    },
    content <- function(file) {
      ggsave(
        filename = file,
        plot = values$TM$map,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 1.5,
        bg = "white"
      )
    },
    contentType = "png"
  )

  output$TMTable <- renderUI({
    req(values$TMAP_ready > 0)
    tmData = values$TM$words
    renderBibliobox(
      tmData,
      nrow = 10,
      filename = "Thematic_Map_Terms",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 5:7,
      dom = FALSE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$TMTableCluster <- renderUI({
    req(values$TMAP_ready > 0)
    tmData <- values$TM$clusters
    renderBibliobox(
      tmData,
      nrow = 10,
      filename = "Thematic_Map_Clusters",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 2:3,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$TMTableDocument <- renderUI({
    req(values$TMAP_ready > 0)
    tmDataDoc <- values$TM$documentToClusters
    renderBibliobox(
      tmDataDoc,
      nrow = 10,
      filename = "Thematic_Map_Documents",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = c(7:8, 10:(ncol(tmDataDoc) - 2), ncol(tmDataDoc)),
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportTM, {
    if (!is.null(values$TM$words)) {
      popUp(title = NULL, type = "waiting")
      list_df <- list(
        values$TM$params,
        values$TM$words,
        values$TM$clusters,
        values$TM$documentToClusters
      )
      list_plot <- list(values$TM$map, values$TM$net$graph)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "ThematicMap",
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Thematic Map", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Thematic Evolution ----
  observeEvent(input$TEStop, {
    values$TEremove.terms <- data.frame(
      stopword = trimws(readStopwordsFile(
        file = input$TEStop,
        sep = input$TESep
      ))
    )
    values$GenericSL <- values$TEremove.terms
    popUpGeneric(
      title = "Stopword list",
      type = NULL,
      color = c("#1d8fe1"),
      subtitle = uiOutput("stopwordList"),
      btn_labels = "OK"
    )
  })

  observeEvent(input$TESyn, {
    synonyms <- trimws(readSynWordsFile(
      file = input$TESyn,
      sep = input$TESynSep
    ))
    term <- unlist(lapply(strsplit(synonyms, ";"), function(l) {
      l[1]
    }))
    synList <- unlist(lapply(strsplit(synonyms, ";"), function(l) {
      paste0(trimws(l[-1]), collapse = ";")
    }))
    values$TEsyn.terms <- data.frame(term = term, synonyms = synList)
    values$GenericSYN <- values$TEsyn.terms
    popUpGeneric(
      title = "Synonym List",
      type = NULL,
      color = c("#1d8fe1"),
      subtitle = uiOutput("synonymList"),
      btn_labels = "OK"
    )
  })

  output$sliders <- renderUI({
    numSlices <- as.integer(input$numSlices)
    v = quantile(
      values$M$PY,
      seq(0, 1, by = (1 / (numSlices + 1))),
      na.rm = TRUE
    )
    v = round(v[-c(1, length(v))], 0)
    lapply(1:numSlices, function(i) {
      numericInput(
        inputId = paste0("Slice", i),
        label = paste("Cutting Year", i),
        value = v[i],
        min = min(values$M$PY, na.rm = TRUE) + 1,
        max = max(values$M$PY, na.rm = TRUE) - 1,
        step = 1
      )
    })
  })

  observeEvent(input$applyTE, {
    if (input$TEfield %in% c("TI", "AB")) {
      ngrams <- as.numeric(input$TEngrams)
    } else {
      ngrams <- 1
    }

    ### load file with terms to remove
    if (input$TEStopFile == "Y") {
      remove.terms <- trimws(values$TEremove.terms$stopword)
    } else {
      remove.terms <- NULL
    }
    #values$TEremove.terms <- remove.terms
    ### end of block

    ### load file with synonyms
    if (input$TESynFile == "Y") {
      synonyms <- values$TEsyn.terms %>%
        group_by(term) %>%
        mutate(term = paste0(term, ";", synonyms)) %>%
        select(term)
      synonyms <- synonyms$term
    } else {
      synonyms <- NULL
    }
    #values$TEsyn.terms <- synonyms
    ### end of block

    values$yearSlices <- as.numeric()
    if (is.null(input$numSlices)) {
      values$yearSlices <- median(values$M$PY, na.rm = TRUE)
    } else {
      for (i in 1:as.integer(input$numSlices)) {
        if (length(input[[paste0("Slice", i)]]) > 0) {
          values$yearSlices <- c(values$yearSlices, input[[paste0("Slice", i)]])
        }
      }
    }

    if (length(values$yearSlices) > 0) {
      ## Cache check: skip thematicEvolution() if params unchanged
      cache_key <- make_cache_key(
        fp = data_fingerprint(values$M),
        field = input$TEfield,
        yearSlices = values$yearSlices,
        n = input$nTE,
        minFreq = input$fTE,
        size = input$sizeTE,
        cluster = input$TECluster,
        n.labels = input$TEn.labels,
        ngrams = ngrams,
        remove.terms = remove.terms,
        synonyms = synonyms,
        seed = values$random_seed,
        alpha = input$TEalpha,
        minFlow = input$minFlowTE
      )

      if (identical(cache_key, values$cache_TE_key)) {
        values$TE_ready <- values$TE_ready + 1
        return()
      }

      values$TE_computing <- TRUE
      values$TE_error <- NULL
      showNotification(
        "Thematic Evolution: computing...",
        id = "TE_progress",
        type = "message",
        duration = NULL
      )

      # Snapshot variables for the future
      M_data <- values$M
      field <- input$TEfield
      yearSlices <- values$yearSlices
      n <- input$nTE
      minFreq <- input$fTE
      size <- input$sizeTE
      cluster <- input$TECluster
      n.labels <- input$TEn.labels
      seed <- values$random_seed
      alpha <- input$TEalpha
      minFlow <- input$minFlowTE

      # Async: heavy computation in background
      p <- promises::future_promise(
        {
          bibliometrix::thematicEvolution(
            M_data,
            field = field,
            yearSlices,
            n = n,
            minFreq = minFreq,
            size = size,
            cluster = cluster,
            n.labels = n.labels,
            repel = FALSE,
            ngrams = ngrams,
            remove.terms = remove.terms,
            synonyms = synonyms,
            seed = seed,
            assign.evolution.colors = list(assign = TRUE, alpha = alpha)
          )
        },
        seed = TRUE,
        packages = c("bibliometrix", "tidyr", "dplyr")
      ) %...>%
        (function(nexus) {
          removeNotification("TE_progress")
          if (isFALSE(nexus$check)) {
            values$TE_computing <- FALSE
            values$TE_error <- "No topics in one or more periods. Please select a different set of parameters."
            showNotification(values$TE_error, type = "error", duration = 8)
            return()
          }
          values$nexus <- nexus
          for (i in 1:(length(yearSlices) + 1)) {
            values$nexus$TM[[i]]$words <- values$nexus$TM[[i]]$words[, -c(4, 6)]
            values$nexus$TM[[i]]$clusters <- values$nexus$TM[[i]]$clusters[, c(
              9,
              5:8,
              11
            )]
            names(values$nexus$TM[[i]]$clusters) <- c(
              "Cluster",
              "CallonCentrality",
              "CallonDensity",
              "RankCentrality",
              "RankDensity",
              "ClusterFrequency"
            )

            values$nexus$TM[[i]]$documentToClusters$DI <- paste0(
              '<a href=\"https://doi.org/',
              values$nexus$TM[[i]]$documentToClusters$DI,
              '\" target=\"_blank\">',
              values$nexus$TM[[i]]$documentToClusters$DI,
              '</a>'
            )
            names(values$nexus$TM[[i]]$documentToClusters)[1:9] <- c(
              "DOI",
              "Authors",
              "Title",
              "Source",
              "Year",
              "TotalCitation",
              "SR",
              "TCperYear",
              "NTC"
            )
            values$nexus$TM[[i]]$documentToClusters <- values$nexus$TM[[
              i
            ]]$documentToClusters[c(
              "DOI",
              "Authors",
              "Title",
              "Source",
              "Year",
              "TotalCitation",
              "TCperYear",
              "NTC",
              "SR"
            )]
          }
          values$nexus$Data <- values$nexus$Data[
            values$nexus$Data$Inc_index > 0,
            -c(4, 8)
          ]
          values$TEplot <- plotThematicEvolution(
            Nodes = values$nexus$Nodes,
            Edges = values$nexus$Edges,
            min.flow = minFlow
          )
          values$cache_TE_key <- cache_key
          values$TE_computing <- FALSE
          values$TE_ready <- values$TE_ready + 1
        }) %...!%
        (function(err) {
          removeNotification("TE_progress")
          values$TE_computing <- FALSE
          values$TE_error <- conditionMessage(err)
          showNotification(
            paste("Thematic Evolution error:", values$TE_error),
            type = "error",
            duration = 8
          )
        })

      return(p) # Return promise so Shiny manages async lifecycle
    }
  })

  output$TEPlot <- plotly::renderPlotly({
    req(values$TE_ready > 0)
    values$TEplot
  })

  # gemini button for word network
  output$TEGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$TEGemini, values)
  })

  session$onFlushed(function() {
    shinyjs::runjs("$('#TEPlot').trigger('resize');")
  })

  output$TEplot.save <- downloadHandler(
    filename = function() {
      paste("ThematicEvolution-", Sys.Date(), ".zip", sep = "")
    },
    content <- function(file) {
      #go to a temp dir to avoid permission issues
      tmpdir <- getWD()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      files <- filenameTE <- paste(
        "ThematicEvolution_",
        Sys.Date(),
        ".png",
        sep = ""
      )

      for (i in 1:length(values$nexus$TM)) {
        fileName <- paste(
          "ThematicEvolution-Map_",
          i,
          "_",
          Sys.Date(),
          ".png",
          sep = ""
        )
        ggsave(
          filename = fileName,
          plot = values$nexus$TM[[i]]$map,
          dpi = values$dpi,
          height = values$h,
          width = values$h * 1.5,
          bg = "white"
        )
        files <- c(fileName, files)
      }
      plot2png(
        values$TEplot,
        filename = filenameTE,
        zoom = 2,
        type = "plotly"
      )
      zip::zip(file, files)
    },
    contentType = "zip"
  )

  output$TETable <- renderUI({
    req(values$TE_ready > 0)
    TEData = values$nexus$Data
    names(TEData) = c(
      "From",
      "To",
      "Words",
      "Weighted Inclusion Index",
      "Inclusion Index",
      "Occurrences",
      "Stability Index"
    )
    renderBibliobox(
      TEData,
      nrow = 10,
      filename = "Thematic_Evolution",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = c(4, 5, 7),
      dom = TRUE,
      size = '85%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 2,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$TMPlot1 <- renderPlotly({
    req(values$TE_ready > 0)
    if (length(values$nexus$TM) >= 1) {
      plot.ly(values$nexus$TM[[1]]$map, size = 0.07, aspectratio = 1.3)
    } else {
      emptyPlot("You have selected fewer periods!")
    }
  })

  output$TMPlot2 <- renderPlotly({
    req(values$TE_ready > 0)
    if (length(values$nexus$TM) >= 2) {
      plot.ly(values$nexus$TM[[2]]$map, size = 0.07, aspectratio = 1.3)
    } else {
      emptyPlot("You have selected fewer periods!")
    }
  })

  output$TMPlot3 <- renderPlotly({
    req(values$TE_ready > 0)
    if (length(values$nexus$TM) >= 3) {
      plot.ly(values$nexus$TM[[3]]$map, size = 0.07, aspectratio = 1.3)
    } else {
      emptyPlot("You have selected fewer periods!")
    }
  })

  output$TMPlot4 <- renderPlotly({
    req(values$TE_ready > 0)
    if (length(values$nexus$TM) >= 4) {
      plot.ly(values$nexus$TM[[4]]$map, size = 0.07, aspectratio = 1.3)
    } else {
      (emptyPlot("You have selected fewer periods!"))
    }
  })

  output$TMPlot5 <- renderPlotly({
    req(values$TE_ready > 0)
    if (length(values$nexus$TM) >= 5) {
      plot.ly(values$nexus$TM[[5]]$map, size = 0.07, aspectratio = 1.3)
    } else {
      (emptyPlot("You have selected fewer periods!"))
    }
  })

  output$NetPlot1 <- renderVisNetwork({
    req(values$TE_ready > 0)
    k = 1
    values$network1 <- igraph2vis(
      g = values$nexus$Net[[k]]$graph,
      curved = (input$coc.curved == "Yes"),
      labelsize = input$labelsize,
      opacity = input$cocAlpha,
      type = input$layout,
      shape = input$coc.shape,
      net = values$nexus$Net[[k]],
      noOverlap = input$noOverlapTE
    )
    values$network1$VIS
  })

  output$NetPlot2 <- renderVisNetwork({
    req(values$TE_ready > 0)
    k = 2
    values$network2 <- igraph2vis(
      g = values$nexus$Net[[k]]$graph,
      curved = (input$coc.curved == "Yes"),
      labelsize = input$labelsize,
      opacity = input$cocAlpha,
      type = input$layout,
      shape = input$coc.shape,
      net = values$nexus$Net[[k]],
      noOverlap = input$noOverlapTE
    )
    values$network2$VIS
  })

  output$NetPlot3 <- renderVisNetwork({
    req(values$TE_ready > 0)
    k = 3
    values$network3 <- igraph2vis(
      g = values$nexus$Net[[k]]$graph,
      curved = (input$coc.curved == "Yes"),
      labelsize = input$labelsize,
      opacity = input$cocAlpha,
      type = input$layout,
      shape = input$coc.shape,
      net = values$nexus$Net[[k]],
      noOverlap = input$noOverlapTE
    )
    values$network3$VIS
  })

  output$NetPlot4 <- renderVisNetwork({
    req(values$TE_ready > 0)
    k = 4
    values$network4 <- igraph2vis(
      g = values$nexus$Net[[k]]$graph,
      curved = (input$coc.curved == "Yes"),
      labelsize = input$labelsize,
      opacity = input$cocAlpha,
      type = input$layout,
      shape = input$coc.shape,
      net = values$nexus$Net[[k]],
      noOverlap = input$noOverlapTE
    )
    values$network4$VIS
  })

  output$NetPlot5 <- renderVisNetwork({
    req(values$TE_ready > 0)
    k = 5
    values$network5 <- igraph2vis(
      g = values$nexus$Net[[k]]$graph,
      curved = (input$coc.curved == "Yes"),
      labelsize = input$labelsize,
      opacity = input$cocAlpha,
      type = input$layout,
      shape = input$coc.shape,
      net = values$nexus$Net[[k]],
      noOverlap = input$noOverlapTE
    )
    values$network5$VIS
  })

  output$TMTable1 <- renderUI({
    req(values$TE_ready > 0)
    tmData = values$nexus$TM[[1]]$words
    renderBibliobox(
      tmData,
      nrow = 10,
      filename = "Thematic_Map_Period_1_Terms",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 5:7,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$TMTable2 <- renderUI({
    req(values$TE_ready > 0)
    tmData = values$nexus$TM[[2]]$words
    renderBibliobox(
      tmData,
      nrow = 10,
      filename = "Thematic_Map_Period_2_Terms",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 5:7,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$TMTable3 <- renderUI({
    req(values$TE_ready > 0)
    tmData = values$nexus$TM[[3]]$words
    renderBibliobox(
      tmData,
      nrow = 10,
      filename = "Thematic_Map_Period_3_Terms",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 5:7,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$TMTable4 <- renderUI({
    req(values$TE_ready > 0)
    tmData = values$nexus$TM[[4]]$words
    renderBibliobox(
      tmData,
      nrow = 10,
      filename = "Thematic_Map_Period_4_Terms",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 5:7,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$TMTable5 <- renderUI({
    req(values$TE_ready > 0)
    tmData = values$nexus$TM[[5]]$words
    renderBibliobox(
      tmData,
      nrow = 10,
      filename = "Thematic_Map_Period_5_Terms",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 5:7,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$TMTableCluster1 <- renderUI({
    req(values$TE_ready > 0)
    tmData <- values$nexus$TM[[1]]$clusters
    renderBibliobox(
      tmData,
      nrow = 10,
      filename = "Thematic_Map_Period_1_Clusters",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 2:3,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$TMTableCluster2 <- renderUI({
    req(values$TE_ready > 0)
    tmData <- values$nexus$TM[[2]]$clusters
    renderBibliobox(
      tmData,
      nrow = 10,
      filename = "Thematic_Map_Period_2_Clusters",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 2:3,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$TMTableCluster3 <- renderUI({
    req(values$TE_ready > 0)
    tmData <- values$nexus$TM[[3]]$clusters
    renderBibliobox(
      tmData,
      nrow = 10,
      filename = "Thematic_Map_Period_3_Clusters",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 2:3,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$TMTableCluster4 <- renderUI({
    req(values$TE_ready > 0)
    tmData <- values$nexus$TM[[4]]$clusters
    renderBibliobox(
      tmData,
      nrow = 10,
      filename = "Thematic_Map_Period_4_Clusters",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 2:3,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$TMTableCluster5 <- renderUI({
    req(values$TE_ready > 0)
    tmData <- values$nexus$TM[[5]]$clusters
    renderBibliobox(
      tmData,
      nrow = 10,
      filename = "Thematic_Map_Period_5_Clusters",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 2:3,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$TMTableDocument1 <- renderUI({
    req(values$TE_ready > 0)
    tmDataDoc <- values$nexus$TM[[1]]$documentToClusters
    renderBibliobox(
      tmDataDoc,
      nrow = 10,
      filename = "Thematic_Map_Period_1_Documents",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = c(7:8, 10:(ncol(tmDataDoc) - 2), ncol(tmDataDoc)),
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$TMTableDocument2 <- renderUI({
    req(values$TE_ready > 0)
    tmDataDoc <- values$nexus$TM[[2]]$documentToClusters
    tmDataDoc$DI <- paste0(
      '<a href=\"https://doi.org/',
      tmDataDoc$DI,
      '\" target=\"_blank\">',
      tmDataDoc$DI,
      '</a>'
    )
    names(tmDataDoc)[1:9] <- c(
      "DOI",
      "Authors",
      "Title",
      "Source",
      "Year",
      "TotalCitation",
      "TCperYear",
      "NTC",
      "SR"
    )
    renderBibliobox(
      tmDataDoc,
      nrow = 10,
      filename = "Thematic_Map_Period_2_Documents",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = c(7:8, 10:(ncol(tmDataDoc) - 2), ncol(tmDataDoc)),
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$TMTableDocument3 <- renderUI({
    req(values$TE_ready > 0)
    tmDataDoc <- values$nexus$TM[[3]]$documentToClusters
    tmDataDoc$DI <- paste0(
      '<a href=\"https://doi.org/',
      tmDataDoc$DI,
      '\" target=\"_blank\">',
      tmDataDoc$DI,
      '</a>'
    )
    names(tmDataDoc)[1:9] <- c(
      "DOI",
      "Authors",
      "Title",
      "Source",
      "Year",
      "TotalCitation",
      "TCperYear",
      "NTC",
      "SR"
    )

    renderBibliobox(
      tmDataDoc,
      nrow = 10,
      filename = "Thematic_Map_Period_3_Documents",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = c(7:8, 10:(ncol(tmDataDoc) - 2), ncol(tmDataDoc)),
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$TMTableDocument4 <- renderUI({
    req(values$TE_ready > 0)
    tmDataDoc <- values$nexus$TM[[4]]$documentToClusters
    tmDataDoc$DI <- paste0(
      '<a href=\"https://doi.org/',
      tmDataDoc$DI,
      '\" target=\"_blank\">',
      tmDataDoc$DI,
      '</a>'
    )
    names(tmDataDoc)[1:9] <- c(
      "DOI",
      "Authors",
      "Title",
      "Source",
      "Year",
      "TotalCitation",
      "TCperYear",
      "NTC",
      "SR"
    )

    renderBibliobox(
      tmDataDoc,
      nrow = 10,
      filename = "Thematic_Map_Period_4_Documents",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = c(7:8, 10:(ncol(tmDataDoc) - 2), ncol(tmDataDoc)),
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  output$TMTableDocument5 <- renderUI({
    req(values$TE_ready > 0)
    tmDataDoc <- values$nexus$TM[[5]]$documentToClusters
    tmDataDoc$DI <- paste0(
      '<a href=\"https://doi.org/',
      tmDataDoc$DI,
      '\" target=\"_blank\">',
      tmDataDoc$DI,
      '</a>'
    )
    names(tmDataDoc)[1:9] <- c(
      "DOI",
      "Authors",
      "Title",
      "Source",
      "Year",
      "TotalCitation",
      "TCperYear",
      "NTC",
      "SR"
    )

    renderBibliobox(
      tmDataDoc,
      nrow = 10,
      filename = "Thematic_Map_Period_5_Documents",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = c(7:8, 10:(ncol(tmDataDoc) - 2), ncol(tmDataDoc)),
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportTE, {
    if (!is.null(values$nexus$Data)) {
      popUp(title = NULL, type = "waiting")
      sheetname <- "ThematicEvolution"
      list_df <- list(values$nexus$params, values$nexus$Data)
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      #values$wb <- res$wb
      #values$fileTFP <- screenSh(selector = "#TEPlot") ## screenshot
      values$fileTEplot <- screenSh(values$TEplot, zoom = 2, type = "plotly")
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$fileTEplot, res$col)
      )

      ## Periods
      L <- length(values$nexus$TM)
      wb <- res$wb
      for (l in 1:L) {
        if (!is.null(values$nexus$TM[[l]]$words)) {
          list_df <- list(
            values$nexus$TM[[l]]$params,
            values$nexus$TM[[l]]$words,
            values$nexus$TM[[l]]$clusters,
            values$nexus$TM[[l]]$documentToClusters
          )
          list_plot <- list(
            values$nexus$TM[[l]]$map,
            values$nexus$TM[[l]]$net$graph
          )
          wb <- addSheetToReport(
            list_df,
            list_plot,
            sheetname = paste("TE_Period_", l, sep = ""),
            wb = wb
          )
          #
        }
      }
      values$wb <- wb
      popUp(title = "Thematic Evolution", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  # INTELLECTUAL STRUCTURE ####
  ### Co-citation network ----
  observeEvent(input$applyCocit, {
    # Extract params
    n <- input$citNodes
    label.n <- input$citLabels
    cit_field <- input$citField
    cit_sep <- input$citSep
    cit_shortlabel <- input$citShortlabel
    cit_layout <- input$citlayout
    cit_curved <- (input$cocit.curved == "Yes")
    cit_labelsize <- input$citlabelsize
    cit_shape <- input$cocit.shape
    cit_shadow <- (input$cocit.shadow == "Yes")
    cit_noOverlap <- input$citNoOverlap
    cit_label_cex <- (input$citlabel.cex == "Yes")
    cit_edges_min <- input$citedges.min
    cit_isolates <- (input$cit.isolates == "yes")
    cit_edgesize <- input$citedgesize
    cit_cluster <- input$cocitCluster
    cit_repulsion <- input$cocit.repulsion / 2
    shortlabel <- (cit_shortlabel == "Yes")

    if (label.n > n) {
      label.n <- n
    }

    # Synchronous: metaTagExtraction + biblioNetwork (modifies M + builds matrix)
    M_data <- values$M
    need_rebuild <- (dim(values$NetRefs)[1] == 1 |
      !(cit_field == values$citField) |
      !(cit_sep == values$citSep) |
      !(cit_shortlabel == values$citShortlabel) |
      (dim(values$NetRefs)[1] != n))

    if (need_rebuild) {
      values$citField <- cit_field
      values$citSep <- cit_sep
      values$citShortlabel <- cit_shortlabel
      NetRefs <- switch(
        cit_field,
        CR = {
          bibliometrix::biblioNetwork(
            M_data,
            analysis = "co-citation",
            network = "references",
            n = n,
            sep = cit_sep,
            shortlabel = shortlabel
          )
        },
        CR_AU = {
          if (!("CR_AU" %in% names(M_data))) {
            M_data <- bibliometrix::metaTagExtraction(
              M_data,
              Field = "CR_AU",
              sep = cit_sep
            )
            values$M <- M_data
          }
          bibliometrix::biblioNetwork(
            M_data,
            analysis = "co-citation",
            network = "authors",
            n = n,
            sep = cit_sep
          )
        },
        CR_SO = {
          if (!("CR_SO" %in% names(M_data))) {
            M_data <- bibliometrix::metaTagExtraction(
              M_data,
              Field = "CR_SO",
              sep = cit_sep
            )
            values$M <- M_data
          }
          bibliometrix::biblioNetwork(
            M_data,
            analysis = "co-citation",
            network = "sources",
            n = n,
            sep = cit_sep
          )
        }
      )
      values$NetRefs <- NetRefs
      values$Title <- switch(
        cit_field,
        CR = "Cited References network",
        CR_AU = "Cited Authors network",
        CR_SO = "Cited Sources network"
      )
    } else {
      NetRefs <- values$NetRefs
    }
    net_title <- values$Title

    showNotification(
      "Co-citation Network: computing...",
      id = "COCIT_progress",
      type = "message",
      duration = NULL
    )

    p <- promises::future_promise(
      {
        bibliometrix::networkPlot(
          NetRefs,
          normalize = NULL,
          Title = net_title,
          type = cit_layout,
          size.cex = TRUE,
          size = 5,
          remove.multiple = FALSE,
          edgesize = cit_edgesize * 3,
          labelsize = cit_labelsize,
          label.cex = cit_label_cex,
          curved = cit_curved,
          label.n = label.n,
          edges.min = cit_edges_min,
          label.color = FALSE,
          remove.isolates = cit_isolates,
          alpha = 0.7,
          cluster = cit_cluster,
          community.repulsion = cit_repulsion,
          verbose = FALSE
        )
      },
      seed = TRUE,
      packages = c("bibliometrix", "dplyr", "igraph", "ggplot2", "Matrix")
    ) %...>%
      (function(cocitnet) {
        removeNotification("COCIT_progress")
        values$cocitnet <- cocitnet
        values$COCITnetwork <- igraph2vis(
          g = cocitnet$graph,
          curved = cit_curved,
          labelsize = cit_labelsize,
          opacity = 0.7,
          type = cit_layout,
          shape = cit_shape,
          net = cocitnet,
          shadow = cit_shadow,
          noOverlap = cit_noOverlap
        )
        values$cocitOverlay <- overlayPlotly(values$COCITnetwork$VIS)
        values$degreePlot <- degreePlot(cocitnet)
        values$COCIT_ready <- values$COCIT_ready + 1
      }) %...!%
      (function(err) {
        removeNotification("COCIT_progress")
        showNotification(
          paste("Co-citation Network error:", conditionMessage(err)),
          type = "error",
          duration = 8
        )
      })
    return(p)
  })

  output$cocitPlot <- renderVisNetwork({
    req(values$COCIT_ready > 0)
    isolate(values$COCITnetwork$VIS)
  })

  output$cocitOverlay <- renderPlotly({
    req(values$COCIT_ready > 0)
    values$cocitOverlay
  })

  # gemini button for co-citation network
  output$cocitGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$cocitGemini, values)
  })

  output$network.cocit <- downloadHandler(
    filename = function() {
      paste("Co_citation_network-", Sys.Date(), ".zip", sep = "")
    },
    content <- function(file) {
      tmpdir <- getWD()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      # print(tmpdir)
      #igraph::write.graph(values$obj$graph_pajek,file=file, format="pajek")
      myfile <- paste("mynetwork-", Sys.Date(), sep = "")
      files <- paste0(myfile, c(".net", ".vec", ".clu"))
      graph2Pajek(values$cocitnet$graph, filename = myfile)
      # print(files)
      # print(dir())
      zip::zip(file, files)
    },
    contentType = "zip"
  )

  output$cocitTable <- renderUI({
    req(values$COCIT_ready > 0)
    cocitData = values$cocitnet$cluster_res
    names(cocitData) = c(
      "Node",
      "Cluster",
      "Betweenness",
      "Closeness",
      "PageRank"
    )
    renderBibliobox(
      cocitData,
      nrow = 10,
      filename = "CoCitation_Network",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 3:5,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  #### save coc network image as html ----
  output$networkCocit.fig <- downloadHandler(
    filename = "network.html",
    content <- function(con) {
      savenetwork(con, values$COCITnetwork$VIS)
    },
    contentType = "html"
  )

  ### Degree Plot Co-citation analysis ####
  output$cocitDegree <- renderPlotly({
    req(values$COCIT_ready > 0)
    #p <- degreePlot(values$cocitnet)
    plot.ly(values$degreePlot)
  })

  observeEvent(input$reportCOCIT, {
    if (!is.null(values$cocitnet$cluster_res)) {
      names(values$cocitnet$cluster_res) <- c(
        "Node",
        "Cluster",
        "Betweenness",
        "Closeness",
        "PageRank"
      )
      sheetname <- "CoCitNet"
      list_df <- list(values$cocitnet$cluster_res)
      list_plot <- list(values$degreePlot)
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      #values$wb <- res$wb
      values$wb <- addGgplotsWb(
        list_plot,
        wb = res$wb,
        res$sheetname,
        col = res$col + 15,
        width = 12,
        height = 8,
        dpi = 75
      )
      #values$fileTFP <- screenSh(selector = "#cocitPlot") ## screenshot
      values$fileCOCIT <- screenSh(
        values$COCITnetwork$VIS,
        zoom = 2,
        type = "vis"
      )
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$fileCOCIT, res$col)
      )
      popUp(title = "Co-citation Network", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### Historiograph ----
  observeEvent(input$applyHist, {
    ## Cache check: skip historiograph() if params unchanged
    hist_nodes <- input$histNodes
    hist_size <- input$histsize
    hist_isolates <- input$hist.isolates
    hist_labelsize <- input$histlabelsize
    hist_titlelabel <- input$titlelabel

    cache_key <- make_cache_key(
      fp = data_fingerprint(values$M),
      histNodes = hist_nodes,
      histsize = hist_size,
      hist.isolates = hist_isolates,
      histlabelsize = hist_labelsize,
      titlelabel = hist_titlelabel
    )

    if (identical(cache_key, values$cache_HIST_key)) {
      values$HIST_ready <- values$HIST_ready + 1
      return()
    }

    M_data <- values$M
    showNotification(
      "Historiograph: computing...",
      id = "HIST_progress",
      type = "message",
      duration = NULL
    )

    p <- promises::future_promise(
      {
        histResults <- bibliometrix::histNetwork(
          M_data,
          min.citations = 0,
          sep = ";"
        )
        if (!is.list(histResults) || is.null(histResults$histData)) {
          stop(
            "No direct citations found among the documents in the collection."
          )
        }
        histResults$histData <- histResults$histData %>%
          tibble::rownames_to_column(var = "SR")
        histPlot <- bibliometrix::histPlot(
          histResults,
          n = hist_nodes,
          size = hist_size,
          remove.isolates = (hist_isolates == "yes"),
          labelsize = hist_labelsize,
          label = hist_titlelabel,
          verbose = FALSE
        )
        if (is.null(histPlot$layout)) {
          stop(
            "No direct citations found among the documents in the collection."
          )
        }
        list(histResults = histResults, histPlot = histPlot)
      },
      seed = TRUE,
      packages = c(
        "bibliometrix",
        "dplyr",
        "tidyr",
        "tibble",
        "igraph",
        "ggplot2"
      )
    ) %...>%
      (function(res) {
        removeNotification("HIST_progress")
        histResults <- res$histResults
        histPlot <- res$histPlot

        histResults$histData$DOI <- paste0(
          '<a href=\"https://doi.org/',
          histResults$histData$DOI,
          '\" target=\"_blank\">',
          histResults$histData$DOI,
          "</a>"
        )

        histResults$histData <- histResults$histData %>%
          dplyr::left_join(
            histPlot$layout %>% dplyr::select(name, color),
            by = c("Paper" = "name")
          ) %>%
          tidyr::drop_na(color) %>%
          dplyr::mutate(cluster = match(color, unique(color))) %>%
          dplyr::select(!color) %>%
          dplyr::group_by(cluster) %>%
          dplyr::arrange(Year, .by_group = TRUE)

        values$histResults <- histResults
        values$histPlot <- histPlot
        values$histlog <- histPlot
        values$cache_HIST_key <- cache_key
        values$HIST_ready <- values$HIST_ready + 1
      }) %...!%
      (function(err) {
        removeNotification("HIST_progress")
        showNotification(
          paste("Historiograph error:", conditionMessage(err)),
          type = "error",
          duration = 8
        )
      })
    return(p)
  })

  output$histPlotVis <- renderVisNetwork({
    req(values$HIST_ready > 0)
    values$histPlotVis <- hist2vis(
      values$histPlot,
      curved = TRUE,
      labelsize = input$histlabelsize,
      nodesize = input$histsize,
      opacity = 0.7,
      shape = "dot",
      labeltype = input$titlelabel,
      timeline = FALSE
    )
    values$histPlotVis$VIS
  })

  output$histTable <- renderUI({
    req(values$HIST_ready > 0)
    Data <- values$histResults$histData
    renderBibliobox(
      Data,
      nrow = 10,
      filename = "Historiograph_Network",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE,
      summary = "historiograph"
    )
  })

  # gemini button for word network
  output$histGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$histGemini, values)
  })

  observeEvent(input$reportHIST, {
    if (!is.null(values$histResults$histData)) {
      sheetname <- "Historiograph"
      list_df <- list(values$histResults$histData)
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      #values$fileTFP <- screenSh(selector = "#histPlotVis") ## screenshot
      values$fileHIST <- screenSh(
        values$histPlotVis$VIS,
        zoom = 2,
        type = "vis"
      )
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$fileHIST, res$col)
      )
      popUp(title = "Historiograph", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  # SOCIAL STRUCTURE ####
  ### Collaboration network ----
  observeEvent(input$applyCol, {
    # Extract params
    n <- input$colNodes
    label.n <- input$colLabels
    col_field <- input$colField
    col_layout <- input$collayout
    col_curved <- (input$soc.curved == "Yes")
    col_labelsize <- input$collabelsize
    col_alpha <- input$colAlpha
    col_shape <- input$col.shape
    col_shadow <- (input$col.shadow == "Yes")
    col_noOverlap <- input$colNoOverlap
    col_normalize <- if (input$colnormalize == "none") {
      NULL
    } else {
      input$colnormalize
    }
    col_label_cex <- (input$collabel.cex == "Yes")
    col_edges_min <- input$coledges.min
    col_isolates <- (input$col.isolates == "yes")
    col_edgesize <- input$coledgesize
    col_cluster <- input$colCluster
    col_repulsion <- input$col.repulsion / 2
    col_filterMaxAuthors <- input$col.filterMaxAuthors

    if (label.n > n) {
      label.n <- n
    }
    layout_type <- if (col_layout == "worldmap") "auto" else col_layout

    # Synchronous: metaTagExtraction + biblioNetwork
    M_data <- values$M
    need_rebuild <- (dim(values$ColNetRefs)[1] == 1 |
      !(col_field == values$colField) |
      (dim(values$ColNetRefs)[1] != n))

    if (need_rebuild) {
      values$colField <- col_field
      if (!"nAU" %in% names(M_data)) {
        M_data$nAU <- stringr::str_count(M_data$AU, ";") + 1
        values$M <- M_data
      }
      ColNetRefs <- switch(
        col_field,
        COL_AU = {
          M_AU <- if (col_filterMaxAuthors) {
            M_data %>% dplyr::filter(nAU <= 20)
          } else {
            M_data
          }
          values$fieldCOL <- "AU"
          values$Title <- "Author Collaboration network"
          bibliometrix::biblioNetwork(
            M_AU,
            analysis = "collaboration",
            network = "authors",
            n = n,
            sep = ";"
          )
        },
        COL_UN = {
          if (!("AU_UN" %in% names(M_data))) {
            M_data <- bibliometrix::metaTagExtraction(
              M_data,
              Field = "AU_UN",
              sep = ";"
            )
            values$M <- M_data
          }
          values$fieldCOL <- "AU_UN"
          values$Title <- "Edu Collaboration network"
          bibliometrix::biblioNetwork(
            M_data,
            analysis = "collaboration",
            network = "universities",
            n = n,
            sep = ";"
          )
        },
        COL_CO = {
          if (!("AU_CO" %in% names(M_data))) {
            M_data <- bibliometrix::metaTagExtraction(
              M_data,
              Field = "AU_CO",
              sep = ";"
            )
            values$M <- M_data
          }
          values$fieldCOL <- "AU_CO"
          values$Title <- "Country Collaboration network"
          bibliometrix::biblioNetwork(
            M_data,
            analysis = "collaboration",
            network = "countries",
            n = n,
            sep = ";"
          )
        }
      )
      values$ColNetRefs <- ColNetRefs
    } else {
      ColNetRefs <- values$ColNetRefs
    }
    net_title <- values$Title
    fieldCOL <- values$fieldCOL

    showNotification(
      "Collaboration Network: computing...",
      id = "COL_progress",
      type = "message",
      duration = NULL
    )

    p <- promises::future_promise(
      {
        bibliometrix::networkPlot(
          ColNetRefs,
          normalize = col_normalize,
          Title = net_title,
          type = layout_type,
          size.cex = TRUE,
          size = 5,
          remove.multiple = FALSE,
          edgesize = col_edgesize * 3,
          labelsize = col_labelsize,
          label.cex = col_label_cex,
          curved = col_curved,
          label.n = label.n,
          edges.min = col_edges_min,
          label.color = FALSE,
          alpha = col_alpha,
          remove.isolates = col_isolates,
          cluster = col_cluster,
          community.repulsion = col_repulsion,
          verbose = FALSE
        )
      },
      seed = TRUE,
      packages = c("bibliometrix", "dplyr", "igraph", "ggplot2", "Matrix")
    ) %...>%
      (function(colnet) {
        removeNotification("COL_progress")

        # Add year info
        g <- colnet$graph
        Y <- authors2Years(M_data, fieldCOL)
        label_df <- data.frame(Item = igraph::V(g)$name)
        df <- label_df %>%
          dplyr::left_join(
            Y %>% dplyr::mutate(Item = tolower(Item)),
            by = "Item"
          ) %>%
          dplyr::rename(year_med = FirstYear)
        igraph::V(g)$year_med <- df$year_med
        colnet$graph <- g
        values$colnet <- colnet

        if (is.null(dim(colnet$cluster_res))) {
          values$colnet$cluster_res <- data.frame(
            Node = NA,
            Cluster = NA,
            Betweenness = NA,
            Closeness = NA,
            PageRank = NA
          )
        } else {
          names(values$colnet$cluster_res) <- c(
            "Node",
            "Cluster",
            "Betweenness",
            "Closeness",
            "PageRank"
          )
        }

        values$COLnetwork <- igraph2vis(
          g = colnet$graph,
          curved = col_curved,
          labelsize = col_labelsize,
          opacity = col_alpha,
          type = col_layout,
          shape = col_shape,
          net = colnet,
          shadow = col_shadow,
          noOverlap = col_noOverlap
        )
        values$colOverlay <- overlayPlotly(values$COLnetwork$VIS)
        values$degreePlot <- degreePlot(colnet)
        values$years_col <- sort(unique(c(
          M_data %>% tidyr::drop_na(PY) %>% dplyr::pull(PY)
        )))
        values$index_col <- 0
        values$playing_col <- FALSE
        values$paused_col <- FALSE
        values$current_year_col <- min(values$years_col)
        output$year_slider_colUI <- renderUI({
          sliderInput(
            "year_slider_col",
            "Year",
            min = min(values$years_col),
            max = max(values$years_col),
            value = min(values$years_col),
            step = 1,
            animate = FALSE,
            width = "100%",
            ticks = FALSE,
            round = TRUE,
            sep = ""
          )
        })
        values$COL_ready <- values$COL_ready + 1
      }) %...!%
      (function(err) {
        removeNotification("COL_progress")
        showNotification(
          paste("Collaboration Network error:", conditionMessage(err)),
          type = "error",
          duration = 8
        )
      })
    return(p)
  })
  output$colPlot <- renderVisNetwork({
    req(values$COL_ready > 0)
    values$COLnetwork$VIS
  })

  output$colOverlay <- renderPlotly({
    req(values$COL_ready > 0)
    values$colOverlay
  })

  ## Collaboration Network over Time ####
  observe({
    req(values$COLnetwork$VIS)
    invalidateLater(as.numeric(input$speed_col), session)
    isolate({
      # years <- sort(unique(c(values$COLnetwork$VIS$x$nodes$year_med)))
      if (
        values$playing_col &&
          !values$paused_col &&
          values$index_col < length(values$years_col)
      ) {
        values$index_col <- values$index_col + 1
        yr <- values$years_col[values$index_col]
        values$current_year_col <- yr
        updateSliderInput(
          session,
          "year_slider_col",
          value = yr,
          min = min(values$years_col),
          max = max(values$years_col)
        )
      }
    })
  })

  # funzione per creare la rete
  render_network_col <- reactive({
    req(values$COLnetwork$VIS)
    selected_year_col <- values$current_year_col
    show_nodes <- values$COLnetwork$VIS$x$nodes %>%
      dplyr::filter(year_med <= selected_year_col) %>%
      mutate(title = paste(title, year_med, sep = " "))
    show_edges <- values$COLnetwork$VIS$x$edges %>%
      dplyr::filter(from %in% show_nodes$id & to %in% show_nodes$id)

    coords <- show_nodes %>% select(x, y) %>% as.matrix()

    values$COLnetworkOverTime <- visNetwork::visNetwork(
      nodes = show_nodes,
      edges = show_edges,
      type = "full",
      smooth = TRUE,
      physics = FALSE
    ) %>%
      visNetwork::visNodes(
        shadow = TRUE,
        shape = "dot",
        font = list(
          color = show_nodes$font.color,
          size = show_nodes$font.size,
          vadjust = show_nodes$font.vadjust
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
        manipulation = FALSE,
        height = "100%",
        width = "100%"
      )

    values$COLnetworkOverTime
  })

  # aggiorna rete quando cambia lo slider
  observeEvent(input$year_slider_col, {
    values$current_year_col <- as.numeric(input$year_slider_col)
    output$colOverTime <- renderVisNetwork({
      render_network_col()
    })
  })

  output$colYearUI <- renderUI({
    req(values$current_year_col)
    h3(
      paste(watchEmoji(values$current_year_col), values$current_year_col),
      style = "text-align: left; color: #0e4770; font-weight: bold;"
    )
  })

  # start
  observeEvent(input$start_col, {
    values$index_col <- 0
    values$playing_col <- TRUE
    values$paused_col <- FALSE
    values$current_year_col <- min(values$years_col)
    updateSliderInput(session, "year_slider_col", value = min(values$years_col))
    output$colOverTime <- renderVisNetwork({
      render_network_col()
    })
  })

  # pausa
  observeEvent(input$pause_col, {
    values$paused_col <- !values$paused_col
    if (values$paused_col) {
      output$export_colUI <- renderUI({
        actionButton("export_col", "⬇ Export", width = "90%")
      })
    } else {
      output$export_colUI <- renderUI({})
    }
  })

  # reset
  observeEvent(input$reset_col, {
    values$playing_col <- FALSE
    values$paused_col <- FALSE
    values$index_col <- 0
    # years <- sort(unique(c(values$COCnetwork$VIS$x$nodes$year_med)))
    values$current_year_col <- min(values$years_col)
    updateSliderInput(session, "year_slider_col", value = min(values$years_col))
    output$colOverTime <- renderVisNetwork({
      nodes <- values$COLnetwork$VIS$x$nodes %>% dplyr::filter(year_med < 0)
      edges <- values$COLnetwork$VIS$x$edges %>%
        dplyr::filter(from %in% nodes$id)
      visNetwork(nodes = nodes, edges = edges)
    })
  })

  # gemini button for word network
  output$colGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$colGemini, values)
  })

  output$network.col <- downloadHandler(
    filename = function() {
      paste("Collaboration_network-", Sys.Date(), ".zip", sep = "")
    },
    content <- function(file) {
      tmpdir <- getWD()
      owd <- setwd(tmpdir)
      on.exit(setwd(owd))
      # print(tmpdir)
      #igraph::write.graph(values$obj$graph_pajek,file=file, format="pajek")
      myfile <- paste("mynetwork-", Sys.Date(), sep = "")
      files <- paste0(myfile, c(".net", ".vec", ".clu"))
      graph2Pajek(values$colnet$graph, filename = myfile)
      # print(files)
      # print(dir())
      zip::zip(file, files)
    },
    contentType = "zip"
  )

  output$colTable <- renderUI({
    req(values$COL_ready > 0)
    colData = values$colnet$cluster_res
    renderBibliobox(
      colData,
      nrow = 10,
      filename = "Collaboration_Network",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = 3:5,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  #### save coc network image as html ####
  output$networkCol.fig <- downloadHandler(
    filename = "network.html",
    content <- function(con) {
      savenetwork(con, values$COLnetwork$VIS)
    },
    contentType = "html"
  )

  ### Degree Plot Collaboration analysis ####
  output$colDegree <- renderPlotly({
    req(values$COL_ready > 0)
    p <- degreePlot(values$colnet)
    plot.ly(p)
  })

  observeEvent(input$reportCOL, {
    if (!is.null(values$colnet$cluster_res)) {
      sheetname <- "CollabNet"
      list_df <- list(values$colnet$params, values$colnet$cluster_res)
      list_plot <- list(values$degreePlot)
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      values$wb <- addGgplotsWb(
        list_plot,
        wb = res$wb,
        res$sheetname,
        col = res$col + 15,
        width = 12,
        height = 8,
        dpi = 75
      )
      #values$fileTFP <- screenSh(selector = "#colPlot") ## screenshot
      values$fileCOL <- screenSh(values$COLnetwork$VIS, zoom = 2, type = "vis")
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$fileCOL, res$col)
      )
      popUp(title = "Collaboration Network", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ### WPPlot ----
  observeEvent(input$applyWM, {
    M_data <- values$M
    wm_min_edges <- input$WMedges.min
    wm_edgesize <- input$WMedgesize * 2

    showNotification(
      "World Collaboration Map: computing...",
      id = "WM_progress",
      type = "message",
      duration = NULL
    )

    p <- promises::future_promise(
      {
        countrycollaboration_plotly(
          M_data,
          min.edges = wm_min_edges,
          edge_opacity = 0.4,
          edgesize = wm_edgesize,
          min_edgesize = 1
        )
      },
      seed = TRUE,
      globals = list(
        countrycollaboration_plotly = countrycollaboration_plotly,
        count.duplicates = count.duplicates,
        M_data = M_data,
        wm_min_edges = wm_min_edges,
        wm_edgesize = wm_edgesize
      ),
      packages = c("bibliometrix", "dplyr", "plotly", "igraph", "geosphere")
    ) %...>%
      (function(WMmap) {
        removeNotification("WM_progress")
        WMmap$tab <- WMmap$tab[, c(1, 2, 11)]
        names(WMmap$tab) <- c("From", "To", "Frequency")
        values$WMmap <- WMmap
        values$WM_ready <- values$WM_ready + 1
      }) %...!%
      (function(err) {
        removeNotification("WM_progress")
        showNotification(
          paste("World Map error:", conditionMessage(err)),
          type = "error",
          duration = 8
        )
      })
    return(p)
  })

  # gemini button for World Map
  output$WMGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$WMGemini, values)
  })

  observeEvent(input$CCplot.save, {
    screen2export(
      values$WMmap$g,
      filename = "CountryCollaborationMap",
      type = "plotly"
    )
  })

  output$WMPlot <- renderPlotly({
    req(values$WM_ready > 0)
    values$WMmap$g
  })

  output$WMTable <- renderUI({
    req(values$WM_ready > 0)
    colData = values$WMmap$tab
    renderBibliobox(
      colData,
      nrow = 10,
      filename = "Collaboration_WorldMap",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = TRUE,
      size = '100%',
      filter = "top",
      columnShort = NULL,
      columnSmall = NULL,
      round = 3,
      title = "",
      button = TRUE,
      escape = FALSE,
      selection = FALSE
    )
  })

  observeEvent(input$reportCOLW, {
    if (!is.null(values$WMmap$tab)) {
      list_df <- list(values$WMmap$tab)
      sheetname <- "CollabWorldMap"
      ind <- which(regexpr(sheetname, values$wb$sheet_names) > -1)
      if (length(ind) > 0) {
        sheetname <- paste(sheetname, "(", length(ind) + 1, ")", sep = "")
      }
      values$fileWMap <- screenSh(values$WMmap$g, zoom = 2, type = "plotly")
      values$list_file <- rbind(
        values$list_file,
        c(sheetname, values$fileWMap, 1)
      )
      #list_plot <- list(values$WMmap$g)
      wb <- addSheetToReport(
        list_df,
        list_plot = NULL,
        sheetname = sheetname,
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "Countries' Collaboration World Map", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  # CONTENT ANALYSIS ----
  content_analysis_server(input, output, session, values)

  # REPORT ----
  ### Report Save xlsx ----
  output$report.save <- downloadHandler(
    filename = function() {
      paste("BiblioshinyReport-", Sys.Date(), ".xlsx", sep = "")
    },
    content <- function(file) {
      wb_export <- copyWorkbook(values$wb)
      if (nrow(values$list_file) > 0) {
        wb_export <- addScreenWb(df = values$list_file, wb = wb_export) #, width=10, height=7, dpi=300)
      }
      sheetToRemove <- setdiff(sheets(wb_export), input$reportSheets)
      if (length(sheetToRemove) > 0) {
        for (i in sheetToRemove) {
          removeWorksheet(wb_export, i)
        }
      }
      sheetToAdd <- sheets(wb_export)
      for (i in sheetToAdd) {
        setColWidths(
          wb_export,
          sheet = i,
          cols = 1,
          widths = 30,
          hidden = FALSE
        )
      }
      openxlsx::saveWorkbook(wb_export, file = file)
    },
    contentType = "xlsx"
  )

  ### Report UI elements
  observe({
    output$reportSheets <- renderUI({
      prettyCheckboxGroup(
        inputId = "reportSheets",
        label = NULL, #short2long(df=values$dfLabel, myC=values$myChoices),
        choices = short2long(df = values$dfLabel, myC = values$myChoices),
        selected = values$myChoices,
        icon = icon("check"),
        animation = "pulse",
        status = "primary",
        bigger = T,
        fill = TRUE
      )
    })
  })

  observe({
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "reportSheets",
      #label = short2long(df=values$dfLabel, myC=values$myChoices),
      choices = short2long(df = values$dfLabel, myC = values$myChoices),
      selected = if (!input$noSheets) values$myChoices,
      prettyOptions = list(
        animation = "pulse",
        status = "info",
        bigger = T
      )
    )
  })

  observe({
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "reportSheets",
      choices = short2long(df = values$dfLabel, myC = values$myChoices),
      selected = if (input$allSheets) values$myChoices,
      prettyOptions = list(
        animation = "pulse",
        status = "info",
        bigger = T
      )
    )
  })

  observeEvent(input$deleteAll, {
    ask_confirmation(
      inputId = "delete_confirmation",
      title = "Want to confirm?",
      text = "All the results will be removed from the report",
      type = "warning",
      btn_labels = c("CANCEL", "CONFIRM"),
    )
  })

  observeEvent(
    input$delete_confirmation,
    {
      if (isTRUE(input$delete_confirmation)) {
        values$myChoices <- "Empty Report"
        values$list_file <- data.frame(sheet = NULL, file = NULL, n = NULL)
        values$wb <- openxlsx::createWorkbook()
      }
    },
    ignoreNULL = TRUE
  )

  ### screenshot buttons ----
  JScode_screenshot <- "
    var link = document.createElement('a');
    link.href = '%s';
    link.download = '%s';
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
  "

  observeEvent(input$missingDataSave, {
    screen2export(
      values$missingdf_formatted,
      filename = "missingDataTable",
      type = "df2html"
    )
  })

  observeEvent(input$screenTFP, {
    screen2export(
      values$TFP,
      filename = "ThreeFieldPlot",
      type = "plotly"
    )
  })

  observeEvent(input$screenWC, {
    screen2export(
      obj = values$WordCloud,
      filename = "WordCloud",
      type = "plotly"
    )
  })

  observeEvent(input$screenTREEMAP, {
    screen2export(
      obj = values$WTreeMap,
      filename = "TreeMap",
      type = "plotly"
    )
  })

  observeEvent(input$screenCOC, {
    screen2export(
      obj = values$COCnetwork$VIS,
      filename = "Co_occurrenceNetwork",
      type = "vis"
    )
  })

  observeEvent(input$export_coc, {
    screen2export(
      obj = values$COCnetworkOverTime,
      filename = paste0(
        "Co-occurence_Network-Year-",
        values$current_year_col,
        "_"
      ),
      type = "vis"
    )
  })

  observeEvent(input$screenCOCIT, {
    screen2export(
      obj = values$COCITnetwork$VIS,
      filename = "Co_citationNetwork",
      type = "vis"
    )
  })

  observeEvent(input$screenHIST, {
    screen2export(
      obj = values$histPlotVis$VIS,
      filename = "Historiograph",
      type = "vis"
    )
  })

  observeEvent(input$screenCOL, {
    screen2export(
      obj = values$COLnetwork$VIS,
      filename = "Collaboration_Network",
      type = "vis"
    )
  })

  observeEvent(input$export_col, {
    screen2export(
      obj = values$COLnetworkOverTime,
      filename = paste0(
        "Collaboration_Network-Year-",
        values$current_year_col,
        "_"
      ),
      type = "vis"
    )
  })

  ## TALL EXPORT ----
  observe({
    req(values$M)
    ind <- which(values$corpusCol %in% names(values$M))
    corpusCol <- values$corpusCol[ind]
    updateMultiInput(
      session,
      'tallFields',
      selected = character(0),
      choices = names(corpusCol)
    )
  })

  observe({
    req(values$M)
    ind <- which(values$metadataCol %in% names(values$M))
    metadataCol <- values$metadataCol[ind]
    updateMultiInput(
      session,
      'tallMetadata',
      selected = character(0),
      choices = names(metadataCol)
    )
  })

  observeEvent(
    eventExpr = {
      input$tallRun
    },
    handlerExpr = {
      req(input$tallFields)
      values$tallDf <- tallExport(
        values$M,
        input$tallFields,
        input$tallMetadata,
        values$metadataCol
      )
    }
  )

  output$tallTable <- renderUI({
    req(values$tallDf)
    renderBibliobox(
      values$tallDf,
      nrow = 3,
      filename = "tallDf",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = FALSE,
      size = '70%',
      filter = "none",
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

  output$tall.save <- downloadHandler(
    filename = function() {
      paste("tallFile-", Sys.Date(), ".csv", sep = "")
    },
    content <- function(file) {
      write.csv(values$tallDf, file = file, row.names = FALSE)
    },
    contentType = "csv"
  )

  observeEvent(
    eventExpr = {
      values$TALLmissing
    },
    handlerExpr = {
      if (values$TALLmissing) {
        output$tallBttn1 <- renderUI({
          div(
            style = "border-radius: 10px; border-width: 3px; font-size: 15px;",
            align = "center",
            width = "100%",
            actionBttn(
              inputId = "installTall",
              label = strong("Install TALL"),
              width = "100%",
              style = "pill",
              color = "danger",
              icon = icon(name = "cloud-download", lib = "glyphicon")
            )
          )
        })
        output$tallBttn2 <- renderUI("")
      } else {
        output$tallBttn1 <- renderUI("")
        output$tallBttn2 <- renderUI({
          if (require("tall", quietly = TRUE)) {
            div(
              style = "border-radius: 10px; border-width: 3px; font-size: 15px;",
              align = "center",
              width = "100%",
              actionBttn(
                inputId = "launchTall",
                label = strong("Launch TALL"),
                width = "100%",
                style = "pill",
                color = "primary",
                icon = icon(name = "play", lib = "glyphicon")
              )
            )
          }
        })
      }
    }
  )

  observeEvent(
    eventExpr = {
      input$installTall
    },
    handlerExpr = {
      pak::pkg_install("tall")
      popUpGeneric(
        title = NULL,
        type = "success",
        color = c("#1d8fe1"),
        subtitle = "TALL has been successfully installed",
        btn_labels = "OK",
        size = "40%"
      )
      if (suppressMessages(require("tall", quietly = TRUE))) {
        values$TALLmissing <- FALSE
      }
    }
  )

  observeEvent(
    eventExpr = {
      input$launchTall
    },
    handlerExpr = {
      system('Rscript -e "tall::tall()"')
    }
  )

  ## SETTING ----
  observeEvent(input$dpi, {
    values$dpi <- as.numeric(input$dpi)
  })

  observeEvent(input$h, {
    values$h <- as.numeric(input$h)
  })

  # Server code for randomize seed button
  observeEvent(input$randomize_seed, {
    # Generate a random seed between 1 and 999999
    new_seed <- sample(1:999999, 1)

    # Update the random_seed input with the new value
    updateNumericInput(session, "random_seed", value = new_seed)

    # Optional: Show notification
    showNotification(
      paste("Random seed set to:", new_seed),
      type = "message",
      duration = 2
    )
  })

  # Optional: Observer to apply the seed globally when it changes
  observe({
    req(input$random_seed)

    # Set the global random seed for reproducibility
    set.seed(input$random_seed)

    # Store in reactive values for access across the app
    values$random_seed <- input$random_seed
  })

  output$apiStatus <- renderUI({
    if (values$geminiAPI) {
      last <- showGeminiAPI()
      output$status <- renderText(paste0("✅ API key has been set: ", last))
    }
  })

  output$geminiOutputSize <- renderUI({
    list(
      selectInput(
        inputId = "gemini_output_size",
        label = "Max Output (in tokens)",
        selected = ifelse(
          is.null(values$gemini_output_size),
          "medium",
          values$gemini_output_size
        ),
        choices = c("Medium" = "medium", "Large" = "large")
      ),
      conditionalPanel(
        condition = "input.gemini_output_size == 'medium'",
        helpText(strong("Free Tier Output:")),
        helpText(em("Medium -> 16384 Tokens"))
      ),
      conditionalPanel(
        condition = "input.gemini_output_size == 'large'",
        helpText(strong("Free Tier Output:")),
        helpText(em("Large -> 32768 Tokens"))
      )
    )
  })

  output$geminiModelChoice <- renderUI({
    list(
      selectInput(
        inputId = "gemini_api_model",
        label = "Select the Gemini Model",
        choices = c(
          "Gemini 2.5 Flash" = "2.5-flash",
          "Gemini 2.5 Flash Lite" = "2.5-flash-lite",
          "Gemini 3.0 Flash" = "3-flash-preview",
          "Gemini 3.0 Pro" = "3-pro-preview"
          # "Gemini 2.0 Flash Lite" = "2.0-flash-lite",
          # "Gemini 1.5 Flash" = "1.5-flash",
          # "Gemini 1.5 Flash Lite" = "1.5-flash-8b"
        ),
        selected = ifelse(
          is.null(values$gemini_api_model),
          "2.5-flash",
          values$gemini_api_model
        )
      ),
      conditionalPanel(
        condition = "input.gemini_api_model == '2.5-flash'",
        helpText(strong("Free Tier Rate Limits:")),
        helpText(em(
          "Request per Minutes: 10",
          tags$br(),
          "Requests per Day: 500",
          tags$br(),
          "Latency time: High"
        ))
      ),
      conditionalPanel(
        condition = "input.gemini_api_model == '2.5-flash-lite'",
        helpText(strong("Free Tier Rate Limits:")),
        helpText(em(
          "Request per Minutes: 15",
          tags$br(),
          "Requests per Day: 500",
          tags$br(),
          "Latency time: Low"
        ))
      )
    )
  })

  observeEvent(input$gemini_api_model, {
    if (!is.null(input$gemini_api_model)) {
      saveGeminiModel(
        model = c(input$gemini_api_model, input$gemini_output_model),
        file = paste0(homeFolder(), "/.biblio_gemini_model.txt", collapse = "")
      )
      values$gemini_api_model <- input$gemini_api_model
      values$gemini_output_size <- input$gemini_output_size
    }
  })

  observeEvent(input$gemini_output_size, {
    if (!is.null(input$gemini_output_size)) {
      saveGeminiModel(
        model = c(input$gemini_api_model, input$gemini_output_size),
        file = paste0(homeFolder(), "/.biblio_gemini_model.txt", collapse = "")
      )
      values$gemini_api_model <- input$gemini_api_model
      values$gemini_output_size <- input$gemini_output_size
    }
  })

  observeEvent(input$set_key, {
    key <- input$api_key

    # Show validating message
    output$apiStatus <- renderUI({
      output$status <- renderText("Validating API key...")
    })

    # Async: validate API key in background
    promises::future_promise(
      {
        setGeminiAPI(key)
      },
      seed = TRUE
    ) %...>%
      (function(last) {
        if (!last$valid) {
          output$apiStatus <- renderUI({
            output$status <- renderText(last$message)
          })
          values$geminiAPI <- FALSE
        } else {
          output$apiStatus <- renderUI({
            output$status <- renderText(paste0(
              "✅ API key has been set: ",
              last$message
            ))
          })
          values$geminiAPI <- TRUE
          home <- homeFolder()
          path_gemini_key <- paste0(
            home,
            "/.biblio_gemini_key.txt",
            collapse = ""
          )
          Sys.setenv(GEMINI_API_KEY = key)
          writeLines(key, path_gemini_key)
        }
      }) %...!%
      (function(err) {
        output$apiStatus <- renderUI({
          output$status <- renderText(paste(
            "Error validating key:",
            conditionMessage(err)
          ))
        })
        values$geminiAPI <- FALSE
      })
  })

  observeEvent(input$remove_key, {
    if (values$geminiAPI) {
      home <- homeFolder()
      path_gemini_key <- paste0(home, "/.biblio_gemini_key.txt", collapse = "")
      file.remove(path_gemini_key)
      values$geminiAPI <- FALSE
      output$apiStatus <- renderUI({
        output$status <- renderText(paste0("❌ API key has been removed"))
      })
    }
  })

  # ============================================
  # OpenAlex API Key
  # ============================================

  # Show API key status on load
  output$oaApiKeyStatus <- renderUI({
    if (!is.null(values$oaApiKey) && nchar(values$oaApiKey) >= 10) {
      masked_key <- paste0("...", substr(values$oaApiKey, nchar(values$oaApiKey) - 3, nchar(values$oaApiKey)))
      div(
        style = "color: #155724; background-color: #d4edda; padding: 8px 12px; border-radius: 4px; font-size: 13px;",
        icon("check-circle"),
        sprintf(" API key active: %s", masked_key)
      )
    } else {
      div(
        style = "color: #856404; background-color: #fff3cd; padding: 8px 12px; border-radius: 4px; font-size: 13px;",
        icon("exclamation-triangle"),
        " No API key set. Limited to 100 credits/day (testing only)."
      )
    }
  })

  # Pre-fill API key input if already saved
  observeEvent(
    TRUE,
    {
      if (!is.null(values$oaApiKey) && nchar(values$oaApiKey) >= 10) {
        updateTextInput(session, "oaApiKey", value = values$oaApiKey)
      }
    },
    once = TRUE
  )

  # Save API key
  observeEvent(input$oaSetApiKey, {
    key <- trimws(input$oaApiKey)
    if (is.null(key) || nchar(key) < 10) {
      output$oaApiKeyStatus <- renderUI({
        div(
          style = "color: #721c24; background-color: #f8d7da; padding: 8px 12px; border-radius: 4px; font-size: 13px;",
          icon("times-circle"),
          " Please enter a valid API key (at least 10 characters)."
        )
      })
      return()
    }

    home <- homeFolder()
    path_oa_apikey <- file.path(home, ".biblio_openalex_apikey.txt")
    writeLines(key, path_oa_apikey)
    Sys.setenv(openalexR.apikey = key)
    options(openalexR.apikey = key)
    values$oaApiKey <- key

    masked_key <- paste0("...", substr(key, nchar(key) - 3, nchar(key)))
    output$oaApiKeyStatus <- renderUI({
      div(
        style = "color: #155724; background-color: #d4edda; padding: 8px 12px; border-radius: 4px; font-size: 13px;",
        icon("check-circle"),
        sprintf(" API key saved: %s", masked_key)
      )
    })
  })

  # Remove API key
  observeEvent(input$oaRemoveApiKey, {
    home <- homeFolder()
    path_oa_apikey <- file.path(home, ".biblio_openalex_apikey.txt")
    if (file.exists(path_oa_apikey)) {
      file.remove(path_oa_apikey)
    }
    Sys.unsetenv("openalexR.apikey")
    options(openalexR.apikey = NULL)
    values$oaApiKey <- NULL
    updateTextInput(session, "oaApiKey", value = "")

    output$oaApiKeyStatus <- renderUI({
      div(
        style = "color: #856404; background-color: #fff3cd; padding: 8px 12px; border-radius: 4px; font-size: 13px;",
        icon("exclamation-triangle"),
        " API key removed. Limited to 100 credits/day (testing only)."
      )
    })
  })

  # ============================================
  # OpenAlex Polite Pool Email
  # ============================================

  # Show status on load
  output$oaEmailStatus <- renderUI({
    if (!is.null(values$oaPoliteEmail) && nchar(values$oaPoliteEmail) >= 5) {
      div(
        style = "color: #155724; background-color: #d4edda; padding: 8px 12px; border-radius: 4px; font-size: 13px;",
        icon("check-circle"),
        sprintf(" Polite pool active: %s", values$oaPoliteEmail)
      )
    } else {
      div(
        style = "color: #856404; background-color: #fff3cd; padding: 8px 12px; border-radius: 4px; font-size: 13px;",
        icon("exclamation-triangle"),
        " No email set. Using common pool (slower rate limits)."
      )
    }
  })

  # Pre-fill email input if already saved
  observeEvent(
    TRUE,
    {
      if (!is.null(values$oaPoliteEmail) && nchar(values$oaPoliteEmail) >= 5) {
        updateTextInput(session, "oaPoliteEmail", value = values$oaPoliteEmail)
      }
    },
    once = TRUE
  )

  # Save email
  observeEvent(input$oaSetEmail, {
    email <- trimws(input$oaPoliteEmail)
    if (is.null(email) || !grepl("^[^@]+@[^@]+\\.[^@]+$", email)) {
      output$oaEmailStatus <- renderUI({
        div(
          style = "color: #721c24; background-color: #f8d7da; padding: 8px 12px; border-radius: 4px; font-size: 13px;",
          icon("times-circle"),
          " Please enter a valid email address."
        )
      })
      return()
    }

    home <- homeFolder()
    path_oa_email <- file.path(home, ".biblio_openalex_email.txt")
    writeLines(email, path_oa_email)
    Sys.setenv(openalexR.mailto = email)
    options(openalexR.mailto = email)
    values$oaPoliteEmail <- email

    output$oaEmailStatus <- renderUI({
      div(
        style = "color: #155724; background-color: #d4edda; padding: 8px 12px; border-radius: 4px; font-size: 13px;",
        icon("check-circle"),
        sprintf(" Email saved. Polite pool active: %s", email)
      )
    })
  })

  # Remove email
  observeEvent(input$oaRemoveEmail, {
    home <- homeFolder()
    path_oa_email <- file.path(home, ".biblio_openalex_email.txt")
    if (file.exists(path_oa_email)) {
      file.remove(path_oa_email)
    }
    Sys.unsetenv("openalexR.mailto")
    options(openalexR.mailto = NULL)
    values$oaPoliteEmail <- NULL
    updateTextInput(session, "oaPoliteEmail", value = "")

    output$oaEmailStatus <- renderUI({
      div(
        style = "color: #856404; background-color: #fff3cd; padding: 8px 12px; border-radius: 4px; font-size: 13px;",
        icon("exclamation-triangle"),
        " Email removed. Using common pool (slower rate limits)."
      )
    })
  })
}

# END ####
