### packages for biblishiny()
libraries <- function(){
  if (!(require(bibliometrix))){install.packages("bibliometrix"); require(bibliometrix)}
  #if (!(require(badger, quietly=TRUE))){install.packages("badger"); require(badger, quietly=TRUE)} 
  suppressPackageStartupMessages({
    if (!(require(shiny, quietly=TRUE))){install.packages("shiny"); require(shiny, quietly=TRUE)} 
    if (!(require(igraph, quietly=TRUE))){install.packages("igraph"); require(igraph, quietly=TRUE)} 
    if (!(require(DT, quietly=TRUE))){install.packages("DT")};require(DT, quietly =TRUE)
    if (!(require(ggplot2, quietly=TRUE))){install.packages("ggplot2"); require(ggplot2, quietly=TRUE)} 
    if (!(require(shinycssloaders, quietly=TRUE))){install.packages("shinycssloaders")} 
    if (!(require(wordcloud2, quietly=TRUE))){install.packages("wordcloud2")} 
    if (!require(ggmap, quietly=TRUE)){install.packages("ggmap"); require(ggmap, quietly=TRUE)}
    if (!require(maps, quietly=TRUE)){install.packages("maps"); require(maps, quietly=TRUE)}
    if (!require(visNetwork, quietly=TRUE)){install.packages("visNetwork"); require(visNetwork, quietly=TRUE)}
    if (!require(plotly, quietly=TRUE)){install.packages("plotly"); require(plotly, quietly=TRUE)}
    if (!require(fontawesome, quietly=TRUE)){install.packages("fontawesome"); require(fontawesome, quietly=TRUE)}
    if (!require(shinydashboardPlus, quietly=TRUE)){install.packages("shinydashboardPlus"); require(shinydashboardPlus, quietly=TRUE)}
    if (!require(shinydashboard, quietly=TRUE)){install.packages("shinydashboard"); require(shinydashboard, quietly=TRUE)}
    if (!require(shinyjs, quietly=TRUE)){install.packages("shinyjs"); require(shinyjs, quietly=TRUE)}
    if (!require(RCurl, quietly=TRUE)){install.packages("RCurl")}
    if (!require(openxlsx, quietly=TRUE)){install.packages("openxlsx"); require(openxlsx, quietly=TRUE)}
    if (!require(shinyWidgets, quietly=TRUE)){install.packages("shinyWidgets"); require(shinyWidgets, quietly=TRUE)}
    
    ## Currently "webshot2" 0.1.1 generates empty screenshots on windows 10 for graphics created with visnetwork.
    ## This workaround installs the previous version 0.1.0 to temporarily fix the problem.
    if (!require(webshot2,quietly=TRUE)){
      install.packages("https://cran.r-project.org/src/contrib/Archive/webshot2/webshot2_0.1.0.tar.gz", repos = NULL, type = "source")
    }else{
      pkgs <- installed.packages()[, "Version"]
      vers <- pkgs["webshot2"]
      if (vers!="0.1.0"){
        detach("package:webshot2", unload = TRUE, force=TRUE)
        install.packages("https://cran.r-project.org/src/contrib/Archive/webshot2/webshot2_0.1.0.tar.gz", repos = NULL, type = "source")
      }
    }
    ##
    
    if (!(require(chromote, quietly=TRUE))){install.packages("chromote"); require(chromote, quietly=TRUE)}
    
    ### workaround for webshot2 on shinyapps.io
    if (!(require(curl, quietly=TRUE))){install.packages("curl"); require(curl, quietly=TRUE)}
    if (!(require(pagedown, quietly=TRUE))){install.packages("pagedown"); require(pagedown, quietly=TRUE)}
    ##
    require(Matrix, quietly = TRUE)
    require(dimensionsR, quietly = TRUE)
    require(pubmedR, quietly = TRUE)
    require(dplyr, quietly = TRUE)
    require(tidyr, quietly = TRUE)
    
    # packages not automatically downloaded by visNetwork
    if (!require(sparkline, quietly=TRUE)){install.packages("sparkline")}#; require(sparkline, quietly=TRUE)}
    if (!require(tidygraph, quietly=TRUE)){install.packages("tidygraph")}#; require(tidygraph, quietly=TRUE)}
    if (!require(ggraph, quietly=TRUE)){install.packages("ggraph")}#; require(tidygraph, quietly=TRUE)}
  })
}

messageItem2 <- function (from, message, icon = shiny::icon("user"), time = NULL, 
                          href = NULL, inputId = NULL){
  if (is.null(href)) 
    href <- "#"
  shiny::tags$li(shiny::a(id = inputId, class = if (!is.null(inputId)) 
    "action-button", href = href, target = "_blank", icon, shiny::h4(from, if (!is.null(time)) 
      shiny::tags$small(shiny::icon("clock-o"), time)), shiny::p(message)))
}