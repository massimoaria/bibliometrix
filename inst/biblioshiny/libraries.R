### packages for biblishiny()
libraries <- function(){
  if (!(require(bibliometrix))){install.packages("bibliometrix"); require(bibliometrix, quietly=TRUE)}
  if (!(require(shiny))){install.packages("shiny"); require(shiny, quietly=TRUE)} 
  if (!(require(igraph))){install.packages("igraph"); require(igraph, quietly=TRUE)} 
  if (!(require(DT))){install.packages("DT")};require(DT, quietly =TRUE)
  if (!(require(ggplot2))){install.packages("ggplot2"); require(ggplot2, quietly=TRUE)} 
  if (!(require(shinycssloaders))){install.packages("shinycssloaders")} 
  if (!(require(wordcloud2))){install.packages("wordcloud2")} 
  if (!require(ggmap)){install.packages("ggmap"); require(ggmap, quietly=TRUE)}
  if (!require(maps)){install.packages("maps"); require(maps, quietly=TRUE)}
  if (!require(visNetwork)){install.packages("visNetwork"); require(visNetwork, quietly=TRUE)}
  if (!require(plotly)){install.packages("plotly"); require(plotly, quietly=TRUE)}
  if (!require(fontawesome)){install.packages("fontawesome"); require(fontawesome, quietly=TRUE)}
  #if (!require(dashboardthemes)){install.packages("dashboardthemes"); require(dashboardthemes, quietly=TRUE)}
  if (!require(shinydashboardPlus)){install.packages("shinydashboardPlus"); require(shinydashboardPlus, quietly=TRUE)}
  if (!require(shinydashboard)){install.packages("shinydashboard"); require(shinydashboard, quietly=TRUE)}
  if (!require(shinyjs)){install.packages("shinyjs"); require(shinyjs, quietly=TRUE)}
  if (!require(shinyscreenshot)){install.packages("shinyscreenshot"); require(shinyscreenshot, quietly=TRUE)}
  if (!require(openxlsx)){install.packages("openxlsx"); require(openxlsx, quietly=TRUE)}
  #if (!require(shinyalert)){install.packages("shinyalert"); require(shinyalert, quietly=TRUE)}
  if (!require(shinyWidgets)){install.packages("shinyWidgets"); require(shinyWidgets, quietly=TRUE)}
  require(Matrix, quietly = TRUE)
  require(dimensionsR, quietly = TRUE)
  require(pubmedR, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  require(tidyr, quietly = TRUE)
}

messageItem2 <- function (from, message, icon = shiny::icon("user"), time = NULL, 
                          href = NULL, inputId = NULL){
  if (is.null(href)) 
    href <- "#"
  shiny::tags$li(shiny::a(id = inputId, class = if (!is.null(inputId)) 
    "action-button", href = href, target = "_blank", icon, shiny::h4(from, if (!is.null(time)) 
      shiny::tags$small(shiny::icon("clock-o"), time)), shiny::p(message)))
}