### packages for biblishiny()
libraries <- function() {
  if (!require(pak, quietly = TRUE)) {
    install.packages("pak")
    require(pak, quietly = TRUE)
  }
  if (!require(httr2, quietly = TRUE)) {
    install.packages("httr2")
    require(httr2, quietly = TRUE)
  }
  if (!(require(base64enc))) {
    pkg_install("base64enc")
    require(base64enc)
  }
  if (!(require(bibliometrix))) {
    pkg_install("bibliometrix")
    require(bibliometrix)
  }
  if (!(require(zip, quietly = TRUE))) {
    pkg_install("zip")
  }
  suppressPackageStartupMessages({
    if (!(require(shiny, quietly = TRUE))) {
      pkg_install("shiny")
      require(shiny, quietly = TRUE)
    }
    if (!(require(igraph, quietly = TRUE))) {
      pkg_install("igraph")
      require(igraph, quietly = TRUE)
    }
    if (!(require(DT, quietly = TRUE))) {
      pkg_install("DT")
    }
    require(DT, quietly = TRUE)
    if (!(require(ggplot2, quietly = TRUE))) {
      pkg_install("ggplot2")
      require(ggplot2, quietly = TRUE)
    }
    if (!(require(shinycssloaders, quietly = TRUE))) {
      pkg_install("shinycssloaders")
    }
    if (!(require(wordcloud2, quietly = TRUE))) {
      pkg_install("wordcloud2")
    }
    if (!require(ggmap, quietly = TRUE)) {
      pkg_install("ggmap")
      require(ggmap, quietly = TRUE)
    }
    if (!require(maps, quietly = TRUE)) {
      pkg_install("maps")
      require(maps, quietly = TRUE)
    }
    if (!require(visNetwork, quietly = TRUE)) {
      pkg_install("visNetwork")
      require(visNetwork, quietly = TRUE)
    }
    if (!require(plotly, quietly = TRUE)) {
      pkg_install("plotly")
      require(plotly, quietly = TRUE)
    }
    if (!require(fontawesome, quietly = TRUE)) {
      pkg_install("fontawesome")
      require(fontawesome, quietly = TRUE)
    }
    if (!require(shinydashboardPlus, quietly = TRUE)) {
      pkg_install("shinydashboardPlus")
      require(shinydashboardPlus, quietly = TRUE)
    }
    if (!require(shinydashboard, quietly = TRUE)) {
      pkg_install("shinydashboard")
      require(shinydashboard, quietly = TRUE)
    }
    if (!require(shinyjs, quietly = TRUE)) {
      pkg_install("shinyjs")
      require(shinyjs, quietly = TRUE)
    }
    if (!require(RCurl, quietly = TRUE)) {
      pkg_install("RCurl")
    }
    if (!require(openxlsx, quietly = TRUE)) {
      pkg_install("openxlsx")
      require(openxlsx, quietly = TRUE)
    }
    if (!require(shinyWidgets, quietly = TRUE)) {
      pkg_install("shinyWidgets")
      require(shinyWidgets, quietly = TRUE)
    }

    if (!(require(chromote, quietly = TRUE))) {
      pkg_install("chromote")
      require(chromote, quietly = TRUE)
    }

    ### workaround for webshot2 on shinyapps.io
    if (!(require(curl, quietly = TRUE))) {
      pkg_install("curl")
      require(curl, quietly = TRUE)
    }
    if (!(require(pagedown, quietly = TRUE))) {
      pkg_install("pagedown")
      require(pagedown, quietly = TRUE)
    }
    ##
    require(Matrix, quietly = TRUE)
    require(dimensionsR, quietly = TRUE)
    require(pubmedR, quietly = TRUE)
    require(dplyr, quietly = TRUE)
    require(tidyr, quietly = TRUE)

    # packages not automatically downloaded by visNetwork
    if (!require(sparkline, quietly = TRUE)) {
      pkg_install("sparkline")
    } # ; require(sparkline, quietly=TRUE)}
    if (!require(tidygraph, quietly = TRUE)) {
      pkg_install("tidygraph")
    } # ; require(tidygraph, quietly=TRUE)}
    if (!require(ggraph, quietly = TRUE)) {
      pkg_install("ggraph")
    } # ; require(tidygraph, quietly=TRUE)}
  })
}

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
