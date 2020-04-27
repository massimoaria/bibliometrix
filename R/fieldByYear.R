#' Field Tag distribution by Year
#'
#' It calculates the median year for each item of a field tag. 
#' @param M is a bibliographic data frame obtained by \code{\link{convert2df}} function.
#' @param field is a character object. It indicates one of the field tags of the
#'   standard ISI WoS Field Tag codify.
#' @param timespan is a vector with the min and max year. If it is = NULL, the analysis is performed on the entire period. Default is \code{timespan = NULL}.
#' @param min.freq is an integer. It indicates the min frequency of the items to include in the analysis
#' @param n.items is an integer. I indicates the maximun number of items per year to include in the plot.
#' @param labelsize is an integer. It indicates the label size in the plot. Default is \code{labelsize=5}.
#' @param graph is logical. If TRUE the function plots Filed Tag distribution by Year graph. Default is \code{graph = TRUE}.
#' @return The function \code{fieldByYear} returns a list containing threeobjects:
#' \tabular{lll}{
#' \code{df}  \tab   \tab is a data frame\cr
#' \code{df_graph}\tab    \tab is a data frame with data used to build the graph\cr
#' \code{graph}   \tab   \tab a ggplot object}
#'
#' @examples
#' data(management)
#' timespan=c(2005,2015)
#' res <- fieldByYear(management, field = "ID", timespan = timespan, 
#'                     min.freq = 5, n.items = 5, graph = TRUE)

#'
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis
#' @seealso \code{\link{summary}} method for class '\code{bibliometrix}'
#' 
#' @export
#' 

fieldByYear <- function(M,
                        field = "ID",
                        timespan = NULL,
                        min.freq = 1,
                        n.items = 5,
                        labelsize = 5,
                        graph = TRUE) {
  
  A <- cocMatrix(M, Field = field, binary = FALSE)
  n <- colSums(A)
  #A=tdIdf(A)
  med <- apply(A, 2, function(x) {
    round(median(rep(M$PY, x)))
  })
  
  med <-
    data.frame(
      item = names(med),
      freq = as.numeric(n),
      year = as.numeric(med),
      stringsAsFactors = FALSE
    )
  
  # if timespan is null, timespan is set to the whole period
  if (is.null(timespan) | length(timespan)!=2){
    timespan=as.numeric(range(med$year, na.rm = TRUE))
  }
  
  df <- med %>%
    arrange(desc(.data$freq)) %>%
    dplyr::filter(.data$freq >= min.freq) %>%
    dplyr::filter(.data$year >= timespan[1] & .data$year <= timespan[2]) %>%
    group_by(.data$year) %>%
    top_n(n.items, .data$freq)
  
  g <- ggplot(df, aes(x = df$year, y = log(df$freq))) +
    geom_point(color = adjustcolor("royalblue4", alpha.f = 0.5), size = 1) +
    geom_text_repel(aes(label = tolower(df$item)), size=labelsize, color=adjustcolor("royalblue4", alpha.f = 0.7)) +
    scale_x_continuous(breaks = seq(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE), by = 2)) +
    ylab("log(frequency)") +
    xlab("year") +
    labs(title= "Trend Topics") +
    theme(
      legend.position = 'right'
      ,
      text = element_text(color = "#444444")
      ,
      panel.background = element_rect(fill = 'gray97')
      ,
      panel.grid.minor = element_line(color = '#FFFFFF')
      ,
      panel.grid.major = element_line(color = '#FFFFFF')
      ,
      plot.title = element_text(size = 24)
      ,
      axis.title = element_text(size = 14, color = '#555555')
      ,
      axis.title.y = element_text(
        vjust = 1,
        angle = 90,
        face = "bold"
      )
      ,
      axis.title.x = element_text(hjust = .95, face = "bold")
      ,
      axis.text.x = element_text(face = "bold")
      ,
      axis.text.y = element_text(face = "bold")
    )
  
  if (isTRUE(graph)) {
    plot(g)
  }
  
  results <- list(df = med,
                 df_graph = df,
                 graph = g)
  
  return(results)
  
}

  