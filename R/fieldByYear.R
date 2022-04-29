#' Field Tag distribution by Year
#'
#' It calculates the median year for each item of a field tag. 
#' @param M is a bibliographic data frame obtained by \code{\link{convert2df}} function.
#' @param field is a character object. It indicates one of the field tags of the
#'   standard ISI WoS Field Tag codify.
#' @param timespan is a vector with the min and max year. If it is = NULL, the analysis is performed on the entire period. Default is \code{timespan = NULL}.
#' @param min.freq is an integer. It indicates the min frequency of the items to include in the analysis
#' @param n.items is an integer. I indicates the maximum number of items per year to include in the plot.
#' @param labelsize is deprecated argument. It will be removed in the next update.
#' @param remove.terms is a character vector. It contains a list of additional terms to delete from the documents before term extraction. The default is \code{remove.terms = NULL}.
#' @param synonyms is a character vector. Each element contains a list of synonyms, separated by ";",  that will be merged into a single term (the first word contained in the vector element). The default is \code{synonyms = NULL}.
#' @param dynamic.plot is a logical. If TRUE plot aesthetics are optimized for plotly package.
#' @param graph is logical. If TRUE the function plots Filed Tag distribution by Year graph. Default is \code{graph = TRUE}.
#' @return The function \code{fieldByYear} returns a list containing threeobjects:
#' \tabular{lll}{
#' \code{df}  \tab   \tab is a data frame\cr
#' \code{df_graph}\tab    \tab is a data frame with data used to build the graph\cr
#' \code{graph}   \tab   \tab a ggplot object}
#'
#' @examples
#' data(management, package = "bibliometrixData")
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
                        min.freq = 2,
                        n.items = 5,
                        labelsize = NULL,
                        remove.terms = NULL,
                        synonyms = NULL,
                        dynamic.plot = FALSE,
                        graph = TRUE) {
  
  A <- cocMatrix(M, Field = field, binary = FALSE, remove.terms = remove.terms, synonyms = synonyms)
  n <- colSums(as.array(A))
  
  #A=tdIdf(A)
  
  
  trend_med <- apply(A, 2, function(x) {   
    round(quantile(rep(M$PY, x), c(0.25,0.50,0.75), na.rm=TRUE))
  })
  
  trend_med <- as_tibble(t(trend_med)) %>% 
    rename("year_q1"='25%', "year_med"='50%', "year_q3"='75%') %>%  
    mutate(item=rownames(t(trend_med)), freq=n) %>% 
    relocate(c(.data$item,.data$freq), .data$year_q1)

  # if timespan is null, timespan is set to the whole period
  if (is.null(timespan) | length(timespan)!=2){
    timespan <- as.numeric(range(trend_med$year_med, na.rm = TRUE))
  }
  
  df <- trend_med %>%
    mutate(item = tolower(.data$item)) %>%
    group_by(.data$year_med) %>%
    arrange(desc(.data$freq), .data$item) %>%
    arrange(desc(.data$year_med)) %>%   
    dplyr::slice_head(n=n.items) %>% 
    dplyr::filter(.data$freq >= min.freq) %>%
    dplyr::filter(between(.data$year_med, timespan[1],timespan[2])) %>%
    mutate(item = fct_reorder(.data$item, .data$freq))
  
  data("logo",envir=environment())
  logo <- grid::rasterGrob(logo,interpolate = TRUE)
  
  yrange <- range(unlist(df[,which(regexpr("year",names(df))>-1)]))
  
  x <- c(0+0.5,0.05+length(levels(df$item))*0.125)+1
  y <- c(yrange[2]-0.02-diff(yrange)*0.125,yrange[2]-0.02)
  
  g <- ggplot(df, aes(x=.data$item, y=.data$year_med, 
                      text = paste("Term: ", .data$item,"\nYear: ",
                                   .data$year_med ,"\nTerm frequency: ",.data$freq )))+
    geom_point(aes(size = .data$freq), alpha=0.6, color="dodgerblue4")+ 
    scale_size(range=c(2,6))+
    #scale_alpha(range=c(0.3,1))+
    scale_y_continuous(breaks = seq(min(df$year_q1),max(df$year_q3), by=2))+
    guides(size = guide_legend(order = 1, "Term frequency"), alpha = guide_legend(order = 2, "Term frequency"))+
    theme(legend.position = 'right'
          #,aspect.ratio = 1
          ,text = element_text(color = "#444444")
          ,panel.background = element_rect(fill = '#FFFFFF')
          ,panel.grid.major.x = element_blank()
          ,panel.grid.major.y = element_line(color = 'grey95')
          ,plot.title = element_text(size = 24)
          ,axis.title = element_text(size = 14, color = '#555555')
          ,axis.title.y = element_text(vjust = 1, angle = 90, face="bold")
          ,axis.title.x = element_text(hjust = .95)
          ,axis.text.x = element_text(face="bold", angle = 90)#, size=labelsize)
          ,axis.text.y = element_text(face="bold",)
          ,axis.line.x = element_line(color="black", size=0.5)
    ) + annotation_custom(logo, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
 
   if (!isTRUE(dynamic.plot)){
    g <- g+geom_vline(xintercept=nrow(df)-(which(c(diff(df$year_med))==-1)-0.5), color="grey70",alpha=0.6, linetype=6)+
      geom_point(aes(y=.data$year_q1), alpha=0.6, size = 3, color="royalblue4", shape="|")+
      geom_point(aes(y=.data$year_q3), alpha=0.6, size = 3, color="royalblue4", shape="|")
  }
  
 g <- g+
    labs(title="Trend Topics", 
         x="Term",
         y="Year")+
    geom_segment(data=df, aes(x = .data$item, y = .data$year_q1, xend = .data$item, yend = .data$year_q3), size=1.0, color="royalblue4", alpha=0.3) +
    coord_flip() 
  
  if (isTRUE(graph)) {
    print(g) 
  }
  
  
  
  results <- list(df = trend_med, df_graph = df, graph = g)
  
  return(results)
  
}

  