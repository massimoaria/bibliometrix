utils::globalVariables(c("AU","listAU","logo","N.Articles", "N.Authors", "Theoretical"))
#' Lotka's law coefficient estimation
#'
#' It estimates Lotka's law coefficients for scientific productivity (\cite{Lotka A.J., 1926}).\cr\cr
#'
#' Reference:
#' Lotka, A. J. (1926). The frequency distribution of scientific productivity. Journal of the Washington academy of sciences, 16(12), 317-323.\cr
#'
#' @param M is an object of the class '\code{bibliometrixDB}'.
#' @return The function \code{lotka} returns a list of summary statistics of the Lotka's law estimation of an object of class \code{bibliometrix}.
#'
#' the list contains the following objects:
#' \tabular{lll}{
#' \code{Beta}  \tab   \tab Beta coefficient\cr
#' \code{C}   \tab   \tab Constant coefficient\cr
#' \code{R2} \tab   \tab Goodness of Fit\cr
#' \code{fitted} \tab     \tab Fitted Values\cr
#' \code{p.value} \tab     \tab Pvalue of two-sample Kolmogorov-Smirnov test between the empirical and the theoretical Lotka's Law distribution (with Beta=2)\cr
#' \code{AuthorProd}    \tab   \tab Authors' Productivity frequency table\cr
#' \code{g}   \tab    \tab Lotka's law plot\cr
#' \code{g_shiny}   \tab  \tab Lotka's law plot for biblioshiny}
#'
#' @examples
#' data(management, package = "bibliometrixData")
#' L <- lotka(management)
#' L
#'
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis
#' @seealso \code{\link{summary}} method for class '\code{bibliometrix}'
#'
#' @export


lotka <- function(M) {
  if (!inherits(M, "bibliometrixDB")) {
    cat('\n argument M have to be an object of class "bibliometrixDB"\n')
    return(NA)
  }

  # Author Productivity (LOTKA's LAW)
  AUdf <- M %>%
    mutate(
      listAU = stringr::str_split(as.character(AU), ";"),              
      listAU = purrr::map(listAU, stringr::str_trim)                      
    ) %>%
    select(listAU) %>%
    unnest(cols = listAU) %>%                      
    count(listAU, sort = TRUE, name = "Freq") %>%
    rename(AU = listAU)
  
  
  AuthorProd <- aggregate(AUdf, by = list(AUdf$Freq), "length")
  AuthorProd[, 2] <- as.numeric(AuthorProd[, 2])
  AuthorProd[, 3] <- AuthorProd[, 2] / sum(AuthorProd[, 2])
  names(AuthorProd) <- c("N.Articles", "N.Authors", "Freq")
  LOTKA <- (lm(log10(Freq) ~ log10(N.Articles), data = AuthorProd))
  Yt <- LOTKA$coeff[1] - 2 * log10(AuthorProd[, 1])
  p <- suppressWarnings(ks.test(AuthorProd[, 3], 10^Yt, exact = FALSE)$p.value)
  C <-  10^(as.numeric(LOTKA$coeff[1]))
  Beta <-  abs(as.numeric(LOTKA$coeff[2]))
  R2 <-  summary(LOTKA)$r.squared 
  fitted <-  10^LOTKA$fitted.values

  ## plot
  AuthorProd$Theoretical <- 10^(log10(C)-2*log10(AuthorProd[,1]))
  AuthorProd$Theoretical <- AuthorProd$Theoretical/sum(AuthorProd$Theoretical)
  
  x <- c(max(AuthorProd[,1])-0.02-diff(range(AuthorProd[,1]))*0.125, max(AuthorProd[,1])-0.02)+1
  y <- c(min(AuthorProd[,3]*100),min(AuthorProd[,3]*100)+diff(range(AuthorProd[,3]*100))*0.125)
  
  data("logo",package="bibliometrix",envir=environment())
  logoGrid <- grid::rasterGrob(logo,interpolate = TRUE)
  
  g_shiny <- ggplot2::ggplot(AuthorProd, aes(x = N.Articles, y = Freq*100, text=paste("N.Articles: ",Freq,"\n% of production: ",round(N.Authors*100,1)))) +
    geom_line(aes(group="NA")) +
    #geom_area(aes(group="NA"),fill = 'grey90', alpha = .5) +
    geom_line(data=AuthorProd, aes(y=Theoretical*100, group="NA"),linetype = "dashed",color="black",alpha=0.8)+
    xlim(0,max(AuthorProd$N.Articles)+1)+
    labs(x = 'Documents written'
         , y = '% of Authors'
         , title = "Author Productivity through Lotka's Law") +
    theme(text = element_text(color = "#444444")
          ,panel.background = element_rect(fill = '#FFFFFF')
          ,panel.grid.minor = element_line(color = '#EFEFEF')
          ,panel.grid.major = element_line(color = '#EFEFEF')
          ,plot.title = element_text(size = 24)
          ,axis.title = element_text(size = 14, color = '#555555')
          ,axis.title.y = element_text(vjust = 1, angle = 0)
          ,axis.title.x = element_text(hjust = 0)
          ,axis.line.x = element_line(color="black",linewidth=0.5)
          ,axis.line.y = element_line(color="black",linewidth=0.5)
    )
  g <- g_shiny +
    annotation_custom(logoGrid, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2]) 
  
  AuthorProd <- AuthorProd %>% 
    rename("Documents written" = "N.Articles",
           "N. of Authors" = "N.Authors",
           "Proportion of Authors" = "Freq")
  
  L <- list(Beta = Beta, C = C, R2 = R2, fitted = fitted, p.value = p, AuthorProd = AuthorProd, g=g, g_shiny=g_shiny)
  return(L)
}
