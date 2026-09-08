utils::globalVariables(c(
  "AU", "Freq", "Country", "Collaboration", "Year",
  "MeanTCperYear", "MeanTCperArt"
))
#' Plotting bibliometric analysis results
#'
#' \code{plot} method for class '\code{bibliometrix}'
#' @param x is the object for which plots are desired.
#' @param ... can accept two arguments:\cr
#' \code{k} is an integer, used for plot formatting (number of objects). Default value is 10.\cr
#' \code{pause} is a logical, used to allow pause in screen scrolling of results. Default value is \code{pause = FALSE}.
#' @return The function \code{plot} returns a list of plots of class \code{ggplot2}.
#'
#'
#' @examples
#' data(scientometrics, package = "bibliometrixData")
#'
#' results <- biblioAnalysis(scientometrics)
#'
#' plot(results, k = 10, pause = FALSE)
#'
#' @seealso The bibliometric analysis function \code{\link{biblioAnalysis}}.
#' @seealso \code{\link{summary}} to compute a list of summary statistics of the object of class \code{bibliometrix}.
#'
#' @method plot bibliometrix
#' @export


plot.bibliometrix <- function(x, ...) {
  data("logo", package = "bibliometrix", envir = environment())
  logo <- grid::rasterGrob(logo, interpolate = TRUE)

  ## The collection may carry no year of publication at all.
  hasYears <- sum(!is.na(x$Years)) > 0

  if (!inherits(x, "bibliometrix")) {
    cat('\n argument "x" have to be an object of class "bibliometrix"\n')
    return(NA)
  }
  graphs <- list()

  arguments <- list(...)
  if (sum(names(arguments) == "k") == 0) {
    k <- 10
  } else {
    k <- arguments$k
  }
  if (sum(names(arguments) == "pause") == 0) {
    pause <- FALSE
  } else {
    pause <- arguments$pause
  }

  if (pause == TRUE) {
    cat("Hit <Return> to see next plot: ")
    line <- readline()
  }

  ## An export that omits the author field supports no author ranking: the
  ## data frame below would have no rows and no columns to map onto.
  if (!is.null(x$Authors)) {
    xx <- as.data.frame(x$Authors[1:k])
    ## k may be larger than the number of authors. Those rows are empty: they
    ## would be drawn as an NA bar and would leave the logo box below undefined.
    xx <- xx[!is.na(xx$Freq), ]
    kAU <- nrow(xx)
    xcoord <- c(kAU - 0.2 - (kAU) * 0.15, kAU - 0.02) + 1
    ycoord <- c(max(xx$Freq), max(xx$Freq) - logoDelta(xx$Freq))
    # Authors


    g <- ggplot(data = xx, aes(x = AU, y = Freq)) +
      geom_bar(stat = "identity", fill = "grey90") +
      labs(title = "Most productive Authors", x = "Authors") +
      labs(y = "N. of Documents") +
      theme(
        text = element_text(color = "#444444"),
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid.minor = element_line(color = "#EFEFEF"),
        panel.grid.major = element_line(color = "#EFEFEF"),
        plot.title = element_text(size = 18),
        axis.title = element_text(size = 14, color = "#555555"),
        axis.title.y = element_text(vjust = 1, angle = 90),
        axis.title.x = element_text(hjust = 0),
        axis.text.x = element_text(size = 10),
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5)
      ) +
      annotation_custom(logo, xmin = xcoord[1], xmax = xcoord[2], ymin = ycoord[1], ymax = ycoord[2]) +
      coord_flip()
    plot(g)

    graphs$MostProdAuthors <- g
  } else {
    graphs$MostProdAuthors <- NA
  }

  if (pause == TRUE) {
    cat("Hit <Return> to see next plot: ")
    line <- readline()
  }
  if (!is.na(x$CountryCollaboration[1, 1])) {
    # Countries
    xx <- x$CountryCollaboration[1:k, ] 
    xx <- xx[!is.na(xx$Country),]
    xx <- xx[order(-(xx$SCP + xx$MCP)), ]
    xx1 <- cbind(xx[, 1:2], rep("SCP", nrow(xx)))
    names(xx1) <- c("Country", "Freq", "Collaboration")
    xx2 <- cbind(xx[, c(1, 3)], rep("MCP", nrow(xx)))
    names(xx2) <- c("Country", "Freq", "Collaboration")
    xx <- rbind(xx2, xx1)
    xx$Country <- factor(xx$Country, levels = xx$Country[1:dim(xx2)[1]])

    Freq <- x$CountryCollaboration$SCP[1:(nrow(xx)/2)] + x$CountryCollaboration$MCP[1:(nrow(xx)/2)]
    st <- floor(nrow(xx) / 10)
    # xcoord <- c(st-0.2-(st)*0.85, 0.02)+1
    xcoord <- c(1, max(st, 3))
    ycoord <- c(max(Freq), max(Freq) - logoDelta(Freq))

    g <- suppressWarnings(ggplot(data = xx, aes(x = Country, y = Freq, fill = Collaboration)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits = rev(levels(xx$Country))) +
      scale_fill_discrete(
        name = "Collaboration",
        breaks = c("SCP", "MCP")
      ) +
      labs(
        title = "Most Productive Countries", x = "Countries", y = "N. of Documents",
        caption = "SCP: Single Country Publications, MCP: Multiple Country Publications"
      ) +
      theme(
        text = element_text(color = "#444444"),
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid.minor = element_line(color = "#EFEFEF"),
        panel.grid.major = element_line(color = "#EFEFEF"),
        plot.title = element_text(size = 18),
        axis.title = element_text(size = 14, color = "#555555"),
        axis.title.y = element_text(vjust = 1, angle = 90),
        axis.title.x = element_text(hjust = 0),
        axis.text.x = element_text(size = 10),
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5)
      ) +
      annotation_custom(logo, xmin = xcoord[1], xmax = xcoord[2], ymin = ycoord[1], ymax = ycoord[2]) +
      coord_flip())


    plot(g)
    graphs$MostProdCountries <- g
  } else {
    graphs$MostProdCountries <- NA
  }

  if (pause == TRUE) {
    cat("Hit <Return> to see next plot: ")
    line <- readline()
  }

  # Articles per Year

  if (hasYears) {
    Tab <- table(x$Years)

    ## inserting missing years
    YY <- setdiff(seq(min(x$Years, na.rm = TRUE), max(x$Years, na.rm = TRUE)), names(Tab))
    Y <- data.frame(Year = as.numeric(c(names(Tab), YY)), Freq = c(as.numeric(Tab), rep(0, length(YY))))
    Y <- Y[order(Y$Year), ]

    names(Y) <- c("Year", "Freq")

    xcoord <- c(max(Y$Year) - 0.02 - logoDelta(Y$Year), max(Y$Year) - 0.02) + 1
    ycoord <- c(min(Y$Freq), min(Y$Freq) + logoDelta(Y$Freq))

    g <- ggplot(Y, aes(x = Year, y = Freq)) +
      geom_line() +
      geom_area(fill = "grey90", alpha = .5) +
      labs(
        x = "Year",
        y = "Articles",
        title = "Annual Scientific Production"
      ) +
      scale_x_continuous(breaks = (Y$Year[seq(1, length(Y$Year), by = 2)])) +
      theme(
        text = element_text(color = "#444444"),
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid.minor = element_line(color = "#EFEFEF"),
        panel.grid.major = element_line(color = "#EFEFEF"),
        plot.title = element_text(size = 18),
        axis.title = element_text(size = 14, color = "#555555"),
        axis.title.y = element_text(vjust = 1, angle = 90),
        axis.title.x = element_text(hjust = 0),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5)
      ) +
      annotation_custom(logo, xmin = xcoord[1], xmax = xcoord[2], ymin = ycoord[1], ymax = ycoord[2])

    plot(g)
    graphs$AnnualScientProd <- g
  } else {
    graphs$AnnualScientProd <- NA
  }


  Table2 <- NA
  ## Both citation plots aggregate the times cited by year of publication, so
  ## they need each of the two fields. isTRUE() guards the database test itself:
  ## a collection assembled by hand may carry no DB, and %in% on NULL is
  ## logical(0), which if() cannot evaluate.
  if (!isTRUE(x$DB %in% c("COCHRANE", "PUBMED")) && !is.null(x$TotalCitation) && hasYears) {
    if (pause == TRUE) {
      cat("Hit <Return> to see next plot: ")
      line <- readline()
    }


    # Total Citation Plot
    Table2 <- aggregate(x$TotalCitation, by = list(x$Years), length)
    Table2$xx <- aggregate(x$TotalCitation, by = list(x$Years), mean)$x
    Table2$Annual <- NA
    d <- date()
    d <- as.numeric(substring(d, nchar(d) - 3, nchar(d)))
    Table2$Years <- d - Table2$Group.1
    Table2$Annual <- Table2$xx / Table2$Years
    names(Table2) <- c("Year", "N", "MeanTCperArt", "MeanTCperYear", "CitableYears")

    ## inserting missing years
    YY <- setdiff(seq(min(x$Years, na.rm = TRUE), max(x$Years, na.rm = TRUE)), Table2$Year)
    if (length(YY > 0)) {
      YY <- data.frame(YY, 0, 0, 0, 0)
      names(YY) <- c("Year", "N", "MeanTCperArt", "MeanTCperYear", "CitableYears")
      Table2 <- rbind(Table2, YY)
      Table2 <- Table2[order(Table2$Year), ]
      row.names(Table2) <- Table2$Year
    }

    xcoord <- c(max(Table2$Year) - 0.02 - logoDelta(Table2$Year), max(Table2$Year) - 0.02) + 1
    Table2$MeanTCperYear[is.nan(Table2$MeanTCperYear)] <- 0
    ycoord <- c(min(Table2$MeanTCperYear), min(Table2$MeanTCperYear) + logoDelta(Table2$MeanTCperYear))

    g <- ggplot(Table2, aes(x = Year, y = MeanTCperYear)) +
      geom_line() +
      geom_area(fill = "grey90", alpha = .5) +
      labs(
        x = "Year",
        y = "Citations",
        title = "Average Article Citations per Year"
      ) +
      scale_x_continuous(breaks = (Table2$Year[seq(1, length(Table2$Year), by = 2)])) +
      theme(
        text = element_text(color = "#444444"),
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid.minor = element_line(color = "#EFEFEF"),
        panel.grid.major = element_line(color = "#EFEFEF"),
        plot.title = element_text(size = 18),
        axis.title = element_text(size = 14, color = "#555555"),
        axis.title.y = element_text(vjust = 1, angle = 90),
        axis.title.x = element_text(hjust = 0),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5)
      ) +
      annotation_custom(logo, xmin = xcoord[1], xmax = xcoord[2], ymin = ycoord[1], ymax = ycoord[2])

    plot(g)
    graphs$AverArtCitperYear <- g

    if (pause == TRUE) {
      cat("Hit <Return> to see next plot: ")
      line <- readline()
    }

    xcoord <- c(max(Table2$Year) - 0.02 - logoDelta(Table2$Year), max(Table2$Year) - 0.02) + 1
    ycoord <- c(min(Table2$MeanTCperArt), min(Table2$MeanTCperArt) + logoDelta(Table2$MeanTCperArt))

    g <- ggplot(Table2, aes(x = Year, y = MeanTCperArt)) +
      geom_line() +
      geom_area(fill = "grey90", alpha = .5) +
      labs(
        x = "Year",
        y = "Citations",
        title = "Average Total Citations per Year"
      ) +
      scale_x_continuous(breaks = (Table2$Year[seq(1, length(Table2$Year), by = 2)])) +
      theme(
        text = element_text(color = "#444444"),
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid.minor = element_line(color = "#EFEFEF"),
        panel.grid.major = element_line(color = "#EFEFEF"),
        plot.title = element_text(size = 18),
        axis.title = element_text(size = 14, color = "#555555"),
        axis.title.y = element_text(vjust = 1, angle = 90),
        axis.title.x = element_text(hjust = 0),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.line.y = element_line(color = "black", linewidth = 0.5)
      ) +
      annotation_custom(logo, xmin = xcoord[1], xmax = xcoord[2], ymin = ycoord[1], ymax = ycoord[2])
    plot(g)
    graphs$AverTotCitperYear <- g
  } else {
    graphs$AverArtCitperYear <- NA
    graphs$AverTotCitperYear <- NA
  }
  invisible(graphs)
}

## Extent of the box the logo is drawn in, as a fraction of the range of the
## values on that axis. A collection of a single document -- or one whose values
## are all equal -- has a range of zero: the box collapses to a line and grid
## stops on a raster whose ratio is undefined ("missing value where TRUE/FALSE
## needed"). One unit of that axis is used instead, which is one document or one
## year.
logoDelta <- function(v, frac = 0.15, fallback = 1) {
  v <- v[is.finite(v)]
  if (length(v) == 0) {
    return(fallback)
  }
  d <- diff(range(v)) * frac
  if (d == 0) fallback else d
}
