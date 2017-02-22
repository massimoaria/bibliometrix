\name{bibliometrix-package}
\alias{bibliometrix-package}
\alias{bibliometrix}
\docType{package}
\title{
Tool for quantitative research in scientometrics and bibliometrics.
}
\description{
It provides various routines for importing bibliographic data from SCOPUS and Thomson Reuters' ISI Web of Knowledge databases, perfoming bibliometric analysis and building data matrices for co-citation, 
coupling and scientific collaboration analysis.
}
\details{
\tabular{ll}{
Package: \tab bibliometrix\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2016-05-05\cr
License: \tab GPL-3\cr
}

}
\author{
Massimo Aria <massimo.aria@unina.it>, Corrado Cuccurullo <corrado.cuccurullo@unina2.it>

Maintainer: Massimo Aria <massimo.aria@unina.it>
}
\references{
Cuccurullo C., Aria M., Sarto F. (2016) Foundations and Trends in Performance Management. 
A Twenty-five Years Bibliometric Analysis in Business and Public Administration Domains, Scientometrics, DOI: 10.1007/s11192-016-1948-8.

Koseoglu, M. A. (2016). Growth and structure of authorship and co-authorship network in the strategic management realm: 
Evidence from the Strategic Management Journal. BRQ Business Research Quarterly.

Batagelj, V., Cerinsek, M. (2013). On bibliographic networks. Scientometrics, 96(3), 845-864.

Yan, E., Ding, Y. (2012). Scholarly network similarities: How bibliographic coupling networks, 
citation networks, cocitation networks, topical networks, coauthorship networks, 
and coword networks relate to each other. Journal of the American Society for Information Science and Technology, 63(7), 1313-1326.

Rousseau, R. (2010). Bibliographic coupling and co-citation as dual notions. The Janus faced scholar. 
A Festschrift in honour of Peter Ingwersen, 173-183.

Leydesdorff, L., Vaughan, L. (2006). Co-occurrence matrices and their applications in information science: 
Extending ACA to the Web environment. Journal of the American Society for Information Science and technology, 57(12), 1616-1628.

}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
%~~ documentation directory ~~
\keyword{ package }

\examples{
## load scientometrics data set
# data(scientometrics_text)

## Convert text data into a bibliographic data frame
# scient_df <- convert2df(scientometrics_text, dbsource="isi", format="plaintext")

## Perform a bibliometric analysis of the bibliographic data frame  
# results <- biblioAnalysis(scient_df)

## summarize results
# summary(results, k=10, pause=FALSE)

## plot results
# plot(results, k=10, pause=FALSE)

## Estimate Lotka's law coefficients
# L=lotka(results)
# L

## Perform authors' dominance analysis
#DF=dominance(results)
#DF

}
