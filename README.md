
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bibliometrix

## An R-tool for comprehensive science mapping analysis.

[![bibliometrix: An R-tool for comprehensive science mapping
analysis.](https://www.bibliometrix.org/JOI-badge.svg)](https://doi.org/10.1016/j.joi.2017.08.007)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/massimoaria/e2tree/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/massimoaria/bibliometrix/actions/workflows/R-CMD-check.yaml)
[![cran
version](http://www.r-pkg.org/badges/version/bibliometrix)](https://cran.r-project.org/package=bibliometrix)
[![rstudio mirror
downloads](https://cranlogs.r-pkg.org/badges/bibliometrix)](https://github.com/metacran/cranlogs.app)
[![](http://cranlogs.r-pkg.org/badges/grand-total/bibliometrix)](https://cran.r-project.org/package=bibliometrix)

<p align="center">
<img src="https://www.bibliometrix.org/logo_new.png" width="400"  />
</p>

## Overview

**bibliometrix** package provides a set of tools for quantitative
research in bibliometrics and scientometrics.

Bibliometrics turns the main tool of science, quantitative analysis, on
itself. Essentially, bibliometrics is the application of quantitative
analysis and statistics to publications such as journal articles and
their accompanying citation counts. Quantitative evaluation of
publication and citation data is now used in almost all scientific
fields to evaluate growth, maturity, leading authors, conceptual and
intellectual maps, trends of a scientific community.

Bibliometrics is also used in research performance evaluation,
especially in university and government labs, and also by policymakers,
research directors and administrators, information specialists and
librarians, and scholars themselves.

**bibliometrix** supports scholars in three key phases of analysis:

- Data importing and conversion to R format;

- Bibliometric analysis of a publication dataset;

- Building and plotting matrices for co-citation, coupling,
  collaboration, and co-word analysis. Matrices are the input data for
  performing network analysis, multiple correspondence analysis, and any
  other data reduction techniques.

## biblioshiny

**bibliometrix** includes **biblioshiny: bibliometrix for no-coders**

**biblioshiny** is a *shiny app providing a web-interface for
bibliometrix*.

It supports scholars in easy use of the main features of bibliometrix:

- Data importing and conversion to data frame collection

- Data filtering

- Analytics and Plots for three different level metrics:

  - Sources

  - Authors

  - Documents

- Analysis of three structures of Knowledge (K-structures):

  - Conceptual Structure

  - Intellectual Structure

  - Social Strucutre

### How to use biblioshiny

Please follow the biblioshiny tutorial at the section tutorial of
bibliometrix website <https://www.bibliometrix.org/>

## How to cite bibliometrix

If you use this package for your research, you must cite it.

To cite bibliometrix in publications, please use:

Aria, M. & Cuccurullo, C. (2017) **bibliometrix: An R-tool for
comprehensive science mapping analysis**, *Journal of Informetrics*,
11(4), pp 959-975, Elsevier.

## Community

Official website: <https://www.bibliometrix.org>

CRAN page: <https://cran.r-project.org/package=bibliometrix>

GitHub repository: <https://github.com/massimoaria/bibliometrix>

Tutorials

How to use:
<https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html>

Data importing and converting:
<https://www.bibliometrix.org/vignettes/Data-Importing-and-Converting.html>

## Installation

Stable version from CRAN

Developers version from GitHub

Load `bibliometrix`

``` r
library('bibliometrix')
#> Please note that our software is open source and available for use, distributed under the MIT license.
#> When it is used in a publication, we ask that authors properly cite the following reference:
#> 
#> Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, 
#>                         Journal of Informetrics, 11(4), pp 959-975, Elsevier.
#> 
#> Failure to properly cite the software is considered a violation of the license.
#>                         
#> For information and bug reports:
#>                         - Take a look at https://www.bibliometrix.org
#>                         - Send an email to info@bibliometrix.org   
#>                         - Write a post on https://github.com/massimoaria/bibliometrix/issues
#>                         
#> Help us to keep Bibliometrix and Biblioshiny free to download and use by contributing with a small donation to support our research team (https://bibliometrix.org/donate.html)
#> 
#>                         
#> To start with the Biblioshiny app, please digit:
#> biblioshiny()
```

## Data loading and converting

The export file can be read and converted using by R using the function
*convert2df*:

**convert2df**(*file*, *dbsource*, *format*)

The argument *file* is a character vector containing the name of export
files downloaded from SCOPUS, Clarivate Analytics WoS, OpenAlex, Digital
Science Dimensions, PubMed or Cochrane CDSR website. *file* can also
contains the name of a json/xlm object download using OpenAlex, Digital
Science Dimenions or PubMed APIs (through the packages *openalexR*,
*dimensionsR* and *pubmedR*.

es. file \<- c(“file1.txt”,“file2.txt”, …)

``` r
## An example from bibliometrix vignettes

file <- c("https://www.bibliometrix.org/datasets/management1.txt","https://www.bibliometrix.org/datasets/management2.txt")

M <- convert2df(file = file, dbsource = "wos", format = "plaintext")
#> 
#> Converting your wos collection into a bibliographic dataframe
#> 
#> Done!
#> 
#> 
#> Generating affiliation field tag AU_UN from C1:  Done!
```

*convert2df* creates a bibliographic data frame with cases corresponding
to manuscripts and variables to Field Tag in the original export file.

Each manuscript contains several elements, such as authors’ names,
title, keywords and other information. All these elements constitute the
bibliographic attributes of a document, also called metadata.

Data frame columns are named using the standard Clarivate Analytics WoS
Field Tag codify
[(https://www.bibliometrix.org/documents/Field_Tags_bibliometrix.pdf)](https://www.bibliometrix.org/documents/Field_Tags_bibliometrix.pdf).

## Check completeness of metadata included in the bibliographic data frame

After importing a bibliographic data frame, we can check the
completeness of the metadata included in it through **missingData()**.

**missingData**(*M*)

The argument *M* is a bibliographic data frame obtained by
**convert2df** function.

``` r
## An example from bibliometrix vignettes

com <- missingData(M)

com$mandatoryTags
#>    tag          description missing_counts missing_pct     status
#> 1   AU               Author              0        0.00  Excellent
#> 2   DT        Document Type              0        0.00  Excellent
#> 3   SO              Journal              0        0.00  Excellent
#> 4   LA             Language              0        0.00  Excellent
#> 5   WC   Science Categories              0        0.00  Excellent
#> 6   TI                Title              0        0.00  Excellent
#> 7   TC       Total Citation              0        0.00  Excellent
#> 8   CR     Cited References              2        0.22       Good
#> 9   AB             Abstract              7        0.78       Good
#> 10  PY     Publication Year              9        1.00       Good
#> 11  RP Corresponding Author             10        1.11       Good
#> 12  C1          Affiliation             21        2.34       Good
#> 13  DI                  DOI             36        4.01       Good
#> 14  DE             Keywords             79        8.80       Good
#> 15  ID        Keywords Plus             97       10.80 Acceptable
```

**missingData** returns a list containing two data frame. The first one,
*allTags* includes the results for all metadata in M. The latter,
*mandatoryTags*, reports the results only for the metadata needed to
perform analyses with bibliometrix or biblioshiny.

The column *status* classifies the percentage of missing value in 5
categories: “Excellent” (0%), “Good” (0.01% to 10.00%), “Acceptable”
(from 10.01% to 20.00%), “Poor” (from 20.01% to 50.00%), “Critical”
(from 50.01% to 99.99%), “Completely missing” (100%).

## Bibliometric Analysis

The first step is to perform a descriptive analysis of the bibliographic
data frame.

The function *biblioAnalysis* calculates main bibliometric measures
using this syntax:

``` r
results <- biblioAnalysis(M, sep = ";")
```

The function *biblioAnalysis* returns an object of class “bibliometrix”.

To summarize main results of the bibliometric analysis, use the generic
function *summary*. It displays main information about the bibliographic
data frame and several tables, such as annual scientific production, top
manuscripts per number of citations, most productive authors, most
productive countries, total citation per country, most relevant sources
(journals) and most relevant keywords.

*summary* accepts two additional arguments. *k* is a formatting value
that indicates the number of rows of each table. *pause* is a logical
value (TRUE or FALSE) used to allow (or not) pause in screen scrolling.
Choosing k=10 you decide to see the first 10 Authors, the first 10
sources, etc.

``` r
S <- summary(object = results, k = 10, pause = FALSE)
#> 
#> 
#> MAIN INFORMATION ABOUT DATA
#> 
#>  Timespan                              1985 : 2022 
#>  Sources (Journals, Books, etc)        281 
#>  Documents                             898 
#>  Annual Growth Rate %                  0 
#>  Document Average Age                  9.19 
#>  Average citations per doc             37.12 
#>  Average citations per year per doc    3.454 
#>  References                            43935 
#>  
#> DOCUMENT TYPES                     
#>  article                         862 
#>  article; book chapter           1 
#>  article; early access           9 
#>  article; proceedings paper      26 
#>  
#> DOCUMENT CONTENTS
#>  Keywords Plus (ID)                    1918 
#>  Author's Keywords (DE)                2243 
#>  
#> AUTHORS
#>  Authors                               2079 
#>  Author Appearances                    2657 
#>  Authors of single-authored docs       112 
#>  
#> AUTHORS COLLABORATION
#>  Single-authored docs                  121 
#>  Documents per Author                  0.432 
#>  Co-Authors per Doc                    2.96 
#>  International co-authorships %        36.41 
#>  
#> 
#> Annual Scientific Production
#> 
#>  Year    Articles
#>     1985        2
#>     1986        2
#>     1988        1
#>     1990        1
#>     1992        4
#>     1993        5
#>     1994        4
#>     1995        7
#>     1996        4
#>     1997        3
#>     1998        4
#>     1999        6
#>     2000        3
#>     2001        4
#>     2002        4
#>     2003        5
#>     2004        5
#>     2005       10
#>     2006       13
#>     2007       11
#>     2008       13
#>     2009       18
#>     2010       32
#>     2011       33
#>     2012       27
#>     2013       34
#>     2014       34
#>     2015       62
#>     2016       62
#>     2017       80
#>     2018       81
#>     2019      125
#>     2020      141
#>     2021       47
#>     2022        2
#> 
#> Annual Percentage Growth Rate 0 
#> 
#> 
#> Most Productive Authors
#> 
#>    Authors        Articles Authors        Articles Fractionalized
#> 1     MERIGO JM         20    KOSTOFF RN                     7.77
#> 2     PORTER AL         19    PORTER AL                      5.84
#> 3     KOSTOFF RN        16    MERIGO JM                      5.42
#> 4     KUMAR S           15    KAJIKAWA Y                     4.62
#> 5     KAJIKAWA Y        14    KUMAR S                        4.28
#> 6     ZHANG Y            9    KOSEOGLU MA                    3.07
#> 7     ABRAMO G           8    SHILBURY D                     3.00
#> 8     D'ANGELO CA        8    ABRAMO G                       2.58
#> 9     KOSEOGLU MA        8    D'ANGELO CA                    2.58
#> 10    YOUTIE J           8    CULLEN JG                      2.50
#> 
#> 
#> Top manuscripts per citations
#> 
#>                                  Paper                                     DOI   TC TCperYear   NTC
#> 1  CHEN HC, 2012, MIS QUART                     NA                             2161    166.23 15.64
#> 2  ZUPIC I, 2015, ORGAN RES METHODS             10.1177/1094428114562629        844     84.40 17.17
#> 3  RAMOS-RODRIGUEZ AR, 2004, STRATEGIC MANAGE J 10.1002/smj.397                 667     31.76  3.76
#> 4  VOLBERDA HW, 2010, ORGAN SCI                 10.1287/orsc.1090.0503          626     41.73  9.82
#> 5  DAIM TU, 2006, TECHNOL FORECAST SOC          10.1016/j.techfore.2006.04.004  569     29.95  5.67
#> 6  KOSTOFF RN, 2001, IEEE T ENG MANAGE          10.1109/17.922473               387     16.12  2.66
#> 7  NERUR SP, 2008, STRATEG MANAGE J             10.1002/smj.659                 353     20.76  3.48
#> 8  MELIN G, 2000, RES POLICY                    10.1016/S0048-7333(99)00031-1   336     13.44  2.15
#> 9  MOED HF, 1985, RES POLICY                    10.1016/0048-7333(85)90012-5    310      7.75  1.81
#> 10 MURRAY F, 2002, RES POLICY                   10.1016/S0048-7333(02)00070-7   301     13.09  2.40
#> 
#> 
#> Corresponding Author's Countries
#> 
#>           Country Articles   Freq SCP MCP MCP_Ratio
#> 1  USA                 146 0.1644  92  54     0.370
#> 2  CHINA                84 0.0946  41  43     0.512
#> 3  SPAIN                72 0.0811  51  21     0.292
#> 4  BRAZIL               65 0.0732  52  13     0.200
#> 5  ITALY                49 0.0552  31  18     0.367
#> 6  UNITED KINGDOM       47 0.0529  22  25     0.532
#> 7  GERMANY              42 0.0473  29  13     0.310
#> 8  AUSTRALIA            31 0.0349  19  12     0.387
#> 9  NETHERLANDS          31 0.0349  20  11     0.355
#> 10 INDIA                26 0.0293  17   9     0.346
#> 
#> 
#> SCP: Single Country Publications
#> 
#> MCP: Multiple Country Publications
#> 
#> 
#> Total Citations per Country
#> 
#>       Country      Total Citations Average Article Citations
#> 1  USA                        8896                     60.93
#> 2  SPAIN                      2843                     39.49
#> 3  UNITED KINGDOM             2143                     45.60
#> 4  NETHERLANDS                2110                     68.06
#> 5  CHINA                      1939                     23.08
#> 6  ITALY                      1566                     31.96
#> 7  GERMANY                    1449                     34.50
#> 8  JAPAN                      1104                     46.00
#> 9  SLOVENIA                   1100                    157.14
#> 10 BRAZIL                     1074                     16.52
#> 
#> 
#> Most Relevant Sources
#> 
#>                                                   Sources        Articles
#> 1  TECHNOLOGICAL FORECASTING AND SOCIAL CHANGE                         97
#> 2  RESEARCH POLICY                                                     83
#> 3  TECHNOLOGY ANALYSIS & STRATEGIC MANAGEMENT                          31
#> 4  JOURNAL OF BUSINESS RESEARCH                                        28
#> 5  SCIENCE AND PUBLIC POLICY                                           25
#> 6  TECHNOVATION                                                        19
#> 7  JOURNAL OF TECHNOLOGY TRANSFER                                      12
#> 8  INTERNATIONAL JOURNAL OF CONTEMPORARY HOSPITALITY MANAGEMENT        10
#> 9  INTERNATIONAL JOURNAL OF INNOVATION AND TECHNOLOGY MANAGEMENT       10
#> 10 R & D MANAGEMENT                                                    10
#> 
#> 
#> Most Relevant Keywords
#> 
#>    Author Keywords (DE)      Articles Keywords-Plus (ID)     Articles
#> 1      BIBLIOMETRICS              234 SCIENCE                     146
#> 2      BIBLIOMETRIC ANALYSIS      161 INNOVATION                  130
#> 3      CITATION ANALYSIS           62 PERFORMANCE                 117
#> 4      INNOVATION                  44 IMPACT                      116
#> 5      BIBLIOMETRIC STUDY          30 MANAGEMENT                  113
#> 6      TEXT MINING                 30 KNOWLEDGE                    82
#> 7      VOSVIEWER                   30 INTELLECTUAL STRUCTURE       75
#> 8      LITERATURE REVIEW           29 TECHNOLOGY                   62
#> 9      BIBLIOMETRIC                28 JOURNALS                     59
#> 10     CO-CITATION ANALYSIS        28 MODEL                        58
```

Some basic plots can be drawn using the generic function plot:

``` r
plot(x = results, k = 10, pause = FALSE)
```

<img src="man/figures/README-plot generic function-1.png" width="100%" /><img src="man/figures/README-plot generic function-2.png" width="100%" /><img src="man/figures/README-plot generic function-3.png" width="100%" /><img src="man/figures/README-plot generic function-4.png" width="100%" /><img src="man/figures/README-plot generic function-5.png" width="100%" />

## Bibliographic network matrices

Manuscript’s attributes are connected to each other through the
manuscript itself: author(s) to journal, keywords to publication date,
etc.

These connections of different attributes generate bipartite networks
that can be represented as rectangular matrices (Manuscripts x
Attributes).

Furthermore, scientific publications regularly contain references to
other scientific works. This generates a further network, namely,
co-citation or coupling network.

These networks are analyzed in order to capture meaningful properties of
the underlying research system, and in particular to determine the
influence of bibliometric units such as scholars and journals.

### biblioNetwork function

The function *biblioNetwork* calculates, starting from a bibliographic
data frame, the most frequently used networks: Coupling, Co-citation,
Co-occurrences, and Collaboration.

*biblioNetwork* uses two arguments to define the network to compute:

- *analysis* argument can be “co-citation”, “coupling”, “collaboration”,
  or “co-occurrences”.

- *network* argument can be “authors”, “references”, “sources”,
  “countries”, “universities”, “keywords”, “author_keywords”, “titles”
  and “abstracts”.

i.e. the following code calculates a classical co-citation network:

    NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")

## Visualizing bibliographic networks

All bibliographic networks can be graphically visualized or modeled.

Using the function *networkPlot*, you can plot a network created by
*biblioNetwork* using R routines.

The main argument of *networkPlot* is type. It indicates the network map
layout: circle, kamada-kawai, mds, etc.

In the following, we propose some examples.

### Country Scientific Collaboration

``` r
# Create a country collaboration network

M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.8)
```

<img src="man/figures/README-Country collaboration-1.png" width="100%" />

### Co-Citation Network

``` r
# Create a co-citation network

NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", n=30, sep = ";")

# Plot the network
net=networkPlot(NetMatrix, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)
```

<img src="man/figures/README-Co-citation network-1.png" width="100%" />

### Keyword co-occurrences

``` r
# Create keyword co-occurrences network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
```

<img src="man/figures/README-Keyword c-occurrences-1.png" width="100%" />

## Co-Word Analysis: The conceptual structure of a field

The aim of the co-word analysis is to map the conceptual structure of a
framework using the word co-occurrences in a bibliographic collection.

The analysis can be performed through dimensionality reduction
techniques such as Multidimensional Scaling (MDS), Correspondence
Analysis (CA) or Multiple Correspondence Analysis (MCA).

Here, we show an example using the function *conceptualStructure* that
performs a CA or MCA to draw a conceptual structure of the field and
K-means clustering to identify clusters of documents which express
common concepts. Results are plotted on a two-dimensional map.

*conceptualStructure* includes natural language processing (NLP)
routines (see the function *termExtraction*) to extract terms from
titles and abstracts. In addition, it implements the Porter’s stemming
algorithm to reduce inflected (or sometimes derived) words to their word
stem, base or root form.

``` r

# Conceptual Structure using keywords (method="CA")

CS <- conceptualStructure(M,field="ID", method="MCA", minDegree=10, clust=5, stemming=FALSE, labelsize=15, documents=20, graph=FALSE)
plot(CS$graph_terms)
```

<img src="man/figures/README-Co-Word Analysis-1.png" width="100%" />

``` r
plot(CS$graph_dendogram)
```

<img src="man/figures/README-Co-Word Analysis-2.png" width="100%" />

## Historical Direct Citation Network

The historiographic map is a graph proposed by E. Garfield to represent
a chronological network map of most relevant direct citations resulting
from a bibliographic collection.

The function histNetwork generates a chronological direct citation
network matrix which can be plotted using *histPlot*:

``` r
# Create a historical citation network

histResults <- histNetwork(M, sep = ";")
#> 
#> WOS DB:
#> Searching local citations (LCS) by reference items (SR) and DOIs...
#> 
#> Analyzing 62646 reference items...
#> 
#> Found 422 documents with no empty Local Citations (LCS)

# Plot a historical co-citation network
net <- histPlot(histResults, n=20, size = FALSE,label="short")
```

<img src="man/figures/README-Historical Co-citation network-1.png" width="100%" />

    #> 
    #>  Legend
    #> 
    #>                                                                         Label
    #> 1                  MOED HF, 1985, RES POLICY DOI 10.1016/0048-7333(85)90012-5
    #> 2                           HOFFMAN DL, 1993, J CONSUM RES DOI 10.1086/209319
    #> 3      PORTER AL, 1995, TECHNOL FORECAST SOC DOI 10.1016/0040-1625(95)00022-3
    #> 4      WATTS RJ, 1997, TECHNOL FORECAST SOC DOI 10.1016/S0040-1625(97)00050-4
    #> 5                   KOSTOFF RN, 2001, IEEE T ENG MANAGE DOI 10.1109/17.922473
    #> 6                VERBEEK A, 2002, INT J MANAG REV DOI 10.1111/1468-2370.00083
    #> 7            RAMOS-RODRIGUEZ AR, 2004, STRATEGIC MANAGE J DOI 10.1002/SMJ.397
    #> 8      DAIM TU, 2006, TECHNOL FORECAST SOC DOI 10.1016/J.TECHFORE.2006.04.004
    #> 9  SCHILDT HA, 2006, ENTREP THEORY PRACT DOI 10.1111/J.1540-6520.2006.00126.X
    #> 10         CASILLAS J, 2007, FAM BUS REV DOI 10.1111/J.1741-6248.2007.00092.X
    #> 11                       NERUR SP, 2008, STRATEG MANAGE J DOI 10.1002/SMJ.659
    #> 12                  PODSAKOFF PM, 2008, J MANAGE DOI 10.1177/0149206308319533
    #> 13  KAJIKAWA Y, 2008, TECHNOL FORECAST SOC DOI 10.1016/J.TECHFORE.2007.05.005
    #> 14             LANDSTROM H, 2012, RES POLICY DOI 10.1016/J.RESPOL.2012.03.009
    #> 15             FAGERBERG J, 2012, RES POLICY DOI 10.1016/J.RESPOL.2012.03.008
    #> 16                  SHAFIQUE M, 2013, STRATEGIC MANAGE J DOI 10.1002/SMJ.2002
    #> 17              ZUPIC I, 2015, ORGAN RES METHODS DOI 10.1177/1094428114562629
    #> 18               MERIGO JM, 2015, J BUS RES DOI 10.1016/J.JBUSRES.2015.04.006
    #> 19             LAENGLE S, 2017, EUR J OPER RES DOI 10.1016/J.EJOR.2017.04.027
    #> 20           VALENZUELA L, 2017, J BUS IND MARK DOI 10.1108/JBIM-04-2016-0079
    #>                                                                                                                                                                                                                        Author_Keywords
    #> 1                                                                                                                                                                                                                                 <NA>
    #> 2                                                                                                                                                                                                                                 <NA>
    #> 3                                                                                                                                                                                                                                 <NA>
    #> 4                                                                                                                                                                                                                                 <NA>
    #> 5  BIBLIOMETRICS; CITATION; CO-CITATION; CO-OCCURRENCE; CO-WORD; DECISION AIDS; PATENT CITATION; RETROSPECTIVE ANALYSES; ROADMAPPING; ROADMAPS; SCIENCE AND TECHNOLOGY; TECHNOLOGY INSERTION; TECHNOLOGY STRATEGY; TECHNOLOGY TRANSFER
    #> 6                                                                                                                                                                                                                                 <NA>
    #> 7                                                                                                                                                                   STRATEGIC MANAGEMENT RESEARCH; BIBLIOMETRICS; CO-CITATION ANALYSIS
    #> 8                                                                                                                                                                                                                                 <NA>
    #> 9                                                                                                                                                                                                                                 <NA>
    #> 10                                                                                                                                                                                                                                <NA>
    #> 11                                                                                                                  AUTHOR CO-CITATION ANALYSIS; PATHFINDER ANALYSIS; INFORMATION THEORY; STRATEGIC MANAGEMENT RESEARCH; BIBLIOMETRICS
    #> 12                                                                                                                                                     CITATION ANALYSIS; BIBLIOMETRIC TECHNIQUES; SCHOLARLY IMPACT; UNIVERSITY IMPACT
    #> 13                                                                                                                           EMERGING TECHNOLOGIES; FORECASTING; CITATION NETWORK; BIBLIOMETRICS; SUSTAINABLE ENERGY; RENEWABLE ENERGY
    #> 14                                                                                                                                                                  ENTREPRENEURSHIP; RESEARCH FIELD; HANDBOOKS; BIBLIOMETRIC ANALYSIS
    #> 15                                                                                                                                                    INNOVATION STUDIES; NEW SCIENTIFIC FIELDS; SPECIALISMS; BIBLIOMETRICS; HANDBOOKS
    #> 16                                                                                                                                      INNOVATION; MULTIDISCIPLINARITY; KNOWLEDGE CONVERGENCE; ABSORPTIVE CAPACITY; CREATIVE CAPACITY
    #> 17                                                                                                                                                                 BIBLIOMETRICS; CO-CITATION; BIBLIOGRAPHIC COUPLING; SCIENCE MAPPING
    #> 18                                                                                                                                                                  BUSINESS RESEARCH; BIBLIOMETRICS; WEB OF SCIENCE; JOURNAL ANALYSIS
    #> 19                                                                                                                                          OPERATIONAL RESEARCH; MANAGEMENT SCIENCE; BIBLIOMETRICS; WEB OF SCIENCE; CITATION ANALYSIS
    #> 20                                                                                                                                      BIBLIOMETRICS; SCIENCE MAPPING; BUSINESS MARKETING; JOURNAL OF BUSINESS & INDUSTRIAL MARKETING
    #>                                                                                                                                                                                                                                         KeywordsPlus
    #> 1                                                                                                                                                                                                                                               <NA>
    #> 2                                                                                                                                                                                 SCIENTOMETRIC TRANSACTION MATRICES; CO-CITATION ANALYSIS; NETWORKS
    #> 3                                                                                                                                                                                                                                               <NA>
    #> 4                                                                                                                                                                                                                              TECHNOLOGY; DIFFUSION
    #> 5                                                                                                                                                                                                        TECHNICAL INTELLIGENCE; DATABASE TOMOGRAPHY
    #> 6                                                                                                                                UNIVERSITY-RESEARCH PERFORMANCE; CITATION ANALYSIS; BASIC RESEARCH; CO-CITATION; ECONOMICS; FLANDERS; POLICY; FIELD
    #> 7                                                                                              AUTHOR COCITATION ANALYSIS; DIVERSIFICATION STRATEGY; COMPETITIVE ADVANTAGE; LOCAL SEARCH; KNOWLEDGE; FIRM; PERFORMANCE; IMPACT; DISCIPLINE; BEHAVIOR
    #> 8                                                                                                                                                                                                                                               <NA>
    #> 9                                                                                                                                                                VENTURE PERFORMANCE; KNOWLEDGE; ORGANIZATIONS; INNOVATION; CREATION; CONTEXT; ENTRY
    #> 10                                                                                                                                                               AUTHOR COCITATION ANALYSIS; LIMITATIONS; PARADIGMS; SYSTEMS; SCIENCE; POLICY; FIRMS
    #> 11                                                                                                                                                                                         CITATION ANALYSIS; JOURNALS; SYSTEMS; DISCIPLINE; SCIENCE
    #> 12                                                                                                                   RESEARCH PRODUCTIVITY; JOB-PERFORMANCE; JOURNALS; FACULTY; SCIENCE; MODEL; INFORMETRICS; METAANALYSIS; PERSONALITY; RECOGNITION
    #> 13                                                                                                                                                              HYDROGEN FUTURES; BIBLIOMETRICS; POLICY; SCIENCE; RENEWABLES; MECHANISMS; PRIORITIES
    #> 14                                                                                                                                                    SCIENCE POLICY; INNOVATION; ECONOMICS; GROWTH; SPILLOVERS; DISCOVERY; EMERGENCE; MARKET; FIELD
    #> 15                                                                                                                                                                                                                         SOCIAL-SCIENCE; ECONOMICS
    #> 16                                                                                                                                                              SCIENTIFIC LITERATURES; AUTHOR COCITATION; CITATION ANALYSIS; MANAGEMENT; TECHNOLOGY
    #> 17 PRODUCT-INNOVATION-MANAGEMENT; EXPLORATORY FACTOR-ANALYSIS; AUTHOR COCITATION ANALYSIS; INTELLECTUAL STRUCTURE; STRATEGIC-MANAGEMENT; BUSINESS ETHICS; ENTREPRENEURSHIP-RESEARCH; OPERATIONS MANAGEMENT; ABSORPTIVE-CAPACITY; INFORMATION-SCIENCE
    #> 18                                                                                                                                      RETROSPECTIVE EVALUATION; RESEARCH PRODUCTIVITY; ECONOMICS; AUTHORS; MANAGEMENT; ARTICLES; RANKINGS; DECADES
    #> 19                                                                                                                                                                                                                     MANAGEMENT SCIENCE; ECONOMICS
    #> 20                                                                                                                                                                                                                          PRODUCT; HISTORY; IMPACT
    #>                                 DOI Year LCS GCS
    #> 1      10.1016/0048-7333(85)90012-5 1985  22 310
    #> 2                    10.1086/209319 1993  34 127
    #> 3      10.1016/0040-1625(95)00022-3 1995  20 165
    #> 4     10.1016/S0040-1625(97)00050-4 1997  18 168
    #> 5                 10.1109/17.922473 2001  19 387
    #> 6           10.1111/1468-2370.00083 2002  18 117
    #> 7                   10.1002/smj.397 2004 108 667
    #> 8    10.1016/j.techfore.2006.04.004 2006  51 569
    #> 9  10.1111/j.1540-6520.2006.00126.x 2006  35 167
    #> 10 10.1111/j.1741-6248.2007.00092.x 2007  21 101
    #> 11                  10.1002/smj.659 2008  67 353
    #> 12         10.1177/0149206308319533 2008  32 271
    #> 13   10.1016/j.techfore.2007.05.005 2008  25 160
    #> 14     10.1016/j.respol.2012.03.009 2012  21 186
    #> 15     10.1016/j.respol.2012.03.008 2012  22 174
    #> 16                 10.1002/smj.2002 2013  20 132
    #> 17         10.1177/1094428114562629 2015  71 844
    #> 18    10.1016/j.jbusres.2015.04.006 2015  36 179
    #> 19       10.1016/j.ejor.2017.04.027 2017  22 165
    #> 20        10.1108/JBIM-04-2016-0079 2017  25 118

## Main Authors’ references (about bibliometrics)

Aria, M. & Cuccurullo, C. (2017). bibliometrix: An R-tool for
comprehensive science mapping analysis, *Journal of Informetrics*,
11(4), pp 959-975, Elsevier, DOI: 10.1016/j.joi.2017.08.007
(<https://doi.org/10.1016/j.joi.2017.08.007>)

Aria, M., Cuccurullo, C., D’Aniello, L., Misuraca, M., & Spano, M.
(2022). Thematic Analysis as a New Culturomic Tool: The Social Media
Coverage on COVID-19 Pandemic in Italy. *Sustainability*, 14(6), 3643,
(<https://doi.org/10.3390/su14063643>)

Belfiore, A., Salatino, A., & Osborne, F. (2022). Characterising
Research Areas in the field of AI. *arXiv preprint
arXiv:2205.13471*.(<https://doi.org/10.48550/arXiv.2205.13471>)

Belfiore, A., Cuccurullo, C., & Aria, M. (2022). IoT in healthcare: A
scientometric analysis. *Technological Forecasting and Social Change*,
184, 122001. (<https://doi.org/10.1016/j.techfore.2022.122001>)

D’Aniello, L., Spano, M., Cuccurullo, C., & Aria, M. (2022). Academic
Health Centers’ configurations, scientific productivity, and impact:
insights from the Italian setting. *Health Policy*.
(<https://doi.org/10.1016/j.healthpol.2022.09.007>)

Aria M., Misuraca M., Spano M. (2020) Mapping the evolution of social
research and data science on 30 years of Social Indicators Research,
*Social Indicators Research*. (DOI:
<https://doi.org/10.1007/s11205-020-02281-3>)

Aria M., Alterisio A., Scandurra A, Pinelli C., D’Aniello B, (2021) The
scholar’s best friend: research trends in dog cognitive and behavioural
studies, A*nimal Cognition*.
(<https://doi.org/10.1007/s10071-020-01448-2>)

Cuccurullo, C., Aria, M., & Sarto, F. (2016). Foundations and trends in
performance management. A twenty-five years bibliometric analysis in
business and public administration domains, *Scientometrics*, DOI:
10.1007/s11192-016-1948-8 (<https://doi.org/10.1007/s11192-016-1948-8>)

Cuccurullo, C., Aria, M., & Sarto, F. (2015). Twenty years of research
on performance management in business and public administration domains.
Presentation at the *Correspondence Analysis and Related Methods
conference (CARME 2015)* in September 2015
(<https://www.bibliometrix.org/documents/2015Carme_cuccurulloetal.pdf>)

Sarto, F., Cuccurullo, C., & Aria, M. (2014). Exploring healthcare
governance literature: systematic review and paths for future research.
*Mecosan*
(<https://www.francoangeli.it/Riviste/Scheda_Rivista.aspx?IDarticolo=52780&lingua=en>)

Cuccurullo, C., Aria, M., & Sarto, F. (2013). Twenty years of research
on performance management in business and public administration domains.
In *Academy of Management Proceedings* (Vol. 2013, No. 1, p. 14270).
Academy of Management
(<https://doi.org/10.5465/AMBPP.2013.14270abstract>)
