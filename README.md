
bibliometrix
====================================

## An R-tool for comprehensive science mapping analysis. 

[![bibliometrix: An R-tool for comprehensive science mapping analysis.](https://www.bibliometrix.org/altro/JOI-badge.svg)](https://doi.org/10.1016/j.joi.2017.08.007)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://www.bibliometrix.org/passing.png)](https://www.bibliometrix.org)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/bibliometrix)](https://github.com/metacran/cranlogs.app)
[![cran version](http://www.r-pkg.org/badges/version/bibliometrix)](https://cran.r-project.org/package=bibliometrix)


<p align="center">
<img src="https://www.bibliometrix.org/logo.png" width="300"  />
</p>

## Overview

**bibliometrix** package provides a set of tools for quantitative research in bibliometrics and scientometrics.

Bibliometrics turns the main tool of science, quantitative analysis, on itself. Essentially, bibliometrics is the application of quantitative analysis and statistics to publications such as journal articles and their accompanying citation counts. Quantitative evaluation of publication and citation data is now used in almost all scientific fields to evaluate growth, maturity, leading authors, conceptual and intellectual maps, trends of a scientific community.

Bibliometrics is also used in research performance evaluation, especially in university and government labs, and also by policymakers, research directors and administrators, information specialists and librarians, and scholars themselves.

**bibliometrix** supports scholars in three key phases of analysis:

* Data importing and conversion to R format;

* Bibliometric analysis of a publication dataset;

* Building and plotting matrices for co-citation, coupling, collaboration, and co-word analysis. Matrices are the input data for performing network analysis, multiple correspondence analysis, and any other data reduction techniques.


## biblioshiny

**bibliometrix** includes **biblioshiny: bibliometrix for no-coders**

**biblioshiny** is a *shiny app providing a web-interface for bibliometrix*.

It supports scholars in easy use of the main features of bibliometrix:

* Data importing and conversion to data frame collection

* Data filtering

* Analytics and Plots for three different level metrics:

    - Sources  

    - Authors  

    - Documents   

* Analysis of three structures of Knowledge (K-structures):  

    - Conceptual Structure  

    - Intellectual Structure  

    - Social Strucutre



### How to use biblioshiny

Please follow the biblioshiny tutorial at www.bibliometrix.org/biblioshiny 

## Suggested citation

If you use this package for your research, we would appreciate a citation.

To cite bibliometrix in publications, please use:

Aria, M. & Cuccurullo, C. (2017) **bibliometrix: An R-tool for comprehensive science mapping analysis**, *Journal of Informetrics*, 11(4), pp 959-975, Elsevier.



## Community

Official website:   https://www.bibliometrix.org

CRAN page:          https://cran.r-project.org/package=bibliometrix
    
GitHub repository:  https://github.com/massimoaria/bibliometrix

Tutorial:           http://htmlpreview.github.io/?https://github.com/massimoaria/bibliometrix/master/vignettes/bibliometrix-vignette.html

Slides:             https://www.slideshare.net/MassimoAria/bibliometrix-phd-seminar




## Installation


Stable version from CRAN


```r
install.packages("bibliometrix")
```

Developers version from GitHub


```r
install.packages("devtools")
devtools::install_github("massimoaria/bibliometrix")
```

Load `bibliometrix`


```r
library('bibliometrix')
```




## Data loading and converting

The export file can be read and converted using by R using the function *convert2df*:

**convert2df**(*file*, *dbsource*, *format*)

The argument *file* is a character vector containing the name of export files downloaded from SCOPUS, Clarivate Analytics WOS, Digital Science Dimenions, PubMed or Cochrane CDSR website. *file* can also contains the name of a json/xlm object download using  Digital Science Dimenions or PubMed APIs (through the packages *dimensionsR* and *pubmedR*.

es. file <- c("file1.txt","file2.txt", ...)


```{r Data loading and Converting}
## An example from bibliometrix vignettes

file <- "https://www.bibliometrix.org/datasets/savedrecs.bib"

M <- convert2df(file = file, dbsource = "isi", format = "bibtex")
```

*convert2df* creates a bibliographic data frame with cases corresponding to manuscripts and variables to Field Tag in the original export file.

Each manuscript contains several elements, such as authors' names, title, keywords and other information. All these elements constitute the bibliographic attributes of a document, also called metadata.

Data frame columns are named using the standard Clarivate Analytics WoS Field Tag codify [(https://www.bibliometrix.org/documents/Field_Tags_bibliometrix.pdf)](https://www.bibliometrix.org/documents/Field_Tags_bibliometrix.pdf). 




## Bibliometric Analysis

The first step is to perform a descriptive analysis of the bibliographic data frame.

The function *biblioAnalysis* calculates main bibliometric measures using this syntax:
 
```{r biblioAnalysis}
results <- biblioAnalysis(M, sep = ";")
```

The function *biblioAnalysis* returns an object of class "bibliometrix".


To summarize main results of the bibliometric analysis, use the generic function *summary*.
It displays main information about the bibliographic data frame and several tables, such as annual scientific production, top manuscripts per number of citations, most productive authors, most productive countries, total citation per country, most relevant sources (journals) and most relevant keywords.

*summary* accepts two additional arguments. *k* is a formatting value that indicates the number of rows of each table. *pause* is a logical value (TRUE or FALSE) used to allow (or not) pause in screen scrolling.
Choosing k=10 you decide to see the first 10 Authors, the first 10 sources, etc.

```{r summary generic function}
S <- summary(object = results, k = 10, pause = FALSE)
```

Some basic plots can be drawn using the generic function plot:

```{r plot generic function, fig.width=7}
plot(x = results, k = 10, pause = FALSE)
```




## Bibliographic network matrices

Manuscript's attributes are connected to each other through the manuscript itself: author(s) to journal, keywords to publication date, etc.

These connections of different attributes generate bipartite networks that can be represented as rectangular matrices (Manuscripts x Attributes).

Furthermore, scientific publications regularly contain references to
other scientific works. This generates a further network, namely, co-citation or coupling network.

These networks are analyzed in order to capture meaningful properties of the underlying research system, and in particular to determine the influence of bibliometric units such as scholars and journals.



### biblioNetwork function

The function *biblioNetwork* calculates, starting from a bibliographic data frame, the most frequently used networks: Coupling, Co-citation, Co-occurrences, and Collaboration.

*biblioNetwork* uses two arguments to define the network to compute:

* *analysis* argument can be "co-citation", "coupling", "collaboration",  or "co-occurrences".

* *network* argument can be "authors", "references", "sources", "countries", "universities", "keywords", "author_keywords", "titles" and "abstracts".

i.e. the following code calculates a classical co-citation network:

```{r}
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ".  ")
```




## Visualizing bibliographic networks


All bibliographic networks can be graphically visualized or modeled.

Using the function *networkPlot*, you can plot a network created by *biblioNetwork* using R routines.

The main argument of *networkPlot* is type. It indicates the network map layout: circle, kamada-kawai, mds, etc.

In the following, we propose some examples.



### Country Scientific Collaboration

```{r Country collaboration, fig.height=7, fig.width=7, warning=FALSE}
# Create a country collaboration network

M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.8)

```



### Co-Citation Network

```{r Co-citation network, fig.height=7, fig.width=7, warning=FALSE}
# Create a co-citation network

NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ".  ")

# Plot the network
net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)

```



### Keyword co-occurrences

```{r Keyword c-occurrences, fig.height=7, fig.width=7, warning=FALSE}
# Create keyword co-occurrences network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

```




## Co-Word Analysis: The conceptual structure of a field 

The aim of the co-word analysis is to map the conceptual structure of a framework using the word co-occurrences in a bibliographic collection.

The analysis can be performed through dimensionality reduction techniques such as Multidimensional Scaling (MDS), Correspondence Analysis (CA) or Multiple Correspondence Analysis (MCA). 

Here, we show an example using the function *conceptualStructure* that performs a CA or MCA to draw a conceptual structure of the field and K-means clustering to identify clusters of documents which express common concepts. Results are plotted on a two-dimensional map.

*conceptualStructure* includes natural language processing (NLP) routines (see the function *termExtraction*) to extract terms from titles and abstracts.  In addition, it implements the Porter's stemming algorithm to reduce inflected (or sometimes derived) words to their word stem, base or root form.


```{r Co-Word Analysis, fig.height=9, fig.width=9, warning=FALSE}

# Conceptual Structure using keywords (method="CA")

CS <- conceptualStructure(M,field="ID", method="CA", minDegree=4, k.max=8, stemming=FALSE, labelsize=10, documents=10)

```




## Historical Direct Citation Network

The historiographic map is a graph proposed by E. Garfield to represent a chronological network map of most relevant direct citations resulting from a bibliographic collection.

The function histNetwork generates a chronological direct citation network matrix which can be plotted using *histPlot*:

```{r Historical Co-citation network, fig.height=9, fig.width=7, warning=FALSE}
# Create a historical citation network

histResults <- histNetwork(M, sep = ".  ")

# Plot a historical co-citation network
net <- histPlot(histResults, n=20, size = FALSE,label=TRUE, arrowsize = 0.5)

```




## Main Authors' references (about bibliometrics)

Aria, M. & Cuccurullo, C. (2017).  *bibliometrix*: An R-tool for comprehensive science mapping
  analysis, *Journal of Informetrics*, 11(4), pp 959-975, Elsevier, DOI: 10.1016/j.joi.2017.08.007 (https://doi.org/10.1016/j.joi.2017.08.007).

Cuccurullo, C., Aria, M., & Sarto, F. (2016). Foundations and trends in performance management. A twenty-five years bibliometric analysis in business and public administration domains, *Scientometrics*, DOI: 10.1007/s11192-016-1948-8 (https://doi.org/10.1007/s11192-016-1948-8).


Cuccurullo, C., Aria, M., & Sarto, F.  (2015). Twenty years of research on performance management in business and public administration domains. Presentation at the *Correspondence Analysis and Related Methods conference (CARME 2015)* in September 2015 (https://www.bibliometrix.org/documents/2015Carme_cuccurulloetal.pdf).


Sarto, F., Cuccurullo, C., & Aria, M. (2014). Exploring healthcare governance literature: systematic review and paths for future research. *Mecosan* (https://www.francoangeli.it/Riviste/Scheda_Rivista.aspx?IDarticolo=52780&lingua=en).


Cuccurullo, C., Aria, M., & Sarto, F. (2013). Twenty years of research on performance management in business and public administration domains. In *Academy of Management Proceedings* (Vol. 2013, No. 1, p. 14270). Academy of Management (https://doi.org/10.5465/AMBPP.2013.14270abstract).
