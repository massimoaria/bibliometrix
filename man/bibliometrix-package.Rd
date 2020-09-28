\name{bibliometrix-package}
\alias{bibliometrix-package}
\alias{bibliometrix}
\docType{package}
\title{
\packageTitle{bibliometrix}
}
\description{
\packageDescription{bibliometrix}
}
\details{

INSTALLATION

- Stable version from CRAN:

install.packages("bibliometrix")

- Or development version from GitHub:

install.packages("devtools")
devtools::install_github("massimoaria/bibliometrix")

- Load "bibliometrix"

library('bibliometrix')



DATA LOADING AND CONVERTING

The export file can be imported and converted by R using the function *convert2df*:

file <- ("https://www.bibliometrix.org/datasets/savedrecs.txt")

M <- convert2df(file, dbsource = "wos", format = "bibtex")

*convert2df* creates a bibliographic data frame with cases corresponding to manuscripts and variables to Field Tag in the original export file.
Each manuscript contains several elements, such as authors' names, title, keywords and other information. All these elements constitute the bibliographic attributes of a document, also called metadata.
Data frame columns are named using the standard Clarivate Analytics WoS Field Tag codify. 



BIBLIOMETRIC ANALYSIS

The first step is to perform a descriptive analysis of the bibliographic data frame.
The function *biblioAnalysis* calculates main bibliometric measures using this syntax:

results <- biblioAnalysis(M, sep = ";")

The function *biblioAnalysis* returns an object of class "bibliometrix".

To summarize main results of the bibliometric analysis, use the generic function *summary*.
It displays main information about the bibliographic data frame and several tables, such as annual scientific production, top manuscripts per number of citations, most productive authors, most productive countries, total citation per country, most relevant sources (journals) and most relevant keywords.
*summary* accepts two additional arguments. *k* is a formatting value that indicates the number of rows of each table. *pause* is a logical value (TRUE or FALSE) used to allow (or not) pause in screen scrolling.
Choosing k=10 you decide to see the first 10 Authors, the first 10 sources, etc.

S <- summary(object = results, k = 10, pause = FALSE)

Some basic plots can be drawn using the generic function plot:

plot(x = results, k = 10, pause = FALSE)



BIBLIOGRAPHIC NETWORK MATRICES

Manuscript's attributes are connected to each other through the manuscript itself: author(s) to journal, keywords to publication date, etc.
These connections of different attributes generate bipartite networks that can be represented as rectangular matrices (Manuscripts x Attributes).
Furthermore, scientific publications regularly contain references to
other scientific works. This generates a further network, namely, co-citation or coupling network.
These networks are analyzed in order to capture meaningful properties of the underlying research system, and in particular to determine the influence of bibliometric units such as scholars and journals.


*biblioNetwork* function

The function *biblioNetwork* calculates, starting from a bibliographic data frame, the most frequently used networks: Coupling, Co-citation, Co-occurrences, and Collaboration.
*biblioNetwork* uses two arguments to define the network to compute:
- *analysis* argument can be "co-citation", "coupling", "collaboration",  or "co-occurrences".
- *network* argument can be "authors", "references", "sources", "countries", "universities", "keywords", "author_keywords", "titles" and "abstracts".

i.e. the following code calculates a classical co-citation network:

NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")



VISUALIZING BIBLIOGRAPHIC NETWORKS 

All bibliographic networks can be graphically visualized or modeled.
Using the function *networkPlot*, you can plot a network created by *biblioNetwork* using R routines.

The main argument of *networkPlot* is type. It indicates the network map layout: circle, kamada-kawai, mds, etc.

In the following, we propose some examples.



### Country Scientific Collaboration

# Create a country collaboration network

M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")

NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network

net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.8)



### Co-Citation Network

# Create a co-citation network

NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")

# Plot the network

net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)


### Keyword co-occurrences

# Create keyword co-occurrences network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network

net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)



CO-WORD ANALYSIS: THE CONCEPTUAL STRUCTURE OF A FIELD 

The aim of the co-word analysis is to map the conceptual structure of a framework using the word co-occurrences in a bibliographic collection.
The analysis can be performed through dimensionality reduction techniques such as Multidimensional Scaling (MDS), Correspondence Analysis (CA) or Multiple Correspondence Analysis (MCA). 
Here, we show an example using the function *conceptualStructure* that performs a CA or MCA to draw a conceptual structure of the field and K-means clustering to identify clusters of documents which express common concepts. Results are plotted on a two-dimensional map.
*conceptualStructure* includes natural language processing (NLP) routines (see the function *termExtraction*) to extract terms from titles and abstracts.  In addition, it implements the Porter's stemming algorithm to reduce inflected (or sometimes derived) words to their word stem, base or root form.

# Conceptual Structure using keywords (method="MCA")

CS <- conceptualStructure(M,field="ID", method="MCA", minDegree=4, clust=4 ,k.max=8, stemming=FALSE, labelsize=10, documents=10)



HISTORICAL DIRECT CITATION NETWORK

The historiographic map is a graph proposed by E. Garfield to represent a chronological network map of most relevant direct citations resulting from a bibliographic collection.
The function histNetwork generates a chronological direct citation network matrix which can be plotted using *histPlot*:

# Create a historical citation network

histResults <- histNetwork(M, sep = ";")

# Plot a historical co-citation network

net <- histPlot(histResults, size = 10)

}

\author{
\packageAuthor{bibliometrix}

Maintainer: \packageMaintainer{bibliometrix}
}
\references{
Aria, M. & Cuccurullo, C. (2017).  *bibliometrix*: An R-tool for comprehensive science mapping analysis, *Journal of Informetrics*, 11(4), pp 959-975, Elsevier, DOI: 10.1016/j.joi.2017.08.007 (https://doi.org/10.1016/j.joi.2017.08.007).

Cuccurullo, C., Aria, M., & Sarto, F. (2016). Foundations and trends in performance management. A twenty-five years bibliometric analysis in business and public administration domains, *Scientometrics*, DOI: 10.1007/s11192-016-1948-8 (https://doi.org/10.1007/s11192-016-1948-8).

Cuccurullo, C., Aria, M., & Sarto, F.  (2015). Twenty years of research on performance management in business and public administration domains. Presentation at the *Correspondence Analysis and Related Methods conference (CARME 2015)* in September 2015 (https://www.bibliometrix.org/documents/2015Carme_cuccurulloetal.pdf).

Sarto, F., Cuccurullo, C., & Aria, M. (2014). Exploring healthcare governance literature: systematic review and paths for future research. *Mecosan* (https://www.francoangeli.it/Riviste/Scheda_Rivista.aspx?IDarticolo=52780&lingua=en).

Cuccurullo, C., Aria, M., & Sarto, F. (2013). Twenty years of research on performance management in business and public administration domains. In *Academy of Management Proceedings* (Vol. 2013, No. 1, p. 14270). Academy of Management (https://doi.org/10.5465/AMBPP.2013.14270abstract).

}

\concept{Science Mapping} 
\concept{Clarivate Analytics Web of Science}
\concept{Scopus}
\concept{citations}
\concept{co-citations}
\concept{network}
\concept{co-authors}
\concept{co-occurences}
\concept{collaboration}
\concept{co-word analysis}

