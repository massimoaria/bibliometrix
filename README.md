[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](http://www.bibliometrix.org/passing.png)](http://www.bibliometrix.org)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/bibliometrix)](https://github.com/metacran/cranlogs.app)
[![cran version](http://www.r-pkg.org/badges/version/bibliometrix)](https://cran.r-project.org/package=bibliometrix)


bibliometrix
====================================

## An R Tool for quantitative research in scientometrics and bibliometrics.

**bibliometrix** package provides a set of tools for quantitative research in bibliometrics and scientometrics.

Bibliometrics turns the main tool of science, quantitative analysis, on itself. Essentially, bibliometrics is the application of quantitative analysis and statistics to publications such as journal articles and their accompanying citation counts. Quantitative evaluation of publication and citation data is now used in almost all scientific fields to evaluate growth, maturity, leading authors, conceptual and intellectual maps, trends of a scientific community.

Bibliometrics is also used in research performance evaluation, especially in university and government labs, and also by policymakers, research directors and administrators, information specialists and librarians, and scholars themselves.

**bibliometrix** supports scholars in three key phases of analysis:

* Data importing and conversion to R format;

* Bibliometric analysis of a publication dataset;

* Building and plotting matrices for co-citation, coupling, collaboration, and co-word analysis. Matrices are the input data for performing network analysis, multiple correspondence analysis, and any other data reduction techniques.


## Installation


Stable version from CRAN


```r
install.packages("bibliometrix")
```

Or development version from GitHub


```r
install.packages("devtools")
devtools::install_github("massimoaria/bibliometrix")
```

Load `bibliometrix`


```r
library('bibliometrix')
```


## Data loading and converting

The export file can be read by R using the function *readFiles*:


```{r Data loading}
D <- readFiles("http://www.bibliometrix.org/datasets/savedrecs.bib")
```

D is a large character vector. 
*readFiles* argument contains the name of files downloaded from SCOPUS or ISI WOS website.

The function *readFiles* combines all the text files onto a single large character vector. Furthermore, the format is converted into UTF-8.

es. D <- readFiles("file1.txt","file2.txt", ...)


The object D can be converted in a  data frame using the function *convert2df*:


```{r Data converting}
M <- convert2df(D, dbsource = "isi", format = "bibtex")
```

*convert2df* creates a bibliographic data frame with cases corresponding to manuscripts and variables to Field Tag in the original export file.

Each manuscript contains several elements, such as authors' names, title, keywords and other information. All these elements constitute the bibliographic attributes of a document, also called metadata.

Data frame columns are named using the standard Clarivate Analytics WoS Field Tag codify [![link](http://www.bibliometrix.org/documents/Field_Tags_bibliometrix.pdf)](http://www.bibliometrix.org/documents/Field_Tags_bibliometrix.pdf). 

