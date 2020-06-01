## ---- echo = FALSE, message = FALSE-------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)
#library(dplyr, warn.conflicts = FALSE)
set.seed(1014)

## ----WOS bibtex, eval=FALSE, include=TRUE-------------------------------------
#  library(bibliometrix)
#  
#  file <- "https://www.bibliometrix.org/datasets/wos_bibtex.bib"
#  
#  M <- convert2df(file, dbsource = "wos", format = "bibtex")
#  
#  head(M["TC"])

## ----WOS plaintext, eval=FALSE, include=TRUE----------------------------------
#  file <- "https://www.bibliometrix.org/datasets/wos_plaintext.txt"
#  
#  M <- convert2df(file, dbsource = "wos", format = "plaintext")
#  
#  head(M["TC"])

## ----WOS endnote, eval=FALSE, include=TRUE------------------------------------
#  file <- "https://www.bibliometrix.org/datasets/wos_plaintext.ciw"
#  
#  M <- convert2df(file, dbsource = "wos", format = "endnote")
#  
#  head(M["TC"])

## ----Scopus csv, eval=FALSE, include=TRUE-------------------------------------
#  library(bibliometrix)
#  
#  file <- "https://www.bibliometrix.org/datasets/scopus_csv.csv"
#  
#  M <- convert2df(file, dbsource = "scopus", format = "csv")
#  
#  head(M["TC"])

## ----Scopus bibtex, eval=FALSE, include=TRUE----------------------------------
#  library(bibliometrix)
#  
#  file <- "https://www.bibliometrix.org/datasets/scopus_bibtex.bib"
#  
#  M <- convert2df(file, dbsource = "scopus", format = "bibtex")
#  
#  head(M["TC"])

## ----Dimensions Excel, eval=FALSE, include=TRUE-------------------------------
#  file <- "https://www.bibliometrix.org/datasets/dimensions_excel.xlsx"
#  
#  M <- convert2df(file, dbsource = "dimensions", format = "excel")
#  
#  head(M["TC"])

## ----Dimensions csv, eval=FALSE, include=TRUE---------------------------------
#  file <- "https://www.bibliometrix.org/datasets/dimensions_csv.csv"
#  
#  M <- convert2df(file, dbsource = "dimensions", format = "csv")
#  
#  head(M["TC"])

## ----Pubmed txt, eval=FALSE, include=TRUE-------------------------------------
#  file <- "https://www.bibliometrix.org/datasets/pubmed_txt.txt"
#  
#  M <- convert2df(file, dbsource = "pubmed", format = "pubmed")
#  
#  head(M["TC"])

## ----Cochrane plaintext, eval=FALSE, include=TRUE-----------------------------
#  file <- "https://www.bibliometrix.org/datasets/cochrane_plaintext.txt"
#  
#  M <- convert2df(file, dbsource = "cochrane", format = "plaintext")
#  
#  head(M["TC"])

