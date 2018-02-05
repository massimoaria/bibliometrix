## ----echo=FALSE----------------------------------------------------------
cat(paste("bibliometrix ",packageVersion("bibliometrix")))

## ----Package citation, echo=FALSE----------------------------------------
citation("bibliometrix")

## ----bibliometrix loading------------------------------------------------
library(bibliometrix)   ### load bibliometrix package

## ----Data loading--------------------------------------------------------

D <- readFiles("http://www.bibliometrix.org/datasets/savedrecs.bib")

## ----Data converting-----------------------------------------------------
M <- convert2df(D, dbsource = "isi", format = "bibtex")

## ----biblioAnalysis------------------------------------------------------
results <- biblioAnalysis(M, sep = ";")

## ----summary generic function--------------------------------------------
S <- summary(object = results, k = 10, pause = FALSE)

## ----plot generic function-----------------------------------------------
plot(x = results, k = 10, pause = FALSE)

## ------------------------------------------------------------------------
# M$CR[1]

## ----Article citation----------------------------------------------------
CR <- citations(M, field = "article", sep = ".  ")
CR$Cited[1:10]

## ----Author citation-----------------------------------------------------
CR <- citations(M, field = "author", sep = ".  ")
CR$Cited[1:10]

## ----Local Author citation-----------------------------------------------
CR <- localCitations(M, sep = ".  ")
CR$Authors[1:10,]
CR$Papers[1:10,]

## ----Dominance Ranking---------------------------------------------------
DF <- dominance(results, k = 10)
DF

## ----h-index-------------------------------------------------------------

indices <- Hindex(M, authors="BORNMANN L", sep = ";",years=10)

# Bornmann's impact indices:
indices$H

# Bornmann's citations
indices$CitationList


## ----h-index 10 authors--------------------------------------------------

authors=gsub(","," ",names(results$Authors)[1:10])

indices <- Hindex(M, authors, sep = ";",years=50)

indices$H

## ----Lotka law-----------------------------------------------------------
L <- lotka(results)

# Author Productivity. Empirical Distribution
L$AuthorProd

# Beta coefficient estimate
L$Beta

# Constant
L$C

# Goodness of fit
L$R2

# P-value of K-S two sample test
L$p.value


## ----Lotka law comparison, out.width='300px', dpi=200--------------------
# Observed distribution
Observed=L$AuthorProd[,3]

# Theoretical distribution with Beta = 2
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))

plot(L$AuthorProd[,1],Theoretical,type="l",col="red",ylim=c(0, 1), xlab="Articles",ylab="Freq. of Authors",main="Scientific Productivity")
lines(L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("Theoretical (B=2)","Observed"),col=c("red","blue"),lty = c(1,1,1),cex=0.6,bty="n")

## ----Bipartite network---------------------------------------------------
A <- cocMatrix(M, Field = "SO", sep = ";")

## ----Most relevant sources-----------------------------------------------
sort(Matrix::colSums(A), decreasing = TRUE)[1:5]

## ------------------------------------------------------------------------
# A <- cocMatrix(M, Field = "CR", sep = ".  ")

## ------------------------------------------------------------------------
# A <- cocMatrix(M, Field = "AU", sep = ";")

## ------------------------------------------------------------------------
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
# A <- cocMatrix(M, Field = "AU_CO", sep = ";")

## ------------------------------------------------------------------------
# A <- cocMatrix(M, Field = "DE", sep = ";")

## ------------------------------------------------------------------------
# A <- cocMatrix(M, Field = "ID", sep = ";")

## ------------------------------------------------------------------------
# NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ".  ")

## ----similarity, fig.height=7, fig.width=7, warning=FALSE----------------
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")

# plot authors' similarity (first 20 authors), using salton similarity index
net=networkPlot(NetMatrix, normalize = "salton", weighted=T, n = 20, Title = "Authors' Coupling", type = "fruchterman", size=FALSE,remove.multiple=TRUE)


## ------------------------------------------------------------------------
# NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ".  ")

## ------------------------------------------------------------------------
# NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "authors", sep = ";")

## ------------------------------------------------------------------------
# NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

## ----Country collaboration, fig.height=7, fig.width=7, warning=FALSE-----
# Create a country collaboration network

M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.8)


## ----Co-citation network, fig.height=7, fig.width=7, warning=FALSE-------
# Create a co-citation network

NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ".  ")

# Plot the network
net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)


## ----Keyword c-occurrences, fig.height=7, fig.width=7, warning=FALSE-----
# Create keyword co-occurrencies network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)


## ----Co-Word Analysis, fig.height=7, fig.width=7, warning=FALSE----------

# Conceptual Structure using keywords

CS <- conceptualStructure(M,field="ID", minDegree=4, k.max=5, stemming=FALSE, labelsize=10)


## ----Historical Co-citation network, fig.height=9, fig.width=7, warning=FALSE----
# Create a historical citation network

histResults <- histNetwork(M, n = 20, sep = ".  ")

# Plot a historical co-citation network
net <- histPlot(histResults, size = FALSE,label=TRUE, arrowsize = 0.5)


