## ----echo=FALSE---------------------------------------------------------------
cat(paste("bibliometrix ",packageVersion("bibliometrix")))

## ----Package citation, eval=FALSE, include=FALSE------------------------------
#  citation("bibliometrix")

## ----bibliometrix loading-----------------------------------------------------
library(bibliometrix)   ### load bibliometrix package

## ----Data converting----------------------------------------------------------
file <- "https://www.bibliometrix.org/datasets/savedrecs.bib"

M <- convert2df(file = file, dbsource = "isi", format = "bibtex")

## ----biblioAnalysis-----------------------------------------------------------
results <- biblioAnalysis(M, sep = ";")

## ----summary generic function-----------------------------------------------------------------------------------------
options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)

## ----plot generic function, fig.width=7-------------------------------------------------------------------------------
plot(x = results, k = 10, pause = FALSE)

## ---------------------------------------------------------------------------------------------------------------------
# M$CR[1]

## ----Article citation-------------------------------------------------------------------------------------------------
CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10])

## ----Author citation--------------------------------------------------------------------------------------------------
CR <- citations(M, field = "author", sep = ";")
cbind(CR$Cited[1:10])

## ----Local Author citation--------------------------------------------------------------------------------------------
CR <- localCitations(M, sep = ";")
CR$Authors[1:10,]
CR$Papers[1:10,]

## ----Dominance Ranking------------------------------------------------------------------------------------------------
DF <- dominance(results, k = 10)
DF

## ----h-index----------------------------------------------------------------------------------------------------------

indices <- Hindex(M, field = "author", elements="BORNMANN L", sep = ";", years = 10)

# Bornmann's impact indices:
indices$H

# Bornmann's citations
indices$CitationList


## ----h-index 10 authors-----------------------------------------------------------------------------------------------

authors=gsub(","," ",names(results$Authors)[1:10])

indices <- Hindex(M, field = "author", elements=authors, sep = ";", years = 50)

indices$H

## ----AuthorProdOverTime, fig.height=6, fig.width=8--------------------------------------------------------------------

topAU <- authorProdOverTime(M, k = 10, graph = TRUE)

## Table: Author's productivity per year
head(topAU$dfAU)

## Table: Auhtor's documents list
#head(topAU$dfPapersAU)

## ----Lotka law--------------------------------------------------------------------------------------------------------
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


## ----Lotka law comparison, out.width='300px', dpi=200-----------------------------------------------------------------
# Observed distribution
Observed=L$AuthorProd[,3]

# Theoretical distribution with Beta = 2
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))

plot(L$AuthorProd[,1],Theoretical,type="l",col="red",ylim=c(0, 1), xlab="Articles",ylab="Freq. of Authors",main="Scientific Productivity")
lines(L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("Theoretical (B=2)","Observed"),col=c("red","blue"),lty = c(1,1,1),cex=0.6,bty="n")

## ----Bipartite network------------------------------------------------------------------------------------------------
A <- cocMatrix(M, Field = "SO", sep = ";")

## ----Most relevant sources--------------------------------------------------------------------------------------------
sort(Matrix::colSums(A), decreasing = TRUE)[1:5]

## ---------------------------------------------------------------------------------------------------------------------
A <- cocMatrix(M, Field = "CR", sep = ".  ")

## ---------------------------------------------------------------------------------------------------------------------
 A <- cocMatrix(M, Field = "AU", sep = ";")

## ---------------------------------------------------------------------------------------------------------------------
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
# A <- cocMatrix(M, Field = "AU_CO", sep = ";")

## ---------------------------------------------------------------------------------------------------------------------
A <- cocMatrix(M, Field = "DE", sep = ";")

## ---------------------------------------------------------------------------------------------------------------------
A <- cocMatrix(M, Field = "ID", sep = ";")

## ---------------------------------------------------------------------------------------------------------------------
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ".  ")

## ----similarity, fig.height=9, fig.width=9, warning=FALSE-------------------------------------------------------------
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")


#net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 100, Title = "Authors' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)


## ---------------------------------------------------------------------------------------------------------------------
# NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ".  ")

## ---------------------------------------------------------------------------------------------------------------------
# NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "authors", sep = ";")

## ---------------------------------------------------------------------------------------------------------------------
# NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

## ---------------------------------------------------------------------------------------------------------------------
# An example of a classical keyword co-occurrences network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
netstat <- networkStat(NetMatrix)

## ---------------------------------------------------------------------------------------------------------------------
names(netstat$network)

## ---------------------------------------------------------------------------------------------------------------------
names(netstat$vertex)

## ---------------------------------------------------------------------------------------------------------------------
summary(netstat, k=10)

## ----Country collaboration, fig.height=7, fig.width=7, warning=FALSE--------------------------------------------------
# Create a country collaboration network

M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")


## ----Co-citation network, fig.height=7, fig.width=7, warning=FALSE----------------------------------------------------
# Create a co-citation network

# NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")

# Plot the network
#net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)


## ----Keyword c-occurrences, fig.height=7, fig.width=7, warning=FALSE--------------------------------------------------
# Create keyword co-occurrences network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)


## ----Co-Word Analysis, fig.height=9, fig.width=9, warning=FALSE-------------------------------------------------------

# Conceptual Structure using keywords (method="CA")

CS <- conceptualStructure(M,field="ID", method="CA", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=10)


## ----Historical Co-citation network, fig.height=7, fig.width=10, warning=FALSE--------------------------------------------------
# Create a historical citation network
options(width=130)
histResults <- histNetwork(M, min.citations = 1, sep = ";")

# Plot a historical co-citation network
net <- histPlot(histResults, n=15, size = 10, labelsize=5)


