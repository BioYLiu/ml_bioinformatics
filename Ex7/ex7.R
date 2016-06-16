library(colonCA)
library(EMA)

# install EMA package is tedious:
#
# Install biomaRt dependencies:
# Ubuntu: sudo apt-get install libcurl4-openssl-dev libxml2-dev
# install.packages("XML")
# install.packages("RCurl")
#
# Install EMA dependencies and EMA
#
# source("https://bioconductor.org/biocLite.R")
# biocLite("biomaRt")
# biocLite("affy")
# biocLite("siggenes")
# biocLite("gcrma")
# biocLite("AnnotationDbi")
# install.packages("EMA")


# Task 1A
# colonCA dataset is an ExpressionSet object
data(colonCA)
colon.ds <- log(exprs(colonCA))

pvalues <- apply(colon.ds, 1, function(x) {
  return (t.test(x[colonCA$class=='t'], x[colonCA$class=='n'])$p.value)
})
                   
alpha = 0.0001
result <- pvalues[pvalues <= alpha]


# Task 1B
diff.exp = colon.ds[pvalues <= alpha,]
cluster.complete.genes = clustering(diff.exp, metric="pearson",  method = "complete")
cluster.complete.samples = clustering(t(diff.exp), metric="pearson",  method = "complete")

cluster.average.genes = clustering(diff.exp, metric="pearson", method = "average")
cluster.average.samples = clustering(t(diff.exp), metric="pearson", method = "average")

cluster.ward.genes = clustering(diff.exp, metric="pearson", method = "ward")
cluster.ward.samples = clustering(t(diff.exp), metric="pearson", method = "ward")

# Heatmap for cluster with average linkage
clustering.plot(
                tree = cluster.average.genes,
                tree.sup = cluster.average.samples,
                data = diff.exp
                )

# Heatmap for cluster with complete linkage
clustering.plot(
                tree = cluster.complete.genes,
                tree.sup = cluster.complete.samples,
                data = diff.exp
                )

# Heatmap for cluster with Ward's method
clustering.plot(
                tree = cluster.ward.genes,
                tree.sup = cluster.ward.samples,
                data = diff.exp
                )
