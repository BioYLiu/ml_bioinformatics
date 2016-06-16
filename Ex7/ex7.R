library(colonCA)

# install EMA package is boring:
#
# biomaRt dependencies
#   Ubuntu: sudo apt-get install libcurl4-openssl-dev libxml2-dev
#   install.packages("XML")
#   install.packages("RCurl")
#
# source("https://bioconductor.org/biocLite.R")
# biocLite("biomaRt")
#
# The rest of EMA dependencies
#
# biocLite("affy")
# biocLite("siggenes")
# biocLite("gcrma")
# biocLite("AnnotationDbi")

library(EMA)

# colonCA dataset is an ExpressionSet object
data(colonCA)

colon.ds = log(exprs(colonCA))
colon.ds = as.data.frame(colon.ds)

#negative = colon.ds[, colonCA$class == 'n']
#positive = colon.ds[, colonCA$class == 't']

pvalues <- apply(colon.ds, 1,
                 function(x) t.test(x ~ colonCA$class)$p.value)
alpha = 0.0001
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
