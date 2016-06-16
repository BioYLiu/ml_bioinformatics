# source("https://bioconductor.org/biocLite.R")
# biocLite("colonCA")
# library(colonCA)
data(colonCA)

exprs = log(exprs(colonCA))
pheno = pData(colonCA)

pvals = apply(exprs, 1, function(row) {
  t = row[pheno$class == 't']
  n = row[pheno$class == 'n']
  test = t.test(t, n)
  return(test$p.value)
})

diff = pvals < 0.0001
pvals[diff]
