
# install package colonCA (as root in Ubuntu)
# > source("https://bioconductor.org/biocLite.R")
# > biocLite("colonCA")

# 1a. Load `colonCA` dataset and use the function prcomp to calculate the PCA
# values (_____ / 4)

library(colonCA)
# colonCA dataset is an ExpressionSet object
data(colonCA)

colon.ds = log(exprs(colonCA))
colon.ds = t(colon.ds)
#colon.ds = as.data.frame(colon.ds)
#colon.ds$diagnostic = colonCA$class

# we apply the transponse function ?
colon.pca = prcomp(colon.ds,
                   center = TRUE,
                   scale. = TRUE
                   )
#print(colon.pca$rotation[1:5, 1:5])

# 1b. 2D PCA plot from normal patients and patients with cancer (_____ / 4)
# What do you observe?

par(mfrow=c(1, 2))

negative = apply(colon.pca$rotation[, colonCA$class == 'n'], 1, sum)
positive = apply(colon.pca$rotation[, colonCA$class == 't'], 1, sum)

plot(negative ~ positive,
     pch = 21,
     bg = c('red', 'green')[unclass(colonCA$class)],
    )

# 1c. screeplot of eigenvalues (_____ / 2)
# colon.pca contains a `sdev` component
screeplot(colon.pca, npcs = 62)

# 1d. principal components analysis (_____ / 4)
summary(colon.pca)

# Which proportion of the overall variance do the first 2 principal components
# explain?  => 0.55905
# How many principal components would you need to explain 90% and 95% of the
# overall variance?
# To explain 90% we need the first 21 PCA whose cumulative proportion is 0.90082
# To explain 95% we need the first 34 PCA whose cumulative proportion is 0.94972
