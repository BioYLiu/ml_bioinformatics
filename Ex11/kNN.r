
library(colonCA)
data(colonCA)

#
# knn function implements the kNN algorithm
# @param k The number of nearest neighbors
# @param dataset The dataset we will work with
# @param dataset.factor The factor values of the dataset, that is, the labels
# @param training The number of training elements
# @return A vector with the correspondant classification
#

knn <- function(k, dataset, dataset.factor, training=50) {

    classification = vector()
    distances <- as.matrix(dist(dataset))
    no.labels = length(levels(dataset.factor))

    indexes = sample(1:nrow(dataset))
    shuffled.dataset <- dataset[indexes,]
    training.dataset <- shuffled.dataset[1:training,]

    for (i in indexes[1:training]) {

        ordered.row <- order(distances[,i])
        k.nearest.neighbors = ordered.row[2:(k+1)]

        # majority voting
        voting = table(colonCA$class[k.nearest.neighbors])
        classification <- append(classification, names(sort(voting))[no.labels])
    }
    result <- list(indexes[1:training], classification)
}

colon.ds <- exprs(colonCA)
colon.ds <- t(colon.ds)
colon.ds <- colon.ds[1:20, 1:8]

# knn(3, iris)
classification <- knn(3, colon.ds, colonCA$class, training=3)
