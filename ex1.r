##TODO:
# A. organize code (Shinho)
# B. find a way to represent species with string (Omar)
# C. write the result ".csv" (Omar)
# D. make the bar plot beautiful (Shinho)
# E. complete with other species (Baldomero)
# F. write the explanation (Baldomero)

# size for testing: default 150
size = 150

# -----------------------------------------------------------------------------
# assignment a
# function for calculating Euclidean Distance
euclidean_distance <- function(p, q) {
    ed = 0
    for (i in 1:4) {
        ed <- ed + (p[,i] - q[,i]) ^ 2
    }
    ed <- sqrt(ed)
    return(ed)
}

data(iris) #load iris data
iris2 <- iris[-5] # delete column 5 with string
# compute distances using dist() function
distances <- dist(head(iris2, size))
# print(distances)

# compute distances using self-created function
distances2 <- matrix(nrow=size, ncol=size)
for (p in 1:size) {
  for (q in 1:size) {
    row_p = iris2[p,]
    row_q = iris2[q,]
    distances2[p, q] <- euclidean_distance(row_p, row_q)
  }
}
# print(distances2)



# -----------------------------------------------------------------------------
# assignment b
# data frame

Flower = c()
Species = c()
Nearest.neighbor = c()
NN.species = c()

for (i in 1:size) {
    Flower[i] <- i
    #Species[i] <- iris[i, 5] # get `Species`
    Species[i] <- iris[i, 5] # get `Species`

    # dirty code
    line = distances2[i,]
    nn = sort(line)[2]
    index_nn = which(line == nn)
    Nearest.neighbor[i] <- index_nn
    NN.species[i] <- iris[index_nn, 5] # get `Species`
}

data_frame = data.frame(Flower, Species, Nearest.neighbor, NN.species)
print(data_frame)
write.csv(data_frame, file = "flowers_distance.csv")

# -----------------------------------------------------------------------------
# assignment c
# cc = data.frame ( table ( data_frame$Species, data_frame$NN.species ) [,]) 
cc = table ( data_frame$Species, data_frame$NN.species ) [,]

# -----------------------------------------------------------------------------
# assignment d
# print (cc)
#barplot(cc, main="blah", xlab="number of neighbors", col=c("darkblue", "red", "gray"),
 #       legend = rownames(counts), beside=TRUE)

# -----------------------------------------------------------------------------
# assignment e. histograms

par(mfrow=c(3, 4))

# setosa

setosa <- iris[iris$Species == 'setosa',]

hist(setosa$Sepal.Length, col="Tomato", main="Setosa Sepal.Length", xlab="Sepal Length")
hist(setosa$Sepal.Width, col="Tomato", main="Setosa Sepal.Width", xlab="Sepal Width")
hist(setosa$Petal.Length, col="Tomato", main="Setosa Petal.Length", xlab="Petal Length")
hist(setosa$Petal.Width, col="Tomato", main="Setosa Petal.Width", xlab="Petal Width")

# versicolor

versicolor <- iris[iris$Species == 'versicolor',]

hist(versicolor$Sepal.Length, col="Violet", main="Versicolor Sepal.Length", xlab="Sepal Length")
hist(versicolor$Sepal.Width, col="Violet", main="Versicolor Sepal.Width", xlab="Sepal Width")
hist(versicolor$Petal.Length, col="Violet", main="Versicolor Petal.Length", xlab="Petal Length")
hist(versicolor$Petal.Width, col="Violet", main="Versicolor Petal.Width", xlab="Petal Width")

# virginica

virginica <- iris[iris$Species == 'virginica',]

hist(virginica$Sepal.Length, col="Turquoise", main="Virginica Sepal.Length", xlab="Sepal Length")
hist(virginica$Sepal.Width, col="Turquoise", main="Virginica Sepal.Width", xlab="Sepal Width")
hist(virginica$Petal.Length, col="Turquoise", main="Virginica Petal.Length", xlab="Petal Length")
hist(virginica$Petal.Width, col="Turquoise", main="Virginica Petal.Width", xlab="Petal Width")




#for (i in colnames(iris2)) {
    #in c("Sepal.Length", "Sepal.Width", "Petal.Lenght", "Petal.Width")) {
#    ss <- subset(iris, Species == "setosa", select=c(i))
#    ss <- as.numeric(unlist(setosa))
#    hist(ss, main=i)
    # virginica <- iris[iris$Species == 'virginica',]
    # hist(virginica$Sepal.Length, main=i)
#}


# -----------------------------------------------------------------------------
# assignment f.

