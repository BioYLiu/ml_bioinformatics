

data(iris)

# shuffle the dataset and get training and test dataset
shuffled.iris <- iris[sample(1:nrow(iris)), ]
test.ds <- shuffled.iris[1:30,]
training.ds <- shuffled.iris[31:150,]

png(filename="task3.png")
par(mfrow=c(4, 3))

labels = names(iris)[-5]
indexes = c(1:4)
for (x in indexes) {
    for (y in indexes) {
        if (x != y) {

            a = training.ds[,x]
            b = training.ds[,y]

            plot(a~b,
                 pch = 22,
                 bg = c('red', 'green', 'blue')[unclass(iris$Species)],
                 xlab = labels[x],
                 ylab = labels[y]
                 # xlim = c(0,7),
                 # ylim = c(0,7)
                 )
            model = lm(a~b)
            abline(model, col='brown')
        }
    }
}
dev.off()

# Because each of the plots show a correlation between the columns we can
# conclude that one of the predictors can be expressed as a linear combination
# of the others.
