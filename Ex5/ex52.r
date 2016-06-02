# import package nnet to implement multinomial logistic regression
library(nnet)

# 1. Fit a logistic regression model on iris dataset
# Multinomial logistic regression with nnet package
data(iris)

# shuffle the dataset and get training and test dataset
shuffled.iris <- iris[sample(1:nrow(iris)), ]
test.ds <- shuffled.iris[1:30,]
training.ds <- shuffled.iris[31:150,]

formula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
multinomial.model <- multinom(formula, training.ds)

#predict(multinomial.model)
probs <- predict(multinomial.model, test.ds, "probs")

# Binomial logistic regression using `glm` function
# for setosa
"
setosa.ds = shuffled.iris
newcol <- data.frame(isSetosa=(setosa.ds$Species == 'setosa'))
setosa.ds <- cbind(setosa.ds, newcol)
test.setosa <- setosa.ds[1:30,]
training.setosa <- setosa.ds[31:150,]

formula <- isSetosa ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
model.setosa <- glm(formula, data=training.setosa, family='binomial')
#prob <- predict(model.setosa, newdata=test.ds, type='response')
"

# for versicolor
# for virginica
