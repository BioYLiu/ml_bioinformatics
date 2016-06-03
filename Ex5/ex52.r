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
print(multinomial.model)

#Call:
#multinom(formula = formula, data = training.ds)
#
#Coefficients:
#           (Intercept) Sepal.Length Sepal.Width Petal.Length Petal.Width
#versicolor    15.78584    -5.753264   -6.333713     12.78611   -2.309163
#virginica    -22.61359    -8.586707  -12.026601     22.15053   13.246169
#
#Residual Deviance: 11.38084 
#AIC: 31.38084 

e = predict(multinomial.model)

# Binomial logistic regression using `glm` function

# for setosa
setosa.ds = shuffled.iris
training.setosa <- setosa.ds[31:150,]
newcol <- data.frame(isSetosa=(training.setosa$Species == 'setosa'))
training.setosa <- cbind(training.setosa, newcol)

formula <- isSetosa ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
model.setosa <- glm(formula, data=training.setosa, family='binomial')
print(model.setosa)
#Call:  glm(formula = formula, family = "binomial", data = training.setosa)
#
#Coefficients:
# (Intercept)  Sepal.Length   Sepal.Width  Petal.Length   Petal.Width  
#       8.083         4.034        11.240       -22.097        -4.758  
#
#Degrees of Freedom: 119 Total (i.e. Null);  115 Residual
#Null Deviance:      152.8 
#Residual Deviance: 2.285e-09    AIC: 10
e = predict(model.setosa, newdata=test.ds, type='response')

# for versicolor
versicolor.ds = shuffled.iris
training.versicolor <- versicolor.ds[31:150,]
newcol <- data.frame(isVersicolor=(training.versicolor$Species == 'versicolor'))
training.versicolor <- cbind(training.versicolor, newcol)

formula <- isVersicolor ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
model.versicolor <- glm(formula, data=training.versicolor, family='binomial')
print(model.versicolor)
#Call:  glm(formula = formula, family = "binomial", data = training.versicolor)
#
#Coefficients:
# (Intercept)  Sepal.Length   Sepal.Width  Petal.Length   Petal.Width  
#      7.7785       -0.3307       -2.7866        1.2605       -2.5890  
#
#Degrees of Freedom: 119 Total (i.e. Null);  115 Residual
#Null Deviance:      152.8 
#Residual Deviance: 115.4        AIC: 125.4
e = predict(model.versicolor, newdata=test.ds, type='response')

# for virginica
virginica.ds = shuffled.iris
training.virginica <- virginica.ds[31:150,]
newcol <- data.frame(isVirginica=(training.virginica$Species == 'virginica'))
training.virginica <- cbind(training.virginica, newcol)

formula <- isVirginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
model.virginica <- glm(formula, data=training.virginica, family='binomial')
print(model.virginica)

#Call:  glm(formula = formula, family = "binomial", data = training.virginica)
#
#Coefficients:
# (Intercept)  Sepal.Length   Sepal.Width  Petal.Length   Petal.Width  
#     -38.802        -2.830        -5.672         9.420        15.584  
#
#Degrees of Freedom: 119 Total (i.e. Null);  115 Residual
#Null Deviance:      152.8 
#Residual Deviance: 11.38        AIC: 21.38
e = predict(model.virginica, newdata=test.ds, type='response')

