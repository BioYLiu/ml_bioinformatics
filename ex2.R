#Ex1
# A : DONE
# B (Shinho): Need explanation
# C (Baldo): Which differences do you observe? What could be a possible explanation?
# D (Shinho): labeling

#Ex2 (Omar)
# A : labeling & comments
# B : In which cases does the linear regression line give a good fit to the data?
#  On the basis of the scatter plots, describe the relationship/dependence between and for each data set.


#======================================================
#Ex 1
#task a
iris
colnames(iris)[1:4]

setosa <- iris[iris$Species == 'setosa',]
versicolor <- iris[iris$Species == 'versicolor',]
virginica <- iris[iris$Species == 'virginica',]
boxplot(setosa[1:4], main="setosa", col=c("red","green","yellow","gray"), names=colnames(iris)[1:4])
boxplot(versicolor[1:4], main="versicolor", col=c("red","green","yellow","gray"), names=colnames(iris)[1:4])
boxplot(virginica[1:4], main="virginica", col=c("red","green","yellow","gray"), names=colnames(iris)[1:4])

#task b
(mean(iris$Sepal.Length) - median(iris$Sepal.Length)) / sd(iris$Sepal.Length)
(mean(iris$Sepal.Width) - median(iris$Sepal.Width)) / sd(iris$Sepal.Width)
(mean(iris$Petal.Length) - median(iris$Petal.Length)) / sd(iris$Petal.Length)
(mean(iris$Petal.Width) - median(iris$Petal.Width)) / sd(iris$Petal.Width)

# hist(iris$Sepal.Length, col="Tomato", main="Sepal.Length", xlab="Sepal Length", breaks=20)
# hist(iris$Sepal.Width, col="Tomato", main="Sepal.Width", xlab="Sepal Width", breaks=20)
# hist(iris$Petal.Length, col="Tomato", main="Petal.Length", xlab="Petal Length", breaks=20)
# hist(iris$Petal.Width, col="Tomato", main="Petal.Width", xlab="Petal Width", breaks=20)

d <- density(iris$Sepal.Length) # returns the density data 
plot(d) # plots the results
d <- density(iris$Sepal.Width) # returns the density data 
plot(d) # plots the results
d <- density(iris$Petal.Length) # returns the density data 
plot(d) # plots the results
d <- density(iris$Petal.Width) # returns the density data 
plot(d) # plots the results


# refer to : https://en.wikipedia.org/wiki/Skewness
# positive: mean is greater than median.
# negative: mean is less than median.

# task c
co <- cor(iris[1:4])
cor(iris[1:4], method="spearman")

cor(setosa[1:4])
cor(setosa[1:4], method="spearman")

cor(versicolor[1:4])
cor(versicolor[1:4], method="spearman")

cor(virginica[1:4])
cor(virginica[1:4], method="spearman")

plot(iris)
plot(setosa)
plot(versicolor)
plot(virginica)
#task d
pdf('my_test.pdf',width=6,height=4,paper='special') 

levelplot(cor(iris[1:4]), colorkey = T, region = T, col.regions=heat.colors)
levelplot(cor(setosa[1:4]), colorkey = T, region = T, col.regions=heat.colors)
levelplot(cor(versicolor[1:4]), colorkey = T, region = T, col.regions=heat.colors)
levelplot(cor(virginica[1:4]), colorkey = T, region = T, col.regions=heat.colors)

dev.off()

#======================================================
#2.
#======================================================
# A-i
dat = read.csv("exercise2-2.csv")

# Here we calculate the mean and the variance for each colum in the dat dataset
meanByCol = apply(dat, 2, mean)
varByCol = apply(dat, 2, var)
sdByCol = apply(dat, 2, sd)

meanByCol
varByCol
# A-ii
# calculate correlation between X and Y.
c1 = cor(dat$x1, dat$y1)
c2 = cor(dat$x2, dat$y2)
c3 = cor(dat$x3, dat$y3)
c4 = cor(dat$x4, dat$y4)

# A-iii
# Here calculate the values for a and b, that will be used later to draw the regression line
b1 <- c1 * sdByCol[1] / sdByCol[2]
#b1 <- 0.5
a1 <- meanByCol[2] - (b1 * meanByCol[1])

b2 <- c2 * sqrt(varByCol[3]) / sqrt(varByCol[4])
#b2 = 0.5
a2 <- meanByCol[4] - (b1 * meanByCol[3])

b3 <- c3 * sqrt(varByCol[5]) / sqrt(varByCol[6])
#b3 <- 0.5
a3 <- meanByCol[6] - (b1 * meanByCol[5])

b4 <- c4 * sqrt(varByCol[7]) / sqrt(varByCol[8])
#b4 <- 0.5
a4 <- meanByCol[8] - (b1 * meanByCol[7])


#==================================================
#B

# With color red we draw a line using the equation line.
# With color blue we draw a line using the `lm` function provide by R.

# We can see that the case 3 is the data which better fits the regression line. Almost perfect. But it does exist an outlier value. 

# If we compare the scatterplots and the correlation value that we calculated before we can check that...
# 1. we have a very well distributed data, and a normal distribution.
# 2. we can see a relationship between X and Y, this one is a functional relationship.
# 3. we have a linear relationship between X and Y.
# 4. we can see that is not a linear relationship, the outlier modifies markedly the regression line.

plot(dat$x1, dat$y1, xlim=c(0, 20), ylim=c(-5,15), xlab="X1", ylab="Y1", main="X1-Y1 scatterplot")
abline(a=a1, b=b1, col="red")
#abline(lm(dat$y1~dat$x1), col="blue")

plot(dat$x2, dat$y2, xlim=c(0, 20), ylim=c(-5,15), xlab="X2", ylab="Y2", main="X2-Y2 scatterplot")
abline(a=a2, b=b2, col="red")
#abline(lm(dat$y2~dat$x2), col="blue")

plot(dat$x3, dat$y3, xlim=c(0, 20), ylim=c(-5,15), xlab="X3", ylab="Y3", main="X3-Y3 scatterplot")
abline(a=a3, b=b3, col="red")
#abline(lm(dat$y3~dat$x3), col="blue")

plot(dat$x4, dat$y4, xlim=c(0, 20), ylim=c(-5,15), xlab="X4", ylab="Y4", main="X4-Y4 scatterplot")
abline(a=a4, b=b4, col="red")
#abline(lm(dat$y4~dat$x4), col="blue")