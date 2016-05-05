#Ex1
# A : DONE
# B (Shinho): Need explanation
# C : DONE
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

# Pearson's Correlation
# 
# +1: Perfect direct linear relationship (correlation)
# -1: Perfect decreasing linear relationship (anti-correlation)
# 0: uncorrelated
# 
# One can observe that when considering the whole Iris data set, 
# there is a decreasing linear relationship between the variables 
# Petal.Width/Lenght and Sepal.Width, on the contrary there is 
# an increasing correlation between the variables Sepal.Length 
# and Petal.Width/Length.
# 
# In contrast when every group of species is analyzed
# individually one can observe that there is only an 
# increasing correlation. In the case of species type Setosa
# the correlation between Sepal.Length and Petal.Width/Length 
# reduces considerably.
# 
# One possible explanation for these discrepancies between
# correlations is the existence/addition of “outliers” affecting
# the measure when the data is correlated all together and individually.  

co <- cor(iris[1:4])
corsp <- cor(iris[1:4], method="spearman")

cor(setosa[1:4])
corspVetosa <- cor(setosa[1:4], method="spearman")

cor(versicolor[1:4])
corspVersicolor <- cor(versicolor[1:4], method="spearman")

cor(virginica[1:4])
corspVirginica <- cor(virginica[1:4], method="spearman")

#plot(iris)
#plot(setosa)
#plot(versicolor)
#plot(virginica)

#task d
pdf('my_test.pdf',width=6,height=4,paper='special') 

# displays Pearson's Correlation

levelplot(cor(iris[1:4]), colorkey = T, region = T, main="Iris data set (Pearson's Correlation)", col.regions=heat.colors)
levelplot(cor(setosa[1:4]), colorkey = T, region = T, main="Setosa (Pearson's Correlation)", col.regions=heat.colors)
levelplot(cor(versicolor[1:4]), colorkey = T, region = T, main="Versicolor (Pearson's Correlation)", col.regions=heat.colors)
levelplot(cor(virginica[1:4]), colorkey = T, region = T, main="Virginica (Pearson's Correlation)", col.regions=heat.colors)

# displays spearman's Correlation

levelplot(corsp, colorkey = T, region = T, main="Iris data set (Spearman's Correlation)", col.regions=heat.colors)
levelplot(corspVetosa, colorkey = T, region = T, main="Setosa (Spearman's Correlation)", col.regions=heat.colors)
levelplot(corspVersicolor, colorkey = T, region = T, main="Versicolor (Spearman's Correlation)", col.regions=heat.colors)
levelplot(corspVirginica, colorkey = T, region = T, main="Virginica (Spearman's Correlation)", col.regions=heat.colors)

dev.off()

#======================================================
#2.
#======================================================
# A-i
dat = read.csv("exercise2-2.csv")

meanByCol = apply(dat,2,mean)
varByCol = apply(dat, 2, var)

meanByCol
varByCol
# A-ii
cor(dat$x1, dat$y1)
cor(dat$x2, dat$y2)
cor(dat$x3, dat$y3)
cor(dat$x4, dat$y4)

# A-iii
# b= pearson * sqrt(var x)/sqrt(var y)
# a= mean(y) - b*mean(x)
b1 <- cor(dat$x1, dat$y1) * sqrt(varByCol[1]) / sqrt(varByCol[2])
a1 <- meanByCol[2] - (b1 * meanByCol[1])
b2 <- cor(dat$x2, dat$y2) * sqrt(varByCol[3]) / sqrt(varByCol[4])
a2 <- meanByCol[4] - (b1 * meanByCol[3])
b3 <- cor(dat$x3, dat$y3) * sqrt(varByCol[5]) / sqrt(varByCol[6])
a3 <- meanByCol[6] - (b1 * meanByCol[5])
b4 <- cor(dat$x4, dat$y4) * sqrt(varByCol[7]) / sqrt(varByCol[8])
a4 <- meanByCol[8] - (b1 * meanByCol[7])


#==================================================
#B
plot(dat$x1, dat$y1, xlim=c(0, 20), ylim=c(-5,15))
abline(a=a1, b=b1, col="red")
abline(lm(dat$y1~dat$x1), col="blue")

plot(dat$x2, dat$y2, xlim=c(0, 20), ylim=c(-5,15))
abline(a=a2, b=b2, col="red")
abline(lm(dat$y2~dat$x2), col="blue")

plot(dat$x3, dat$y3, xlim=c(0, 20), ylim=c(-5,15))
abline(a=a3, b=b3, col="red")
abline(lm(dat$y3~dat$x3), col="blue")

plot(dat$x4, dat$y4, xlim=c(0, 20), ylim=c(-5,15))
abline(a=a4, b=b4, col="red")
abline(lm(dat$y4~dat$x4), col="blue")
