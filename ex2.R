#Ex1
# A : DONE
# B (Shinho): Need explanation - done
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

#------------------------------------------------------
#task b
(mean(iris$Sepal.Length) - median(iris$Sepal.Length)) / sd(iris$Sepal.Length)
(mean(iris$Sepal.Width) - median(iris$Sepal.Width)) / sd(iris$Sepal.Width)
(mean(iris$Petal.Length) - median(iris$Petal.Length)) / sd(iris$Petal.Length)
(mean(iris$Petal.Width) - median(iris$Petal.Width)) / sd(iris$Petal.Width)

# EXPLANATION:
# Distribution Skewness: is a measure of the asymmetry of the probability distribution 
# of a real-valued random variable about its mean. 
# The skewness value can be positive or negative, or even undefined.
# Negative skew: 
#   Mean is greater than median.
#   The left tail is longer; the mass of the distribution is concentrated on the right of the figure.
# Positive skew: 
#   Mean is less than median.
#   The right tail is longer; the mass of the distribution is concentrated on the left of the figure.
  
# Sepal Length and Width has positive skew.
# from the below density plot, we can see that the right tail is slightly longer than left tail on 
# Sepal Length and Width.

# Petal Length and Width has negative skew.
# from the below density plot of the petal length and width, we can see two hump but the most dense part is 
# on the right side (slightly).

# we also can say, the absolute of the skewness is bigger, the data is more concentrated on one-side.

d <- density(iris$Sepal.Length) # returns the density data 
plot(d) # plots the results
d <- density(iris$Sepal.Width) # returns the density data 
plot(d) # plots the results
d <- density(iris$Petal.Length) # returns the density data 
plot(d) # plots the results
d <- density(iris$Petal.Width) # returns the density data 
plot(d) # plots the results



#------------------------------------------------------
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


#------------------------------------------------------
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
