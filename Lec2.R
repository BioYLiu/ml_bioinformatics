x=c(5.423, 6.239, 8.288, 4.999, 5.399)
quantile(x)

#IQR

par(mfrow=c(2, 2))

hist(x, col="red", main="tumor")
hist(x, col="red", main="tumor", breaks="Scott")
hist(x, col="red", main="tumor", breaks=4)

x=c(5.423, 6.239, 8.288, 4.999, 5.399)
y = c(8, 11, 5, 11, 14)
X = cbind(x,y)
boxplot(X,col=c("red","green"), names=c("tumor","normal"))

# Correlation between tumor and normal in R:
x=c(5.423, 6.239, 8.288, 4.999, 5.399)
y=c(1.234, 0.283, 1.488, 1.048, 0.599)
plot(x,y)
cor(x,y)
# Rank correlation
cor(x,y, method="spearman")
cor(x,y, method="kendall")


