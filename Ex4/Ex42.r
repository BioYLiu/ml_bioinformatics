
# Exercise 4.2

# install `multtest` package
# source("https://bioconductor.org/biocLite.R")
# biocLite("multtest")

setwd("~/current-lectures/ml_bioinformatics/Ex4/")
library(multtest)
data(golub)

# a) calculate the mean and variance of all pooled expression data
meanGolub = apply(golub, 1, mean)
varGolub = apply(golub, 1, var)
sdGolub = apply(golub, 1, sd)

# b) means and standard deviations of the expression level for every gene for
# the classes ALL and AML

# i. create glb.fac
glb.fac = factor(golub.cl, levels=0:1, labels=c("ALL", "AML"))

meanALL = apply(golub[, glb.fac=="ALL"], 1, mean)
sdALL = apply(golub[, glb.fac=="ALL"], 1, sd)
meanAML = apply(golub[, glb.fac=="AML"], 1, mean)
sdAML = apply(golub[, glb.fac=="AML"], 1, sd)

# ii. 5 genes with the largest mean expression for ALL
largestALL = order(meanALL, decreasing = T)
print("5 genes with the largest mean expression for ALL")
print(golub.gnames[largestALL[1:5], 2])

# ii. 5 genes with the largest mean expression for AML
largestAML = order(meanAML, decreasing = T)
print("5 genes with the largest mean expression for AML")
print(golub.gnames[largestAML[1:5], 2])

# selection of the oncogene
oncogenes = grep('oncogene', golub.gnames[ , 2], ignore.case = T)

# iii. 5 oncogenes with the largest mean expression for ALL
meanOncoALL = apply(golub[oncogenes, glb.fac=="ALL"], 1, mean)
largestOncoALL = order(meanOncoALL, decreasing = T)
loncoALL = oncogenes[largestOncoALL[1:5]]
print("5 oncogenes with the largest mean expression for ALL")
print(golub.gnames[loncoALL, 2])

# iii. 5 oncogenes with the largest mean expression for AML
meanOncoAML = apply(golub[oncogenes, glb.fac=="AML"], 1, mean)
largestOncoAML = order(meanOncoAML, decreasing = T)
loncoAML = oncogenes[largestOncoAML[1:5]]
print("5 oncogenes with the largest mean expression for AML")
print(golub.gnames[loncoAML, 2])

# iv. genes with largest different in expression between the two classes
difference <- order(abs(meanAML-meanALL), decreasing=TRUE)

# print this on a file
names = golub.gnames[difference[1:5], 2]
mean = meanGolub[difference[1:5]]
standard_deviation = sdGolub[difference[1:5]]
data_frame = data.frame(names, mean, standard_deviation)
write.csv(data_frame, file = "Ex4_2b_iv.csv")


# 2c. Select gene 1042
geneCCND3 = split(golub[1042, ], glb.fac)

# i. Boxplot for the expression data
png(filename="boxplot.png")
boxplot(golub[1042,] ~ glb.fac,
        main="Boxplot GeneCCND3",
        xlab="Class",
        ylab="Expression",
        col=c("tomato", "green")
        )
dev.off()

# ii. Q-Q plot with theoretical normal distribution

#
png(filename="qq-plot.png")
par(mfrow=c(1, 2))
qqnorm(geneCCND3$ALL)
qqline(geneCCND3$ALL)
qqnorm(geneCCND3$AML)
qqline(geneCCND3$AML)
dev.off()

# iii. We have to apply the unpaired two-sample t-test (page 94).
# we can see that the size and variance of the two groups are different:
# length(geneCCND3$AML) == length(geneCCND3$ALL)
# var(geneCCND3$AML) == var(geneCCND3$ALL)
# so, we can apply the Welch t-test

ttest = t.test(geneCCND3$AML, geneCCND3$ALL)

# iv. Use a non-parametric test. The Kolmogorov-Smirnov test
kolmogorov = ks.test(geneCCND3$AML, geneCCND3$ALL)

# Shapiro-wilk test
a = shapiro.test(geneCCND3$AML)
b = shapiro.test(geneCCND3$ALL)

# 2d. Perform t-test for all genes comparing the distributions for ALL and AML.

# i. comparison of p-value and alpha
# if p-value < 0.05 we find a significant difference

pvalues <- apply(golub, 1, function(x) t.test(x ~ glb.fac)$p.value)
alpha = 0.05
results = table(pvalues < alpha)
total_i = results[names(results) == T]
print("Genes with significant differences are:")
print(total_i)

# ii. Given that we have more hypothesis, we increase the likelihood of a rare
# event, then we increase the likelihood of incorrectly rejecting a null
# hypothesis.

# iii. Using Bonferroni correction we will redefine the alpha value
# alpha = alpha / number of samples

bonferroni = alpha/nrow(golub)
conf_level = 1 - bonferroni

# modify the alpha value with the parameter conf.level in t.test function
pvalues <- apply(golub, 1, function(x) t.test(x ~ glb.fac, conf.level=conf_level)$p.value)
results = table(pvalues < bonferroni)

total_iii = results[names(results) == T]
print("Genes with significant differences are:")
print(total_iii)
