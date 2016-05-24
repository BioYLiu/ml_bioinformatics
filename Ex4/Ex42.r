
# Exercise 4.2
# install `multtest` package
# source("https://bioconductor.org/biocLite.R")
# biocLite("multtest")

library(multtest)
data(golub)

# a) calculate the mean and variance of all pooled expression data
meanGolub = apply(golub, 1, mean)
varGolub = apply(golub, 1, var)
sdGolub = apply(golub, 1, sd)

# b) means and standard deviations of the expression level for every gene for
# the classes ALL and AML

meanALL = apply(golub[, golub.cl==0], 1, mean)
sdALL = apply(golub[, golub.cl==0], 1, sd)
meanAML = apply(golub[, golub.cl==1], 1, mean)
sdAML = apply(golub[, golub.cl==1], 1, sd)

# i. create golub.fac
glb.fac = factor(golub.cl, levels=0:1, labels=c("ALL", "AML"))

# ii. 5 genes with the largest mean expression for ALL
largestALL = order(meanALL, decreasing = T)
#print(largestALL)
#print(golub.gnames[largestALL[1:5], 2])

# ii. 5 genes with the largest mean expression for AML
largestAML = order(meanAML, decreasing = T)
#print(largestAML)
#print(golub.gnames[largestAML[1:5], 2])

# selection of the oncogenes
oncogenes = grep('oncogene', golub.gnames[ , 2], ignore.case = T)

# iii. 5 oncogenes with the largest mean expression for ALL
meanOncoALL = apply(golub[oncogenes, glb.fac=="ALL"], 1, mean)
largestOncoALL = order(meanOncoALL, decreasing = T)
loncoALL = oncogenes[largestOncoALL[1:5]]
#print("5 oncogenes with the largest mean expression for ALL")
#print(golub.gnames[loncoALL, 2])

# iii. 5 oncogenes with the largest mean expression for AML
meanOncoAML = apply(golub[oncogenes, glb.fac=="AML"], 1, mean)
largestOncoAML = order(meanOncoAML, decreasing = T)
loncoAML = oncogenes[largestOncoAML[1:5]]
#print("5 oncogenes with the largest mean expression for AML")
#print(golub.gnames[loncoAML, 2])


# iv. genes with largest different in expression between the two classes
difference <- order(abs(meanAML-meanALL), decreasing=TRUE)
#print("Genes with largest different in expression between the two classes")

# print this on a file
names = golub.gnames[difference[1:5], 2]
mean = meanGolub[difference[1:5]]
standard_deviation = sdGolub[difference[1:5]]
data_frame = data.frame(names, mean, standard_deviation)
write.csv(data_frame, file = "Ex4 2b iv.csv")
