#Group members (Name, Student ID, E-Mail):
  
#* Baldomero Valdez,  Valenzuela, 2905175, baldmer.w@gmail.com
#* Omar Trinidad Gutierrez Mendez, 2850441, omar.vpa@gmail.com
#* Shinho Kang, 2890169, wis.shinho.kang@gmail.com

#==============================================
# 7-2
#B)
#install.packages("mclust")
library(mclust)
library(colonCA)

data(colonCA)
colon.ds <- log(exprs(colonCA))

pvalues <- apply(colon.ds, 1, function(x) {
  return (t.test(x[colonCA$class=='t'], x[colonCA$class=='n'])$p.value)
})
alpha = 0.0001
colon.signif = colon.ds[pvalues <= alpha,]

# GMM based clustering
# i) patients based on their gene expression profiles
cl1 = Mclust(t(colon.signif), G=2:10)
summary(cl1)
plot(cl1,  what = "BIC")


# ii) differentially expressed genes based on their profiles across patients
cl2 = Mclust(colon.signif, G=2:10)
summary(cl2)
plot(cl2,  what = "BIC")



#==============================================
# C)Standardize the gene expressions for each gene separately 
# by unit variance scaling
scaled.colon.signif <- scale(colon.signif, center=TRUE, scale=TRUE)

# i) GMM for patients
cl3 = Mclust(t(scaled.colon.signif), G=2:10)
summary(cl3)
plot(cl3,  what = "BIC")
plot(cl3,  what = "classification")

# ii) GMM for genes
cl4 = Mclust(scaled.colon.signif, G=2:10)
summary(cl4)
plot(cl4,  what = "BIC")
plot(cl4,  what = "classification")




