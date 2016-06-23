#Group members (Name, Student ID, E-Mail):  

# * Baldomero Valdez,  Valenzuela, 2905175, baldmer.w@gmail.com
# * Omar Trinidad Gutierrez Mendez, 2850441, omar.vpa@gmail.com
# * Shinho Kang, 2890169, wis.shinho.kang@gmail.com

library(GSVAdata)
library(GSVA)

# 1.a

data(gbm_VerhaakEtAl)

# gbm_eset
# head(pData(gbm_eset))

# get genes expressions, the matrix is already in the proper format.

data_gbm = exprs(gbm_eset)
# data_gbm[1:5,1:3]

# For the purpose of selecting the most informative genes for class detection,
# we reduce the dataset to the top 2,000 most variable genes, measured by median
# absolute deviation. 

mads = apply(data_gbm,1,mad)
data_gbm = data_gbm[rev(order(mads))[1:2000],]

data_gbm


# 2.a

data(leukemia)
leukemia_eset

# get gene expressions

data_leukemia = exprs(leukemia_eset)
# data_leukemia[1:5,1:3]

# Prioritize the gene expressions based on their median absolute deviation (MAD) and
# select the 2000 top genes

mads = apply(data_leukemia,1,mad)
data_leukemia = data_leukemia[rev(order(mads))[1:2000],]

data_leukemia
