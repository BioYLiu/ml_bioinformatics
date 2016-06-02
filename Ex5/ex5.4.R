
# 5.4

# preparing the data for ANOVA analysis, the data is needed in long format

measures <- c(
  3.3, 2.3, 2.5, 1.3, 2, 1.5,          # Stim 1
  1.2, 0.9, 1.5, 1.5, 0.7, 1.8,        # Stim 2  
  3.2, 4.0, 2.7, 3, 3.5, 3.3)          # Stim 3    

# stimulation conditions

stim <- factor(c(rep(1,6), rep(2,6), rep(3,6)))

# cell line (A=1, B=2)

cellLine <- c(rep(1,3), rep(2,3), rep(1,3), rep(2,3), rep(1,3), rep(2,3))

# combine the data into a data frame

gene <- data.frame(cbind(measures, stim, cellLine))

boxplot(gene$measures~gene$cellLine*gene$stim)

#tapply(gene$measures, list(stim), mean)
#tapply(gene$measures, list(cellLine), mean)
#tapply(gene$measures, list(stim, cellLine), mean)

fit <- lm(gene$measures~gene$cellLine*gene$stim)
fit

# Coefficients:
#  (Intercept)            gene$cellLine                    gene$stim  
# 3.2000 (1st group avg)  -1.4000 (diff 2nd group to 1st)  -0.2333 (diff 3rd group to 1st) 

# Analysis of Variance
# group means are not significantly different

# null hypothesis: there is no difference across the levels of cell line/stim,
# reject if Pr(>F) is highly significant. 

# Both hypothesis could not be rejected.

#aov2 <- aov(measures~cellLine+stim+cellLine:stim, data=gene)
#summary(aov2)

anova(fit)

# residual interaction between cellLine and stim
# boxplot(residuals(fit)~cellLine*stim)

# difference between observed values and fitted values

residuals(fit)

#summary(fit)

plot(fit)

