# Group members (Name, Student ID, E-Mail):
# 1. Baldomero Valdez,  Valenzuela, 2905175, baldmer.w@gmail.com
# 2. Omar Trinidad Gutierrez Mendez, 2850441, omar.vpa@gmail.com
# 3. Shinho Kang, 2890169, wis.shinho.kang@gmail.com

#=========================================
# TASK 1
#=========================================
sample.mean <- 4
sample.sd <- 0.7
n <- 5

# a) Compute the standard error of the mean
se <- sample.sd / sqrt(n)
# => se : 0.3130495

# b) Compute the 95% confidence interval
# 2.5% quantile of t-distribution with df n-1
t.qt <- qt(0.025, n-1)
interval <- c(sample.mean + se*t.qt, sample.mean - se*t.qt)
size <- interval[2] - interval[1]
# => interval: [3.130835, 4.869165]
# => size: [1.73833]

# c) half the size of the confidence interval
n2 <- 12 # or 13
se2 <- sample.sd / sqrt(n2)
t.qt2 <- qt(0.025, n2-1)
interval2 <- c(sample.mean + se2*t.qt2, sample.mean - se2*t.qt2)
size2 <- interval2[2] - interval2[1]
# => interval2: [3.555241, 4.444759]
# => size2: 0.8895176 (when n=12)
# => size2: 0.8460115 (when n=13)
