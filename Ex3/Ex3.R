# Group members (Name, Student ID, E-Mail):
# 1. Baldomero Valdez,  Valenzuela, 2905175, baldmer.w@gmail.com
# 2. Omar Trinidad Gutierrez Mendez, 2850441, omar.vpa@gmail.com
# 3. Shinho Kang, 2890169, wis.shinho.kang@gmail.com


# Implement a R function multinom, which takes as arguments
#   i) a sample matrix (each row being one sample vector)
#   ii) a probability parameter vector p for a multinomial distribution 
#   iii) the number of trials n.
# The function computes and returns:
#   - the expectation value
#   - the population variance/covariance matrix
#   - the sample mean (R functions apply and mean)
#   - the sample covariance matrix (R function cov)
#   - the value of the probability mass function for each sample (Tip: you can use R function dmultinom)

# In addition multinom should show an appropriate bar diagram visualzing p.
# Test your function with 50 random drawings from the multinomial distribution in task 3 (R function rmultinom) 
# and show your obtained results. 

# ** How close are the empirical mean, variance and covariances 
# to the theoretically expected results? 
# Which behavior would you expect, if the number of samples is increased and decreased, 
# respectively? Please discuss.**

# Parameters
#   m: sample matrix
#   p: probability vector
#   n: number of trials
multinom <- function(m, p, n) {
  #   - the expectation value
  ex <- p*n
  print ('=============================')
  print ('Expectation Value')
  print (ex)
  
  #   - the population variance/covariance matrix
  var_m<-n*p*(1-p)
  print ('=============================')
  print ('Population variance')
  print (var_m)
  
  cov_m<-matrix(0,ncol=6,nrow=6)
  for (i in 1:length(p)) {
    for (j in 1:length(p)) {
      #print (-1 * n * i * j)
      cov_m[i,j] <- (-1 * n * p[i] * p[j])
    }
  }
  print ('=============================')
  print ('Population covariance matrix')
  print (cov_m)
  
  #   - the sample mean (R functions apply and mean)
  print ('=============================')
  print ('Sample Mean')
  print (apply(m, 2, mean))
  #apply(m,1,function(x) sum(x*c(1,2,3,4,5,6))/n)
  
  #   - the sample covariance matrix (R function cov)
  print ('=============================')
  print ('Sample Covariance Matrix')
  print (cov(m))
  
  #   - the value of the probability mass function for each sample (Tip: you can use R function dmultinom)
  print ('=============================')
  print ('Value of the probability mass function')
  print (apply(m, 1, function(x) dmultinom(x, prob=p)))
  
  # visualizing p
  barplot(p, xlab = "value", ylab="probability", axisnames = T, names.arg = c(1,2,3,4,5,6))
}


# probability vector
p <- c(0.2,0.4/3,0.2,0.4/3,0.4/3,0.2)
# number of trials
n <- 10
# sample matrix - 50 random drawings from the multinomial distribution, transpose the matrix (row=sample vector)
m<-t(rmultinom(10, n, p))

multinom(m,p,n)
