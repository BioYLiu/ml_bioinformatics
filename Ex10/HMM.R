#library("HMM", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

#==========================
#TASK 2-A

# Initialise HMM
# Fair dice and Loaded dice
# transProbs F->L: 0.05, L->F: 0.1
# emiisionProbs for Loaded dice : (0.1, 0.1, 0.1, 0.1, 0.1, 0.5)
hmm = initHMM(c("Fair","Loaded"), c(1:6), transProbs=matrix(c(.95,.1,.05,.9),2),
              emissionProbs=matrix(c(1/6,.1,1/6,.1,1/6,.1,1/6,.1,1/6,.1,1/6,.5),2))
print(hmm)
# Generate a sequence of 2000 observations
simHMM(hmm, 2000)

#==========================
#TASK 2-B
