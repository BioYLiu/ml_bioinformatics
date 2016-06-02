# Group members (Name, Student ID, E-Mail):
# 1. Baldomero Valdez,  Valenzuela, 2905175, baldmer.w@gmail.com
# 2. Omar Trinidad Gutierrez Mendez, 2850441, omar.vpa@gmail.com
# 3. Shinho Kang, 2890169, wis.shinho.kang@gmail.com

#=========================================
# TASK 3
#=========================================
# original data
species.richness <- c(32, 29, 35, 36, 41)
lake.area <- c(2.0, 0.9, 3.1, 3.0, 3.0)

# pearson's correlation - original
cor.original <- cor(species.richness, lake.area)

# 1000 times of permutations
N <- 1000
cnt <- 0
for (i in 1:N) {
  # randomly sampling vectors
  species.richness.random <- sample(species.richness)
  lake.area.random <- sample(lake.area)
  
  # pearson's correlation - random
  cor.random <- cor(species.richness.random, lake.area.random)
  
  # if cor.random is greater than or equal to cor.original
  # count variable + 1
  if (cor.random >= cor.original) {
    cnt <- cnt+1
  }
}
print ("===============")
print (paste("P-Value: ", cnt/N))

