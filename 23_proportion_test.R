# Proportion test
titanic <- read.csv("datasets/titanic.csv")
prop.test(sum(titanic$Survived), nrow(titanic), p=0.5)