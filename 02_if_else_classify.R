# Classify customers based on fare
titanic <- read.csv("datasets/titanic.csv")
titanic$Class <- ifelse(titanic$Fare > 100, "Premium", ifelse(titanic$Fare > 30, "Standard", "Economy"))
table(titanic$Class)