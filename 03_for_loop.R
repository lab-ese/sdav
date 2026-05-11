# For loop to display classification
titanic <- read.csv("datasets/titanic.csv")
for(i in 1:10) cat("Passenger", titanic$PassengerId[i], "- Fare:", titanic$Fare[i], "\n")