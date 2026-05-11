# For loop to display + visualization
titanic <- read.csv("~/Desktop/SDAV/datasets/titanic.csv")
for(i in 1:10) cat("Passenger", titanic$PassengerId[i], "- Fare:", titanic$Fare[i], "\n")

dev.new()
hist(titanic$Fare, breaks=30, col="steelblue", main="Fare Distribution")