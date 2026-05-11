# Classify customers based on fare
titanic <- read.csv("~/Desktop/SDAV/datasets/titanic.csv")
titanic$Class <- ifelse(titanic$Fare > 100, "Premium", ifelse(titanic$Fare > 30, "Standard", "Economy"))

dev.new()
par(mfrow=c(1,2))
barplot(table(titanic$Class), col=c("red","green","blue"), main="Passenger Class")
pie(table(titanic$Class), main="Class Distribution")