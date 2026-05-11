# Function to calculate total revenue + visualization
revenue <- function(data, col) sum(data[[col]], na.rm = TRUE)
titanic <- read.csv("datasets/titanic.csv")
cat("Total Revenue:", revenue(titanic, "Fare"), "\n")

titanic$Class <- ifelse(titanic$Fare > 100, "Premium", ifelse(titanic$Fare > 30, "Standard", "Economy"))

dev.new()
barplot(tapply(titanic$Fare, titanic$Class, sum), col=c("red","green","blue"), main="Revenue by Class")