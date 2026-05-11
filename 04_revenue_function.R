# Function to calculate total revenue
revenue <- function(data, col) sum(data[[col]], na.rm = TRUE)
titanic <- read.csv("datasets/titanic.csv")
revenue(titanic, "Fare")