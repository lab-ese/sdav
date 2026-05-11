# ============================================================================
# QUESTION 3: Use for loop to display classification
# ============================================================================

if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

# Load dataset
titanic_url <- "https://raw.githubusercontent.com/datasets/titanic/master/train.csv"
titanic_data <- tryCatch({
  read.csv(titanic_url)
}, error = function(e) {
  set.seed(42)
  data.frame(
    PassengerId = 1:100,
    Survived = sample(c(0,1), 100, replace = TRUE),
    Pclass = sample(1:3, 100, replace = TRUE),
    Sex = sample(c("male","female"), 100, replace = TRUE),
    Age = round(runif(100, 1, 70), 1),
    Fare = round(runif(100, 5, 500), 2)
  )
})

# Classify fare
classify_fare <- function(fare) {
  if (is.na(fare)) return("Unknown")
  if (fare > 150) return("Premium")
  else if (fare > 50) return("Standard")
  else return("Economy")
}

titanic_data$Class <- sapply(titanic_data$Fare, classify_fare)

cat("=== Display first 15 passenger classifications ===\n")
for (i in 1:15) {
  p <- titanic_data[i, ]
  cat(sprintf("ID: %d | Age: %.1f | Fare: $%.2f | Class: %s\n",
              p$PassengerId, p$Age, p$Fare, p$Class))
}

cat("\n=== For loop with conditional: Premium passengers ===\n")
cat("Passengers with Premium class (Fare > $150):\n")
count <- 0
for (i in 1:nrow(titanic_data)) {
  if (titanic_data$Class[i] == "Premium") {
    p <- titanic_data[i, ]
    cat(sprintf("  ID: %d | Fare: $%.2f | Age: %.1f\n", p$PassengerId, p$Fare, p$Age))
    count <- count + 1
    if (count >= 8) break
  }
}
cat(sprintf("Showing first %d of %d premium passengers\n", count,
            sum(titanic_data$Class == "Premium")))

cat("\n=== For loop with conditional: Young survivors ===\n")
cat("Young passengers (Age < 30) who survived:\n")
count <- 0
for (i in 1:nrow(titanic_data)) {
  if (titanic_data$Survived[i] == 1 && titanic_data$Age[i] < 30) {
    p <- titanic_data[i, ]
    cat(sprintf("  ID: %d | Age: %.1f | Fare: $%.2f\n", p$PassengerId, p$Age, p$Fare))
    count <- count + 1
    if (count >= 10) break
  }
}

cat("\n=== Summary statistics using for loop ===\n")
total_fare <- 0
for (i in 1:nrow(titanic_data)) {
  total_fare <- total_fare + titanic_data$Fare[i]
}
cat("Total fare (calculated via loop):", round(total_fare, 2), "\n")

cat("\n=== Question 3 Complete ===\n")