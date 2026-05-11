# ============================================================================
# QUESTION 2: Use if/else to classify customers
# ============================================================================

if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

# Load Titanic dataset
titanic_url <- "https://raw.githubusercontent.com/datasets/titanic/master/train.csv"
titanic_data <- tryCatch({
  read.csv(titanic_url)
}, error = function(e) {
  set.seed(42)
  data.frame(
    PassengerId = 1:100,
    Survived = sample(c(0,1), 100, replace = TRUE),
    Pclass = sample(1:3, 100, replace = TRUE),
    Name = paste("Passenger", 1:100),
    Sex = sample(c("male","female"), 100, replace = TRUE),
    Age = round(runif(100, 1, 70), 1),
    SibSp = sample(0:5, 100, replace = TRUE),
    Parch = sample(0:4, 100, replace = TRUE),
    Fare = round(runif(100, 5, 500), 2),
    Embarked = sample(c("S","C","Q"), 100, replace = TRUE)
  )
})

cat("=== Classification based on Fare ===\n")

# Function to classify passengers by fare
classify_fare <- function(fare) {
  if (is.na(fare)) {
    return("Unknown")
  } else if (fare > 150) {
    return("Premium")
  } else if (fare > 50) {
    return("Standard")
  } else {
    return("Economy")
  }
}

# Apply classification
titanic_data$Class <- sapply(titanic_data$Fare, classify_fare)
cat("Passenger Classification:\n")
print(table(titanic_data$Class))

# Classification based on Age
cat("\n=== Classification based on Age ===\n")

classify_age <- function(age) {
  if (is.na(age)) return("Unknown")
  if (age < 18) return("Child")
  else if (age < 35) return("Young Adult")
  else if (age < 55) return("Middle Age")
  else return("Senior")
}

titanic_data$AgeGroup <- sapply(titanic_data$Age, classify_age)
cat("Age Classification:\n")
print(table(titanic_data$AgeGroup))

# Classification based on Survival
cat("\n=== Classification based on Survival Status ===\n")

classify_survival <- function(survived) {
  if (is.na(survived)) return("Unknown")
  if (survived == 1) return("Survived")
  else return("Did Not Survive")
}

titanic_data$SurvivalStatus <- sapply(titanic_data$Survived, classify_survival)
cat("Survival Classification:\n")
print(table(titanic_data$SurvivalStatus))

# Cross-tabulation
cat("\n=== Cross-tabulation: Class vs Age Group ===\n")
print(table(titanic_data$Class, titanic_data$AgeGroup))

cat("\n=== Question 2 Complete ===\n")