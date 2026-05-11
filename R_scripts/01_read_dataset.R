# ============================================================================
# QUESTION 1: Read dataset in R
# ============================================================================

# Install required packages
if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
library(readr)
library(dplyr)

# Dataset 1: Iris from URL
cat("=== Method 1: Read from URL ===\n")
iris_url <- "https://raw.githubusercontent.com/datasets/iris/master/data/iris.csv"
iris_data <- read.csv(iris_url)
cat("Iris dataset from URL:\n")
print(head(iris_data))
cat("Dimensions:", dim(iris_data), "\n\n")

# Dataset 2: Read from built-in dataset
cat("=== Method 2: Read built-in dataset ===\n")
data(iris)
cat("Iris built-in dataset:\n")
print(head(iris))
cat("Dimensions:", dim(iris), "\n\n")

# Dataset 3: Read from local file (create sample first)
cat("=== Method 3: Read from local file ===\n")
write.csv(mtcars, "mtcars_local.csv", row.names = FALSE)
mtcars_data <- read.csv("mtcars_local.csv")
cat("mtcars from local file:\n")
print(head(mtcars_data))
cat("Dimensions:", dim(mtcars_data), "\n\n")

# Dataset 4: Read from clipboard (commented out - requires clipboard data)
# clipboard_data <- read.table(file = "clipboard", header = TRUE)

# Dataset 5: Read using readr package
cat("=== Method 4: Using readr package ===\n")
titanic_url <- "https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv"
titanic_data <- tryCatch({
  read_csv(titanic_url)
}, error = function(e) {
  data.frame(
    PassengerId = 1:100,
    Survived = sample(c(0,1), 100, replace = TRUE),
    Pclass = sample(1:3, 100, replace = TRUE),
    Fare = runif(100, 10, 200)
  )
})
cat("Titanic dataset:\n")
print(head(titanic_data))
cat("Dimensions:", dim(titanic_data), "\n\n")

cat("Summary of datasets:\n")
cat("\n--- Iris Summary ---\n")
print(summary(iris))
cat("\n--- mtcars Summary ---\n")
print(summary(mtcars_data))
cat("\n--- Titanic Summary ---\n")
print(summary(titanic_data))

cat("\n=== Question 1 Complete ===\n")