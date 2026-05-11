# ============================================================================
# QUESTION 4: Create a function to calculate total revenue
# ============================================================================

if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

# Load datasets
data(mtcars)
data(iris)

titanic_url <- "https://raw.githubusercontent.com/datasets/titanic/master/train.csv"
titanic_data <- tryCatch({
  read.csv(titanic_url)
}, error = function(e) {
  set.seed(42)
  data.frame(
    PassengerId = 1:100,
    Pclass = sample(1:3, 100, replace = TRUE),
    Fare = round(runif(100, 5, 500), 2),
    Survived = sample(c(0,1), 100, replace = TRUE)
  )
})

# ============================================================================
# Function to calculate total revenue
# ============================================================================

calculate_total_revenue <- function(data, revenue_column, group_by = NULL) {
  if (!revenue_column %in% names(data)) {
    stop("Column not found in dataset")
  }
  
  if (is.null(group_by)) {
    total <- sum(data[[revenue_column]], na.rm = TRUE)
    return(list(
      total_revenue = total,
      n_observations = nrow(data),
      mean_revenue = total / nrow(data)
    ))
  } else {
    if (!group_by %in% names(data)) {
      stop("Group column not found in dataset")
    }
    result <- data %>%
      group_by(.data[[group_by]]) %>%
      summarise(
        TotalRevenue = sum(.data[[revenue_column]], na.rm = TRUE),
        MeanRevenue = mean(.data[[revenue_column]], na.rm = TRUE),
        Count = n()
      )
    return(result)
  }
}

# ============================================================================
# Examples
# ============================================================================

cat("=== Example 1: Total Revenue from Titanic Fares ===\n")
result1 <- calculate_total_revenue(titanic_data, "Fare")
cat("Total Fare Revenue:", round(result1$total_revenue, 2), "\n")
cat("Number of Passengers:", result1$n_observations, "\n")
cat("Average Fare:", round(result1$mean_revenue, 2), "\n")

cat("\n=== Example 2: Revenue by Passenger Class ===\n")
result2 <- calculate_total_revenue(titanic_data, "Fare", group_by = "Pclass")
print(result2)

cat("\n=== Example 3: Revenue by Survival Status ===\n")
result3 <- calculate_total_revenue(titanic_data, "Fare", group_by = "Survived")
print(result3)

cat("\n=== Example 4: Using mtcars dataset ===\n")
# Create revenue-like column (total revenue = hp * mpg)
mtcars$RevenueProxy <- mtcars$hp * mtcars$mpg
result4 <- calculate_total_revenue(mtcars, "RevenueProxy", group_by = "cyl")
cat("Revenue proxy by cylinder type:\n")
print(result4)

cat("\n=== Example 5: Using Iris dataset ===\n")
# Simulate revenue based on petal dimensions
iris$RevenueProxy <- iris$Petal.Length * iris$Petal.Width * 10
result5 <- calculate_total_revenue(iris, "RevenueProxy", group_by = "Species")
cat("Revenue by species:\n")
print(result5)

cat("\n=== Question 4 Complete ===\n")