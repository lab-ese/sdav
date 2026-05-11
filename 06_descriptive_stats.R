# ============================================================================
# QUESTION 6: Compute descriptive statistics
# ============================================================================

if (!require("moments")) install.packages("moments")
library(moments)

# Load datasets
data(iris)
data(mtcars)

# ============================================================================
# Function to compute comprehensive descriptive statistics
# ============================================================================

compute_descriptive_stats <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  
  # Calculate mode
  get_mode <- function(data) {
    uniq <- unique(data)
    uniq[which.max(tabulate(match(data, uniq)))]
  }
  
  stats <- data.frame(
    Statistic = c("Count (n)", "Mean", "Median", "Mode", "Standard Deviation",
                  "Variance", "Minimum", "Maximum", "Range", "Sum",
                  "Skewness", "Kurtosis"),
    Value = c(n, mean(x), median(x), get_mode(x), sd(x), var(x),
               min(x), max(x), max(x) - min(x), sum(x),
               skewness(x), kurtosis(x))
  )
  return(stats)
}

# ============================================================================
# Example 1: Iris Sepal Length
# ============================================================================
cat("=== Descriptive Statistics: Iris Sepal.Length ===\n")
stats1 <- compute_descriptive_stats(iris$Sepal.Length)
print(stats1)

cat("\n=== Descriptive Statistics: Iris Sepal.Width ===\n")
stats2 <- compute_descriptive_stats(iris$Sepal.Width)
print(stats2)

cat("\n=== Descriptive Statistics: Iris Petal.Length ===\n")
stats3 <- compute_descriptive_stats(iris$Petal.Length)
print(stats3)

cat("\n=== Descriptive Statistics: mtcars MPG ===\n")
stats4 <- compute_descriptive_stats(mtcars$mpg)
print(stats4)

cat("\n=== Descriptive Statistics: mtcars Horsepower ===\n")
stats5 <- compute_descriptive_stats(mtcars$hp)
print(stats5)

# ============================================================================
# Manual calculation demonstration
# ============================================================================
cat("\n=== Manual Calculation for Iris Sepal.Length ===\n")
x <- iris$Sepal.Length
cat("Mean:", mean(x), "\n")
cat("Median:", median(x), "\n")
cat("Mode:", names(sort(-table(x)))[1], "\n")
cat("Standard Deviation:", sd(x), "\n")
cat("Variance:", var(x), "\n")
cat("Range:", min(x), "to", max(x), "\n")

cat("\n=== Question 6 Complete ===\n")