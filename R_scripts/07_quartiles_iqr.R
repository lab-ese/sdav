# ============================================================================
# QUESTION 7: Compute Quartiles & IQR
# ============================================================================

if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

# Load datasets
data(iris)
data(mtcars)

# ============================================================================
# Function to compute quartiles and IQR
# ============================================================================

compute_quartiles <- function(x, detailed = TRUE) {
  x <- x[!is.na(x)]
  
  q <- quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1))
  iqr_val <- IQR(x)
  
  if (detailed) {
    result <- data.frame(
      Quartile = c("Min (0%)", "Q1 (25%)", "Median (50%)", "Q3 (75%)", "Max (100%)"),
      Value = as.numeric(q)
    )
    result <- rbind(result, data.frame(Quartile = "IQR", Value = iqr_val))
    return(result)
  } else {
    return(list(quartiles = q, iqr = iqr_val))
  }
}

# ============================================================================
# Function to identify outliers using IQR method
# ============================================================================

identify_outliers <- function(x) {
  x <- x[!is.na(x)]
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  
  outliers <- x[x < lower | x > upper]
  
  return(list(
    lower_bound = lower,
    upper_bound = upper,
    n_outliers = length(outliers),
    outlier_values = sort(outliers)
  ))
}

# ============================================================================
# Example 1: Iris Sepal.Length
# ============================================================================
cat("=== Quartiles & IQR: Iris Sepal.Length ===\n")
q1 <- compute_quartiles(iris$Sepal.Length)
print(q1)

cat("\nOutlier detection for Sepal.Length:\n")
o1 <- identify_outliers(iris$Sepal.Length)
cat("Lower bound:", o1$lower_bound, "\n")
cat("Upper bound:", o1$upper_bound, "\n")
cat("Number of outliers:", o1$n_outliers, "\n")
if (o1$n_outliers > 0) {
  cat("Outlier values:", o1$outlier_values, "\n")
}

# ============================================================================
# Example 2: Iris Sepal.Width
# ============================================================================
cat("\n=== Quartiles & IQR: Iris Sepal.Width ===\n")
q2 <- compute_quartiles(iris$Sepal.Width)
print(q2)

cat("\nOutlier detection for Sepal.Width:\n")
o2 <- identify_outliers(iris$Sepal.Width)
cat("Lower bound:", o2$lower_bound, "\n")
cat("Upper bound:", o2$upper_bound, "\n")
cat("Number of outliers:", o2$n_outliers, "\n")
if (o2$n_outliers > 0) {
  cat("Outlier values:", o2$outlier_values, "\n")
}

# ============================================================================
# Example 3: mtcars MPG
# ============================================================================
cat("\n=== Quartiles & IQR: mtcars MPG ===\n")
q3 <- compute_quartiles(mtcars$mpg)
print(q3)

cat("\nOutlier detection for MPG:\n")
o3 <- identify_outliers(mtcars$mpg)
cat("Lower bound:", o3$lower_bound, "\n")
cat("Upper bound:", o3$upper_bound, "\n")
cat("Number of outliers:", o3$n_outliers, "\n")

# ============================================================================
# Example 4: mtcars Horsepower
# ============================================================================
cat("\n=== Quartiles & IQR: mtcars Horsepower ===\n")
q4 <- compute_quartiles(mtcars$hp)
print(q4)

cat("\nOutlier detection for Horsepower:\n")
o4 <- identify_outliers(mtcars$hp)
cat("Lower bound:", o4$lower_bound, "\n")
cat("Upper bound:", o4$upper_bound, "\n")
cat("Number of outliers:", o4$n_outliers, "\n")
if (o4$n_outliers > 0) {
  cat("Outlier values:", o4$outlier_values, "\n")
}

# ============================================================================
# Comparison across groups
# ============================================================================
cat("\n=== Quartiles by Species (Sepal.Width) ===\n")
iris %>%
  group_by(Species) %>%
  summarise(
    Q1 = quantile(Sepal.Width, 0.25),
    Median = median(Sepal.Width),
    Q3 = quantile(Sepal.Width, 0.75),
    IQR = IQR(Sepal.Width)
  )

cat("\n=== Question 7 Complete ===\n")