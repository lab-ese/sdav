# ============================================================================
# QUESTION 25: Perform t-test (General)
# ============================================================================

# ============================================================================
# Comprehensive t-test examples
# ============================================================================

cat("=== Comprehensive t-test Examples ===\n\n")

# ============================================================================
# Example 1: Paired t-test - Iris Petal.Length vs Petal.Width
# ============================================================================
cat("=== Example 1: Paired t-test ===\n")
cat("H0: mean(Petal.Length) = mean(Petal.Width)\n")
cat("H1: mean(Petal.Length) != mean(Petal.Width)\n\n")

t_result1 <- t.test(iris$Petal.Length, iris$Petal.Width, paired = TRUE)
print(t_result1)

cat("\nInterpretation:\n")
if (t_result1$p.value < 0.05) {
  cat("Reject H0 - significant difference between paired measurements\n")
} else {
  cat("Fail to reject H0\n")
}

# ============================================================================
# Example 2: Welch's t-test (unequal variances)
# ============================================================================
cat("\n=== Example 2: Welch's t-test ===\n")
cat("Comparing Sepal.Length: Setosa vs Virginica (unequal variances)\n\n")

setosa_sepal <- iris$Sepal.Length[iris$Species == "setosa"]
virginica_sepal <- iris$Sepal.Length[iris$Species == "virginica"]

t_result2 <- t.test(setosa_sepal, virginica_sepal, var.equal = FALSE)
print(t_result2)

# ============================================================================
# Example 3: One-sample t-test (two-tailed)
# ============================================================================
cat("\n=== Example 3: One-sample t-test (two-tailed) ===\n")
cat("H0: mean(iris$Petal.Width) = 1.5\n\n")

t_result3 <- t.test(iris$Petal.Width, mu = 1.5)
print(t_result3)

# ============================================================================
# Example 4: One-sample t-test (one-tailed)
# ============================================================================
cat("\n=== Example 4: One-sample t-test (one-tailed, less) ===\n")
cat("H0: mean(mtcars$mpg) >= 20\n")
cat("H1: mean(mtcars$mpg) < 20\n\n")

t_result4 <- t.test(mtcars$mpg, mu = 20, alternative = "less")
print(t_result4)

# ============================================================================
# Example 5: Two-sample t-test with different data
# ============================================================================
cat("\n=== Example 5: Two-sample t-test (MPG by cylinders) ===\n")
cat("Comparing 6-cylinder vs 8-cylinder cars\n\n")

mpg_6cyl <- mtcars$mpg[mtcars$cyl == 6]
mpg_8cyl <- mtcars$mpg[mtcars$cyl == 8]

cat("6-cylinder mean:", round(mean(mpg_6cyl), 2), "\n")
cat("8-cylinder mean:", round(mean(mpg_8cyl), 2), "\n")

t_result5 <- t.test(mpg_6cyl, mpg_8cyl, alternative = "greater")
print(t_result5)

# ============================================================================
# Example 6: Paired t-test with mtcars
# ============================================================================
cat("\n=== Example 6: Paired t-test (mtcars) ===\n")
cat("Compare qsec (1/4 mile time) for cars with different gears\n\n")

# Create paired data (first 10 cars with 4 gears vs same cars with different)
t_result6 <- t.test(mtcars$qsec[mtcars$gear == 4], 
                     mtcars$qsec[mtcars$gear == 3], 
                     paired = FALSE)
print(t_result6)

# ============================================================================
# Summary table
# ============================================================================
cat("\n=== Summary of t-tests ===\n")
cat("1. Paired t-test: Compare two related samples\n")
cat("2. Welch's t-test: Unequal variances\n")
cat("3. One-sample: Compare sample mean to population mean\n")
cat("4. One-tailed: Directional hypothesis\n")
cat("5. Two-sample: Compare two independent groups\n")

# t-test assumptions
cat("\n=== Assumptions of t-test ===\n")
cat("1. Observations are independent\n")
cat("2. Data is normally distributed (or large n)\n")
cat("3. For two-sample: equal variances (or use Welch's)\n")

cat("\n=== Question 25 Complete ===\n")