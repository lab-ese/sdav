# ============================================================================
# QUESTION 22: Compare means using independent t-test
# ============================================================================

# ============================================================================
# Independent t-test: Same as two-sample t-test
# ============================================================================

cat("=== Independent t-test (Compare Means) ===\n\n")

# ============================================================================
# Example 1: Compare Petal.Length between Setosa and Virginica
# ============================================================================
cat("=== Example 1: Iris Petal.Length - Setosa vs Virginica ===\n")
cat("H0: mean(Setosa) = mean(Virginica)\n")
cat("H1: mean(Setosa) != mean(Virginica)\n\n")

setosa_petal <- iris$Petal.Length[iris$Species == "setosa"]
virginica_petal <- iris$Petal.Length[iris$Species == "virginica"]

t_result1 <- t.test(setosa_petal, virginica_petal, var.equal = TRUE)
print(t_result1)

cat("\nInterpretation:\n")
cat("t-statistic:", round(t_result1$statistic, 4), "\n")
cat("p-value:", format(t_result1$p.value, digits = 6), "\n")
if (t_result1$p.value < 0.05) {
  cat("Conclusion: Reject H0 - significant difference in means\n")
} else {
  cat("Conclusion: Fail to reject H0\n")
}

# ============================================================================
# Example 2: Compare MPG - Automatic vs Manual transmission
# ============================================================================
cat("\n=== Example 2: mtcars MPG - Automatic vs Manual ===\n")
mtcars$transmission <- ifelse(mtcars$am == 0, "Automatic", "Manual")
mpg_auto <- mtcars$mpg[mtcars$transmission == "Automatic"]
mpg_manual <- mtcars$mpg[mtcars$transmission == "Manual"]

cat("Automatic: mean =", round(mean(mpg_auto), 2), ", n =", length(mpg_auto), "\n")
cat("Manual: mean =", round(mean(mpg_manual), 2), ", n =", length(mpg_manual), "\n")

t_result2 <- t.test(mpg_auto, mpg_manual)
print(t_result2)

# ============================================================================
# Example 3: Welch's t-test (unequal variances)
# ============================================================================
cat("\n=== Example 3: Welch's t-test (unequal variances) ===\n")
t_result3 <- t.test(setosa_petal, virginica_petal, var.equal = FALSE)
print(t_result3)

# ============================================================================
# Example 4: One-tailed test
# ============================================================================
cat("\n=== Example 4: One-tailed test ===\n")
cat("H0: mean(Manual) <= mean(Automatic)\n")
cat("H1: mean(Manual) > mean(Automatic)\n\n")

t_result4 <- t.test(mpg_manual, mpg_auto, alternative = "greater")
print(t_result4)

# ============================================================================
# Effect size (Cohen's d)
# ============================================================================
cat("\n=== Effect Size (Cohen's d) ===\n")

cohens_d <- function(x, y) {
  n1 <- length(x)
  n2 <- length(y)
  pooled_sd <- sqrt(((n1-1)*sd(x)^2 + (n2-1)*sd(y)^2) / (n1+n2-2))
  (mean(x) - mean(y)) / pooled_sd
}

d <- cohens_d(setosa_petal, virginica_petal)
cat("Cohen's d =", round(d, 4), "\n")
cat("Interpretation: ", ifelse(abs(d) < 0.2, "small", 
       ifelse(abs(d) < 0.5, "medium", "large")), "effect\n")

# ============================================================================
# Paired vs independent
# ============================================================================
cat("\n=== Question 22 Complete ===\n")