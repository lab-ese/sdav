# ============================================================================
# QUESTION 17: Perform One-sample t-test
# ============================================================================

# ============================================================================
# One-sample t-test: H0: mu = mu0 vs H1: mu != mu0
# ============================================================================

cat("=== One-sample t-test ===\n\n")

# ============================================================================
# Example 1: Test if Iris Sepal.Length mean = 5.5
# ============================================================================
cat("=== Example 1: Iris Sepal.Length ===\n")
cat("H0: mean = 5.5\n")
cat("H1: mean != 5.5\n\n")

t_result1 <- t.test(iris$Sepal.Length, mu = 5.5)
print(t_result1)

cat("\nInterpretation:\n")
cat("t-statistic:", round(t_result1$statistic, 4), "\n")
cat("p-value:", format(t_result1$p.value, digits = 6), "\n")
cat("95% CI:", round(t_result1$conf.int[1], 4), "to", round(t_result1$conf.int[2], 4), "\n")
if (t_result1$p.value < 0.05) {
  cat("Conclusion: Reject H0 at alpha=0.05 - significant difference from 5.5\n")
} else {
  cat("Conclusion: Fail to reject H0 - no significant difference from 5.5\n")
}

# ============================================================================
# Example 2: Test if mtcars MPG mean = 20
# ============================================================================
cat("\n=== Example 2: mtcars MPG ===\n")
cat("H0: mean = 20\n")
cat("H1: mean != 20\n\n")

t_result2 <- t.test(mtcars$mpg, mu = 20)
print(t_result2)

cat("\nInterpretation:\n")
if (t_result2$p.value < 0.05) {
  cat("Conclusion: Reject H0 at alpha=0.05\n")
} else {
  cat("Conclusion: Fail to reject H0 at alpha=0.05\n")
}

# ============================================================================
# Example 3: One-tailed test
# ============================================================================
cat("\n=== Example 3: One-tailed test (greater) ===\n")
cat("H0: mean <= 6\n")
cat("H1: mean > 6\n\n")

t_result3 <- t.test(iris$Petal.Length, mu = 6, alternative = "greater")
print(t_result3)

# ============================================================================
# Example 4: Using different mu0 value
# ============================================================================
cat("\n=== Example 4: Iris Petal.Width, mu = 1.5 ===\n")
cat("H0: mean = 1.5\n")
cat("H1: mean != 1.5\n\n")

t_result4 <- t.test(iris$Petal.Width, mu = 1.5)
print(t_result4)

# ============================================================================
# Manual t-test calculation
# ============================================================================
cat("\n=== Manual Calculation: Iris Sepal.Length ===\n")
x <- iris$Sepal.Length
mu0 <- 5.5
n <- length(x)
xbar <- mean(x)
s <- sd(x)
t_stat <- (xbar - mu0) / (s / sqrt(n))

cat("n =", n, "\n")
cat("xbar =", round(xbar, 4), "\n")
cat("s =", round(s, 4), "\n")
cat("t = (", round(xbar, 4), " - 5.5) / (", round(s, 4), "/ sqrt(", n, "))\n", sep = "")
cat("t =", round(t_stat, 4), "\n")
cat("p-value = 2 * P(T > |t|) = 2 * (1 - pt(", round(abs(t_stat), 4), ", ", n-1, "))\n", sep = "")
cat("p-value =", format(2 * (1 - pt(abs(t_stat), n-1)), digits = 6), "\n")

cat("\n=== Question 17 Complete ===\n")