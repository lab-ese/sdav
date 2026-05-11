# ============================================================================
# QUESTION 24: Perform Z-test
# ============================================================================

# ============================================================================
# Z-test: Test for known population standard deviation
# ============================================================================

cat("=== Z-test ===\n\n")

# ============================================================================
# Function to perform z-test
# ============================================================================

z_test <- function(x, mu, sigma, alternative = "two.sided") {
  n <- length(x)
  xbar <- mean(x)
  z <- (xbar - mu) / (sigma / sqrt(n))
  
  if (alternative == "two.sided") {
    p_value <- 2 * pnorm(-abs(z))
  } else if (alternative == "less") {
    p_value <- pnorm(z)
  } else {
    p_value <- pnorm(z, lower.tail = FALSE)
  }
  
  return(list(z_statistic = z, p_value = p_value, mean = xbar, n = n))
}

# ============================================================================
# Example 1: Test if Iris Sepal.Length mean = 5.8 (known sigma = 0.8)
# ============================================================================
cat("=== Example 1: Iris Sepal.Length ===\n")
cat("H0: mean = 5.8, known sigma = 0.8\n")
cat("H1: mean != 5.8\n\n")

z_result1 <- z_test(iris$Sepal.Length, mu = 5.8, sigma = 0.8)
cat("Sample mean:", round(z_result1$mean, 4), "\n")
cat("Z-statistic:", round(z_result1$z_statistic, 4), "\n")
cat("p-value:", format(z_result1$p_value, digits = 6), "\n")

if (z_result1$p_value < 0.05) {
  cat("Conclusion: Reject H0 at alpha=0.05\n")
} else {
  cat("Conclusion: Fail to reject H0\n")
}

# ============================================================================
# Example 2: Test if mtcars MPG = 20 (known sigma = 6)
# ============================================================================
cat("\n=== Example 2: mtcars MPG ===\n")
cat("H0: mean = 20, known sigma = 6\n")
cat("H1: mean != 20\n\n")

z_result2 <- z_test(mtcars$mpg, mu = 20, sigma = 6)
cat("Sample mean:", round(z_result2$mean, 4), "\n")
cat("Z-statistic:", round(z_result2$z_statistic, 4), "\n")
cat("p-value:", format(z_result2$p_value, digits = 6), "\n")

# ============================================================================
# Example 3: One-tailed z-test
# ============================================================================
cat("\n=== Example 3: One-tailed test ===\n")
cat("H0: mean <= 6\n")
cat("H1: mean > 6\n\n")

z_result3 <- z_test(iris$Petal.Length, mu = 6, sigma = 1.5, alternative = "greater")
cat("Z-statistic:", round(z_result3$z_statistic, 4), "\n")
cat("p-value:", format(z_result3$p_value, digits = 6), "\n")

# ============================================================================
# Using t-test as approximation (when sigma unknown)
# ============================================================================
cat("\n=== Approximation using t-test (for large n) ===\n")
cat("For n > 30, t-distribution approximates z-distribution\n")
t_approx <- t.test(iris$Sepal.Length, mu = 5.8)
cat("t-statistic:", round(t_approx$statistic, 4), "\n")
cat("p-value:", format(t_approx$p.value, digits = 6), "\n")

# ============================================================================
# Manual calculation
# ============================================================================
cat("\n=== Manual Calculation ===\n")
cat("Formula: Z = (xbar - mu0) / (sigma / sqrt(n))\n\n")
x <- iris$Sepal.Length
mu0 <- 5.8
sigma <- 0.8
n <- length(x)
xbar <- mean(x)
z <- (xbar - mu0) / (sigma / sqrt(n))

cat("n =", n, "\n")
cat("xbar =", round(xbar, 4), "\n")
cat("sigma =", sigma, "\n")
cat("Z = (", round(xbar, 4), " - ", mu0, ") / (", sigma, " / sqrt(", n, "))\n", sep = "")
cat("Z =", round(z, 4), "\n")
cat("P(|Z| > ", round(z, 2), ") = 2 * (1 - pnorm(", abs(round(z, 2)), "))\n", sep = "")
cat("p-value =", format(2 * (1 - pnorm(abs(z))), digits = 6), "\n")

# Confidence interval using z
cat("\n=== 95% CI using Z ===\n")
se <- sigma / sqrt(n)
ci_lower <- xbar - 1.96 * se
ci_upper <- xbar + 1.96 * se
cat("95% CI: [", round(ci_lower, 4), ", ", round(ci_upper, 4), "]\n")

cat("\n=== Question 24 Complete ===\n")