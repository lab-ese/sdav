# ============================================================================
# QUESTION 18: Perform Two-sample t-test
# ============================================================================

# ============================================================================
# Two-sample t-test: Compare means of two independent groups
# ============================================================================

cat("=== Two-sample t-test ===\n\n")

# ============================================================================
# Example 1: Compare Setosa vs Versicolor Sepal.Length
# ============================================================================
cat("=== Example 1: Iris Species - Setosa vs Versicolor ===\n")
cat("H0: mean(Setosa) = mean(Versicolor)\n")
cat("H1: mean(Setosa) != mean(Versicolor)\n\n")

setosa_sepal <- iris$Sepal.Length[iris$Species == "setosa"]
versicolor_sepal <- iris$Sepal.Length[iris$Species == "versicolor"]

t_result1 <- t.test(setosa_sepal, versicolor_sepal)
print(t_result1)

cat("\nInterpretation:\n")
if (t_result1$p.value < 0.05) {
  cat("Conclusion: Reject H0 - significant difference between species\n")
} else {
  cat("Conclusion: Fail to reject H0 - no significant difference\n")
}

# ============================================================================
# Example 2: Compare MPG for 4-cylinder vs 8-cylinder cars
# ============================================================================
cat("\n=== Example 2: mtcars - 4-cylinder vs 8-cylinder MPG ===\n")
cat("H0: mean(4-cyl) = mean(8-cyl)\n")
cat("H1: mean(4-cyl) != mean(8-cyl)\n\n")

mpg_4cyl <- mtcars$mpg[mtcars$cyl == 4]
mpg_8cyl <- mtcars$mpg[mtcars$cyl == 8]

t_result2 <- t.test(mpg_4cyl, mpg_8cyl)
print(t_result2)

cat("\nInterpretation:\n")
if (t_result2$p.value < 0.05) {
  cat("Conclusion: Reject H0 - significant difference in MPG\n")
} else {
  cat("Conclusion: Fail to reject H0\n")
}

# ============================================================================
# Example 3: Equal variances assumed (pooled t-test)
# ============================================================================
cat("\n=== Example 3: Pooled t-test (equal variances) ===\n")
t_result3 <- t.test(setosa_sepal, versicolor_sepal, var.equal = TRUE)
print(t_result3)

# ============================================================================
# Example 4: Welch's t-test (unequal variances)
# ============================================================================
cat("\n=== Example 4: Welch's t-test (unequal variances) ===\n")
t_result4 <- t.test(setosa_sepal, versicolor_sepal, var.equal = FALSE)
print(t_result4)

# ============================================================================
# Example 5: One-tailed test
# ============================================================================
cat("\n=== Example 5: One-tailed test ===\n")
cat("H0: mean(4-cyl) <= mean(8-cyl)\n")
cat("H1: mean(4-cyl) > mean(8-cyl)\n\n")

t_result5 <- t.test(mpg_4cyl, mpg_8cyl, alternative = "greater")
print(t_result5)

# ============================================================================
# Manual calculation
# ============================================================================
cat("\n=== Manual Calculation ===\n")
cat("Setosa vs Versicolor Sepal.Length:\n")
n1 <- length(setosa_sepal)
n2 <- length(versicolor_sepal)
xbar1 <- mean(setosa_sepal)
xbar2 <- mean(versicolor_sepal)
s1 <- sd(setosa_sepal)
s2 <- sd(versicolor_sepal)

cat("n1 =", n1, ", n2 =", n2, "\n")
cat("xbar1 =", round(xbar1, 4), ", xbar2 =", round(xbar2, 4), "\n")
cat("s1 =", round(s1, 4), ", s2 =", round(s2, 4), "\n")

# Pooled variance
sp2 <- ((n1-1)*s1^2 + (n2-1)*s2^2) / (n1 + n2 - 2)
cat("Pooled variance =", round(sp2, 4), "\n")

# t-statistic
t_stat <- (xbar1 - xbar2) / sqrt(sp2 * (1/n1 + 1/n2))
cat("t-statistic =", round(t_stat, 4), "\n")
cat("p-value =", format(2 * (1 - pt(abs(t_stat), n1+n2-2)), digits = 6), "\n")

cat("\n=== Question 18 Complete ===\n")