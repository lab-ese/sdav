# ============================================================================
# QUESTION 21: Perform F-test
# ============================================================================

# ============================================================================
# F-test: Compare variances of two populations
# ============================================================================

cat("=== F-test for Equality of Variances ===\n\n")

# ============================================================================
# Example 1: Compare Setosa vs Versicolor Sepal.Length variance
# ============================================================================
cat("=== Example 1: Iris Setosa vs Versicolor (Sepal.Length) ===\n")
cat("H0: variances are equal\n")
cat("H1: variances are not equal\n\n")

setosa_sepal <- iris$Sepal.Length[iris$Species == "setosa"]
versicolor_sepal <- iris$Sepal.Length[iris$Species == "versicolor"]

var1 <- var(setosa_sepal)
var2 <- var(versicolor_sepal)
cat("Variance(Setosa) =", round(var1, 4), "\n")
cat("Variance(Versicolor) =", round(var2, 4), "\n")

# F-test using var.test
f_result1 <- var.test(setosa_sepal, versicolor_sepal)
print(f_result1)

cat("\nInterpretation:\n")
if (f_result1$p.value < 0.05) {
  cat("p-value < 0.05: Reject H0 - variances are different\n")
} else {
  cat("p-value >= 0.05: Fail to reject H0 - variances are equal\n")
}

# ============================================================================
# Example 2: Compare 4-cylinder vs 8-cylinder MPG variance
# ============================================================================
cat("\n=== Example 2: mtcars 4-cyl vs 8-cyl MPG variance ===\n")
mpg_4cyl <- mtcars$mpg[mtcars$cyl == 4]
mpg_8cyl <- mtcars$mpg[mtcars$cyl == 8]

var_4cyl <- var(mpg_4cyl)
var_8cyl <- var(mpg_8cyl)
cat("Variance(4-cyl) =", round(var_4cyl, 4), "\n")
cat("Variance(8-cyl) =", round(var_8cyl, 4), "\n")

f_result2 <- var.test(mpg_4cyl, mpg_8cyl)
print(f_result2)

# ============================================================================
# Example 3: Compare Petal.Length variance across species
# ============================================================================
cat("\n=== Example 3: Iris Petal.Length variance by species ===\n")

setosa_petal <- iris$Petal.Length[iris$Species == "setosa"]
virginica_petal <- iris$Petal.Length[iris$Species == "virginica"]

var.setosa <- var(setosa_petal)
var.virginica <- var(virginica_petal)
cat("Variance(Setosa) =", round(var.setosa, 4), "\n")
cat("Variance(Virginica) =", round(var.virginica, 4), "\n")

f_result3 <- var.test(setosa_petal, virginica_petal)
print(f_result3)

# ============================================================================
# Manual F-test calculation
# ============================================================================
cat("\n=== Manual Calculation ===\n")

# F = s1^2 / s2^2 (larger variance in numerator)
F_stat <- max(var1, var2) / min(var1, var2)
df1 <- length(setosa_sepal) - 1
df2 <- length(versicolor_sepal) - 1

cat("F =", round(F_stat, 4), "\n")
cat("df1 =", df1, ", df2 =", df2, "\n")
cat("p-value =", format(pf(F_stat, df1, df2, lower.tail = FALSE), digits = 6), "\n")

# ============================================================================
# Note: F-test is sensitive to non-normality
# ============================================================================
cat("\n=== Note ===\n")
cat("F-test assumes both samples are normally distributed.\n")
cat("For non-normal data, consider Levene's test or Bartlett's test.\n")

# Bartlett's test for homogeneity of variances
cat("\nBartlett's test for Iris Sepal.Length by species:\n")
bartlett_result <- bartlett.test(Sepal.Length ~ Species, data = iris)
print(bartlett_result)

cat("\n=== Question 21 Complete ===\n")