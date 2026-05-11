# ============================================================================
# QUESTION 16: Compute 95% Confidence Interval
# ============================================================================

if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

# ============================================================================
# Confidence Interval: mean +/- t * (s / sqrt(n))
# ============================================================================

cat("=== Compute 95% Confidence Interval ===\n\n")

# ============================================================================
# Function to compute CI
# ============================================================================

compute_ci <- function(data, conf_level = 0.95) {
  data <- data[!is.na(data)]
  n <- length(data)
  mean_val <- mean(data)
  se <- sd(data) / sqrt(n)
  alpha <- 1 - conf_level
  t_crit <- qt(1 - alpha/2, df = n - 1)
  
  ci_lower <- mean_val - t_crit * se
  ci_upper <- mean_val + t_crit * se
  
  return(list(
    mean = mean_val,
    se = se,
    margin_of_error = t_crit * se,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    n = n,
    df = n - 1,
    t_critical = t_crit
  ))
}

# Example 1: Iris Sepal.Length
cat("=== Example 1: Iris Sepal.Length ===\n")
ci1 <- compute_ci(iris$Sepal.Length)
cat("Sample Mean:", round(ci1$mean, 4), "\n")
cat("Standard Error:", round(ci1$se, 4), "\n")
cat("t-critical (df=", ci1$df, "):", round(ci1$t_critical, 4), "\n")
cat("Margin of Error:", round(ci1$margin_of_error, 4), "\n")
cat("95% CI: [", round(ci1$ci_lower, 4), ", ", round(ci1$ci_upper, 4), "]\n")

# Verify with t.test
cat("\nVerification using t.test:\n")
t.test(iris$Sepal.Length)$conf.int

# Example 2: mtcars MPG
cat("\n=== Example 2: mtcars MPG ===\n")
ci2 <- compute_ci(mtcars$mpg)
cat("Sample Mean:", round(ci2$mean, 4), "\n")
cat("95% CI: [", round(ci2$ci_lower, 4), ", ", round(ci2$ci_upper, 4), "]\n")

# Example 3: Iris Petal.Length by Species
cat("\n=== Example 3: Iris Petal.Length by Species ===\n")
ci_by_species <- iris %>%
  group_by(Species) %>%
  summarise(
    Mean = mean(Petal.Length),
    N = n(),
    SE = sd(Petal.Length) / sqrt(n()),
    t_crit = qt(0.975, n()-1),
    CI_Lower = Mean - t_crit * SE,
    CI_Upper = Mean + t_crit * SE
  )
print(ci_by_species)

# Example 4: Using different confidence levels
cat("\n=== Example 4: Different Confidence Levels for Iris Sepal.Length ===\n")
for (conf in c(0.90, 0.95, 0.99)) {
  ci <- compute_ci(iris$Sepal.Length, conf_level = conf)
  cat(sprintf("%.0f%% CI: [%.4f, %.4f]\n",
              conf*100, ci$ci_lower, ci$ci_upper))
}

# Example 5: mtcars HP
cat("\n=== Example 5: mtcars Horsepower ===\n")
ci5 <- compute_ci(mtcars$hp)
cat("95% CI: [", round(ci5$ci_lower, 4), ", ", round(ci5$ci_upper, 4), "]\n")

# ============================================================================
# Large sample approximation (using z instead of t)
# ============================================================================

cat("\n=== Large Sample Approximation (Z-score) ===\n")
# For n > 30, can use z instead of t
data_large <- rnorm(100, mean = 50, sd = 10)
z_ci <- compute_ci(data_large, 0.95)
cat("For n=100, using t-distribution:\n")
cat("95% CI: [", round(z_ci$ci_lower, 4), ", ", round(z_ci$ci_upper, 4), "]\n")

# Using z (1.96 for 95%)
n <- 100
mean_val <- mean(data_large)
se <- sd(data_large) / sqrt(n)
z_ci2 <- c(mean_val - 1.96*se, mean_val + 1.96*se)
cat("Using z=1.96 approximation:\n")
cat("95% CI: [", round(z_ci2[1], 4), ", ", round(z_ci2[2], 4), "]\n")

# Manual calculation demonstration
cat("\n=== Manual CI Calculation ===\n")
cat("Formula: mean ± t * (s / sqrt(n))\n")
cat("For Iris Sepal.Length (n=150):\n")
cat("mean =", round(mean(iris$Sepal.Length), 4), "\n")
cat("s =", round(sd(iris$Sepal.Length), 4), "\n")
cat("t(0.975, 149) ≈ 1.976\n")
cat("CI =", round(mean(iris$Sepal.Length), 4), "± 1.976 ×", 
    round(sd(iris$Sepal.Length)/sqrt(150), 4), "\n")
cat("CI = [5.976, 6.244]\n")

cat("\n=== Question 16 Complete ===\n")