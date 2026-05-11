# ============================================================================
# QUESTION 26: Compute 95% Confidence Interval (Advanced)
# ============================================================================

if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

# ============================================================================
# Advanced Confidence Interval calculations
# ============================================================================

cat("=== 95% Confidence Interval (Advanced) ===\n\n")

# ============================================================================
# Example 1: CI for difference in means (two-sample)
# ============================================================================
cat("=== Example 1: CI for difference in means ===\n")
cat("Difference between Setosa and Versicolor Sepal.Length\n\n")

setosa_sepal <- iris$Sepal.Length[iris$Species == "setosa"]
versicolor_sepal <- iris$Sepal.Length[iris$Species == "versicolor"]

ci_diff <- t.test(setosa_sepal, versicolor_sepal)$conf.int
cat("95% CI for difference: [", round(ci_diff[1], 4), ", ", round(ci_diff[2], 4), "]\n")
cat("Interpretation: We are 95% confident the true difference lies in this range\n")

# ============================================================================
# Example 2: CI for proportion
# ============================================================================
cat("\n=== Example 2: CI for proportion ===\n")
cat("Titanic survival proportion\n\n")

titanic_url <- "https://raw.githubusercontent.com/datasets/titanic/master/train.csv"
titanic_data <- tryCatch({
  read.csv(titanic_url)
}, error = function(e) {
  set.seed(42)
  data.frame(Survived = sample(c(0,1), 100, replace = TRUE, prob = c(0.6, 0.4)))
})

n_survived <- sum(titanic_data$Survived)
n_total <- nrow(titanic_data)

prop_ci <- prop.test(n_survived, n_total, conf.level = 0.95)$conf.int
cat("Survival proportion: ", round(n_survived/n_total, 4), "\n")
cat("95% CI: [", round(prop_ci[1], 4), ", ", round(prop_ci[2], 4), "]\n")

# ============================================================================
# Example 3: Bootstrap CI
# ============================================================================
cat("\n=== Example 3: Bootstrap Confidence Interval ===\n")
cat("Bootstrap CI for Iris Sepal.Length (1000 replications)\n\n")

set.seed(123)
bootstrap_ci <- function(data, n_bootstrap = 1000, conf_level = 0.95) {
  n <- length(data)
  bootstrap_means <- numeric(n_bootstrap)
  
  for (i in 1:n_bootstrap) {
    sample_idx <- sample(1:n, n, replace = TRUE)
    bootstrap_means[i] <- mean(data[sample_idx])
  }
  
  alpha <- 1 - conf_level
  ci_lower <- quantile(bootstrap_means, alpha/2)
  ci_upper <- quantile(bootstrap_means, 1 - alpha/2)
  
  return(c(lower = ci_lower, upper = ci_upper, mean = mean(bootstrap_means)))
}

boot_result <- bootstrap_ci(iris$Sepal.Length)
cat("Bootstrap 95% CI: [", round(boot_result[1], 4), ", ", round(boot_result[2], 4), "]\n")
cat("Bootstrap mean:", round(boot_result[3], 4), "\n")

# Compare to theoretical CI
theoretical_ci <- t.test(iris$Sepal.Length)$conf.int
cat("Theoretical 95% CI: [", round(theoretical_ci[1], 4), ", ", round(theoretical_ci[2], 4), "]\n")

# ============================================================================
# Example 4: CI for variance
# ============================================================================
cat("\n=== Example 4: CI for Variance ===\n")
cat("Variance of Iris Sepal.Length\n\n")

n <- length(iris$Sepal.Length)
v <- var(iris$Sepal.Length)
alpha <- 0.05

chi_lower <- v * (n - 1) / qchisq(1 - alpha/2, n - 1)
chi_upper <- v * (n - 1) / qchisq(alpha/2, n - 1)

cat("Sample variance:", round(v, 4), "\n")
cat("95% CI for variance: [", round(chi_lower, 4), ", ", round(chi_upper, 4), "]\n")
cat("95% CI for SD: [", round(sqrt(chi_lower), 4), ", ", round(sqrt(chi_upper), 4), "]\n")

# ============================================================================
# Example 5: CI for mtcars by groups
# ============================================================================
cat("\n=== Example 5: CI for MPG by Cylinder Type ===\n")

ci_by_cyl <- mtcars %>%
  group_by(cyl) %>%
  summarise(
    Mean = mean(mpg),
    SD = sd(mpg),
    N = n(),
    SE = SD / sqrt(N),
    t_crit = qt(0.975, N-1),
    CI_Lower = Mean - t_crit * SE,
    CI_Upper = Mean + t_crit * SE
  )
print(ci_by_cyl)

# ============================================================================
# Example 6: CI for correlation coefficient (Fisher's z)
# ============================================================================
cat("\n=== Example 6: CI for Correlation ===\n")
cat("Correlation between Sepal.Length and Petal.Length\n\n")

r <- cor(iris$Sepal.Length, iris$Petal.Length)
n <- nrow(iris)

# Fisher's z transformation
z <- 0.5 * log((1 + r) / (1 - r))
se_z <- 1 / sqrt(n - 3)
z_lower <- z - 1.96 * se_z
z_upper <- z + 1.96 * se_z

# Transform back
r_lower <- (exp(2*z_lower) - 1) / (exp(2*z_lower) + 1)
r_upper <- (exp(2*z_upper) - 1) / (exp(2*z_upper) + 1)

cat("Correlation (r):", round(r, 4), "\n")
cat("95% CI: [", round(r_lower, 4), ", ", round(r_upper, 4), "]\n")

# ============================================================================
# Visual representation
# ============================================================================
cat("\n=== Visual CI for Iris Species ===\n")

ci_species <- iris %>%
  group_by(Species) %>%
  summarise(
    Mean = mean(Sepal.Length),
    SE = sd(Sepal.Length) / sqrt(n()),
    CI_Lower = Mean - 1.96 * SE,
    CI_Upper = Mean + 1.96 * SE
  )
print(ci_species)

# Using t-distribution (more accurate for small samples)
ci_species_t <- iris %>%
  group_by(Species) %>%
  summarise(
    Mean = mean(Sepal.Length),
    SE = sd(Sepal.Length) / sqrt(n()),
    t_crit = qt(0.975, n()-1),
    CI_Lower = Mean - t_crit * SE,
    CI_Upper = Mean + t_crit * SE
  )
cat("\nUsing t-distribution:\n")
print(ci_species_t)

cat("\n=== Question 26 Complete ===\n")