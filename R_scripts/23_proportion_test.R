# ============================================================================
# QUESTION 23: Perform Proportion test
# ============================================================================

# ============================================================================
# Proportion test: Compare observed proportion to expected
# ============================================================================

cat("=== Proportion Test ===\n\n")

# ============================================================================
# Example 1: One-sample proportion test - Titanic survival
# ============================================================================
cat("=== Example 1: Titanic Survival Rate ===\n")
cat("H0: survival rate = 0.5 (50%)\n")
cat("H1: survival rate != 0.5\n\n")

titanic_url <- "https://raw.githubusercontent.com/datasets/titanic/master/train.csv"
titanic_data <- tryCatch({
  read.csv(titanic_url)
}, error = function(e) {
  set.seed(42)
  data.frame(Survived = sample(c(0,1), 100, replace = TRUE, prob = c(0.6, 0.4)))
})

n_survived <- sum(titanic_data$Survived)
n_total <- nrow(titanic_data)
p_observed <- n_survived / n_total

cat("Survived:", n_survived, "out of", n_total, "\n")
cat("Observed proportion:", round(p_observed, 4), "\n")

prop_result1 <- prop.test(n_survived, n = n_total, p = 0.5)
print(prop_result1)

cat("\nInterpretation:\n")
if (prop_result1$p.value < 0.05) {
  cat("p-value < 0.05: Reject H0 - survival rate significantly different from 50%\n")
} else {
  cat("p-value >= 0.05: Fail to reject H0\n")
}

# ============================================================================
# Example 2: Two-sample proportion test - Male vs Female survival
# ============================================================================
cat("\n=== Example 2: Survival Rate - Male vs Female ===\n")
cat("H0: survival rates are equal\n")
cat("H1: survival rates are different\n\n")

n_male <- sum(titanic_data$Sex == "male")
n_female <- sum(titanic_data$Sex == "female")
survived_male <- sum(titanic_data$Survived == 1 & titanic_data$Sex == "male")
survived_female <- sum(titanic_data$Survived == 1 & titanic_data$Sex == "female")

cat("Males: ", survived_male, "/", n_male, " = ", round(survived_male/n_male, 4), "\n")
cat("Females:", survived_female, "/", n_female, " = ", round(survived_female/n_female, 4), "\n")

prop_result2 <- prop.test(c(survived_male, survived_female), c(n_male, n_female))
print(prop_result2)

# ============================================================================
# Example 3: Different confidence level
# ============================================================================
cat("\n=== Example 3: 99% Confidence Interval for Survival ===\n")
prop_result3 <- prop.test(n_survived, n = n_total, conf.level = 0.99)
cat("99% CI: ", round(prop_result3$conf.int[1], 4), "to", round(prop_result3$conf.int[2], 4), "\n")

# ============================================================================
# Manual calculation
# ============================================================================
cat("\n=== Manual Calculation ===\n")

# Z-test for proportion
p0 <- 0.5
se <- sqrt(p0 * (1 - p0) / n_total)
z_stat <- (p_observed - p0) / se
p_value <- 2 * pnorm(-abs(z_stat))

cat("Z-statistic =", round(z_stat, 4), "\n")
cat("p-value =", format(p_value, digits = 6), "\n")

# ============================================================================
# Chi-square test equivalent for proportions
# ============================================================================
cat("\n=== Chi-square test for 2x2 (equivalent) ===\n")
contingency <- matrix(c(survived_male, n_male - survived_male,
                       survived_female, n_female - survived_female),
                     nrow = 2)
chi_result <- chisq.test(contingency)
print(chi_result)

cat("\n=== Question 23 Complete ===\n")