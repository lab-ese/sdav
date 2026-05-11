# ============================================================================
# QUESTION 20: Perform Chi-square test
# ============================================================================

# ============================================================================
# Chi-square test: Test independence between categorical variables
# ============================================================================

cat("=== Chi-square Test ===\n\n")

# Load Titanic data
titanic_url <- "https://raw.githubusercontent.com/datasets/titanic/master/train.csv"
titanic_data <- tryCatch({
  read.csv(titanic_url)
}, error = function(e) {
  set.seed(42)
  data.frame(
    Survived = sample(c(0,1), 100, replace = TRUE, prob = c(0.6, 0.4)),
    Sex = sample(c("male","female"), 100, replace = TRUE),
    Pclass = sample(1:3, 100, replace = TRUE)
  )
})

# ============================================================================
# Example 1: Sex vs Survival
# ============================================================================
cat("=== Example 1: Sex vs Survival ===\n")
cat("H0: Sex and Survival are independent\n")
cat("H1: Sex and Survival are not independent\n\n")

contingency1 <- table(titanic_data$Sex, titanic_data$Survived)
print(contingency1)

chi_result1 <- chisq.test(contingency1)
print(chi_result1)

cat("\nInterpretation:\n")
if (chi_result1$p.value < 0.05) {
  cat("p-value < 0.05: Reject H0 - significant association between Sex and Survival\n")
} else {
  cat("p-value >= 0.05: Fail to reject H0\n")
}

# ============================================================================
# Example 2: Pclass vs Survival
# ============================================================================
cat("\n=== Example 2: Pclass vs Survival ===\n")
contingency2 <- table(titanic_data$Pclass, titanic_data$Survived)
print(contingency2)

chi_result2 <- chisq.test(contingency2)
print(chi_result2)

# ============================================================================
# Example 3: Sex vs Pclass
# ============================================================================
cat("\n=== Example 3: Sex vs Pclass ===\n")
contingency3 <- table(titanic_data$Sex, titanic_data$Pclass)
print(contingency3)

chi_result3 <- chisq.test(contingency3)
print(chi_result3)

# ============================================================================
# Using iris dataset (create categorical from continuous)
# ============================================================================
cat("\n=== Example 4: Sepal.Length Category vs Species ===\n")
iris$SepalCat <- cut(iris$Sepal.Length, breaks = c(0, 5.5, 6.5, 10), 
                     labels = c("Small", "Medium", "Large"))

contingency4 <- table(iris$SepalCat, iris$Species)
print(contingency4)

chi_result4 <- chisq.test(contingency4)
print(chi_result4)

# ============================================================================
# Manual chi-square calculation
# ============================================================================
cat("\n=== Manual Calculation: Sex vs Survival ===\n")

# Get expected frequencies
n <- sum(contingency1)
row_totals <- rowSums(contingency1)
col_totals <- colSums(contingency1)

cat("Observed frequencies:\n")
print(contingency1)

cat("\nExpected frequencies:\n")
expected <- outer(row_totals, col_totals) / n
print(round(expected, 2))

# Chi-square statistic
chi_sq <- sum((contingency1 - expected)^2 / expected)
cat("\nChi-square statistic:", round(chi_sq, 4), "\n")

# Degrees of freedom
df <- (nrow(contingency1) - 1) * (ncol(contingency1) - 1)
cat("Degrees of freedom:", df, "\n")

# p-value
p_val <- 1 - pchisq(chi_sq, df)
cat("p-value:", format(p_val, digits = 6), "\n")

cat("\n=== Question 20 Complete ===\n")