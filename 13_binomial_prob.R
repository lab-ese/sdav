# ============================================================================
# QUESTION 13: Compute Binomial probability P(X=2)
# ============================================================================

# ============================================================================
# Binomial Distribution: P(X = k) = C(n,k) * p^k * (1-p)^(n-k)
# ============================================================================

# Example 1: Basic binomial calculation
cat("=== Binomial Probability P(X=2) ===\n\n")

# Scenario: n=10 trials, p=0.3 probability of success, find P(X=2)
n <- 10
p <- 0.3
x <- 2

prob <- dbinom(x, size = n, prob = p)
cat("Example 1: n=10, p=0.3, find P(X=2)\n")
cat("P(X=2) = dbinom(2, size=10, prob=0.3)\n")
cat("P(X=2) =", round(prob, 6), "\n")

# Example 2: Using actual data proportions
cat("\n=== Example 2: Titanic Survival Rate ===\n")
titanic_url <- "https://raw.githubusercontent.com/datasets/titanic/master/train.csv"
titanic_data <- tryCatch({
  read.csv(titanic_url)
}, error = function(e) {
  set.seed(42)
  data.frame(Survived = sample(c(0,1), 100, replace = TRUE, prob = c(0.6, 0.4)))
})

# Calculate survival rate from data
p_survival <- sum(titanic_data$Survived) / nrow(titanic_data)
cat("Survival probability from data:", round(p_survival, 4), "\n")

# Probability exactly 2 survive in 10 passengers
n2 <- 10
prob2 <- dbinom(2, size = n2, prob = p_survival)
cat("P(exactly 2 survive in 10) =", round(prob2, 6), "\n")

# Example 3: Different parameters
cat("\n=== Example 3: Various n values ===\n")
cat("p = 0.5, find P(X=2) for different n:\n")
for (n_val in c(5, 10, 15, 20)) {
  prob_val <- dbinom(2, size = n_val, prob = 0.5)
  cat(sprintf("  n=%d: P(X=2) = %.6f\n", n_val, prob_val))
}

# Example 4: Complete binomial distribution
cat("\n=== Example 4: Complete distribution for n=10, p=0.3 ===\n")
probs <- dbinom(0:10, size = 10, prob = 0.3)
cat("X\tP(X=k)\n")
cat("-\t-------\n")
for (i in 0:10) {
  cat(sprintf("%d\t%.6f\n", i, probs[i+1]))
}

cat("\nMean of distribution (np):", 10 * 0.3, "\n")
cat("Variance of distribution (np(1-p)):", 10 * 0.3 * 0.7, "\n")

# Visualization
cat("\n=== Binomial Distribution Plot ===\n")
png("binomial_p2.png", width = 800, height = 600)
barplot(probs, names.arg = 0:10,
        main = "Binomial Distribution (n=10, p=0.3)\nP(X=2) marked",
        xlab = "Number of Successes (k)", ylab = "Probability",
        col = ifelse(0:10 == 2, "red", "steelblue"))
dev.off()
cat("Saved: binomial_p2.png\n")

# Manual calculation (using combinations)
cat("\n=== Manual Calculation Verification ===\n")
# P(X=2) = C(10,2) * (0.3)^2 * (0.7)^8
# C(10,2) = 10! / (2! * 8!) = 45
choose_result <- choose(10, 2)
manual_prob <- choose_result * (0.3^2) * (0.7^8)
cat("C(10,2) =", choose_result, "\n")
cat("Manual P(X=2) = 45 * 0.09 * 0.7^8\n")
cat("Manual P(X=2) =", round(manual_prob, 6), "\n")
cat("dbinom result =", round(prob, 6), "\n")
cat("Match:", ifelse(abs(prob - manual_prob) < 0.0001, "YES", "NO"), "\n")

cat("\n=== Question 13 Complete ===\n")