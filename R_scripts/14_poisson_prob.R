# ============================================================================
# QUESTION 14: Compute Poisson probability
# ============================================================================

# ============================================================================
# Poisson Distribution: P(X=k) = (lambda^k * e^(-lambda)) / k!
# ============================================================================

cat("=== Poisson Probability P(X=k) ===\n\n")

# Example 1: Basic Poisson calculation
lambda <- 3  # Average rate
x <- 2       # Number of events

prob <- dpois(x, lambda = lambda)
cat("Example 1: lambda=3 (avg events), find P(X=2)\n")
cat("P(X=2) = dpois(2, lambda=3)\n")
cat("P(X=2) =", round(prob, 6), "\n")

# Example 2: Different lambda values
cat("\n=== Example 2: Various lambda for X=2 ===\n")
for (l in c(1, 2, 3, 5, 10)) {
  p <- dpois(2, lambda = l)
  cat(sprintf("  lambda=%d: P(X=2) = %.6f\n", l, p))
}

# Example 3: Complete distribution for lambda=4
cat("\n=== Example 3: Complete distribution for lambda=4 ===\n")
cat("X\tP(X=k)\n")
cat("-\t-------\n")
probs <- dpois(0:15, lambda = 4)
for (i in 0:15) {
  cat(sprintf("%d\t%.6f\n", i, probs[i+1]))
}

# Example 4: Using average from real data (mtcars)
cat("\n=== Example 4: Using mtcars HP average ===\n")
avg_hp <- mean(mtcars$hp)
cat("Average HP in mtcars:", round(avg_hp, 2), "\n")
# Simulate: probability of exactly 100 HP events (interpretive)
prob100 <- dpois(100, lambda = avg_hp)
cat("P(exactly 100 HP) with lambda=", round(avg_hp, 2), ":", prob100, "\n")
cat("(Note: This is illustrative as HP is continuous)\n")

# Example 5: Iris data
cat("\n=== Example 5: Iris Petal.Length distribution ===\n")
mean_petal <- mean(iris$Petal.Length)
cat("Mean petal length:", round(mean_petal, 2), "\n")
prob_5 <- dpois(5, lambda = mean_petal)
cat("P(X=5) with lambda =", round(mean_petal, 2), ":", round(prob_5, 6), "\n")

# Visualization
cat("\n=== Poisson Distribution Plot ===\n")
png("poisson_prob.png", width = 800, height = 600)
par(mfrow = c(1, 2))

# Plot for lambda=3
probs3 <- dpois(0:12, 3)
barplot(probs3, names.arg = 0:12,
        main = "Poisson (lambda=3)",
        xlab = "k", ylab = "P(X=k)",
        col = "purple")

# Plot comparison for different lambda
probs2 <- dpois(0:12, 2)
probs5 <- dpois(0:12, 5)
plot(0:12, probs2, type = "b", col = "blue", pch = 19,
     main = "Poisson: Different lambda", xlab = "k", ylab = "P")
lines(0:12, probs3, type = "b", col = "red", pch = 19)
lines(0:12, probs5, type = "b", col = "green", pch = 19)
legend("topright", c("lambda=2", "lambda=3", "lambda=5"),
       col = c("blue", "red", "green"), pch = 19)

dev.off()
cat("Saved: poisson_prob.png\n")

# Manual verification
cat("\n=== Manual Calculation Verification ===\n")
# P(X=2) for lambda=3: (3^2 * e^(-3)) / 2! = 9 * e^(-3) / 2
manual_prob <- (3^2 * exp(-3)) / 2
cat("Manual: (3^2 * e^(-3)) / 2! =", round(manual_prob, 6), "\n")
cat("dpois result:", round(prob, 6), "\n")
cat("Match:", ifelse(abs(prob - manual_prob) < 0.0001, "YES", "NO"), "\n")

# Cumulative probability
cat("\n=== Cumulative Probability ===\n")
cat("P(X <= 2) with lambda=3:\n")
cat("ppois(2, lambda=3) =", round(ppois(2, lambda = 3), 6), "\n")
cat("P(X > 2) = 1 - P(X <= 2) =", round(1 - ppois(2, lambda = 3), 6), "\n")

cat("\n=== Question 14 Complete ===\n")