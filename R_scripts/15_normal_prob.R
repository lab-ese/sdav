# ============================================================================
# QUESTION 15: Find Normal probability P(X<60)
# ============================================================================

# ============================================================================
# Normal Distribution: X ~ N(mu, sigma^2)
# P(X < x) = pnorm(x, mean = mu, sd = sigma)
# ============================================================================

cat("=== Normal Probability P(X < 60) ===\n\n")

# Example 1: Standard normal calculation
cat("Example 1: X ~ N(50, 10), find P(X < 60)\n")
mu <- 50
sigma <- 10
x <- 60

prob <- pnorm(x, mean = mu, sd = sigma)
cat("P(X < 60) = pnorm(60, mean=50, sd=10)\n")
cat("Z-score = (60-50)/10 = 1\n")
cat("P(X < 60) =", round(prob, 6), "\n")

# Example 2: Using Iris dataset
cat("\n=== Example 2: Iris Sepal.Length ===\n")
mu_iris <- mean(iris$Sepal.Length)
sigma_iris <- sd(iris$Sepal.Length)
x_iris <- 6.0

prob_iris <- pnorm(x_iris, mean = mu_iris, sd = sigma_iris)
cat("Sepal.Length ~ N(", round(mu_iris, 2), ", ", round(sigma_iris, 2), ")\n", sep = "")
cat("P(Sepal.Length < 6.0) =", round(prob_iris, 6), "\n")

# Example 3: Using mtcars MPG
cat("\n=== Example 3: mtcars MPG ===\n")
mu_mtcars <- mean(mtcars$mpg)
sigma_mtcars <- sd(mtcars$mpg)
x_mtcars <- 20

prob_mtcars <- pnorm(x_mtcars, mean = mu_mtcars, sd = sigma_mtcars)
cat("MPG ~ N(", round(mu_mtcars, 2), ", ", round(sigma_mtcars, 2), ")\n", sep = "")
cat("P(MPG < 20) =", round(prob_mtcars, 6), "\n")

# Example 4: Different x values
cat("\n=== Example 4: Various P(X < x) for N(50, 10) ===\n")
for (x_val in c(30, 40, 50, 60, 70, 80)) {
  p <- pnorm(x_val, mean = 50, sd = 10)
  z <- (x_val - 50) / 10
  cat(sprintf("  P(X < %d) = %.4f (Z = %.2f)\n", x_val, p, z))
}

# Example 5: Symmetric probability
cat("\n=== Example 5: P(X > 60) and P(40 < X < 60) ===\n")
cat("P(X > 60) = 1 - P(X < 60) =", round(1 - prob, 6), "\n")
cat("P(40 < X < 60) = P(X < 60) - P(X < 40)\n")
p_40 <- pnorm(40, mean = 50, sd = 10)
cat("        =", round(prob, 4), "-", round(p_40, 4), "=", round(prob - p_40, 4), "\n")

# Visualization
cat("\n=== Normal Distribution Plot ===\n")
png("normal_prob_x60.png", width = 800, height = 600)

# Plot normal curve
x_vals <- seq(20, 80, length = 200)
y_vals <- dnorm(x_vals, mean = 50, sd = 10)
plot(x_vals, y_vals, type = "l", col = "darkblue", lwd = 2,
     main = "Normal Distribution N(50, 10)\nP(X < 60) shaded",
     xlab = "X", ylab = "Density")

# Shade area P(X < 60)
x_fill <- seq(20, 60, length = 100)
y_fill <- dnorm(x_fill, mean = 50, sd = 10)
polygon(c(20, x_fill, 60), c(0, y_fill, 0), col = "lightblue", border = NA)

# Add vertical line at x=60
abline(v = 60, col = "red", lty = 2)
text(60, 0.03, "x=60", col = "red")

# Add legend
legend("topright", legend = c("P(X<60)", "Area"),
       col = c("red", "lightblue"), pch = c(NA, 15))

dev.off()
cat("Saved: normal_prob_x60.png\n")

# Z-score explanation
cat("\n=== Z-score Calculation ===\n")
cat("For X ~ N(mu, sigma), Z = (X - mu) / sigma\n")
cat("Z = (60 - 50) / 10 = 1\n")
cat("Using Z-table: P(Z < 1) = 0.8413\n")
cat("pnorm gives:", round(pnorm(1), 4), "\n")

# Standard normal verification
cat("\n=== Standard Normal Verification ===\n")
cat("P(Z < 1) using pnorm(1) =", round(pnorm(1), 6), "\n")
cat("This equals P(X < 60) for N(50, 10):", round(prob, 6), "\n")

cat("\n=== Question 15 Complete ===\n")