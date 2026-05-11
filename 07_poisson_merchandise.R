# Poisson distribution - Merchandise Sales
df <- read.csv("datasets/merchandise_sales.csv")
lambda <- mean(df$Sales) / 1000
cat("Lambda:", lambda, "\n")
cat("P(X=30):", dpois(30, lambda), "\n")
cat("P(X<=40):", ppois(40, lambda), "\n")

dev.new()
barplot(dpois(20:50, lambda), names=20:50, main="Poisson Distribution")