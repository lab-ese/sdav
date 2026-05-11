# E-commerce - structure, summary, SD, variance, CV
df <- read.csv("datasets/ecommerce.csv")
print(str(df))
print(summary(df))
cat("\nSD:", sd(df$Amount), "\n")
cat("Variance:", var(df$Amount), "\n")
cat("CV:", (sd(df$Amount)/mean(df$Amount))*100, "%\n")