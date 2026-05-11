# ============================================================================
# QUESTION 19: Conduct One-way ANOVA
# ============================================================================

if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

# ============================================================================
# One-way ANOVA: Compare means across 3+ groups
# ============================================================================

cat("=== One-way ANOVA ===\n\n")

# ============================================================================
# Example 1: Sepal.Length across Iris Species
# ============================================================================
cat("=== Example 1: Iris Sepal.Length by Species ===\n")
cat("H0: All species have equal mean Sepal.Length\n")
cat("H1: At least one species differs\n\n")

anova_result1 <- aov(Sepal.Length ~ Species, data = iris)
print(summary(anova_result1))

cat("\nInterpretation:\n")
p_val1 <- summary(anova_result1)[[1]]$"Pr(>F)"[1]
if (p_val1 < 0.05) {
  cat("p-value < 0.05: Reject H0 - significant difference between species\n")
} else {
  cat("p-value >= 0.05: Fail to reject H0\n")
}

# ============================================================================
# Example 2: MPG across Cylinder types
# ============================================================================
cat("\n=== Example 2: mtcars MPG by Cylinders ===\n")
cat("H0: All cylinder types have equal mean MPG\n")
cat("H1: At least one differs\n\n")

anova_result2 <- aov(mpg ~ factor(cyl), data = mtcars)
print(summary(anova_result2))

p_val2 <- summary(anova_result2)[[1]]$"Pr(>F)"[1]
cat("\nInterpretation:\n")
if (p_val2 < 0.05) {
  cat("p-value < 0.05: Reject H0 - significant difference\n")
} else {
  cat("p-value >= 0.05: Fail to reject H0\n")
}

# ============================================================================
# Post-hoc test: Tukey HSD
# ============================================================================
cat("\n=== Post-hoc: Tukey HSD for MPG ===\n")
tukey_result <- TukeyHSD(anova_result2)
print(tukey_result)

# ============================================================================
# Example 3: Petal.Length across Species
# ============================================================================
cat("\n=== Example 3: Iris Petal.Length by Species ===\n")
anova_result3 <- aov(Petal.Length ~ Species, data = iris)
print(summary(anova_result3))

# ============================================================================
# Manual ANOVA calculation
# ============================================================================
cat("\n=== Manual ANOVA Calculation: Iris Sepal.Length ===\n")

# Group means
group_means <- iris %>%
  group_by(Species) %>%
  summarise(mean = mean(Sepal.Length), n = n())
print(group_means)

# Overall mean
grand_mean <- mean(iris$Sepal.Length)

# SST (Total Sum of Squares)
SST <- sum((iris$Sepal.Length - grand_mean)^2)

# SSB (Between Groups)
ssb <- sum(group_means$n * (group_means$mean - grand_mean)^2)

# SSW (Within Groups)
ssw <- SST - ssb

# Degrees of freedom
df_between <- length(unique(iris$Species)) - 1
df_within <- nrow(iris) - length(unique(iris$Species))

# Mean squares
MSB <- ssb / df_between
MSW <- ssw / df_within

# F-statistic
F_stat <- MSB / MSW

# p-value
p_val <- 1 - pf(F_stat, df_between, df_within)

cat("\nSST (Total):", round(SST, 4), "\n")
cat("SSB (Between):", round(ssb, 4), "\n")
cat("SSW (Within):", round(ssw, 4), "\n")
cat("df (between):", df_between, "\n")
cat("df (within):", df_within, "\n")
cat("MSB:", round(MSB, 4), "\n")
cat("MSW:", round(MSW, 4), "\n")
cat("F-statistic:", round(F_stat, 4), "\n")
cat("p-value:", format(p_val, digits = 6), "\n")

cat("\n=== Question 19 Complete ===\n")