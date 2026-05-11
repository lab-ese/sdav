# ============================================================================
# QUESTION 8: Compare datasets and conclude
# ============================================================================

if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

# Load datasets
data(iris)
data(mtcars)
data(USArrests)

# ============================================================================
# Compare 1: Sepal Length across Iris Species
# ============================================================================
cat("=== Compare 1: Sepal.Length across Iris Species ===\n")
comp1 <- iris %>%
  group_by(Species) %>%
  summarise(
    Mean = mean(Sepal.Length),
    Median = median(Sepal.Length),
    SD = sd(Sepal.Length),
    Variance = var(Sepal.Length),
    Min = min(Sepal.Length),
    Max = max(Sepal.Length),
    N = n()
  )
print(comp1)

cat("\nConclusion: Setosa has notably smaller sepal length than versicolor and virginica.\n")

# ============================================================================
# Compare 2: MPG across Cylinder types in mtcars
# ============================================================================
cat("\n=== Compare 2: MPG across Cylinder types (mtcars) ===\n")
comp2 <- mtcars %>%
  group_by(cyl) %>%
  summarise(
    Mean = mean(mpg),
    Median = median(mpg),
    SD = sd(mpg),
    Variance = var(mpg),
    Min = min(mpg),
    Max = max(mpg),
    N = n()
  )
print(comp2)

cat("\nConclusion: Strong negative correlation between cylinder count and MPG.\n")

# ============================================================================
# Compare 3: Murder rates across US states (USArrests)
# ============================================================================
cat("\n=== Compare 3: Murder rates in USArrests ===\n")
comp3 <- USArrests %>%
  summarise(
    Mean = mean(Murder),
    Median = median(Murder),
    SD = sd(Murder),
    Min = min(Murder),
    Max = max(Murder),
    N = n()
  )
print(comp3)

# Compare urban vs rural (using UrbanPop)
cat("\nMurder rates by Urban Population level:\n")
USArrests$UrbanCategory <- ifelse(USArrests$UrbanPop > 65, "High Urban", "Low Urban")
comp3b <- USArrests %>%
  group_by(UrbanCategory) %>%
  summarise(
    Mean = mean(Murder),
    Median = median(Murder),
    SD = sd(Murder),
    N = n()
  )
print(comp3b)

# ============================================================================
# Compare 4: Petal Length across species
# ============================================================================
cat("\n=== Compare 4: Petal.Length across Iris Species ===\n")
comp4 <- iris %>%
  group_by(Species) %>%
  summarise(
    Mean = mean(Petal.Length),
    SD = sd(Petal.Length),
    N = n()
  )
print(comp4)

cat("\nConclusion: Very clear separation - setosa has much smaller petals.\n")

# ============================================================================
# Compare 5: Compare 3 datasets - central tendency
# ============================================================================
cat("\n=== Compare 5: Central Tendency Comparison ===\n")
comparison_df <- data.frame(
  Dataset = c("Iris Sepal.Length", "mtcars MPG", "USArrests Murder"),
  Mean = c(mean(iris$Sepal.Length), mean(mtcars$mpg), mean(USArrests$Murder)),
  Median = c(median(iris$Sepal.Length), median(mtcars$mpg), median(USArrests$Murder)),
  SD = c(sd(iris$Sepal.Length), sd(mtcars$mpg), sd(USArrests$Murder))
)
print(comparison_df)

# ============================================================================
# Statistical comparison - t-tests between groups
# ============================================================================
cat("\n=== Statistical Test: ANOVA for Iris Species ===\n")
anova_result <- aov(Sepal.Length ~ Species, data = iris)
print(summary(anova_result))
cat("\nConclusion: Significant difference exists between species (p < 0.05).\n")

cat("\n=== Statistical Test: t-test for MPG (4 vs 8 cylinders) ===\n")
mpg_4cyl <- mtcars$mpg[mtcars$cyl == 4]
mpg_8cyl <- mtcars$mpg[mtcars$cyl == 8]
t_result <- t.test(mpg_4cyl, mpg_8cyl)
print(t_result)
cat("\nConclusion: Significant difference in MPG between 4 and 8 cylinder cars.\n")

cat("\n=== Question 8 Complete ===\n")