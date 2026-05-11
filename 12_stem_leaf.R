# ============================================================================
# QUESTION 12: Create Stem-and-leaf plot and interpret
# ============================================================================

# Load datasets
data(iris)
data(mtcars)

# ============================================================================
# STEM-AND-LEAF PLOTS using built-in stem() function
# ============================================================================

cat("=== Stem-and-Leaf Plot 1: Iris Sepal.Length ===\n")
cat("Interpretation: Most values between 4.5-7.0 cm, symmetric distribution\n\n")
stem(iris$Sepal.Length)

cat("\n=== Stem-and-Leaf Plot 2: Iris Sepal.Width ===\n")
cat("Interpretation: Values concentrated 2.5-3.5 cm range, slight left skew\n\n")
stem(iris$Sepal.Width)

cat("\n=== Stem-and-Leaf Plot 3: Iris Petal.Length ===\n")
cat("Interpretation: Clear separation - setosa (small ~1-2), others larger (~3-7)\n\n")
stem(iris$Petal.Length)

cat("\n=== Stem-and-Leaf Plot 4: mtcars MPG ===\n")
cat("Interpretation: Range from ~10 to ~35, slight right skew\n\n")
stem(mtcars$mpg)

cat("\n=== Stem-and-Leaf Plot 5: mtcars Horsepower ===\n")
cat("Interpretation: Wide range 50-335, heavily right-skewed\n\n")
stem(mtcars$hp)

# ============================================================================
# Custom stem-and-leaf plot function
# ============================================================================

cat("\n=== Custom Stem-and-Leaf ===\n")

custom_stem_leaf <- function(x, scale = 1) {
  x <- x[!is.na(x)]
  x <- round(x, 1)
  stem_values <- floor(x * 10)
  leaves <- (x * 10) - stem_values
  
  stem_table <- table(stem_values)
  cat("Stem | Leaves\n")
  cat("-----|----------------\n")
  for (i in sort(as.numeric(names(stem_table)))) {
    leaf_vals <- sort((stem_values[stem_values == i] %% 10))
    leaf_str <- paste(leaf_vals, collapse = " ")
    cat(sprintf("%3d  | %s\n", i, leaf_str))
  }
}

cat("\nCustom stem-and-leaf for Iris Sepal.Length:\n")
custom_stem_leaf(iris$Sepal.Length)

# ============================================================================
# Detailed interpretation
# ============================================================================

cat("\n=== Detailed Interpretation ===\n")

# Iris analysis
cat("\n1. Iris Sepal.Length:\n")
cat("   - Minimum:", min(iris$Sepal.Length), "cm\n")
cat("   - Maximum:", max(iris$Sepal.Length), "cm\n")
cat("   - Median:", median(iris$Sepal.Length), "cm\n")
cat("   - Distribution: Approximately normal/symmetric\n")

# mtcars analysis
cat("\n2. mtcars MPG:\n")
cat("   - Minimum:", min(mtcars$mpg), "mpg\n")
cat("   - Maximum:", max(mtcars$mpg), "mpg\n")
cat("   - Median:", median(mtcars$mpg), "mpg\n")
cat("   - Distribution: Slightly right-skewed\n")

# Species comparison
cat("\n3. Iris Petal.Length by Species:\n")
for (sp in levels(iris$Species)) {
  pet_len <- iris$Petal.Length[iris$Species == sp]
  cat(sprintf("   %s: Min=%.1f, Max=%.1f, Mean=%.2f\n",
              sp, min(pet_len), max(pet_len), mean(pet_len)))
}
cat("   Interpretation: Clear separation - setosa has much smaller petals\n")

cat("\n=== Question 12 Complete ===\n")