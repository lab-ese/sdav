# ============================================================================
# QUESTION 9: Plot Histogram and Boxplot
# ============================================================================

if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# Load datasets
data(iris)
data(mtcars)

# ============================================================================
# HISTOGRAMS
# ============================================================================

# Histogram 1: Iris Sepal.Length
cat("=== Histogram 1: Iris Sepal.Length ===\n")
p1 <- ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.2, fill = "steelblue", color = "white") +
  labs(title = "Histogram of Iris Sepal Length", x = "Sepal Length (cm)", y = "Frequency") +
  theme_minimal()
print(p1)

# Save plot
ggsave("histogram_iris_sepal_length.png", p1, width = 8, height = 6)
cat("Saved: histogram_iris_sepal_length.png\n")

# Histogram 2: mtcars MPG
cat("\n=== Histogram 2: mtcars MPG ===\n")
p2 <- ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "coral", color = "white") +
  labs(title = "Histogram of Miles Per Gallon", x = "MPG", y = "Frequency") +
  theme_minimal()
print(p2)

ggsave("histogram_mtcars_mpg.png", p2, width = 8, height = 6)
cat("Saved: histogram_mtcars_mpg.png\n")

# Histogram 3: Iris Petal.Length by Species
cat("\n=== Histogram 3: Iris Petal.Length by Species ===\n")
p3 <- ggplot(iris, aes(x = Petal.Length, fill = Species)) +
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  labs(title = "Histogram of Petal Length by Species", x = "Petal Length (cm)", y = "Frequency") +
  theme_minimal()
print(p3)

ggsave("histogram_petal_length_species.png", p3, width = 8, height = 6)
cat("Saved: histogram_petal_length_species.png\n")

# ============================================================================
# BOXPLOTS
# ============================================================================

# Boxplot 1: Iris Sepal.Length by Species
cat("\n=== Boxplot 1: Iris Sepal.Length by Species ===\n")
p4 <- ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  labs(title = "Boxplot of Sepal Length by Species", x = "Species", y = "Sepal Length (cm)") +
  theme_minimal()
print(p4)

ggsave("boxplot_iris_sepal_species.png", p4, width = 8, height = 6)
cat("Saved: boxplot_iris_sepal_species.png\n")

# Boxplot 2: mtcars MPG by Cylinders
cat("\n=== Boxplot 2: mtcars MPG by Cylinders ===\n")
p5 <- ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
  geom_boxplot() +
  labs(title = "Boxplot of MPG by Number of Cylinders", x = "Cylinders", y = "MPG") +
  theme_minimal()
print(p5)

ggsave("boxplot_mtcars_mpg_cylinders.png", p5, width = 8, height = 6)
cat("Saved: boxplot_mtcars_mpg_cylinders.png\n")

# Boxplot 3: Iris Petal.Width by Species
cat("\n=== Boxplot 3: Iris Petal.Width by Species ===\n")
p6 <- ggplot(iris, aes(x = Species, y = Petal.Width, fill = Species)) +
  geom_boxplot() +
  labs(title = "Boxplot of Petal Width by Species", x = "Species", y = "Petal Width (cm)") +
  theme_minimal()
print(p6)

ggsave("boxplot_iris_petal_species.png", p6, width = 8, height = 6)
cat("Saved: boxplot_iris_petal_species.png\n")

# ============================================================================
# Base R plots (alternative)
# ============================================================================

cat("\n=== Base R Plots ===\n")

# Histogram using base R
png("histogram_base_sepal.png", width = 800, height = 600)
hist(iris$Sepal.Length, main = "Histogram: Sepal Length (Base R)",
     xlab = "Sepal Length", col = "lightblue", border = "darkblue")
dev.off()
cat("Saved: histogram_base_sepal.png\n")

# Boxplot using base R
png("boxplot_base_iris.png", width = 800, height = 600)
boxplot(Sepal.Length ~ Species, data = iris,
        main = "Boxplot: Sepal Length by Species (Base R)",
        xlab = "Species", ylab = "Sepal Length",
        col = c("lightblue", "lightgreen", "lightyellow"))
dev.off()
cat("Saved: boxplot_base_iris.png\n")

cat("\n=== Question 9 Complete ===\n")