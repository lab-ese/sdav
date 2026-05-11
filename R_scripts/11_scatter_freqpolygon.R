# ============================================================================
# QUESTION 11: Plot Scatter plot and Frequency polygon
# ============================================================================

if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# Load datasets
data(iris)
data(mtcars)

# ============================================================================
# SCATTER PLOTS using ggplot2
# ============================================================================

# Scatter plot 1: Iris Sepal.Length vs Sepal.Width
cat("=== Scatter Plot 1: Sepal Length vs Sepal.Width ===\n")
p1 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot: Sepal Length vs Sepal Width",
       x = "Sepal Length (cm)", y = "Sepal Width (cm)") +
  theme_minimal()
print(p1)

ggsave("scatter_iris_sepal.png", p1, width = 8, height = 6)
cat("Saved: scatter_iris_sepal.png\n")

# Scatter plot 2: mtcars Weight vs MPG
cat("\n=== Scatter Plot 2: Weight vs MPG ===\n")
p2 <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot: Weight vs MPG",
       x = "Weight (1000 lbs)", y = "Miles per Gallon") +
  theme_minimal()
print(p2)

ggsave("scatter_mtcars_weight_mpg.png", p2, width = 8, height = 6)
cat("Saved: scatter_mtcars_weight_mpg.png\n")

# Scatter plot 3: Iris Petal Length vs Petal Width
cat("\n=== Scatter Plot 3: Petal Length vs Petal Width ===\n")
p3 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot: Petal Length vs Petal Width",
       x = "Petal Length (cm)", y = "Petal Width (cm)") +
  theme_minimal()
print(p3)

ggsave("scatter_iris_petal.png", p3, width = 8, height = 6)
cat("Saved: scatter_iris_petal.png\n")

# Scatter plot 4: mtcars Horsepower vs MPG
cat("\n=== Scatter Plot 4: Horsepower vs MPG ===\n")
p4 <- ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(size = 3, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot: Horsepower vs MPG",
       x = "Horsepower", y = "Miles per Gallon") +
  theme_minimal()
print(p4)

ggsave("scatter_mtcars_hp_mpg.png", p4, width = 8, height = 6)
cat("Saved: scatter_mtcars_hp_mpg.png\n")

# ============================================================================
# FREQUENCY POLYGONS
# ============================================================================

# Frequency polygon 1: Iris Sepal.Length
cat("\n=== Frequency Polygon 1: Iris Sepal.Length ===\n")
p5 <- ggplot(iris, aes(x = Sepal.Length)) +
  geom_freqpoly(binwidth = 0.2, color = "steelblue", size = 1.5) +
  labs(title = "Frequency Polygon: Sepal Length",
       x = "Sepal Length (cm)", y = "Frequency") +
  theme_minimal()
print(p5)

ggsave("freqpolygon_iris_sepal.png", p5, width = 8, height = 6)
cat("Saved: freqpolygon_iris_sepal.png\n")

# Frequency polygon 2: mtcars MPG
cat("\n=== Frequency Polygon 2: mtcars MPG ===\n")
p6 <- ggplot(mtcars, aes(x = mpg)) +
  geom_freqpoly(binwidth = 2, color = "coral", size = 1.5) +
  labs(title = "Frequency Polygon: MPG Distribution",
       x = "Miles per Gallon", y = "Frequency") +
  theme_minimal()
print(p6)

ggsave("freqpolygon_mtcars_mpg.png", p6, width = 8, height = 6)
cat("Saved: freqpolygon_mtcars_mpg.png\n")

# Frequency polygon 3: Compare Species
cat("\n=== Frequency Polygon 3: Compare Species (Sepal.Width) ===\n")
p7 <- ggplot(iris, aes(x = Sepal.Width, color = Species)) +
  geom_freqpoly(binwidth = 0.1, size = 1.2) +
  labs(title = "Frequency Polygon: Sepal Width by Species",
       x = "Sepal Width (cm)", y = "Frequency") +
  theme_minimal()
print(p7)

ggsave("freqpolygon_iris_species.png", p7, width = 8, height = 6)
cat("Saved: freqpolygon_iris_species.png\n")

# ============================================================================
# Base R Plots
# ============================================================================

cat("\n=== Base R Scatter Plot ===\n")
png("scatter_base.png", width = 800, height = 600)
plot(iris$Sepal.Length, iris$Sepal.Width,
     main = "Scatter Plot: Sepal Length vs Width (Base R)",
     xlab = "Sepal Length", ylab = "Sepal Width",
     col = as.numeric(iris$Species), pch = 19)
legend("topright", legend = levels(iris$Species),
       col = 1:3, pch = 19, title = "Species")
dev.off()
cat("Saved: scatter_base.png\n")

# Base R frequency polygon
png("freqpolygon_base.png", width = 800, height = 600)
hist_sepal <- hist(iris$Sepal.Length, plot = FALSE)
lines(hist_sepal$mids, hist_sepal$counts, type = "b",
      col = "steelblue", pch = 16)
dev.off()
cat("Saved: freqpolygon_base.png\n")

cat("\n=== Question 11 Complete ===\n")