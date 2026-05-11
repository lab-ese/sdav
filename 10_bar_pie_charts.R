# ============================================================================
# QUESTION 10: Plot Bar chart and Pie chart
# ============================================================================

if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# Load datasets
data(iris)
data(mtcars)

# ============================================================================
# BAR CHARTS using ggplot2
# ============================================================================

# Bar chart 1: Iris species counts
cat("=== Bar Chart 1: Iris Species Distribution ===\n")
p1 <- ggplot(iris, aes(x = Species, fill = Species)) +
  geom_bar() +
  labs(title = "Bar Chart: Iris Species Distribution", x = "Species", y = "Count") +
  theme_minimal()
print(p1)

ggsave("barchart_iris_species.png", p1, width = 8, height = 6)
cat("Saved: barchart_iris_species.png\n")

# Bar chart 2: mtcars cylinder distribution
cat("\n=== Bar Chart 2: Cylinder Distribution in mtcars ===\n")
p2 <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  labs(title = "Bar Chart: Number of Cylinders", x = "Cylinders", y = "Count") +
  theme_minimal()
print(p2)

ggsave("barchart_mtcars_cylinders.png", p2, width = 8, height = 6)
cat("Saved: barchart_mtcars_cylinders.png\n")

# Bar chart 3: Average MPG by cylinders
cat("\n=== Bar Chart 3: Average MPG by Cylinders ===\n")
mpg_by_cyl <- mtcars %>%
  group_by(cyl) %>%
  summarise(avg_mpg = mean(mpg))

p3 <- ggplot(mpg_by_cyl, aes(x = factor(cyl), y = avg_mpg, fill = factor(cyl))) +
  geom_bar(stat = "identity") +
  labs(title = "Bar Chart: Average MPG by Cylinders", x = "Cylinders", y = "Average MPG") +
  theme_minimal()
print(p3)

ggsave("barchart_avg_mpg_cylinders.png", p3, width = 8, height = 6)
cat("Saved: barchart_avg_mpg_cylinders.png\n")

# ============================================================================
# PIE CHARTS
# ============================================================================

# Pie chart 1: Iris species distribution
cat("\n=== Pie Chart 1: Iris Species Distribution ===\n")
species_counts <- as.data.frame(table(iris$Species))
colnames(species_counts) <- c("Species", "Count")

p4 <- ggplot(species_counts, aes(x = "", y = Count, fill = Species)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart: Iris Species Distribution") +
  theme_minimal() +
  theme(axis.text.x = element_blank())
print(p4)

ggsave("piechart_iris_species.png", p4, width = 8, height = 6)
cat("Saved: piechart_iris_species.png\n")

# Pie chart 2: Cylinder distribution
cat("\n=== Pie Chart 2: Cylinder Distribution ===\n")
cyl_counts <- as.data.frame(table(mtcars$cyl))
colnames(cyl_counts) <- c("Cylinders", "Count")

p5 <- ggplot(cyl_counts, aes(x = "", y = Count, fill = Cylinders)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart: Cylinder Distribution") +
  theme_minimal() +
  theme(axis.text.x = element_blank())
print(p5)

ggsave("piechart_mtcars_cylinders.png", p5, width = 8, height = 6)
cat("Saved: piechart_mtcars_cylinders.png\n")

# ============================================================================
# BASE R BAR AND PIE CHARTS
# ============================================================================

cat("\n=== Base R Plots ===\n")

# Bar chart using base R
png("barchart_base_iris.png", width = 800, height = 600)
barplot(table(iris$Species),
        main = "Bar Chart: Iris Species (Base R)",
        xlab = "Species", ylab = "Count",
        col = c("cyan", "magenta", "yellow"),
        ylim = c(0, 60))
dev.off()
cat("Saved: barchart_base_iris.png\n")

# Pie chart using base R
png("piechart_base_iris.png", width = 800, height = 600)
pie(table(iris$Species),
    main = "Pie Chart: Iris Species (Base R)",
    col = c("cyan", "magenta", "yellow"))
dev.off()
cat("Saved: piechart_base_iris.png\n")

# Bar chart with horizontal bars
png("barchart_horizontal.png", width = 800, height = 600)
barplot(table(mtcars$cyl),
        main = "Bar Chart: Cylinders (Horizontal)",
        xlab = "Count", ylab = "Cylinders",
        col = rainbow(3),
        horiz = TRUE)
dev.off()
cat("Saved: barchart_horizontal.png\n")

cat("\n=== Question 10 Complete ===\n")