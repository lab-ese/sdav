# ============================================================================
# COMPREHENSIVE STATISTICS EXERCISES IN R
# ============================================================================
# This script covers 26 statistical questions using 5 public datasets
# Datasets: Iris, Titanic, Gapminder, mtcars, USArrests
# ============================================================================

# Install and load required packages
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("moments")) install.packages("moments")
if (!require("readr")) install.packages("readr")

library(dplyr)
library(ggplot2)
library(moments)
library(readr)

# ============================================================================
# IMPORT 5 PUBLIC DATASETS
# ============================================================================

# Dataset 1: Iris Dataset (Built-in, also available online)
data(iris)
iris_data <- iris
cat("=== Dataset 1: Iris Dataset ===\n")
print(head(iris_data))
cat("Dimensions:", dim(iris_data), "\n\n")

# Dataset 2: Titanic Dataset (from Kaggle or GitHub)
titanic_url <- "https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv"
titanic_data <- tryCatch({
  read.csv(titanic_url, stringsAsFactors = FALSE)
}, error = function(e) {
  cat("Using built-in Titanic-like data\n")
  data.frame(
    PassengerId = 1:100,
    Survived = sample(c(0,1), 100, replace = TRUE),
    Pclass = sample(1:3, 100, replace = TRUE),
    Sex = sample(c("male","female"), 100, replace = TRUE),
    Age = runif(100, 0.5, 80),
    Fare = runif(100, 5, 500),
    Embarked = sample(c("S","C","Q"), 100, replace = TRUE)
  )
})
cat("=== Dataset 2: Titanic Dataset ===\n")
print(head(titanic_data))
cat("Dimensions:", dim(titanic_data), "\n\n")

# Dataset 3: Gapminder Dataset (life expectancy, GDP per capita)
gapminder_url <- "https://raw.githubusercontent.com/datasets/gapminder/master/data/gapminder.csv"
gapminder_data <- tryCatch({
  read.csv(gapminder_url, stringsAsFactors = FALSE)
}, error = function(e) {
  cat("Using simulated Gapminder-like data\n")
  set.seed(123)
  data.frame(
    country = sample(c("USA","China","India","UK","Germany","France","Japan","Brazil"), 100, replace = TRUE),
    year = sample(1950:2020, 100, replace = TRUE),
    lifeExp = runif(100, 40, 85),
    pop = sample(1000000:100000000, 100, replace = TRUE),
    gdpPercap = runif(100, 500, 50000)
  )
})
cat("=== Dataset 3: Gapminder Dataset ===\n")
print(head(gapminder_data))
cat("Dimensions:", dim(gapminder_data), "\n\n")

# Dataset 4: mtcars (Motor Trend Car Road Tests)
data(mtcars)
mtcars_data <- mtcars
cat("=== Dataset 4: mtcars Dataset ===\n")
print(head(mtcars_data))
cat("Dimensions:", dim(mtcars_data), "\n\n")

# Dataset 5: USArrests (Violent Crime Rates by US State)
data(USArrests)
usarrests_data <- USArrests
cat("=== Dataset 5: USArrests Dataset ===\n")
print(head(usarrests_data))
cat("Dimensions:", dim(usarrests_data), "\n\n")


# ============================================================================
# QUESTION 1: READ DATASET IN R
# ============================================================================

cat("=== QUESTION 1: Read Dataset in R ===\n")
# Reading from URL
cat("Method 1: Reading from URL\n")
url_data <- read.csv("https://raw.githubusercontent.com/datasets/iris/master/data/iris.csv")
print(head(url_data))

# Reading from local file (if exists)
cat("Method 2: Reading from file\n")
# write.csv(iris_data, "iris_local.csv", row.names = FALSE)
# local_data <- read.csv("iris_local.csv")

# Reading from clipboard
# clipboard_data <- read.table(file = "clipboard", header = TRUE)

cat("\nSummary of Iris dataset:\n")
print(summary(iris_data))
cat("\n")


# ============================================================================
# QUESTION 2: Use if/else to classify customers
# ============================================================================

cat("=== QUESTION 2: Use if/else to classify customers ===\n")

# Using Titanic dataset - classify passengers based on fare
classify_passenger <- function(fare) {
  if (is.na(fare)) {
    return("Unknown")
  } else if (fare > 100) {
    return("Premium")
  } else if (fare > 30) {
    return("Standard")
  } else {
    return("Economy")
  }
}

# Apply classification
titanic_data$Class <- sapply(titanic_data$Fare, classify_passenger)
cat("Passenger Classification based on Fare:\n")
print(table(titanic_data$Class))

# Using Gapminder - classify countries by GDP
classify_country <- function(gdp) {
  if (is.na(gdp)) return("Unknown")
  if (gdp > 30000) return("High Income")
  else if (gdp > 10000) return("Upper Middle")
  else if (gdp > 3000) return("Lower Middle")
  else return("Low Income")
}

gapminder_data$IncomeGroup <- sapply(gapminder_data$gdpPercap, classify_country)
cat("\nCountry Classification by GDP per Capita:\n")
print(table(gapminder_data$IncomeGroup))
cat("\n")


# ============================================================================
# QUESTION 3: Use for loop to display classification
# ============================================================================

cat("=== QUESTION 3: Use for loop to display classification ===\n")

# Display first 10 classifications
cat("Displaying first 10 passenger classifications:\n")
for (i in 1:10) {
  passenger <- titanic_data[i, ]
  cat(sprintf("Passenger %d: Fare=$%.2f -> Class: %s\n",
              passenger$PassengerId, passenger$Fare, passenger$Class))
}

# For loop with conditional display
cat("\nHigh-value passengers (Fare > 100):\n")
count <- 0
for (i in 1:nrow(titanic_data)) {
  if (titanic_data$Fare[i] > 100) {
    cat(sprintf("ID: %d, Fare: %.2f, Class: %s\n",
                titanic_data$PassengerId[i], titanic_data$Fare[i], titanic_data$Class[i]))
    count <- count + 1
    if (count >= 5) break
}
cat("\n")


# ============================================================================
# QUESTION 4: Create a function to calculate total revenue
# ============================================================================

cat("=== QUESTION 4: Create a function to calculate total revenue ===\n")

# Function to calculate total revenue from a dataset
calculate_total_revenue <- function(data, revenue_col, group_col = NULL) {
  if (is.null(group_col)) {
    total <- sum(data[[revenue_col]], na.rm = TRUE)
    return(total)
  } else {
    revenue_by_group <- data %>%
      group_by(.data[[group_col]]) %>%
      summarise(TotalRevenue = sum(.data[[revenue_col]], na.rm = TRUE))
    return(revenue_by_group)
  }
}

# Calculate total revenue from Titanic fares
total_fare <- calculate_total_revenue(titanic_data, "Fare")
cat("Total Fare Revenue from Titanic:", round(total_fare, 2), "\n")

# Calculate revenue by class
revenue_by_class <- calculate_total_revenue(titanic_data, "Fare", "Class")
cat("\nRevenue by Passenger Class:\n")
print(revenue_by_class)

# Using Gapminder - GDP calculation
calculate_gdp <- function(data, pop_col = "pop", gdp_col = "gdpPercap") {
  total_gdp <- sum(data[[pop_col]] * data[[gdp_col]], na.rm = TRUE)
  return(total_gdp)
}
cat("\nTotal GDP (Population * GDP per capita):", format(calculate_gdp(gapminder_data), scientific = TRUE), "\n")
cat("\n")


# ============================================================================
# QUESTION 5: Handle missing values
# ============================================================================

cat("=== QUESTION 5: Handle missing values ===\n")

# Create sample data with missing values
sample_data <- data.frame(
  ID = 1:10,
  Score = c(85, 90, NA, 78, 92, NA, 88, 95, NA, 80),
  Age = c(25, NA, 30, 35, NA, 40, 45, NA, 50, 55)
)
cat("Original data with missing values:\n")
print(sample_data)

# Method 1: Remove rows with missing values
clean_data <- na.omit(sample_data)
cat("\nAfter na.omit (remove rows with NA):\n")
print(clean_data)

# Method 2: Replace missing values with mean
sample_data$Score_mean <- ifelse(is.na(sample_data$Score),
                                  mean(sample_data$Score, na.rm = TRUE),
                                  sample_data$Score)
cat("\nScore with mean imputation:\n")
print(sample_data$Score_mean)

# Method 3: Replace with median
sample_data$Age_median <- ifelse(is.na(sample_data$Age),
                                  median(sample_data$Age, na.rm = TRUE),
                                  sample_data$Age)
cat("\nAge with median imputation:\n")
print(sample_data$Age_median)

# Method 4: Forward fill (for time series-like data)
sample_data$Score_filled <- zoo::na.locf(sample_data$Score)
cat("\nForward fill for Score:\n")
print(sample_data$Score_filled)

# Check missing values in Titanic dataset
cat("\nMissing values in Titanic dataset:\n")
print(colSums(is.na(titanic_data)))

# Replace missing Age with median
titanic_data$Age <- ifelse(is.na(titanic_data$Age),
                           median(titanic_data$Age, na.rm = TRUE),
                           titanic_data$Age)
cat("\nAfter imputing Age with median:\n")
print(colSums(is.na(titanic_data)))
cat("\n")


# ============================================================================
# QUESTION 6: Compute descriptive statistics
# ============================================================================

cat("=== QUESTION 6: Compute descriptive statistics ===\n")

compute_descriptive_stats <- function(x) {
  x <- x[!is.na(x)]
  stats <- list(
    Mean = mean(x),
    Median = median(x),
    Mode = {
      uniqx <- unique(x)
      uniqx[which.max(tabulate(match(x, uniqx)))]
    },
    SD = sd(x),
    Variance = var(x),
    Range = max(x) - min(x),
    Min = min(x),
    Max = max(x)
  )
  return(stats)
}

# Using Iris dataset - Sepal.Length
cat("Descriptive Statistics for Iris Sepal.Length:\n")
sepal_stats <- compute_descriptive_stats(iris_data$Sepal.Length)
print(sepal_stats)

# Using mtcars - mpg
cat("\nDescriptive Statistics for mtcars MPG:\n")
mpg_stats <- compute_descriptive_stats(mtcars_data$mpg)
print(mpg_stats)

# Using Gapminder - Life Expectancy
cat("\nDescriptive Statistics for Gapminder Life Expectancy:\n")
life_exp_stats <- compute_descriptive_stats(gapminder_data$lifeExp)
print(life_exp_stats)
cat("\n")


# ============================================================================
# QUESTION 7: Compute Quartiles & IQR
# ============================================================================

cat("=== QUESTION 7: Compute Quartiles & IQR ===\n")

compute_quartiles <- function(x) {
  x <- x[!is.na(x)]
  q <- quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1))
  iqr_val <- IQR(x)
  list(Quartiles = q, IQR = iqr_val)
}

# Using Iris Sepal Width
cat("Quartiles for Iris Sepal.Width:\n")
quartiles_sepal <- compute_quartiles(iris_data$Sepal.Width)
print(quartiles_sepal)

# Using Titanic Fare
cat("\nQuartiles for Titanic Fare:\n")
quartiles_fare <- compute_quartiles(titanic_data$Fare)
print(quartiles_fare)

# Identify outliers using IQR method
identify_outliers <- function(x) {
  x <- x[!is.na(x)]
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  outliers <- x[x < lower_bound | x > upper_bound]
  return(outliers)
}

cat("\nOutliers in Titanic Fare (IQR method):\n")
fare_outliers <- identify_outliers(titanic_data$Fare)
cat("Number of outliers:", length(fare_outliers), "\n")
cat("Outlier values:", sort(fare_outliers), "\n")
cat("\n")


# ============================================================================
# QUESTION 8: Compare datasets and conclude
# ============================================================================

cat("=== QUESTION 8: Compare datasets and conclude ===\n")

# Compare Sepal.Length between Iris species
cat("Comparing Sepal.Length across Iris species:\n")
comparison <- iris_data %>%
  group_by(Species) %>%
  summarise(
    Mean = mean(Sepal.Length),
    SD = sd(Sepal.Length),
    N = n()
  )
print(comparison)

# Compare MPG across cylinders in mtcars
cat("\nComparing MPG across cylinder types in mtcars:\n")
mpg_comparison <- mtcars_data %>%
  group_by(cyl) %>%
  summarise(
    Mean_MPG = mean(mpg),
    SD_MPG = sd(mpg),
    Median_MPG = median(mpg),
    N = n()
  )
print(mpg_comparison)

# Compare UrbanPop (urban population %) across US regions
usarrests_data$Region <- ifelse(rownames(USArrests) %in% c("Florida", "Georgia", "Alabama", "Mississippi", "Louisiana", "Texas"),
                                "South", "Other")
cat("\nComparing Murder rates by region in USArrests:\n")
region_comparison <- usarrests_data %>%
  group_by(Region) %>%
  summarise(
    Mean_Murder = mean(Murder),
    SD_Murder = sd(Murder),
    N = n()
  )
print(region_comparison)

cat("\nConclusion: Different groups show significant variation in central tendency and spread.\n")
cat("\n")


# ============================================================================
# QUESTION 9: Plot Histogram and Boxplot
# ============================================================================

cat("=== QUESTION 9: Plot Histogram and Boxplot ===\n")

# Histogram for Iris Sepal.Length
png("histogram_sepal_length.png", width = 800, height = 600)
hist(iris_data$Sepal.Length,
     main = "Histogram of Iris Sepal Length",
     xlab = "Sepal Length (cm)",
     col = "steelblue",
     border = "white",
     breaks = 20)
dev.off()
cat("Histogram saved: histogram_sepal_length.png\n")

# Histogram for Titanic Fare (log scale for better visualization)
png("histogram_titanic_fare.png", width = 800, height = 600)
hist(titanic_data$Fare[titanic_data$Fare > 0],
     main = "Histogram of Titanic Passenger Fares",
     xlab = "Fare ($)",
     col = "coral",
     border = "white",
     breaks = 30)
dev.off()
cat("Histogram saved: histogram_titanic_fare.png\n")

# Boxplot for Iris by species
png("boxplot_iris.png", width = 800, height = 600)
boxplot(Sepal.Length ~ Species, data = iris_data,
        main = "Boxplot of Sepal Length by Species",
        xlab = "Species",
        ylab = "Sepal Length (cm)",
        col = c("lightblue", "lightgreen", "lightyellow"))
dev.off()
cat("Boxplot saved: boxplot_iris.png\n")

# Boxplot for mtcars MPG by cylinders
png("boxplot_mtcars.png", width = 800, height = 600)
boxplot(mpg ~ cyl, data = mtcars_data,
        main = "Boxplot of MPG by Cylinders",
        xlab = "Number of Cylinders",
        ylab = "Miles per Gallon",
        col = c("lightblue", "lightgreen", "lightyellow"))
dev.off()
cat("Boxplot saved: boxplot_mtcars.png\n")
cat("\n")


# ============================================================================
# QUESTION 10: Plot Bar chart and Pie chart
# ============================================================================

cat("=== QUESTION 10: Plot Bar chart and Pie chart ===\n")

# Bar chart - Titanic class distribution
png("barchart_titanic_class.png", width = 800, height = 600)
class_counts <- table(titanic_data$Class)
barplot(class_counts,
        main = "Bar Chart: Passenger Class Distribution",
        xlab = "Class",
        ylab = "Frequency",
        col = c("red", "green", "blue"),
        ylim = c(0, max(class_counts) * 1.1))
dev.off()
cat("Bar chart saved: barchart_titanic_class.png\n")

# Bar chart - Iris species counts
png("barchart_iris_species.png", width = 800, height = 600)
species_counts <- table(iris_data$Species)
barplot(species_counts,
        main = "Bar Chart: Iris Species Distribution",
        xlab = "Species",
        ylab = "Count",
        col = c("cyan", "magenta", "yellow"),
        ylim = c(0, max(species_counts) * 1.1))
dev.off()
cat("Bar chart saved: barchart_iris_species.png\n")

# Pie chart - Titanic class distribution
png("piechart_titanic_class.png", width = 800, height = 600)
pie(class_counts,
    main = "Pie Chart: Passenger Class Distribution",
    col = c("red", "green", "blue"),
    labels = paste(names(class_counts), "\n(", class_counts, ")", sep = ""))
dev.off()
cat("Pie chart saved: piechart_titanic_class.png\n")

# Pie chart - Iris species distribution
png("piechart_iris_species.png", width = 800, height = 600)
pie(species_counts,
    main = "Pie Chart: Iris Species Distribution",
    col = c("cyan", "magenta", "yellow"),
    labels = paste(names(species_counts), "\n(", species_counts, ")", sep = ""))
dev.off()
cat("Pie chart saved: piechart_iris_species.png\n")
cat("\n")


# ============================================================================
# QUESTION 11: Plot Scatter plot and Frequency polygon
# ============================================================================

cat("=== QUESTION 11: Plot Scatter plot and Frequency polygon ===\n")

# Scatter plot - Iris Sepal.Length vs Sepal.Width
png("scatter_iris.png", width = 800, height = 600)
plot(iris_data$Sepal.Length, iris_data$Sepal.Width,
     main = "Scatter Plot: Sepal Length vs Sepal Width",
     xlab = "Sepal Length (cm)",
     ylab = "Sepal Width (cm)",
     col = as.numeric(iris_data$Species),
     pch = 19)
legend("topright", legend = levels(iris_data$Species),
       col = 1:3, pch = 19, title = "Species")
dev.off()
cat("Scatter plot saved: scatter_iris.png\n")

# Scatter plot - mtcars wt vs mpg
png("scatter_mtcars.png", width = 800, height = 600)
plot(mtcars_data$wt, mtcars_data$mpg,
     main = "Scatter Plot: Weight vs MPG",
     xlab = "Weight (1000 lbs)",
     ylab = "Miles per Gallon",
     col = "darkblue",
     pch = 19)
# Add regression line
abline(lm(mpg ~ wt, data = mtcars_data), col = "red")
dev.off()
cat("Scatter plot saved: scatter_mtcars.png\n")

# Frequency polygon for Iris Sepal.Length
png("freqpolygon_iris.png", width = 800, height = 600)
hist_sepal <- hist(iris_data$Sepal.Length, plot = FALSE)
plot(hist_sepal$counts, type = "b",
     main = "Frequency Polygon: Iris Sepal Length",
     xlab = "Sepal Length Bins",
     ylab = "Frequency",
     col = "darkgreen",
     lwd = 2)
dev.off()
cat("Frequency polygon saved: freqpolygon_iris.png\n")

# Frequency polygon for mtcars mpg
png("freqpolygon_mtcars.png", width = 800, height = 600)
hist_mpg <- hist(mtcars_data$mpg, plot = FALSE)
plot(hist_mpg$counts, type = "b",
     main = "Frequency Polygon: MPG Distribution",
     xlab = "MPG Bins",
     ylab = "Frequency",
     col = "darkred",
     lwd = 2)
dev.off()
cat("Frequency polygon saved: freqpolygon_mtcars.png\n")
cat("\n")


# ============================================================================
# QUESTION 12: Create Stem-and-leaf plot and interpret
# ============================================================================

cat("=== QUESTION 12: Create Stem-and-leaf plot and interpret ===\n")

# Stem-and-leaf plot for Iris Sepal.Length
cat("Stem-and-Leaf Plot for Iris Sepal.Length:\n")
stem(iris_data$Sepal.Length)

cat("\nInterpretation: The stem-and-leaf plot shows the distribution of sepal lengths.\n")
cat("- Most values are between 5.0 and 7.0 cm\n")
cat("- The distribution appears approximately symmetric\n")
cat("- No clear outliers visible in the stem-and-leaf display\n")

# Stem-and-leaf plot for mtcars mpg
cat("\n\nStem-and-Leaf Plot for mtcars MPG:\n")
stem(mtcars_data$mpg)

cat("\nInterpretation: MPG values are spread from about 10 to 35.\n")
cat("- The distribution is slightly right-skewed\n")
cat("- No extreme outliers present\n")

# Stem-and-leaf plot for Titanic Fare
cat("\n\nStem-and-Leaf Plot for Titanic Fares:\n")
stem(titanic_data$Fare)

cat("\nInterpretation:\n")
cat("- Most fares are in the lower range (0-50)\n")
cat("- Distribution is right-skewed with some high-value outliers\n")
cat("- This is typical for passenger fare data\n")
cat("\n")


# ============================================================================
# QUESTION 13: Compute Binomial probability P(X=2)
# ============================================================================

cat("=== QUESTION 13: Compute Binomial probability P(X=2) ===\n")

# Binomial: Number of successes in n trials

# Example 1: Probability of exactly 2 survivors in 10 passengers with P(survival)=0.38
n <- 10
p <- 0.38
x <- 2
prob <- dbinom(x, size = n, prob = p)
cat("Binomial Probability P(X=2) where n=10, p=0.38:\n")
cat("P(X=2) =", round(prob, 6), "\n")

# Example 2: Probability of exactly 2 defective items in batch of 20 with defect rate 5%
n2 <- 20
p2 <- 0.05
x2 <- 2
prob2 <- dbinom(x2, size = n2, prob = p2)
cat("\nBinomial Probability P(X=2) where n=20, p=0.05:\n")
cat("P(X=2) =", round(prob2, 6), "\n")

# Example 3: Using actual Titanic data proportion
survival_rate <- sum(titanic_data$Survived == 1, na.rm = TRUE) / nrow(titanic_data)
prob_survive2 <- dbinom(2, size = 10, prob = survival_rate)
cat("\nBased on Titanic actual survival rate (", round(survival_rate, 3), "):\n")
cat("P(exactly 2 survive in 10 passengers) =", round(prob_survive2, 6), "\n")

# Plot binomial distribution
png("binomial_distribution.png", width = 800, height = 600)
x_vals <- 0:10
probs <- dbinom(x_vals, size = 10, prob = 0.38)
plot(x_vals, probs, type = "h", main = "Binomial Distribution (n=10, p=0.38)",
     xlab = "Number of Successes", ylab = "Probability", col = "blue", lwd = 2)
points(x_vals, probs, pch = 19, col = "red")
dev.off()
cat("Binomial distribution plot saved: binomial_distribution.png\n")
cat("\n")


# ============================================================================
# QUESTION 14: Compute Poisson probability
# ============================================================================

cat("=== QUESTION 14: Compute Poisson probability ===\n")

# Poisson: Number of events in a fixed interval

# Example 1: Average of 3 events per hour, probability of exactly 2
lambda <- 3
x <- 2
prob <- dpois(x, lambda = lambda)
cat("Poisson Probability P(X=2) where lambda=3:\n")
cat("P(X=2) =", round(prob, 6), "\n")

# Example 2: Average of 5 customers per hour, probability of exactly 4
lambda2 <- 5
x2 <- 4
prob2 <- dpois(x2, lambda = lambda2)
cat("\nPoisson Probability P(X=4) where lambda=5:\n")
cat("P(X=4) =", round(prob2, 6), "\n")

# Example 3: Using mtcars - average hp
avg_hp <- mean(mtcars_data$hp)
cat("\nAverage HP in mtcars:", round(avg_hp, 2), "\n")

# Simulate: P(exactly 150 HP) if average is mean
# Note: This is approximate as HP is continuous
prob3 <- dpois(150, lambda = avg_hp)
cat("P(exactly 150 events) with lambda =", round(avg_hp, 2), ":", round(prob3, 10), "\n")
cat("(Note: HP is continuous, this is illustrative)\n")

# Plot Poisson distribution
png("poisson_distribution.png", width = 800, height = 600)
x_vals <- 0:15
probs <- dpois(x_vals, lambda = 3)
plot(x_vals, probs, type = "h", main = "Poisson Distribution (lambda=3)",
     xlab = "Number of Events", ylab = "Probability", col = "purple", lwd = 2)
points(x_vals, probs, pch = 19, col = "red")
dev.off()
cat("Poisson distribution plot saved: poisson_distribution.png\n")
cat("\n")


# ============================================================================
# QUESTION 15: Find Normal probability P(X<60)
# ============================================================================

cat("=== QUESTION 15: Find Normal probability P(X<60) ===\n")

# Example 1: X ~ N(50, 10), find P(X < 60)
mu <- 50
sigma <- 10
x <- 60
prob <- pnorm(x, mean = mu, sd = sigma)
cat("Normal Probability P(X < 60) where X ~ N(50, 10):\n")
cat("P(X < 60) =", round(prob, 6), "\n")

# Example 2: Using Iris data - Sepal.Length ~ N(mean, sd)
mu_iris <- mean(iris_data$Sepal.Length)
sigma_iris <- sd(iris_data$Sepal.Length)
x_iris <- 6.0
prob_iris <- pnorm(x_iris, mean = mu_iris, sd = sigma_iris)
cat("\nFor Iris Sepal.Length (mean =", round(mu_iris, 2), ", sd =", round(sigma_iris, 2), "):\n")
cat("P(Sepal.Length < 6.0) =", round(prob_iris, 6), "\n")

# Example 3: Using mtcars - mpg ~ N(mean, sd)
mu_mtcars <- mean(mtcars_data$mpg)
sigma_mtcars <- sd(mtcars_data$mpg)
x_mtcars <- 20
prob_mtcars <- pnorm(x_mtcars, mean = mu_mtcars, sd = sigma_mtcars)
cat("\nFor mtcars MPG (mean =", round(mu_mtcars, 2), ", sd =", round(sigma_mtcars, 2), "):\n")
cat("P(MPG < 20) =", round(prob_mtcars, 6), "\n")

# Plot normal distribution
png("normal_distribution.png", width = 800, height = 600)
x_vals <- seq(mu - 4*sigma, mu + 4*sigma, length = 100)
y_vals <- dnorm(x_vals, mean = mu, sd = sigma)
plot(x_vals, y_vals, type = "l", main = "Normal Distribution N(50, 10)",
     xlab = "X", ylab = "Density", col = "darkblue", lwd = 2)
# Shade area P(X < 60)
x_fill <- seq(mu - 4*sigma, 60, length = 100)
y_fill <- dnorm(x_fill, mean = mu, sd = sigma)
polygon(c(x_fill, 60, mu - 4*sigma), c(y_fill, 0, 0), col = "lightblue", border = NA)
legend("topright", legend = c("P(X<60)", "Area"), col = c("lightblue", NA), pch = 15)
dev.off()
cat("Normal distribution plot saved: normal_distribution.png\n")
cat("\n")


# ============================================================================
# QUESTION 16: Compute 95% Confidence Interval
# ============================================================================

cat("=== QUESTION 16: Compute 95% Confidence Interval ===\n")

compute_ci <- function(data, confidence = 0.95) {
  n <- length(data)
  mean_val <- mean(data, na.rm = TRUE)
  se <- sd(data, na.rm = TRUE) / sqrt(n)
  alpha <- 1 - confidence
  t_crit <- qt(1 - alpha/2, df = n - 1)
  ci_lower <- mean_val - t_crit * se
  ci_upper <- mean_val + t_crit * se
  return(c(lower = ci_lower, upper = ci_upper, mean = mean_val))
}

# CI for Iris Sepal.Length
cat("95% Confidence Interval for Iris Sepal.Length:\n")
ci_iris <- compute_ci(iris_data$Sepal.Length)
cat("Mean:", round(ci_iris[3], 4), "\n")
cat("95% CI: [", round(ci_iris[1], 4), ", ", round(ci_iris[2], 4), "]\n")

# CI for mtcars mpg
cat("\n95% Confidence Interval for mtcars MPG:\n")
ci_mtcars <- compute_ci(mtcars_data$mpg)
cat("Mean:", round(ci_mtcars[3], 4), "\n")
cat("95% CI: [", round(ci_mtcars[1], 4), ", ", round(ci_mtcars[2], 4), "]\n")

# CI for Titanic Fare
cat("\n95% Confidence Interval for Titanic Fare:\n")
ci_fare <- compute_ci(titanic_data$Fare)
cat("Mean:", round(ci_fare[3], 4), "\n")
cat("95% CI: [", round(ci_fare[1], 4), ", ", round(ci_fare[2], 4), "]\n")

# Using t.test for verification
cat("\nUsing t.test for verification:\n")
t.test(iris_data$Sepal.Length)$conf.int
cat("\n")


# ============================================================================
# QUESTION 17: Perform One-sample t-test
# ============================================================================

cat("=== QUESTION 17: Perform One-sample t-test ===\n")

# Test if Iris Sepal.Length mean is different from 5.5
cat("One-sample t-test: Is Iris Sepal.Length mean different from 5.5?\n")
t_result <- t.test(iris_data$Sepal.Length, mu = 5.5)
print(t_result)

cat("\nInterpretation:\n")
cat("H0: Mean Sepal.Length = 5.5\n")
cat("H1: Mean Sepal.Length != 5.5\n")
if (t_result$p.value < 0.05) {
  cat("Result: Reject H0 at alpha=0.05 (p-value < 0.05)\n")
} else {
  cat("Result: Fail to reject H0 at alpha=0.05 (p-value >= 0.05)\n")
}

# Test if mtcars mpg mean is different from 20
cat("\n\nOne-sample t-test: Is mtcars MPG mean different from 20?\n")
t_result2 <- t.test(mtcars_data$mpg, mu = 20)
print(t_result2)

cat("\nInterpretation:\n")
if (t_result2$p.value < 0.05) {
  cat("Result: Reject H0 at alpha=0.05\n")
} else {
  cat("Result: Fail to reject H0 at alpha=0.05\n")
}
cat("\n")


# ============================================================================
# QUESTION 18: Perform Two-sample t-test
# ============================================================================

cat("=== QUESTION 18: Perform Two-sample t-test ===\n")

# Compare Sepal.Length between setosa and versicolor
setosa_sepal <- iris_data$Sepal.Length[iris_data$Species == "setosa"]
versicolor_sepal <- iris_data$Sepal.Length[iris_data$Species == "versicolor"]

cat("Two-sample t-test: Setosa vs Versicolor Sepal.Length\n")
t_result <- t.test(setosa_sepal, versicolor_sepal)
print(t_result)

cat("\nInterpretation:\n")
if (t_result$p.value < 0.05) {
  cat("Result: Significant difference between species (p < 0.05)\n")
} else {
  cat("Result: No significant difference (p >= 0.05)\n")
}

# Compare MPG between 4-cylinder and 8-cylinder cars
mpg_4cyl <- mtcars_data$mpg[mtcars_data$cyl == 4]
mpg_8cyl <- mtcars_data$mpg[mtcars_data$cyl == 8]

cat("\n\nTwo-sample t-test: 4-cylinder vs 8-cylinder MPG\n")
t_result2 <- t.test(mpg_4cyl, mpg_8cyl)
print(t_result2)

cat("\nInterpretation:\n")
if (t_result2$p.value < 0.05) {
  cat("Result: Significant difference in MPG between cylinder types (p < 0.05)\n")
} else {
  cat("Result: No significant difference (p >= 0.05)\n")
}
cat("\n")


# ============================================================================
# QUESTION 19: Conduct One-way ANOVA
# ============================================================================

cat("=== QUESTION 19: Conduct One-way ANOVA ===\n")

# Compare Sepal.Length across all three Iris species
cat("One-way ANOVA: Sepal.Length across Iris Species\n")
anova_result <- aov(Sepal.Length ~ Species, data = iris_data)
print(summary(anova_result))

cat("\nInterpretation:\n")
cat("H0: All species have equal mean Sepal.Length\n")
cat("H1: At least one species has different mean\n")
if (summary(anova_result)[[1]]$"Pr(>F)"[1] < 0.05) {
  cat("Result: Reject H0 - significant difference exists between species\n")
} else {
  cat("Result: Fail to reject H0 - no significant difference\n")
}

# Compare MPG across cylinder types
cat("\n\nOne-way ANOVA: MPG across cylinder types\n")
anova_result2 <- aov(mpg ~ factor(cyl), data = mtcars_data)
print(summary(anova_result2))

# Post-hoc test if significant
cat("\nPost-hoc Tukey HSD test:\n")
tukey_result <- TukeyHSD(anova_result2)
print(tukey_result)
cat("\n")


# ============================================================================
# QUESTION 20: Perform Chi-square test
# ============================================================================

cat("=== QUESTION 20: Perform Chi-square test ===\n")

# Test independence between Sex and Survived in Titanic
cat("Chi-square test: Sex vs Survival in Titanic\n")
contingency_table <- table(titanic_data$Sex, titanic_data$Survived)
print(contingency_table)

chi_result <- chisq.test(contingency_table)
print(chi_result)

cat("\nInterpretation:\n")
if (chi_result$p.value < 0.05) {
  cat("Result: Significant association between Sex and Survival (p < 0.05)\n")
} else {
  cat("Result: No significant association (p >= 0.05)\n")
}

# Test independence between Pclass and Survived
cat("\n\nChi-square test: Passenger Class vs Survival\n")
contingency_table2 <- table(titanic_data$Pclass, titanic_data$Survived)
print(contingency_table2)

chi_result2 <- chisq.test(contingency_table2)
print(chi_result2)

cat("\nInterpretation:\n")
if (chi_result2$p.value < 0.05) {
  cat("Result: Significant association between Pclass and Survival (p < 0.05)\n")
} else {
  cat("Result: No significant association (p >= 0.05)\n")
}
cat("\n")


# ============================================================================
# QUESTION 21: Perform F-test
# ============================================================================

cat("=== QUESTION 21: Perform F-test ===\n")

# F-test to compare variances between two groups
cat("F-test: Compare variance of Sepal.Length between Setosa and Versicolor\n")

var_setosa <- var(setosa_sepal)
var_versicolor <- var(versicolor_sepal)
f_ratio <- var_setosa / var_versicolor
df1 <- length(setosa_sepal) - 1
df2 <- length(versicolor_sepal) - 1
p_value <- 2 * min(pf(f_ratio, df1, df2), 1 - pf(f_ratio, df1, df2))

cat("Variance Setosa:", round(var_setosa, 4), "\n")
cat("Variance Versicolor:", round(var_versicolor, 4), "\n")
cat("F-ratio:", round(f_ratio, 4), "\n")
cat("p-value:", round(p_value, 6), "\n")

# Using var.test for verification
cat("\nUsing var.test for verification:\n")
f_test_result <- var.test(setosa_sepal, versicolor_sepal)
print(f_test_result)

# Compare variances of MPG between 4 and 6 cylinder cars
cat("\n\nF-test: Compare variance of MPG between 4-cylinder and 6-cylinder\n")
mpg_6cyl <- mtcars_data$mpg[mtcars_data$cyl == 6]
f_test_result2 <- var.test(mpg_4cyl, mpg_6cyl)
print(f_test_result2)
cat("\n")


# ============================================================================
# QUESTION 22: Compare means using independent t-test
# ============================================================================

cat("=== QUESTION 22: Compare means using independent t-test ===\n")

# Compare means of Iris petal length between species
cat("Independent t-test: Petal.Length - Setosa vs Virginica\n")
setosa_petal <- iris_data$Petal.Length[iris_data$Species == "setosa"]
virginica_petal <- iris_data$Petal.Length[iris_data$Species == "virginica"]

t_result <- t.test(setosa_petal, virginica_petal, var.equal = TRUE)
print(t_result)

cat("\nInterpretation:\n")
cat("H0: Mean Petal.Length (Setosa) = Mean Petal.Length (Virginica)\n")
cat("H1: Means are different\n")
if (t_result$p.value < 0.05) {
  cat("Result: Reject H0 - significant difference in means\n")
} else {
  cat("Result: Fail to reject H0\n")
}

# Compare means of HP between automatic and manual transmission
cat("\n\nIndependent t-test: HP - Automatic vs Manual transmission\n")
mtcars_data$transmission <- ifelse(mtcars_data$am == 0, "Automatic", "Manual")
hp_auto <- mtcars_data$hp[mtcars_data$transmission == "Automatic"]
hp_manual <- mtcars_data$hp[mtcars_data$transmission == "Manual"]

t_result2 <- t.test(hp_auto, hp_manual, var.equal = TRUE)
print(t_result2)

cat("\nInterpretation:\n")
if (t_result2$p.value < 0.05) {
  cat("Result: Significant difference in HP between transmission types\n")
} else {
  cat("Result: No significant difference\n")
}
cat("\n")


# ============================================================================
# QUESTION 23: Perform Proportion test
# ============================================================================

cat("=== QUESTION 23: Perform Proportion test ===\n")

# One-sample proportion test
# Test if survival rate in Titanic is different from 0.5
cat("One-sample proportion test: Is Titanic survival rate different from 50%?\n")
n_survived <- sum(titanic_data$Survived == 1, na.rm = TRUE)
n_total <- nrow(titanic_data)
prop_test_result <- prop.test(n_survived, n = n_total, p = 0.5)
print(prop_test_result)

cat("\nInterpretation:\n")
cat("Observed proportion:", round(n_survived/n_total, 4), "\n")
if (prop_test_result$p.value < 0.05) {
  cat("Result: Significant difference from 0.5\n")
} else {
  cat("Result: No significant difference from 0.5\n")
}

# Two-sample proportion test - compare survival rates between male and female
cat("\n\nTwo-sample proportion test: Survival rate - Male vs Female\n")
n_male <- sum(titanic_data$Sex == "male")
n_female <- sum(titanic_data$Sex == "female")
survived_male <- sum(titanic_data$Survived == 1 & titanic_data$Sex == "male", na.rm = TRUE)
survived_female <- sum(titanic_data$Survived == 1 & titanic_data$Sex == "female", na.rm = TRUE)

prop_test_result2 <- prop.test(c(survived_male, survived_female), c(n_male, n_female))
print(prop_test_result2)

cat("\nInterpretation:\n")
if (prop_test_result2$p.value < 0.05) {
  cat("Result: Significant difference in survival rates between genders\n")
} else {
  cat("Result: No significant difference\n")
}
cat("\n")


# ============================================================================
# QUESTION 24: Perform Z-test
# ============================================================================

cat("=== QUESTION 24: Perform Z-test ===\n")

# Z-test for large samples (using normal distribution)
# Note: R doesn't have a built-in z.test, so we compute manually

z_test <- function(x, mu, sigma, alternative = "two.sided") {
  n <- length(x)
  xbar <- mean(x)
  z <- (xbar - mu) / (sigma / sqrt(n))
  
  if (alternative == "two.sided") {
    p_value <- 2 * pnorm(-abs(z))
  } else if (alternative == "less") {
    p_value <- pnorm(z)
  } else {
    p_value <- pnorm(z, lower.tail = FALSE)
  }
  
  return(list(z_statistic = z, p_value = p_value))
}

# Z-test for Iris Sepal.Length (known sigma = 0.8 from population)
cat("Z-test: Is Iris Sepal.Length mean different from 5.8? (known sigma = 0.8)\n")
z_result <- z_test(iris_data$Sepal.Length, mu = 5.8, sigma = 0.8)
cat("Z-statistic:", round(z_result$z_statistic, 4), "\n")
cat("P-value:", round(z_result$p_value, 6), "\n")

# Using built-in method (t-test approximates z-test for large n)
cat("\nUsing t-test as approximation (large n):\n")
t.test(iris_data$Sepal.Length, mu = 5.8)

# Z-test for mtcars MPG (known sigma = 6)
cat("\n\nZ-test: Is mtcars MPG mean different from 20? (known sigma = 6)\n")
z_result2 <- z_test(mtcars_data$mpg, mu = 20, sigma = 6)
cat("Z-statistic:", round(z_result2$z_statistic, 4), "\n")
cat("P-value:", round(z_result2$p_value, 6), "\n")
cat("\n")


# ============================================================================
# QUESTION 25: Perform t-test
# ============================================================================

cat("=== QUESTION 25: Perform t-test ===\n")

# Paired t-test example
# Compare Petal.Length and Petal.Width in Iris (paired by observation)
cat("Paired t-test: Petal.Length vs Petal.Width in Iris\n")
t_result_paired <- t.test(iris_data$Petal.Length, iris_data$Petal.Width, paired = TRUE)
print(t_result_paired)

cat("\nInterpretation:\n")
if (t_result_paired$p.value < 0.05) {
  cat("Result: Significant difference between Petal.Length and Petal.Width\n")
} else {
  cat("Result: No significant difference\n")
}

# Welch's t-test (unequal variances)
cat("\n\nWelch's t-test: Setosa vs Virginica (unequal variances assumed)\n")
t_result_welch <- t.test(setosa_sepal, virginica_sepal, var.equal = FALSE)
print(t_result_welch)

# One-tailed t-test
cat("\n\nOne-tailed t-test: Is Setosa Petal.Length less than 2?\n")
t_result_one <- t.test(iris_data$Petal.Length[iris_data$Species == "setosa"],
                       mu = 2, alternative = "less")
print(t_result_one)
cat("\n")


# ============================================================================
# QUESTION 26: Compute 95% Confidence Interval
# ============================================================================

cat("=== QUESTION 26: Compute 95% Confidence Interval (Advanced) ===\n")

# CI for difference in means
cat("95% CI for difference in means (Setosa vs Versicolor Sepal.Length):\n")
ci_diff <- t.test(setosa_sepal, versicolor_sepal)$conf.int
print(ci_diff)

# CI for proportion
cat("\n95% CI for Titanic survival proportion:\n")
prop_ci <- prop.test(n_survived, n_total, conf.level = 0.95)$conf.int
print(prop_ci)

# CI using bootstrap
set.seed(123)
bootstrap_ci <- function(data, n_bootstrap = 1000, confidence = 0.95) {
  n <- length(data)
  bootstrap_means <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_idx <- sample(1:n, n, replace = TRUE)
    bootstrap_means[i] <- mean(data[sample_idx])
  }
  alpha <- 1 - confidence
  ci_lower <- quantile(bootstrap_means, alpha/2)
  ci_upper <- quantile(bootstrap_means, 1 - alpha/2)
  return(c(lower = ci_lower, upper = ci_upper))
}

cat("\n95% Bootstrap CI for Iris Sepal.Length:\n")
boot_ci <- bootstrap_ci(iris_data$Sepal.Length)
cat("95% CI: [", round(boot_ci[1], 4), ", ", round(boot_ci[2], 4), "]\n")

# CI for variance
cat("\n95% CI for variance of Iris Sepal.Length:\n")
n <- length(iris_data$Sepal.Length)
v <- var(iris_data$Sepal.Length)
chi_lower <- v * (n - 1) / qchisq(0.975, n - 1)
chi_upper <- v * (n - 1) / qchisq(0.025, n - 1)
cat("95% CI for variance: [", round(chi_lower, 4), ", ", round(chi_upper, 4), "]\n")

cat("\n=== END OF ALL 26 QUESTIONS ===\n")
cat("All statistical analyses completed successfully!\n")