# ============================================================================
# QUESTION 5: Handle missing values
# ============================================================================

if (!require("dplyr")) install.packages("dplyr")
if (!require("zoo")) install.packages("zoo")
library(dplyr)
library(zoo)

# Create sample data with missing values
cat("=== Create sample data with missing values ===\n")
set.seed(42)
sample_data <- data.frame(
  ID = 1:20,
  Score = c(85, 90, NA, 78, 92, NA, 88, 95, NA, 80,
            72, 89, NA, 91, 85, 87, NA, 93, 79, 88),
  Age = c(25, NA, 30, 35, NA, 40, 45, NA, 50, 55,
          28, 33, 38, NA, 48, 52, 42, NA, 37, 44),
  Income = c(50000, 55000, NA, 60000, 65000, NA, 55000, 70000,
             NA, 45000, 52000, NA, 68000, 58000, 62000, NA,
             49000, 72000, NA, 56000)
)
cat("Original data with missing values:\n")
print(sample_data)

# ============================================================================
# Method 1: Listwise deletion (remove rows with any NA)
# ============================================================================
cat("\n=== Method 1: Listwise Deletion (na.omit) ===\n")
clean_data <- na.omit(sample_data)
cat("Data after removing rows with NA:\n")
print(clean_data)
cat("Rows removed:", nrow(sample_data) - nrow(clean_data), "\n")

# ============================================================================
# Method 2: Mean imputation
# ============================================================================
cat("\n=== Method 2: Mean Imputation ===\n")
sample_data_mean <- sample_data
sample_data_mean$Score <- ifelse(is.na(sample_data_mean$Score),
                                  mean(sample_data_mean$Score, na.rm = TRUE),
                                  sample_data_mean$Score)
cat("Score after mean imputation:\n")
print(sample_data_mean$Score)

# ============================================================================
# Method 3: Median imputation
# ============================================================================
cat("\n=== Method 3: Median Imputation ===\n")
sample_data_median <- sample_data
sample_data_median$Age <- ifelse(is.na(sample_data_median$Age),
                                  median(sample_data_median$Age, na.rm = TRUE),
                                  sample_data_median$Age)
cat("Age after median imputation:\n")
print(sample_data_median$Age)

# ============================================================================
# Method 4: Mode imputation (for categorical-like data)
# ============================================================================
cat("\n=== Method 4: Mode Imputation ===\n")
get_mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}
sample_data_mode <- sample_data
# Create a categorical column for demonstration
sample_data_mode$ScoreCategory <- ifelse(sample_data$Score > 85, "High", "Low")
sample_data_mode$ScoreCategory[5] <- NA
mode_val <- get_mode(sample_data_mode$ScoreCategory)
sample_data_mode$ScoreCategory <- ifelse(is.na(sample_data_mode$ScoreCategory),
                                          mode_val,
                                          sample_data_mode$ScoreCategory)
cat("Mode imputed categorical variable:\n")
print(table(sample_data_mode$ScoreCategory))

# ============================================================================
# Method 5: Forward fill (Last Observation Carried Forward)
# ============================================================================
cat("\n=== Method 5: Forward Fill (using zoo) ===\n")
sample_data_filled <- sample_data
sample_data_filled$Score <- na.locf(sample_data_filled$Score)
sample_data_filled$Age <- na.locf(sample_data_filled$Age)
cat("After forward fill:\n")
print(sample_data_filled)

# ============================================================================
# Method 6: Backward fill
# ============================================================================
cat("\n=== Method 6: Backward Fill ===\n")
sample_data_backfilled <- sample_data
sample_data_backfilled$Score <- na.locf(sample_data_backfilled$Score, fromLast = TRUE)
sample_data_backfilled$Age <- na.locf(sample_data_backfilled$Age, fromLast = TRUE)
cat("After backward fill:\n")
print(sample_data_backfilled)

# ============================================================================
# Real dataset example with Titanic
# ============================================================================
cat("\n=== Real Example: Handle missing values in Titanic ===\n")
titanic_url <- "https://raw.githubusercontent.com/datasets/titanic/master/train.csv"
titanic_data <- tryCatch({
  read.csv(titanic_url)
}, error = function(e) {
  set.seed(42)
  data.frame(
    PassengerId = 1:100,
    Survived = sample(c(0,1), 100, replace = TRUE),
    Pclass = sample(1:3, 100, replace = TRUE),
    Sex = sample(c("male","female"), 100, replace = TRUE),
    Age = c(rep(NA, 20), runif(80, 1, 70)),
    Fare = runif(100, 5, 500)
  )
})

cat("Missing values before处理:\n")
print(colSums(is.na(titanic_data)))

# Impute Age with median
titanic_data$Age[is.na(titanic_data$Age)] <- median(titanic_data$Age, na.rm = TRUE)

# Impute Fare with mean
titanic_data$Fare[is.na(titanic_data$Fare)] <- mean(titanic_data$Fare, na.rm = TRUE)

cat("\nMissing values after imputation:\n")
print(colSums(is.na(titanic_data)))

cat("\n=== Question 5 Complete ===\n")