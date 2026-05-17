# =============================================================================
# Script 09: F-Test — Compare Variances of Two Datasets (Any Two CSVs)
# -----------------------------------------------------------------------------
# Generic: works on ANY two CSVs. Auto-detects numeric columns in each.
# Change CSV_PATH_1 and CSV_PATH_2 to your files.
# =============================================================================

CSV_PATH_1 <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)),
                        "data", "merchandise_sales.csv")
CSV_PATH_2 <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)),
                        "data", "ecommerce.csv")

ALPHA <- 0.05

# ── Generic helpers ────────────────────────────────────────────────────────────
load_csv <- function(path) {
  if (!file.exists(path)) stop(paste("File not found:", path))
  df <- read.csv(path, stringsAsFactors = FALSE)
  cat(sprintf("  Loaded '%s'  |  %d rows  x  %d columns\n",
              basename(path), nrow(df), ncol(df)))
  df
}
get_numeric_cols <- function(df) names(df)[sapply(df, is.numeric)]

# Pick the numeric column with highest variance (most "spread" to test)
pick_target_col <- function(df) {
  nc <- get_numeric_cols(df)
  if (length(nc) == 0) stop("No numeric columns found in dataset.")
  vars  <- sapply(nc, function(col) var(na.omit(df[[col]])))
  nc[which.max(vars)]
}

# ── 1. Load both datasets ─────────────────────────────────────────────────────
cat("=== Loading Datasets ===\n")
df1 <- load_csv(CSV_PATH_1)
df2 <- load_csv(CSV_PATH_2)
cat("\n")

# ── 2. Auto-select numeric columns ───────────────────────────────────────────
col1 <- pick_target_col(df1)
col2 <- pick_target_col(df2)

cat(sprintf("Dataset 1 — '%s' | Column selected: '%s'\n", basename(CSV_PATH_1), col1))
cat(sprintf("Dataset 2 — '%s' | Column selected: '%s'\n\n", basename(CSV_PATH_2), col2))
cat("(Auto-selected column with highest variance. Change 'pick_target_col' to override.)\n\n")

x <- na.omit(df1[[col1]])
y <- na.omit(df2[[col2]])

# ── 3. Descriptive stats ──────────────────────────────────────────────────────
cat("=== Descriptive Statistics ===\n")
cat(sprintf("%-30s  %12s  %12s\n", "Statistic", "Dataset 1", "Dataset 2"))
cat(sprintf("%-30s  %12s  %12s\n", strrep("-", 28), strrep("-", 12), strrep("-", 12)))
cat(sprintf("%-30s  %12s  %12s\n",   "File",        basename(CSV_PATH_1), basename(CSV_PATH_2)))
cat(sprintf("%-30s  %12s  %12s\n",   "Column",      col1, col2))
cat(sprintf("%-30s  %12d  %12d\n",   "n",           length(x), length(y)))
cat(sprintf("%-30s  %12.4f  %12.4f\n", "Mean",      mean(x), mean(y)))
cat(sprintf("%-30s  %12.4f  %12.4f\n", "Variance",  var(x), var(y)))
cat(sprintf("%-30s  %12.4f  %12.4f\n", "Std Dev",   sd(x), sd(y)))
cat(sprintf("%-30s  %12.4f  %12.4f\n", "Min",       min(x), min(y)))
cat(sprintf("%-30s  %12.4f  %12.4f\n", "Max",       max(x), max(y)))

# ── 4. F-test ─────────────────────────────────────────────────────────────────
cat(sprintf("\n=== F-Test: H0: var(%s) == var(%s) ===\n", col1, col2))
ftest <- var.test(x, y, ratio = 1, alternative = "two.sided", conf.level = 1 - ALPHA)
print(ftest)

# ── 5. Interpretation ─────────────────────────────────────────────────────────
cat("\n=== Interpretation ===\n")
cat(sprintf("  F-statistic : %.6f\n", ftest$statistic))
cat(sprintf("  df1 / df2   : %d / %d\n", ftest$parameter[1], ftest$parameter[2]))
cat(sprintf("  p-value     : %.6f\n", ftest$p.value))
cat(sprintf("  Alpha       : %.2f\n", ALPHA))
cat(sprintf("  95%% CI for ratio: [%.4f, %.4f]\n", ftest$conf.int[1], ftest$conf.int[2]))

if (ftest$p.value < ALPHA) {
  cat(sprintf("\nConclusion: Reject H0. The variances of '%s' and '%s' differ significantly\n",
              col1, col2))
  cat(sprintf("(p = %.4f < %.2f). The two datasets have unequal variances.\n", ftest$p.value, ALPHA))
} else {
  cat(sprintf("\nConclusion: Fail to reject H0. No significant difference in variances\n"))
  cat(sprintf("(p = %.4f >= %.2f). Variances are not significantly different.\n", ftest$p.value, ALPHA))
}

# ── 6. Visual comparison ──────────────────────────────────────────────────────
par(mfrow = c(1, 2))

hist(x, col = "steelblue", border = "white",
     main = sprintf("%s\n(var = %.2f)", col1, var(x)),
     xlab = col1, las = 1)

hist(y, col = "coral", border = "white",
     main = sprintf("%s\n(var = %.2f)", col2, var(y)),
     xlab = col2, las = 1)

par(mfrow = c(1, 1))
