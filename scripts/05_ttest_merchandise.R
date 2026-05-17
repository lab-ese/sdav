# =============================================================================
# Script 05: One-Sample t-Test
# -----------------------------------------------------------------------------
# Generic: works on ANY CSV. Auto-detects numeric columns.
# Tests whether the sample mean of a numeric column differs from a population
# mean (mu). If mu is not set, uses the column's theoretical midpoint.
# Change CSV_PATH and mu to suit your data.
# =============================================================================

CSV_PATH <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)),
                      "data", "merchandise_sales.csv")

# ── Configuration (change these if needed) ────────────────────────────────────
# mu = hypothesised population mean (set to NULL for auto)
# col_index = which numeric column to test (1 = first, 2 = second, ...)
MU        <- NULL
COL_INDEX <- 1
ALPHA     <- 0.05   # significance level

# ── Generic helpers ────────────────────────────────────────────────────────────
load_csv <- function(path) {
  if (!file.exists(path)) stop(paste("File not found:", path))
  df <- read.csv(path, stringsAsFactors = FALSE)
  cat(sprintf("Loaded '%s'  |  %d rows  x  %d columns\n\n",
              basename(path), nrow(df), ncol(df)))
  df
}
get_numeric_cols <- function(df) names(df)[sapply(df, is.numeric)]

# ── 1. Load ───────────────────────────────────────────────────────────────────
df       <- load_csv(CSV_PATH)
num_cols <- get_numeric_cols(df)

cat(sprintf("Numeric columns: %s\n", paste(num_cols, collapse = ", ")))

target_col <- num_cols[min(COL_INDEX, length(num_cols))]
vals       <- na.omit(df[[target_col]])

cat(sprintf("Testing column : '%s'  (n = %d)\n\n", target_col, length(vals)))

# ── 2. Set mu ─────────────────────────────────────────────────────────────────
if (is.null(MU)) {
  # Use overall mean rounded to nearest 10 as a "population benchmark"
  MU <- round(mean(vals) * 0.9, -1)
  cat(sprintf("mu not specified — using auto-generated mu = %.2f\n", MU))
  cat("(Set MU manually at the top of the script for a real test.)\n\n")
}

# ── 3. Descriptive stats ──────────────────────────────────────────────────────
cat("=== Descriptive Statistics ===\n")
cat(sprintf("  n          : %d\n",      length(vals)))
cat(sprintf("  Sample Mean: %.4f\n",    mean(vals)))
cat(sprintf("  Std Dev    : %.4f\n",    sd(vals)))
cat(sprintf("  Std Error  : %.4f\n",    sd(vals) / sqrt(length(vals))))
cat(sprintf("  Hypothesised mu: %.4f\n", MU))

# ── 4. One-sample t-test ──────────────────────────────────────────────────────
cat(sprintf("\n=== One-Sample t-Test: H0: mu = %.4f ===\n", MU))
test_result <- t.test(vals, mu = MU, alternative = "two.sided", conf.level = 1 - ALPHA)
print(test_result)

# ── 5. Interpretation ─────────────────────────────────────────────────────────
cat("\n=== Interpretation ===\n")
cat(sprintf("  t-statistic : %.4f\n",  test_result$statistic))
cat(sprintf("  df          : %.0f\n",  test_result$parameter))
cat(sprintf("  p-value     : %.6f\n",  test_result$p.value))
cat(sprintf("  Alpha       : %.2f\n",  ALPHA))
cat(sprintf("  95%% CI      : [%.4f, %.4f]\n",
            test_result$conf.int[1], test_result$conf.int[2]))

if (test_result$p.value < ALPHA) {
  cat(sprintf("\nConclusion: Reject H0. The sample mean (%.4f) differs significantly\n", mean(vals)))
  cat(sprintf("from the hypothesised population mean (%.4f) at alpha = %.2f.\n", MU, ALPHA))
} else {
  cat(sprintf("\nConclusion: Fail to reject H0. Insufficient evidence that the sample mean\n"))
  cat(sprintf("(%.4f) differs from the hypothesised mean (%.4f) at alpha = %.2f.\n", mean(vals), MU, ALPHA))
}

# ── 6. Visualisation ──────────────────────────────────────────────────────────
ci <- test_result$conf.int
hist(vals, col = "lightblue", border = "white",
     main = paste("One-Sample t-Test:", target_col),
     xlab = target_col, las = 1)
abline(v = mean(vals), col = "blue",  lwd = 2, lty = 1)
abline(v = MU,         col = "red",   lwd = 2, lty = 2)
abline(v = ci[1],      col = "darkgreen", lwd = 1.5, lty = 3)
abline(v = ci[2],      col = "darkgreen", lwd = 1.5, lty = 3)
legend("topright",
       legend = c(sprintf("Sample Mean = %.2f", mean(vals)),
                  sprintf("mu (H0) = %.2f", MU),
                  "95% CI bounds"),
       col    = c("blue", "red", "darkgreen"),
       lwd    = 2, lty = c(1, 2, 3), bty = "n")
