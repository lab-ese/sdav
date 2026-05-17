# =============================================================================
# Script 04: Histogram | Box-and-Whisker Plot | Stem-and-Leaf Display
# -----------------------------------------------------------------------------
# Generic: works on ANY CSV. Auto-detects numeric columns for plots.
# Change CSV_PATH to point at your file.
# =============================================================================

CSV_PATH <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)),
                      "data", "merchandise_sales.csv")

# ── Generic helpers ────────────────────────────────────────────────────────────
load_csv <- function(path) {
  if (!file.exists(path)) stop(paste("File not found:", path))
  df <- read.csv(path, stringsAsFactors = FALSE)
  cat(sprintf("Loaded '%s'  |  %d rows  x  %d columns\n\n",
              basename(path), nrow(df), ncol(df)))
  df
}
get_numeric_cols <- function(df) names(df)[sapply(df, is.numeric)]

# ── 1. Load data ──────────────────────────────────────────────────────────────
df      <- load_csv(CSV_PATH)
num_cols <- get_numeric_cols(df)

cat(sprintf("Numeric columns available: %s\n\n", paste(num_cols, collapse = ", ")))

# Use first numeric column as the target; change index to pick another
target_col <- num_cols[1]
cat(sprintf("Plotting column: '%s'  (change 'target_col' to use a different column)\n\n", target_col))

vals <- na.omit(df[[target_col]])

# ── 2. Histogram ──────────────────────────────────────────────────────────────
cat("=== Histogram ===\n")

# Sturges rule for bin count (works for any n)
n_bins <- ceiling(1 + log2(length(vals)))

hist(vals,
     breaks  = n_bins,
     col     = "steelblue",
     border  = "white",
     main    = paste("Histogram of", target_col),
     xlab    = target_col,
     ylab    = "Frequency",
     las     = 1)

abline(v    = mean(vals),   col = "red",    lwd = 2, lty = 2)
abline(v    = median(vals), col = "green3", lwd = 2, lty = 3)
legend("topright",
       legend = c(sprintf("Mean = %.2f", mean(vals)),
                  sprintf("Median = %.2f", median(vals))),
       col    = c("red", "green3"),
       lwd    = 2, lty = c(2, 3), bty = "n")

cat(sprintf("  Bins used : %d (Sturges rule)\n", n_bins))
cat(sprintf("  Mean      : %.4f\n", mean(vals)))
cat(sprintf("  Median    : %.4f\n", median(vals)))

# ── 3. Box-and-Whisker Plot ───────────────────────────────────────────────────
cat("\n=== Box-and-Whisker Plot ===\n")

boxplot(vals,
        col      = "coral",
        border   = "darkred",
        main     = paste("Box-and-Whisker Plot of", target_col),
        ylab     = target_col,
        notch    = FALSE,
        outline  = TRUE,
        whisklty = 1,
        staplelty= 1)

# Overlay points for small datasets (n < 100)
if (length(vals) < 100) {
  stripchart(vals, method = "jitter", vertical = TRUE, pch = 16,
             col = rgb(0.2, 0.2, 0.8, 0.5), add = TRUE, cex = 0.8)
}

bp_stats <- boxplot.stats(vals)
cat(sprintf("  Lower Whisker : %.4f\n", bp_stats$stats[1]))
cat(sprintf("  Q1 (25%%)      : %.4f\n", bp_stats$stats[2]))
cat(sprintf("  Median (Q2)   : %.4f\n", bp_stats$stats[3]))
cat(sprintf("  Q3 (75%%)      : %.4f\n", bp_stats$stats[4]))
cat(sprintf("  Upper Whisker : %.4f\n", bp_stats$stats[5]))
cat(sprintf("  Outliers      : %s\n",
            if (length(bp_stats$out) == 0) "None"
            else paste(round(bp_stats$out, 2), collapse = ", ")))

# ── 4. Stem-and-Leaf Display ──────────────────────────────────────────────────
cat("\n=== Stem-and-Leaf Display ===\n")
cat(sprintf("  Column: %s\n\n", target_col))

# Scale automatically for very large numbers
scale_factor <- 1
if (max(abs(vals)) > 10000)  scale_factor <- 1000
if (max(abs(vals)) > 100000) scale_factor <- 10000

scaled_vals <- vals / scale_factor

if (scale_factor > 1) {
  cat(sprintf("  (Values divided by %g for display)\n\n", scale_factor))
}

stem(scaled_vals)

cat(sprintf("\nNote: Each leaf represents one observation. Scale factor = %g\n", scale_factor))
