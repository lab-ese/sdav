# =============================================================================
# Script 08: Chi-Square Distribution — Probability Calculations
# -----------------------------------------------------------------------------
# Generic: works on ANY CSV. Auto-selects a numeric column and derives
# degrees of freedom. Computes chi-square probabilities and critical values.
# Change CSV_PATH as needed.
# =============================================================================

CSV_PATH <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)),
                      "data", "airline_delays.csv")

# ── Configuration ──────────────────────────────────────────────────────────────
# DF_OVERRIDE: set to an integer to force degrees of freedom, or NULL for auto
DF_OVERRIDE <- NULL
ALPHA       <- 0.05

# ── Generic helpers ────────────────────────────────────────────────────────────
load_csv <- function(path) {
  if (!file.exists(path)) stop(paste("File not found:", path))
  df <- read.csv(path, stringsAsFactors = FALSE)
  cat(sprintf("Loaded '%s'  |  %d rows  x  %d columns\n\n",
              basename(path), nrow(df), ncol(df)))
  df
}
get_numeric_cols <- function(df) names(df)[sapply(df, is.numeric)]
get_cat_cols     <- function(df) names(df)[sapply(df, function(x) is.character(x) | is.factor(x))]

# ── 1. Load ───────────────────────────────────────────────────────────────────
df <- load_csv(CSV_PATH)

num_cols <- get_numeric_cols(df)
cat_cols <- get_cat_cols(df)

cat(sprintf("Numeric columns  : %s\n", paste(num_cols, collapse = ", ")))
cat(sprintf("Character columns: %s\n\n", paste(cat_cols, collapse = ", ")))

# ── 2. Determine degrees of freedom ──────────────────────────────────────────
if (!is.null(DF_OVERRIDE)) {
  df_chi <- DF_OVERRIDE
  cat(sprintf("Using manually set df = %d\n\n", df_chi))
} else if (length(cat_cols) >= 1) {
  # df = (levels - 1) for the first categorical column
  cat_col <- cat_cols[1]
  n_levels <- length(unique(na.omit(df[[cat_col]])))
  df_chi   <- n_levels - 1
  cat(sprintf("Auto df: categorical column '%s' has %d levels → df = %d\n\n",
              cat_col, n_levels, df_chi))
} else {
  # Fallback: use number of numeric columns - 1
  df_chi <- length(num_cols) - 1
  cat(sprintf("Auto df: using (number of numeric columns - 1) = %d\n\n", df_chi))
}

df_chi <- max(df_chi, 1)   # ensure df >= 1

# ── 3. Chi-Square distribution properties ────────────────────────────────────
cat(sprintf("=== Chi-Square Distribution: df = %d ===\n", df_chi))
cat(sprintf("  Mean (theoretical)     = %d\n",   df_chi))
cat(sprintf("  Variance (theoretical) = %d\n",   2 * df_chi))
cat(sprintf("  Std Dev (theoretical)  = %.4f\n", sqrt(2 * df_chi)))

# ── 4. Critical value and tail probabilities ──────────────────────────────────
critical_val <- qchisq(1 - ALPHA, df = df_chi)
cat(sprintf("\n=== Critical Values (alpha = %.2f) ===\n", ALPHA))
cat(sprintf("  Upper tail critical value  (alpha = %.2f): %.4f\n", ALPHA, critical_val))
cat(sprintf("  Lower tail critical value  (alpha = %.2f): %.4f\n", ALPHA, qchisq(ALPHA, df = df_chi)))
cat(sprintf("  Two-tail critical values   (alpha/2 each): %.4f  and  %.4f\n",
            qchisq(ALPHA / 2, df_chi), qchisq(1 - ALPHA / 2, df_chi)))

# ── 5. Probability table for a range of x values ─────────────────────────────
x_max  <- ceiling(qchisq(0.999, df = df_chi))
x_vals <- seq(0, x_max, by = max(1, round(x_max / 20)))

cat(sprintf("\n=== Chi-Square Probability Table  (df = %d) ===\n", df_chi))
cat(sprintf("  %-10s  %-16s  %-16s  %-16s\n", "x", "f(x) density", "P(X <= x)", "P(X > x)"))
cat(strrep("-", 62), "\n")
for (x in x_vals) {
  cat(sprintf("  %-10.3f  %-16.6f  %-16.6f  %-16.6f\n",
              x, dchisq(x, df_chi), pchisq(x, df_chi), pchisq(x, df_chi, lower.tail = FALSE)))
}

# ── 6. P-value for observed chi-square stat from data ────────────────────────
if (length(num_cols) >= 1) {
  obs_col  <- num_cols[1]
  obs_vals <- na.omit(df[[obs_col]])
  # Compute a test chi-square stat: sum((O - E)^2 / E) using frequency table
  freq_table <- table(cut(obs_vals, breaks = df_chi + 1))
  expected   <- length(obs_vals) / (df_chi + 1)
  observed   <- as.numeric(freq_table)
  chi_stat   <- sum((observed - expected)^2 / expected)
  p_val      <- pchisq(chi_stat, df = df_chi, lower.tail = FALSE)

  cat(sprintf("\n=== Computed Chi-Square Statistic from '%s' ===\n", obs_col))
  cat(sprintf("  Chi-square statistic : %.4f\n", chi_stat))
  cat(sprintf("  Degrees of freedom   : %d\n",   df_chi))
  cat(sprintf("  p-value              : %.6f\n",  p_val))
  cat(sprintf("  Critical value (%.2f): %.4f\n",  ALPHA, critical_val))
  cat(sprintf("  Decision             : %s\n",
              if (chi_stat > critical_val) "Reject H0" else "Fail to reject H0"))
}

# ── 7. Plot chi-square density ────────────────────────────────────────────────
x_plot <- seq(0.01, x_max, length.out = 500)
y_plot <- dchisq(x_plot, df = df_chi)

plot(x_plot, y_plot, type = "l", lwd = 2, col = "steelblue",
     main = sprintf("Chi-Square Distribution (df = %d)", df_chi),
     xlab = "x", ylab = "Density", las = 1)

# Shade critical region
x_tail <- x_plot[x_plot >= critical_val]
y_tail <- dchisq(x_tail, df = df_chi)
polygon(c(critical_val, x_tail, x_max), c(0, y_tail, 0),
        col = rgb(1, 0.2, 0.2, 0.4), border = NA)
abline(v = critical_val, col = "red", lty = 2, lwd = 1.5)
legend("topright",
       legend = c("Chi-Square Density",
                  sprintf("Critical region (alpha = %.2f)", ALPHA)),
       col    = c("steelblue", rgb(1, 0.2, 0.2, 0.6)),
       lwd    = c(2, 8), bty = "n")
