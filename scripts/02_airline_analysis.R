# =============================================================================
# Script 02: Read Any CSV — Structure, Summary, Mean / Median / Mode
# -----------------------------------------------------------------------------
# Generic: works on ANY CSV. Auto-detects numeric columns.
# Change CSV_PATH to point at your file.
# =============================================================================

CSV_PATH <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)),
                      "data", "airline_delays.csv")

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

mode_val <- function(x) {
  x  <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ── 1. Load and inspect ────────────────────────────────────────────────────────
df <- load_csv(CSV_PATH)

cat("=== str() — Structure ===\n")
str(df)

cat("\n=== summary() — Summary Statistics ===\n")
print(summary(df))

# ── 2. Detect columns ─────────────────────────────────────────────────────────
num_cols <- get_numeric_cols(df)
cat_cols <- get_cat_cols(df)

cat(sprintf("\nNumeric columns  (%d): %s\n", length(num_cols), paste(num_cols, collapse = ", ")))
cat(sprintf("Character columns(%d): %s\n",  length(cat_cols), paste(cat_cols, collapse = ", ")))

# ── 3. Mean, Median, Mode for every numeric column ───────────────────────────
cat("\n=== Descriptive Statistics for All Numeric Columns ===\n")
cat(sprintf("%-22s %12s %12s %12s\n", "Column", "Mean", "Median", "Mode"))
cat(strrep("-", 60), "\n")

for (col in num_cols) {
  vals   <- na.omit(df[[col]])
  m_mean <- round(mean(vals), 3)
  m_med  <- round(median(vals), 3)
  m_mode <- mode_val(vals)
  cat(sprintf("%-22s %12.3f %12.3f %12g\n", col, m_mean, m_med, m_mode))
}

# ── 4. Focused deep-dive on the primary numeric column ───────────────────────
primary <- num_cols[1]
cat(sprintf("\n=== Deep-Dive: '%s' ===\n", primary))
vals <- na.omit(df[[primary]])

cat(sprintf("  Count      : %d\n",      length(vals)))
cat(sprintf("  Mean       : %.4f\n",    mean(vals)))
cat(sprintf("  Median     : %.4f\n",    median(vals)))
cat(sprintf("  Mode       : %g\n",      mode_val(vals)))
cat(sprintf("  Std Dev    : %.4f\n",    sd(vals)))
cat(sprintf("  Variance   : %.4f\n",    var(vals)))
cat(sprintf("  Min        : %g\n",      min(vals)))
cat(sprintf("  Max        : %g\n",      max(vals)))
cat(sprintf("  Range      : %g\n",      diff(range(vals))))
cat(sprintf("  IQR        : %.4f\n",    IQR(vals)))
