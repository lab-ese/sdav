# =============================================================================
# Script 03: Read Any CSV — Structure, Summary, SD / Variance / CV
# -----------------------------------------------------------------------------
# Generic: works on ANY CSV. Auto-detects numeric columns.
# Change CSV_PATH to point at your file.
# =============================================================================

CSV_PATH <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)),
                      "data", "ecommerce.csv")

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

# Coefficient of Variation (%)
cv <- function(x) {
  x <- na.omit(x)
  if (mean(x) == 0) return(NA)
  (sd(x) / abs(mean(x))) * 100
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

# ── 3. SD, Variance, and CV for every numeric column ─────────────────────────
cat("\n=== Dispersion Statistics for All Numeric Columns ===\n")
cat(sprintf("%-22s %14s %16s %12s\n", "Column", "Std Deviation", "Variance", "CV (%)"))
cat(strrep("-", 68), "\n")

for (col in num_cols) {
  vals <- na.omit(df[[col]])
  cat(sprintf("%-22s %14.4f %16.4f %12.2f\n",
              col, sd(vals), var(vals), cv(vals)))
}

# ── 4. Focused deep-dive on the primary numeric column ───────────────────────
primary <- num_cols[1]
cat(sprintf("\n=== Deep-Dive: '%s' ===\n", primary))
vals <- na.omit(df[[primary]])

cat(sprintf("  n              : %d\n",      length(vals)))
cat(sprintf("  Mean           : %.4f\n",    mean(vals)))
cat(sprintf("  Std Deviation  : %.4f\n",    sd(vals)))
cat(sprintf("  Variance       : %.4f\n",    var(vals)))
cat(sprintf("  CV             : %.2f%%\n",  cv(vals)))
cat(sprintf("  Min / Max      : %g / %g\n", min(vals), max(vals)))
cat(sprintf("  IQR            : %.4f\n",    IQR(vals)))

# Interpretation
cat(sprintf("\nInterpretation: CV of %.1f%% indicates %s dispersion relative to the mean.\n",
            cv(vals),
            ifelse(cv(vals) < 15, "low",
                   ifelse(cv(vals) < 35, "moderate", "high"))))
