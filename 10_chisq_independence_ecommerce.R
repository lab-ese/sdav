# =============================================================================
# Script 10: Chi-Square Test for Independence Between Two Categorical Variables
# -----------------------------------------------------------------------------
# Generic: works on ANY CSV. Auto-detects two categorical columns with
# 2–15 levels each, then runs a chi-square test of independence.
# Change CSV_PATH as needed. Optionally set COL1 and COL2 manually.
# =============================================================================

CSV_PATH <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)),
                      "data", "ecommerce.csv")

# Override: set to column names (strings) or NULL for auto-detection
COL1  <- NULL   # e.g., "Category"
COL2  <- NULL   # e.g., "Returned"
ALPHA <- 0.05

# ── Generic helpers ────────────────────────────────────────────────────────────
load_csv <- function(path) {
  if (!file.exists(path)) stop(paste("File not found:", path))
  df <- read.csv(path, stringsAsFactors = FALSE)
  cat(sprintf("Loaded '%s'  |  %d rows  x  %d columns\n\n",
              basename(path), nrow(df), ncol(df)))
  df
}

# Return categorical columns with between min_lev and max_lev unique values
eligible_cat_cols <- function(df, min_lev = 2, max_lev = 15) {
  candidates <- names(df)[sapply(df, function(x) is.character(x) | is.factor(x))]
  Filter(function(col) {
    n <- length(unique(na.omit(df[[col]])))
    n >= min_lev && n <= max_lev
  }, candidates)
}

# ── 1. Load ───────────────────────────────────────────────────────────────────
df       <- load_csv(CSV_PATH)
cat_cols <- eligible_cat_cols(df)

cat(sprintf("Eligible categorical columns: %s\n\n", paste(cat_cols, collapse = ", ")))

# ── 2. Select the two columns ─────────────────────────────────────────────────
if (is.null(COL1) || is.null(COL2)) {
  if (length(cat_cols) < 2) stop("Need at least 2 categorical columns with 2–15 levels.")
  COL1 <- cat_cols[1]
  COL2 <- cat_cols[2]
  cat(sprintf("Auto-selected columns: '%s' and '%s'\n", COL1, COL2))
  cat("(Set COL1 and COL2 at the top of the script to override.)\n\n")
} else {
  cat(sprintf("Using specified columns: '%s' and '%s'\n\n", COL1, COL2))
}

# ── 3. Contingency table ──────────────────────────────────────────────────────
df_clean  <- na.omit(df[, c(COL1, COL2)])
cont_tab  <- table(df_clean[[COL1]], df_clean[[COL2]])
colnames(cont_tab) <- paste0(COL2, "=", colnames(cont_tab))
rownames(cont_tab) <- paste0(COL1, "=", rownames(cont_tab))

cat("=== Observed Contingency Table ===\n")
print(cont_tab)

# Row and column totals
cat("\n=== With Marginal Totals ===\n")
print(addmargins(cont_tab))

# Row percentages
cat("\n=== Row Proportions ===\n")
print(round(prop.table(cont_tab, margin = 1) * 100, 2))

# ── 4. Chi-square test ────────────────────────────────────────────────────────
cat(sprintf("\n=== Chi-Square Test of Independence ===\n"))
cat(sprintf("  H0: '%s' and '%s' are independent.\n", COL1, COL2))
cat(sprintf("  H1: '%s' and '%s' are NOT independent.\n\n", COL1, COL2))

chi_test <- chisq.test(cont_tab, correct = FALSE)
print(chi_test)

# Warn about small expected frequencies
exp_tab <- chi_test$expected
low_cells <- sum(exp_tab < 5)
if (low_cells > 0) {
  cat(sprintf("\nWarning: %d cell(s) have expected frequency < 5. ", low_cells))
  cat("Consider using Fisher's exact test.\n")
  cat("\n=== Fisher's Exact Test (as backup) ===\n")
  # Fisher only works on 2x2; if larger, skip
  if (nrow(cont_tab) == 2 && ncol(cont_tab) == 2) {
    print(fisher.test(cont_tab))
  } else {
    cat("Fisher's exact test requires a 2x2 table; skipping for this table size.\n")
  }
}

# ── 5. Expected frequencies ───────────────────────────────────────────────────
cat("\n=== Expected Frequencies (under H0) ===\n")
print(round(chi_test$expected, 2))

# ── 6. Residuals (Pearson) ────────────────────────────────────────────────────
cat("\n=== Pearson Residuals ===\n")
cat("  (Residual > |2| suggests a cell deviates significantly from independence)\n")
print(round(chi_test$residuals, 4))

# ── 7. Cramér's V (effect size) ───────────────────────────────────────────────
n     <- sum(cont_tab)
k     <- min(nrow(cont_tab), ncol(cont_tab))
cramer_v <- sqrt(chi_test$statistic / (n * (k - 1)))

cat(sprintf("\n=== Effect Size: Cramér's V ===\n"))
cat(sprintf("  Cramér's V = %.4f\n", cramer_v))
cat(sprintf("  Interpretation: %s association\n",
            ifelse(cramer_v < 0.1, "Negligible",
                   ifelse(cramer_v < 0.3, "Small",
                          ifelse(cramer_v < 0.5, "Moderate", "Strong")))))

# ── 8. Interpretation ─────────────────────────────────────────────────────────
cat(sprintf("\n=== Interpretation ===\n"))
cat(sprintf("  Chi-square statistic : %.4f\n", chi_test$statistic))
cat(sprintf("  Degrees of freedom   : %d\n",   chi_test$parameter))
cat(sprintf("  p-value              : %.6f\n",  chi_test$p.value))
cat(sprintf("  Alpha                : %.2f\n",  ALPHA))

if (chi_test$p.value < ALPHA) {
  cat(sprintf("\nConclusion: Reject H0. '%s' and '%s' are NOT independent\n", COL1, COL2))
  cat(sprintf("(p = %.4f < %.2f). There is a statistically significant association.\n",
              chi_test$p.value, ALPHA))
} else {
  cat(sprintf("\nConclusion: Fail to reject H0. No significant association between\n"))
  cat(sprintf("'%s' and '%s' (p = %.4f >= %.2f).\n", COL1, COL2, chi_test$p.value, ALPHA))
}

# ── 9. Mosaic plot ────────────────────────────────────────────────────────────
mosaicplot(cont_tab,
           main  = sprintf("Mosaic Plot: %s vs %s", COL1, COL2),
           color = TRUE,
           shade = TRUE,
           las   = 2,
           cex.axis = 0.8)
