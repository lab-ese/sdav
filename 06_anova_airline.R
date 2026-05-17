# =============================================================================
# Script 06: One-Way ANOVA — Compare Means Across Groups
# -----------------------------------------------------------------------------
# Generic: works on ANY CSV. Auto-detects a categorical grouping column
# (with 3+ levels) and a numeric response column.
# Change CSV_PATH and GROUP_COL_INDEX / RESPONSE_COL_INDEX if needed.
# =============================================================================

CSV_PATH <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)),
                      "data", "airline_delays.csv")

ALPHA <- 0.05   # significance level

# ── Generic helpers ────────────────────────────────────────────────────────────
load_csv <- function(path) {
  if (!file.exists(path)) stop(paste("File not found:", path))
  df <- read.csv(path, stringsAsFactors = FALSE)
  cat(sprintf("Loaded '%s'  |  %d rows  x  %d columns\n\n",
              basename(path), nrow(df), ncol(df)))
  df
}

get_numeric_cols <- function(df) names(df)[sapply(df, is.numeric)]

# Find categorical columns with at least min_levels unique values
get_group_cols <- function(df, min_levels = 3) {
  candidates <- names(df)[sapply(df, function(x) is.character(x) | is.factor(x))]
  Filter(function(col) length(unique(na.omit(df[[col]]))) >= min_levels, candidates)
}

# ── 1. Load ───────────────────────────────────────────────────────────────────
df         <- load_csv(CSV_PATH)
num_cols   <- get_numeric_cols(df)
group_cols <- get_group_cols(df, min_levels = 3)

cat(sprintf("Numeric columns    : %s\n", paste(num_cols,   collapse = ", ")))
cat(sprintf("Grouping candidates: %s\n", paste(group_cols, collapse = ", ")))

# Auto-select: first grouping col, first numeric col
group_col    <- group_cols[1]
response_col <- num_cols[1]

cat(sprintf("\nGroup    column : '%s'\n", group_col))
cat(sprintf("Response column : '%s'\n\n", response_col))

# ── 2. Prepare data ───────────────────────────────────────────────────────────
df_clean         <- df[, c(group_col, response_col)]
df_clean         <- na.omit(df_clean)
df_clean$group_f <- as.factor(df_clean[[group_col]])
groups           <- levels(df_clean$group_f)

cat(sprintf("Groups (%d levels): %s\n\n", length(groups), paste(groups, collapse = ", ")))

# ── 3. Group-level descriptive stats ──────────────────────────────────────────
cat("=== Group-Level Descriptive Statistics ===\n")
cat(sprintf("%-20s %6s %10s %10s %10s\n", "Group", "n", "Mean", "Std Dev", "Median"))
cat(strrep("-", 58), "\n")
for (g in groups) {
  v <- df_clean[[response_col]][df_clean$group_f == g]
  cat(sprintf("%-20s %6d %10.3f %10.3f %10.3f\n",
              g, length(v), mean(v), sd(v), median(v)))
}

# ── 4. One-way ANOVA ──────────────────────────────────────────────────────────
formula_str <- paste(response_col, "~ group_f")
model       <- aov(as.formula(formula_str), data = df_clean)

cat("\n=== ANOVA Table ===\n")
anova_table <- summary(model)
print(anova_table)

f_stat  <- anova_table[[1]]$`F value`[1]
p_value <- anova_table[[1]]$`Pr(>F)`[1]

# ── 5. Interpretation ─────────────────────────────────────────────────────────
cat("\n=== Interpretation ===\n")
cat(sprintf("  F-statistic : %.4f\n", f_stat))
cat(sprintf("  p-value     : %.6f\n", p_value))
cat(sprintf("  Alpha       : %.2f\n", ALPHA))

if (p_value < ALPHA) {
  cat(sprintf("\nConclusion: Reject H0. At least one group mean of '%s' differs\n", response_col))
  cat(sprintf("significantly across '%s' groups (p = %.4f < %.2f).\n", group_col, p_value, ALPHA))

  # Post-hoc Tukey HSD
  cat("\n=== Post-Hoc: Tukey HSD ===\n")
  tukey <- TukeyHSD(model, "group_f")
  print(tukey)
} else {
  cat(sprintf("\nConclusion: Fail to reject H0. No significant difference in '%s'\n", response_col))
  cat(sprintf("across groups (p = %.4f >= %.2f).\n", p_value, ALPHA))
}

# ── 6. Box plot for visual comparison ────────────────────────────────────────
boxplot(as.formula(paste(response_col, "~ group_f")),
        data   = df_clean,
        col    = rainbow(length(groups), alpha = 0.6),
        border = "gray30",
        main   = paste("One-Way ANOVA:", response_col, "by", group_col),
        xlab   = group_col,
        ylab   = response_col,
        las    = 1)
abline(h   = mean(df_clean[[response_col]], na.rm = TRUE),
       col = "red", lty = 2, lwd = 1.5)
legend("topright", legend = "Grand Mean", col = "red", lty = 2, bty = "n")
