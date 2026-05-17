# =============================================================================
# Script 07: Poisson Distribution — Probability of Events in a Fixed Interval
# -----------------------------------------------------------------------------
# Generic: works on ANY CSV. Auto-selects a numeric column with small integers
# (counts). Fits a Poisson distribution and computes probabilities.
# Change CSV_PATH as needed.
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

# Prefer a count-like column: integer, non-negative, low range
pick_count_col <- function(df) {
  num_cols <- get_numeric_cols(df)
  # Score columns: integers with small range score highest
  scores <- sapply(num_cols, function(col) {
    vals <- na.omit(df[[col]])
    is_int   <- all(vals == floor(vals))
    is_pos   <- all(vals >= 0)
    low_max  <- max(vals) < 500   # prefer count-scale
    small_sd <- sd(vals) < 50
    sum(c(is_int, is_pos, low_max, small_sd)) - (max(vals) / 1000)
  })
  num_cols[which.max(scores)]
}

# ── 1. Load ───────────────────────────────────────────────────────────────────
df         <- load_csv(CSV_PATH)
num_cols   <- get_numeric_cols(df)
count_col  <- pick_count_col(df)

cat(sprintf("Numeric columns   : %s\n", paste(num_cols, collapse = ", ")))
cat(sprintf("Selected for Poisson analysis: '%s'\n", count_col))
cat("(Column was auto-selected as the most count-like numeric column.)\n\n")

vals <- na.omit(df[[count_col]])

# ── 2. Estimate lambda ────────────────────────────────────────────────────────
lambda <- mean(vals)
cat(sprintf("=== Poisson Parameter (lambda) ===\n"))
cat(sprintf("  lambda (estimated rate) = %.4f\n", lambda))
cat(sprintf("  This is the average number of '%s' per observation.\n\n", count_col))

# ── 3. Probability computations ───────────────────────────────────────────────
max_k <- max(ceiling(lambda + 4 * sqrt(lambda)), 15)
k_vals <- 0:max_k

cat("=== Poisson Probability Table ===\n")
cat(sprintf("  P(X = k)  using lambda = %.4f\n\n", lambda))
cat(sprintf("  %-6s  %-14s  %-16s  %-16s\n", "k", "P(X = k)", "P(X <= k)", "P(X > k)"))
cat(strrep("-", 58), "\n")

for (k in k_vals) {
  p_exact <- dpois(k, lambda)
  p_cum   <- ppois(k, lambda)
  p_upper <- 1 - p_cum
  if (p_exact > 0.0001 || k <= 5) {
    cat(sprintf("  %-6d  %-14.6f  %-16.6f  %-16.6f\n", k, p_exact, p_cum, p_upper))
  }
}

# ── 4. Specific probability queries ──────────────────────────────────────────
cat(sprintf("\n=== Key Probability Queries for lambda = %.4f ===\n", lambda))
k_query <- ceiling(lambda)   # ask about the ceiling of mean

cat(sprintf("  P(X = %d)       = %.6f\n",  k_query,     dpois(k_query, lambda)))
cat(sprintf("  P(X <= %d)      = %.6f\n",  k_query,     ppois(k_query, lambda)))
cat(sprintf("  P(X > %d)       = %.6f\n",  k_query,     ppois(k_query, lambda, lower.tail = FALSE)))
cat(sprintf("  P(X = 0)        = %.6f  (probability of no events)\n", dpois(0, lambda)))
cat(sprintf("  P(X >= 1)       = %.6f  (probability of at least one event)\n",
            1 - dpois(0, lambda)))

# ── 5. Goodness-of-fit check ─────────────────────────────────────────────────
cat("\n=== Observed vs Expected (Poisson Fit) ===\n")
obs_table <- table(factor(vals, levels = 0:max(vals)))
exp_probs <- dpois(0:max(vals), lambda)
expected  <- exp_probs * length(vals)

cat(sprintf("  %-6s  %-10s  %-10s\n", "k", "Observed", "Expected"))
cat(strrep("-", 30), "\n")
for (k in 0:min(max(vals), 15)) {
  obs_k <- ifelse(k <= max(vals), as.integer(obs_table[as.character(k)]), 0)
  obs_k[is.na(obs_k)] <- 0
  cat(sprintf("  %-6d  %-10d  %-10.2f\n", k, obs_k, expected[k + 1]))
}

# ── 6. Plot: Observed vs Poisson ──────────────────────────────────────────────
k_range <- 0:min(ceiling(lambda + 4 * sqrt(lambda)), max(vals))
barplot(dpois(k_range, lambda),
        names.arg = k_range,
        col       = "steelblue",
        border    = "white",
        main      = sprintf("Poisson Distribution: %s  (lambda = %.2f)", count_col, lambda),
        xlab      = sprintf("Number of %s", count_col),
        ylab      = "Probability",
        las       = 1)
