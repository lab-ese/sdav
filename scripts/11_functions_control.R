# =============================================================================
# Script 11: User-Defined Functions | If-Else Control Flow
# -----------------------------------------------------------------------------
# Part A: Function to compute the average of any numeric vector + tests
# Part B: If-else program to classify a number as Positive / Negative / Zero
# Part C: Applied to any CSV — runs both on auto-detected numeric columns
# Change CSV_PATH to use a different file.
# =============================================================================

CSV_PATH <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)),
                      "data", "merchandise_sales.csv")

# ═════════════════════════════════════════════════════════════════════════════
# PART A: User-Defined Function — compute_average()
# ═════════════════════════════════════════════════════════════════════════════
cat("══════════════════════════════════════════════\n")
cat(" PART A: compute_average() Function\n")
cat("══════════════════════════════════════════════\n\n")

# Function definition
compute_average <- function(x, na_rm = TRUE, trim = 0) {
  if (!is.numeric(x))    stop("Input must be a numeric vector.")
  if (length(x) == 0)    stop("Input vector is empty.")
  if (all(is.na(x)))     stop("All values are NA.")
  mean(x, na.rm = na_rm, trim = trim)
}

# ── Unit tests ────────────────────────────────────────────────────────────────
run_test <- function(description, expr, expected = NULL, should_error = FALSE) {
  result <- tryCatch(
    { val <- eval(expr); list(ok = TRUE, val = val) },
    error = function(e) list(ok = FALSE, msg = conditionMessage(e))
  )

  if (should_error) {
    status <- if (!result$ok) "PASS (expected error)" else "FAIL (no error thrown)"
  } else {
    if (!result$ok) {
      status <- paste("FAIL — error:", result$msg)
    } else if (!is.null(expected)) {
      status <- if (isTRUE(all.equal(result$val, expected))) "PASS" else
                  sprintf("FAIL — got %.4f, expected %.4f", result$val, expected)
    } else {
      status <- sprintf("PASS — result: %.4f", result$val)
    }
  }
  cat(sprintf("  %-45s [%s]\n", description, status))
}

cat("--- Test Suite for compute_average() ---\n")
run_test("Basic integer vector",           quote(compute_average(1:10)),            5.5)
run_test("Basic float vector",             quote(compute_average(c(2.5, 3.5, 4.0))), 10/3)
run_test("Single element",                 quote(compute_average(42)),              42)
run_test("All same values",                quote(compute_average(rep(7, 5))),       7)
run_test("Vector with NAs (na_rm=TRUE)",   quote(compute_average(c(1, 2, NA, 4))), 7/3)
run_test("Negative numbers",               quote(compute_average(c(-5, -3, -1))),  -3)
run_test("Mixed positive and negative",    quote(compute_average(c(-10, 0, 10))),  0)
run_test("Trimmed mean (trim=0.1)",        quote(compute_average(c(1,2,3,4,100), trim=0.1)), NULL)
run_test("Non-numeric input → error",      quote(compute_average(c("a","b","c"))), should_error = TRUE)
run_test("Empty vector → error",           quote(compute_average(numeric(0))),     should_error = TRUE)

cat(sprintf("\ncompute_average(1:100)  = %.2f\n",  compute_average(1:100)))
cat(sprintf("compute_average(c(0))   = %.2f\n",   compute_average(0)))
cat(sprintf("compute_average(rnorm(1000)) ≈ %.4f  (expected ~0)\n", compute_average(rnorm(1000))))

# ═════════════════════════════════════════════════════════════════════════════
# PART B: If-Else — classify_number()
# ═════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════\n")
cat(" PART B: classify_number() — If-Else Control\n")
cat("══════════════════════════════════════════════\n\n")

classify_number <- function(x) {
  if (!is.numeric(x) || length(x) != 1) stop("Provide a single numeric value.")
  if (is.na(x))  return("NA / Missing")
  if (x > 0)     return("Positive")
  if (x < 0)     return("Negative")
  return("Zero")
}

# Extended classifier with magnitude
classify_detailed <- function(x) {
  if (!is.numeric(x) || length(x) != 1) stop("Provide a single numeric value.")
  if (is.na(x))   return("NA / Missing")

  sign_str <- if (x > 0) "Positive" else if (x < 0) "Negative" else "Zero"

  if (x == 0) return("Zero")

  magnitude <- abs(x)
  mag_str   <- if (magnitude < 1)       "fractional"
               else if (magnitude < 10)  "single-digit"
               else if (magnitude < 100) "double-digit"
               else if (magnitude < 1e3) "triple-digit"
               else                      "large"

  sprintf("%s (%s)", sign_str, mag_str)
}

# Test cases
test_numbers <- c(-1000, -42.7, -1, -0.5, 0, 0.001, 1, 7, 55, 999, 1e6, NA)

cat("--- classify_number() Test Cases ---\n")
cat(sprintf("  %-14s  %-12s  %-30s\n", "Input", "Basic", "Detailed"))
cat(strrep("-", 60), "\n")
for (val in test_numbers) {
  basic    <- classify_number(val)
  detailed <- classify_detailed(val)
  cat(sprintf("  %-14s  %-12s  %-30s\n",
              ifelse(is.na(val), "NA", as.character(val)),
              basic, detailed))
}

# ── Interactive classifier (run in RStudio console to use) ────────────────────
cat("\n--- Interactive Classifier ---\n")
cat("To classify a custom number, call:\n")
cat("  classify_number(your_number)\n")
cat("  classify_detailed(your_number)\n")

# ═════════════════════════════════════════════════════════════════════════════
# PART C: Apply both functions to CSV data
# ═════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════\n")
cat(" PART C: Applied to CSV Data\n")
cat("══════════════════════════════════════════════\n\n")

load_csv <- function(path) {
  if (!file.exists(path)) stop(paste("File not found:", path))
  df <- read.csv(path, stringsAsFactors = FALSE)
  cat(sprintf("Loaded '%s'  |  %d rows  x  %d columns\n\n",
              basename(path), nrow(df), ncol(df)))
  df
}
get_numeric_cols <- function(df) names(df)[sapply(df, is.numeric)]

df       <- load_csv(CSV_PATH)
num_cols <- get_numeric_cols(df)

cat("=== compute_average() on every numeric column ===\n")
cat(sprintf("  %-25s  %12s\n", "Column", "Average"))
cat(strrep("-", 40), "\n")
for (col in num_cols) {
  avg <- compute_average(na.omit(df[[col]]))
  cat(sprintf("  %-25s  %12.4f\n", col, avg))
}

# Apply classify_number to each column's mean
cat("\n=== classify_number() on column averages ===\n")
cat(sprintf("  %-25s  %12s  %-20s\n", "Column", "Mean", "Classification"))
cat(strrep("-", 60), "\n")
for (col in num_cols) {
  avg  <- compute_average(na.omit(df[[col]]))
  cls  <- classify_detailed(avg)
  cat(sprintf("  %-25s  %12.4f  %-20s\n", col, avg, cls))
}
