# =============================================================================
# Script 01: Student Data Frame | Subsetting | ggplot2 Scatter Plot
# -----------------------------------------------------------------------------
# Works on any data frame with a numeric score column.
# The student data is created internally; CSV path is optional for extension.
# =============================================================================

# ── 1. Create student data frame ──────────────────────────────────────────────
set.seed(42)
n <- 20

marks_vec <- sample(40:100, n, replace = TRUE)

assign_grade <- function(m) {
  cut(m,
      breaks = c(0, 40, 55, 70, 85, 100),
      labels = c("F", "D", "C", "B", "A"),
      right  = TRUE)
}

student_df <- data.frame(
  Name  = paste0("Student_", sprintf("%02d", 1:n)),
  Marks = marks_vec,
  Grade = assign_grade(marks_vec),
  stringsAsFactors = FALSE
)

cat("=== Full Student Data Frame ===\n")
print(student_df)
cat(sprintf("\nDimensions: %d rows x %d columns\n", nrow(student_df), ncol(student_df)))

# ── 2. Subsetting: students scoring above 75 ──────────────────────────────────
threshold <- 75
above_threshold <- student_df[student_df$Marks > threshold, ]

cat(sprintf("\n=== Students Scoring Above %d ===\n", threshold))
print(above_threshold)
cat(sprintf("\n%d out of %d students scored above %d (%.1f%%)\n",
            nrow(above_threshold), nrow(student_df), threshold,
            100 * nrow(above_threshold) / nrow(student_df)))

# Grade-wise summary
cat("\n=== Grade Distribution ===\n")
print(table(student_df$Grade))

# ── 3. Install and load ggplot2 ───────────────────────────────────────────────
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  cat("Installing ggplot2...\n")
  install.packages("ggplot2")
}
library(ggplot2)
cat("ggplot2 loaded successfully.\n")

# ── 4. Scatter plot (generic: x = row index, y = numeric score column) ────────
student_df$Index <- seq_len(nrow(student_df))

# Highlight above-threshold points
student_df$AboveThreshold <- ifelse(student_df$Marks > threshold, "Above 75", "Below/Equal 75")

p <- ggplot(student_df, aes(x = Index, y = Marks, color = Grade, shape = AboveThreshold)) +
  geom_point(size = 4, alpha = 0.85) +
  geom_hline(yintercept = threshold, linetype = "dashed", color = "red", linewidth = 0.9) +
  annotate("text", x = 1.2, y = threshold + 2.5,
           label = paste("Threshold =", threshold), hjust = 0, color = "red", size = 3.5) +
  scale_color_brewer(palette = "Set1", name = "Grade") +
  scale_shape_manual(values = c("Above 75" = 16, "Below/Equal 75" = 4), name = "Status") +
  labs(title    = "Student Marks — Scatter Plot",
       subtitle = sprintf("n = %d students | %d above threshold", n, nrow(above_threshold)),
       x        = "Student Index",
       y        = "Marks") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

print(p)

output_path <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)), "01_student_scatter.png")
tryCatch(
  ggsave(output_path, plot = p, width = 9, height = 5, dpi = 150),
  error = function(e) ggsave("01_student_scatter.png", plot = p, width = 9, height = 5, dpi = 150)
)
cat("Plot saved.\n")
