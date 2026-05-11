# Airline Delays - structure, summary, mean median mode
df <- read.csv("datasets/airline_delays.csv")
print(str(df))
print(summary(df))
cat("\nMean:", mean(df$Delay), "\n")
cat("Median:", median(df$Delay), "\n")
mode_val <- names(sort(-table(df$Delay))[1])
cat("Mode:", mode_val, "\n")