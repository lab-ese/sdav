# Function to compute average + if-else classification
avg <- function(x) sum(x)/length(x)
data <- c(10,20,30,40,50)
cat("Average:", avg(data), "\n")

num <- -5
if(num > 0) cat("Positive\n") else if(num < 0) cat("Negative\n") else cat("Zero\n")