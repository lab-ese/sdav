# Chi-square distribution - Airline Delays
df <- read.csv("datasets/airline_delays.csv")
mean_delay <- mean(df$Delay)
cat("Mean delay:", mean_delay, "\n")
cat("P(X<40):", pchisq(40, df=length(df)-1), "\n")

dev.new()
x <- 0:150
plot(x, dchisq(x, df=19), type="l", main="Chi-square Distribution")