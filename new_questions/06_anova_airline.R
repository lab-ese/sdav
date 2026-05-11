# One-way ANOVA on Airline Delays
df <- read.csv("datasets/airline_delays.csv")
result <- aov(Delay ~ Airline, data=df)
print(summary(result))

dev.new()
boxplot(Delay ~ Airline, data=df, main="Delay by Airline")