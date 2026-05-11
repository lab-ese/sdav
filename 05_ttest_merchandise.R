# One-sample t-test on Merchandise Sales
df <- read.csv("datasets/merchandise_sales.csv")
result <- t.test(df$Sales, mu=35000)
print(result)

dev.new()
hist(df$Sales, col="steelblue", prob=TRUE, main="Sales Distribution")
abline(v=mean(df$Sales), col="blue"); abline(v=35000, col="red", lty=2)