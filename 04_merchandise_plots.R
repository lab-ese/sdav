# Merchandise Sales - histogram, boxplot, stem-leaf
df <- read.csv("datasets/merchandise_sales.csv")
dev.new()
par(mfrow=c(1,3))
hist(df$Sales, col="steelblue", main="Histogram")
boxplot(df$Sales, main="Boxplot")
stem(df$Sales)