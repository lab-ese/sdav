# t-test general + visualization
result <- t.test(iris$Petal.Length, iris$Petal.Width, paired=TRUE)
print(result)

dev.new()
par(mfrow=c(1,2))
plot(iris$Petal.Length, iris$Petal.Width, main="Before vs After")
abline(lm(iris$Petal.Width~iris$Petal.Length), col="red")
hist(result$statistic, col="steelblue", main="t-distribution")