# 95% Confidence Interval + visualization
ci <- t.test(iris$Sepal.Length)$conf.int
cat("95% CI: [", ci[1], ",", ci[2], "]\n")

dev.new()
hist(iris$Sepal.Length, col="steelblue", main="95% CI for Sepal Length", prob=TRUE)
abline(v=ci[1],col="red"); abline(v=ci[2],col="red"); abline(v=mean(iris$Sepal.Length),col="green")