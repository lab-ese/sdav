# 95% CI Advanced + visualization
ci <- t.test(iris$Sepal.Length)$conf.int
cat("95% CI: [", ci[1], ",", ci[2], "]\n")

dev.new()
par(mfrow=c(1,2))
hist(iris$Sepal.Length, col="steelblue", main="CI Region", prob=TRUE)
abline(v=ci[1],col="red"); abline(v=ci[2],col="red")
abline(v=mean(iris$Sepal.Length),col="green",lwd=2)
polygon(c(ci[1],seq(ci[1],ci[2],length=20),ci[2]),c(0,dnorm(seq(ci[1],ci[2],length=20),mean(iris$Sepal.Length),sd(iris$Sepal.Length)/sqrt(150)),0),col=rgb(1,0,0,0.3))

# Bootstrap CI
set.seed(123)
boot_mean <- replicate(1000, mean(sample(iris$Sepal.Length, replace=TRUE)))
boot_ci <- quantile(boot_mean, c(0.025, 0.975))
hist(boot_mean, col="green", main="Bootstrap CI")
abline(v=boot_ci[1],col="red"); abline(v=boot_ci[2],col="red")