# One-sample t-test + visualization
result <- t.test(iris$Sepal.Length, mu=5.5)
print(result)

dev.new()
x <- seq(4,7,length=200)
y <- dt((x-mean(iris$Sepal.Length))/(sd(iris$Sepal.Length)/sqrt(150)), df=149)
plot(x,y,type="l",main="t-distribution",col="blue")
abline(v=5.5,col="red",lty=2); abline(v=mean(iris$Sepal.Length),col="green")