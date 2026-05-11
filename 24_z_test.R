# Z-test + visualization
z <- (mean(iris$Sepal.Length)-5.5)/(sd(iris$Sepal.Length)/sqrt(nrow(iris)))
p_val <- 2*pnorm(-abs(z))
cat("Z =", z, "p-value =", p_val, "\n")

dev.new()
x <- seq(-4,4,length=200)
y <- dnorm(x)
plot(x,y,type="l",main="Standard Normal",col="blue")
polygon(c(x[x<=-abs(z)],-abs(z)),c(y[x<=-abs(z)],0),col="red")
polygon(c(abs(z),x[x>=abs(z)]),c(0,y[x>=abs(z)]),col="red")