# Normal probability + visualization
prob <- pnorm(60, mean=50, sd=10)
cat("P(X<60) =", prob, "\n")

dev.new()
x <- seq(20,80,length=200)
y <- dnorm(x,50,10)
plot(x,y,type="l",main="Normal(50,10) - P(X<60)",col="blue")
polygon(c(20,x[x<=60],60),c(0,y[x<=60],0),col="lightblue")
abline(v=60,col="red",lty=2)