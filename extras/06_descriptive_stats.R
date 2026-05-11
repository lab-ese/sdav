# Descriptive statistics + visualization
data <- iris$Sepal.Length
stats <- c(mean=mean(data), median=median(data), sd=sd(data), var=var(data), range=max(data)-min(data))
print(stats)

dev.new()
par(mfrow=c(1,2))
hist(data, col="steelblue", main="Sepal Length Distribution", prob=TRUE)
lines(density(data), col="red")
abline(v=mean(data), col="blue"); abline(v=median(data), col="green")
legend("topright", c("Mean","Median"), col=c("blue","green"), lty=1)
boxplot(data, main="Boxplot")