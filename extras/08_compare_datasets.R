# Compare datasets + visualization
result <- aggregate(Sepal.Length ~ Species, iris, mean)
print(result)

dev.new()
barplot(result$Sepal.Length, names=result$Species, col=c("red","green","blue"), main="Mean Sepal Length by Species")
abline(h=mean(iris$Sepal.Length), col="black", lty=2)