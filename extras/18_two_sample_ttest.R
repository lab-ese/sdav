# Two-sample t-test + visualization
result <- t.test(Sepal.Length~Species, data=subset(iris, Species!="virginica"))
print(result)

dev.new()
par(mfrow=c(1,2))
boxplot(Sepal.Length~Species, data=subset(iris, Species!="virginica"), main="Comparison")
stripchart(Sepal.Length~Species, data=subset(iris, Species!="virginica"), add=TRUE, method="jitter")