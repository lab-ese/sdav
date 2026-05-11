# One-way ANOVA + visualization
result <- summary(aov(Sepal.Length~Species, iris))
print(result)

dev.new()
boxplot(Sepal.Length~Species, data=iris, main="ANOVA Comparison")
points(tapply(iris$Sepal.Length, iris$Species, mean), pch=18, col="red", cex=2)