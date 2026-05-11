# Histogram and Boxplot
dev.new()
par(mfrow=c(1,2))
hist(iris$Sepal.Length, main="Histogram")
boxplot(Sepal.Length ~ Species, iris, main="Boxplot")