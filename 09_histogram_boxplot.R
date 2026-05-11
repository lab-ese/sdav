# Histogram and Boxplot
par(mfrow=c(1,2))
hist(iris$Sepal.Length)
boxplot(Sepal.Length ~ Species, iris)