# Scatter plot and Frequency polygon
dev.new()
par(mfrow=c(1,2))
plot(iris$Sepal.Length, iris$Sepal.Width, main="Scatter Plot")
hist(iris$Sepal.Length, freq=FALSE, main="Frequency Polygon")
lines(density(iris$Sepal.Length))