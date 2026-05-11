# Scatter plot and Frequency polygon
par(mfrow=c(1,2))
plot(iris$Sepal.Length, iris$Sepal.Width)
hist(iris$Sepal.Length, freq=FALSE); lines(density(iris$Sepal.Length))