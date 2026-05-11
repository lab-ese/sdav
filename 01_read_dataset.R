# Read datasets
data(iris)
titanic <- read.csv("datasets/titanic.csv")
mtcars <- read.csv("datasets/mtcars.csv")

dev.new()
par(mfrow=c(2,2))
head(iris)
head(titanic)
head(mtcars)
plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species, main="Iris Data")