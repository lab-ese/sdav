# Z-test
z <- (mean(iris$Sepal.Length)-5.5)/(sd(iris$Sepal.Length)/sqrt(nrow(iris)))
2*pnorm(-abs(z))