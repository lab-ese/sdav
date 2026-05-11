# F-test + visualization
result <- var.test(iris$Sepal.Length[iris$Species=="setosa"], 
         iris$Sepal.Length[iris$Species=="versicolor"])
print(result)

dev.new()
x <- seq(0,3,length=200)
y <- df(x, 49, 49)
plot(x,y,type="l",main="F-distribution(df1=49,df2=49)",col="blue")
abline(v=result$statistic,col="red")