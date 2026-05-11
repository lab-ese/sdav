# Independent t-test + visualization
result <- t.test(iris$Petal.Length[iris$Species=="setosa"], 
       iris$Petal.Length[iris$Species=="virginica"])
print(result)

dev.new()
par(mfrow=c(1,2))
boxplot(Petal.Length~Species, data=subset(iris, Species!="versicolor"), main="Petal Length")
stripchart(Petal.Length~Species, data=subset(iris, Species!="versicolor"), add=TRUE, method="jitter")