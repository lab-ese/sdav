# Independent t-test
t.test(iris$Petal.Length[iris$Species=="setosa"], 
       iris$Petal.Length[iris$Species=="virginica"])