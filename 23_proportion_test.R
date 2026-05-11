# Proportion test + visualization
titanic <- read.csv("datasets/titanic.csv")
result <- prop.test(sum(titanic$Survived), nrow(titanic), p=0.5)
print(result)

dev.new()
barplot(c(0.5, sum(titanic$Survived)/nrow(titanic)), names=c("Expected","Observed"), 
        col=c("gray","steelblue"), main="Proportion Test")