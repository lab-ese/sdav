# Chi-square test + visualization
titanic <- read.csv("datasets/titanic.csv")
result <- chisq.test(table(titanic$Sex, titanic$Survived))
print(result)

dev.new()
mosaicplot(table(titanic$Sex, titanic$Survived), color=TRUE, main="Sex vs Survival")