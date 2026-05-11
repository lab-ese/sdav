# Chi-square test
titanic <- read.csv("datasets/titanic.csv")
chisq.test(table(titanic$Sex, titanic$Survived))