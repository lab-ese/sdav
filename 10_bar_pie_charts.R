# Bar chart and Pie chart
par(mfrow=c(1,2))
barplot(table(iris$Species))
pie(table(iris$Species))