# Bar chart and Pie chart
dev.new()
par(mfrow=c(1,2))
barplot(table(iris$Species), main="Bar Chart")
pie(table(iris$Species), main="Pie Chart")