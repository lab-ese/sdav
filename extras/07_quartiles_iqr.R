# Quartiles & IQR + visualization
data <- iris$Sepal.Length
q <- quantile(data); i <- IQR(data)
print(q); cat("IQR:", i, "\n")

dev.new()
boxplot(data, main="Boxplot with Quartiles")
abline(h=q[2], col="blue"); abline(h=q[3], col="green"); abline(h=q[4], col="blue")
text(1.3, q[2], "Q1"); text(1.3, q[3], "Q2"); text(1.3, q[4], "Q3")