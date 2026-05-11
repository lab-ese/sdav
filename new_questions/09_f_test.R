# F-test - Merchandise Sales vs E-commerce
merch <- read.csv("datasets/merchandise_sales.csv")
ecom <- read.csv("datasets/ecommerce.csv")

result <- var.test(merch$Sales, ecom$Amount)
print(result)

dev.new()
boxplot(list(Merchandise=merch$Sales, Ecommerce=ecom$Amount), main="Variance Comparison")