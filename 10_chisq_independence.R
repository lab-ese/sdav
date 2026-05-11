# Chi-square test for independence - E-commerce
df <- read.csv("datasets/ecommerce.csv")
contingency <- table(df$Category, df$Payment)
result <- chisq.test(contingency)
print(result)

dev.new()
mosaicplot(contingency, color=TRUE, main="Category vs Payment")