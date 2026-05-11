# Binomial probability + visualization
p <- dbinom(0:10, size=10, prob=0.3)
cat("P(X=2) =", p[3], "\n")

dev.new()
barplot(p, names=0:10, col=ifelse(0:10==2,"red","steelblue"), main="Binomial(n=10,p=0.3)")