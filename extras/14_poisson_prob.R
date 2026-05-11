# Poisson probability + visualization
p <- dpois(0:12, lambda=3)
cat("P(X=2) =", p[3], "\n")

dev.new()
barplot(p, names=0:12, col=ifelse(0:12==2,"red","purple"), main="Poisson(lambda=3)")