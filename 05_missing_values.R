# Handle missing values + visualization
x <- c(1,2,NA,4,5,NA,7,8,NA,10)
x_imputed <- x
x_imputed[is.na(x)] <- mean(x, na.rm = TRUE)

dev.new()
par(mfrow=c(1,2))
barplot(c(sum(is.na(x)), sum(!is.na(x))), names=c("Missing","Imputed"), col=c("red","green"), main="Before/After Imputation")
plot(x, type="b", col="red", main="Original (with NA)"); plot(x_imputed, type="b", col="green", main="After Imputation")