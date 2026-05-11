# Handle missing values
x <- c(1,2,NA,4,5)
x[is.na(x)] <- mean(x, na.rm = TRUE)  # mean imputation
x