# Student data frame, subsetting, ggplot2 scatter plot

# Create data frame
students <- data.frame(
  Name = c("Aarav", "Rohan", "Ishaan", "Diya", "Ananya", "Arjun", "Meera", "Rahul", "Kavya", "Vihaan"),
  Marks = c(85,92,78,88,45,67,95,72,81,58),
  Grade = c("A","A","B","A","F","C","A","B","B","D")
)

# Subset above 75
high_scorers <- subset(students, Marks > 75)
print(high_scorers)

# Install and load ggplot2
install.packages("ggplot2", repos="http://cran.rstudio.com/", quiet=TRUE)
library(ggplot2)

dev.new()
ggplot(students, aes(x=Name, y=Marks, color=Grade)) + 
  geom_point(size=5) + 
  ggtitle("Student Marks Scatter Plot") +
  theme(axis.text.x=element_text(angle=45))