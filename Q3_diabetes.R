# Code 1 - Visualization: Diabetes (Pima Indians) Dataset
df <- read.csv("data.csv")

# a) Histogram and Boxplot
par(mfrow=c(1,2), mar=c(5,4,3,2))
hist(df$Glucose, col="steelblue", main="Histogram - Glucose", xlab="Glucose Level", breaks=20)
boxplot(df$Glucose, col="tomato", main="Boxplot - Glucose", ylab="Glucose Level")

readline("Press Enter for next plot...")

# b) Bar Chart and Pie Chart
par(mfrow=c(1,2), mar=c(5,4,3,2))
outcome_counts <- table(df$Outcome)
names(outcome_counts) <- c("Non-Diabetic","Diabetic")
barplot(outcome_counts, col=c("green","red"), main="Bar Chart - Diabetes Outcome", ylab="Count")
pie(outcome_counts, col=c("green","red"), main="Pie Chart - Diabetes Outcome")

readline("Press Enter for next plot...")

# c) Scatter Plot and Frequency Polygon
par(mfrow=c(1,2), mar=c(5,4,3,2))
plot(df$BMI, df$Glucose, pch=19, col="purple", cex=0.6,
     main="Scatter - BMI vs Glucose", xlab="BMI", ylab="Glucose")
h <- hist(df$BMI, breaks=20, plot=FALSE)
plot(h$mids, h$counts, type="l", col="red", lwd=2,
     main="Freq Polygon - BMI", xlab="BMI", ylab="Frequency")
points(h$mids, h$counts, pch=19, col="red")

readline("Press Enter for next plot...")

# d) Stem-and-Leaf Plot
par(mfrow=c(1,1))
cat("\n--- Stem-and-Leaf Plot (Age) ---\n")
stem(df$Age)
cat("\nInterpretation: The stem-and-leaf plot shows age distribution of patients.")
cat("\nMost patients are aged 21-35, with a strong right skew toward younger ages.\n")
