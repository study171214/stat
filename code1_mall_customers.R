# Code 1 - Visualization: Mall Customers Dataset
df <- read.csv("Mall_Customers.csv")

# a) Histogram and Boxplot
par(mfrow=c(1,2), mar=c(5,4,3,2))
hist(df$Annual.Income..k.., col="steelblue", main="Histogram - Annual Income", xlab="Income (k$)", breaks=15)
boxplot(df$Annual.Income..k.., col="tomato", main="Boxplot - Annual Income", ylab="Income (k$)")

readline("Press Enter for next plot...")

# b) Bar Chart and Pie Chart
par(mfrow=c(1,2), mar=c(5,4,3,2))
gender_counts <- table(df$Gender)
barplot(gender_counts, col=c("pink","lightblue"), main="Bar Chart - Gender Distribution", ylab="Count")
pie(gender_counts, col=c("pink","lightblue"), main="Pie Chart - Gender Distribution")

readline("Press Enter for next plot...")

# c) Scatter Plot and Frequency Polygon
par(mfrow=c(1,2), mar=c(5,4,3,2))
plot(df$Annual.Income..k.., df$Spending.Score..1.100., pch=19, col="purple",
     main="Scatter - Income vs Spending", xlab="Annual Income (k$)", ylab="Spending Score")
h <- hist(df$Spending.Score..1.100., breaks=15, plot=FALSE)
plot(h$mids, h$counts, type="l", col="red", lwd=2,
     main="Freq Polygon - Spending Score", xlab="Spending Score", ylab="Frequency")
points(h$mids, h$counts, pch=19, col="red")

readline("Press Enter for next plot...")

# d) Stem-and-Leaf Plot
par(mfrow=c(1,1))
cat("\n--- Stem-and-Leaf Plot (Age) ---\n")
stem(df$Age)
cat("\nInterpretation: The stem-and-leaf plot reveals that most customers are aged 25-45.")
cat("\nThe distribution is right-skewed with fewer elderly customers.\n")
