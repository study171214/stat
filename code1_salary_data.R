# Code 1 - Visualization: Salary Data Dataset
df <- read.csv("Salary Data.csv")
df <- na.omit(df)

# a) Histogram and Boxplot
par(mfrow=c(1,2), mar=c(5,4,3,2))
hist(df$Salary, col="steelblue", main="Histogram - Salary", xlab="Salary ($)", breaks=15)
boxplot(df$Salary, col="tomato", main="Boxplot - Salary", ylab="Salary ($)")

readline("Press Enter for next plot...")

# b) Bar Chart and Pie Chart
par(mfrow=c(1,2), mar=c(5,4,3,2))
gender_counts <- table(df$Gender)
barplot(gender_counts, col=c("pink","lightblue","lightgreen"), main="Bar Chart - Gender", ylab="Count")
edu_counts <- table(df$Education.Level)
pie(edu_counts, col=rainbow(length(edu_counts)), main="Pie Chart - Education Level", cex=0.8)

readline("Press Enter for next plot...")

# c) Scatter Plot and Frequency Polygon
par(mfrow=c(1,2), mar=c(5,4,3,2))
plot(df$Years.of.Experience, df$Salary, pch=19, col="blue",
     main="Scatter - Experience vs Salary", xlab="Years of Experience", ylab="Salary ($)")
h <- hist(df$Salary, breaks=15, plot=FALSE)
plot(h$mids, h$counts, type="l", col="red", lwd=2,
     main="Freq Polygon - Salary", xlab="Salary ($)", ylab="Frequency")
points(h$mids, h$counts, pch=19, col="red")

readline("Press Enter for next plot...")

# d) Stem-and-Leaf Plot
par(mfrow=c(1,1))
cat("\n--- Stem-and-Leaf Plot (Years of Experience) ---\n")
stem(df$Years.of.Experience)
cat("\nInterpretation: The stem-and-leaf plot shows experience distribution.")
cat("\nMost employees have 2-15 years of experience, with a right-skewed distribution.\n")
