# Code 1 - Visualization: Titanic Dataset
df <- read.csv("Titanic-Dataset.csv")
df <- df[!is.na(df$Age), ]

# a) Histogram and Boxplot
par(mfrow=c(1,2), mar=c(5,4,3,2))
hist(df$Age, col="steelblue", main="Histogram - Age", xlab="Age", breaks=20)
boxplot(df$Age, col="tomato", main="Boxplot - Age", ylab="Age")

readline("Press Enter for next plot...")

# b) Bar Chart and Pie Chart
par(mfrow=c(1,2), mar=c(5,4,3,2))
surv_counts <- table(df$Survived)
names(surv_counts) <- c("Died","Survived")
barplot(surv_counts, col=c("gray","green"), main="Bar Chart - Survival", ylab="Count")
class_counts <- table(df$Pclass)
names(class_counts) <- c("1st","2nd","3rd")
pie(class_counts, col=c("gold","red","brown"), main="Pie - Passenger Class")

readline("Press Enter for next plot...")

# c) Scatter Plot and Frequency Polygon
par(mfrow=c(1,2), mar=c(5,4,3,2))
plot(df$Age, df$Fare, pch=19, col="darkblue", cex=0.6,
     main="Scatter - Age vs Fare", xlab="Age", ylab="Fare")
h <- hist(df$Fare, breaks=30, plot=FALSE)
plot(h$mids, h$counts, type="l", col="red", lwd=2,
     main="Freq Polygon - Fare", xlab="Fare", ylab="Frequency")
points(h$mids, h$counts, pch=19, col="red", cex=0.7)

readline("Press Enter for next plot...")

# d) Stem-and-Leaf Plot
par(mfrow=c(1,1))
cat("\n--- Stem-and-Leaf Plot (Age, first 100 values) ---\n")
stem(df$Age[1:100])
cat("\nInterpretation: The stem-and-leaf plot shows passenger age distribution.")
cat("\nMost passengers were aged 20-40. Children and elderly were minority groups.\n")
