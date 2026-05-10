# Code 1 - Visualization: Data Science Salaries Dataset
df <- read.csv("ds_salaries.csv")

# a) Histogram and Boxplot
par(mfrow=c(1,2), mar=c(5,4,3,2))
hist(df$salary_in_usd, col="steelblue", main="Histogram - Salary (USD)", xlab="Salary ($)", breaks=20)
boxplot(df$salary_in_usd, col="tomato", main="Boxplot - Salary (USD)", ylab="Salary ($)")

readline("Press Enter for next plot...")

# b) Bar Chart and Pie Chart
par(mfrow=c(1,2), mar=c(5,4,3,2))
exp_counts <- table(df$experience_level)
barplot(exp_counts, col=rainbow(4), main="Bar Chart - Experience Level", ylab="Count")
size_counts <- table(df$company_size)
pie(size_counts, col=c("gold","skyblue","salmon"), main="Pie Chart - Company Size")

readline("Press Enter for next plot...")

# c) Scatter Plot and Frequency Polygon
par(mfrow=c(1,2), mar=c(5,4,3,2))
plot(df$work_year, df$salary_in_usd, pch=19, col="darkblue", cex=0.6,
     main="Scatter - Year vs Salary", xlab="Work Year", ylab="Salary (USD)")
h <- hist(df$salary_in_usd, breaks=25, plot=FALSE)
plot(h$mids, h$counts, type="l", col="red", lwd=2,
     main="Freq Polygon - Salary", xlab="Salary (USD)", ylab="Frequency")
points(h$mids, h$counts, pch=19, col="red", cex=0.7)

readline("Press Enter for next plot...")

# d) Stem-and-Leaf Plot
par(mfrow=c(1,1))
cat("\n--- Stem-and-Leaf Plot (Salary in USD / 1000, first 60 values) ---\n")
stem(df$salary_in_usd[1:60] / 1000)
cat("\nInterpretation: The stem-and-leaf plot shows DS salaries (in thousands USD).")
cat("\nMost salaries cluster between $50K-$150K, with some outliers above $300K.\n")
