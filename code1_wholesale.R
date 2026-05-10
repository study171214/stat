# Code 1 - Visualization: Wholesale Customers Dataset
df <- read.csv("Wholesale customers data.csv")

# a) Histogram and Boxplot
par(mfrow=c(1,2), mar=c(5,4,3,2))
hist(df$Fresh, col="steelblue", main="Histogram - Fresh Spending", xlab="Fresh ($)", breaks=20)
boxplot(df$Fresh, col="tomato", main="Boxplot - Fresh Spending", ylab="Fresh ($)")

readline("Press Enter for next plot...")

# b) Bar Chart and Pie Chart
par(mfrow=c(1,2), mar=c(5,4,3,2))
channel_counts <- table(df$Channel)
names(channel_counts) <- c("Hotel/Restaurant","Retail")
barplot(channel_counts, col=c("orange","cyan"), main="Bar Chart - Channel", ylab="Count")
region_counts <- table(df$Region)
names(region_counts) <- paste("Region", names(region_counts))
pie(region_counts, col=rainbow(length(region_counts)), main="Pie Chart - Region")

readline("Press Enter for next plot...")

# c) Scatter Plot and Frequency Polygon
par(mfrow=c(1,2), mar=c(5,4,3,2))
plot(df$Grocery, df$Detergents_Paper, pch=19, col="darkgreen",
     main="Scatter - Grocery vs Detergents", xlab="Grocery", ylab="Detergents_Paper")
h <- hist(df$Milk, breaks=20, plot=FALSE)
plot(h$mids, h$counts, type="l", col="red", lwd=2,
     main="Freq Polygon - Milk Spending", xlab="Milk ($)", ylab="Frequency")
points(h$mids, h$counts, pch=19, col="red")

readline("Press Enter for next plot...")

# d) Stem-and-Leaf Plot
par(mfrow=c(1,1))
cat("\n--- Stem-and-Leaf Plot (Delicassen, first 50 values) ---\n")
stem(df$Delicassen[1:50])
cat("\nInterpretation: The stem-and-leaf plot shows delicassen spending is right-skewed.")
cat("\nMost customers spend low amounts, with few high spenders.\n")
