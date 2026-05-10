# Code 1 - Visualization: 2015 World Happiness Dataset
df <- read.csv("2015.csv")

# a) Histogram and Boxplot
par(mfrow=c(1,2), mar=c(5,4,3,2))
hist(df$Happiness.Score, col="steelblue", main="Histogram - Happiness Score", xlab="Happiness Score", breaks=15)
boxplot(df$Happiness.Score, col="tomato", main="Boxplot - Happiness Score", ylab="Happiness Score")

readline("Press Enter for next plot...")

# b) Bar Chart and Pie Chart
par(mfrow=c(1,2), mar=c(5,4,3,2))
region_counts <- table(df$Region)
barplot(sort(region_counts, decreasing=TRUE), col=rainbow(length(region_counts)),
        main="Bar Chart - Countries per Region", las=2, cex.names=0.5, ylab="Count")
top5 <- sort(region_counts, decreasing=TRUE)[1:5]
pie(top5, col=rainbow(5), main="Pie Chart - Top 5 Regions", cex=0.8)

readline("Press Enter for next plot...")

# c) Scatter Plot and Frequency Polygon
par(mfrow=c(1,2), mar=c(5,4,3,2))
plot(df$Economy..GDP.per.Capita., df$Happiness.Score, pch=19, col="blue",
     main="Scatter - GDP vs Happiness", xlab="GDP per Capita", ylab="Happiness Score")
h <- hist(df$Happiness.Score, breaks=15, plot=FALSE)
plot(h$mids, h$counts, type="l", col="red", lwd=2,
     main="Frequency Polygon - Happiness Score", xlab="Happiness Score", ylab="Frequency")
points(h$mids, h$counts, pch=19, col="red")

readline("Press Enter for next plot...")

# d) Stem-and-Leaf Plot
par(mfrow=c(1,1))
cat("\n--- Stem-and-Leaf Plot (Happiness Score) ---\n")
stem(df$Happiness.Score)
cat("\nInterpretation: The stem-and-leaf plot shows the distribution of happiness scores.")
cat("\nMost scores cluster between 4.0 and 6.0, indicating a roughly normal distribution")
cat("\nwith a slight left skew. Few countries score above 7.0 or below 3.0.\n")
