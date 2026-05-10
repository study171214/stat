# Code 1 - Visualization: Online Sales Data Dataset
df <- read.csv("Online Sales Data.csv")

# a) Histogram and Boxplot
par(mfrow=c(1,2), mar=c(5,4,3,2))
hist(df$Total.Revenue, col="steelblue", main="Histogram - Total Revenue", xlab="Revenue ($)", breaks=15)
boxplot(df$Total.Revenue, col="tomato", main="Boxplot - Total Revenue", ylab="Revenue ($)")

readline("Press Enter for next plot...")

# b) Bar Chart and Pie Chart
par(mfrow=c(1,2), mar=c(5,4,3,2))
cat_counts <- table(df$Product.Category)
barplot(sort(cat_counts, decreasing=TRUE), col=rainbow(length(cat_counts)),
        main="Bar Chart - Product Categories", las=2, cex.names=0.7, ylab="Count")
region_counts <- table(df$Region)
pie(region_counts, col=rainbow(length(region_counts)), main="Pie Chart - Sales by Region")

readline("Press Enter for next plot...")

# c) Scatter Plot and Frequency Polygon
par(mfrow=c(1,2), mar=c(5,4,3,2))
plot(df$Unit.Price, df$Total.Revenue, pch=19, col="darkgreen",
     main="Scatter - Price vs Revenue", xlab="Unit Price", ylab="Total Revenue")
h <- hist(df$Unit.Price, breaks=15, plot=FALSE)
plot(h$mids, h$counts, type="l", col="red", lwd=2,
     main="Freq Polygon - Unit Price", xlab="Unit Price", ylab="Frequency")
points(h$mids, h$counts, pch=19, col="red")

readline("Press Enter for next plot...")

# d) Stem-and-Leaf Plot
par(mfrow=c(1,1))
cat("\n--- Stem-and-Leaf Plot (Units Sold) ---\n")
stem(df$Units.Sold)
cat("\nInterpretation: The stem-and-leaf plot shows units sold per transaction.")
cat("\nMost transactions involve 1-5 units, with very few bulk orders.\n")
