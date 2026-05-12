# Histogram
hist(df$Age,
     col="blue",
     main="Age Histogram",
     xlab="Age")

# Boxplot
boxplot(df$Age,
        col="red",
        main="Age Boxplot")

# Bar Chart
barplot(table(df$Survived),
        col=c("gray","green"),
        main="Survival Count")

# Pie Chart
pie(table(cut(sales$Total.Revenue[1:10],breaks=3)),
    col=c('orange','yellow','green'),
    labels=c('Low','Medium','High'))

# Scatter Plot
plot(df$Age, df$Fare,
     col="blue",
     pch=19,
     main="Age vs Fare",
     xlab="Age",
     ylab="Fare")

# Frequency Polygon
h <- hist(df$Fare, plot=FALSE)

plot(h$mids, h$counts,
     type="l",
     col="red",
     main="Fare Frequency Polygon",
     xlab="Fare",
     ylab="Frequency")