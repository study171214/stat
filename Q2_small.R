# Read datasets
sal <- read.csv("Salary Data.csv")
ds  <- read.csv("ds_salaries.csv")

# Remove missing values
sal <- na.omit(sal)
ds  <- na.omit(ds)

# Basic statistics
cat("Mean Salary (General):", mean(sal$Salary), "\n")
cat("Median Salary (General):", median(sal$Salary), "\n")
cat("SD Salary (General):", sd(sal$Salary), "\n\n")

cat("Mean Salary (DS):", mean(ds$salary_in_usd), "\n")
cat("Median Salary (DS):", median(ds$salary_in_usd), "\n")
cat("SD Salary (DS):", sd(ds$salary_in_usd), "\n\n")

# Quartiles and IQR
cat("General Salary IQR:", IQR(sal$Salary), "\n")
cat("DS Salary IQR:", IQR(ds$salary_in_usd), "\n\n")

# Conclusion
if(mean(ds$salary_in_usd) > mean(sal$Salary)){
  cat("DS salaries are higher on average.\n")
} else {
  cat("General salaries are higher on average.\n")
}

# Graphs
par(mfrow=c(1,2))

hist(sal$Salary,
     main="General Salary",
     col="skyblue",
     xlab="Salary")

hist(ds$salary_in_usd,
     main="DS Salary",
     col="pink",
     xlab="Salary")


-------------------------------------------------------------------------------------------------------------------------------------------
  # Read datasets
  titanic <- read.csv("Titanic-Dataset.csv")
mall <- read.csv("Mall_Customers.csv")

# Fill missing values
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm=TRUE)
titanic$Fare[is.na(titanic$Fare)] <- median(titanic$Fare, na.rm=TRUE)

# Basic statistics
cat("Titanic Mean Age:", mean(titanic$Age), "\n")
cat("Titanic Mean Fare:", mean(titanic$Fare), "\n\n")

cat("Mall Mean Age:", mean(mall$Age), "\n")
cat("Mall Mean Income:", mean(mall$`Annual Income (k$)`), "\n")
cat("Mall Mean Spending Score:", mean(mall$`Spending Score (1-100)`), "\n\n")

# IQR
cat("Titanic Age IQR:", IQR(titanic$Age), "\n")
cat("Mall Income IQR:", IQR(mall$`Annual Income (k$)`), "\n\n")

# Conclusion
if(mean(mall$`Annual Income (k$)`) > mean(titanic$Fare)){
  cat("Mall customers have higher average income than Titanic fare.\n")
}

# Graphs
par(mfrow=c(1,2))

hist(titanic$Age,
     main="Titanic Age",
     col="skyblue",
     xlab="Age")

hist(mall$`Annual Income (k$)`,
     main="Mall Income",
     col="pink",
     xlab="Income")

-------------------------------------------------------------------------------------------------------------------------------------------
  
  # Read datasets
  happy <- read.csv("2015.csv")
diabetes <- read.csv("data.csv")

# Replace 0 with NA and fill with median
diabetes$Glucose[diabetes$Glucose == 0] <- NA
diabetes$BMI[diabetes$BMI == 0] <- NA

diabetes$Glucose[is.na(diabetes$Glucose)] <- median(diabetes$Glucose, na.rm=TRUE)
diabetes$BMI[is.na(diabetes$BMI)] <- median(diabetes$BMI, na.rm=TRUE)

# Basic statistics
cat("Mean Happiness Score:", mean(happy$Happiness.Score), "\n")
cat("Mean GDP:", mean(happy$Economy..GDP.per.Capita.), "\n\n")

cat("Mean Glucose:", mean(diabetes$Glucose), "\n")
cat("Mean BMI:", mean(diabetes$BMI), "\n\n")

# IQR
cat("Happiness Score IQR:", IQR(happy$Happiness.Score), "\n")
cat("Glucose IQR:", IQR(diabetes$Glucose), "\n\n")

# Conclusion
cat("Happiness and health datasets analyzed successfully.\n")

# Graphs
par(mfrow=c(1,2))

hist(happy$Happiness.Score,
     main="Happiness Score",
     col="skyblue",
     xlab="Score")

hist(diabetes$Glucose,
     main="Glucose",
     col="pink",
     xlab="Glucose")

--------------------------------------------------------------------------------------------------------------------------------------------
  # Read datasets
  happy <- read.csv("2015.csv")
diabetes <- read.csv("data.csv")

# Replace 0 with NA and fill with median
diabetes$Glucose[diabetes$Glucose == 0] <- NA
diabetes$BMI[diabetes$BMI == 0] <- NA

diabetes$Glucose[is.na(diabetes$Glucose)] <- median(diabetes$Glucose, na.rm=TRUE)
diabetes$BMI[is.na(diabetes$BMI)] <- median(diabetes$BMI, na.rm=TRUE)

# Basic statistics
cat("Mean Happiness Score:", mean(happy$Happiness.Score), "\n")
cat("Mean GDP:", mean(happy$Economy..GDP.per.Capita.), "\n\n")

cat("Mean Glucose:", mean(diabetes$Glucose), "\n")
cat("Mean BMI:", mean(diabetes$BMI), "\n\n")

# IQR
cat("Happiness Score IQR:", IQR(happy$Happiness.Score), "\n")
cat("Glucose IQR:", IQR(diabetes$Glucose), "\n\n")

# Conclusion
cat("Happiness and health datasets analyzed successfully.\n")

# Graphs
par(mfrow=c(1,2))

hist(happy$Happiness.Score,
     main="Happiness Score",
     col="skyblue",
     xlab="Score")

hist(diabetes$Glucose,
     main="Glucose",
     col="pink",
     xlab="Glucose")
--------------------------------------------------------------------------------------------------------------------------------------------
  # Read datasets
  online <- read.csv("Online Sales Data.csv")
wholesale <- read.csv("Wholesale customers data.csv")

# Fill missing values
online$Total.Revenue[is.na(online$Total.Revenue)] <- mean(online$Total.Revenue, na.rm=TRUE)
online$Unit.Price[is.na(online$Unit.Price)] <- mean(online$Unit.Price, na.rm=TRUE)

# Basic statistics
cat("Mean Online Revenue:", mean(online$Total.Revenue), "\n")
cat("Mean Unit Price:", mean(online$Unit.Price), "\n\n")

cat("Mean Fresh Spend:", mean(wholesale$Fresh), "\n")
cat("Mean Grocery Spend:", mean(wholesale$Grocery), "\n\n")

# IQR
cat("Online Revenue IQR:", IQR(online$Total.Revenue), "\n")
cat("Wholesale Fresh IQR:", IQR(wholesale$Fresh), "\n\n")

# Conclusion
cat("Online and wholesale datasets analyzed successfully.\n")

# Graphs
par(mfrow=c(1,2))

hist(online$Total.Revenue,
     main="Online Revenue",
     col="skyblue",
     xlab="Revenue")

hist(wholesale$Fresh,
     main="Wholesale Fresh",
     col="pink",
     xlab="Spend")