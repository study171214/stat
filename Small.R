---------------------------------------------------------------------------------------------------------------------------------------
  
  # Read dataset
  mall <- read.csv("Mall_Customers.csv")

# Spending classification function
classify_spender <- function(x) {
  if(x <= 33){
    return("Low")
  } else if(x <= 66){
    return("Medium")
  } else {
    return("High")
  }
}

# First 10 customers
for(i in 1:10){
  cat("Customer", mall$CustomerID[i],
      "- Score:", mall$`Spending Score (1-100)`[i],
      "-", classify_spender(mall$`Spending Score (1-100)`[i]), "\n")
}

# Summary
cat("\nAverage Income:",
    mean(mall$`Annual Income (k$)`), "\n")

cat("Average Spending Score:",
    mean(mall$`Spending Score (1-100)`), "\n")

# Distribution
table(sapply(mall$`Spending Score (1-100)`, classify_spender))

# Gender count
table(mall$Gender)

---------------------------------------------------------------------------------------------------------------------------------------
  
  # Read dataset
  sales <- read.csv("Online_Sales_Data.csv")

# Fill missing values
sales$Total.Revenue[is.na(sales$Total.Revenue)] <- mean(sales$Total.Revenue, na.rm=TRUE)

# Revenue classification function
classify_transaction <- function(x) {
  if(x < 100){
    return("Low")
  } else if(x < 500){
    return("Medium")
  } else {
    return("High")
  }
}

# First 10 transactions
for(i in 1:10){
  cat(sales$Product.Name[i],
      "-", sales$Total.Revenue[i],
      "-", classify_transaction(sales$Total.Revenue[i]), "\n")
}

# Summary
cat("\nTotal Revenue:",
    sum(sales$Total.Revenue), "\n")

cat("Average Revenue:",
    mean(sales$Total.Revenue), "\n")

# Distribution
table(sapply(sales$Total.Revenue, classify_transaction))

# Revenue by category
aggregate(Total.Revenue ~ Product.Category,
          data=sales,
          sum)

---------------------------------------------------------------------------------------------------------------------------------------
  # Read dataset
  happiness <- read.csv("2015.csv")

# Happiness classification
classify_happiness <- function(x){
  if(x >= 7){
    return("High")
  } else if(x >= 5){
    return("Medium")
  } else {
    return("Low")
  }
}

# First 10 countries
for(i in 1:10){
  cat(happiness$Country[i],
      "-", happiness$Happiness.Score[i],
      "-", classify_happiness(happiness$Happiness.Score[i]), "\n")
}

# Summary
cat("\nAverage Happiness Score:",
    mean(happiness$Happiness.Score), "\n")

cat("Highest Score:",
    max(happiness$Happiness.Score), "\n")

cat("Lowest Score:",
    min(happiness$Happiness.Score), "\n")

# Distribution
table(sapply(happiness$Happiness.Score,
             classify_happiness))

# Top 5 countries
head(happiness[order(-happiness$Happiness.Score),
               c("Country","Happiness.Score")], 5)
---------------------------------------------------------------------------------------------------------------------------------------
  
  # Read dataset
  salary_df <- read.csv("Salary_Data.csv")

# Fill NA values
salary_df$Salary[is.na(salary_df$Salary)] <- mean(salary_df$Salary, na.rm=TRUE)

# Salary classification
classify_salary <- function(x){
  if(x < 50000){
    return("Entry")
  } else if(x < 100000){
    return("Mid")
  } else {
    return("Senior")
  }
}

# First 10 records
for(i in 1:10){
  cat("Employee", i,
      "-", salary_df$Salary[i],
      "-", classify_salary(salary_df$Salary[i]), "\n")
}

# Statistics
cat("\nAverage Salary:", mean(salary_df$Salary), "\n")
cat("Highest Salary:", max(salary_df$Salary), "\n")
cat("Lowest Salary:", min(salary_df$Salary), "\n")

# Salary groups count
table(sapply(salary_df$Salary, classify_salary))

# Average salary by education level
aggregate(Salary ~ Education.Level,
          data=salary_df,
          mean)

---------------------------------------------------------------------------------------------------------------------------------------
  # Read dataset
  titanic <- read.csv("Titanic-Dataset.csv")

# Fill missing Age values
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age, na.rm=TRUE)

# Survival classification
classify_survival <- function(sex, age){
  if(sex == "female"){
    return("High")
  } else if(age < 15){
    return("Medium")
  } else {
    return("Low")
  }
}

# First 10 passengers
for(i in 1:10){
  cat("Passenger", titanic$PassengerId[i],
      "-", classify_survival(titanic$Sex[i],
                             titanic$Age[i]), "\n")
}

# Summary
cat("\nTotal Passengers:", nrow(titanic), "\n")
cat("Survival Rate:",
    mean(titanic$Survived)*100, "%\n")

cat("Average Fare:",
    mean(titanic$Fare, na.rm=TRUE), "\n")

# Distribution
table(mapply(classify_survival,
             titanic$Sex,
             titanic$Age))

# Survival by class
aggregate(Survived ~ Pclass,
          data=titanic,
          mean)

---------------------------------------------------------------------------------------------------------------------------------------
  
  # Read dataset
  wholesale <- read.csv("Wholesale_customers_data.csv")

# Total spend
wholesale$Total_Spend <- rowSums(
  wholesale[,c("Fresh","Milk","Grocery",
               "Frozen","Detergents_Paper",
               "Delicassen")]
)

# Customer classification
classify_customer <- function(x){
  if(x < 20000){
    return("Small")
  } else if(x < 60000){
    return("Medium")
  } else {
    return("Large")
  }
}

# First 10 customers
for(i in 1:10){
  cat("Customer", i,
      "-", wholesale$Total_Spend[i],
      "-", classify_customer(wholesale$Total_Spend[i]), "\n")
}

# Summary
cat("\nAverage Spend:",
    mean(wholesale$Total_Spend), "\n")

cat("Maximum Spend:",
    max(wholesale$Total_Spend), "\n")

cat("Minimum Spend:",
    min(wholesale$Total_Spend), "\n")

# Distribution
table(sapply(wholesale$Total_Spend, classify_customer))

# Average spend by channel
aggregate(Total_Spend ~ Channel,
          data=wholesale,
          mean)

---------------------------------------------------------------------------------------------------------------------------------------
  
  # Read dataset
  diabetes <- read.csv("data.csv")

# Fill missing BMI values
diabetes$BMI[diabetes$BMI == 0] <- NA
diabetes$BMI[is.na(diabetes$BMI)] <- mean(diabetes$BMI, na.rm=TRUE)

# BMI classification
classify_bmi <- function(x){
  if(x < 18.5){
    return("Underweight")
  } else if(x < 25){
    return("Normal")
  } else if(x < 30){
    return("Overweight")
  } else {
    return("Obese")
  }
}

# First 10 patients
for(i in 1:10){
  cat("Patient", i,
      "-", diabetes$BMI[i],
      "-", classify_bmi(diabetes$BMI[i]), "\n")
}

# Summary
cat("\nAverage BMI:",
    mean(diabetes$BMI), "\n")

cat("Average Glucose:",
    mean(diabetes$Glucose), "\n")

cat("Diabetes Rate:",
    mean(diabetes$Outcome)*100, "%\n")

# BMI category count
table(sapply(diabetes$BMI, classify_bmi))

# Diabetes rate by BMI
diabetes$BMI_Category <- sapply(diabetes$BMI, classify_bmi)

aggregate(Outcome ~ BMI_Category,
          data=diabetes,
          mean)

--------------------------------------------------------------------------------------------------------------------------------------
  
  # Read dataset
  ds <- read.csv("ds_salaries.csv")

# Fill missing salary values
ds$salary_in_usd[is.na(ds$salary_in_usd)] <- mean(ds$salary_in_usd, na.rm=TRUE)

# Salary classification
classify_salary <- function(x){
  if(x < 70000){
    return("Low")
  } else if(x < 130000){
    return("Medium")
  } else {
    return("High")
  }
}

# First 10 employees
for(i in 1:10){
  cat(ds$job_title[i],
      "-", ds$salary_in_usd[i],
      "-", classify_salary(ds$salary_in_usd[i]), "\n")
}

# Summary
cat("\nAverage Salary:",
    mean(ds$salary_in_usd), "\n")

cat("Maximum Salary:",
    max(ds$salary_in_usd), "\n")

cat("Minimum Salary:",
    min(ds$salary_in_usd), "\n")

# Distribution
table(sapply(ds$salary_in_usd, classify_salary))

# Average salary by experience level
aggregate(salary_in_usd ~ experience_level,
          data=ds,
          mean)