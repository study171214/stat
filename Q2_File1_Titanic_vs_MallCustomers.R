titanic <- read.csv("Titanic-Dataset.csv", stringsAsFactors=FALSE)
mall    <- read.csv("Mall_Customers.csv",   stringsAsFactors=FALSE)

cat("=== DATASET INFO ===\n")
cat("Titanic  - Rows:", nrow(titanic), "| Cols:", ncol(titanic), "\n")
cat("Mall     - Rows:", nrow(mall),    "| Cols:", ncol(mall),    "\n")

cat("\n=== MISSING VALUES ===\n")
cat("Titanic missing per column:\n")
print(colSums(is.na(titanic)))
cat("Mall missing per column:\n")
print(colSums(is.na(mall)))

titanic$Age[is.na(titanic$Age)]   <- median(titanic$Age, na.rm=TRUE)
titanic$Fare[is.na(titanic$Fare)] <- median(titanic$Fare, na.rm=TRUE)
titanic <- titanic[!is.na(titanic$Embarked) & titanic$Embarked != "", ]
cat("\nAfter cleaning - Titanic missing:\n")
print(colSums(is.na(titanic[, c("Age","Fare","Embarked")])))

stat_mode <- function(x) { x <- x[!is.na(x)]; ux <- unique(x); ux[which.max(tabulate(match(x,ux)))] }

desc <- function(x, label) {
  cat("\n---", label, "---\n")
  cat("Mean    :", round(mean(x, na.rm=TRUE), 3), "\n")
  cat("Median  :", median(x, na.rm=TRUE), "\n")
  cat("Mode    :", stat_mode(x), "\n")
  cat("SD      :", round(sd(x, na.rm=TRUE), 3), "\n")
  cat("Variance:", round(var(x, na.rm=TRUE), 3), "\n")
  cat("Range   :", diff(range(x, na.rm=TRUE)), "\n")
  cat("Min     :", min(x, na.rm=TRUE), "| Max:", max(x, na.rm=TRUE), "\n")
  q <- quantile(x, c(0.25, 0.75), na.rm=TRUE)
  cat("Q1      :", q[1], "| Q3:", q[2], "| IQR:", IQR(x, na.rm=TRUE), "\n")
}

cat("\n=== DESCRIPTIVE STATISTICS ===\n")
desc(titanic$Age,  "Titanic - Age")
desc(titanic$Fare, "Titanic - Fare")
desc(mall$Age,                      "Mall - Age")
desc(mall$`Annual.Income..k..`,     "Mall - Annual Income (k$)")
desc(mall$`Spending.Score..1.100.`, "Mall - Spending Score")

cat("\n=== QUARTILES & IQR ===\n")
for (col in c("Age","Fare")) {
  q <- quantile(titanic[[col]], na.rm=TRUE)
  cat("Titanic", col, "- Q1:", q[2], "| Q2:", q[3], "| Q3:", q[4], "| IQR:", IQR(titanic[[col]], na.rm=TRUE), "\n")
}
for (col in c("Age","Annual.Income..k..","Spending.Score..1.100.")) {
  q <- quantile(mall[[col]], na.rm=TRUE)
  cat("Mall", col, "- Q1:", q[2], "| Q2:", q[3], "| Q3:", q[4], "| IQR:", IQR(mall[[col]], na.rm=TRUE), "\n")
}

cat("\n=== COMPARISON & CONCLUSION ===\n")
cat("Titanic Age   - Mean:", round(mean(titanic$Age),2),  "| SD:", round(sd(titanic$Age),2),  "\n")
cat("Mall Age      - Mean:", round(mean(mall$Age),2),     "| SD:", round(sd(mall$Age),2),     "\n")
cat("Titanic Fare  - Mean:", round(mean(titanic$Fare),2), "| SD:", round(sd(titanic$Fare),2), "\n")
cat("Mall Income   - Mean:", round(mean(mall$`Annual.Income..k..`),2), "| SD:", round(sd(mall$`Annual.Income..k..`),2), "\n")
cat("Mall Spending - Mean:", round(mean(mall$`Spending.Score..1.100..`),2), "| SD:", round(sd(mall$`Spending.Score..1.100..`),2), "\n")
cat("\nConclusion:\n")
cat("- Titanic passengers had mean age", round(mean(titanic$Age),1), "vs Mall customers", round(mean(mall$Age),1), "\n")
cat("- Titanic Fare has high SD (", round(sd(titanic$Fare),1), ") indicating wide wealth disparity\n")
cat("- Mall Spending Score SD =", round(sd(mall$`Spending.Score..1.100..`),1), "showing varied customer engagement\n")

par(mfrow=c(2,3))
hist(titanic$Age,  col="steelblue", border="white", main="Titanic: Age",      xlab="Age")
hist(titanic$Fare, col="steelblue", border="white", main="Titanic: Fare",     xlab="Fare")
hist(mall$Age,                      col="tomato",   border="white", main="Mall: Age",          xlab="Age")
hist(mall$`Annual.Income..k..`,     col="tomato",   border="white", main="Mall: Annual Income",xlab="Income (k$)")
hist(mall$`Spending.Score..1.100.`, col="tomato",   border="white", main="Mall: Spending Score",xlab="Score")
boxplot(titanic$Age, mall$Age, names=c("Titanic Age","Mall Age"), col=c("steelblue","tomato"), main="Age Comparison")
par(mfrow=c(1,1))
