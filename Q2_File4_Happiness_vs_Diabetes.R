happy    <- read.csv("2015.csv",   stringsAsFactors=FALSE)
diabetes <- read.csv("data.csv",   stringsAsFactors=FALSE)

cat("=== DATASET INFO ===\n")
cat("World Happiness 2015 - Rows:", nrow(happy),    "| Cols:", ncol(happy),    "\n")
cat("Diabetes Health Data - Rows:", nrow(diabetes), "| Cols:", ncol(diabetes), "\n")
print(head(happy[, c("Country","Happiness.Score","Economy..GDP.per.Capita.","Health..Life.Expectancy.")], 3))
print(head(diabetes, 3))

cat("\n=== MISSING VALUES ===\n")
cat("Happiness missing:\n");  print(colSums(is.na(happy)))
cat("Diabetes missing:\n");   print(colSums(is.na(diabetes)))

diabetes$Glucose[diabetes$Glucose == 0]           <- NA
diabetes$BloodPressure[diabetes$BloodPressure == 0] <- NA
diabetes$BMI[diabetes$BMI == 0]                   <- NA
diabetes$SkinThickness[diabetes$SkinThickness == 0] <- NA
diabetes$Insulin[diabetes$Insulin == 0]            <- NA

cat("\nDiabetes after replacing 0s with NA (zero = biologically impossible):\n")
print(colSums(is.na(diabetes)))

for (col in c("Glucose","BloodPressure","BMI","SkinThickness","Insulin")) {
  diabetes[[col]][is.na(diabetes[[col]])] <- round(median(diabetes[[col]], na.rm=TRUE), 1)
}
cat("After median imputation - no missing values remain\n")

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
desc(happy$Happiness.Score,              "Happiness - Happiness Score")
desc(happy$Economy..GDP.per.Capita.,     "Happiness - GDP per Capita")
desc(happy$Health..Life.Expectancy.,     "Happiness - Life Expectancy")
desc(happy$Freedom,                      "Happiness - Freedom")
desc(happy$Trust..Government.Corruption.,"Happiness - Trust (Govt Corruption)")
desc(diabetes$Glucose,                   "Diabetes - Glucose")
desc(diabetes$BloodPressure,             "Diabetes - Blood Pressure")
desc(diabetes$BMI,                       "Diabetes - BMI")
desc(diabetes$Age,                       "Diabetes - Age")

cat("\n=== QUARTILES & IQR ===\n")
for (col in c("Happiness.Score","Economy..GDP.per.Capita.","Health..Life.Expectancy.")) {
  q <- quantile(happy[[col]], na.rm=TRUE)
  cat("Happiness", col, "- Q1:", q[2], "| Q2:", q[3], "| Q3:", q[4], "| IQR:", IQR(happy[[col]], na.rm=TRUE), "\n")
}
for (col in c("Glucose","BloodPressure","BMI","Age")) {
  q <- quantile(diabetes[[col]], na.rm=TRUE)
  cat("Diabetes", col, "- Q1:", q[2], "| Q2:", q[3], "| Q3:", q[4], "| IQR:", IQR(diabetes[[col]], na.rm=TRUE), "\n")
}

cat("\n=== COMPARISON & CONCLUSION ===\n")
cat("Happiness Score   - Mean:", round(mean(happy$Happiness.Score),3),          "| SD:", round(sd(happy$Happiness.Score),3), "\n")
cat("GDP per Capita    - Mean:", round(mean(happy$Economy..GDP.per.Capita.),3),  "| SD:", round(sd(happy$Economy..GDP.per.Capita.),3), "\n")
cat("Life Expectancy   - Mean:", round(mean(happy$Health..Life.Expectancy.),3),  "| SD:", round(sd(happy$Health..Life.Expectancy.),3), "\n")
cat("Diabetes Glucose  - Mean:", round(mean(diabetes$Glucose),3),               "| SD:", round(sd(diabetes$Glucose),3), "\n")
cat("Diabetes BMI      - Mean:", round(mean(diabetes$BMI),3),                   "| SD:", round(sd(diabetes$BMI),3), "\n")
cat("Diabetes Age      - Mean:", round(mean(diabetes$Age),3),                   "| SD:", round(sd(diabetes$Age),3), "\n")
cat("\nConclusion:\n")
cat("- Happiness Score CV =", round(sd(happy$Happiness.Score)/mean(happy$Happiness.Score)*100,2), "% - relatively low variability across countries\n")
cat("- Diabetes Glucose CV =", round(sd(diabetes$Glucose)/mean(diabetes$Glucose)*100,2), "% - wider spread indicating varied patient conditions\n")
cat("- Happiness dataset covers", nrow(happy), "countries; Diabetes has", nrow(diabetes), "patient records\n")
cat("- Both datasets required cleaning: Happiness had no NAs; Diabetes had biologically-impossible 0s imputed\n")
diab_rate <- round(mean(diabetes$Outcome)*100,1)
cat("- Diabetes outcome rate in dataset:", diab_rate, "% of patients are diabetic\n")

par(mfrow=c(2,4))
hist(happy$Happiness.Score,             col="steelblue", border="white", main="Happiness Score",    xlab="Score")
hist(happy$Economy..GDP.per.Capita.,    col="steelblue", border="white", main="GDP per Capita",     xlab="GDP")
hist(happy$Health..Life.Expectancy.,    col="steelblue", border="white", main="Life Expectancy",    xlab="Score")
hist(happy$Freedom,                     col="steelblue", border="white", main="Freedom",            xlab="Score")
hist(diabetes$Glucose,                  col="tomato",    border="white", main="Glucose",            xlab="mg/dL")
hist(diabetes$BloodPressure,            col="tomato",    border="white", main="Blood Pressure",     xlab="mmHg")
hist(diabetes$BMI,                      col="tomato",    border="white", main="BMI",                xlab="BMI")
hist(diabetes$Age,                      col="tomato",    border="white", main="Age",                xlab="Years")
par(mfrow=c(1,1))

par(mfrow=c(1,2))
boxplot(happy[, c("Happiness.Score","Economy..GDP.per.Capita.","Health..Life.Expectancy.","Freedom")],
        col=c("steelblue","cornflowerblue","dodgerblue","skyblue"),
        main="World Happiness Boxplots", las=2)
boxplot(diabetes[, c("Glucose","BloodPressure","BMI","Age")],
        col=c("tomato","coral","salmon","lightsalmon"),
        main="Diabetes Health Boxplots", las=2)
par(mfrow=c(1,1))

plot(happy$Economy..GDP.per.Capita., happy$Happiness.Score, pch=19, col="steelblue",
     main="GDP per Capita vs Happiness Score", xlab="GDP per Capita", ylab="Happiness Score")
abline(lm(Happiness.Score ~ Economy..GDP.per.Capita., data=happy), col="red", lwd=2)

plot(diabetes$BMI, diabetes$Glucose, pch=19,
     col=ifelse(diabetes$Outcome==1,"tomato","steelblue"),
     main="BMI vs Glucose (Red=Diabetic)", xlab="BMI", ylab="Glucose")
legend("topright", legend=c("Diabetic","Non-Diabetic"), col=c("tomato","steelblue"), pch=19)
