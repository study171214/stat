sal  <- read.csv("Salary Data.csv",   stringsAsFactors=FALSE)
ds   <- read.csv("ds_salaries.csv",   stringsAsFactors=FALSE)

cat("=== DATASET INFO ===\n")
cat("Salary_Data - Rows:", nrow(sal), "| Cols:", ncol(sal), "\n")
cat("DS Salaries - Rows:", nrow(ds),  "| Cols:", ncol(ds),  "\n")
print(head(sal, 3))
print(head(ds[, c("work_year","experience_level","job_title","salary_in_usd","company_size")], 3))

cat("\n=== MISSING VALUES ===\n")
cat("Salary_Data missing:\n"); print(colSums(is.na(sal)))
cat("DS Salaries missing:\n"); print(colSums(is.na(ds)))

sal <- sal[!is.na(sal$Salary) & !is.na(sal$`Years.of.Experience`), ]
ds  <- ds[!is.na(ds$salary_in_usd), ]
cat("After cleaning - no missing values in key columns\n")

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
desc(sal$Salary,                  "Salary_Data - Salary (USD)")
desc(sal$`Years.of.Experience`,   "Salary_Data - Years of Experience")
desc(sal$Age,                     "Salary_Data - Age")
desc(ds$salary_in_usd,            "DS Salaries - Salary in USD")
desc(ds$remote_ratio,             "DS Salaries - Remote Ratio")

cat("\n=== QUARTILES & IQR ===\n")
for (col in c("Salary","Years.of.Experience","Age")) {
  q <- quantile(sal[[col]], na.rm=TRUE)
  cat("Salary_Data", col, "- Q1:", q[2], "| Q2:", q[3], "| Q3:", q[4], "| IQR:", IQR(sal[[col]], na.rm=TRUE), "\n")
}
q <- quantile(ds$salary_in_usd, na.rm=TRUE)
cat("DS Salaries salary_in_usd - Q1:", q[2], "| Q2:", q[3], "| Q3:", q[4], "| IQR:", IQR(ds$salary_in_usd, na.rm=TRUE), "\n")

cat("\n=== COMPARISON & CONCLUSION ===\n")
cat("Salary_Data Mean Salary :", round(mean(sal$Salary),2),       "USD | SD:", round(sd(sal$Salary),2), "\n")
cat("DS Salaries Mean Salary :", round(mean(ds$salary_in_usd),2), "USD | SD:", round(sd(ds$salary_in_usd),2), "\n")
cat("Salary_Data Median      :", median(sal$Salary), "\n")
cat("DS Salaries Median      :", median(ds$salary_in_usd), "\n")
cat("Salary_Data CV          :", round(sd(sal$Salary)/mean(sal$Salary)*100,2), "%\n")
cat("DS Salaries CV          :", round(sd(ds$salary_in_usd)/mean(ds$salary_in_usd)*100,2), "%\n")
cat("\nConclusion:\n")
cat("- DS professionals earn on average", round(mean(ds$salary_in_usd) - mean(sal$Salary),0), "USD more than general salary dataset\n")
if (sd(ds$salary_in_usd) > sd(sal$Salary)) {
  cat("- DS Salaries show more spread (SD =", round(sd(ds$salary_in_usd),0), ") indicating higher salary variability by role/experience\n")
} else {
  cat("- Salary_Data shows more spread (SD =", round(sd(sal$Salary),0), ")\n")
}
cat("- DS salary IQR =", IQR(ds$salary_in_usd), "vs General salary IQR =", IQR(sal$Salary), "\n")

par(mfrow=c(2,2))
hist(sal$Salary,       col="steelblue", border="white", main="Salary_Data: Salary",    xlab="USD")
hist(ds$salary_in_usd, col="tomato",    border="white", main="DS Salaries: Salary USD",xlab="USD")
boxplot(sal$Salary,       col="steelblue", main="Salary_Data Boxplot",    ylab="USD")
boxplot(ds$salary_in_usd, col="tomato",    main="DS Salaries Boxplot",    ylab="USD")
par(mfrow=c(1,1))

plot(sal$`Years.of.Experience`, sal$Salary, pch=19, col="steelblue",
     main="Salary_Data: Experience vs Salary", xlab="Years of Experience", ylab="Salary")
abline(lm(Salary ~ `Years.of.Experience`, data=sal), col="red", lwd=2)
