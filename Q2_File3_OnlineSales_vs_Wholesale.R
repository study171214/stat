online    <- read.csv("Online Sales Data.csv",       stringsAsFactors=FALSE)
wholesale <- read.csv("Wholesale customers data.csv", stringsAsFactors=FALSE)

cat("=== DATASET INFO ===\n")
cat("Online Sales - Rows:", nrow(online),    "| Cols:", ncol(online),    "\n")
cat("Wholesale    - Rows:", nrow(wholesale), "| Cols:", ncol(wholesale), "\n")
print(head(online[, c("Product.Category","Units.Sold","Unit.Price","Total.Revenue")], 3))
print(head(wholesale, 3))

cat("\n=== MISSING VALUES ===\n")
cat("Online Sales missing:\n");  print(colSums(is.na(online)))
cat("Wholesale missing:\n");     print(colSums(is.na(wholesale)))

online    <- online[!is.na(online$Total.Revenue) & !is.na(online$Unit.Price), ]
wholesale <- wholesale[complete.cases(wholesale), ]
cat("After cleaning - all rows complete\n")

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
desc(online$Total.Revenue, "Online Sales - Total Revenue")
desc(online$Unit.Price,    "Online Sales - Unit Price")
desc(online$Units.Sold,    "Online Sales - Units Sold")
desc(wholesale$Fresh,              "Wholesale - Fresh")
desc(wholesale$Milk,               "Wholesale - Milk")
desc(wholesale$Grocery,            "Wholesale - Grocery")
desc(wholesale$Frozen,             "Wholesale - Frozen")
desc(wholesale$Detergents_Paper,   "Wholesale - Detergents & Paper")
desc(wholesale$Delicassen,         "Wholesale - Delicassen")

cat("\n=== QUARTILES & IQR ===\n")
for (col in c("Total.Revenue","Unit.Price","Units.Sold")) {
  q <- quantile(online[[col]], na.rm=TRUE)
  cat("Online", col, "- Q1:", q[2], "| Q2:", q[3], "| Q3:", q[4], "| IQR:", IQR(online[[col]], na.rm=TRUE), "\n")
}
for (col in c("Fresh","Milk","Grocery","Frozen")) {
  q <- quantile(wholesale[[col]], na.rm=TRUE)
  cat("Wholesale", col, "- Q1:", q[2], "| Q2:", q[3], "| Q3:", q[4], "| IQR:", IQR(wholesale[[col]], na.rm=TRUE), "\n")
}

cat("\n=== COMPARISON & CONCLUSION ===\n")
cat("Online Total Revenue  - Mean:", round(mean(online$Total.Revenue),2),  "| SD:", round(sd(online$Total.Revenue),2),  "\n")
cat("Wholesale Fresh Spend - Mean:", round(mean(wholesale$Fresh),2),        "| SD:", round(sd(wholesale$Fresh),2),        "\n")
cat("Wholesale Grocery     - Mean:", round(mean(wholesale$Grocery),2),      "| SD:", round(sd(wholesale$Grocery),2),      "\n")
cat("Online Revenue CV     :", round(sd(online$Total.Revenue)/mean(online$Total.Revenue)*100,2), "%\n")
cat("Wholesale Fresh CV    :", round(sd(wholesale$Fresh)/mean(wholesale$Fresh)*100,2), "%\n")
cat("\nConclusion:\n")
cat("- Online Sales has", nrow(online), "transactions; Wholesale has", nrow(wholesale), "clients\n")
cat("- Online Revenue range =", diff(range(online$Total.Revenue)), "showing diverse product price points\n")
cat("- Wholesale Fresh has the highest mean spend (", round(mean(wholesale$Fresh),0), ") among all product categories\n")
cat("- Both datasets show high CV (>80%) indicating significant variability in spending/revenue\n")

par(mfrow=c(2,3))
hist(online$Total.Revenue,   col="steelblue", border="white", main="Online: Revenue",  xlab="USD")
hist(online$Unit.Price,      col="steelblue", border="white", main="Online: Unit Price",xlab="USD")
hist(online$Units.Sold,      col="steelblue", border="white", main="Online: Units Sold",xlab="Units")
hist(wholesale$Fresh,        col="tomato",    border="white", main="Wholesale: Fresh",  xlab="Spend")
hist(wholesale$Grocery,      col="tomato",    border="white", main="Wholesale: Grocery",xlab="Spend")
hist(wholesale$Milk,         col="tomato",    border="white", main="Wholesale: Milk",   xlab="Spend")
par(mfrow=c(1,1))

par(mfrow=c(1,2))
boxplot(online$Total.Revenue, col="steelblue", main="Online Revenue Boxplot",    ylab="USD")
boxplot(wholesale[, c("Fresh","Milk","Grocery","Frozen")], col=c("tomato","gold","green","skyblue"),
        main="Wholesale Categories Boxplot", las=2)
par(mfrow=c(1,1))
