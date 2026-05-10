# ============================================================
# Dataset: Online Sales Data (Online_Sales_Data.csv)
# ============================================================

sales <- read.csv("Online_Sales_Data.csv", stringsAsFactors = FALSE)

cat("Columns:", ncol(sales), "\n")
cat("Rows:", nrow(sales), "\n\n")

# ----------------------------------------------------------
# Classification function: Revenue tier per transaction
# ----------------------------------------------------------
classify_transaction <- function(revenue) {
  if (revenue < 100) {
    return("Low Revenue Transaction")
  } else if (revenue < 500) {
    return("Medium Revenue Transaction")
  } else {
    return("High Revenue Transaction")
  }
}

cat("Example:", classify_transaction(sales$Total.Revenue[1]), "\n\n")

# ----------------------------------------------------------
# Loop: Print classification for first 10 transactions
# ----------------------------------------------------------
cat("=== Transaction Revenue Classification ===\n")
for (i in 1:min(10, nrow(sales))) {
  rev      <- sales$Total.Revenue[i]
  product  <- sales$Product.Name[i]
  category <- sales$Product.Category[i]
  cat("Txn", i,
      "| Category:", category,
      "| Product:", product,
      "| Revenue:", rev,
      "|", classify_transaction(rev), "\n")
}

# ----------------------------------------------------------
# Function: Overall sales summary
# ----------------------------------------------------------
calculate_sales_summary <- function(revenues, units) {
  cat("\n=== Sales Summary ===\n")
  cat("Total Revenue   :", sum(revenues, na.rm = TRUE), "\n")
  cat("Average Revenue :", round(mean(revenues, na.rm = TRUE), 2), "\n")
  cat("Total Units Sold:", sum(units, na.rm = TRUE), "\n")
  cat("Max Single Sale :", max(revenues, na.rm = TRUE), "\n")
  return(invisible(NULL))
}

calculate_sales_summary(sales$Total.Revenue, sales$Units.Sold)

# ----------------------------------------------------------
# Segment distribution
# ----------------------------------------------------------
segments <- sapply(sales$Total.Revenue, classify_transaction)
cat("\n=== Transaction Tier Distribution ===\n")
print(table(segments))

# ----------------------------------------------------------
# Revenue by category
# ----------------------------------------------------------
cat("\n=== Revenue by Product Category ===\n")
cat_rev <- aggregate(Total.Revenue ~ Product.Category, data = sales, FUN = sum)
cat_rev <- cat_rev[order(-cat_rev$Total.Revenue), ]
print(cat_rev)
