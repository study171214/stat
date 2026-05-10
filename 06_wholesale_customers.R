# ============================================================
# Dataset: Wholesale Customers (Wholesale_customers_data.csv)
# ============================================================

wholesale <- read.csv("Wholesale_customers_data.csv", stringsAsFactors = FALSE)

cat("Columns:", ncol(wholesale), "\n")
cat("Rows:", nrow(wholesale), "\n\n")

# ----------------------------------------------------------
# Classification function: Customer type by total spend
# ----------------------------------------------------------
classify_wholesale_customer <- function(total_spend) {
  if (total_spend < 20000) {
    return("Small Account")
  } else if (total_spend < 60000) {
    return("Medium Account")
  } else {
    return("Large Account")
  }
}

# Compute total spend per customer (sum of all product categories)
wholesale$Total_Spend <- rowSums(
  wholesale[, c("Fresh", "Milk", "Grocery", "Frozen",
                "Detergents_Paper", "Delicassen")],
  na.rm = TRUE
)

cat("Example:", classify_wholesale_customer(wholesale$Total_Spend[1]), "\n\n")

# ----------------------------------------------------------
# Loop: Print classification for first 10 customers
# ----------------------------------------------------------
cat("=== Wholesale Customer Account Classification ===\n")
for (i in 1:min(10, nrow(wholesale))) {
  spend   <- wholesale$Total_Spend[i]
  channel <- ifelse(wholesale$Channel[i] == 1, "Horeca", "Retail")
  region  <- wholesale$Region[i]
  cat("Customer", i,
      "| Channel:", channel,
      "| Region:", region,
      "| Total Spend:", spend,
      "|", classify_wholesale_customer(spend), "\n")
}

# ----------------------------------------------------------
# Function: Category spend summary
# ----------------------------------------------------------
summarise_wholesale <- function(df) {
  categories <- c("Fresh", "Milk", "Grocery", "Frozen",
                  "Detergents_Paper", "Delicassen")
  cat("\n=== Category Spend Summary ===\n")
  for (cat_name in categories) {
    cat(sprintf("%-20s Total: %10.0f  |  Avg: %8.2f\n",
        cat_name,
        sum(df[[cat_name]], na.rm = TRUE),
        mean(df[[cat_name]], na.rm = TRUE)))
  }
  return(invisible(NULL))
}

summarise_wholesale(wholesale)

# ----------------------------------------------------------
# Account size segment distribution
# ----------------------------------------------------------
segments <- sapply(wholesale$Total_Spend, classify_wholesale_customer)
cat("\n=== Account Size Distribution ===\n")
print(table(segments))

# ----------------------------------------------------------
# Average spend by channel
# ----------------------------------------------------------
cat("\n=== Average Total Spend by Channel ===\n")
ch <- aggregate(Total_Spend ~ Channel, data = wholesale, FUN = mean)
ch$Channel <- ifelse(ch$Channel == 1, "Horeca", "Retail")
print(ch)
