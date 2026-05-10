# ============================================================
# Dataset: Mall Customers (Mall_Customers.csv)
# ============================================================

mall <- read.csv("Mall_Customers.csv", stringsAsFactors = FALSE)

cat("Columns:", ncol(mall), "\n")
cat("Rows:", nrow(mall), "\n\n")

# ----------------------------------------------------------
# Classification function: Spending segment
# ----------------------------------------------------------
classify_spender <- function(spending_score) {
  if (spending_score <= 33) {
    return("Low Spender")
  } else if (spending_score <= 66) {
    return("Moderate Spender")
  } else {
    return("High Spender")
  }
}

cat("Example:", classify_spender(mall$Spending.Score..1.100.[1]), "\n\n")

# ----------------------------------------------------------
# Loop: Print classification for first 10 customers
# ----------------------------------------------------------
cat("=== Customer Spending Classification ===\n")
for (i in 1:min(10, nrow(mall))) {
  score  <- mall$Spending.Score..1.100.[i]
  income <- mall$Annual.Income..k..[i]
  cat("Customer", mall$CustomerID[i],
      "| Income:", income, "k$",
      "| Spending Score:", score,
      "|", classify_spender(score), "\n")
}

# ----------------------------------------------------------
# Function: Income and spending summary
# ----------------------------------------------------------
summarise_mall <- function(incomes, scores) {
  cat("\n=== Mall Customer Summary ===\n")
  cat("Average Annual Income :", round(mean(incomes, na.rm = TRUE), 2), "k$\n")
  cat("Average Spending Score:", round(mean(scores,  na.rm = TRUE), 2), "\n")
  cat("Max Income            :", max(incomes, na.rm = TRUE), "k$\n")
  cat("Min Income            :", min(incomes, na.rm = TRUE), "k$\n")
}

summarise_mall(mall$Annual.Income..k.., mall$Spending.Score..1.100.)

# ----------------------------------------------------------
# Segment distribution
# ----------------------------------------------------------
segments <- sapply(mall$Spending.Score..1.100., classify_spender)
cat("\n=== Spending Segment Distribution ===\n")
print(table(segments))

# ----------------------------------------------------------
# Gender breakdown
# ----------------------------------------------------------
cat("\n=== Gender Breakdown ===\n")
print(table(mall$Gender))
