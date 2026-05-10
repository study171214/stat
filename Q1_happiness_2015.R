# ============================================================
# Dataset: 2015 World Happiness Report (2015.csv)
# ============================================================

happiness <- read.csv("2015.csv", stringsAsFactors = FALSE)

cat("Columns:", ncol(happiness), "\n")
cat("Rows:", nrow(happiness), "\n\n")

# ----------------------------------------------------------
# Classification function: Happiness tier based on score
# ----------------------------------------------------------
classify_happiness <- function(score) {
  if (score >= 7.0) {
    return("High Happiness Country")
  } else if (score >= 5.0) {
    return("Medium Happiness Country")
  } else {
    return("Low Happiness Country")
  }
}

cat("Example:", classify_happiness(happiness$Happiness.Score[1]), "\n\n")

# ----------------------------------------------------------
# Loop: Print classification for first 10 rows
# ----------------------------------------------------------
cat("=== Country Happiness Classification ===\n")
for (i in 1:min(10, nrow(happiness))) {
  score <- happiness$Happiness.Score[i]
  country <- happiness$Country[i]
  cat("Country:", country,
      "| Score:", round(score, 3),
      "|", classify_happiness(score), "\n")
}

# ----------------------------------------------------------
# Function: Summarise happiness metrics
# ----------------------------------------------------------
summarise_happiness <- function(scores) {
  total   <- sum(scores, na.rm = TRUE)
  avg     <- mean(scores, na.rm = TRUE)
  highest <- max(scores, na.rm = TRUE)
  lowest  <- min(scores, na.rm = TRUE)

  cat("\n=== Happiness Score Summary ===\n")
  cat("Total Score Sum:", round(total, 3), "\n")
  cat("Average Score  :", round(avg, 3), "\n")
  cat("Highest Score  :", round(highest, 3), "\n")
  cat("Lowest Score   :", round(lowest, 3), "\n")

  return(avg)
}

summarise_happiness(happiness$Happiness.Score)

# ----------------------------------------------------------
# Segment distribution
# ----------------------------------------------------------
segments <- sapply(happiness$Happiness.Score, classify_happiness)
cat("\n=== Happiness Tier Distribution ===\n")
print(table(segments))

# ----------------------------------------------------------
# Top 5 happiest countries
# ----------------------------------------------------------
cat("\n=== Top 5 Happiest Countries ===\n")
top5 <- happiness[order(-happiness$Happiness.Score), ][1:5, c("Country", "Happiness.Score", "Region")]
print(top5)
