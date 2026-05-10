# ============================================================
# Dataset: Salary Data (Salary_Data.csv)
# ============================================================

salary_df <- read.csv("Salary_Data.csv", stringsAsFactors = FALSE)

# Clean salary column
salary_df$Salary <- as.numeric(salary_df$Salary)
salary_df <- salary_df[!is.na(salary_df$Salary), ]

cat("Columns:", ncol(salary_df), "\n")
cat("Rows:", nrow(salary_df), "\n\n")

# ----------------------------------------------------------
# Classification function: Salary band
# ----------------------------------------------------------
classify_salary_band <- function(salary) {
  if (salary < 50000) {
    return("Entry Level")
  } else if (salary < 100000) {
    return("Mid Level")
  } else {
    return("Senior / Executive Level")
  }
}

cat("Example:", classify_salary_band(salary_df$Salary[1]), "\n\n")

# ----------------------------------------------------------
# Loop: Print classification for first 10 employees
# ----------------------------------------------------------
cat("=== Employee Salary Band Classification ===\n")
for (i in 1:min(10, nrow(salary_df))) {
  sal   <- salary_df$Salary[i]
  title <- salary_df$Job.Title[i]
  exp   <- salary_df$Years.of.Experience[i]
  cat("Employee", i,
      "| Job:", title,
      "| Experience:", exp, "yrs",
      "| Salary:", sal,
      "|", classify_salary_band(sal), "\n")
}

# ----------------------------------------------------------
# Function: Salary statistics
# ----------------------------------------------------------
salary_stats <- function(salaries) {
  cat("\n=== Salary Statistics ===\n")
  cat("Total Payroll   :", sum(salaries, na.rm = TRUE), "\n")
  cat("Average Salary  :", round(mean(salaries, na.rm = TRUE), 2), "\n")
  cat("Median Salary   :", median(salaries, na.rm = TRUE), "\n")
  cat("Highest Salary  :", max(salaries, na.rm = TRUE), "\n")
  cat("Lowest Salary   :", min(salaries, na.rm = TRUE), "\n")
  return(invisible(NULL))
}

salary_stats(salary_df$Salary)

# ----------------------------------------------------------
# Segment distribution
# ----------------------------------------------------------
segments <- sapply(salary_df$Salary, classify_salary_band)
cat("\n=== Salary Band Distribution ===\n")
print(table(segments))

# ----------------------------------------------------------
# Average salary by education level
# ----------------------------------------------------------
cat("\n=== Average Salary by Education Level ===\n")
edu_avg <- aggregate(Salary ~ Education.Level, data = salary_df, FUN = mean)
edu_avg$Salary <- round(edu_avg$Salary, 2)
print(edu_avg[order(-edu_avg$Salary), ])
