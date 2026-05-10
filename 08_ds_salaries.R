# ============================================================
# Dataset: Data Science Salaries (ds_salaries.csv)
# ============================================================

ds <- read.csv("ds_salaries.csv", stringsAsFactors = FALSE)

# Drop index column if present
if ("Unnamed..0" %in% colnames(ds)) {
  ds$Unnamed..0 <- NULL
}

ds$salary_in_usd <- as.numeric(ds$salary_in_usd)
ds <- ds[!is.na(ds$salary_in_usd), ]

cat("Columns:", ncol(ds), "\n")
cat("Rows:", nrow(ds), "\n\n")

# ----------------------------------------------------------
# Classification function: DS salary tier
# ----------------------------------------------------------
classify_ds_salary <- function(salary_usd) {
  if (salary_usd < 70000) {
    return("Junior / Below Market")
  } else if (salary_usd < 130000) {
    return("Market Rate")
  } else {
    return("Senior / Above Market")
  }
}

cat("Example:", classify_ds_salary(ds$salary_in_usd[1]), "\n\n")

# ----------------------------------------------------------
# Loop: Print classification for first 10 records
# ----------------------------------------------------------
cat("=== Data Scientist Salary Classification ===\n")
for (i in 1:min(10, nrow(ds))) {
  d <- ds[i, ]
  exp_label <- switch(d$experience_level,
    "EN" = "Entry",
    "MI" = "Mid",
    "SE" = "Senior",
    "EX" = "Executive",
    d$experience_level)
  cat("Employee", i,
      "| Title:", d$job_title,
      "| Exp:", exp_label,
      "| Remote:", paste0(d$remote_ratio, "%"),
      "| USD Salary:", d$salary_in_usd,
      "|", classify_ds_salary(d$salary_in_usd), "\n")
}

# ----------------------------------------------------------
# Function: DS salary market summary
# ----------------------------------------------------------
summarise_ds_salaries <- function(salaries) {
  cat("\n=== DS Salary Market Summary ===\n")
  cat("Total Records   :", length(salaries), "\n")
  cat("Average Salary  :", round(mean(salaries, na.rm = TRUE), 2), "USD\n")
  cat("Median Salary   :", median(salaries, na.rm = TRUE), "USD\n")
  cat("Highest Salary  :", max(salaries, na.rm = TRUE), "USD\n")
  cat("Lowest Salary   :", min(salaries, na.rm = TRUE), "USD\n")
  return(invisible(NULL))
}

summarise_ds_salaries(ds$salary_in_usd)

# ----------------------------------------------------------
# Salary tier distribution
# ----------------------------------------------------------
segments <- sapply(ds$salary_in_usd, classify_ds_salary)
cat("\n=== Salary Tier Distribution ===\n")
print(table(segments))

# ----------------------------------------------------------
# Average salary by experience level
# ----------------------------------------------------------
cat("\n=== Average USD Salary by Experience Level ===\n")
exp_avg <- aggregate(salary_in_usd ~ experience_level, data = ds, FUN = mean)
exp_avg$salary_in_usd <- round(exp_avg$salary_in_usd, 2)
exp_avg$experience_level <- factor(
  exp_avg$experience_level,
  levels = c("EN", "MI", "SE", "EX"),
  labels = c("Entry", "Mid", "Senior", "Executive")
)
print(exp_avg[order(exp_avg$experience_level), ])
