# ============================================================
# Dataset: Diabetes (data.csv — Pima Indians Diabetes)
# ============================================================

diabetes <- read.csv("data.csv", stringsAsFactors = FALSE)

diabetes$BMI     <- as.numeric(diabetes$BMI)
diabetes$Glucose <- as.numeric(diabetes$Glucose)

cat("Columns:", ncol(diabetes), "\n")
cat("Rows:", nrow(diabetes), "\n\n")

# ----------------------------------------------------------
# Classification function: BMI risk category
# ----------------------------------------------------------
classify_bmi_risk <- function(bmi) {
  if (is.na(bmi) || bmi == 0) {
    return("Unknown")
  } else if (bmi < 18.5) {
    return("Underweight")
  } else if (bmi < 25.0) {
    return("Normal Weight")
  } else if (bmi < 30.0) {
    return("Overweight")
  } else {
    return("Obese")
  }
}

cat("Example:", classify_bmi_risk(diabetes$BMI[1]), "\n\n")

# ----------------------------------------------------------
# Loop: Print classification for first 10 patients
# ----------------------------------------------------------
cat("=== Patient BMI Risk Classification ===\n")
for (i in 1:min(10, nrow(diabetes))) {
  p <- diabetes[i, ]
  cat("Patient", i,
      "| BMI:", p$BMI,
      "| Glucose:", p$Glucose,
      "| Age:", p$Age,
      "| Diabetic:", ifelse(p$Outcome == 1, "Yes", "No"),
      "|", classify_bmi_risk(p$BMI), "\n")
}

# ----------------------------------------------------------
# Function: Clinical statistics summary
# ----------------------------------------------------------
summarise_diabetes <- function(df) {
  cat("\n=== Clinical Summary ===\n")
  cat("Total Patients      :", nrow(df), "\n")
  cat("Diabetic Patients   :", sum(df$Outcome, na.rm = TRUE), "\n")
  cat("Diabetes Rate       :", round(mean(df$Outcome, na.rm = TRUE) * 100, 2), "%\n")
  cat("Average BMI         :", round(mean(df$BMI[df$BMI > 0], na.rm = TRUE), 2), "\n")
  cat("Average Glucose     :", round(mean(df$Glucose[df$Glucose > 0], na.rm = TRUE), 2), "\n")
  cat("Average Age         :", round(mean(df$Age, na.rm = TRUE), 2), "\n")
  return(invisible(NULL))
}

summarise_diabetes(diabetes)

# ----------------------------------------------------------
# BMI risk segment distribution
# ----------------------------------------------------------
segments <- sapply(diabetes$BMI, classify_bmi_risk)
cat("\n=== BMI Risk Category Distribution ===\n")
print(table(segments))

# ----------------------------------------------------------
# Diabetes rate by BMI category
# ----------------------------------------------------------
cat("\n=== Diabetes Rate by BMI Category ===\n")
diabetes$BMI_Category <- sapply(diabetes$BMI, classify_bmi_risk)
rate_by_bmi <- aggregate(Outcome ~ BMI_Category, data = diabetes, FUN = mean)
rate_by_bmi$DiabetesRate <- paste0(round(rate_by_bmi$Outcome * 100, 1), "%")
print(rate_by_bmi[, c("BMI_Category", "DiabetesRate")])
