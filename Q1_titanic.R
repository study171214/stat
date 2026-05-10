# ============================================================
# Dataset: Titanic (Titanic-Dataset.csv)
# ============================================================

titanic <- read.csv("Titanic-Dataset.csv", stringsAsFactors = FALSE)

titanic$Age  <- as.numeric(titanic$Age)
titanic$Fare <- as.numeric(titanic$Fare)

cat("Columns:", ncol(titanic), "\n")
cat("Rows:", nrow(titanic), "\n\n")

# ----------------------------------------------------------
# Classification function: Passenger risk profile
# ----------------------------------------------------------
classify_survival_risk <- function(pclass, sex, age) {
  if (is.na(age)) age <- 30          # impute median-ish
  if (sex == "female" && pclass == 1) {
    return("High Survival Chance")
  } else if (sex == "female" || (sex == "male" && age < 15)) {
    return("Moderate Survival Chance")
  } else {
    return("Low Survival Chance")
  }
}

cat("Example:", classify_survival_risk(
  titanic$Pclass[1], titanic$Sex[1], titanic$Age[1]), "\n\n")

# ----------------------------------------------------------
# Loop: Print classification for first 10 passengers
# ----------------------------------------------------------
cat("=== Passenger Survival Risk Classification ===\n")
for (i in 1:min(10, nrow(titanic))) {
  p   <- titanic[i, ]
  cat("Passenger", p$PassengerId,
      "| Name:", p$Name,
      "| Class:", p$Pclass,
      "| Sex:", p$Sex,
      "| Age:", ifelse(is.na(p$Age), "NA", p$Age),
      "| Survived:", p$Survived,
      "|", classify_survival_risk(p$Pclass, p$Sex, p$Age), "\n")
}

# ----------------------------------------------------------
# Function: Fare and survival summary
# ----------------------------------------------------------
summarise_titanic <- function(df) {
  cat("\n=== Titanic Summary ===\n")
  cat("Total Passengers   :", nrow(df), "\n")
  cat("Survivors          :", sum(df$Survived, na.rm = TRUE), "\n")
  cat("Survival Rate      :", round(mean(df$Survived, na.rm = TRUE) * 100, 2), "%\n")
  cat("Average Fare       :", round(mean(df$Fare, na.rm = TRUE), 2), "\n")
  cat("Average Age        :", round(mean(df$Age, na.rm = TRUE), 2), "\n")
  return(invisible(NULL))
}

summarise_titanic(titanic)

# ----------------------------------------------------------
# Risk segment distribution
# ----------------------------------------------------------
segments <- mapply(classify_survival_risk,
                   titanic$Pclass, titanic$Sex, titanic$Age)
cat("\n=== Survival Risk Segment Distribution ===\n")
print(table(segments))

# ----------------------------------------------------------
# Survival by class and sex
# ----------------------------------------------------------
cat("\n=== Survival Rate by Passenger Class ===\n")
class_surv <- aggregate(Survived ~ Pclass, data = titanic, FUN = mean)
class_surv$SurvivalRate <- paste0(round(class_surv$Survived * 100, 1), "%")
print(class_surv[, c("Pclass", "SurvivalRate")])
