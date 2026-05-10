# Code 2 - Hypothesis Testing: Salary Data
df <- na.omit(read.csv("Salary Data.csv"))
decide <- function(p) ifelse(p<0.05, "Reject H0", "Fail to reject H0")

# a) One-sample t-test (H0: mean salary = 100000)
cat("=== a) One-Sample t-test ===\n")
t1 <- t.test(df$Salary, mu=100000)
print(t1); cat("Result:", decide(t1$p.value), "\n")

# b) Two-sample t-test (H0: male salary = female salary)
cat("\n=== b) Two-Sample t-test ===\n")
t2 <- t.test(df$Salary[df$Gender=="Male"], df$Salary[df$Gender=="Female"])
print(t2); cat("Result:", decide(t2$p.value), "\n")

# c) One-way ANOVA (H0: salary same across education levels)
cat("\n=== c) One-Way ANOVA ===\n")
a <- summary(aov(Salary ~ Education.Level, data=df))
print(a); cat("Result:", decide(a[[1]]$`Pr(>F)`[1]), "\n")
