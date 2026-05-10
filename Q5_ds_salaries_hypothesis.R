# Code 2 - Hypothesis Testing: DS Salaries
df <- read.csv("ds_salaries.csv")
decide <- function(p) ifelse(p<0.05, "Reject H0", "Fail to reject H0")

# a) One-sample t-test (H0: mean salary = 100000 USD)
cat("=== a) One-Sample t-test ===\n")
t1 <- t.test(df$salary_in_usd, mu=100000)
print(t1); cat("Result:", decide(t1$p.value), "\n")

# b) Two-sample t-test (H0: senior salary = mid-level salary)
cat("\n=== b) Two-Sample t-test ===\n")
t2 <- t.test(df$salary_in_usd[df$experience_level=="SE"], df$salary_in_usd[df$experience_level=="MI"])
print(t2); cat("Result:", decide(t2$p.value), "\n")

# c) One-way ANOVA (H0: salary same across experience levels)
cat("\n=== c) One-Way ANOVA ===\n")
a <- summary(aov(salary_in_usd ~ experience_level, data=df))
print(a); cat("Result:", decide(a[[1]]$`Pr(>F)`[1]), "\n")
