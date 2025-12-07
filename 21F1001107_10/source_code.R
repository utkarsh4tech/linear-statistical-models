# w10Q1

## On report except part h and i

# w10Q1h

t_calculated <- 1.732 # The t-statistic we found manually
n <- 3 # Sample size
p <- 2 # Number of parameters (beta0, beta1)
alpha <- 0.05 # Significance level (5%)

df <- n - p
print(paste("Degrees of Freedom:", df))


# For a two-tailed test, we look at 1 - alpha/2 (i.e., 0.975)
t_critical <- qt(1 - alpha / 2, df)

cat("\n--- Hypothesis Test Results ---\n")
cat("Calculated t-statistic: ", t_calculated, "\n")
cat("Critical t-value:       ", t_critical, "\n")

if (abs(t_calculated) > t_critical) {
    print("Conclusion: REJECT Null Hypothesis (Significant Relationship)")
} else {
    print("Conclusion: FAIL TO REJECT Null Hypothesis (No Significant Relationship)")
}

# w10Q1i

Y <- c(80, 70, 60)
X <- c(6, 6, 5)
model <- lm(Y ~ X)

# Summary to check hypothesis
summary(model)


# w10Q2

# 1. Import Data
data <- read.table("21F1001107_10/data/Battery.txt", header = TRUE)
View(data)
# 2. Ensure 'Brand' is treated as a factor
data$trt <- as.factor(data$trt)

# 3. Perform ANOVA
# For a balanced design, aov() is sufficient for the F-test
model <- aov(lifetime ~ trt, data = data)

# 4. View Results
summary(model)


anova_table <- summary(model)[[1]]
MS_Brand <- anova_table["trt", "Mean Sq"]
MS_Error <- anova_table["Residuals", "Mean Sq"]
n <- 4 # Number of replications

sigma_brand_sq <- (MS_Brand - MS_Error) / n
cat("Estimated Variance Component for Brand:", sigma_brand_sq)
