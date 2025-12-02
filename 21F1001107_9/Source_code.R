"
The dataset ‘InsectSpray’ has data on the count of insects in areas treated with one of
6 different types of sprays.
Note: The dataset is already in the proper format for the one-way analysis of variance
– a vector with the data (count), and one with a factor describing the level (spray).
Based on the given information, answer the following questions:
"

# W9Q1a

"
Import dataset in R
"

require(datasets)
data(InsectSprays)
head(InsectSprays)

# W9Q1b

"
Using R, plot a side-by-side box plot to see if the treatment means are equal.
"

boxplot(count ~ spray,
    data = InsectSprays,
    main = "Insect Count by Spray Type",
    xlab = "Spray Type",
    ylab = "Insect Count",
    col = "lightblue"
)

# W9Q1c

"
Perform a one-way ANOVA to check if the
treatment means are equal. Do they agree?
"

anova_model <- aov(count ~ spray, data = InsectSprays)
summary(anova_model)

# Since p-value (<2e-16) < alpha we reject the null hypothesis
# and conclude that the treatment means are not equal.


# Q2
"
A manufacturing company has purchased three new machines
of different types, say, m1, m2 and m3. Owner of the company wants to analyse
the effectiveness of the machines for which he wants to
observe the five outputs from each machine.
"

# W9Q2a

"
Simulate 50 positive values from discrete uniform distribution for machine m1.
Repeat it for m2 and m3 by varying parameters of the distribution.
"

set.seed(123) # Set seed for reproducibility

n <- 50

m1 <- sample(10:20, n, replace = TRUE) # Machine 1 values
m2 <- sample(15:25, n, replace = TRUE) # Machine 2 values
m3 <- sample(20:30, n, replace = TRUE) # Machine 3 values

# W9Q2b

"
Consider the following models:
• yij = mi + ϵij
• yij = 3 + 4mi + ϵij
• yij = −1 + 2mi + ϵij
Note: Simulate error terms, ϵij from the N(0, 1)
distribution and use the simulated values of part (a).
Store the values in the data frame with two columns, i.e. Machine type and
Effectiveness score (yij ’s)
"

epsilon <- rnorm(n * 3, mean = 0, sd = 1)

# Create a combined factor vector for Machine Type
machine_type <- factor(rep(c("m1", "m2", "m3"), each = n))

# Combine the 'm' values into a single vector for calculation
m_values <- c(m1, m2, m3)

# --- Model 1: y = m + epsilon ---
y1 <- m_values + epsilon
data_model1 <- data.frame(Machine = machine_type, Effectiveness = y1)

# --- Model 2: y = 3 + 4m + epsilon ---
y2 <- 3 + (4 * m_values) + epsilon
data_model2 <- data.frame(Machine = machine_type, Effectiveness = y2)

# --- Model 3: y = -1 + 2m + epsilon ---
y3 <- -1 + (2 * m_values) + epsilon
data_model3 <- data.frame(Machine = machine_type, Effectiveness = y3)

# W9Q2c

"
Define null and alternative hypotheses to perform one-way ANOVA.
"

null_hypo <- "The mean effectiveness scores for all three machines are equal."

alternate_hypo <- "At least one machine's mean effectiveness
score is different from the others."

# W9Q2d
"
For each of the model, write an R - code (from scratch) to perform one-way ANOVA
and comment on the obtained result about acceptance/ rejection of hypothesis.
"

perform_anova_scratch <- function(df) {
    # 1. Calculate Grand Mean
    grand_mean <- mean(df$Effectiveness)

    # 2. Calculate Group Means
    group_means <- aggregate(Effectiveness ~ Machine, df, mean)

    # 3. Calculate SS_Between (Treatment Sum of Squares)
    SS_Between <- 0
    for (lvl in group_means$Machine) {
        ni <- sum(df$Machine == lvl)
        mi <- group_means$Effectiveness[group_means$Machine == lvl]
        SS_Between <- SS_Between + (ni * (mi - grand_mean)^2)
    }

    # 4. Calculate SS_Total (Total Sum of Squares)
    SS_Total <- sum((df$Effectiveness - grand_mean)^2)

    # 5. Calculate SS_Error (Residual Sum of Squares)
    SS_Error <- SS_Total - SS_Between

    # 6. Degrees of Freedom
    k <- length(unique(df$Machine)) # Number of groups (3)
    N <- nrow(df) # Total observations (150)
    df_between <- k - 1
    df_error <- N - k

    # 7. Mean Squares
    MS_Between <- SS_Between / df_between
    MS_Error <- SS_Error / df_error

    # 8. F-Statistic
    F_stat <- MS_Between / MS_Error

    # 9. P-value
    p_value <- 1 - pf(F_stat, df_between, df_error)

    # Output Results
    cat("--- ANOVA From Scratch ---\n")
    cat("F-Statistic:", F_stat, "\n")
    cat("P-value:", p_value, "\n")

    if (p_value < 0.05) {
        cat("Result: Reject H0 (Means are different)\n\n")
    } else {
        cat("Result: Accept H0 (Means are equal)\n\n")
    }
}

# Run for all 3 models
print("Model 1 Analysis:")
perform_anova_scratch(data_model1)

print("Model 2 Analysis:")
perform_anova_scratch(data_model2)

print("Model 3 Analysis:")
perform_anova_scratch(data_model3)

# W9Q2e

"
For each of the above-mentioned models, perform one-way ANOVA by using inbuilt
function in R. Comment on the obtained result and compare with the outputs
obtained in part (d)
"

check_builtin <- function(df, model_name) {
    cat("--- Built-in ANOVA for", model_name, "---\n")
    res <- aov(Effectiveness ~ Machine, data = df)
    print(summary(res))
    cat("\n----------------------------------\n")
}

check_builtin(data_model1, "Model 1")
check_builtin(data_model2, "Model 2")
check_builtin(data_model3, "Model 3")
