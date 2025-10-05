library(ggplot2)

# Set a seed for reproducibility of random simulations
set.seed(123)


# W2Q1a
"
χ^2 distribution
(a) For each i = 1, 2, . . . , 100 simulate 1000 random values from
Normal distribution with mean 10 and variance 16
by using command rnorm in R. Denote this data for each i as Xi
"
n_simulations <- 100
n_values <- 1000
pop_mean <- 10
pop_variance <- 16
pop_sd <- sqrt(pop_variance)

data_list <- vector("list", n_simulations)

for (i in 1:n_simulations) {
    data_list[[i]] <- rnorm(n_values, mean = pop_mean, sd = pop_sd)
}

print(data_list)

# W2Q1b
"
Calculate the means of each Xi(i = 1, 2, . . . , 100) and
store it in a new vector ‘Mi’.
"
M <- sapply(data_list, mean)
print(M)

# W2Q1c
"
Plot the histogram of ‘M = {Mi: 1 ≤ i ≤ 100}’ using the command hist() in
R-software. What should the distribution of M be?
"
hist(M,
    main = "Histogram of Sample Means (M)",
    xlab = "Sample Mean",
    col = "lightblue",
    border = "black",
    probability = TRUE
)
lines(density(M), col = "red", lwd = 2) # Add a density curve

# According to the Central Limit Theorem (CLT), the distribution of sample means
# (M) should be approximately Normal. Since the underlying population is Normal,
# the distribution of sample means is exactly Normal.
# The theoretical mean of this distribution is the population mean, mu = 10.
# The theoretical variance is sigma^2/n = 16/1000 = 0.016.
# So, M should follow N(10, 0.016).

# W2Q1d
"
Calculate the sample variance S_i^2
of each Xi(i = 1, 2, . . . , 100).
"
S_sq <- sapply(data_list, var)
print(S_sq)

# W2Q1e
"
Plot the histogram of ‘CHISQ= {(999/16)S_i^2: 1 ≤ i ≤ 100}’
using the command hist() in R-software.
What should the distribution of CHISQ be?
"
CHISQ <- (n_values - 1) / pop_variance * S_sq

hist(CHISQ,
    main = "Histogram of the Chi-Square Statistic",
    xlab = "Value of (n-1)S^2 / sigma^2",
    col = "lightgreen",
    border = "black",
    probability = TRUE,
    breaks = 20
)
# Overlay the theoretical chi-square density curve
curve(dchisq(x, df = n_values - 1), add = TRUE, col = "blue", lwd = 2)

# the CHISQ vector should follow a Chi-squared
# distribution with 999 degrees of freedom.

# W2Q1f
"
Calculate the statistic Ti = (Mi − 10)/(Si/√1000)
for each i = 1, 2, . . . , 100.
"

S <- sqrt(S_sq)
T_stat <- (M - pop_mean) / (S / sqrt(n_values))
print(T_stat)

# W2W1g

"
Plot the histogram of ‘T = {Ti: i = 1, 2, 3, . . . , 100}’
using the command hist() in R-software.
What should the distribution of T be?
"
hist(T_stat,
    main = "Histogram of the T-Statistic",
    xlab = "T-statistic Value",
    col = "salmon",
    border = "black",
    probability = TRUE,
    breaks = 20
)
# Overlay the theoretical t-distribution curve
curve(dt(x, df = n_values - 1), add = TRUE, col = "darkred", lwd = 2)
# T should follow a t-distribution with 999 degrees of freedom.

###########################################

# W2Q2a
"
Give a brief description of the dataset and
read the data set as data frame (name it ‘df’) in R.
"
data("faithful")
str(faithful)

df <- faithful


# W2Q2b
"
Using ‘ggplot’ in R, plot the scatter plot between the variables
‘waiting’ and ‘eruptions’. On visualizing the scatter-plot, c
omment on the relationship between ‘waiting’ and ‘eruptions’.
"
ggplot(df, aes(x = eruptions, y = waiting)) +
    geom_point(alpha = 0.6, color = "blue") +
    labs(
        title = "Scatter Plot of Waiting Time vs. Eruption Duration",
        subtitle = "Old Faithful Geyser Data",
        x = "Eruption Duration (minutes)",
        y = "Waiting Time to Next Eruption (minutes)"
    ) +
    theme_minimal()

# The scatter plot shows a positive linear relationship between the duration of
# an eruption and the waiting time until the next eruption. This means that
# longer eruptions tend to be followed by longer waiting periods. The data also
# appears to be clustered into two distinct groups, suggesting that a simple
# linear model might not capture the full complexity of the relationship.


# W2Q2c


# First, create the base scatter plot
p_base <- ggplot(df, aes(x = eruptions, y = waiting)) +
    geom_point(alpha = 0.4, color = "black") +
    labs(
        title = "200 Random Linear Models on the Faithful Dataset",
        x = "Eruption Duration (minutes)",
        y = "Waiting Time to Next Eruption (minutes)"
    )

# Initialize a data frame to store model parameters and their MSE
random_models <- data.frame(
    intercept = numeric(200),
    slope = numeric(200),
    mse = numeric(200)
)

# Generate 200 random models
for (i in 1:200) {
    # Generate random intercept and slope from a plausible range
    # Based on the plot, intercept is ~30-40, slope is ~10
    b0 <- runif(1, min = 20, max = 50)
    b1 <- runif(1, min = 5, max = 15)

    # Calculate predicted values and Mean Squared Error (MSE)
    predictions <- b0 + b1 * df$eruptions
    mse <- mean((df$waiting - predictions)^2)

    # Store the results
    random_models[i, ] <- c(b0, b1, mse)

    # Add the line to the plot
    p_base <- p_base + geom_abline(intercept = b0, slope = b1, color = "gray", alpha = 0.5)
}

# Print the plot with all 200 random lines
print(p_base)

# The `random_models` dataframe now contains the MSE for each model
head(random_models)


# W2Q2d
"
From the above fitted 200 models, select the best one
and write down its equation. Also, mention the reason why you think
the selected model is best among the fitted 200 models.
"
# The best model is the one that minimizes the Mean Squared Error (MSE).
best_random_model <- random_models[which.min(random_models$mse), ]

# Print the details of the best model
print("Best Random Model Details:")
print(best_random_model)

# Write down its equation
cat(paste0(
    "\nEquation of the best fitted random model is:\n",
    "waiting = ", round(best_random_model$intercept, 4), " + ",
    round(best_random_model$slope, 4), " * eruptions\n"
))

# W2Q2e
"
Use the inbuilt function lm() in R, to find the equation of
best fitted linear model (‘waiting’ on ‘eruptions’) for the ‘faithful’ dataset.
"

model_lm <- lm(waiting ~ eruptions, data = df)

# Get the coefficients
lm_coeffs <- coef(model_lm)

# Print the coefficients
print("Coefficients from the lm() function:")
print(lm_coeffs)

# Write the equation of the best-fitted model from lm()
cat(paste0(
    "\nEquation of the best fitted model using lm() is:\n",
    "waiting = ", round(lm_coeffs[1], 4), " + ",
    round(lm_coeffs[2], 4), " * eruptions\n"
))

# W2Q2f
"
Using ‘ggplot’ in R, plot the linear models obtained in part
(d) and (e) in the same scatter plot (plotted in part (b)).
"

# Create a data frame containing the parameters for both lines
line_models <- data.frame(
    model_type = c("Best Random Model", "lm() Model (OLS)"),
    intercept = c(best_random_model$intercept, lm_coeffs[1]),
    slope = c(best_random_model$slope, lm_coeffs[2])
)

# Now, create the plot using this new data frame for the ablines
ggplot(df, aes(x = eruptions, y = waiting)) +
    geom_point(alpha = 0.6, color = "black") +
    labs(
        title = "Comparison of Best Random Model and OLS Model",
        x = "Eruption Duration (minutes)",
        y = "Waiting Time to Next Eruption (minutes)"
    ) +
    # Use the new data frame and map aesthetics for intercept, slope, and color
    geom_abline(
        data = line_models,
        aes(intercept = intercept, slope = slope, color = model_type),
        linewidth = 1
    ) +
    # The manual scale now works perfectly with the 'model_type' variable
    scale_color_manual(
        name = "Fitted Models",
        values = c("Best Random Model" = "red", "lm() Model (OLS)" = "blue")
    ) +
    theme_minimal()


# W2Q2g

"
For each of the fitted models (part (d) and (e)),
plot the ‘residuals’ vs ‘eruptions’ using ‘ggplot’ in R.
Comment on the obtained plots.
"

# 1. Residual plot for the best random model from part (d)
df$residuals_random <- df$waiting - (best_random_model$intercept + best_random_model$slope * df$eruptions)

plot_random_residuals <- ggplot(df, aes(x = eruptions, y = residuals_random)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
        title = "Residuals vs. Eruptions (Best Random Model)",
        x = "Eruption Duration (minutes)",
        y = "Residuals"
    ) +
    theme_minimal()

print(plot_random_residuals)

# 2. Residual plot for the lm() model from part (e)
df$residuals_lm <- residuals(model_lm)

plot_lm_residuals <- ggplot(df, aes(x = eruptions, y = residuals_lm)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
        title = "Residuals vs. Eruptions (lm() Model)",
        x = "Eruption Duration (minutes)",
        y = "Residuals"
    ) +
    theme_minimal()

print(plot_lm_residuals)

# Both residual plots — one from the Best Random Model and the other from the lm() model — display a
# very similar pattern. The residuals are spread around the horizontal line at zero, but they show a distinct
# clustering pattern for shorter and longer eruption durations (around 2 and 4–5 minutes respectively). This # indicates that the residuals are not randomly scattered and there is non-linearity present in the
# relationship between eruption duration and waiting time.

# Conclusion:

# Residuals are centred around zero (no major bias).
# Variance appears roughly constant.
# Clear clustering pattern suggests a non-linear relationship between waiting time and eruption duration.
# The linear assumption of lm() is partially violated.
