"
An analyst collected the data (“forbes.txt”) to study the relationship between atmo-
spheric pressure and the boiling point of the water. The variables recorded in the

dataset are:
• T emp (
◦F): Boiling point in degrees Fahrenheit
• P ressure (Inches Hg): Atmospheric pressure in inches of mercury
• Lpres = 100 × log10(P ressure)
Based on the given information, answer the following questions.
"

# W7Q1a
"
(a) Read the dataset as a data frame in R.
"
df <- read.table(file = "21F1001107_7/data/forbes.txt", header = TRUE)
View(df)

# W7Q1b
"
Using the ‘ggplot’ in R, plot the scatter plot between the ‘Temp’ and ‘Pressure’.
Based on the obtained plot comment on the relationship between two variables.
"

library(ggplot2)

ggplot(df, aes(x = Temp, y = Pressure)) +
    geom_point(
        color = "blue",
        alpha = 0.5
    ) +
    labs(
        title = "Boiling Point (°F) vs. Atmospheric Pressure (Inches Hg)",
        x = "Boiling Point (Temp)",
        y = "Atmospheric Pressure (Pressure)"
    ) +
    theme_minimal()

ans_w7q1b <- "
    The scatter plot shows a strong, positive,
    and non-linear relationship between Temp and Pressure.
    As the boiling point (Temp) increases,
    the atmospheric pressure (Pressure) also increases.
    The relationship appears to be curvilinear (perhaps exponential);
    the pressure increases at an accelerating rate as the temperature rises.
"

# W7Q1c

"
Obtain the least square estimates using normal equations and lm() function in R
for the model:
Lpres = β0 + β1 * Temp + ε
where, ε ∼ N(0, σ2).

Compare the least square estimates computed using the above-mentioned two
methods.
"

# Method 1: Normal Equations
y <- df$Lpres
X <- cbind(1, df$Temp)

# Calculate beta_hat = (X'X)^-1 * X'y
beta_hat_normal <- solve(t(X) %*% X) %*% t(X) %*% y

cat("Estimates from Normal Equations:\n")
print(beta_hat_normal)

# Method 2: lm() function
model_1 <- lm(Lpres ~ Temp, data = df)
beta_hat_lm <- coef(model_1)

cat("\nEstimates from lm() function:\n")
print(beta_hat_lm)

comment <- "
The intercept and slope, B0 and B1, are coming same from both the methods.
"

# W7Q1d
"
Using ‘ggplot’ in R, add a regression line of the
fitted model in the scatter plot between ‘Lpres′ and ‘Temp′.
"

ggplot(df, aes(x = Temp, y = Lpres)) +
    geom_point(color = "darkgreen") +
    geom_smooth(method = "lm", se = FALSE, color = "red") + # Adds the regression line
    labs(
        title = "Lpres vs. Boiling Point (Temp) with Fitted Regression Line",
        x = "Boiling Point (Temp)",
        y = "Lpres (100 * log10(Pressure))"
    ) +
    theme_minimal()

# W7Q1e

"
Compute the Residual Sum of Squares (RSS) and
Total sum of squares (TSS) for the above fitted model.
"

# Get residuals and fitted values from model_1
residuals_1 <- residuals(model_1)
fitted_1 <- fitted(model_1)

# Calculate Residual Sum of Squares (RSS)
rss_1 <- sum(residuals_1^2)
cat("Residual Sum of Squares (RSS):", rss_1, "\n")

# Calculate Total Sum of Squares (TSS)
mean_lpres <- mean(df$Lpres)
tss_1 <- sum((df$Lpres - mean_lpres)^2)
cat("Total Sum of Squares (TSS):", tss_1, "\n")


# W7Q1f

"
Find Residual standard error (RSE) for the above fitted model. And, comment
on the obtained value.
"

# n = number of observations, p = number of parameters (beta_0, beta_1)
n_1 <- nrow(df)
p_1 <- 2 # (Intercept and Temp)
df_1 <- n_1 - p_1 # Degrees of freedom

# Calculate RSE
rse_1 <- sqrt(rss_1 / df_1)
cat("Residual Standard Error (RSE):", rse_1, "\n")

# Verifying this using the summary() function
summary(model_1)$sigma

comment <- "
The Residual Standard Error (RSE) is 0.3790275.
This value represents the average deviation of the observed
Lpres values from the fitted regression line.
Given that the Lpres values range from approximately 131 to 148,
an average error of 0.3790275 is very small,
indicating that the model fits the data extremely well.
"

# W7Q1g

"
Find R^2 for the above fitted model. And, interpret the value obtained.
"
print(summary(model_1)$r.squared)

comment <- "R^2 value of 0.9949627 means that ~ 99.45% of the
variablility observed in Y ( LPres ) can be explained by X ( Temp )
using this linear model. This is an exceptionally high value,
confirming the very strong linear relationship between Lpres and Temp."


# W7Q2a
"
If the model of the above question is modified as follows:
Lpres = β0 + β1 u1 + ε
where, u1 =
1
Ktemp

And, Ktemp represents the temperature in degrees Kelvin which is given by:
Ktemp = 255.37 + 5/9 × Temp.
Based on the information given, answer the following questions:
"
"
Using the ‘ggplot’ in R, plot the scatter plot of ‘Lpres’ versus u1 and comment
on the relationship between two variables.
"

if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

df <- df %>%
    mutate(
        Ktemp = 255.37 + (5 / 9) * Temp,
        u1 = 1 / Ktemp
    )
# Plot Lpres versus u1
ggplot(df, aes(x = u1, y = Lpres)) +
    geom_point(color = "purple") +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(
        title = "Lpres vs. u1 (1 / Ktemp)",
        x = "u1 (1 / Ktemp)",
        y = "Lpres"
    ) +
    theme_minimal()

comment <- "
The scatter plot shows a very strong, negative, and linear relationship
between u1 and Lpres. As u1 (the inverse of temperature in Kelvin) increases,
Lpres decreases linearly.
This transformation appears to have captured the relationship well.
"

# W7Q2b

"
Fit the linear regression for the given model, and summarize your results.
"

model_inv <- lm(Lpres ~ u1, data = df)
beta_hat_lm_inv <- coef(model_inv)

cat("\nEstimates of Lpres ~ u1 from lm() function:\n")
print(beta_hat_lm_inv)
summary(model_inv)

summary <- "
Coefficients:

Intercept (B0): 734.47 (p-value < 2e-16)

Slope (B1): -218968.41 (p-value < 2e-16)

R-squared: 0.9953

Residual Standard Error (RSE): 0.3673

Both coefficients are highly statistically significant.
The R-squared is 0.9953, and the RSE is 0.3673.
"

# W7Q2c

"
Plot the fitted values obtained from model fitted in 1(c) versus the fitted values
from the model 2(b). On the basis of it, commend on if a model can be preferred
over the other.
"

# Get fitted values from model_2
fitted_2 <- fitted(model_inv)

# Create a data frame of fitted values
fitted_df <- data.frame(
    Model_1_Fitted = fitted_1,
    Model_2_Fitted = fitted_2
)

# Plot fitted values against each other
ggplot(fitted_df, aes(x = Model_1_Fitted, y = Model_2_Fitted)) +
    geom_point(color = "orange") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") + # y=x line
    labs(
        title = "Model 2 Fitted Values vs. Model 1 Fitted Values",
        x = "Fitted Values from Model 1 (Lpres ~ Temp)",
        y = "Fitted Values from Model 2 (Lpres ~ u1)"
    ) +
    theme_minimal()

comment <- "
The plot shows the fitted values from both models are extremely similar,
falling almost perfectly on the $y=x$ dashed line.
This means both models provide nearly identical predictions.

Model 2 has a slightly higher R-squared (0.995 vs 0.9953)
and a significantly lower RSE (0.379 vs 0.3673).
This indicates a statistically better fit to the data.
"

# W7Q3

"
Consider a model as

Y = −1 + 5X + u

"

# Set a seed for reproducibility
set.seed(123)

# W7Q3a

"
Generate 80000 random values from the uniform distribution with parameters
a = 0 and b = 30 for X.
"
X_pop <- runif(80000, min = 0, max = 30)
X_pop

# W7Q3b

"
Generate 80000 random values from the normal distribution with parameters
mean = 0 and sd = 20 for u.
"

u_pop <- rnorm(80000, mean = 0, sd = 20)
u_pop

# W7Q3c

"
Using the above generated values, compute the values of Y . And, store the
respective values of X and Y in a data frame.
"

Y_pop <- -1 + 5 * X_pop + u_pop
Y_pop

# Store in a data frame
population_df <- data.frame(X = X_pop, Y = Y_pop)

head(population_df)

# W7Q3d

"
Take a sample of size 100 from the created data frame. Using the sampled values
of X and Y , fit a linear model, i.e. Y = β0 + β1 X + u.
"

sample_df <- population_df[sample(nrow(population_df), 100), ]

# Fit the linear model
model_sample <- lm(Y ~ X, data = sample_df)

cat("Coefficients from one sample:\n")
print(coef(model_sample))


# W7Q3e

"
Repeat the steps in (d) 8000 times, and store the values of the least square esti-
mates of the coefficients in a matrix.
"

# Set up the simulation
n_sims <- 8000
sample_size <- 100
estimates_matrix <- matrix(NA, nrow = n_sims, ncol = 2)
colnames(estimates_matrix) <- c("beta_0", "beta_1")

# Run the loop
for (i in 1:n_sims) {
    # Take a sample of size 100
    sim_sample <- population_df[sample(nrow(population_df), sample_size), ]

    # Fit the model
    fit <- lm(Y ~ X, data = sim_sample)

    # Store the coefficients
    estimates_matrix[i, ] <- coef(fit)
}

# Convert to a data frame for easier plotting
estimates_df <- as.data.frame(estimates_matrix)

cat("First 5 sets of estimates:\n")
head(estimates_df)
View(estimates_df)

# W7Q3f

"
Plot the histograms for the values of βˆ0 obtained in part (e). Also, find the
distribution for βˆ0 and plot the density curve of the
identified distribution on the plotted histogram.
"
# Calculate mean and sd for the density curve
mu_beta0 <- mean(estimates_df$beta_0)
sd_beta0 <- sd(estimates_df$beta_0)

cat("Mean of beta_0 estimates:", mu_beta0, "\n")
cat("SD of beta_0 estimates:", sd_beta0, "\n")

# Plot histogram and density curve [cite: 60, 61]
ggplot(estimates_df, aes(x = beta_0)) +
    geom_histogram(aes(y = ..density..), bins = 50, fill = "lightblue", color = "black") +
    stat_function(
        fun = dnorm,
        args = list(mean = mu_beta0, sd = sd_beta0),
        color = "red",
        size = 1
    ) +
    labs(
        title = "Sampling Distribution of Intercept (β0)",
        subtitle = paste("True β0 = -1, Sample Mean =", round(mu_beta0, 4)),
        x = "β0 Estimate",
        y = "Density"
    ) +
    theme_minimal()

comment <- "
Distribution for β^₀:
- The histogram clearly shows a symmetric, bell-shaped distribution.
    The identified distribution is the Normal distribution.
- The mean of the 8,000 β^₀ estimates (approx. -1.0)
is extremely close to the true value of β₀ = -1,
demonstrating that the OLS estimator for the intercept is unbiased.
"

# W7Q3g

"
Plot the histograms for the values of βˆ1 obtained in part (e). Also, find the

distribution for βˆ1 and plot the density curve of
the identified distribution on the plotted histogram.
"
# Calculate mean and sd for the density curve
mu_beta1 <- mean(estimates_df$beta_1)
sd_beta1 <- sd(estimates_df$beta_1)

cat("Mean of beta_1 estimates:", mu_beta1, "\n")
cat("SD of beta_1 estimates:", sd_beta1, "\n")

# Plot histogram and density curve [cite: 63, 64]
ggplot(estimates_df, aes(x = beta_1)) +
    geom_histogram(aes(y = ..density..), bins = 50, fill = "lightgreen", color = "black") +
    stat_function(
        fun = dnorm,
        args = list(mean = mu_beta1, sd = sd_beta1),
        color = "blue",
        size = 1
    ) +
    labs(
        title = "Sampling Distribution of Slope (β1)",
        subtitle = paste("True β1 = 5, Sample Mean =", round(mu_beta1, 4)),
        x = "β1 Estimate",
        y = "Density"
    ) +
    theme_minimal()

comment <- "
Distribution for β^1:
- The histogram clearly shows a symmetric, bell-shaped distribution.
    The identified distribution is the Normal distribution.
- The mean of the 8,000 β^1 estimates (approx. 5)
is extremely close to the true value of β1 = 5,
demonstrating that the OLS estimator for the intercept is unbiased.
"
