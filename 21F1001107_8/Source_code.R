set.seed(42)

"
Consider the Linear Model:
yi = β0 + β1 xi + ϵi ; 1 ≤ i ≤ 1000
"

# W8Q1a

"
(a) If ϵi ∼ N(0, σ^2), then find the distribution of yi. Elaborate on your answe
"

# Done on Report

# W8Q1b

"
Using R sample one value each for β0, β1 and
one set of xi as generated in code draft provided
"

beta_0 <- runif(1, 5, 12)
beta_1 <- runif(1, -10, 10)
N <- 1000
x <- runif(N, -5, 5)

print(paste("True beta_0:", beta_0))
print(paste("True beta_1:", beta_1))
print(paste("N:", N))
print(paste("x:", x))


# W8Q1c
"
Using R, generate the dataset, i.e. yi
, and store it in a data frame
"

error <- rnorm(N, 0, 1)
y <- beta_0 + beta_1 * x + error
dataset <- data.frame(x, y)

View(dataset)

# W8Q1d

"
Perform 200 iterations of the below and store the values of
β_1, R_1^2, R_0^2 and R_1^2−R_0^2 computed in a data frame called estimates.
"
"
(i) Sample n = 100 members for the data. Find the least square estimates of β0
and β1 for this sampled data.
"

# W8Q1d(i)

n_iterations <- 200
n <- 100

# Initialize a data frame to store results
estimates <- data.frame(
    beta_1_hat = numeric(n_iterations),
    R_0_square = numeric(n_iterations),
    R_1_square = numeric(n_iterations),
    Reg_SS = numeric(n_iterations) # R_1^2 - R_0^2
)

for (i in 1:n_iterations) {
    # i. Sample n=100 members
    sampled_data <- dataset[sample(1:N, n), ]

    x_sample <- sampled_data$x
    y_sample <- sampled_data$y

    # Calculate means
    x_bar <- mean(x_sample)
    y_bar <- mean(y_sample)

    # Calculate LSEs
    beta_1_hat <- sum((y_sample - y_bar) * (x_sample - x_bar)) / sum((x_sample - x_bar)^2)
    beta_0_hat <- y_bar - beta_1_hat * x_bar

    # ii. Compute Sum of Squares
    # R_1^2 (Total Sum of Squares, TSS)
    R_1_square <- sum((y_sample - y_bar)^2)

    # R_0^2 (Residual Sum of Squares, RSS)
    # Using the formula from the draft code
    R_0_square <- sum((y_sample - y_bar)^2) - (sum((y_sample - y_bar) * (x_sample - x_bar)))^2 / sum((x_sample - x_bar)^2)

    # C. R_1^2 - R_0^2 (Regression Sum of Squares, RegSS)
    Reg_SS <- R_1_square - R_0_square

    # iii. Estimate sigma^2
    sigma_2_hat <- R_0_square / (n - 2)

    # Store values
    estimates$beta_1_hat[i] <- beta_1_hat
    estimates$R_0_square[i] <- R_0_square
    estimates$R_1_square[i] <- R_1_square
    estimates$Reg_SS[i] <- Reg_SS
}

print("Q1 Iterations complete. Displaying plots...")

# W8Q1e and # W8Q1f

# 1(e) & (f) Plot histograms and fit densities

# Set up 2x2 plotting area
par(mfrow = c(2, 2))

# 1. beta_1_hat
hist(estimates$beta_1_hat,
    prob = TRUE,
    main = expression(paste("Histogram of ", hat(beta)[1])),
    xlab = expression(hat(beta)[1]), breaks = 20, col = "lightblue"
)
# Overlay Normal density (using estimated parameters from the simulation)
curve(dnorm(x, mean = mean(estimates$beta_1_hat), sd = sd(estimates$beta_1_hat)),
    add = TRUE, col = "red", lwd = 2
)
# Add vertical line for the true beta_1
abline(v = beta_1, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Fitted Density", "True beta_1"), col = c("red", "blue"), lty = c(1, 2))

# 2. R_0^2 (RSS)
hist(estimates$R_0_square,
    prob = TRUE,
    main = expression(paste("Histogram of ", R[0]^2, " (RSS)")),
    xlab = "RSS (Chi-sq(98))", breaks = 20, col = "lightblue"
)
# Overlay Chi-square(n-2) density (df = 98)
curve(dchisq(x, df = n - 2), add = TRUE, col = "red", lwd = 2)
legend("topright", legend = c("Chi-sq(n-2)"), col = "red", lty = 1)

# 3. R_1^2 (TSS)
hist(estimates$R_1_square,
    prob = TRUE,
    main = expression(paste("Histogram of ", R[1]^2, " (TSS)")),
    xlab = "TSS (Chi-sq(99))", breaks = 20, col = "lightblue"
)
# Overlay Chi-square(n-1) density (df = 99)
curve(dchisq(x, df = n - 1), add = TRUE, col = "red", lwd = 2)
legend("topright", legend = c("Chi-sq(n-1)"), col = "red", lty = 1)

# 4. R_1^2 - R_0^2 (RegSS)
hist(estimates$Reg_SS,
    prob = TRUE,
    main = expression(paste("Histogram of ", R[1]^2 - R[0]^2, " (RegSS)")),
    xlab = "RegSS (Chi-sq(1))", breaks = 20, col = "lightblue"
)
# Overlay Chi-square(1) density (df = 1)
curve(dchisq(x, df = 1), add = TRUE, col = "red", lwd = 2)
legend("topright", legend = c("Chi-sq(1)"), col = "red", lty = 1)

# Reset plotting layout
par(mfrow = c(1, 1))

#################################################################
# QUESTION 2
#################################################################

"
Consider the Linear Model:

yi = β0 + β1 x1i + β2 x2i + εi; 1 ≤ i ≤ 1000

"

# W8Q2a

"
If εi ∼ N(0, σ2), then find the distribution of yi.
Elaborate on your answer.
"

# Done on Report


# W8Q2b

"
Using R sample one value each for β0, β1, β2
and one set of x1i and x2i as generated in code draft provided.
"
beta_0_2 <- runif(1, 5, 12)
beta_1_2 <- runif(1, -10, 10)
beta_2_2 <- runif(1, 2, 7)
N_2 <- 1000
x_1 <- runif(N_2, -5, 5)
x_2 <- runif(N_2, 0, 10)

print(paste("Q2 True beta_0:", beta_0_2))
print(paste("Q2 True beta_1:", beta_1_2))
print(paste("Q2 True beta_2:", beta_2_2))

# W8Q2c
error_2 <- rnorm(N_2, 0, 1) # sigma^2 is 1
y_2 <- beta_0_2 + beta_1_2 * x_1 + beta_2_2 * x_2 + error_2
dataset_2 <- data.frame(x_1, x_2, y = y_2) # Renaming y_2 to y for model fitting
View(dataset_2)


# W8Q2d

n_iterations_2 <- 200
n_2 <- 100

# Initialize data frame
estimates2 <- data.frame(
    R_0_square = numeric(n_iterations_2),
    R_1_square = numeric(n_iterations_2),
    Reg_SS = numeric(n_iterations_2) # R_1^2 - R_0^2
)

for (i in 1:n_iterations_2) {
    # i. Sample n=100 members
    sampled_data_2 <- dataset_2[sample(1:N_2, n_2), ]

    # Find LSEs (using R's built-in lm() function for simplicity and stability)
    # This is equivalent to the matrix algebra in the draft
    model <- lm(y ~ x_1 + x_2, data = sampled_data_2)

    # Calculate predicted values
    y_hat <- model$fitted.values
    y_sample <- sampled_data_2$y

    # ii. Compute Sum of Squares
    y_bar <- mean(y_sample)

    # A. R_0^2 (Residual Sum of Squares, RSS)
    R_0_square <- sum((y_sample - y_hat)^2) # Same as sum(model$residuals^2)

    # B. R_1^2 (Total Sum of Squares, TSS)
    R_1_square <- sum((y_sample - y_bar)^2)

    # C. R_1^2 - R_0^2 (Regression Sum of Squares, RegSS)
    Reg_SS <- R_1_square - R_0_square

    # Store values
    estimates2$R_0_square[i] <- R_0_square
    estimates2$R_1_square[i] <- R_1_square
    estimates2$Reg_SS[i] <- Reg_SS
}



# W8Q2e

# Set up 1x3 plotting area
par(mfrow = c(1, 3))

# 1. R_0^2 (RSS)
hist(estimates2$R_0_square,
    prob = TRUE,
    main = expression(paste("Histogram of ", R[0]^2, " (RSS)")),
    xlab = "RSS (Chi-sq(97))", breaks = 20, col = "lightgreen"
)
# Overlay Chi-square(n-3) density (df = 97)
curve(dchisq(x, df = n_2 - 3), add = TRUE, col = "blue", lwd = 2)
legend("topright", legend = c("Chi-sq(n-3)"), col = "blue", lty = 1)

# 2. R_1^2 (TSS)
hist(estimates2$R_1_square,
    prob = TRUE,
    main = expression(paste("Histogram of ", R[1]^2, " (TSS)")),
    xlab = "TSS (Chi-sq(99))", breaks = 20, col = "lightgreen"
)
# Overlay Chi-square(n-1) density (df = 99)
curve(dchisq(x, df = n_2 - 1), add = TRUE, col = "blue", lwd = 2)
legend("topright", legend = c("Chi-sq(n-1)"), col = "blue", lty = 1)

# 3. R_1^2 - R_0^2 (RegSS)
hist(estimates2$Reg_SS,
    prob = TRUE,
    main = expression(paste("Histogram of ", R[1]^2 - R[0]^2, " (RegSS)")),
    xlab = "RegSS (Chi-sq(2))", breaks = 20, col = "lightgreen"
)
# Overlay Chi-square(2) density (df = 2)
curve(dchisq(x, df = 2), add = TRUE, col = "blue", lwd = 2)
legend("topright", legend = c("Chi-sq(2)"), col = "blue", lty = 1)

# Reset plotting layout
par(mfrow = c(1, 1))

# W8Q2f

# done in report
