"
Consider the following models:

yij = μ + αi + εij ;i = 1, 2, . . . , 5 and j = 1, 2, . . . , 20

where, αi ∼ N(0, σ_μ^2) and ε ∼ N(0, σ2)
"
install.packages("lme4")
library(lme4)
set.seed(123)

# W12Q1a
"
Generate one value each of μ, σ_μ^2 and σ2
with the help of rdunif() function in R.
"

true_mu <- sample(10:20, 1) # Generated mu
true_sigma_mu_sq <- sample(1:5, 1) # Generated sigma_mu^2
true_sigma_sq <- sample(1:5, 1) # Generated sigma^2

print(paste("True Mu:", true_mu))
print(paste("True Sigma_Mu_Sq:", true_sigma_mu_sq))
print(paste("True Sigma_Sq:", true_sigma_sq))


# W12Q1b
"
Using the value of σ^2 from (a), generate
100 values of εij by using rnorm() function in R.
"
epsilon_ij <- rnorm(100, mean = 0, sd = sqrt(true_sigma_sq))
print(epsilon_ij)

# W12Q1c

"
Using the value of σ_μ^2 from (a),
generate 5 values of αi
by using rnorm() function in R.
"
alpha_i <- rnorm(5, mean = 0, sd = sqrt(true_sigma_mu_sq))
print("Alpha values (Random Effects):")
print(alpha_i)

# W12Q1d

"
Identify the distribution of yij . Find the values of
yij by using above generated values, μ, αi, εij .
"
# y_ij is a linear combination of Normal variables
# distribution: y_ij ~ N(mu, sigma_mu^2 + sigma^2)

alpha_expanded <- rep(alpha_i, each = 20)
y_ij <- true_mu + alpha_expanded + epsilon_ij
print(y_ij)


# W12Q1e

"
Create a data frame which consists of three columns namely “Observation” (which
contains yij ), “Group” (which contains group number) and “rep” (which contains
repetition number).
"

group_col <- factor(rep(1:5, each = 20))
rep_col <- rep(1:20, times = 5)

df <- data.frame(
    Observation = y_ij,
    Group = group_col,
    rep = rep_col
)

head(df) # Preview data

# W12Q1f
"
Fit random effect linear model by using library(lme4)
in R and find the estimates of μ, σ_μ^2 and σ2 by using summary() command.
"

model <- lmer(Observation ~ 1 + (1 | Group), data = df)

print(summary(model))

print("Estimated Mu (Fixed Effect):")
print(fixef(model))

print("Estimated Variance Components:")
print(VarCorr(model))

# W12Q1g
"
Design (specify Null and Alternative) and perform a hypothesis test to test the
variability across all the five groups. Comment on your conclusion.
"

# H0: sigma_mu^2 = 0 (No variability between groups)
# H1: sigma_mu^2 > 0 (Significant variability between groups)
model_null_groups <- lm(Observation ~ 1, data = df)

# Perform ANOVA comparison
lrt_result <- anova(model, model_null_groups)
print(lrt_result)
# Interpretation: If Pr(>Chisq) < 0.05, we reject H0.

result <- "Since Pr(>Chisq) < 0.05 , we rejct h0"

# W12Q1h
"
Design (specify Null and Alternative) and perform a hypothesis test to decide
whether the grand mean is zero or not.
"
# H0: mu = 0
# H1: mu != 0

# Method: We look at the Fixed Effects table in the summary or use confint.
coefs <- summary(model)$coefficients
t_value <- coefs[3]
std_error <- coefs[2]
print(coefs)
print(t_value)
print(std_error)
# Calculate approximate p-value (Wald test)
p_val_mu <- 2 * (1 - pnorm(abs(t_value)))

print(paste("T-value for mu:", t_value))
print(paste("Approximate P-value for mu:", p_val_mu))
# Interpretation: If p-value < 0.05, reject H0 (Mean is not zero).

answer <- "Since p-value < 0.05 we reject H0"

# W12Q1i
"Find the confidence interval for μ."

ci <- confint(model, parm = "(Intercept)", level = 0.95)
print("95% Confidence Interval for Mu:")
print(ci)


# W12Q2a
"
Identify the values of r, m and n as per the given information in the question.
"

r <- 5 # Employees
m <- 6 # Batches
n <- 3 # Replicates

# W12Q2b
"
Generate one value of μ, σ_α^2, σ_β^2, σ_αβ^2 and σ^2
with the help of rdunif() function in R.
"
true_mu <- sample(50:80, 1) # Global Mean
true_sigma_alpha_sq <- sample(1:10, 1) # Variance for Employee
true_sigma_beta_sq <- sample(1:10, 1) # Variance for Batch
true_sigma_gamma_sq <- sample(1:5, 1) # Variance for Interaction
true_sigma_eps_sq <- sample(1:5, 1) # Variance for Error
print(paste("True Mu:", true_mu))
print(paste("True Var(Alpha):", true_sigma_alpha_sq))
print(paste("True Var(Beta):", true_sigma_beta_sq))
print(paste("True Var(Gamma/Interaction):", true_sigma_gamma_sq))
print(paste("True Var(Error):", true_sigma_eps_sq))

# W12Q2c
"
Using the values from (a) Generate the required number of values of
αi, βj, γijand εijk by using rnorm() function in R.
"
# Employee Effects (alpha_i) - Length r
alpha <- rnorm(r, mean = 0, sd = sqrt(true_sigma_alpha_sq))

# Batch Effects (beta_j) - Length m
beta <- rnorm(m, mean = 0, sd = sqrt(true_sigma_beta_sq))

# Interaction Effects (gamma_ij) - Length r * m
# We generate one interaction term for every combination of Employee and Batch
gamma <- rnorm(r * m, mean = 0, sd = sqrt(true_sigma_gamma_sq))

# Error Terms (epsilon_ijk) - Length r * m * n (Total obs)
total_obs <- r * m * n
epsilon <- rnorm(total_obs, mean = 0, sd = sqrt(true_sigma_eps_sq))


# W12Q2d
"
Find the values of yij by using above generated values.
"
df <- expand.grid(
    Rep = 1:n,
    Batch = factor(1:m),
    Employee = factor(1:r)
)

# Sort strictly to align with how we might view the data (optional but cleaner)
df <- df[order(df$Employee, df$Batch, df$Rep), ]

# Assign the generated effects to the correct rows
# 1. Employee Effect: Repeat each alpha n*m times? No.
#    We simply map the Employee ID to the alpha vector.
df$alpha_val <- alpha[df$Employee]

# 2. Batch Effect: Map Batch ID to beta vector
df$beta_val <- beta[df$Batch]

# 3. Interaction Effect: Map the specific combo of Employee/Batch
#    We create a temporary interaction index
df$inter_idx <- as.numeric(interaction(df$Employee, df$Batch))
#    Note: Interaction order depends on factor levels.
#    A safer way manually:
interaction_matrix <- matrix(gamma, nrow = r, ncol = m)
#    Extract correct gamma for each row
df$gamma_val <- mapply(function(e, b) interaction_matrix[e, b], df$Employee, df$Batch)

# 4. Error Term
df$epsilon_val <- epsilon

# Calculate y_ijk
# y = mu + alpha + beta + gamma + epsilon
df$Observation <- true_mu + df$alpha_val + df$beta_val + df$gamma_val + df$epsilon_val

head(df) # Preview the data

# W12Q2e
"Fit random effect linear model by using library(lme4) in R
and find the estimates of μ, σ_α^2, σ_β^2, σ_αβ^2 and σ^2
by using summary() command.
"

full_model <- lmer(
    Observation ~ 1 + (1 | Employee) + (1 | Batch) + (1 | Employee:Batch),
    data = df
)

# Estimates are found in the summary:
# Fixed Effect (Intercept) = Estimate of mu
# Random Effects Variances = Estimates of sigma^2 components
print(summary(full_model))

# W12Q2f
"
We wish to test the variability between different employees. Write down the
null and alternative hypothesis and comment on the conclusion when the test is
performed on the given data.
"
# H0: sigma_alpha^2 = 0
# H1: sigma_alpha^2 > 0

# Reduced model: Remove Employee random effect
model_no_employee <- lmer(Observation ~ 1 + (1 | Batch) + (1 | Employee:Batch), data = df)

# Perform Likelihood Ratio Test
test_employee <- anova(full_model, model_no_employee)
print("Hypothesis Test for Employee Variability:")
print(test_employee)
