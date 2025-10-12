if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Set a seed for reproducibility of random simulations
set.seed(123)

# W3Q1a
"
Consider the Linear Model:
yij = μi + εij ; 1 ≤ j ≤ ni, 1 ≤ i ≤ 3
(a) If εij ∼ N(0, 1), then find the distribution of yij . Elaborate on your answer.
"
# Done on paper, attached in report.pdf

# W3Q1b

"
Generate possible random values using R for μi and ni
Note: The value of ni’s should be greater than or equal to 100.
"
mu_values <- c(5, 10, 15) # mu_1=5, mu_2=10, mu_3=15
n_values <- sample(100:200, 3) # n_1, n_2, n_3, each >= 100

print("Generated mu values:")
print(mu_values)
print("Generated n values (n1, n2, n3):")
print(n_values)

n1 <- n_values[1]
n2 <- n_values[2]
n3 <- n_values[3]
n <- sum(n_values)


# W3Q1c
"
Using R, generate the dataset, i.e. yij , and store it in a data frame.
"
y1 <- rnorm(n1, mean = mu_values[1], sd = 1)
y2 <- rnorm(n2, mean = mu_values[2], sd = 1)
y3 <- rnorm(n3, mean = mu_values[3], sd = 1)

# Create a data frame
dataset <- data.frame(
    value = c(y1, y2, y3),
    group = factor(rep(1:3, times = n_values))
)

print("the generated dataset:")
View(dataset)

# W3Q1d
"
Using R, find the mean of yi’s denoted by yio using the generated dataset.
"
y1_mean <- mean(y1)
y2_mean <- mean(y2)
y3_mean <- mean(y3)
group_means <- c(y1_mean, y2_mean, y3_mean)

print("Group means (y1_o, y2_o, y3_o):")
print(group_means)


# W3Q1e

"
Using R, compute the following: [3 Marks]
i. Sum of squares within group, i.e. SSW.
ii. Sum of squares between group, i.e. SSB.
iii. Total sum of squares, i.e. T SS.
"

# i

grand_mean <- mean(dataset$value)
ssw1 <- sum((y1 - y1_mean)^2)
ssw2 <- sum((y2 - y2_mean)^2)
ssw3 <- sum((y3 - y3_mean)^2)
SSW <- ssw1 + ssw2 + ssw3
print(paste("SSW:", SSW))

# ii

SSB <- n1 * (y1_mean - grand_mean)^2 +
    n2 * (y2_mean - grand_mean)^2 +
    n3 * (y3_mean - grand_mean)^2
print(paste("SSB:", SSB))

# iii

TSS <- sum((dataset$value - grand_mean)^2)
print(paste("TSS:", TSS))

# W3Q1f

"
Using R, verify that T SS = SSB + SSW.
"
print(paste("SSB + SSW:", SSB + SSW))
print(paste("TSS:", TSS))
print(paste("Is TSS equal to SSB + SSW?", isTRUE(all.equal(TSS, SSB + SSW))))

# W3Q1g

"
Randomly generate a number between 1000 and 1500 by using command rdunif()
and perform that many iterations for the parts (c),(d) and (e) in R.
"

if (!requireNamespace("extraDistr", quietly = TRUE)) install.packages("extraDistr")
library(extraDistr)
set.seed(123)
num_iterations <- rdunif(1, min = 1000, max = 1500)
print(paste("Number of iterations to perform:", num_iterations))

ssw_values <- numeric(num_iterations)
ssb_values <- numeric(num_iterations)
tss_values <- numeric(num_iterations)

# Loop for iterations
for (k in 1:num_iterations) {
    # (c) Generate dataset
    y1 <- rnorm(n1, mean = mu_values[1], sd = 1)
    y2 <- rnorm(n2, mean = mu_values[2], sd = 1)
    y3 <- rnorm(n3, mean = mu_values[3], sd = 1)
    all_y <- c(y1, y2, y3)

    # (d) Calculate means
    y1_mean <- mean(y1)
    y2_mean <- mean(y2)
    y3_mean <- mean(y3)
    grand_mean <- mean(all_y)

    # (e) Compute SSW, SSB, TSS
    SSW <- sum((y1 - y1_mean)^2) + sum((y2 - y2_mean)^2) + sum((y3 - y3_mean)^2)
    SSB <- n1 * (y1_mean - grand_mean)^2 + n2 * (y2_mean - grand_mean)^2 + n3 * (y3_mean - grand_mean)^2
    TSS <- sum((all_y - grand_mean)^2)

    # Store results
    ssw_values[k] <- SSW
    ssb_values[k] <- SSB
    tss_values[k] <- TSS
}



# W3Q1h

"Using R, store the values of SSW, SSB and TSS computed in part (g) in a data
frame.
"
# Create the results data frame
results_df <- data.frame(
    Iteration = 1:num_iterations,
    SSW = ssw_values,
    SSB = ssb_values,
    TSS = tss_values
)

print("The simulation results:")
View(results_df)

# W3Q1i

"
Using R, plot the histogram of values obtained for each of SSW, SSB and TSS.
"

p_ssw <- ggplot(results_df, aes(x = SSW)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    ggtitle("Histogram of Sum of Squares Within Groups (SSW)")

# Histogram for SSB
p_ssb <- ggplot(results_df, aes(x = SSB)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", color = "black") +
    ggtitle("Histogram of Sum of Squares Between Groups (SSB)")

# Histogram for TSS
p_tss <- ggplot(results_df, aes(x = TSS)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "salmon", color = "black") +
    ggtitle("Histogram of Total Sum of Squares (TSS)")

# Print the plots
print(p_ssw)
print(p_ssb)
print(p_tss)

# W3Q1j

"
Using the above plotted histogram, verify the following: [8 Marks]
i. SSW ∼ χ^2 with degree of freedom = n − 3. where, n = n1 + n2 + n3

ii. SSB ∼ χ^2 with degree of freedom = 2.

iii. T SS ∼ χ^2 with degree of freedom = n − 1.
"
print(n)
p_ssw_verify <- p_ssw +
    stat_function(fun = dchisq, args = list(df = n - 3), color = "red", size = 1) +
    ggtitle("Histogram of SSW vs. Chi-squared(df=n-3) Density")

print(p_ssw_verify)


p_ssb_verify <- p_ssb +
    stat_function(fun = dchisq, args = list(df = 2), color = "red") +
    ggtitle("Histogram of SSB vs. Chi-squared(df=2) Density")

print(p_ssb_verify)

p_tss_verify <- p_tss +
    stat_function(fun = dchisq, args = list(df = n - 1), color = "red") +
    ggtitle("Histogram of TSS vs. Chi-squared(df=n-1) Density")

print(p_tss_verify)
