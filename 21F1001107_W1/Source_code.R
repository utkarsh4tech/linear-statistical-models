# W1Q1a
"
Simulate 100 samples from Poisson distribution with parameter λ = 2 by using
command rpois(100, lambda) in R-software and store it in vector x.
"
x <- rpois(100, lambda = 2)
x

# W1Q1b
"
Find the summary of the generated dataset by using
command summary() in R-software.
"
summary(x)

# W1Q1c
"
Plot the histogram of the generated dataset the by
using the command hist() in R-software.
"
hist(x)

# W1Q2a
"
Simulate 1000 samples from Normal distribution with parameters μ = 20 and
σ^2 = 16 by using command rnorm(1000, μ, σ) in R-software and store it in vector
y.
"
y <- rnorm(1000, mean = 20, sd = sqrt(16))
y

# W1Q2b
"
Find the summary of the generated dataset by using command summary() in R-
software.
"
summary(y)

# W1Q2c
"
Plot the histogram of the generated dataset the
by using the command hist() in R-software.
"
hist(y)

# W1Q3a
"
Simulate 50 samples from continuous uniform distribution with parameters b = 40
and a = 20 by using the command runif(50, a, b) in R-software and store it in
vector z.
"
z <- runif(50, min = 20, max = 40)
z

# W1Q3b
"
Find the summary of the generated dataset by using command summary() in R-
software.
"
summary(z)

# W1Q3c
"
Plot the histogram of the generated dataset the by using the command hist() in
R-software.
"
hist(z)

# W1Q4
"
An analyst wishes to study inheritance of traits
from generation to generation. For this,
he collected the data (given in the file heights.txt)
of mothers’ height (Mheight) and
the height of one of their adult daughter (Dheight).
Based on the given information, answer the following:
Note: Explore and try out different commands in R for
computation. Also, the plots should be properly labelled
"

# W1Q4a
"
Read the data as a data frame (name it ‘df’) in R
"
df <- read.table(file = "21F1001107_W1/data/heights.txt", header = TRUE)
df

# W1Q4b
"
Find the dimension of the dataframe using R
"
dim(df)

# W1Q4c
"
Using R, find the summary of each of the columns of the above dataframe.
Comment on the dataset based on output obtained.
"
summary(df)

# W1Q4d
"
Print the first and last 10 rows of each column.
"
print(head(df, 10))
print(tail(df, 10))

# Loading ggplot2 for plotting
# install.packages("ggplot2")
library(ggplot2)


# W1Q4e
"
Using the ‘ggplot’ in R, plot the scatter plot
between the mother and daughter heights.
"
ggplot(df, aes(x = Mheight, y = Dheight)) +
    geom_point(color = "blue", alpha = 0.5) +
    labs(
        title = "Scatter Plot of Daughter's Height vs. Mother's Height",
        x = "Mother's Height (inches)",
        y = "Daughter's Height (inches)"
    ) +
    theme_minimal()

# W1Q4f
"
Using ‘ggplot()-geoms’ in R, draw the function
as a continuous curve in the above plotted scatter plot
"
ggplot(df, aes(x = Mheight, y = Dheight)) +
    geom_point(
        color = "blue",
        alpha = 0.5
    ) +
    geom_smooth(
        method = "lm",
        se = FALSE,
        color = "red"
    ) +
    labs(
        title = "Daughter's Height vs. Mother's Height with Trend Line",
        x = "Mother's Height",
        y = "Daughter's Height"
    ) +
    theme_minimal()

# W1Q4g
"
On visualizing the scatter plot, comment on
the independence of the two variables (Dheight and Mheight)
"
ans_w1q4g <- "
    There is definetly a linear relationship b/w Daughter's and Mother's height,
    i.e., Dheight and  Mheight have a positive linear relationship
    "
