# Question 3
mu <- 33

s1 <- c(0, 2, 6)
s2 <- c(6, 12, 20, 30, 42, 56)
s3 <- c(56, 72, 90)
pi1 <- 2 / 9
pi2 <- 5 / 9
pi3 <- 2 / 9

xbar1 <- mean(s1)
xbar2 <- mean(s2)
xbar3 <- mean(s3)

xbar1 * pi1 + xbar2 * pi2 + xbar3 * pi3 - mu

# Question 11
m <- matrix(c(1:6), ncol = 2)
m

dim(m)

# Question 12
m <- matrix(c(1:6), ncol = 2)

m[4, ]

# Question 18
min_pages <- 1
max_pages <- 15

max_pages - min_pages

# Question 27
x <- c(rep(18, 2), rep(19, 8), rep(20, 7), rep(21, 1), rep(22, 1), rep(31, 1))

sd(x)

# Question 30
p_d_and_e <- 63 / 470
p_e <- 93 / 470
p_d_given_e <- p_d_and_e / p_e

p_d_c_and_e_c <- 153 / 470
p_e_c <- 377 / 470
p_d_c_given_e_c <- p_d_c_and_e_c / p_e_c

odds_exposure <- p_d_given_e / (1 - p_d_given_e)
odds_no_exposure <- p_d_c_given_e_c / (1 - p_d_c_given_e_c)

odds_exposure / odds_no_exposure

# Question 32
x <- c(rep(0, 5), rep(1, 30), rep(2, 45), rep(3, 60), rep(4, 60))
n <- length(x)
p <- 0.025

uncertainty <- qnorm(1 - p, mean = 0, sd = 1) * sqrt(var(x) / n)
mean(x) - uncertainty
mean(x) + uncertainty

# Question 36
library(MASS)

batch1 <- c(6, 7, 56, 6, 8, 22, 48, 10, 13, 14, 40, 8)
batch2 <- c(12, 16, 23, 25, 17, 13, 22, 18, 23, 20, 16, 15)
x <- c(batch1, batch2)

fitdistr(x, densfun = "Poisson")

# Question 37
library("e1071")

skewness(x)
kurtosis(x)
