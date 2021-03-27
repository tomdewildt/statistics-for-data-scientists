# Problem 5.1

#### Consider the exponential CDF_F_E(x) = 1 - exp(-lambda * x), for x > 0 and
#### otherwise equal to zero. Now let X be distributed according to this
#### exponential distribution.

### Setup
library(MASS)

### 1. Determine the relative standard deviation of X.

#### Derive rsd(X) = sqrt(var(X))/mean(X)
#### var(X) = 1 / lambda^2
#### mean(X) = 1 / lambda
####
#### rsd = sqrt(1 / lambda^2) / (1 / lambda) = (1 / lambda) / (1 / lambda) = 1
####
#### Source: https://en.wikipedia.org/wiki/Exponential_distribution

#### Calculate rsd(x) = sqrt(var(x))/mean(x)
x <- rexp(10^6, rate = 1)
rsd <- sqrt(var(x)) / mean(x)
rsd

### 2. Determine the pth quantile.

#### F(X_p(f)) = p
#### F^-1(p) = X_p(f)
####
#### We need to find q such that F(q) = p, so p = 1 - exp(-lambda * q). By
#### solving this equation we get: ?

### 4. Given realizations [0.05, 0.20, 1.72, 0.61, 0.24, 0.79, 0.13, 0.59, 0.26,
###    0.54] from random variables X1,...,X10 which we assume to be distributed
###    i.i.d. exponential, use [R] to compute the maximum likelihood estimate of
###    lambda.
x = c(0.05, 0.20, 1.72, 0.61, 0.24, 0.79, 0.13, 0.59, 0.26, 0.54)
fitdistr(x, densfun = "exponential")

# Problem 5.2

#### Consider the data of the approximately 50000 school children listed in
#### high-school.csv.

### Setup
library("e1071")

path <- "./data/"
file <- "high-school.csv"

high_school_data <- read.csv(paste(path, file, sep = ""))

### 1. Calculate for each numerical variable the average, variance, skewness,
###    and kurtosis.
numerical <- c(4:8, 10:11)

averages <- apply(high_school_data[, numerical], 2, mean)
averages

variances <- apply(high_school_data[, numerical], 2, var)
variances

skewnesses <- apply(high_school_data[, numerical], 2, skewness)
skewnesses

kurtosises <- apply(high_school_data[, numerical], 2, kurtosis)
kurtosises

### 2. Calculate for each numerical variable a 95% confidence interval on the
###    population mean and on the population variance.
p <- 0.025
n <- nrow(high_school_data)

#### Calculate (x̄ - Z_1-p * sqrt(S^2 / n), x̄ + Z_1-p * sqrt(S^2 / n)]
means <- apply(high_school_data[, numerical], 2, function(x) {
  uncertainty <- qnorm(1 - p, mean = 0, sd = 1) * sqrt(var(x) / n)
  lower <- mean(x) - uncertainty
  upper <- mean(x) + uncertainty
  c(lower, upper)
})
means

### 3. Calculate for each numerical variable the 0.20th quantile.
quantiles <- apply(high_school_data[, numerical], 2, function(x) {
  quantile(x, prob = 0.20)
})
quantiles

### 4. Calculate the proportion of children that do not sport and calculate a
###    95% confidence interval on the population proportion.
high_school_no_sports <- as.numeric(high_school_data$SPORTS == 0)

#### Calculate prop = successes / observations
prop_no_sports <- mean(high_school_no_sports)
prop_no_sports

#### Calculate var = probability * (1 - probability)
var_no_sports <- prop_no_sports * (1 - prop_no_sports)
var_no_sports

#### Calculate (x̄ - Z_1-p * sqrt(S^2 / n), x̄ + Z_1-p * sqrt(S^2 / n)]
uncertainty <- qnorm(1 - p, mean = 0, sd = 1) * sqrt(var_no_sports / n)
lower <- prop_no_sports - uncertainty
upper <- prop_no_sports + uncertainty

c(lower, upper)

# Problem 5.5

#### Consider the exponential CDF FE(x) = 1 - exp(-lambda(x - eta)) for x > eta
#### and otherwise equal to zero. Assume that the random variables X1, X2, ...,
#### Xn are i.i.d. exponentially Exp(eta, lambda) distributed.

### 1. Determine the moment estimator for lambda in case eta = 0.

#### (1) Determine population mean: mu(f_theta) = E(x) = 1 / lambda
#### (2) Determine sample mean: x̄ = sum_i=1^n X_i / n
#### (3) Set sample mean equal to population mean: x̄ = 1 / lambda
#### (4) Solve for parameter of interest: lambda = 1 / x̄

# Problem 5.6

#### Consider the data on approximately 50000 children at high schools in the
#### Netherlands and focus on the variable SPORTS. Create a data set that
#### contains only positive values (i.e., eliminate the zeroes). Assume that the
#### data on SPORTS are from a gamma distribution with density function.

### 2. Use the results from part 1 to determine the moment estimates for alpha
###    and beta based on the SPORTS data.
high_school_sports <- high_school_data[high_school_data$SPORTS > 0, ]

n <- nrow(high_school_sports)

#### Calculate sample moments x̄ & M_2
xbar <- mean(high_school_sports$SPORTS)
xbar

m2 <- ((n - 1) / n) * var(high_school_sports$SPORTS)
m2

#### Calculate alpha = x̄^2 / M_2
alpha = xbar^2 / m2
alpha

#### Calculate beta = x̄ / M_2
beta = xbar / m2
beta

### 3. Use [R] to compute the maximum likelihood estimates for alpha and beta.
fitdistr(high_school_sports$SPORTS, densfun = "gamma", lower = c(0, 0))
