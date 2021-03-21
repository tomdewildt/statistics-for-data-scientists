# Problem 5.1

#### Consider the exponential CDF_F_E(x) = 1 - exp(-lambda * x), for x > 0 and
#### otherwise equal to zero. Now let X be distributed according to this
#### exponential distribution.

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

#### Calculate ((n-1) * S^2 / x_p(f_x^2), (n-1) * S^2 / x_1-p(f_x^2)]
variances <- apply(high_school_data[, numerical], 2, function(x) {
  lower <- (n - 1) * var(x) / qchisq(p, df = n - 1, lower.tail = FALSE)
  upper <- (n - 1) * var(x) / qchisq(1 - p, df = n - 1, lower.tail = FALSE)
  c(lower, upper)
})
variances

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
