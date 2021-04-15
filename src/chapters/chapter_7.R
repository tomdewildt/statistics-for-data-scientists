# 7.2 Bootstrapping

### 7.2.1 The non-parametric bootstrap

#### Setup
n <- 50
sample_data <- rnorm(n, mean = 32, sd = 5)

#### Non-parametric bootstrap mean
k <- 10000
boots <- rep(NA, times = k)
for (k in 1:k) {
  #### Sample from the data with replacement
  resample <- sample(sample_data, size = n, replace = TRUE)

  boots[k] <- mean(resample)
}

hist(boots, breaks = 50)

#### Non-parametric bootstrap variance
k <- 10000
boots <- rep(NA, times = k)
for (k in 1:k) {
  #### Sample from the data with replacement
  resample <- sample(sample_data, size = n, replace = TRUE)

  boots[k] <- var(resample)
}

hist(boots, breaks = 50)

### 7.2.2 The parametric bootstrap

#### Parametric bootstrap mean
k <- 10000
boots <- rep(NA, times = k)

xbar <- mean(sample_data)
s <- sd(sample_data)

for (k in 1:k) {
  #### Sample from the data with replacement
  resample <- rnorm(n, mean = xbar, sd = s)

  boots[k] <- mean(resample)
}

summary(boots)

### 7.2.3 The boot package

#### Setup
library(boot)

#### Non-parametric bootstrap mean
bmean <- function(data, indices) {
  mean(data[indices])
}

boots <- boot(sample_data, statistic = bmean, R = 10000)
boots

# 7.3 Null-hypothesis testing: the one sided test

#### Setup
library(e1071)

path <- "./data/"
file <- "high-school.csv"

high_school_data <- read.csv(paste(path, file, sep = ""))

#### Calculate skewness
skewness(high_school_data$TV, type = 3)
hist(high_school_data$TV)

#### Asymptotic test (tn): H0 = μ(f) ≤ μ = 14 ; Ha = μ(f) > 14
mu0 <- 14

ybar <- mean(high_school_data$TV)
ybar

s <- sd(high_school_data$TV)
s

n <- nrow(high_school_data)
n

tn <- (ybar - mu0) / (s / sqrt(n))
tn

#### Compare test statistic to critical value
tn > qnorm(0.95, mean = 0, sd = 1)

# 7.5 Examining the normality assumption
high_school_boys_data <- high_school_data[high_school_data$GENDER == "Boy", ]

qqnorm(high_school_boys_data$HEIGHT)
qqline(high_school_boys_data$HEIGHT)
qqnorm(high_school_boys_data$TV)
qqline(high_school_boys_data$TV)
