################################################################################
################################### 0. Setup ###################################
################################################################################

library(e1071) # skewness, kurtosis
library(MASS) # fitdistr
library(outliers) # grubbs.test

################################################################################
################################### 1. Basics ##################################
################################################################################

### 1.1 Dimensions (rows, columns)
dim(data)

### 1.2 First n rows (n = 6)
head(data, n = 6)

### 1.3 Summary
summary(data)

### 1.4 Mean
mean(data$column)

### 1.5 Variance
var(data$column)

### 1.6 Standard Deviation
sd(data$column)

### 1.7 Median
median(data$column)

### 1.8 Mode
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode(data$column)

### 1.9 Min & Max
min(data$column)
max(data$column)

### 1.10 Unique
unique(data$column)

### 1.11 Quantile (probs = c(0, 0.25, 0.50, 0.75, 1.00))
quantile(data$column, probs = c(0, 0.25, 0.50, 0.75, 1.00))

### 1.12 Interquartile Range (IQR)
first <- quantile(data$column, probs = c(0.25))
third <- quantile(data$column, probs = c(0.75))
first - third

### 1.13 Mean Absolute Deviation (MAD)
sum(abs(data$column - mean(data$column))) / length(data$column)

### 1.14 Mean Squared Deviation (MSD)
(length(data$column) - 1) * var(data$column) / length(data$column)

### 1.15 Skewness
skewness(data$column)

### 1.16 Kurtosis
kurtosis(data$column)

################################################################################
################################## 2. Sampling #################################
################################################################################

### 2.1 Simple Random Sampling (n = 1, replacement = FALSE)
sample(data$column, 1, replace = FALSE)

### 2.2 Simple Random Subsets (N = population size, n = sample size)
N <- 9
n <- 3
combn(N, n)

### 2.3 Systematic Random Subsets (X = population)
X <- 1:9
rbind(X[1:3], X[4:6], X[7:9])

### 2.4 Stratified Random Subsets (X = population)
X <- 1:9
unname(t(expand.grid(X[1:3], X[4:6], X[7:9])))

### 2.5 Bias (E_T = estimated value(s), theta = actual value)
mean(E_T) - theta

### 2.6 MSE (E_T = estimated value(s), theta = actual value)
mse <- mean((E_T - theta)^2)

### 2.7 SE (E_T = estimated value(s), theta = actual value)
sqrt(mean((E_T - mean(E_T))^2))

################################################################################
############################## 4. Random Variables #############################
################################################################################

### 4.1 Bernoulli PMF (k = successes, n = observations, p = probability)
k <- 10
n <- 20
p <- 0.5
dbinom(k, size = n, prob = p)

### 4.2 Bernoulli CDF (k = successes, n = observations, p = probability)
k <- 10
n <- 20
p <- 0.5
pbinom(k, size = n, prob = p)

#### 4.3 Normal PDF (x = value, mu = mean, sigma = var)
x <- 1
mu <- 0
sigma <- 1
dnorm(x, mean = mu, sd = sqrt(sigma))

#### 4.4 Normal CDF (x = value, mu = mean, sigma = var)
x <- 1
mu <- 0
sigma <- 1
pnorm(x, mean = mu, sd = sqrt(sigma))

### 4.5 Draw From Normal Distribution (n = times, mu = mean, sigma = var)
n <- 1000000
mu <- 10
sigma <- 3
rnorm(n, mean = mu, sd = sqrt(sigma))

################################################################################
################################# 5. Estimation ################################
################################################################################

### 5.1 95% Confidence Interval (p = CI 95%, n = total rows)
p <- 0.025
n <- nrow(data$column)

uncertainty <- qnorm(1 - p, mean = 0, sd = 1) * sqrt(var(data$column) / n)
lower <- mean(data$column) - uncertainty
upper <- mean(data$column) + uncertainty

### 5.2 Maximum Likelihood Estimate ([beta,exponential,gamma,lognormal,normal])
fitdistr(data$column, densfun = "normal")

################################################################################
############################# 6. Multiple Variables ############################
################################################################################

### 6.1 Contingency Table
data_table <- table(data$column1, data$column2)
data_table <- addmargins(data_table)

### 6.2 Chi-Squared (data_table = contingency table)
chisq.test(data_table)

### 6.3 Correlation Coefficient (data_table = contingency table, n = sum)
sqrt(chisq.test(data_table) / n)

### 6.4 Cramer's V (data_table = contingency table, n = sum, t = min rows/cols)
t <- min(nrow(data_table) - 1, ncol(data_table) - 1)
sqrt(chi_squared / (n * t))

### 6.5 Covariance
cov(data$column1, data$column2)

### 6.6 Correlation (use = complete.obs (handle NA values), method = pearson)
cor(data$column1, data$column2, use = "complete.obs", method = "pearson")

################################################################################
############################# 7. Hypothesis Testing ############################
################################################################################

### 7.1 Non-parametric Bootstrap (k = times, n = total rows)
k <- 1000
n <- nrow(data$column)

means <- rep(NA, times = k)
for (k in 1:k) {
  resample <- sample(data$column, size = n, replace = TRUE)

  means[k] <- mean(resample)
}
means

### 7.2 One-sided Test (alternative = [greater, less], mu = h0)
t.test(data$column, alternative = "greater", mu = 10)

### 7.3 Two-sided Test (alternative = two.sided, mu = h0)
t.test(data$column, alternative = "two.sided", mu = 10)

### 7.4 Equivalence Test (n = total rows, a = CI 90%, mu = h0, delta = margin)
n <- nrow(data$column)
a <- 0.05

mu <- 10
delta <- 2

uncertainty <- qnorm(1 - a, mean = 0, sd = 1) * sqrt(var(data$column) / n)
difference <- mean(data$column) - mu

lower <- difference - uncertainty
upper <- difference + uncertainty
ci <- c(lower, upper)

-delta < ci[1] & ci[2] < delta

### 7.5 Two-sided (Equal Means) Test (alternative = two.sided)
t.test(data$column1, data$column2, alternative = "two.sided", var.equal = FALSE)

### 7.6 Two-sided (Equal Means & Variance) Test (alternative = two.sided)
t.test(data$column1, data$column2, alternative = "two.sided", var.equal = TRUE)

### 7.7 Variance (Equal Variances) Test
var.test(data$column1, data$column1)

### 7.8 Grubbs (Outlier) Test
grubbs.test(data$column)

### 7.9 Check Normally Distributed
qqnorm(data$column)
qqline(data$column)

################################################################################
################################## 8. Modeling #################################
################################################################################

### 8.1 Linear Regression Model (column1 = β0 + β1*column2)
lm(column1 ~ 1 + column2, data = data)

### 8.2 Linear Regression Model (column1 = β0 + β1*column2 + β2*column3)
lm(column1 ~ 1 + column2 + column3, data = data)

### 8.3 Linear Regression Model
### (column1 = β0 + β1*column2 + β2*column3 + β3(column3*column4))
lm(column1 ~ 1 + column2 * column3 + column4, data = data)

### 8.4 Model Design Matrix (m = model)
model.matrix(m)

### 8.5 Model Coefficients (m = model)
coef(m)

### 8.6 MSE (column1 = dependent variable, m = model, n = total rows)
sum((data$column1 - predict(m, data = data))^2) / n

### 8.7 Akaike Information Criterion (AIC) (m1 = model 1, m2 = model 2)
AIC(m1, m2)

### 8.8 Bayesian Information Criterion (BIC) (m1 = model 1, m2 = model 2)
BIC(m1, m2)

################################################################################
################################### 99. Extra ##################################
################################################################################

### 99.1 Set Seed
set.seed(42)

### 99.2 Selecting Data (rows = row indexes, columns = column indexes)
data[rows, columns]

### 99.3 Deleting Data
data[data$column > 0, ]

### 99.4 Generating Data (value = value to repeat, times = amount of times)
value <- 10
times <- 5
rep(value, times)
