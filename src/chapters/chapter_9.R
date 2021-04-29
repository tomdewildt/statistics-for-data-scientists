# 9.5 Bayesian linear models: an example in [R]

### 9.5.1 Setting up the data

#### Setup
set.seed(13412348)

#### Generate data
n <- 100
x <- runif(n, min = -10, max = 10)
y <- rnorm(n, mean = 2 + 0.5 * x, sd = 1)

#### Fit ML regression
ml <- lm(y ~ x)

#### Plot data points and regression line
plot(x, y)
abline(ml, col = "red")

#### Print summary
summary(ml)

### 9.5.2 Fitting a Bayesian model

#### Setup
library(MCMCpack)

#### Fit Bayesian linear model
bayes1 <- MCMCregress(y ~ x)

#### Plot model
plot(bayes1)

#### Print summary
summary(bayes1)

### 9.5.3 Exploring prior information

#### Fit Bayesian linear model
bayes2 <- MCMCregress(y ~ x, b0 = 10, B0 = 10)

#### Plot model
plot(bayes2)

#### Print summary
summary(bayes2)
