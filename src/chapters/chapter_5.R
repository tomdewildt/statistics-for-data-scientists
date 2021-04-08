# 5.6 Method of Moments

### 5.6.2 MME calculation in [R] for lognormal

#### Setup
path <- "./data/"
file <- "high-school.csv"

high_school_data <- read.csv(paste(path, file, sep = ""))
summary(high_school_data$TV)

#### Create high school watch tv data
high_school_watch_tv <- high_school_data[high_school_data$TV > 0, ]
summary(high_school_watch_tv$TV)

#### Compute sample moments to estimate population parameters
n <- nrow(high_school_watch_tv)
n

xbar <- mean(high_school_watch_tv$TV)
xbar

m2 <- ((n - 1) / n) * var(high_school_watch_tv$TV)
m2

#### Estimate population parameters using sample moment
mu <- 2 * log(xbar) - 0.5 * log(xbar^2 + m2)
mu

sigma2 <- log(xbar^2 + m2) - 2 * log(xbar)
sigma2

# 5.7 Maximum likelihood estimation

### 5.7.2 MLE calculation in [R] for lognormal

#### Setup
library(stats4)
library(MASS)

#### Define log likelihood
minuslogl <- function(mu, sigma) {
  densities <- dlnorm(high_school_watch_tv$TV, meanlog = mu, sdlog = sigma)

  -sum(log(densities))
}

#### Maximize log likelihood (mle)
mle(minuslogl, start = list(mu = 10, sigma = 5))

#### Maximize log likelihood (fitdistr)
fitdistr(high_school_watch_tv$TV, densfun = "lognormal")
